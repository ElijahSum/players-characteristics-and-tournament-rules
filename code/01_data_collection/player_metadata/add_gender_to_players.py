#!/usr/bin/env python3
from __future__ import annotations

import csv
import html
import re
import unicodedata
import xml.etree.ElementTree as ET
import zipfile
from collections import Counter, defaultdict
from dataclasses import dataclass
from pathlib import Path

from lookup_missing_birthdays import fide_forms, max_rating, target_forms, transliterate_cyrillic

try:
    import gender_guesser.detector as gender_detector
except ImportError:
    gender_detector = None


PLAYERS = Path("players_final_data_merged.csv")
TOURNAMENTS = Path("../data/final_regression_data_tournaments_2022_2026.csv")
FIDE_ZIP = Path("fide_players_list_xml.zip")
FIDE_XML = "players_list_xml_foa.xml"

GENDER_FINDINGS = Path("gender_findings.csv")
FIDE_GENDER_FINDINGS = Path("gender_findings_fide.csv")
PRIOR_FIDE_MATCHES = Path("missing_birthdays_fide_matches.csv")

FIDE_SOURCE = (
    "FIDE players_list_xml_foa.xml downloaded 2026-05-12 from "
    "https://ratings.fide.com/download/players_list_xml.zip"
)

TITLE_STRENGTH = {
    "GM": 10,
    "IM": 9,
    "WGM": 8,
    "FM": 7,
    "WIM": 6,
    "CM": 5,
    "WFM": 4,
    "WCM": 3,
    "NM": 2,
    "WNM": 1,
    "NO TITLE": 0,
    "": -1,
}


@dataclass(frozen=True)
class FideCandidate:
    fide_id: str
    fide_name: str
    fide_country: str
    fide_sex: str
    fide_title: str
    fide_rating: int
    fide_birthday: str
    match_kind: str
    match_strength: int
    match_values: str


def blank(value: str | None) -> bool:
    return not (value or "").strip()


def norm_title(value: str | None) -> str:
    return (value or "").strip().upper()


def read_csv(path: Path) -> tuple[list[dict[str, str]], list[str]]:
    with path.open(newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        return list(reader), reader.fieldnames or []


def birthday_year(value: str | None) -> str:
    text = (value or "").strip()
    if not text:
        return ""
    try:
        return str(int(float(text)))
    except ValueError:
        match = re.search(r"\b(18|19|20)\d{2}\b", text)
        return match.group(0) if match else ""


def int_rating(value: str | None) -> int:
    try:
        return int(float((value or "").strip()))
    except ValueError:
        return 0


def player_max_rating(row: dict[str, str]) -> int:
    return max(
        int_rating(row.get("classic_rating")),
        int_rating(row.get("rapid_rating")),
        int_rating(row.get("blitz_rating")),
    )


def load_titles() -> tuple[dict[str, str], dict[str, str], set[str]]:
    title_counts: dict[str, Counter[str]] = defaultdict(Counter)
    with TOURNAMENTS.open(newline="", encoding="utf-8") as fh:
        for row in csv.DictReader(fh):
            title_counts[row["player_name"]][norm_title(row.get("player_title"))] += 1

    title_by_player: dict[str, str] = {}
    title_values_by_player: dict[str, str] = {}
    players_with_w_title: set[str] = set()
    for player, counts in title_counts.items():
        titles = [title for title in counts if title]
        if any(title.startswith("W") for title in titles):
            players_with_w_title.add(player)

        meaningful = [title for title in titles if title != "NO TITLE"]
        if meaningful:
            # Prefer a women-specific title when present, because the requested
            # merge is also used as certain gender evidence.
            w_titles = [title for title in meaningful if title.startswith("W")]
            pool = w_titles or meaningful
            chosen = sorted(
                pool,
                key=lambda item: (counts[item], TITLE_STRENGTH.get(item, -1)),
                reverse=True,
            )[0]
        elif titles:
            chosen = "No Title"
        else:
            chosen = ""

        title_by_player[player] = chosen
        title_values_by_player[player] = ";".join(
            title for title, _ in sorted(
                counts.items(),
                key=lambda item: (TITLE_STRENGTH.get(item[0], -1), item[1], item[0]),
                reverse=True,
            )
            if title
        )
    return title_by_player, title_values_by_player, players_with_w_title


def title_gender(row: dict[str, str], players_with_w_title: set[str]) -> dict[str, str] | None:
    title = norm_title(row.get("player_title"))
    if title.startswith("W") or row["player_name"] in players_with_w_title:
        return {
            "gender": "female",
            "gender_source": "player_title_starts_with_W",
            "gender_confidence": "certain",
            "gender_evidence": f"player_title={row.get('player_title', '')}",
        }
    return None


def build_target_indexes(rows: list[dict[str, str]]):
    exact_index: dict[str, set[int]] = defaultdict(set)
    first_last_index: dict[str, set[int]] = defaultdict(set)
    for idx, row in enumerate(rows):
        if blank(row.get("real_name")):
            continue
        forms = target_forms(row["real_name"])
        for form in forms["exact"] | forms["swapped"]:
            exact_index[form].add(idx)
        for form in forms["first_last"]:
            first_last_index[form].add(idx)
    return exact_index, first_last_index


def fide_display_name(raw: str) -> str:
    raw = re.sub(r"\s+", " ", raw or "").strip()
    if "," in raw:
        last, first = [part.strip() for part in raw.split(",", 1)]
        return f"{first} {last}".strip()
    return raw


def fide_title(fields: dict[str, str]) -> str:
    for key in ("title", "w_title", "foa_title", "o_title"):
        value = norm_title(fields.get(key))
        if value:
            return value
    return ""


def add_candidate(
    by_row: dict[int, list[FideCandidate]],
    row_idx: int,
    fields: dict[str, str],
    match_kind: str,
    match_strength: int,
    match_values: set[str],
) -> None:
    sex = norm_title(fields.get("sex"))
    if sex not in {"M", "F"}:
        return
    by_row[row_idx].append(
        FideCandidate(
            fide_id=fields.get("fideid", ""),
            fide_name=fide_display_name(fields.get("name", "")),
            fide_country=norm_title(fields.get("country")),
            fide_sex=sex,
            fide_title=fide_title(fields),
            fide_rating=max_rating(
                {
                    "fide_standard_rating": fields.get("rating", ""),
                    "fide_rapid_rating": fields.get("rapid_rating", ""),
                    "fide_blitz_rating": fields.get("blitz_rating", ""),
                }
            ),
            fide_birthday=birthday_year(fields.get("birthday")),
            match_kind=match_kind,
            match_strength=match_strength,
            match_values=";".join(sorted(match_values)),
        )
    )


def collect_fide_candidates(rows: list[dict[str, str]]) -> tuple[dict[int, list[FideCandidate]], dict[str, str]]:
    exact_index, first_last_index = build_target_indexes(rows)
    by_row: dict[int, list[FideCandidate]] = defaultdict(list)
    sex_by_fide_id: dict[str, str] = {}
    parsed = 0

    with zipfile.ZipFile(FIDE_ZIP) as archive:
        with archive.open(FIDE_XML) as fh:
            for _, elem in ET.iterparse(fh, events=("end",)):
                if elem.tag != "player":
                    continue
                parsed += 1
                fields = {child.tag: (child.text or "").strip() for child in elem}
                sex = norm_title(fields.get("sex"))
                fide_id = fields.get("fideid", "")
                if fide_id and sex in {"M", "F"}:
                    sex_by_fide_id[fide_id] = sex
                raw_name = fields.get("name", "")
                forms = fide_forms(raw_name)

                exact_matches = forms["exact"]
                for form in exact_matches:
                    for row_idx in exact_index.get(form, ()):
                        add_candidate(by_row, row_idx, fields, "exact_full_name", 3, {form})

                for form in forms["first_last"] | forms["exact"]:
                    for row_idx in first_last_index.get(form, ()):
                        add_candidate(by_row, row_idx, fields, "first_last_partial", 1, {form})

                if parsed % 250000 == 0:
                    print(f"parsed {parsed} FIDE players", flush=True)
                elem.clear()
    print(f"parsed {parsed} FIDE players", flush=True)
    return by_row, sex_by_fide_id


def candidate_features(row: dict[str, str], candidate: FideCandidate) -> tuple[bool, bool, bool, bool]:
    country_match = bool(norm_title(row.get("federation")) and norm_title(row.get("federation")) == candidate.fide_country)
    birthday_match = bool(birthday_year(row.get("birthday")) and birthday_year(row.get("birthday")) == candidate.fide_birthday)
    rating = player_max_rating(row)
    rating_match = bool(rating and candidate.fide_rating and abs(rating - candidate.fide_rating) <= 10)
    title = norm_title(row.get("player_title"))
    title_match = bool(title and title != "NO TITLE" and title == candidate.fide_title)
    return country_match, birthday_match, rating_match, title_match


def choose_fide_gender(row: dict[str, str], candidates: list[FideCandidate]) -> dict[str, str] | None:
    if not candidates:
        return None

    unique: dict[tuple[str, str, str], FideCandidate] = {}
    for candidate in candidates:
        unique[(candidate.fide_id, candidate.match_kind, candidate.match_values)] = candidate
    candidates = list(unique.values())

    tiers: list[tuple[str, list[FideCandidate]]] = []
    exact = [candidate for candidate in candidates if candidate.match_strength == 3]
    supported = [
        candidate
        for candidate in exact
        if any(candidate_features(row, candidate))
    ]
    country_or_birthday = [
        candidate
        for candidate in candidates
        if candidate.match_strength >= 1
        and (candidate_features(row, candidate)[0] or candidate_features(row, candidate)[1])
    ]
    tiers.append(("exact_supported", supported))
    tiers.append(("exact_name", exact))
    tiers.append(("country_or_birthday_supported", country_or_birthday))

    for tier_name, tier_candidates in tiers:
        if not tier_candidates:
            continue
        sexes = {candidate.fide_sex for candidate in tier_candidates}
        if len(sexes) != 1:
            continue
        best = sorted(
            tier_candidates,
            key=lambda candidate: (
                candidate.match_strength,
                sum(candidate_features(row, candidate)),
                candidate.fide_rating,
            ),
            reverse=True,
        )[0]
        country_match, birthday_match, rating_match, title_match = candidate_features(row, best)
        confidence = "high" if tier_name != "exact_name" or len(tier_candidates) == 1 else "medium"
        return {
            "gender": "female" if best.fide_sex == "F" else "male",
            "gender_source": "fide_sex",
            "gender_confidence": confidence,
            "gender_evidence": (
                f"{tier_name}; fide_id={best.fide_id}; fide_name={best.fide_name}; "
                f"fide_sex={best.fide_sex}; match={best.match_kind}; "
                f"country_match={country_match}; birthday_match={birthday_match}; "
                f"rating_match={rating_match}; title_match={title_match}; "
                f"candidate_count={len(tier_candidates)}"
            ),
        }
    return None


def load_prior_fide_gender(rows: list[dict[str, str]], sex_by_fide_id: dict[str, str]) -> dict[str, dict[str, str]]:
    if not PRIOR_FIDE_MATCHES.exists():
        return {}
    player_rows: dict[str, list[dict[str, str]]] = defaultdict(list)
    with PRIOR_FIDE_MATCHES.open(newline="", encoding="utf-8") as fh:
        for row in csv.DictReader(fh):
            confidence = row.get("confidence", "")
            if confidence not in {"high_unique_country", "high_same_year_country"}:
                continue
            sex = sex_by_fide_id.get(row.get("fide_id", ""))
            if sex in {"M", "F"}:
                row = dict(row)
                row["fide_sex"] = sex
                player_rows[row["player_name"]].append(row)

    output = {}
    valid_players = {row["player_name"] for row in rows}
    for player, matches in player_rows.items():
        if player not in valid_players:
            continue
        sexes = {match["fide_sex"] for match in matches}
        if len(sexes) != 1:
            continue
        best = sorted(
            matches,
            key=lambda match: int_rating(match.get("fide_standard_rating")) + int_rating(match.get("fide_rapid_rating")) + int_rating(match.get("fide_blitz_rating")),
            reverse=True,
        )[0]
        output[player] = {
            "gender": "female" if best["fide_sex"] == "F" else "male",
            "gender_source": "fide_sex_prior_birthdate_match",
            "gender_confidence": "high",
            "gender_evidence": (
                f"fide_id={best.get('fide_id', '')}; fide_name={best.get('fide_name', '')}; "
                f"fide_sex={best['fide_sex']}; prior_confidence={best.get('confidence', '')}"
            ),
        }
    return output


FEMALE_FIRST_NAMES = {
    "anna", "anastasia", "alexandra", "aleksandra", "alisa", "aline", "amanda", "amelia",
    "ana", "andrea", "angelina", "antonia", "aria", "arina", "ayelen", "beatriz", "carina",
    "carla", "carmen", "carolina", "catherine", "charlotte", "daria", "diana", "dinara",
    "dorothea", "elena", "elisabeth", "elizabeth", "ella", "emily", "emma", "ester",
    "fatima", "gabriela", "harika", "irina", "jennifer", "jovana", "julia", "katerina",
    "kateryna", "katherine", "laura", "lei", "lena", "liana", "maria", "mariya", "mary",
    "melissa", "monika", "natalia", "natalija", "nino", "nona", "olga", "patricia",
    "polina", "qiyu", "sabrina", "sara", "sarah", "sopiko", "sophia", "sofia", "tan",
    "tania", "tatiana", "tatjana", "valentina", "varvara", "victoria", "viktoria", "wenjun",
    "deysi", "yifan", "zhu", "zoya",
}

MALE_FIRST_NAMES = {
    "aaron", "abhijeet", "abraham", "adham", "aditya", "ahmed", "alex", "alexander",
    "alexandr", "alexei", "alexey", "alireza", "andrew", "andrey", "anton", "antonio",
    "armand", "armen", "arthur", "artur", "avel", "awonder", "benjamin", "boris",
    "brandon", "carlos", "christian", "daniil", "daniel", "david", "denis", "dmitry",
    "dommaraju", "eduardo", "eric", "ernesto", "evgeny", "fabiano", "felix", "gabriel",
    "gadir", "george", "gregory", "hans", "hikaru", "igor", "ivan", "jan", "jason",
    "jeffery", "jorden", "jose", "juan", "julio", "kirill", "krikor", "levon", "liam",
    "magnus", "maxim", "maxime", "michael", "mikhail", "mohamed", "muhammad", "nikita",
    "nikolai", "nils", "nodirbek", "oleksandr", "pavel", "peter", "praggnanandhaa",
    "radoslaw", "rahul", "raunak", "richard", "robert", "sam", "samuel", "sanan",
    "sergey", "shakhriyar", "surya", "tigran", "vasyl", "vidit", "viktor", "vincent",
    "adhiban", "aleksandr", "alojzije", "andriy", "ansumana", "anushurvan",
    "aravindh", "artai", "artemy", "baris", "barte", "bashiq", "baver", "bergson",
    "bharath", "bintang", "borna", "buddika", "cahandar", "can", "chenitha",
    "chris", "christofer", "cristhian", "cristobal", "cyrano", "dalson", "davi",
    "deepan", "eithan", "elamier", "estiven", "fahid", "gara", "gennadii",
    "gahan", "girinath", "gui", "guilio", "harshavardhan", "harut", "helvert",
    "illia", "islom", "jaivvardhan", "janukshan", "jeromino", "jhon", "joerg",
    "jozsef", "karthikeyan", "kasimov", "krishna", "krisna", "laxman", "leykunmesfin",
    "joao", "komal", "licael", "liduino", "maicol", "marceley", "matfei", "matheus", "meftahi",
    "miraziz", "mohamadmiran", "nawin", "nitin", "poobesh", "pranav", "premnath",
    "rathanvel", "rathnakaran", "ricando", "rohith", "romulo", "sadiq", "samarth",
    "sampi", "saptarshi", "sariatullah", "sauravh", "seferov", "sekk", "semetey",
    "shadhursshaan", "silvius", "srinath", "stany", "senthil", "suduva", "udith",
    "viani", "visakh", "vladimir", "volodar", "wellington", "wesley", "willyam",
    "witek", "yagiz", "yan", "yank", "yanki", "yasel", "yashas", "yasser", "yefry",
    "yuniesky", "yu", "yuri", "zakharov",
}

PLAYER_GENDER_OVERRIDES = {
    "Akylbek2021": ("male", "A Daurimbetov is matched to a male-coded Central Asian surname/initial profile."),
    "BashiqChess": ("male", "Md Bashiq Imrose: Md/Bashiq are male-coded names."),
    "Hidden_Dragon": ("male", "Guill. Vallin: Guill. is a male-coded abbreviation of Guillaume/Guillermo."),
    "Kostya0705": ("male", "Konstnatin/Konstantin Andreev is male-coded."),
    "MacDaddyMac": ("male", "Sully McConnell is male-coded in this context."),
    "Mbneu": ("male", "Matthia Bach is male-coded."),
    "McLean12": ("male", "CM McLean Handjaba is male-coded in this context."),
    "Os55555": ("male", "Os Misael King: Misael is male-coded."),
    "Phelathegreat": ("male", "Mabusela Johannes: Johannes is male-coded."),
    "Piotrek1979": ("male", "Piotrek/Piotr Mickiewicz is male-coded."),
    "RobPeresian": ("male", "Robinson Perez is male-coded."),
    "SaiHanThiha": ("male", "Sai Han Thiha is male-coded."),
    "SharapovEvgeny": ("male", "Evgeny is male-coded."),
    "SirParistonHill": ("male", "Pariston Hill is male-coded in this context."),
    "SouS007": ("male", "Lord Adrian Soderstrom: Adrian is male-coded."),
    "TheChessKitchenTwitch": ("male", "Davíð Kjartansson is male-coded."),
    "Treasurechess777": ("male", "Lamel Mc Bryde is male-coded in this context."),
    "Undisputed92": ("male", "Shyaamnikhil P is male-coded."),
    "YoreDea": ("male", "אהרן/Aharon is male-coded."),
    "ashwanitiwari": ("male", "Chess.com/FIDE context identifies this as Ashwani Tiwari; Ashwani is male-coded here."),
    "armago": ("male", "Marichal Gonzalez is male-coded in this chess-player context."),
    "campochess": ("male", "Campo Elias Guzman is male-coded."),
    "catalin009": ("male", "Catalin is male-coded."),
    "copanchess": ("male", "Nilson Cardenas is male-coded."),
    "hasagoodchesser": ("male", "Yankı Taşpınar is male-coded."),
    "kngopal": ("male", "Kavikondala Nagendra Gopal is male-coded."),
    "kyawlayhtaik8": ("male", "Ko Htaik is male-coded in this context."),
    "lupta_robin": ("male", "Dragomirescu Robin is male-coded in this context."),
    "the-do0on": ("male", "مساعد المطيري / Musaad Al-Mutairi is male-coded."),
    "DrWonderKid": ("male", "Barış Çınar Şahbudak is male-coded."),
    "dulerile": ("male", "Nebojsa is male-coded."),
    "Kimmich_Mindset": ("male", "Lukas is male-coded."),
    "Matheusmdr2005": ("male", "Ribeiro Domingues Ribeiro is male-coded in this context."),
    "VKMATTA": ("male", "Vinaykumar is male-coded."),
    "vraghav2010": ("male", "Raghav Vijay is male-coded."),
    "xiaoxuan2012": ("male", "Zhang Haoxuan is male-coded."),
    "yankitaspinar": ("male", "Yankı Taşpınar is male-coded."),
}

FIRST_NAME_SKIP_TOKENS = {
    "c",
    "cm",
    "cro",
    "dr",
    "fm",
    "gm",
    "im",
    "mf",
    "mi",
    "mn",
    "mr",
    "wcm",
    "wfm",
    "wgm",
    "wim",
}


def first_name(real_name: str) -> str:
    text = html.unescape(transliterate_cyrillic(real_name or ""))
    text = text.replace("ı", "i").replace("İ", "I")
    text = unicodedata.normalize("NFKD", text)
    text = "".join(ch for ch in text if not unicodedata.combining(ch))
    text = re.sub(r"\[[^\]]+\]", " ", text)
    text = re.sub(r"[^A-Za-zÀ-ÖØ-öø-ÿĀ-žǍ-ȳ' -]+", " ", text)
    parts = [part.strip(" '-").lower() for part in text.split() if part.strip(" '-")]
    parts = [part for part in parts if part not in FIRST_NAME_SKIP_TOKENS]
    return parts[0] if parts else ""


def name_heuristic_gender(row: dict[str, str]) -> dict[str, str] | None:
    if blank(row.get("real_name")):
        return None
    if row.get("player_name") in PLAYER_GENDER_OVERRIDES:
        gender, evidence = PLAYER_GENDER_OVERRIDES[row["player_name"]]
        return {
            "gender": gender,
            "gender_source": "manual_cultural_name_heuristic",
            "gender_confidence": "medium",
            "gender_evidence": evidence,
        }
    first = first_name(row["real_name"])
    if not first:
        return None
    if gender_detector is not None:
        detector = gender_detector.Detector(case_sensitive=False)
        guessed = detector.get_gender(first)
        if guessed in {"female", "mostly_female"}:
            return {
                "gender": "female",
                "gender_source": "first_name_gender_guesser",
                "gender_confidence": "medium",
                "gender_evidence": f"first_name={first}; gender_guesser={guessed}",
            }
        if guessed in {"male", "mostly_male"}:
            return {
                "gender": "male",
                "gender_source": "first_name_gender_guesser",
                "gender_confidence": "medium",
                "gender_evidence": f"first_name={first}; gender_guesser={guessed}",
            }
    if first in FEMALE_FIRST_NAMES:
        return {
            "gender": "female",
            "gender_source": "first_name_heuristic",
            "gender_confidence": "medium",
            "gender_evidence": f"first_name={first}",
        }
    if first in MALE_FIRST_NAMES:
        return {
            "gender": "male",
            "gender_source": "first_name_heuristic",
            "gender_confidence": "medium",
            "gender_evidence": f"first_name={first}",
        }
    return None


def main() -> None:
    rows, fieldnames = read_csv(PLAYERS)
    title_by_player, title_values_by_player, players_with_w_title = load_titles()

    for row in rows:
        row["player_title"] = title_by_player.get(row["player_name"], "")
        row["player_title_values"] = title_values_by_player.get(row["player_name"], "")

    fide_candidates, sex_by_fide_id = collect_fide_candidates(rows)
    prior_fide_gender = load_prior_fide_gender(rows, sex_by_fide_id)
    fide_findings = []
    all_findings = []

    for idx, row in enumerate(rows):
        finding = title_gender(row, players_with_w_title)
        if finding is None:
            finding = choose_fide_gender(row, fide_candidates.get(idx, []))
            if finding and finding["gender_source"] == "fide_sex":
                fide_findings.append({"player_name": row["player_name"], **finding})
        if finding is None:
            finding = prior_fide_gender.get(row["player_name"])
            if finding:
                fide_findings.append({"player_name": row["player_name"], **finding})
        if finding is None:
            finding = name_heuristic_gender(row)
        if finding is None:
            finding = {
                "gender": "unknown",
                "gender_source": "",
                "gender_confidence": "",
                "gender_evidence": "",
            }
        row.update(finding)
        all_findings.append({"player_name": row["player_name"], **finding})

    new_fields = [
        field
        for field in fieldnames
        if field not in {"player_title", "player_title_values", "gender", "gender_source", "gender_confidence", "gender_evidence"}
    ]
    new_fields += ["player_title", "player_title_values", "gender", "gender_source", "gender_confidence", "gender_evidence"]

    with PLAYERS.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=new_fields)
        writer.writeheader()
        writer.writerows(rows)

    finding_fields = ["player_name", "gender", "gender_source", "gender_confidence", "gender_evidence"]
    with GENDER_FINDINGS.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=finding_fields)
        writer.writeheader()
        writer.writerows(all_findings)
    with FIDE_GENDER_FINDINGS.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=finding_fields)
        writer.writeheader()
        writer.writerows(fide_findings)

    counts = Counter(row["gender"] for row in rows)
    sources = Counter(row["gender_source"] or "unknown" for row in rows)
    print(f"rows written: {len(rows)}")
    print(f"gender counts: {dict(counts)}")
    print(f"gender source counts: {dict(sources)}")
    print(f"wrote {PLAYERS}")
    print(f"wrote {GENDER_FINDINGS}")
    print(f"wrote {FIDE_GENDER_FINDINGS}")


if __name__ == "__main__":
    main()
