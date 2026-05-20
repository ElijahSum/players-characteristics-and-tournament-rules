#!/usr/bin/env python3
import csv
import re
import unicodedata
import xml.etree.ElementTree as ET
import zipfile
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path


BASE_INPUT = Path("players_final_data_merged_with_chesscom_names.csv")
ORIGINAL_INPUT = Path("players_final_data_merged.csv")
CHESSCOM_CACHE = Path("chesscom_profile_cache.jsonl")
FIDE_ZIP = Path("fide_players_list_xml.zip")
FIDE_XML = "players_list_xml_foa.xml"

CHESSCOM_FINDINGS = Path("real_name_findings_chesscom.csv")
BING_FINDINGS = Path("real_name_findings_bing.csv")
FIDE_FINDINGS = Path("real_name_findings_fide_username.csv")
COMBINED_FINDINGS = Path("real_name_findings_combined.csv")
OUTPUT = Path("players_final_data_merged_with_real_names.csv")

FIDE_SOURCE = (
    "FIDE players_list_xml_foa.xml downloaded 2026-05-12 from "
    "https://ratings.fide.com/download/players_list_xml.zip"
)

TITLE_FILTERS = {"GM", "IM", "FM", "CM", "WGM", "WIM", "WFM", "WCM"}
STOP_TOKENS = {
    "gm",
    "im",
    "fm",
    "cm",
    "wgm",
    "wim",
    "wfm",
    "wcm",
    "nm",
    "wnm",
    "chess",
    "jr",
    "sr",
    "ii",
    "iii",
    "iv",
    "v",
}

ISO2_TO_FIDE = {
    "AR": {"ARG"},
    "AM": {"ARM"},
    "AU": {"AUS"},
    "AT": {"AUT"},
    "AZ": {"AZE"},
    "BD": {"BAN"},
    "BE": {"BEL"},
    "BG": {"BUL"},
    "BR": {"BRA"},
    "BY": {"BLR"},
    "CA": {"CAN"},
    "CH": {"SUI"},
    "CL": {"CHI"},
    "CN": {"CHN"},
    "CO": {"COL"},
    "CU": {"CUB"},
    "CZ": {"CZE"},
    "DE": {"GER"},
    "DK": {"DEN"},
    "EC": {"ECU"},
    "EE": {"EST"},
    "EG": {"EGY"},
    "ES": {"ESP"},
    "FI": {"FIN"},
    "FR": {"FRA"},
    "GB": {"ENG", "SCO", "WLS", "GBR"},
    "GE": {"GEO"},
    "GR": {"GRE"},
    "HK": {"HKG"},
    "HR": {"CRO"},
    "HT": {"HAI"},
    "HU": {"HUN"},
    "ID": {"INA"},
    "IL": {"ISR"},
    "IN": {"IND"},
    "IR": {"IRI"},
    "IS": {"ISL"},
    "IT": {"ITA"},
    "JP": {"JPN"},
    "KG": {"KGZ"},
    "KR": {"KOR"},
    "KZ": {"KAZ"},
    "LT": {"LTU"},
    "LV": {"LAT"},
    "MD": {"MDA"},
    "ME": {"MNE"},
    "MK": {"MKD"},
    "MX": {"MEX"},
    "MY": {"MAS"},
    "NG": {"NGR"},
    "NI": {"NCA"},
    "NL": {"NED"},
    "NO": {"NOR"},
    "PA": {"PAN"},
    "PE": {"PER"},
    "PH": {"PHI"},
    "PK": {"PAK"},
    "PL": {"POL"},
    "PT": {"POR"},
    "RO": {"ROU"},
    "RS": {"SRB"},
    "RU": {"RUS", "FID"},
    "SE": {"SWE"},
    "SG": {"SGP"},
    "SI": {"SLO"},
    "SK": {"SVK"},
    "TH": {"THA"},
    "TR": {"TUR"},
    "TW": {"TPE"},
    "UA": {"UKR"},
    "US": {"USA"},
    "UY": {"URU"},
    "UZ": {"UZB"},
    "VE": {"VEN"},
    "VN": {"VIE"},
    "ZA": {"RSA"},
}


@dataclass(frozen=True)
class MissingSpec:
    username: str
    user_alpha: str
    tokens: tuple[str, ...]
    long_tokens: tuple[str, ...]
    single_initials: tuple[str, ...]
    country_codes: frozenset[str]
    chesscom_title: str


@dataclass(frozen=True)
class FideEntry:
    fide_id: str
    raw_name: str
    display_name: str
    country: str
    titles: frozenset[str]
    max_rating: int
    tokens: tuple[str, ...]
    surname_tokens: tuple[str, ...]
    given_tokens: tuple[str, ...]
    variants: frozenset[str]


def blank(value):
    return not (value or "").strip()


def ascii_fold(value):
    value = unicodedata.normalize("NFKD", value)
    return "".join(ch for ch in value if not unicodedata.combining(ch))


def normalize_word(value):
    return ascii_fold(value).lower()


def alpha_only(value):
    return re.sub(r"[^a-z]", "", normalize_word(value))


def split_camel(value):
    value = re.sub(r"([a-z])([A-Z])", r"\1 \2", value)
    value = re.sub(r"([A-Z]+)([A-Z][a-z])", r"\1 \2", value)
    return value


def name_tokens(value):
    return tuple(
        token
        for token in re.findall(r"[a-z]+", normalize_word(value))
        if len(token) >= 2 and token not in STOP_TOKENS
    )


def username_parts(username):
    base = split_camel(username)
    parts = re.findall(r"[A-Za-zÀ-ÖØ-öø-ÿĀ-žǍ-ȳ]+", base)
    folded = [normalize_word(part) for part in parts]
    tokens = tuple(part for part in folded if len(part) >= 2 and part not in STOP_TOKENS)
    long_tokens = tuple(part for part in folded if len(part) >= 3 and part not in STOP_TOKENS)
    single_initials = tuple(part for part in folded if len(part) == 1)
    return tokens, long_tokens, single_initials


def country_from_profile(profile):
    country_url = profile.get("country") or ""
    iso2 = country_url.rsplit("/", 1)[-1].upper()
    return frozenset(ISO2_TO_FIDE.get(iso2, set()))


def read_jsonl_profiles(path):
    import json

    profiles = {}
    if not path.exists():
        return profiles
    with path.open(encoding="utf-8") as fh:
        for line in fh:
            if line.strip():
                item = json.loads(line)
                profiles[item["player_name"]] = item.get("profile") or {}
    return profiles


def read_rows(path):
    with path.open(newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        return list(reader), reader.fieldnames or []


def make_specs(rows, profiles):
    specs = []
    for row in rows:
        if not blank(row.get("real_name")):
            continue
        username = row["player_name"]
        profile = profiles.get(username, {})
        tokens, long_tokens, single_initials = username_parts(username)
        specs.append(
            MissingSpec(
                username=username,
                user_alpha=alpha_only(username),
                tokens=tokens,
                long_tokens=long_tokens,
                single_initials=single_initials,
                country_codes=country_from_profile(profile),
                chesscom_title=(profile.get("title") or "").upper(),
            )
        )
    return specs


def int_field(value):
    try:
        return int(value or 0)
    except ValueError:
        return 0


def display_fide_name(raw):
    raw = re.sub(r"\s+", " ", raw).strip()
    if "," in raw:
        last, first = [part.strip() for part in raw.split(",", 1)]
        return f"{first} {last}".strip()
    return raw


def fide_name_parts(raw):
    if "," in raw:
        last, first = [part.strip() for part in raw.split(",", 1)]
        surname = name_tokens(last)
        given = name_tokens(first)
    else:
        tokens = name_tokens(raw)
        given = tokens[:1]
        surname = tokens[-1:] if len(tokens) > 1 else ()
    return given, surname


def fide_variants(given, surname, all_tokens):
    variants = set()
    if given and surname:
        variants.add("".join(given + surname))
        variants.add("".join(surname + given))
        variants.add(given[0] + surname[-1])
        variants.add(surname[-1] + given[0])
        variants.add(given[0][0] + surname[-1])
        variants.add(surname[-1] + given[0][0])
    if all_tokens:
        variants.add("".join(all_tokens))
    return frozenset(v for v in variants if len(v) >= 4)


def parse_fide_entries():
    by_country = defaultdict(list)
    exact_index = defaultdict(list)
    total_kept = 0
    with zipfile.ZipFile(FIDE_ZIP) as archive:
        with archive.open(FIDE_XML) as fh:
            for _, elem in ET.iterparse(fh, events=("end",)):
                if elem.tag != "player":
                    continue
                fields = {child.tag: (child.text or "").strip() for child in elem}
                raw_name = fields.get("name", "")
                if not re.search(r"[A-Za-zÀ-ÖØ-öø-ÿĀ-žǍ-ȳ]", raw_name):
                    elem.clear()
                    continue
                ratings = [
                    int_field(fields.get("rating")),
                    int_field(fields.get("rapid_rating")),
                    int_field(fields.get("blitz_rating")),
                ]
                titles = frozenset(
                    title.upper()
                    for title in [
                        fields.get("title", ""),
                        fields.get("w_title", ""),
                        fields.get("foa_title", ""),
                    ]
                    if title
                )
                max_rating = max(ratings)
                if max_rating < 1800 and not titles:
                    elem.clear()
                    continue
                all_tokens = name_tokens(raw_name)
                if len(all_tokens) < 2:
                    elem.clear()
                    continue
                given, surname = fide_name_parts(raw_name)
                entry = FideEntry(
                    fide_id=fields.get("fideid", ""),
                    raw_name=raw_name,
                    display_name=display_fide_name(raw_name),
                    country=(fields.get("country") or "").upper(),
                    titles=titles,
                    max_rating=max_rating,
                    tokens=all_tokens,
                    surname_tokens=surname,
                    given_tokens=given,
                    variants=fide_variants(given, surname, all_tokens),
                )
                by_country[entry.country].append(entry)
                for variant in entry.variants:
                    exact_index[variant].append(entry)
                total_kept += 1
                if total_kept % 100000 == 0:
                    print(f"kept {total_kept} FIDE entries")
                elem.clear()
    print(f"indexed {total_kept} FIDE entries")
    return by_country, exact_index


def title_ok(spec, entry):
    if spec.chesscom_title in TITLE_FILTERS:
        return spec.chesscom_title in entry.titles
    return True


def token_matches(user_token, fide_tokens):
    for fide_token in fide_tokens:
        if user_token == fide_token:
            return True
        if len(user_token) >= 4 and fide_token.startswith(user_token):
            return True
    return False


def match_strength(spec, entry):
    if not title_ok(spec, entry):
        return ""
    if spec.country_codes and entry.country not in spec.country_codes:
        return ""
    if spec.user_alpha in entry.variants:
        return "exact_full_name_variant"
    if len(spec.long_tokens) >= 2 and all(token_matches(t, entry.tokens) for t in spec.long_tokens):
        return "all_username_tokens_in_fide_name"
    if spec.single_initials and len(spec.long_tokens) == 1:
        long_token = spec.long_tokens[0]
        initial_set = set(spec.single_initials)
        surname_match = any(token_matches(long_token, (s,)) for s in entry.surname_tokens)
        given_initial_match = any(g[:1] in initial_set for g in entry.given_tokens)
        if surname_match and given_initial_match:
            return "surname_plus_given_initial"
    return ""


def candidate_pool(spec, by_country, exact_index):
    exact = exact_index.get(spec.user_alpha, [])
    if spec.country_codes:
        country_entries = []
        for country in spec.country_codes:
            country_entries.extend(by_country.get(country, []))
        seen = {id(entry) for entry in exact}
        country_entries.extend(entry for entry in country_entries if id(entry) not in seen)
        return exact + country_entries
    if spec.chesscom_title in TITLE_FILTERS:
        return exact
    return exact


def choose_match(spec, matches):
    if not matches:
        return None
    exact = [item for item in matches if item[0] == "exact_full_name_variant"]
    token = [item for item in matches if item[0] == "all_username_tokens_in_fide_name"]
    initial = [item for item in matches if item[0] == "surname_plus_given_initial"]
    for group in (exact, token, initial):
        if not group:
            continue
        names = {entry.display_name for _, entry in group}
        if len(names) == 1:
            best = max((entry for _, entry in group), key=lambda e: e.max_rating)
            return group[0][0], best, len(group)
        if len(group) == 1:
            return group[0][0], group[0][1], 1
    return None


def load_findings(path):
    rows = []
    if not path.exists():
        return rows
    with path.open(newline="", encoding="utf-8") as fh:
        for row in csv.DictReader(fh):
            rows.append(
                {
                    "player_name": row.get("player_name", ""),
                    "real_name": row.get("real_name", ""),
                    "source": row.get("source", ""),
                    "source_type": row.get("source_type", ""),
                    "evidence_title": row.get("evidence_title", ""),
                    "evidence_snippet": row.get("evidence_snippet", ""),
                }
            )
    return rows


def main():
    input_path = BASE_INPUT if BASE_INPUT.exists() else ORIGINAL_INPUT
    rows, fieldnames = read_rows(input_path)
    profiles = read_jsonl_profiles(CHESSCOM_CACHE)
    specs = make_specs(rows, profiles)
    by_country, exact_index = parse_fide_entries()

    findings = []
    for spec in specs:
        matches = []
        seen = set()
        for entry in candidate_pool(spec, by_country, exact_index):
            key = entry.fide_id
            if key in seen:
                continue
            seen.add(key)
            strength = match_strength(spec, entry)
            if strength:
                matches.append((strength, entry))
        chosen = choose_match(spec, matches)
        if not chosen:
            continue
        strength, entry, candidate_count = chosen
        if spec.country_codes:
            confidence = "high"
        elif strength == "exact_full_name_variant" and spec.chesscom_title in TITLE_FILTERS:
            confidence = "medium"
        else:
            continue
        if confidence != "high":
            continue
        findings.append(
            {
                "player_name": spec.username,
                "real_name": entry.display_name,
                "source": FIDE_SOURCE,
                "source_type": "FIDE player list matched to Chess.com username/title/country",
                "evidence_title": f"FIDE ID {entry.fide_id}: {entry.raw_name}",
                "evidence_snippet": (
                    f"match={strength}; confidence={confidence}; "
                    f"candidate_count={candidate_count}; fide_country={entry.country}; "
                    f"fide_titles={','.join(sorted(entry.titles))}; "
                    f"max_rating={entry.max_rating}; chesscom_title={spec.chesscom_title}"
                ),
            }
        )

    with FIDE_FINDINGS.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(
            fh,
            fieldnames=[
                "player_name",
                "real_name",
                "source",
                "source_type",
                "evidence_title",
                "evidence_snippet",
            ],
        )
        writer.writeheader()
        writer.writerows(findings)

    name_by_player = {row["player_name"]: row["real_name"] for row in findings}
    output_rows = []
    for row in rows:
        row = dict(row)
        if blank(row.get("real_name")) and row["player_name"] in name_by_player:
            row["real_name"] = name_by_player[row["player_name"]]
        output_rows.append(row)

    with OUTPUT.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(output_rows)

    combined = (
        load_findings(CHESSCOM_FINDINGS)
        + load_findings(BING_FINDINGS)
        + load_findings(FIDE_FINDINGS)
    )
    with COMBINED_FINDINGS.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(
            fh,
            fieldnames=[
                "player_name",
                "real_name",
                "source",
                "source_type",
                "evidence_title",
                "evidence_snippet",
            ],
        )
        writer.writeheader()
        writer.writerows(combined)

    print(f"missing searched: {len(specs)}")
    print(f"high-confidence FIDE username matches: {len(findings)}")
    print(f"combined findings: {len(combined)}")
    print(f"blank real_name in output: {sum(1 for r in output_rows if blank(r.get('real_name')))}")
    print(f"wrote {FIDE_FINDINGS}")
    print(f"wrote {COMBINED_FINDINGS}")
    print(f"wrote {OUTPUT}")


if __name__ == "__main__":
    main()
