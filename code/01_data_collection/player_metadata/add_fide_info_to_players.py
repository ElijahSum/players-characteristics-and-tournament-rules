#!/usr/bin/env python3
from __future__ import annotations

import csv
import re
import xml.etree.ElementTree as ET
import zipfile
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path

from lookup_missing_birthdays import fide_forms, max_rating, target_forms


PLAYERS = Path("players_final_data_merged.csv")
FIDE_ZIP = Path("fide_players_list_xml.zip")
FIDE_XML = "players_list_xml_foa.xml"

GENDER_FIDE = Path("gender_findings_fide.csv")
MISSING_BIRTHDAYS_FIDE = Path("missing_birthdays_fide_matches.csv")
REAL_NAME_FIDE_FILES = [
    Path("real_name_findings_fide_username.csv"),
    Path("real_name_findings_fide_exact_username.csv"),
]
FIDE_INFO_FINDINGS = Path("fide_info_findings.csv")

PLACEHOLDERS = {"", "Not found", "nan", "NaN", "None"}
FILL_FIELDS = [
    "classic_rating",
    "rapid_rating",
    "blitz_rating",
    "federation",
    "country_name",
    "gdp_per_capita_ppp",
    "gdp_per_capita_ppp_logged",
    "birthday",
]


@dataclass(frozen=True)
class FideEntry:
    fide_id: str
    name: str
    display_name: str
    country: str
    title: str
    rating: str
    rapid_rating: str
    blitz_rating: str
    birthday: str


@dataclass(frozen=True)
class Candidate:
    entry: FideEntry
    source: str
    match_kind: str
    match_strength: int
    match_values: str


def blankish(value: str | None) -> bool:
    return (value or "").strip() in PLACEHOLDERS


def clean_int(value: str | None) -> str:
    text = (value or "").strip()
    if not text:
        return ""
    try:
        number = int(float(text))
    except ValueError:
        return ""
    return str(number) if number else ""


def display_name(raw: str) -> str:
    raw = re.sub(r"\s+", " ", raw or "").strip()
    if "," in raw:
        last, first = [part.strip() for part in raw.split(",", 1)]
        return f"{first} {last}".strip()
    return raw


def fide_title(fields: dict[str, str]) -> str:
    for key in ("title", "w_title", "foa_title", "o_title"):
        value = (fields.get(key) or "").strip()
        if value:
            return value
    return ""


def make_entry(fields: dict[str, str]) -> FideEntry:
    return FideEntry(
        fide_id=fields.get("fideid", ""),
        name=fields.get("name", ""),
        display_name=display_name(fields.get("name", "")),
        country=(fields.get("country") or "").strip().upper(),
        title=fide_title(fields),
        rating=clean_int(fields.get("rating")),
        rapid_rating=clean_int(fields.get("rapid_rating")),
        blitz_rating=clean_int(fields.get("blitz_rating")),
        birthday=clean_int(fields.get("birthday")),
    )


def read_players() -> tuple[list[dict[str, str]], list[str]]:
    with PLAYERS.open(newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        return list(reader), reader.fieldnames or []


def load_country_metadata(rows: list[dict[str, str]]) -> dict[str, dict[str, str]]:
    meta: dict[str, dict[str, str]] = {}
    for row in rows:
        code = (row.get("federation") or "").strip()
        if not code or code == "Not found":
            continue
        item = meta.setdefault(code, {})
        for field in ("country_name", "gdp_per_capita_ppp", "gdp_per_capita_ppp_logged"):
            if not item.get(field) and not blankish(row.get(field)):
                item[field] = row[field]
    return meta


def add_id_source(id_sources: dict[str, list[tuple[str, str]]], player: str, fide_id: str, source: str) -> None:
    if player and fide_id:
        id_sources[player].append((fide_id, source))


def load_id_sources() -> dict[str, list[tuple[str, str]]]:
    id_sources: dict[str, list[tuple[str, str]]] = defaultdict(list)

    if GENDER_FIDE.exists():
        with GENDER_FIDE.open(newline="", encoding="utf-8") as fh:
            for row in csv.DictReader(fh):
                match = re.search(r"fide_id=([^;]+)", row.get("gender_evidence", ""))
                if match:
                    add_id_source(id_sources, row["player_name"], match.group(1), "gender_findings_fide")

    if MISSING_BIRTHDAYS_FIDE.exists():
        with MISSING_BIRTHDAYS_FIDE.open(newline="", encoding="utf-8") as fh:
            for row in csv.DictReader(fh):
                if row.get("confidence") in {"high_unique_country", "high_same_year_country"}:
                    add_id_source(
                        id_sources,
                        row["player_name"],
                        row.get("fide_id", ""),
                        f"missing_birthdays_fide_matches:{row.get('confidence', '')}",
                    )

    for path in REAL_NAME_FIDE_FILES:
        if not path.exists():
            continue
        with path.open(newline="", encoding="utf-8") as fh:
            for row in csv.DictReader(fh):
                match = re.search(r"FIDE ID ([^:]+):", row.get("evidence_title", ""))
                if match:
                    add_id_source(id_sources, row["player_name"], match.group(1), path.name)

    return id_sources


def needs_fide_fill(row: dict[str, str]) -> bool:
    return bool((row.get("real_name") or "").strip()) and any(blankish(row.get(field)) for field in FILL_FIELDS)


def build_name_indexes(rows: list[dict[str, str]]) -> tuple[dict[str, set[int]], dict[str, set[int]]]:
    exact_index: dict[str, set[int]] = defaultdict(set)
    first_last_index: dict[str, set[int]] = defaultdict(set)
    for idx, row in enumerate(rows):
        if not needs_fide_fill(row):
            continue
        forms = target_forms(row["real_name"])
        for form in forms["exact"] | forms["swapped"]:
            exact_index[form].add(idx)
        for form in forms["first_last"]:
            first_last_index[form].add(idx)
    return exact_index, first_last_index


def collect_candidates(
    rows: list[dict[str, str]],
    id_sources: dict[str, list[tuple[str, str]]],
) -> dict[int, list[Candidate]]:
    needed_ids = {fide_id for row in rows for fide_id, _ in id_sources.get(row["player_name"], [])}
    exact_index, first_last_index = build_name_indexes(rows)
    candidates: dict[int, list[Candidate]] = defaultdict(list)
    parsed = 0

    with zipfile.ZipFile(FIDE_ZIP) as archive:
        with archive.open(FIDE_XML) as fh:
            for _, elem in ET.iterparse(fh, events=("end",)):
                if elem.tag != "player":
                    continue
                parsed += 1
                fields = {child.tag: (child.text or "").strip() for child in elem}
                entry = make_entry(fields)

                if entry.fide_id in needed_ids:
                    for idx, row in enumerate(rows):
                        for fide_id, source in id_sources.get(row["player_name"], []):
                            if fide_id == entry.fide_id and needs_fide_fill(row):
                                candidates[idx].append(
                                    Candidate(entry, source, "known_fide_id", 4, entry.fide_id)
                                )

                forms = fide_forms(entry.name)
                for form in forms["exact"]:
                    for row_idx in exact_index.get(form, ()):
                        candidates[row_idx].append(
                            Candidate(entry, "fide_name_exact", "exact_full_name", 3, form)
                        )

                # Partial first/last matches are only considered later if they
                # have supporting country, birthday, title, or rating evidence.
                for form in forms["first_last"] | forms["exact"]:
                    for row_idx in first_last_index.get(form, ()):
                        candidates[row_idx].append(
                            Candidate(entry, "fide_name_first_last", "first_last_partial", 1, form)
                        )

                if parsed % 250000 == 0:
                    print(f"parsed {parsed} FIDE players", flush=True)
                elem.clear()
    print(f"parsed {parsed} FIDE players", flush=True)
    return candidates


def row_year(value: str | None) -> str:
    return clean_int(value)


def row_rating_values(row: dict[str, str]) -> list[int]:
    values = []
    for field in ("classic_rating", "rapid_rating", "blitz_rating"):
        value = clean_int(row.get(field))
        if value:
            values.append(int(value))
    return values


def support_score(row: dict[str, str], candidate: Candidate) -> tuple[int, list[str]]:
    entry = candidate.entry
    supports = []
    if not blankish(row.get("federation")) and row["federation"].strip().upper() == entry.country:
        supports.append("country")
    if row_year(row.get("birthday")) and row_year(row.get("birthday")) == entry.birthday:
        supports.append("birthday")
    title = (row.get("player_title") or "").strip().upper()
    if title and title != "NO TITLE" and entry.title and title == entry.title.upper():
        supports.append("title")
    fide_ratings = [int(value) for value in (entry.rating, entry.rapid_rating, entry.blitz_rating) if value]
    if fide_ratings and any(abs(player_rating - fide_rating) <= 10 for player_rating in row_rating_values(row) for fide_rating in fide_ratings):
        supports.append("rating")
    if candidate.match_kind == "known_fide_id":
        supports.append("known_fide_id")
    return len(set(supports)), sorted(set(supports))


def choose_candidate(row: dict[str, str], candidates: list[Candidate]) -> Candidate | None:
    unique: dict[tuple[str, str], Candidate] = {}
    for candidate in candidates:
        unique[(candidate.entry.fide_id, candidate.match_kind)] = candidate
    candidates = list(unique.values())
    if not candidates:
        return None

    known = [candidate for candidate in candidates if candidate.match_kind == "known_fide_id"]
    if known:
        scored = sorted(
            known,
            key=lambda candidate: (support_score(row, candidate)[0], max_rating_value(candidate.entry)),
            reverse=True,
        )
        return scored[0]

    exact = [candidate for candidate in candidates if candidate.match_kind == "exact_full_name"]
    if exact:
        supported = [candidate for candidate in exact if support_score(row, candidate)[0] > 0]
        pool = supported or exact
        countries = {candidate.entry.country for candidate in pool}
        birthdays = {candidate.entry.birthday for candidate in pool if candidate.entry.birthday}
        if len(pool) == 1 or len(countries) == 1 or (birthdays and len(birthdays) == 1):
            return sorted(
                pool,
                key=lambda candidate: (support_score(row, candidate)[0], max_rating_value(candidate.entry)),
                reverse=True,
            )[0]

    supported_partial = [
        candidate
        for candidate in candidates
        if candidate.match_kind == "first_last_partial" and support_score(row, candidate)[0] > 0
    ]
    if supported_partial:
        ids = {candidate.entry.fide_id for candidate in supported_partial}
        if len(ids) == 1:
            return supported_partial[0]
        sexes_or_countries = {candidate.entry.country for candidate in supported_partial}
        if len(sexes_or_countries) == 1:
            return sorted(
                supported_partial,
                key=lambda candidate: (support_score(row, candidate)[0], max_rating_value(candidate.entry)),
                reverse=True,
            )[0]
    return None


def max_rating_value(entry: FideEntry) -> int:
    return max(int(value) for value in [entry.rating or "0", entry.rapid_rating or "0", entry.blitz_rating or "0"])


def updates_from_entry(row: dict[str, str], entry: FideEntry, country_meta: dict[str, dict[str, str]]) -> dict[str, str]:
    meta = country_meta.get(entry.country, {})
    proposed = {
        "classic_rating": entry.rating,
        "rapid_rating": entry.rapid_rating,
        "blitz_rating": entry.blitz_rating,
        "federation": entry.country,
        "country_name": meta.get("country_name", ""),
        "gdp_per_capita_ppp": meta.get("gdp_per_capita_ppp", ""),
        "gdp_per_capita_ppp_logged": meta.get("gdp_per_capita_ppp_logged", ""),
        "birthday": entry.birthday,
    }
    return {
        field: value
        for field, value in proposed.items()
        if value and blankish(row.get(field))
    }


def main() -> None:
    rows, fieldnames = read_players()
    country_meta = load_country_metadata(rows)
    id_sources = load_id_sources()
    before_blank = {
        field: sum(blankish(row.get(field)) for row in rows)
        for field in FILL_FIELDS
    }

    candidates = collect_candidates(rows, id_sources)
    finding_rows = []
    updated_rows = 0
    updated_cells = 0

    for idx, row in enumerate(rows):
        if not needs_fide_fill(row):
            continue
        chosen = choose_candidate(row, candidates.get(idx, []))
        if not chosen:
            continue
        updates = updates_from_entry(row, chosen.entry, country_meta)
        if not updates:
            continue
        support_count, supports = support_score(row, chosen)
        for field, value in updates.items():
            row[field] = value
        updated_rows += 1
        updated_cells += len(updates)
        finding_rows.append(
            {
                "player_name": row["player_name"],
                "real_name": row.get("real_name", ""),
                "fide_id": chosen.entry.fide_id,
                "fide_name": chosen.entry.display_name,
                "fide_country": chosen.entry.country,
                "fide_title": chosen.entry.title,
                "fide_standard_rating": chosen.entry.rating,
                "fide_rapid_rating": chosen.entry.rapid_rating,
                "fide_blitz_rating": chosen.entry.blitz_rating,
                "fide_birthday": chosen.entry.birthday,
                "match_source": chosen.source,
                "match_kind": chosen.match_kind,
                "match_values": chosen.match_values,
                "support": ";".join(supports),
                "updated_fields": ";".join(updates),
            }
        )

    with PLAYERS.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)

    with FIDE_INFO_FINDINGS.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(
            fh,
            fieldnames=[
                "player_name",
                "real_name",
                "fide_id",
                "fide_name",
                "fide_country",
                "fide_title",
                "fide_standard_rating",
                "fide_rapid_rating",
                "fide_blitz_rating",
                "fide_birthday",
                "match_source",
                "match_kind",
                "match_values",
                "support",
                "updated_fields",
            ],
        )
        writer.writeheader()
        writer.writerows(finding_rows)

    after_blank = {
        field: sum(blankish(row.get(field)) for row in rows)
        for field in FILL_FIELDS
    }
    print(f"rows updated: {updated_rows}")
    print(f"cells updated: {updated_cells}")
    print(f"blank counts before: {before_blank}")
    print(f"blank counts after: {after_blank}")
    print(f"wrote {PLAYERS}")
    print(f"wrote {FIDE_INFO_FINDINGS}")


if __name__ == "__main__":
    main()
