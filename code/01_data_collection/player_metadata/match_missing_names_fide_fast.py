#!/usr/bin/env python3
import csv
import xml.etree.ElementTree as ET
import zipfile
from collections import defaultdict
from pathlib import Path

from match_missing_names_fide import (
    BING_FINDINGS,
    CHESSCOM_FINDINGS,
    CHESSCOM_CACHE,
    FIDE_FINDINGS,
    FIDE_SOURCE,
    FIDE_XML,
    FIDE_ZIP,
    FideEntry,
    TITLE_FILTERS,
    blank,
    choose_match,
    display_fide_name,
    fide_name_parts,
    fide_variants,
    int_field,
    load_findings,
    make_specs,
    match_strength,
    name_tokens,
    read_jsonl_profiles,
    read_rows,
)


BASE_INPUT = Path("players_final_data_merged_with_chesscom_names.csv")
HTML_FINDINGS = Path("real_name_findings_chesscom_html.csv")
DDG_FINDINGS = Path("real_name_findings_ddg.csv")
FIDE_EXACT_FINDINGS = Path("real_name_findings_fide_exact_username.csv")
COMBINED_FINDINGS = Path("real_name_findings_combined.csv")
OUTPUT = Path("players_final_data_merged_with_real_names.csv")


def iter_fide_entries():
    parsed = 0
    kept = 0
    with zipfile.ZipFile(FIDE_ZIP) as archive:
        with archive.open(FIDE_XML) as fh:
            for _, elem in ET.iterparse(fh, events=("end",)):
                if elem.tag != "player":
                    continue
                parsed += 1
                fields = {child.tag: (child.text or "").strip() for child in elem}
                raw_name = fields.get("name", "")
                all_tokens = name_tokens(raw_name)
                if len(all_tokens) >= 2:
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
                    if max_rating >= 1800 or titles:
                        given, surname = fide_name_parts(raw_name)
                        kept += 1
                        yield FideEntry(
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
                if parsed % 100000 == 0:
                    print(f"parsed {parsed} FIDE players; kept {kept}", flush=True)
                elem.clear()
    print(f"parsed {parsed} FIDE players; kept {kept}", flush=True)


def build_spec_indexes(specs):
    exact = defaultdict(set)
    by_country_prefix = defaultdict(lambda: defaultdict(set))
    for spec in specs:
        if spec.user_alpha:
            exact[spec.user_alpha].add(spec)
        for country in spec.country_codes:
            for token in spec.long_tokens:
                by_country_prefix[country][token].add(spec)
    return exact, by_country_prefix


def candidate_specs(entry, exact_index, prefix_index):
    candidates = set()
    for variant in entry.variants:
        candidates.update(exact_index.get(variant, ()))
    country_prefixes = prefix_index.get(entry.country, {})
    if country_prefixes:
        for token in entry.tokens:
            for length in range(3, len(token) + 1):
                candidates.update(country_prefixes.get(token[:length], ()))
    return candidates


def dedupe_findings(rows):
    out = []
    seen = set()
    for row in rows:
        player = row.get("player_name", "")
        if not player or player in seen:
            continue
        seen.add(player)
        out.append(row)
    return out


def title_compatible(spec, entry):
    if not spec.chesscom_title:
        return True
    if spec.chesscom_title == "NM":
        return True
    if spec.chesscom_title in TITLE_FILTERS:
        return spec.chesscom_title in entry.titles
    return False


def main():
    rows, fieldnames = read_rows(BASE_INPUT)
    profiles = read_jsonl_profiles(CHESSCOM_CACHE)
    specs = make_specs(rows, profiles)
    exact_index, prefix_index = build_spec_indexes(specs)
    matches_by_spec = defaultdict(list)
    exact_matches_by_spec = defaultdict(list)

    for entry in iter_fide_entries():
        for variant in entry.variants:
            for spec in exact_index.get(variant, ()):
                if title_compatible(spec, entry):
                    exact_matches_by_spec[spec].append(entry)
        for spec in candidate_specs(entry, exact_index, prefix_index):
            strength = match_strength(spec, entry)
            if strength:
                matches_by_spec[spec].append((strength, entry))

    findings = []
    for spec in specs:
        chosen = choose_match(spec, matches_by_spec.get(spec, []))
        if not chosen:
            continue
        strength, entry, candidate_count = chosen
        if not spec.country_codes:
            continue
        findings.append(
            {
                "player_name": spec.username,
                "real_name": entry.display_name,
                "source": FIDE_SOURCE,
                "source_type": "FIDE player list matched to Chess.com username/title/country",
                "evidence_title": f"FIDE ID {entry.fide_id}: {entry.raw_name}",
                "evidence_snippet": (
                    f"match={strength}; confidence=high; "
                    f"candidate_count={candidate_count}; fide_country={entry.country}; "
                    f"fide_titles={','.join(sorted(entry.titles))}; "
                    f"max_rating={entry.max_rating}; chesscom_title={spec.chesscom_title}"
                ),
            }
        )

    exact_findings = []
    strict_players = {row["player_name"] for row in findings}
    for spec in specs:
        if spec.username in strict_players:
            continue
        entries = exact_matches_by_spec.get(spec, [])
        if not entries:
            continue
        names = {entry.display_name for entry in entries}
        if len(names) != 1:
            continue
        entry = max(entries, key=lambda item: item.max_rating)
        exact_findings.append(
            {
                "player_name": spec.username,
                "real_name": entry.display_name,
                "source": FIDE_SOURCE,
                "source_type": "FIDE player list exact unique match to Chess.com username",
                "evidence_title": f"FIDE ID {entry.fide_id}: {entry.raw_name}",
                "evidence_snippet": (
                    "match=exact_full_name_variant; confidence=medium_high; "
                    f"candidate_count={len(entries)}; fide_country={entry.country}; "
                    f"fide_titles={','.join(sorted(entry.titles))}; "
                    f"max_rating={entry.max_rating}; chesscom_title={spec.chesscom_title}; "
                    f"chesscom_country_candidates={','.join(sorted(spec.country_codes))}"
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

    with FIDE_EXACT_FINDINGS.open("w", newline="", encoding="utf-8") as fh:
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
        writer.writerows(exact_findings)

    combined = dedupe_findings(
        load_findings(CHESSCOM_FINDINGS)
        + load_findings(BING_FINDINGS)
        + load_findings(DDG_FINDINGS)
        + load_findings(HTML_FINDINGS)
        + findings
        + exact_findings
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

    name_by_player = {row["player_name"]: row["real_name"] for row in combined}
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

    print(f"missing searched: {len(specs)}")
    print(f"high-confidence FIDE username matches: {len(findings)}")
    print(f"exact unique FIDE username matches: {len(exact_findings)}")
    print(f"combined findings: {len(combined)}")
    print(f"blank real_name in output: {sum(1 for row in output_rows if blank(row.get('real_name')))}")
    print(f"wrote {FIDE_FINDINGS}")
    print(f"wrote {FIDE_EXACT_FINDINGS}")
    print(f"wrote {COMBINED_FINDINGS}")
    print(f"wrote {OUTPUT}")


if __name__ == "__main__":
    main()
