#!/usr/bin/env python3
"""Second-pass lookup for unresolved birth years using Wikidata.

This script is intentionally conservative: it only accepts Wikidata entities
with a matching name, a birth-date claim, and a chess-specific signal.
"""

from __future__ import annotations

import argparse
import csv
import json
import re
import subprocess
import time
import unicodedata
import urllib.parse
from pathlib import Path


SEARCH_API = "https://www.wikidata.org/w/api.php?action=wbsearchentities"
ENTITY_API = "https://www.wikidata.org/w/api.php?action=wbgetentities"
USER_AGENT = "search-for-players birth-year research/0.2"
CHESS_QID = "Q718"


def strip_accents(value: str) -> str:
    value = unicodedata.normalize("NFKD", value)
    return "".join(ch for ch in value if not unicodedata.combining(ch))


def normalized(value: str) -> str:
    value = strip_accents(value or "").lower()
    value = re.sub(r"\[[^\]]+\]", " ", value)
    value = value.replace("ø", "o").replace("ł", "l").replace("đ", "d")
    value = re.sub(r"[^a-z0-9а-яёїієґ -]+", " ", value)
    value = value.replace("-", " ")
    return re.sub(r"\s+", " ", value).strip()


def name_forms(value: str) -> set[str]:
    n = normalized(value)
    forms = {n} if n else set()
    parts = n.split()
    if len(parts) >= 2:
        forms.add(" ".join([parts[-1], *parts[:-1]]))
        forms.add(f"{parts[0]} {parts[-1]}")
        forms.add(f"{parts[-1]} {parts[0]}")
    return forms


def curl_json(url: str) -> dict:
    result = subprocess.run(
        ["curl", "-sS", "-L", "-H", f"User-Agent: {USER_AGENT}", url],
        check=True,
        text=True,
        capture_output=True,
    )
    return json.loads(result.stdout)


def search_name(name: str, limit: int = 7) -> list[dict]:
    params = {
        "search": name,
        "language": "en",
        "format": "json",
        "limit": str(limit),
    }
    url = SEARCH_API + "&" + urllib.parse.urlencode(params)
    payload = curl_json(url)
    return payload.get("search", [])


def fetch_entities(qids: list[str]) -> dict:
    if not qids:
        return {}
    params = {
        "ids": "|".join(qids),
        "props": "claims|labels|descriptions|aliases",
        "languages": "en",
        "format": "json",
    }
    url = ENTITY_API + "&" + urllib.parse.urlencode(params)
    return curl_json(url).get("entities", {})


def claim_values(entity: dict, prop: str) -> list:
    values = []
    for claim in entity.get("claims", {}).get(prop, []):
        datavalue = claim.get("mainsnak", {}).get("datavalue", {})
        if "value" in datavalue:
            values.append(datavalue["value"])
    return values


def birth_year(entity: dict) -> str:
    for value in claim_values(entity, "P569"):
        if isinstance(value, dict):
            text = value.get("time", "")
            match = re.match(r"^[+]?(\d{4})-", text)
            if match:
                return match.group(1)
    return ""


def entity_name_forms(entity: dict) -> set[str]:
    values = []
    label = entity.get("labels", {}).get("en", {}).get("value")
    if label:
        values.append(label)
    for alias in entity.get("aliases", {}).get("en", []):
        if alias.get("value"):
            values.append(alias["value"])
    forms = set()
    for value in values:
        forms |= name_forms(value)
    return forms


def chess_signal(search_result: dict, entity: dict) -> str:
    description = (
        entity.get("descriptions", {}).get("en", {}).get("value")
        or search_result.get("description")
        or ""
    ).lower()
    if "chess" in description:
        return "description_contains_chess"
    for value in claim_values(entity, "P641"):
        if isinstance(value, dict) and value.get("id") == CHESS_QID:
            return "sport_chess_claim"
    return ""


def exact_name_match(real_name: str, entity: dict, search_result: dict) -> tuple[bool, str]:
    target_forms = name_forms(real_name)
    entity_forms = entity_name_forms(entity)
    overlap = target_forms & entity_forms
    if overlap:
        return True, ";".join(sorted(overlap))
    match_text = (search_result.get("match") or {}).get("text") or ""
    overlap = target_forms & name_forms(match_text)
    if overlap:
        return True, ";".join(sorted(overlap))
    return False, ""


def load_unresolved(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as fh:
        return list(csv.DictReader(fh))


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--unresolved", type=Path, default=Path("missing_birthdays_unresolved.csv"))
    parser.add_argument("--summary", type=Path, default=Path("missing_birthdays_lookup_summary.csv"))
    parser.add_argument("--found", type=Path, default=Path("missing_birthdays_found.csv"))
    parser.add_argument("--filled", type=Path, default=Path("players_final_data_merged_birthdays_filled.csv"))
    parser.add_argument("--wikidata-found", type=Path, default=Path("missing_birthdays_wikidata_found.csv"))
    parser.add_argument("--combined-summary", type=Path, default=Path("missing_birthdays_lookup_summary_combined.csv"))
    parser.add_argument("--combined-found", type=Path, default=Path("missing_birthdays_found_combined.csv"))
    parser.add_argument("--combined-unresolved", type=Path, default=Path("missing_birthdays_unresolved_combined.csv"))
    parser.add_argument("--combined-filled", type=Path, default=Path("players_final_data_merged_birthdays_filled_combined.csv"))
    parser.add_argument("--sleep", type=float, default=0.12)
    args = parser.parse_args()

    unresolved = load_unresolved(args.unresolved)
    findings = []
    for idx, row in enumerate(unresolved, 1):
        real_name = row["real_name"]
        try:
            results = search_name(real_name)
        except Exception as exc:
            print(f"search failed {idx}/{len(unresolved)} {real_name}: {exc}")
            time.sleep(args.sleep)
            continue
        qids = [item["id"] for item in results if item.get("id")]
        try:
            entities = fetch_entities(qids)
        except Exception as exc:
            print(f"entity fetch failed {idx}/{len(unresolved)} {real_name}: {exc}")
            time.sleep(args.sleep)
            continue

        accepted = []
        by_qid = {item.get("id"): item for item in results}
        for qid, entity in entities.items():
            if entity.get("missing"):
                continue
            year = birth_year(entity)
            if not year:
                continue
            matched, match_values = exact_name_match(real_name, entity, by_qid.get(qid, {}))
            if not matched:
                continue
            signal = chess_signal(by_qid.get(qid, {}), entity)
            if not signal:
                continue
            accepted.append((qid, entity, year, signal, match_values))

        if len(accepted) == 1:
            qid, entity, year, signal, match_values = accepted[0]
            label = entity.get("labels", {}).get("en", {}).get("value", "")
            description = entity.get("descriptions", {}).get("en", {}).get("value", "")
            findings.append(
                {
                    "csv_line": row["csv_line"],
                    "player_name": row["player_name"],
                    "real_name": real_name,
                    "dataset_federation": row["dataset_federation"],
                    "dataset_country_name": row["dataset_country_name"],
                    "found_birth_year": year,
                    "status": "found_medium_confidence_wikidata",
                    "source": f"https://www.wikidata.org/wiki/{qid}",
                    "wikidata_qid": qid,
                    "wikidata_label": label,
                    "wikidata_description": description,
                    "wikidata_signal": signal,
                    "match_values": match_values,
                }
            )
        elif len(accepted) > 1:
            print(f"multiple Wikidata candidates for {row['csv_line']} {real_name}: {[item[0] for item in accepted]}")

        if idx % 50 == 0:
            print(f"searched {idx}/{len(unresolved)}; accepted {len(findings)}")
        time.sleep(args.sleep)

    wd_fields = [
        "csv_line",
        "player_name",
        "real_name",
        "dataset_federation",
        "dataset_country_name",
        "found_birth_year",
        "status",
        "source",
        "wikidata_qid",
        "wikidata_label",
        "wikidata_description",
        "wikidata_signal",
        "match_values",
    ]
    with args.wikidata_found.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=wd_fields)
        writer.writeheader()
        writer.writerows(findings)

    by_line = {row["csv_line"]: row for row in findings}
    with args.summary.open(newline="", encoding="utf-8") as fh:
        summary_reader = csv.DictReader(fh)
        summary_fields = summary_reader.fieldnames or []
        summary_rows = list(summary_reader)

    combined_rows = []
    for row in summary_rows:
        if row["csv_line"] in by_line and not row["status"].startswith("found_"):
            wd = by_line[row["csv_line"]]
            row = dict(row)
            row["found_birth_year"] = wd["found_birth_year"]
            row["status"] = wd["status"]
            row["source"] = wd["source"]
            row["fide_id"] = ""
            row["fide_name"] = ""
            row["fide_country"] = ""
            row["fide_title"] = ""
            row["fide_max_rating"] = ""
            row["match_kind"] = wd["wikidata_signal"]
        combined_rows.append(row)

    with args.combined_summary.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=summary_fields)
        writer.writeheader()
        writer.writerows(combined_rows)

    for path, rows in (
        (args.combined_found, [row for row in combined_rows if row["status"].startswith("found_")]),
        (args.combined_unresolved, [row for row in combined_rows if not row["status"].startswith("found_")]),
    ):
        with path.open("w", newline="", encoding="utf-8") as fh:
            writer = csv.DictWriter(fh, fieldnames=summary_fields)
            writer.writeheader()
            writer.writerows(rows)

    with args.filled.open(newline="", encoding="utf-8") as fh:
        filled_reader = csv.DictReader(fh)
        filled_fields = filled_reader.fieldnames or []
        filled_rows = list(filled_reader)

    for idx, row in enumerate(filled_rows, start=2):
        wd = by_line.get(str(idx))
        if wd and not (row.get("birthday_lookup_status") or "").startswith("found_"):
            row["birthday"] = wd["found_birth_year"]
            row["birthday_lookup_status"] = wd["status"]
            row["birthday_source"] = wd["source"]
            row["birthday_fide_id"] = ""
            row["birthday_fide_name"] = wd["wikidata_label"]

    with args.combined_filled.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=filled_fields)
        writer.writeheader()
        writer.writerows(filled_rows)

    print(f"wikidata found: {len(findings)}")
    print(f"wrote {args.wikidata_found}")
    print(f"wrote {args.combined_summary}")
    print(f"wrote {args.combined_found}")
    print(f"wrote {args.combined_unresolved}")
    print(f"wrote {args.combined_filled}")


if __name__ == "__main__":
    main()
