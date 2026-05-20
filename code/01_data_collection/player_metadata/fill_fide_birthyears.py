#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import re
import unicodedata
import xml.etree.ElementTree as ET
import zipfile
from collections import Counter, defaultdict
from dataclasses import dataclass
from itertools import product
from pathlib import Path


INPUT_CSV = Path("data/players_final_data.csv")
OUTPUT_CSV = Path("outputs/players_final_data_with_birthyears.csv")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Fill missing player birth years from the official FIDE export."
    )
    parser.add_argument(
        "--input",
        type=Path,
        default=INPUT_CSV,
        help=f"Input CSV path (default: {INPUT_CSV})",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=OUTPUT_CSV,
        help=f"Output CSV path (default: {OUTPUT_CSV})",
    )
    parser.add_argument(
        "--fide-xml-zip",
        type=Path,
        default=Path("/tmp/players_list_xml.zip"),
        help="Path to the official FIDE XML ZIP export.",
    )
    return parser.parse_args()


def parse_rating(value: str | None) -> int | None:
    if value is None:
        return None
    value = value.strip()
    if not value:
        return None
    try:
        return int(float(value))
    except ValueError:
        return None


def collapse_spaces(value: str) -> str:
    return re.sub(r"\s+", " ", value).strip()


def normalize_name(value: str | None) -> str:
    if not value:
        return ""
    value = unicodedata.normalize("NFKD", value)
    value = "".join(ch for ch in value if not unicodedata.combining(ch))
    value = value.casefold()
    value = value.replace(",", " ")
    value = re.sub(r"[^0-9\w\s]", " ", value, flags=re.UNICODE)
    return collapse_spaces(value)


def has_letter(value: str) -> bool:
    return any(ch.isalpha() for ch in value)


CYRILLIC_TO_LATIN = {
    "а": ("a",),
    "б": ("b",),
    "в": ("v",),
    "г": ("g",),
    "д": ("d",),
    "е": ("e",),
    "ё": ("e", "yo"),
    "ж": ("zh",),
    "з": ("z",),
    "и": ("i",),
    "й": ("i", "y"),
    "к": ("k",),
    "л": ("l",),
    "м": ("m",),
    "н": ("n",),
    "о": ("o",),
    "п": ("p",),
    "р": ("r",),
    "с": ("s",),
    "т": ("t",),
    "у": ("u",),
    "ф": ("f",),
    "х": ("kh", "h"),
    "ц": ("ts",),
    "ч": ("ch",),
    "ш": ("sh",),
    "щ": ("shch", "sch"),
    "ы": ("y",),
    "э": ("e",),
    "ю": ("yu", "iu"),
    "я": ("ya", "ia"),
    "ь": ("",),
    "ъ": ("",),
    "і": ("i",),
    "ї": ("i", "yi"),
    "є": ("e", "ye"),
    "ґ": ("g",),
    "ў": ("u",),
}


def contains_cyrillic(value: str) -> bool:
    return any("CYRILLIC" in unicodedata.name(ch, "") for ch in value)


def expand_latinized_word(word: str) -> set[str]:
    variants = {word}
    queue = [word]

    while queue and len(variants) < 16:
        current = queue.pop()
        generated = set()
        if current.startswith("ye"):
            generated.add("e" + current[2:])
        if current.startswith("yu"):
            generated.add("iu" + current[2:])
        if current.startswith("ya"):
            generated.add("ia" + current[2:])
        if current.endswith("ii"):
            generated.add(current[:-2] + "iy")
            generated.add(current[:-2] + "y")
        if current.endswith("ei"):
            generated.add(current[:-2] + "ey")
        if current.endswith("yi"):
            generated.add(current[:-2] + "y")
        for item in generated:
            if item not in variants:
                variants.add(item)
                queue.append(item)

    return variants


def transliterate_cyrillic_variants(value: str) -> set[str]:
    tokens = re.findall(r"[^\W\d_]+|\d+|\s+|[^\w\s]", value, flags=re.UNICODE)
    per_token_variants: list[list[str]] = []

    for token in tokens:
        if all("CYRILLIC" in unicodedata.name(ch, "") for ch in token if ch.isalpha()):
            letter_choices = [CYRILLIC_TO_LATIN.get(ch.casefold(), (ch.casefold(),)) for ch in token]
            combined = {"".join(parts) for parts in product(*letter_choices)}
            expanded = set()
            for item in combined:
                expanded.update(expand_latinized_word(item))
            per_token_variants.append(sorted(expanded)[:16])
        else:
            per_token_variants.append([token])

    variants: set[str] = set()
    for parts in product(*per_token_variants):
        raw = "".join(parts)
        normalized = normalize_name(raw)
        if normalized:
            variants.add(normalized)
        if len(variants) >= 64:
            break
    return variants


def lookup_name_variants(value: str | None) -> tuple[str, ...]:
    if not value:
        return ()
    variants = set()
    normalized = normalize_name(value)
    if normalized:
        variants.add(normalized)
    if contains_cyrillic(value):
        variants.update(transliterate_cyrillic_variants(value))
    variants = {item for item in variants if has_letter(item)}
    return tuple(sorted(variants))


def fide_name_variants(name: str) -> set[str]:
    variants = set()
    normalized = normalize_name(name)
    if normalized:
        variants.add(normalized)

    if "," in name:
        left, right = [part.strip() for part in name.split(",", 1)]
        reversed_name = normalize_name(f"{right} {left}")
        if reversed_name:
            variants.add(reversed_name)
    return variants


@dataclass(frozen=True)
class PendingRow:
    index: int
    pattern: str
    name_variants: tuple[str, ...]
    federation: str
    classic: int | None
    rapid: int | None
    blitz: int | None


@dataclass(frozen=True)
class Candidate:
    name: str
    name_variants: tuple[str, ...]
    birthday: str


def select_pattern(
    name_variants: tuple[str, ...],
    federation: str,
    classic: int | None,
    rapid: int | None,
    blitz: int | None,
) -> tuple[str | None, tuple]:
    name_norm = name_variants[0] if name_variants else ""
    if federation and classic is not None and rapid is not None and blitz is not None:
        return "full", (federation, classic, rapid, blitz)
    if federation and classic is not None and rapid is not None:
        return "classic_rapid", (federation, classic, rapid)
    if federation and classic is not None and blitz is not None:
        return "classic_blitz", (federation, classic, blitz)
    if federation and rapid is not None and blitz is not None:
        return "rapid_blitz", (federation, rapid, blitz)
    if federation and classic is not None:
        return "classic", (federation, classic)
    if federation and rapid is not None:
        return "rapid", (federation, rapid)
    if federation and blitz is not None:
        return "blitz", (federation, blitz)
    if federation and name_norm:
        return "name_federation", (federation, name_norm)
    if name_norm:
        return "name_only", (name_norm,)
    return None, ()


def build_pending_rows(rows: list[dict[str, str]]) -> tuple[list[PendingRow], dict[str, dict[tuple, list[int]]]]:
    pending_rows: list[PendingRow] = []
    requested_keys: dict[str, dict[tuple, list[int]]] = defaultdict(lambda: defaultdict(list))

    for index, row in enumerate(rows):
        birthday = (row.get("birthday") or "").strip()
        if birthday:
            continue

        name_variants = lookup_name_variants(row.get("real_name"))

        federation = (row.get("federation") or "").strip().upper()
        classic = parse_rating(row.get("classic_rating"))
        rapid = parse_rating(row.get("rapid_rating"))
        blitz = parse_rating(row.get("blitz_rating"))

        pattern, key = select_pattern(name_variants, federation, classic, rapid, blitz)
        pending = PendingRow(
            index=index,
            pattern=pattern or "unmatchable",
            name_variants=name_variants,
            federation=federation,
            classic=classic,
            rapid=rapid,
            blitz=blitz,
        )
        pending_rows.append(pending)
        if pattern:
            requested_keys[pattern][key].append(index)
        if federation and name_variants:
            for name_variant in name_variants:
                requested_keys["name_federation"][(federation, name_variant)].append(index)
        elif name_variants:
            for name_variant in name_variants:
                requested_keys["name_only"][(name_variant,)].append(index)

    return pending_rows, requested_keys


def candidate_key_sets(player: dict[str, str]) -> dict[str, tuple]:
    federation = player["country"]
    classic = parse_rating(player["rating"])
    rapid = parse_rating(player["rapid_rating"])
    blitz = parse_rating(player["blitz_rating"])
    name_variants = tuple(sorted(fide_name_variants(player["name"])))
    name_primary = name_variants[0] if name_variants else ""

    keys: dict[str, tuple] = {}
    if federation and classic is not None and rapid is not None and blitz is not None:
        keys["full"] = (federation, classic, rapid, blitz)
    if federation and classic is not None and rapid is not None:
        keys["classic_rapid"] = (federation, classic, rapid)
    if federation and classic is not None and blitz is not None:
        keys["classic_blitz"] = (federation, classic, blitz)
    if federation and rapid is not None and blitz is not None:
        keys["rapid_blitz"] = (federation, rapid, blitz)
    if federation and classic is not None:
        keys["classic"] = (federation, classic)
    if federation and rapid is not None:
        keys["rapid"] = (federation, rapid)
    if federation and blitz is not None:
        keys["blitz"] = (federation, blitz)
    if federation and name_primary:
        for variant in name_variants:
            keys[f"name_federation::{variant}"] = (federation, variant)
    if name_primary:
        for variant in name_variants:
            keys[f"name_only::{variant}"] = (variant,)
    return keys


def append_candidates(
    player: dict[str, str],
    requested_keys: dict[str, dict[tuple, list[int]]],
    candidates_by_row: dict[int, list[Candidate]],
) -> None:
    candidate = Candidate(
        name=player["name"],
        name_variants=tuple(sorted(fide_name_variants(player["name"]))),
        birthday=player["birthday"],
    )
    keys = candidate_key_sets(player)

    for pattern in (
        "full",
        "classic_rapid",
        "classic_blitz",
        "rapid_blitz",
        "classic",
        "rapid",
        "blitz",
    ):
        key = keys.get(pattern)
        if key and key in requested_keys.get(pattern, {}):
            for row_index in requested_keys[pattern][key]:
                candidates_by_row[row_index].append(candidate)

    for compound_pattern in ("name_federation", "name_only"):
        for key_name, key in keys.items():
            if not key_name.startswith(f"{compound_pattern}::"):
                continue
            if key in requested_keys.get(compound_pattern, {}):
                for row_index in requested_keys[compound_pattern][key]:
                    candidates_by_row[row_index].append(candidate)


def player_from_element(element: ET.Element) -> dict[str, str]:
    def text(tag: str) -> str:
        node = element.find(tag)
        return (node.text if node is not None and node.text is not None else "").strip()

    return {
        "name": text("name"),
        "country": text("country").upper(),
        "rating": text("rating"),
        "rapid_rating": text("rapid_rating"),
        "blitz_rating": text("blitz_rating"),
        "birthday": text("birthday"),
    }


def resolve_candidate(row: PendingRow, candidates: list[Candidate]) -> Candidate | None:
    if not candidates:
        return None

    deduped: dict[tuple[str, str], Candidate] = {}
    for candidate in candidates:
        deduped[(candidate.name, candidate.birthday)] = candidate
    reduced = list(deduped.values())

    if len(reduced) == 1:
        return reduced[0]

    if row.name_variants:
        row_name_variants = set(row.name_variants)
        by_name = [
            candidate
            for candidate in reduced
            if row_name_variants.intersection(candidate.name_variants)
        ]
        if len(by_name) == 1:
            return by_name[0]
        if by_name:
            reduced = by_name

    birthdays = {candidate.birthday for candidate in reduced if candidate.birthday}
    if len(birthdays) == 1:
        target_birthday = next(iter(birthdays))
        for candidate in reduced:
            if candidate.birthday == target_birthday:
                return candidate

    return None


def stream_candidates(
    xml_zip_path: Path,
    requested_keys: dict[str, dict[tuple, list[int]]],
) -> dict[int, list[Candidate]]:
    candidates_by_row: dict[int, list[Candidate]] = defaultdict(list)

    with zipfile.ZipFile(xml_zip_path) as archive:
        xml_members = [name for name in archive.namelist() if name.endswith(".xml")]
        if not xml_members:
            raise FileNotFoundError(f"No XML file found inside {xml_zip_path}")

        with archive.open(xml_members[0]) as xml_file:
            context = ET.iterparse(xml_file, events=("end",))
            for _event, element in context:
                if element.tag != "player":
                    continue
                player = player_from_element(element)
                if player["birthday"]:
                    append_candidates(player, requested_keys, candidates_by_row)
                element.clear()

    return candidates_by_row


def main() -> None:
    args = parse_args()
    rows: list[dict[str, str]]
    with args.input.open(newline="", encoding="utf-8") as infile:
        reader = csv.DictReader(infile)
        fieldnames = reader.fieldnames
        if fieldnames is None:
            raise ValueError(f"No header found in {args.input}")
        rows = list(reader)

    pending_rows, requested_keys = build_pending_rows(rows)
    candidates_by_row = stream_candidates(args.fide_xml_zip, requested_keys)

    matched = 0
    unresolved = 0
    unresolved_patterns: Counter[str] = Counter()

    for pending in pending_rows:
        if pending.pattern == "unmatchable":
            unresolved += 1
            unresolved_patterns[pending.pattern] += 1
            continue

        candidate = resolve_candidate(pending, candidates_by_row.get(pending.index, []))
        if candidate is None:
            unresolved += 1
            unresolved_patterns[pending.pattern] += 1
            continue

        rows[pending.index]["birthday"] = f"{int(candidate.birthday)}.0"
        matched += 1

    args.output.parent.mkdir(parents=True, exist_ok=True)
    with args.output.open("w", newline="", encoding="utf-8") as outfile:
        writer = csv.DictWriter(outfile, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)

    print(f"Matched missing birthdays: {matched}")
    print(f"Still unresolved: {unresolved}")
    for pattern, count in unresolved_patterns.most_common():
        print(f"Unresolved {pattern}: {count}")
    print(f"Wrote: {args.output}")


if __name__ == "__main__":
    main()
