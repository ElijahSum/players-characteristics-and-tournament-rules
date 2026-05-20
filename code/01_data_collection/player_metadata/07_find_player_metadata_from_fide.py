#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import json
import time
import urllib.error
import urllib.parse
import urllib.request
import xml.etree.ElementTree as ET
import zipfile
from collections import defaultdict
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path

from fill_fide_birthyears import fide_name_variants, lookup_name_variants, parse_rating


DEFAULT_INPUT = Path("outputs/players_in_regression_missing_from_players_final_data.csv")
DEFAULT_OUTPUT = Path("outputs/player_metadata_from_fide.csv")
DEFAULT_FIDE_XML_ZIP = Path("/tmp/players_list_xml.zip")
DEFAULT_CACHE = Path("outputs/chesscom_player_profiles_cache.json")
PLAYER_NAME_COLUMN = "player_name"
FIDE_TITLES = {
    "GM",
    "IM",
    "FM",
    "CM",
    "WGM",
    "WIM",
    "WFM",
    "WCM",
}


@dataclass(frozen=True)
class PlayerRequest:
    player_name: str
    chesscom_username: str
    player_link: str
    input_title: str
    input_real_name: str


@dataclass(frozen=True)
class ChessComProfile:
    username: str
    real_name: str
    title: str


@dataclass(frozen=True)
class PendingRequest:
    request: PlayerRequest
    search_name: str
    search_title: str
    name_variants: tuple[str, ...]
    chesscom_profile: ChessComProfile | None
    profile_status: str
    profile_error: str


@dataclass(frozen=True)
class FideCandidate:
    name: str
    title: str
    federation: str
    classic_rating: int | None
    rapid_rating: int | None
    blitz_rating: int | None
    birth_year: int | None
    name_variants: tuple[str, ...]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Build player metadata from an input CSV by combining Chess.com public profiles "
            "with the official FIDE XML export."
        )
    )
    parser.add_argument(
        "--input",
        type=Path,
        default=DEFAULT_INPUT,
        help=f"Input CSV path (default: {DEFAULT_INPUT})",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=DEFAULT_OUTPUT,
        help=f"Output CSV path (default: {DEFAULT_OUTPUT})",
    )
    parser.add_argument(
        "--fide-xml-zip",
        type=Path,
        default=DEFAULT_FIDE_XML_ZIP,
        help=(
            "Path to the official FIDE XML ZIP export. "
            f"Download it from https://ratings.fide.com/download_lists.phtml (default: {DEFAULT_FIDE_XML_ZIP})"
        ),
    )
    parser.add_argument(
        "--chesscom-cache",
        type=Path,
        default=DEFAULT_CACHE,
        help=f"JSON cache for Chess.com player profiles (default: {DEFAULT_CACHE})",
    )
    parser.add_argument(
        "--input-mode",
        choices=("auto", "pairings", "players", "real-names"),
        default="auto",
        help=(
            "How to interpret the input CSV. "
            "'pairings' expects white/black columns, 'players' expects player_name or username, "
            "'real-names' expects real_name or name."
        ),
    )
    parser.add_argument(
        "--skip-chesscom",
        action="store_true",
        help="Do not query the Chess.com public API. Use only names already present in the input CSV.",
    )
    parser.add_argument(
        "--age-as-of-year",
        type=int,
        default=datetime.now().year,
        help="Reference year used to convert FIDE birth year to age.",
    )
    parser.add_argument(
        "--limit",
        type=int,
        default=None,
        help="Optional limit on the number of unique players to process.",
    )
    parser.add_argument(
        "--request-delay-seconds",
        type=float,
        default=0.0,
        help="Optional delay between uncached Chess.com API requests.",
    )
    parser.add_argument(
        "--chesscom-timeout-seconds",
        type=float,
        default=10.0,
        help="Timeout for each live Chess.com API request.",
    )
    parser.add_argument(
        "--progress-every",
        type=int,
        default=25,
        help="Print a progress update every N processed players.",
    )
    parser.add_argument(
        "--max-consecutive-network-errors",
        type=int,
        default=3,
        help=(
            "Stop making live Chess.com requests after this many consecutive network failures. "
            "Remaining uncached players will be marked as skipped."
        ),
    )
    return parser.parse_args()


def clean_text(value: str | None) -> str:
    return value.strip() if value else ""


def normalize_fide_title(value: str | None) -> str:
    title = clean_text(value).upper()
    return title if title in FIDE_TITLES else ""


def parse_birth_year(value: str | None) -> int | None:
    if value is None:
        return None
    value = value.strip()
    if not value:
        return None
    try:
        return int(float(value))
    except ValueError:
        return None


def derive_age(birth_year: int | None, age_as_of_year: int) -> int | None:
    if birth_year is None:
        return None
    return age_as_of_year - birth_year


def detect_input_mode(fieldnames: list[str], requested_mode: str) -> str:
    if requested_mode != "auto":
        return requested_mode
    if "white_name" in fieldnames and "black_name" in fieldnames:
        return "pairings"
    if "player_name" in fieldnames or "username" in fieldnames:
        return "players"
    if "real_name" in fieldnames or "name" in fieldnames:
        return "real-names"
    raise ValueError(
        "Could not detect input mode. Expected one of: "
        "white_name/black_name, player_name/username, or real_name/name."
    )


def extract_chesscom_username(player_name: str, player_link: str) -> str:
    if player_link:
        parsed = urllib.parse.urlparse(player_link)
        parts = [part for part in parsed.path.split("/") if part]
        if len(parts) >= 2 and parts[-2].lower() == "member":
            return urllib.parse.unquote(parts[-1]).strip()
    return player_name.strip()


def merge_value(current: str, candidate: str) -> str:
    return current if current else candidate


def upsert_request(
    requests_by_key: dict[str, PlayerRequest],
    *,
    player_name: str,
    player_link: str,
    input_title: str,
    input_real_name: str,
) -> None:
    player_name = clean_text(player_name)
    player_link = clean_text(player_link)
    input_title = clean_text(input_title)
    input_real_name = clean_text(input_real_name)
    if not player_name and not input_real_name:
        return

    chesscom_username = extract_chesscom_username(player_name, player_link) if player_name else ""
    dedupe_key = (chesscom_username or input_real_name).casefold()
    if not dedupe_key:
        return

    existing = requests_by_key.get(dedupe_key)
    if existing is None:
        requests_by_key[dedupe_key] = PlayerRequest(
            player_name=player_name or input_real_name,
            chesscom_username=chesscom_username,
            player_link=player_link,
            input_title=input_title,
            input_real_name=input_real_name,
        )
        return

    requests_by_key[dedupe_key] = PlayerRequest(
        player_name=merge_value(existing.player_name, player_name or input_real_name),
        chesscom_username=merge_value(existing.chesscom_username, chesscom_username),
        player_link=merge_value(existing.player_link, player_link),
        input_title=merge_value(existing.input_title, input_title),
        input_real_name=merge_value(existing.input_real_name, input_real_name),
    )


def collect_requests(input_path: Path, input_mode: str, limit: int | None) -> list[PlayerRequest]:
    requests_by_key: dict[str, PlayerRequest] = {}
    with input_path.open("r", encoding="utf-8-sig", newline="") as src:
        reader = csv.DictReader(src)
        fieldnames = reader.fieldnames
        if not fieldnames:
            raise ValueError(f"Input CSV has no header row: {input_path}")

        resolved_mode = detect_input_mode(list(fieldnames), input_mode)
        for row in reader:
            if resolved_mode == "pairings":
                upsert_request(
                    requests_by_key,
                    player_name=row.get("white_name", ""),
                    player_link=row.get("white_link", ""),
                    input_title=row.get("white_title", ""),
                    input_real_name=row.get("white_player", ""),
                )
                upsert_request(
                    requests_by_key,
                    player_name=row.get("black_name", ""),
                    player_link=row.get("black_link", ""),
                    input_title=row.get("black_title", ""),
                    input_real_name=row.get("black_player", ""),
                )
            elif resolved_mode == "players":
                upsert_request(
                    requests_by_key,
                    player_name=row.get("player_name", "") or row.get("username", ""),
                    player_link=row.get("player_link", "") or row.get("link", ""),
                    input_title=row.get("player_title", "") or row.get("title", ""),
                    input_real_name=row.get("real_name", ""),
                )
            elif resolved_mode == "real-names":
                upsert_request(
                    requests_by_key,
                    player_name=row.get("player_name", "") or row.get("name", "") or row.get("real_name", ""),
                    player_link=row.get("player_link", "") or row.get("link", ""),
                    input_title=row.get("player_title", "") or row.get("title", ""),
                    input_real_name=row.get("real_name", "") or row.get("name", ""),
                )
            else:
                raise ValueError(f"Unsupported input mode: {resolved_mode}")

            if limit is not None and len(requests_by_key) >= limit:
                break

    return list(requests_by_key.values())


def load_chesscom_cache(cache_path: Path) -> dict[str, dict[str, str]]:
    if not cache_path.exists():
        return {}
    return json.loads(cache_path.read_text(encoding="utf-8"))


def save_chesscom_cache(cache_path: Path, cache: dict[str, dict[str, str]]) -> None:
    cache_path.parent.mkdir(parents=True, exist_ok=True)
    cache_path.write_text(
        json.dumps(cache, ensure_ascii=True, indent=2, sort_keys=True),
        encoding="utf-8",
    )


def fetch_chesscom_profile(
    username: str,
    *,
    timeout_seconds: float,
) -> tuple[ChessComProfile | None, str, str]:
    encoded_username = urllib.parse.quote(username)
    url = f"https://api.chess.com/pub/player/{encoded_username}"
    request = urllib.request.Request(
        url,
        headers={
            "User-Agent": "player-metadata-builder/1.0",
            "Accept": "application/json",
        },
    )
    try:
        with urllib.request.urlopen(request, timeout=timeout_seconds) as response:
            payload = json.loads(response.read().decode("utf-8"))
    except urllib.error.HTTPError as exc:
        if exc.code == 404:
            return None, "not_found", ""
        return None, "http_error", f"http_{exc.code}"
    except urllib.error.URLError as exc:
        return None, "network_error", str(exc.reason)
    except TimeoutError:
        return None, "timeout", "timeout"
    real_name = clean_text(payload.get("name"))
    title = clean_text(payload.get("title"))
    return ChessComProfile(username=username, real_name=real_name, title=title), "found", ""


def enrich_with_chesscom_profiles(
    requests: list[PlayerRequest],
    *,
    skip_chesscom: bool,
    cache_path: Path,
    request_delay_seconds: float,
    timeout_seconds: float,
    progress_every: int,
    max_consecutive_network_errors: int,
) -> list[PendingRequest]:
    cache = load_chesscom_cache(cache_path)
    pending: list[PendingRequest] = []
    updated_cache = False
    live_requests_disabled = skip_chesscom
    consecutive_network_errors = 0
    cached_hits = 0
    cached_misses = 0
    live_found = 0
    live_not_found = 0
    live_failures = 0

    if skip_chesscom:
        print("Chess.com lookups disabled by --skip-chesscom.")
    else:
        live_candidates = sum(1 for request in requests if request.chesscom_username)
        print(
            "Preparing live Chess.com lookups for "
            f"{live_candidates} of {len(requests)} players..."
        )

    for index, request in enumerate(requests, start=1):
        profile: ChessComProfile | None = None
        profile_status = "skipped" if skip_chesscom else "missing_username"
        profile_error = ""
        if not live_requests_disabled and request.chesscom_username:
            profile_status = "missing_cache"
            cache_key = request.chesscom_username.casefold()
            cached = cache.get(cache_key)
            if cached is not None:
                if cached.get("found") == "1":
                    profile = ChessComProfile(
                        username=request.chesscom_username,
                        real_name=cached.get("real_name", ""),
                        title=cached.get("title", ""),
                    )
                    profile_status = "cached_found"
                    cached_hits += 1
                else:
                    profile_status = "cached_not_found"
                    cached_hits += 1
            else:
                cached_misses += 1
                profile, profile_status, profile_error = fetch_chesscom_profile(
                    request.chesscom_username,
                    timeout_seconds=timeout_seconds,
                )
                if profile_status in {"found", "not_found"}:
                    cache[cache_key] = {
                        "found": "1" if profile is not None else "0",
                        "real_name": profile.real_name if profile else "",
                        "title": profile.title if profile else "",
                    }
                    updated_cache = True
                    consecutive_network_errors = 0
                    if profile is not None:
                        live_found += 1
                    else:
                        live_not_found += 1
                else:
                    live_failures += 1
                    consecutive_network_errors += 1
                    if consecutive_network_errors >= max_consecutive_network_errors:
                        live_requests_disabled = True
                        print(
                            "Chess.com lookups disabled after "
                            f"{consecutive_network_errors} consecutive network failures. "
                            "Remaining uncached players will be marked as skipped."
                        )
                if request_delay_seconds > 0:
                    time.sleep(request_delay_seconds)
        elif request.chesscom_username and live_requests_disabled and not skip_chesscom:
            profile_status = "live_requests_disabled"

        search_name = request.input_real_name or (profile.real_name if profile else "")
        search_title = normalize_fide_title(request.input_title or (profile.title if profile else ""))
        name_variants = lookup_name_variants(search_name)
        pending.append(
            PendingRequest(
                request=request,
                search_name=search_name,
                search_title=search_title,
                name_variants=name_variants,
                chesscom_profile=profile,
                profile_status=profile_status,
                profile_error=profile_error,
            )
        )

        should_print = (
            index == 1
            or index == len(requests)
            or (progress_every > 0 and index % progress_every == 0)
        )
        if should_print:
            print(
                "Prepared lookup context for "
                f"{index}/{len(requests)} players "
                f"(cache_hits={cached_hits}, uncached={cached_misses}, "
                f"live_found={live_found}, live_not_found={live_not_found}, "
                f"live_failures={live_failures})."
            )

    if updated_cache:
        save_chesscom_cache(cache_path, cache)
    return pending


def build_requested_keys(
    pending_requests: list[PendingRequest],
) -> dict[str, dict[tuple[str, ...], list[int]]]:
    requested_keys: dict[str, dict[tuple[str, ...], list[int]]] = defaultdict(lambda: defaultdict(list))
    for index, pending in enumerate(pending_requests):
        if not pending.name_variants:
            continue
        for variant in pending.name_variants:
            requested_keys["name_only"][(variant,)].append(index)
            if pending.search_title:
                requested_keys["name_title"][(variant, pending.search_title)].append(index)
    return requested_keys


def fide_player_from_element(element: ET.Element) -> FideCandidate:
    def text(tag: str) -> str:
        node = element.find(tag)
        return clean_text(node.text if node is not None and node.text is not None else "")

    name = text("name")
    title = normalize_fide_title(text("title"))
    return FideCandidate(
        name=name,
        title=title,
        federation=text("country").upper(),
        classic_rating=parse_rating(text("rating")),
        rapid_rating=parse_rating(text("rapid_rating")),
        blitz_rating=parse_rating(text("blitz_rating")),
        birth_year=parse_birth_year(text("birthday")),
        name_variants=tuple(sorted(fide_name_variants(name))),
    )


def append_fide_candidate(
    candidate: FideCandidate,
    requested_keys: dict[str, dict[tuple[str, ...], list[int]]],
    candidates_by_request: dict[int, list[FideCandidate]],
) -> None:
    if not candidate.name_variants:
        return

    for variant in candidate.name_variants:
        key = (variant,)
        if key in requested_keys.get("name_only", {}):
            for request_index in requested_keys["name_only"][key]:
                candidates_by_request[request_index].append(candidate)

        if candidate.title:
            key_with_title = (variant, candidate.title)
            if key_with_title in requested_keys.get("name_title", {}):
                for request_index in requested_keys["name_title"][key_with_title]:
                    candidates_by_request[request_index].append(candidate)


def stream_fide_candidates(
    xml_zip_path: Path,
    requested_keys: dict[str, dict[tuple[str, ...], list[int]]],
) -> dict[int, list[FideCandidate]]:
    candidates_by_request: dict[int, list[FideCandidate]] = defaultdict(list)
    with zipfile.ZipFile(xml_zip_path) as archive:
        xml_members = [name for name in archive.namelist() if name.endswith(".xml")]
        if not xml_members:
            raise FileNotFoundError(f"No XML file found inside {xml_zip_path}")

        with archive.open(xml_members[0]) as xml_file:
            context = ET.iterparse(xml_file, events=("end",))
            for _event, element in context:
                if element.tag != "player":
                    continue
                candidate = fide_player_from_element(element)
                append_fide_candidate(candidate, requested_keys, candidates_by_request)
                element.clear()

    return candidates_by_request


def validate_fide_xml_zip(xml_zip_path: Path) -> None:
    if not xml_zip_path.exists():
        raise FileNotFoundError(
            f"FIDE XML ZIP not found: {xml_zip_path}. "
            "Download it from https://ratings.fide.com/download_lists.phtml "
            "or pass --fide-xml-zip /path/to/players_list_xml.zip ."
        )
    if not zipfile.is_zipfile(xml_zip_path):
        raise ValueError(f"FIDE XML ZIP path is not a valid zip file: {xml_zip_path}")

    with zipfile.ZipFile(xml_zip_path) as archive:
        xml_members = [name for name in archive.namelist() if name.endswith(".xml")]
        if not xml_members:
            raise FileNotFoundError(f"No XML file found inside {xml_zip_path}")


def resolve_fide_candidate(
    pending: PendingRequest,
    candidates: list[FideCandidate],
) -> tuple[FideCandidate | None, str]:
    if not candidates:
        return None, "no_fide_match"

    deduped: dict[
        tuple[str, str, str, int | None, int | None, int | None, int | None],
        FideCandidate,
    ] = {}
    for candidate in candidates:
        deduped[
            (
                candidate.name,
                candidate.title,
                candidate.federation,
                candidate.classic_rating,
                candidate.rapid_rating,
                candidate.blitz_rating,
                candidate.birth_year,
            )
        ] = candidate
    reduced = list(deduped.values())

    if pending.search_title:
        titled = [candidate for candidate in reduced if candidate.title == pending.search_title]
        if titled:
            reduced = titled

    if len(reduced) == 1:
        return reduced[0], "matched"

    if pending.name_variants:
        exact_name = [
            candidate
            for candidate in reduced
            if set(pending.name_variants).intersection(candidate.name_variants)
        ]
        if len(exact_name) == 1:
            return exact_name[0], "matched"
        if exact_name:
            reduced = exact_name

    unique_payloads = {
        (
            candidate.federation,
            candidate.classic_rating,
            candidate.rapid_rating,
            candidate.blitz_rating,
            candidate.birth_year,
        )
        for candidate in reduced
    }
    if len(unique_payloads) == 1:
        return reduced[0], "matched"

    return None, f"ambiguous_fide_match:{len(reduced)}"


def output_fieldnames() -> list[str]:
    return [
        "player_name",
        "chesscom_username",
        "player_link",
        "input_title",
        "input_real_name",
        "chesscom_real_name",
        "chesscom_title",
        "real_name",
        "federation",
        "classic_rating",
        "rapid_rating",
        "blitz_rating",
        "birth_year",
        "age",
        "match_status",
        "match_note",
    ]


def build_output_rows(
    pending_requests: list[PendingRequest],
    candidates_by_request: dict[int, list[FideCandidate]],
    *,
    age_as_of_year: int,
) -> list[dict[str, str]]:
    rows: list[dict[str, str]] = []
    for index, pending in enumerate(pending_requests):
        candidate, status = resolve_fide_candidate(pending, candidates_by_request.get(index, []))
        chesscom_real_name = (
            pending.chesscom_profile.real_name if pending.chesscom_profile is not None else ""
        )
        chesscom_title = pending.chesscom_profile.title if pending.chesscom_profile is not None else ""
        final_real_name = candidate.name if candidate is not None else (
            pending.request.input_real_name or chesscom_real_name
        )
        birth_year = candidate.birth_year if candidate is not None else None
        age = derive_age(birth_year, age_as_of_year)
        if pending.name_variants:
            match_status = status
        elif pending.profile_status in {"network_error", "timeout", "http_error"}:
            match_status = "chesscom_lookup_failed"
        elif pending.profile_status in {"not_found", "cached_not_found"}:
            match_status = "chesscom_profile_not_found"
        elif pending.profile_status == "live_requests_disabled":
            match_status = "chesscom_lookup_skipped_after_network_failures"
        else:
            match_status = "missing_search_name"

        match_note = pending.profile_error
        if not match_note and match_status.startswith("ambiguous_fide_match"):
            match_note = f"{len(candidates_by_request.get(index, []))} candidate rows"

        rows.append(
            {
                "player_name": pending.request.player_name,
                "chesscom_username": pending.request.chesscom_username,
                "player_link": pending.request.player_link,
                "input_title": pending.request.input_title,
                "input_real_name": pending.request.input_real_name,
                "chesscom_real_name": chesscom_real_name,
                "chesscom_title": chesscom_title,
                "real_name": final_real_name,
                "federation": candidate.federation if candidate is not None else "",
                "classic_rating": (
                    str(candidate.classic_rating) if candidate is not None and candidate.classic_rating is not None else ""
                ),
                "rapid_rating": (
                    str(candidate.rapid_rating) if candidate is not None and candidate.rapid_rating is not None else ""
                ),
                "blitz_rating": (
                    str(candidate.blitz_rating) if candidate is not None and candidate.blitz_rating is not None else ""
                ),
                "birth_year": str(birth_year) if birth_year is not None else "",
                "age": str(age) if age is not None else "",
                "match_status": match_status,
                "match_note": match_note,
            }
        )
    return rows


def write_output(output_path: Path, rows: list[dict[str, str]]) -> None:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with output_path.open("w", encoding="utf-8", newline="") as dst:
        writer = csv.DictWriter(dst, fieldnames=output_fieldnames())
        writer.writeheader()
        writer.writerows(rows)


def print_summary(rows: list[dict[str, str]]) -> None:
    status_counts: dict[str, int] = defaultdict(int)
    for row in rows:
        status_counts[row["match_status"]] += 1
    for status in sorted(status_counts):
        print(f"{status}: {status_counts[status]}")


def main() -> int:
    args = parse_args()
    if not args.input.exists():
        raise FileNotFoundError(f"Input CSV not found: {args.input}")
    validate_fide_xml_zip(args.fide_xml_zip)

    print(f"Collecting unique players from: {args.input}")
    requests = collect_requests(args.input, args.input_mode, args.limit)
    print(f"Collected {len(requests)} unique players.")

    print("Preparing Chess.com / name lookup context...")
    pending_requests = enrich_with_chesscom_profiles(
        requests,
        skip_chesscom=args.skip_chesscom,
        cache_path=args.chesscom_cache,
        request_delay_seconds=args.request_delay_seconds,
        timeout_seconds=args.chesscom_timeout_seconds,
        progress_every=args.progress_every,
        max_consecutive_network_errors=args.max_consecutive_network_errors,
    )

    print("Building requested FIDE keys...")
    requested_keys = build_requested_keys(pending_requests)

    print(f"Streaming FIDE XML from: {args.fide_xml_zip}")
    candidates_by_request = stream_fide_candidates(args.fide_xml_zip, requested_keys)

    print("Resolving player matches...")
    rows = build_output_rows(
        pending_requests,
        candidates_by_request,
        age_as_of_year=args.age_as_of_year,
    )

    print(f"Writing output: {args.output}")
    write_output(args.output, rows)
    print_summary(rows)
    print(f"Wrote {len(rows)} rows to: {args.output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
