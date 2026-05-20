#!/usr/bin/env python3
import csv
import json
import ssl
import time
from datetime import datetime, timezone
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.request import Request, urlopen

import certifi

INPUT = Path("players_final_data_merged.csv")
CACHE = Path("chesscom_profile_cache.jsonl")
FINDINGS = Path("real_name_findings_chesscom.csv")
OUTPUT = Path("players_final_data_merged_with_chesscom_names.csv")
SSL_CONTEXT = ssl.create_default_context(cafile=certifi.where())


def missing_real_name(row):
    return not (row.get("real_name") or "").strip()


def fetch_profile(username):
    url = f"https://api.chess.com/pub/player/{username}"
    req = Request(
        url,
        headers={
            "User-Agent": "search-for-players research; contact: local research script",
            "Accept": "application/json",
        },
    )
    with urlopen(req, timeout=20, context=SSL_CONTEXT) as response:
        return response.status, json.loads(response.read().decode("utf-8"))


def load_cache():
    cache = {}
    if CACHE.exists():
        with CACHE.open(encoding="utf-8") as fh:
            for line in fh:
                if line.strip():
                    item = json.loads(line)
                    cache[item["player_name"]] = item
    return cache


def append_cache(item):
    with CACHE.open("a", encoding="utf-8") as fh:
        fh.write(json.dumps(item, ensure_ascii=False, sort_keys=True) + "\n")


def retryable_cache_item(item):
    error = item.get("error") or ""
    return (
        not item.get("ok")
        and not item.get("status")
        and (
            "CERTIFICATE_VERIFY_FAILED" in error
            or "SSLCertVerificationError" in error
            or "nodename nor servname" in error
        )
    )


def main():
    with INPUT.open(newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        rows = list(reader)
        fieldnames = reader.fieldnames or []

    missing_players = [r["player_name"] for r in rows if missing_real_name(r)]
    cache = load_cache()
    now = datetime.now(timezone.utc).isoformat()

    for idx, username in enumerate(missing_players, 1):
        if username in cache and not retryable_cache_item(cache[username]):
            continue
        item = {
            "player_name": username,
            "fetched_at": now,
            "status": None,
            "ok": False,
            "profile": None,
            "error": None,
        }
        try:
            status, profile = fetch_profile(username)
            item.update({"status": status, "ok": True, "profile": profile})
        except HTTPError as exc:
            body = exc.read().decode("utf-8", errors="replace")
            item.update({"status": exc.code, "error": body[:1000]})
        except URLError as exc:
            item.update({"error": repr(exc.reason)})
        except Exception as exc:
            item.update({"error": repr(exc)})

        append_cache(item)
        cache[username] = item
        if idx % 25 == 0:
            print(f"queried {idx}/{len(missing_players)}")
        time.sleep(0.25)

    name_by_player = {}
    evidence_rows = []
    for username in missing_players:
        item = cache.get(username, {})
        profile = item.get("profile") or {}
        name = (profile.get("name") or "").strip()
        if name:
            name_by_player[username] = name
            evidence_rows.append(
                {
                    "player_name": username,
                    "real_name": name,
                    "source": profile.get("url") or f"https://www.chess.com/member/{username}",
                    "source_type": "Chess.com public player API profile name field",
                    "profile_username": profile.get("username") or "",
                    "title": profile.get("title") or "",
                    "country_api": profile.get("country") or "",
                    "status": profile.get("status") or "",
                }
            )

    with FINDINGS.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(
            fh,
            fieldnames=[
                "player_name",
                "real_name",
                "source",
                "source_type",
                "profile_username",
                "title",
                "country_api",
                "status",
            ],
        )
        writer.writeheader()
        writer.writerows(evidence_rows)

    output_rows = []
    for row in rows:
        row = dict(row)
        if missing_real_name(row) and row["player_name"] in name_by_player:
            row["real_name"] = name_by_player[row["player_name"]]
        output_rows.append(row)

    with OUTPUT.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(output_rows)

    print(f"missing rows: {len(missing_players)}")
    print(f"profiles queried/cached: {len(cache)}")
    print(f"names found from Chess.com API: {len(evidence_rows)}")
    print(f"wrote {FINDINGS}")
    print(f"wrote {OUTPUT}")


if __name__ == "__main__":
    main()
