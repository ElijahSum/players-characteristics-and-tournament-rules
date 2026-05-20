#!/usr/bin/env python3
import base64
import csv
import html
import json
import re
import subprocess
import time
from datetime import datetime, timezone
from pathlib import Path
from urllib.parse import parse_qs, quote_plus, unquote, urlparse


BASE_INPUT = Path("players_final_data_merged_with_chesscom_names.csv")
ORIGINAL_INPUT = Path("players_final_data_merged.csv")
CHESSCOM_FINDINGS = Path("real_name_findings_chesscom.csv")
BING_CACHE = Path("bing_search_cache.jsonl")
BING_FINDINGS = Path("real_name_findings_bing.csv")
COMBINED_FINDINGS = Path("real_name_findings_combined.csv")
OUTPUT = Path("players_final_data_merged_with_real_names.csv")

TITLES = "GM|WGM|IM|WIM|FM|WFM|CM|WCM|NM|WNM"


def blank(value):
    return not (value or "").strip()


def read_rows(path):
    with path.open(newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        return list(reader), reader.fieldnames or []


def strip_tags(value):
    value = re.sub(r"<script\b.*?</script>", " ", value, flags=re.I | re.S)
    value = re.sub(r"<style\b.*?</style>", " ", value, flags=re.I | re.S)
    value = re.sub(r"<[^>]+>", " ", value)
    return re.sub(r"\s+", " ", html.unescape(value)).strip()


def decode_bing_url(url):
    url = html.unescape(url)
    parsed = urlparse(url)
    query = parse_qs(parsed.query)
    encoded = query.get("u", [""])[0]
    if encoded.startswith("a1"):
        payload = encoded[2:]
        payload += "=" * (-len(payload) % 4)
        try:
            return base64.urlsafe_b64decode(payload).decode("utf-8")
        except Exception:
            return url
    return url


def parse_results(page):
    results = []
    blocks = re.findall(r'<li class="b_algo".*?</li>', page, flags=re.I | re.S)
    for block in blocks:
        title_match = re.search(r"<h2[^>]*>\s*<a[^>]+href=\"([^\"]+)\"[^>]*>(.*?)</a>\s*</h2>", block, flags=re.I | re.S)
        snippet_match = re.search(r'<div class="b_caption">\s*<p[^>]*>(.*?)</p>', block, flags=re.I | re.S)
        if not title_match:
            continue
        results.append(
            {
                "title": strip_tags(title_match.group(2)),
                "snippet": strip_tags(snippet_match.group(1)) if snippet_match else "",
                "url": decode_bing_url(title_match.group(1)),
            }
        )
    return results


def clean_candidate(candidate, username):
    candidate = html.unescape(candidate)
    candidate = re.sub(r"\b(?:GM|WGM|IM|WIM|FM|WFM|CM|WCM|NM|WNM)\b", "", candidate)
    candidate = re.sub(r"\s+", " ", candidate).strip(" -–—|:,.()[]")
    lower = candidate.lower()
    bad_bits = [
        "chess.com",
        "chess profile",
        "online chess",
        "profile of",
        "discover",
        "member",
        "profil",
        "perfil",
        "player profile",
        username.lower(),
    ]
    if any(bit in lower for bit in bad_bits):
        return ""
    if re.search(r"\d|[@_/]", candidate):
        return ""
    if not re.fullmatch(r"[A-Za-zÀ-ÖØ-öø-ÿĀ-žǍ-ȳ'’.\- ]{3,80}", candidate):
        return ""
    tokens = [t for t in re.split(r"\s+", candidate) if t]
    if len(tokens) < 2:
        return ""
    return candidate


def candidate_from_text(text, username):
    user_pat = re.escape(username)
    patterns = [
        rf"\b(?:{TITLES})\s+(.+?)\s*\(\s*{user_pat}\s*\)",
        rf"profile of\s+(?:{TITLES}\s+)?(.+?)\s*\(\s*{user_pat}\s*\)",
        rf"\b(?:{TITLES})\s+(.+?)\s+-\s+(?:Chess Profile|Profil|Perfil|Hồ sơ|Şahmat Profili)",
    ]
    for pattern in patterns:
        match = re.search(pattern, text, flags=re.I)
        if match:
            candidate = clean_candidate(match.group(1), username)
            if candidate:
                return candidate
    return ""


def extract_candidate(result, username):
    url_lower = result["url"].lower()
    text = f'{result["title"]} {result["snippet"]}'
    is_chesscom_member = (
        "chess.com/" in url_lower
        and f"/member/{username.lower()}" in url_lower
    )
    if not is_chesscom_member:
        return ""
    return candidate_from_text(text, username)


def fetch_bing(username):
    query = f'"{username}" chess'
    url = f"https://www.bing.com/search?q={quote_plus(query)}&setlang=en-US&cc=US"
    proc = subprocess.run(
        [
            "curl",
            "-L",
            "-s",
            "--compressed",
            "-A",
            "Mozilla/5.0 research",
            "-w",
            "\n__HTTP_STATUS__%{http_code}",
            url,
        ],
        check=False,
        capture_output=True,
        text=True,
        timeout=35,
    )
    body, marker, status_text = proc.stdout.rpartition("\n__HTTP_STATUS__")
    status = int(status_text.strip()) if marker and status_text.strip().isdigit() else None
    if proc.returncode != 0:
        raise RuntimeError(proc.stderr.strip() or f"curl exited {proc.returncode}")
    return status, body


def load_cache():
    cache = {}
    if BING_CACHE.exists():
        with BING_CACHE.open(encoding="utf-8") as fh:
            for line in fh:
                if line.strip():
                    item = json.loads(line)
                    cache[item["player_name"]] = item
    return cache


def append_cache(item):
    with BING_CACHE.open("a", encoding="utf-8") as fh:
        fh.write(json.dumps(item, ensure_ascii=False, sort_keys=True) + "\n")


def cache_is_good(item):
    return bool(item.get("ok") and item.get("status") == 200 and item.get("results"))


def load_existing_findings():
    rows = []
    if CHESSCOM_FINDINGS.exists():
        with CHESSCOM_FINDINGS.open(newline="", encoding="utf-8") as fh:
            for row in csv.DictReader(fh):
                rows.append(
                    {
                        "player_name": row["player_name"],
                        "real_name": row["real_name"],
                        "source": row["source"],
                        "source_type": row["source_type"],
                        "evidence_title": "",
                        "evidence_snippet": "",
                    }
                )
    return rows


def main():
    input_path = BASE_INPUT if BASE_INPUT.exists() else ORIGINAL_INPUT
    rows, fieldnames = read_rows(input_path)
    missing = [r["player_name"] for r in rows if blank(r.get("real_name"))]
    cache = load_cache()
    now = datetime.now(timezone.utc).isoformat()

    for idx, username in enumerate(missing, 1):
        if username in cache and cache_is_good(cache[username]):
            continue
        item = {
            "player_name": username,
            "fetched_at": now,
            "ok": False,
            "status": None,
            "results": [],
            "error": None,
        }
        try:
            status, page = fetch_bing(username)
            item["status"] = status
            item["results"] = parse_results(page)
            item["ok"] = status == 200
        except Exception as exc:
            item["error"] = repr(exc)
        append_cache(item)
        cache[username] = item
        if idx % 25 == 0:
            print(f"queried {idx}/{len(missing)}")
        time.sleep(0.5)

    bing_findings = []
    name_by_player = {}
    for username in missing:
        item = cache.get(username, {})
        for result in item.get("results") or []:
            candidate = extract_candidate(result, username)
            if candidate:
                name_by_player[username] = candidate
                bing_findings.append(
                    {
                        "player_name": username,
                        "real_name": candidate,
                        "source": result["url"],
                        "source_type": "Bing result for indexed Chess.com member page",
                        "evidence_title": result["title"],
                        "evidence_snippet": result["snippet"],
                    }
                )
                break

    with BING_FINDINGS.open("w", newline="", encoding="utf-8") as fh:
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
        writer.writerows(bing_findings)

    combined = load_existing_findings() + bing_findings
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

    print(f"remaining searched: {len(missing)}")
    print(f"names found from Bing indexed Chess.com results: {len(bing_findings)}")
    print(f"combined findings: {len(combined)}")
    print(f"blank real_name in output: {sum(1 for r in output_rows if blank(r.get('real_name')))}")
    print(f"wrote {BING_FINDINGS}")
    print(f"wrote {COMBINED_FINDINGS}")
    print(f"wrote {OUTPUT}")


if __name__ == "__main__":
    main()
