#!/usr/bin/env python3
import csv
import html
import json
import re
import subprocess
import time
from datetime import datetime, timezone
from pathlib import Path


BASE_INPUT = Path("players_final_data_merged_with_chesscom_names.csv")
ORIGINAL_INPUT = Path("players_final_data_merged.csv")
CHESSCOM_API_FINDINGS = Path("real_name_findings_chesscom.csv")
BING_FINDINGS = Path("real_name_findings_bing.csv")
DDG_FINDINGS = Path("real_name_findings_ddg.csv")
HTML_FINDINGS = Path("real_name_findings_chesscom_html.csv")
COMBINED_FINDINGS = Path("real_name_findings_combined.csv")
OUTPUT = Path("players_final_data_merged_with_real_names.csv")

PROFILE_HTML_CACHE = Path("chesscom_profile_html_cache.jsonl")
MASTER_HTML_CACHE = Path("chesscom_master_html_cache.jsonl")


def blank(value):
    return not (value or "").strip()


def read_rows(path):
    with path.open(newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        return list(reader), reader.fieldnames or []


def run_curl(url):
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


def load_cache(path, key_name):
    cache = {}
    if not path.exists():
        return cache
    with path.open(encoding="utf-8") as fh:
        for line in fh:
            if not line.strip():
                continue
            item = json.loads(line)
            cache[item[key_name]] = item
    return cache


def append_cache(path, item):
    with path.open("a", encoding="utf-8") as fh:
        fh.write(json.dumps(item, ensure_ascii=False, sort_keys=True) + "\n")


def js_string(value):
    try:
        return json.loads(f'"{value}"')
    except json.JSONDecodeError:
        return html.unescape(value)


def field_from_js(page, field):
    match = re.search(rf"\b{re.escape(field)}:\s*\"((?:\\.|[^\"\\])*)\"", page)
    return js_string(match.group(1)).strip() if match else ""


def meta_content(page, property_name):
    patterns = [
        rf'<meta\s+property="{re.escape(property_name)}"\s+content="([^"]*)"',
        rf'<meta\s+content="([^"]*)"\s+property="{re.escape(property_name)}"',
        rf'<meta\s+name="{re.escape(property_name)}"\s+content="([^"]*)"',
    ]
    for pattern in patterns:
        match = re.search(pattern, page, flags=re.I)
        if match:
            return html.unescape(match.group(1)).strip()
    return ""


def title_text(page):
    match = re.search(r"<title>(.*?)</title>", page, flags=re.I | re.S)
    return re.sub(r"\s+", " ", html.unescape(match.group(1))).strip() if match else ""


def clean_name(value, username):
    value = re.sub(r"\s+", " ", html.unescape(value or "")).strip(" -–—|")
    if not value:
        return ""
    lower = value.lower()
    if any(bit in lower for bit in ["chess.com", "top chess players", "chess profile"]):
        return ""
    if username.lower() == lower:
        return ""
    if re.search(r"[@_/]|\d", value):
        return ""
    if len(value) > 100:
        return ""
    tokens = [token for token in re.split(r"\s+", value) if token]
    if len(tokens) < 2:
        return ""
    return value


def name_from_player_page(page, username):
    og_title = meta_content(page, "og:title")
    if " | " in og_title:
        candidate = og_title.split(" | ", 1)[0]
        candidate = clean_name(candidate, username)
        if candidate:
            return candidate

    match = re.search(r'"name"\s*:\s*"((?:\\.|[^"\\])*)"', page)
    if match:
        candidate = clean_name(js_string(match.group(1)), username)
        if candidate:
            return candidate

    title = title_text(page)
    if " | " in title:
        candidate = clean_name(title.split(" | ", 1)[0], username)
        if candidate:
            return candidate
    return ""


def finding_from_profile(username, page, master_cache, now):
    first = clean_name(" ".join([field_from_js(page, "firstName"), field_from_js(page, "lastName")]), username)
    if first:
        return {
            "player_name": username,
            "real_name": first,
            "source": f"https://www.chess.com/member/{username}",
            "source_type": "Chess.com profile page firstName/lastName fields",
            "evidence_title": title_text(page),
            "evidence_snippet": "Profile HTML contains non-empty firstName/lastName fields.",
        }

    slug = field_from_js(page, "masterPlayerUrl")
    if not slug:
        return None

    url = f"https://www.chess.com/players/{slug}"
    item = master_cache.get(slug)
    if not item or item.get("status") != 200:
        item = {
            "slug": slug,
            "fetched_at": now,
            "status": None,
            "ok": False,
            "html": "",
            "error": None,
        }
        try:
            status, body = run_curl(url)
            item.update({"status": status, "ok": status == 200, "html": body})
        except Exception as exc:
            item["error"] = repr(exc)
        append_cache(MASTER_HTML_CACHE, item)
        master_cache[slug] = item
        time.sleep(0.2)

    if item.get("status") != 200:
        return None

    name = name_from_player_page(item.get("html") or "", username)
    if not name:
        return None
    return {
        "player_name": username,
        "real_name": name,
        "source": url,
        "source_type": "Chess.com master player page linked from profile masterPlayerUrl field",
        "evidence_title": title_text(item.get("html") or ""),
        "evidence_snippet": f"Member profile links masterPlayerUrl={slug}.",
    }


def load_findings(path):
    if not path.exists():
        return []
    rows = []
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


def dedupe_findings(rows):
    out = []
    seen = set()
    for row in rows:
        key = row["player_name"]
        if not key or key in seen:
            continue
        seen.add(key)
        out.append(row)
    return out


def main():
    input_path = BASE_INPUT if BASE_INPUT.exists() else ORIGINAL_INPUT
    rows, fieldnames = read_rows(input_path)
    missing = [row["player_name"] for row in rows if blank(row.get("real_name"))]
    profile_cache = load_cache(PROFILE_HTML_CACHE, "player_name")
    master_cache = load_cache(MASTER_HTML_CACHE, "slug")
    now = datetime.now(timezone.utc).isoformat()

    for idx, username in enumerate(missing, 1):
        item = profile_cache.get(username)
        if item and item.get("status") == 200:
            continue
        item = {
            "player_name": username,
            "fetched_at": now,
            "status": None,
            "ok": False,
            "html": "",
            "error": None,
        }
        try:
            status, body = run_curl(f"https://www.chess.com/member/{username}")
            item.update({"status": status, "ok": status == 200, "html": body})
        except Exception as exc:
            item["error"] = repr(exc)
        append_cache(PROFILE_HTML_CACHE, item)
        profile_cache[username] = item
        if idx % 25 == 0:
            print(f"queried member pages {idx}/{len(missing)}")
        time.sleep(0.2)

    html_findings = []
    for username in missing:
        item = profile_cache.get(username, {})
        if item.get("status") != 200:
            continue
        finding = finding_from_profile(username, item.get("html") or "", master_cache, now)
        if finding:
            html_findings.append(finding)

    with HTML_FINDINGS.open("w", newline="", encoding="utf-8") as fh:
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
        writer.writerows(html_findings)

    combined = dedupe_findings(
        load_findings(CHESSCOM_API_FINDINGS)
        + load_findings(BING_FINDINGS)
        + load_findings(DDG_FINDINGS)
        + html_findings
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

    print(f"remaining searched: {len(missing)}")
    print(f"names found from Chess.com HTML/master pages: {len(html_findings)}")
    print(f"combined findings: {len(combined)}")
    print(f"blank real_name in output: {sum(1 for row in output_rows if blank(row.get('real_name')))}")
    print(f"wrote {HTML_FINDINGS}")
    print(f"wrote {COMBINED_FINDINGS}")
    print(f"wrote {OUTPUT}")


if __name__ == "__main__":
    main()
