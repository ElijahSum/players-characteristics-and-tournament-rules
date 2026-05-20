import asyncio
import logging
import os
import re
import time
from datetime import datetime

import pandas as pd
from playwright.async_api import async_playwright, TimeoutError as PlaywrightTimeoutError

INPUT_FILE = "new_missed_links_2026_third_iteration.csv"
OUTPUT_DIR = "scrape_outputs"
PROFILE_DIR = "playwright_chess_profile"
STORAGE_STATE_FILE = os.path.join(OUTPUT_DIR, "chess_storage_state.json")

SELECTOR = ".game-overview-row+ .game-overview-row .review-rating-component span"

N_WORKERS = 1
SAVE_EVERY = 5
PAGE_GOTO_TIMEOUT = 45000
TEXT_WAIT_SECONDS = 15
POLL_INTERVAL = 2
HEADLESS = False

PAUSE_EVERY_N_GAMES = 10
PAUSE_DURATION_SECONDS = 50
DEFAULT_BETWEEN_REQUESTS_SLEEP = 2
ANTIBOT_COOLDOWN_SECONDS = 15
ANTIBOT_REFRESH_DELAY_SECONDS = 20
ANTIBOT_MANUAL_SOLVE_TIMEOUT_SECONDS = 120
NO_MOVES_PAUSE_SECONDS = 5

FINAL_CSV = os.path.join(OUTPUT_DIR, "accuracy_results.csv")
FINAL_PARQUET = os.path.join(OUTPUT_DIR, "accuracy_results.parquet")
LOG_FILE = os.path.join(OUTPUT_DIR, "scrape_accuracy.log")
CHECKPOINT_FILE_RE = re.compile(r"worker_(\d+)_checkpoint_(\d+)\.(csv|parquet)$")

os.makedirs(OUTPUT_DIR, exist_ok=True)

logger = logging.getLogger("scraper")
logger.setLevel(logging.INFO)
logger.handlers.clear()

fmt = logging.Formatter("%(asctime)s | %(levelname)s | %(message)s")

fh = logging.FileHandler(LOG_FILE, mode="a", encoding="utf-8")
fh.setFormatter(fmt)
fh.setLevel(logging.INFO)

sh = logging.StreamHandler()
sh.setFormatter(fmt)
sh.setLevel(logging.INFO)

logger.addHandler(fh)
logger.addHandler(sh)


def clean_accuracy(x):
    if x is None:
        return None
    s = str(x).strip().replace("%", "").replace(",", ".")
    try:
        return float(s)
    except Exception:
        return None


def find_latest_worker_checkpoint(worker_id):
    latest = None

    for filename in os.listdir(OUTPUT_DIR):
        match = CHECKPOINT_FILE_RE.fullmatch(filename)
        if not match:
            continue

        file_worker_id = int(match.group(1))
        processed_count = int(match.group(2))
        extension = match.group(3)

        if file_worker_id != worker_id:
            continue

        candidate = {
            "processed_count": processed_count,
            "extension": extension,
            "path": os.path.join(OUTPUT_DIR, filename),
        }

        if latest is None:
            latest = candidate
            continue

        if candidate["processed_count"] > latest["processed_count"]:
            latest = candidate
            continue

        if (
            candidate["processed_count"] == latest["processed_count"]
            and candidate["extension"] == "csv"
            and latest["extension"] != "csv"
        ):
            latest = candidate

    return latest


def read_checkpoint_dataframe(path, extension):
    if extension == "csv":
        return pd.read_csv(path)
    return pd.read_parquet(path)


def load_worker_checkpoint(worker_id):
    checkpoint = find_latest_worker_checkpoint(worker_id)
    if checkpoint is None:
        return [], set(), 0

    path = checkpoint["path"]
    extension = checkpoint["extension"]

    try:
        df_prev = read_checkpoint_dataframe(path, extension)
    except Exception:
        logger.exception(
            f"[worker {worker_id}] failed to load checkpoint | path={path}"
        )
        return [], set(), 0

    if "links" not in df_prev.columns:
        logger.warning(
            f"[worker {worker_id}] checkpoint missing 'links' column | path={path}"
        )
        return [], set(), 0

    df_prev = df_prev[df_prev["links"].notna()].copy()
    df_prev["links"] = df_prev["links"].astype(str).str.strip()
    df_prev = df_prev[df_prev["links"] != ""]
    df_prev = df_prev.drop_duplicates(subset=["links"], keep="last").reset_index(drop=True)

    results = df_prev.to_dict("records")
    scraped_links = set(df_prev["links"].tolist())
    rows_loaded = len(results)
    processed_count = checkpoint["processed_count"]

    if rows_loaded != processed_count:
        logger.warning(
            f"[worker {worker_id}] checkpoint row count mismatch | "
            f"file_count={processed_count} | loaded_rows={rows_loaded} | path={path}"
        )
        processed_count = rows_loaded

    logger.info(
        f"[worker {worker_id}] resuming from checkpoint | processed={processed_count} "
        f"| path={path}"
    )
    return results, scraped_links, processed_count

async def get_page_html_lower(page):
    try:
        return (await page.content()).lower()
    except Exception:
        return ""
    
async def get_visible_text_lower(page):
    try:
        text = await page.locator("body").inner_text()
        return text.lower()
    except Exception:
        return ""
    
async def is_logged_out(page):
    html = await get_page_html_lower(page)
    return (
        "user-logged-out" in html
        or "login_and_go?returnurl=" in html
        or 'aria-label="log in"' in html
    )
async def page_has_accuracy(page):
    """
    Fast check: if the page already contains two numeric accuracy values
    in the target selector, then it is not an anti-bot page.
    """
    try:
        loc = page.locator(SELECTOR)
        n = await loc.count()
        if n < 2:
            return False

        texts = [t.strip() for t in await loc.all_inner_texts()]
        nums = []

        for t in texts:
            if re.search(r"\d", t or ""):
                val = clean_accuracy(t)
                if val is not None:
                    nums.append(val)

        return len(nums) >= 2
    except Exception:
        return False


async def get_analysis_pgn(page):
    try:
        return await page.evaluate("window.chesscom?.analysis?.pgn ?? null")
    except Exception:
        return None


def count_plies_from_current_position(pgn):
    if not pgn:
        return None

    match = re.search(r'\[CurrentPosition\s+"([^"]+)"\]', pgn)
    if not match:
        return None

    fen_parts = match.group(1).strip().split()
    if len(fen_parts) < 6:
        return None

    active_color = fen_parts[1]
    fullmove_text = fen_parts[5]

    try:
        fullmove_number = int(fullmove_text)
    except Exception:
        return None

    if fullmove_number < 1 or active_color not in {"w", "b"}:
        return None

    return (fullmove_number - 1) * 2 + (1 if active_color == "b" else 0)


def count_plies_from_movetext(pgn):
    if not pgn:
        return None

    movetext = re.sub(r"\[[^\]]*\]\s*", " ", pgn)
    movetext = re.sub(r"\{[^{}]*\}", " ", movetext)
    movetext = re.sub(r";[^\r\n]*", " ", movetext)
    movetext = re.sub(r"\$\d+", " ", movetext)

    previous = None
    while movetext != previous:
        previous = movetext
        movetext = re.sub(r"\([^()]*\)", " ", movetext)

    tokens = []
    for token in movetext.split():
        if token in {"1-0", "0-1", "1/2-1/2", "*"}:
            continue
        if re.fullmatch(r"\d+\.(\.\.)?", token):
            continue
        if token == "...":
            continue
        tokens.append(token)

    if not tokens:
        return 0

    return len(tokens)


def get_pgn_ply_count(pgn):
    ply_count = count_plies_from_current_position(pgn)
    if ply_count is not None:
        return ply_count
    return count_plies_from_movetext(pgn)

def looks_like_antibot_page(title, html, visible_text):
    title_blob = (title or "").lower()
    html_blob = (html or "").lower()
    text_blob = (visible_text or "").lower()

    title_signals = [
        "just a moment...",
        "attention required!",
    ]
    text_signals = [
        "verifying you are human",
        "this website uses a security service to protect against malicious bots",
        "this page is displayed while the website verifies you are not a bot",
        "enable javascript and cookies to continue",
        "performance and security by cloudflare",
    ]
    html_signals = [
        "/cdn-cgi/challenge-platform/",
        "window._cf_chl_opt",
        "__cf_chl_",
        "challenges.cloudflare.com/turnstile",
        'id="challenge-error-text"',
        'id="challenge-success-text"',
    ]

    if any(signal in title_blob for signal in title_signals):
        return True

    text_matches = sum(signal in text_blob for signal in text_signals)
    html_matches = sum(signal in html_blob for signal in html_signals)
    return text_matches >= 2 and html_matches >= 1


async def is_antibot_page(page):
    html = await get_page_html_lower(page)
    visible_text = await get_visible_text_lower(page)
    title = ""
    try:
        title = (await page.title()).lower()
    except Exception:
        pass

    return looks_like_antibot_page(title, html, visible_text)



async def wait_until_not_antibot(page, link, worker_id, row_num, timeout_seconds=120):
    """
    Wait until the anti-bot page disappears after a refresh retry.
    Returns True if cleared, False if timeout.
    """
    start = time.perf_counter()
    last_log_second = -1

    while time.perf_counter() - start < timeout_seconds:
        try:
            current_url = page.url
        except Exception:
            current_url = "unknown"

        # If accuracy is already present, stop immediately.
        if await page_has_accuracy(page):
            logger.info(
                f"[worker {worker_id}] row={row_num} | accuracy appeared while waiting | url={current_url}"
            )
            return True

        antibot = await is_antibot_page(page)

        if not antibot:
            logger.info(
                f"[worker {worker_id}] row={row_num} | anti-bot cleared | url={current_url}"
            )
            return True

        elapsed = int(time.perf_counter() - start)
        if elapsed != last_log_second and elapsed % 5 == 0:
            last_log_second = elapsed
            logger.warning(
                f"[worker {worker_id}] row={row_num} | waiting for anti-bot page to clear after refresh "
                f"| waited={elapsed}s | url={current_url}"
            )

        await asyncio.sleep(1)

    logger.error(
        f"[worker {worker_id}] row={row_num} | anti-bot page did not clear after refresh "
        f"within {timeout_seconds}s | link={link}"
    )
    return False


async def recover_from_antibot(page, link, worker_id, row_num):
    try:
        captcha_url = page.url
    except Exception:
        captcha_url = link

    if not captcha_url:
        captcha_url = link

    logger.warning(
        f"[worker {worker_id}] row={row_num} | anti-bot page detected | "
        f"waiting {ANTIBOT_REFRESH_DELAY_SECONDS}s before refresh | url={captcha_url}"
    )
    await asyncio.sleep(ANTIBOT_REFRESH_DELAY_SECONDS)

    if await page_has_accuracy(page):
        logger.info(
            f"[worker {worker_id}] row={row_num} | accuracy appeared during anti-bot grace period | "
            f"url={page.url}"
        )
        return True

    if not await is_antibot_page(page):
        logger.info(
            f"[worker {worker_id}] row={row_num} | anti-bot cleared during grace period | "
            f"skipping refresh | url={page.url}"
        )
        return True

    logger.warning(
        f"[worker {worker_id}] row={row_num} | refreshing anti-bot page | url={captcha_url}"
    )
    await page.goto(
        captcha_url, wait_until="domcontentloaded", timeout=PAGE_GOTO_TIMEOUT
    )
    await asyncio.sleep(2)

    if await page_has_accuracy(page):
        logger.info(
            f"[worker {worker_id}] row={row_num} | accuracy appeared after anti-bot refresh | "
            f"url={page.url}"
        )
        return True

    if not await is_antibot_page(page):
        logger.info(
            f"[worker {worker_id}] row={row_num} | anti-bot cleared after refresh | url={page.url}"
        )
        return True

    logger.warning(
        f"[worker {worker_id}] row={row_num} | anti-bot page still present after refresh | "
        f"waiting up to {ANTIBOT_MANUAL_SOLVE_TIMEOUT_SECONDS}s for manual solve | url={page.url}"
    )
    return await wait_until_not_antibot(
        page,
        link,
        worker_id,
        row_num,
        timeout_seconds=ANTIBOT_MANUAL_SOLVE_TIMEOUT_SECONDS,
    )




async def poll_accuracy(page, timeout_seconds=TEXT_WAIT_SECONDS):
    start = time.perf_counter()

    while time.perf_counter() - start < timeout_seconds:
        try:
            loc = page.locator(SELECTOR)
            n = await loc.count()

            if n >= 2:
                texts = [t.strip() for t in await loc.all_inner_texts()]
                nums = []

                for t in texts:
                    if re.search(r"\d", t or ""):
                        val = clean_accuracy(t)
                        if val is not None:
                            nums.append(val)

                if len(nums) >= 2:
                    return nums[:2], texts
        except Exception:
            pass

        await asyncio.sleep(POLL_INTERVAL)

    return None, None


async def scrape_one(page, link, row_num, worker_id):
    started = time.perf_counter()

    await page.goto(link, wait_until="domcontentloaded", timeout=PAGE_GOTO_TIMEOUT)
    await asyncio.sleep(2)

    pgn = await get_analysis_pgn(page)
    ply_count = get_pgn_ply_count(pgn)

    if ply_count is not None and ply_count <= 1:
        logger.info(
            f"[worker {worker_id}] row={row_num} | short game detected | "
            f"plies={ply_count} | pausing {NO_MOVES_PAUSE_SECONDS}s | {link}"
        )
        await asyncio.sleep(NO_MOVES_PAUSE_SECONDS)
        return {
            "links": link,
            "accuracy1": None,
            "accuracy2": None,
            "ok": False,
            "worker_id": worker_id,
            "error_type": "no_moves",
            "elapsed_seconds": round(time.perf_counter() - started, 2),
            "scraped_at": datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
        }

    # 1. Best case: data is already there
    if await page_has_accuracy(page):
        nums, texts = await poll_accuracy(page)
        logger.info(
            f"[worker {worker_id}] row={row_num} | success (immediate) | acc1={nums[0]} | acc2={nums[1]} | {link}"
        )
        return {
            "links": link,
            "accuracy1": nums[0],
            "accuracy2": nums[1],
            "ok": True,
            "worker_id": worker_id,
            "error_type": None,
            "elapsed_seconds": round(time.perf_counter() - started, 2),
            "scraped_at": datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
        }

    # 2. Only now check anti-bot
    if await is_antibot_page(page):
        cleared = await recover_from_antibot(page, link, worker_id, row_num)

        if not cleared:
            html_path = os.path.join(
                OUTPUT_DIR, f"antibot_timeout_worker{worker_id}_row_{row_num}.html"
            )
            with open(html_path, "w", encoding="utf-8") as f:
                f.write(await page.content())

            return {
                "links": link,
                "accuracy1": None,
                "accuracy2": None,
                "ok": False,
                "worker_id": worker_id,
                "error_type": "antibot_timeout",
                "elapsed_seconds": round(time.perf_counter() - started, 2),
                "scraped_at": datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            }

        await asyncio.sleep(2)

    # 3. Logged out check
    if await is_logged_out(page):
        logger.warning(f"[worker {worker_id}] row={row_num} | logged out detected | {link}")
        return {
            "links": link,
            "accuracy1": None,
            "accuracy2": None,
            "ok": False,
            "worker_id": worker_id,
            "error_type": "logged_out",
            "elapsed_seconds": round(time.perf_counter() - started, 2),
            "scraped_at": datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
        }

    # 4. Normal polling
    nums, texts = await poll_accuracy(page)

    if nums is None:
        html_path = os.path.join(OUTPUT_DIR, f"failed_worker{worker_id}_row_{row_num}.html")
        with open(html_path, "w", encoding="utf-8") as f:
            f.write(await page.content())

        logger.warning(
            f"[worker {worker_id}] row={row_num} | no accuracy found | {link} | saved={html_path}"
        )
        return {
            "links": link,
            "accuracy1": None,
            "accuracy2": None,
            "ok": False,
            "worker_id": worker_id,
            "error_type": "no_accuracy_text",
            "elapsed_seconds": round(time.perf_counter() - started, 2),
            "scraped_at": datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
        }

    logger.info(
        f"[worker {worker_id}] row={row_num} | success | acc1={nums[0]} | acc2={nums[1]} | {link}"
    )
    return {
        "links": link,
        "accuracy1": nums[0],
        "accuracy2": nums[1],
        "ok": True,
        "worker_id": worker_id,
        "error_type": None,
        "elapsed_seconds": round(time.perf_counter() - started, 2),
        "scraped_at": datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
    }


def save_worker_checkpoint(results, worker_id, processed_count):
    if not results:
        return

    df_tmp = pd.DataFrame(results)
    csv_path = os.path.join(OUTPUT_DIR, f"worker_{worker_id}_checkpoint_{processed_count}.csv")
    parquet_path = os.path.join(OUTPUT_DIR, f"worker_{worker_id}_checkpoint_{processed_count}.parquet")

    df_tmp.to_csv(csv_path, index=False)
    df_tmp.to_parquet(parquet_path, index=False)

    logger.info(
        f"[worker {worker_id}] checkpoint saved after {processed_count} links"
    )


async def worker(playwright, links, worker_id, prior_results=None, start_index=0):
    results = list(prior_results or [])
    remaining_links = len(links)
    browser = None

    logger.info(
        f"[worker {worker_id}] started | remaining_links={remaining_links} | "
        f"resume_from={start_index}"
    )

    if not links:
        logger.info(
            f"[worker {worker_id}] nothing to scrape | processed={len(results)}"
        )
        return results

    browser = await playwright.chromium.launch(
        headless=HEADLESS,
        args=[
            "--disable-blink-features=AutomationControlled",
            "--no-sandbox",
            "--disable-dev-shm-usage",
        ],
    )

    context = await browser.new_context(
        storage_state=STORAGE_STATE_FILE,
        viewport={"width": 1440, "height": 900},
        locale="en-US",
        user_agent=(
            "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
            "AppleWebKit/537.36 (KHTML, like Gecko) "
            "Chrome/123.0.0.0 Safari/537.36"
        ),
    )

    page = await context.new_page()

    try:
        for i, link in enumerate(links, start=start_index + 1):
            try:
                result = await scrape_one(page, link, i, worker_id)
            except Exception as e:
                logger.exception(f"[worker {worker_id}] row={i} | exception | {link}")
                result = {
                    "links": link,
                    "accuracy1": None,
                    "accuracy2": None,
                    "ok": False,
                    "worker_id": worker_id,
                    "error_type": type(e).__name__,
                    "elapsed_seconds": None,
                    "scraped_at": datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
                }

            results.append(result)

            if i % SAVE_EVERY == 0:
                save_worker_checkpoint(results, worker_id, i)

            # Long cooldown every 10 processed games
            if i % 10 == 0:
                logger.info(
                    f"[worker {worker_id}] processed {i} games | pausing for 10 seconds"
                )
                await asyncio.sleep(10)

            # Extra cooldown after anti-bot problems
            elif result.get("error_type") in {"antibot_timeout", "antibot_stuck"}:
                logger.warning(
                    f"[worker {worker_id}] row={i} | cooling down for 15 seconds after anti-bot trigger"
                )
                await asyncio.sleep(15)

            # Small default pause between requests
            else:
                await asyncio.sleep(0.3)

    finally:
        await page.close()
        await context.close()
        if browser is not None:
            await browser.close()

    save_worker_checkpoint(results, worker_id, len(results))
    logger.info(f"[worker {worker_id}] finished | processed={len(results)}")
    return results


async def create_auth_state(p):
    """
    First run only:
    open persistent profile, let user log in manually, save storage_state.
    """
    if os.path.exists(STORAGE_STATE_FILE):
        logger.info(f"Using existing storage state: {STORAGE_STATE_FILE}")
        return

    logger.info("No storage state found. Opening persistent browser for manual login.")

    context = await p.chromium.launch_persistent_context(
        PROFILE_DIR,
        headless=False,
        viewport={"width": 1440, "height": 900},
    )

    page = await context.new_page()
    await page.goto("https://www.chess.com", wait_until="domcontentloaded")

    print("\nFIRST RUN ONLY:")
    print("1. A browser window has opened.")
    print("2. Log in to Chess.com manually in that window.")
    print("3. When login is fully completed, return here and press Enter.")
    input()

    await context.storage_state(path=STORAGE_STATE_FILE)
    logger.info(f"Saved storage state to: {STORAGE_STATE_FILE}")

    await context.close()


async def main():
    df = pd.read_csv(INPUT_FILE)
    df = df[df["links"].notna()].copy()
    df["links"] = df["links"].astype(str).str.strip()
    df = df[df["links"] != ""]
    df = df.drop_duplicates(subset=["links"]).reset_index(drop=True)

    links = df["links"].tolist()
    logger.info(f"Total links: {len(links)}")

    chunks = [links[i::N_WORKERS] for i in range(N_WORKERS)]
    worker_inputs = []

    for worker_id, chunk in enumerate(chunks):
        prior_results, scraped_links, processed_count = load_worker_checkpoint(worker_id)
        remaining_links = [link for link in chunk if link not in scraped_links]

        logger.info(
            f"[worker {worker_id}] assigned {len(chunk)} links | "
            f"already_scraped={len(scraped_links)} | remaining={len(remaining_links)}"
        )

        worker_inputs.append(
            {
                "links": remaining_links,
                "worker_id": worker_id,
                "prior_results": prior_results,
                "start_index": processed_count,
            }
        )

    async with async_playwright() as p:
        await create_auth_state(p)

        all_results = await asyncio.gather(
            *[
                worker(
                    p,
                    worker_input["links"],
                    worker_id=worker_input["worker_id"],
                    prior_results=worker_input["prior_results"],
                    start_index=worker_input["start_index"],
                )
                for worker_input in worker_inputs
            ]
        )

    flat = [item for chunk in all_results for item in chunk]
    out = pd.DataFrame(flat)

    out = out.sort_values(by=["ok"], ascending=[False]).drop_duplicates(subset=["links"])
    final = df.merge(out, on="links", how="left")

    final.to_csv(FINAL_CSV, index=False)
    final.to_parquet(FINAL_PARQUET, index=False)

    logger.info(f"finished | rows={len(final)} | saved={FINAL_CSV} and {FINAL_PARQUET}")


if __name__ == "__main__":
    asyncio.run(main())
