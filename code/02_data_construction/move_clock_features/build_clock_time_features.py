#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import math
import sys
import time
from dataclasses import dataclass, field
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
DEFAULT_CPS = [
    ROOT / "outputs" / "whole_dataset_2022_2024" / "centipawn_loss_nodes2000_watch" / "centipawn_loss_watch.csv",
    ROOT / "outputs" / "whole_dataset_2024_2026" / "centipawn_loss_nodes2000_watch" / "centipawn_loss_watch.csv",
]
DEFAULT_OUTPUT = ROOT / "analysis_outputs" / "clock_time_mechanisms_2022_2026" / "player_game_clock_features.csv"
PHASES = ("opening_1_10", "early_middlegame_11_20", "late_middlegame_21_35", "endgame_36_plus")
THRESHOLDS = (5.0, 10.0, 30.0)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Build compact player-game clock and quality features from centipawn CSVs."
    )
    parser.add_argument("--centipawn-csv", action="append", type=Path, default=[])
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--cp-loss-cap", type=float, default=1000.0)
    parser.add_argument("--progress-every-games", type=int, default=25000)
    parser.add_argument("--limit-games", type=int, default=0)
    return parser.parse_args()


def safe_float(value: str, default: float = math.nan) -> float:
    try:
        return float(value) if value != "" else default
    except ValueError:
        return default


def safe_int(value: str, default: int = 0) -> int:
    try:
        return int(float(value)) if value != "" else default
    except ValueError:
        return default


def player_result_from_game(result: str, color: str) -> float:
    if result == "1-0":
        return 1.0 if color == "white" else 0.0
    if result == "0-1":
        return 0.0 if color == "white" else 1.0
    if result in {"1/2-1/2", "0.5-0.5"}:
        return 0.5
    return math.nan


def iter_game_groups(path: Path):
    with path.open("r", encoding="utf-8", newline="") as f:
        reader = csv.DictReader(f)
        required = {
            "game_id",
            "date",
            "result",
            "white",
            "black",
            "ply",
            "fullmove_number",
            "phase",
            "mover_color",
            "mover",
            "cp_loss",
            "eval_before_mover_cp",
            "eval_after_mover_cp",
            "time_before_move",
            "time_after_move",
            "time_spent",
        }
        missing = required.difference(reader.fieldnames or [])
        if missing:
            raise RuntimeError(f"{path} is missing required columns: {sorted(missing)}")

        current_game_id = None
        rows: list[dict[str, str]] = []
        for row in reader:
            game_id = row["game_id"]
            if current_game_id is None:
                current_game_id = game_id
            if game_id != current_game_id:
                yield current_game_id, rows
                current_game_id = game_id
                rows = []
            rows.append(row)
        if current_game_id is not None:
            yield current_game_id, rows


@dataclass
class PlayerClockAgg:
    game_id: str = ""
    date: str = ""
    result: str = ""
    white: str = ""
    black: str = ""
    mover_color: str = ""
    player_name: str = ""
    opponent_name: str = ""
    player_result_pgn: float = math.nan

    n_moves: int = 0
    max_ply: int = 0
    sum_cp_loss_cap: float = 0.0
    sum_cp_loss_raw: float = 0.0
    blunders: int = 0
    mistakes: int = 0
    inaccuracies: int = 0

    total_time_spent: float = 0.0
    sum_time_spent_sq: float = 0.0
    max_time_spent: float = 0.0
    sum_time_before: float = 0.0
    sum_time_after: float = 0.0
    min_time_before: float = math.inf
    min_time_after: float = math.inf
    final_time_after: float = math.nan
    time_after_move_10: float = math.nan
    time_after_move_20: float = math.nan
    time_after_move_30: float = math.nan

    low_before_5: int = 0
    low_before_10: int = 0
    low_before_30: int = 0
    low_after_5: int = 0
    low_after_10: int = 0
    low_after_30: int = 0
    first_low_after_10_ply: int = 0
    first_low_after_30_ply: int = 0

    fast_moves_le_1s: int = 0
    think_moves_ge_10s: int = 0
    think_moves_ge_20s: int = 0
    think_moves_ge_30s: int = 0

    cp_low_before_10_sum: float = 0.0
    cp_nonlow_before_10_sum: float = 0.0
    n_low_before_10_cp: int = 0
    n_nonlow_before_10_cp: int = 0
    blunders_low_before_10: int = 0
    blunders_nonlow_before_10: int = 0

    critical_n: int = 0
    critical_time_sum: float = 0.0
    critical_cp_sum: float = 0.0
    critical_blunders: int = 0
    critical_low_before_10: int = 0

    winning_n: int = 0
    losing_n: int = 0
    first_winning_ply: int = 0
    first_losing_ply: int = 0
    first_winning_time_after: float = math.nan
    first_losing_time_after: float = math.nan
    first_winning_low_after_10: int = 0
    first_losing_low_after_10: int = 0

    first_blunder_ply: int = 0
    first_blunder_low_before_10: int = 0

    last10_n: int = 0
    last10_cp_sum: float = 0.0
    last10_time_sum: float = 0.0
    last10_low_before_10: int = 0
    last10_blunders: int = 0

    phase_n: dict[str, int] = field(default_factory=lambda: {p: 0 for p in PHASES})
    phase_time: dict[str, float] = field(default_factory=lambda: {p: 0.0 for p in PHASES})
    phase_cp: dict[str, float] = field(default_factory=lambda: {p: 0.0 for p in PHASES})
    phase_low_before_10: dict[str, int] = field(default_factory=lambda: {p: 0 for p in PHASES})
    phase_blunders: dict[str, int] = field(default_factory=lambda: {p: 0 for p in PHASES})

    def add_row(self, row: dict[str, str], *, final_ply: int, cp_loss_cap: float) -> None:
        if self.n_moves == 0:
            color = row["mover_color"]
            self.game_id = row["game_id"]
            self.date = row["date"]
            self.result = row["result"]
            self.white = row["white"]
            self.black = row["black"]
            self.mover_color = color
            self.player_name = row["mover"]
            self.opponent_name = row["black"] if color == "white" else row["white"]
            self.player_result_pgn = player_result_from_game(row["result"], color)

        ply = safe_int(row["ply"])
        fullmove = safe_int(row["fullmove_number"])
        phase = row["phase"] if row["phase"] in PHASES else "endgame_36_plus"
        cp_raw = safe_float(row["cp_loss"], 0.0)
        cp_cap = min(cp_raw, cp_loss_cap)
        eval_before = safe_float(row["eval_before_mover_cp"], math.nan)
        eval_after = safe_float(row["eval_after_mover_cp"], math.nan)
        time_before = safe_float(row["time_before_move"], math.nan)
        time_after = safe_float(row["time_after_move"], math.nan)
        time_spent = safe_float(row["time_spent"], math.nan)

        if math.isnan(time_before) or math.isnan(time_after) or math.isnan(time_spent):
            return

        self.n_moves += 1
        self.max_ply = max(self.max_ply, ply)
        self.sum_cp_loss_cap += cp_cap
        self.sum_cp_loss_raw += cp_raw
        self.blunders += int(cp_raw >= 200)
        self.mistakes += int(cp_raw >= 100)
        self.inaccuracies += int(cp_raw >= 50)

        self.total_time_spent += time_spent
        self.sum_time_spent_sq += time_spent * time_spent
        self.max_time_spent = max(self.max_time_spent, time_spent)
        self.sum_time_before += time_before
        self.sum_time_after += time_after
        self.min_time_before = min(self.min_time_before, time_before)
        self.min_time_after = min(self.min_time_after, time_after)
        self.final_time_after = time_after
        if fullmove == 10:
            self.time_after_move_10 = time_after
        elif fullmove == 20:
            self.time_after_move_20 = time_after
        elif fullmove == 30:
            self.time_after_move_30 = time_after

        self.low_before_5 += int(time_before <= 5)
        self.low_before_10 += int(time_before <= 10)
        self.low_before_30 += int(time_before <= 30)
        self.low_after_5 += int(time_after <= 5)
        self.low_after_10 += int(time_after <= 10)
        self.low_after_30 += int(time_after <= 30)
        if time_after <= 10 and self.first_low_after_10_ply == 0:
            self.first_low_after_10_ply = ply
        if time_after <= 30 and self.first_low_after_30_ply == 0:
            self.first_low_after_30_ply = ply

        self.fast_moves_le_1s += int(time_spent <= 1)
        self.think_moves_ge_10s += int(time_spent >= 10)
        self.think_moves_ge_20s += int(time_spent >= 20)
        self.think_moves_ge_30s += int(time_spent >= 30)

        if time_before <= 10:
            self.cp_low_before_10_sum += cp_cap
            self.n_low_before_10_cp += 1
            self.blunders_low_before_10 += int(cp_raw >= 200)
        else:
            self.cp_nonlow_before_10_sum += cp_cap
            self.n_nonlow_before_10_cp += 1
            self.blunders_nonlow_before_10 += int(cp_raw >= 200)

        is_critical = not math.isnan(eval_before) and abs(eval_before) <= 100
        if is_critical:
            self.critical_n += 1
            self.critical_time_sum += time_spent
            self.critical_cp_sum += cp_cap
            self.critical_blunders += int(cp_raw >= 200)
            self.critical_low_before_10 += int(time_before <= 10)

        if not math.isnan(eval_after) and eval_after >= 200:
            self.winning_n += 1
            if self.first_winning_ply == 0:
                self.first_winning_ply = ply
                self.first_winning_time_after = time_after
                self.first_winning_low_after_10 = int(time_after <= 10)
        if not math.isnan(eval_after) and eval_after <= -200:
            self.losing_n += 1
            if self.first_losing_ply == 0:
                self.first_losing_ply = ply
                self.first_losing_time_after = time_after
                self.first_losing_low_after_10 = int(time_after <= 10)

        if cp_raw >= 200 and self.first_blunder_ply == 0:
            self.first_blunder_ply = ply
            self.first_blunder_low_before_10 = int(time_before <= 10)

        if ply > final_ply - 10:
            self.last10_n += 1
            self.last10_cp_sum += cp_cap
            self.last10_time_sum += time_spent
            self.last10_low_before_10 += int(time_before <= 10)
            self.last10_blunders += int(cp_raw >= 200)

        self.phase_n[phase] += 1
        self.phase_time[phase] += time_spent
        self.phase_cp[phase] += cp_cap
        self.phase_low_before_10[phase] += int(time_before <= 10)
        self.phase_blunders[phase] += int(cp_raw >= 200)

    def as_row(self) -> dict[str, str]:
        n = self.n_moves
        mean_time = self.total_time_spent / n if n else math.nan
        mean_cp = self.sum_cp_loss_cap / n if n else math.nan
        var_time = self.sum_time_spent_sq / n - mean_time * mean_time if n else math.nan
        sd_time = math.sqrt(max(0.0, var_time)) if not math.isnan(var_time) else math.nan

        out = {
            "game_id": self.game_id,
            "date": self.date,
            "result": self.result,
            "player_name": self.player_name,
            "opponent_name": self.opponent_name,
            "mover_color": self.mover_color,
            "white": self.white,
            "black": self.black,
            "player_result_pgn": self.player_result_pgn,
            "n_moves": n,
            "max_ply": self.max_ply,
            "mean_cp_loss_cap": mean_cp,
            "total_cp_loss_cap": self.sum_cp_loss_cap,
            "blunder_rate": self.blunders / n if n else math.nan,
            "mistake_rate": self.mistakes / n if n else math.nan,
            "inaccuracy_rate": self.inaccuracies / n if n else math.nan,
            "total_time_spent": self.total_time_spent,
            "mean_time_spent": mean_time,
            "sd_time_spent": sd_time,
            "max_time_spent": self.max_time_spent,
            "mean_time_before": self.sum_time_before / n if n else math.nan,
            "mean_time_after": self.sum_time_after / n if n else math.nan,
            "min_time_before": self.min_time_before if self.min_time_before < math.inf else math.nan,
            "min_time_after": self.min_time_after if self.min_time_after < math.inf else math.nan,
            "final_time_after": self.final_time_after,
            "time_after_move_10": self.time_after_move_10,
            "time_after_move_20": self.time_after_move_20,
            "time_after_move_30": self.time_after_move_30,
            "low_before_5_share": self.low_before_5 / n if n else math.nan,
            "low_before_10_share": self.low_before_10 / n if n else math.nan,
            "low_before_30_share": self.low_before_30 / n if n else math.nan,
            "low_after_5_share": self.low_after_5 / n if n else math.nan,
            "low_after_10_share": self.low_after_10 / n if n else math.nan,
            "low_after_30_share": self.low_after_30 / n if n else math.nan,
            "first_low_after_10_ply": self.first_low_after_10_ply,
            "first_low_after_30_ply": self.first_low_after_30_ply,
            "fast_moves_le_1s_share": self.fast_moves_le_1s / n if n else math.nan,
            "think_moves_ge_10s_share": self.think_moves_ge_10s / n if n else math.nan,
            "think_moves_ge_20s_share": self.think_moves_ge_20s / n if n else math.nan,
            "think_moves_ge_30s_share": self.think_moves_ge_30s / n if n else math.nan,
            "mean_cp_low_before_10": self.cp_low_before_10_sum / self.n_low_before_10_cp if self.n_low_before_10_cp else math.nan,
            "mean_cp_nonlow_before_10": self.cp_nonlow_before_10_sum / self.n_nonlow_before_10_cp if self.n_nonlow_before_10_cp else math.nan,
            "blunder_rate_low_before_10": self.blunders_low_before_10 / self.n_low_before_10_cp if self.n_low_before_10_cp else math.nan,
            "blunder_rate_nonlow_before_10": self.blunders_nonlow_before_10 / self.n_nonlow_before_10_cp if self.n_nonlow_before_10_cp else math.nan,
            "critical_share": self.critical_n / n if n else math.nan,
            "mean_time_critical": self.critical_time_sum / self.critical_n if self.critical_n else math.nan,
            "mean_cp_critical": self.critical_cp_sum / self.critical_n if self.critical_n else math.nan,
            "blunder_rate_critical": self.critical_blunders / self.critical_n if self.critical_n else math.nan,
            "critical_low_before_10_share": self.critical_low_before_10 / self.critical_n if self.critical_n else math.nan,
            "reached_winning_position": int(self.winning_n > 0),
            "reached_losing_position": int(self.losing_n > 0),
            "first_winning_ply": self.first_winning_ply,
            "first_losing_ply": self.first_losing_ply,
            "first_winning_time_after": self.first_winning_time_after,
            "first_losing_time_after": self.first_losing_time_after,
            "first_winning_low_after_10": self.first_winning_low_after_10,
            "first_losing_low_after_10": self.first_losing_low_after_10,
            "converted_winning_position": int(self.winning_n > 0 and self.player_result_pgn == 1.0),
            "escaped_losing_position": int(self.losing_n > 0 and self.player_result_pgn >= 0.5),
            "first_blunder_ply": self.first_blunder_ply,
            "first_blunder_low_before_10": self.first_blunder_low_before_10,
            "last10_mean_cp": self.last10_cp_sum / self.last10_n if self.last10_n else math.nan,
            "last10_mean_time_spent": self.last10_time_sum / self.last10_n if self.last10_n else math.nan,
            "last10_low_before_10_share": self.last10_low_before_10 / self.last10_n if self.last10_n else math.nan,
            "last10_blunder_rate": self.last10_blunders / self.last10_n if self.last10_n else math.nan,
        }
        for phase in PHASES:
            n_phase = self.phase_n[phase]
            out[f"{phase}_moves"] = n_phase
            out[f"{phase}_mean_time_spent"] = self.phase_time[phase] / n_phase if n_phase else math.nan
            out[f"{phase}_mean_cp"] = self.phase_cp[phase] / n_phase if n_phase else math.nan
            out[f"{phase}_low_before_10_share"] = self.phase_low_before_10[phase] / n_phase if n_phase else math.nan
            out[f"{phase}_blunder_rate"] = self.phase_blunders[phase] / n_phase if n_phase else math.nan
        return {k: format_value(v) for k, v in out.items()}


def format_value(value) -> str:
    if value is None:
        return ""
    if isinstance(value, float):
        if math.isnan(value) or math.isinf(value):
            return ""
        return f"{value:.6f}".rstrip("0").rstrip(".")
    return str(value)


def build_features(cp_paths: list[Path], output_path: Path, cp_loss_cap: float, progress_every: int, limit_games: int) -> None:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    start = time.monotonic()
    games_seen = 0
    player_games_written = 0
    rows_written = False

    with output_path.open("w", encoding="utf-8", newline="") as out_f:
        writer = None
        for path in cp_paths:
            for game_id, rows in iter_game_groups(path):
                games_seen += 1
                final_ply = max(safe_int(row["ply"]) for row in rows)
                aggs = {"white": PlayerClockAgg(), "black": PlayerClockAgg()}
                for row in rows:
                    color = row["mover_color"]
                    if color in aggs:
                        aggs[color].add_row(row, final_ply=final_ply, cp_loss_cap=cp_loss_cap)

                for agg in aggs.values():
                    if agg.n_moves == 0:
                        continue
                    out = agg.as_row()
                    if writer is None:
                        writer = csv.DictWriter(out_f, fieldnames=list(out.keys()))
                        writer.writeheader()
                    writer.writerow(out)
                    rows_written = True
                    player_games_written += 1

                if progress_every > 0 and games_seen % progress_every == 0:
                    elapsed = time.monotonic() - start
                    print(
                        f"Clock features: games={games_seen:,} player_games={player_games_written:,} "
                        f"rate={games_seen / elapsed:,.0f} games/s",
                        file=sys.stderr,
                        flush=True,
                    )
                if limit_games and games_seen >= limit_games:
                    break
            if limit_games and games_seen >= limit_games:
                break

    if not rows_written:
        raise RuntimeError("No player-game clock features were written.")
    elapsed = time.monotonic() - start
    print(
        {
            "output": str(output_path),
            "games_seen": games_seen,
            "player_games_written": player_games_written,
            "seconds": round(elapsed, 3),
        }
    )


def main() -> int:
    args = parse_args()
    cp_paths = args.centipawn_csv or DEFAULT_CPS
    for path in cp_paths:
        if not path.exists():
            raise FileNotFoundError(path)
    build_features(
        cp_paths=cp_paths,
        output_path=args.output,
        cp_loss_cap=args.cp_loss_cap,
        progress_every=args.progress_every_games,
        limit_games=args.limit_games,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
