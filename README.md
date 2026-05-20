# players-characteristics-and-tournament-rules

# Master’s thesis
# Master of Data Science
# Playing Styles and Adaptability to Temporal Constraints: Evidence from Titled Tuesday Chess Tournament

**Student:** *Elijah Sumernikov*  
**Supervisor:** *Dmitry Dagaev*  
**Format:** individual  

---

## Short description
This project studies how chess players adjust their **playing style** when facing different **time controls** (temporal constraints). The setting is **Titled Tuesday** - a large, recurring online tournament where many titled players participate.

Core idea: time pressure is not just “less accuracy” overall. It may systematically shift *how* people play (risk-taking, complexity, openings, trading behavior, endgame frequency) and *who* adapts better (experience, style, rating, specialization).

**Project outputs:**
- reproducible data pipeline (collection → cleaning → feature engineering → modeling)
- empirical results on style shifts and adaptation
- figures + tables + robustness checks suitable for a thesis paper
- (optional) interactive report/dashboard for exploration

---

## Research questions
1. **Style shift:** How does playing style change when the time control is faster/slower?
2. **Adaptability:** Which players adapt better to tighter time constraints, and how stable is this “adaptation skill” over time?
3. **Mechanisms:** Is adaptation explained by opening choices, move complexity, risk-taking, or endgame technique?
4. **Heterogeneity:** Do effects differ by rating, title, experience in TT, or baseline style?
5. **Performance link:** Does “style adaptation” predict outcomes (score, streaks, upset probability) beyond rating differences?

---

## Data
### Primary sources
- **Titled Tuesday game data**: pairings, results, timestamps (if available), PGNs (moves), player IDs/ratings, tournament metadata (date, event ID, time control).
- **Engine evaluations (Stockfish)**: per-move evaluation used to build accuracy and complexity measures.

### Unit of observation
- **Game-level (main):** one row per game with aggregated features.
- **Move-level (secondary):** optional for deeper behavioral signatures.

### Key variables
- Outcomes: win/draw/loss, points, performance vs expected, upset indicators.
- Time constraints: time control class (e.g., 3+1 vs 5+1), plus proxies for “effective time pressure”.
- Controls: rating of both players, rating gap, color, round, tournament date fixed effects, opponent fixed effects (optional).

---

## Hypotheses
- **H1 (risk):** Under tighter time constraints, players choose higher-variance lines (more tactical complexity, fewer simplifying trades).
- **H2 (opening):** Faster controls shift opening choice toward systems that reduce calculation load or known traps.
- **H3 (adaptation):** Some players exhibit consistently smaller performance drops (or gains) under fast controls → an “adaptability” trait.
- **H4 (skill interaction):** Stronger players lose less accuracy, but style changes can be non-monotonic (e.g., simplifying to convert).
- **H5 (asymmetry):** Effects differ by color (White presses more; Black chooses more solid lines).

---

## Methodology overview
### Style measurement (game-level feature set)
A “style vector” per game, built from interpretable groups:

1. **Complexity / tactics**
   - evaluation volatility (engine swing frequency)
   - average absolute eval change per move (centipawn volatility)
   - forcing-move proxies (checks, sacrifices, sequences)

2. **Risk-taking**
   - sharp opening families (opening tags)
   - material imbalance frequency and magnitude
   - “play for win” proxies (early repetition / quick simplification)

3. **Simplification / conversion**
   - trade rate (piece exchanges per move)
   - early queen trades
   - move number when reduced-material “endgame” threshold is reached

4. **Accuracy under pressure**
   - centipawn loss, blunder/mistake rates
   - late-game error rates (endgame-specific)

5. **Pace proxies (if timestamps exist)**
   - time spent per move distribution
   - time-trouble indicator

Then either:
- keep interpretable indices (complexity index, simplification index, risk index), or
- reduce dimensions (e.g., PCA) for a compact style representation.

### Identification strategy (main)
Compare the **same players** across different time controls:

- player fixed effects to absorb stable skill/style
- date/tournament fixed effects to absorb meta trends and seasonality

Example models:
- `StyleMetric_ig = β * FastControl_g + controls + PlayerFE + DateFE + ε`
- `Performance_ig = β * FastControl_g + StyleMetrics + controls + FE + ε`

### “Adaptability” estimation
- player-level adaptability score:
  - player-specific response to fast control (random slope / mixed model), or
  - two-step approach (estimate per-player effect, then analyze distribution)
- stability checks:
  - split sample (early vs late period)
  - rolling windows

### Robustness / falsification
- restrict to players who appear in both time controls
- match games by rating-gap bins
- exclude outliers (disconnects, very short games)
- placebo comparisons within same time control across weeks (should be smaller)

---

## Work plan
### Stage 1 — Setup (Checkpoint 1: repo + plan)
- create repository and project skeleton
- define schema (“game”, “player”, “tournament”)
- define feature list and minimum viable dataset
- implement data collection + basic parser

### Stage 2 — Literature + framing
- time pressure in decision-making (behavioral econ / cognitive psych)
- chess style metrics in the literature (accuracy, complexity, risk)
- online chess platform data: credibility and known biases

### Stage 3 — Data collection and dataset construction
- collect TT tournaments for a defined time span
- parse PGNs → structured move-level table
- build game-level table with IDs, ratings, results, metadata
- data QA: duplicates, missingness, inconsistent ratings, corrupted games

### Stage 4 — Engine pipeline (Stockfish)
- choose evaluation budget (depth/nodes trade-off)
- run evaluations (parallelized) and store per-move outputs
- build accuracy + complexity features from engine outputs

### Stage 5 — EDA and baseline facts
- participant distribution, repeated players, rating ranges
- raw outcomes by time control (then rating-adjusted)
- sanity checks (do faster controls increase blunders? by how much?)

### Stage 6 — Main empirical analysis (style shift)
- estimate effects of faster control on style metrics
- produce coefficient plots and compact tables
- heterogeneity: rating, title group, baseline style clusters

### Stage 7 — Adaptability analysis (player-level)
- estimate player-specific fast-control effects
- stability over time (split-sample correlation)
- link adaptability to outcomes (points / performance / upset rates)

### Stage 8 — Mechanisms and interpretation
- opening channel: do openings mediate style shifts?
- complexity channel: does volatility explain performance drops?
- conversion channel: do good adapters simplify more effectively in fast?

### Stage 9 — Write-up and reproducibility
- finalize figures and tables
- discuss limitations and threats to validity
- make pipeline reproducible (run instructions, pinned deps, structure)

---

## Deliverables
- `data_dictionary.md` (variables, definitions, construction notes)
- reproducible pipeline code:
  - collection + parsing
  - engine evaluation
  - feature construction
  - modeling + figure generation
- thesis-ready outputs:
  - main tables
  - robustness appendix
  - key plots (style shifts, adaptability distribution, stability)

---

## Repository structure (planned)
- `data/`
  - `raw/` (not committed; instructions to reproduce)
  - `interim/` (parsed PGNs, intermediate tables)
  - `processed/` (final analysis-ready datasets)
- `notebooks/`
  - `01_collect.ipynb`
  - `02_parse_pgn.ipynb`
  - `03_engine_eval.ipynb`
  - `04_eda.ipynb`
- `src/`
  - `config.py`
  - `collect/`
  - `parse/`
  - `features/`
  - `models/`
  - `viz/`
  - `utils/`
- `scripts/` (CLI entry points for pipeline steps)
- `reports/`
  - `figures/`
  - `tables/`
  - `draft/` (thesis text / notes)
- `environment/`
  - `requirements.txt` or `environment.yml`
  - `Makefile` (optional)

---

## Minimal running instructions (placeholder)
1. Create environment and install dependencies  
2. Collect TT tournaments for selected dates  
3. Parse PGNs into move-level tables  
4. Run Stockfish evaluations (configurable budget)  
5. Build game-level features  
6. Run analysis scripts to reproduce tables and figures  

---

## Risks and limitations
- **Selection bias:** TT participants are not a random sample of all titled players.
- **Rating noise:** online ratings fluctuate; need FE + robustness checks.
- **Engine sensitivity:** evaluation budget affects accuracy metrics; report sensitivity.
- **Missing timestamps:** time-trouble may be proxied, not observed.
- **Meta changes:** opening trends/platform shifts over time → date fixed effects.

---

## Suggested timeline
- Weeks 1–2: data + parser + baseline dataset  
- Weeks 3–4: engine pipeline + initial features  
- Weeks 5–6: main regressions + EDA figures  
- Weeks 7–8: adaptability + robustness + mechanisms  
- Weeks 9–10: writing + reproducibility cleanup  
