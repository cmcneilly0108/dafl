# Guardians Tracker — Design Spec

**Date:** 2026-05-20
**Status:** Draft — awaiting user review
**Type:** New Shiny app (fan tracking, non-fantasy)

## Goal

A personal Shiny R app for following the entire Cleveland Guardians organization — MLB roster through AAA, AA, A+, A, complex (ACL), and DSL. Pure fan/curiosity use case; no fantasy or DAFL crossover. Daily refresh; sourced primarily from `baseballr` (FanGraphs + MLB Stats API), reusing patterns from the existing `LeagueEval` app and `code/inSeasonPulse.r` pipeline.

## Non-goals (v1)

- Statcast / pitch-tracking deep dives (link out to FanGraphs from player detail).
- Multi-org comparison.
- Historical comparable-player lookups.
- Push notifications / Discord integration on transactions.
- Fantasy / DAFL crossover.

## Architecture

Mirrors the established DAFL pattern: a focused Shiny app whose UI sources a dedicated R script that hydrates globals from cached files + SQLite.

### File layout

```
Guardians/
  ui.R                     # navbarPage with five tabs (Depth Chart default)
  server.R                 # sources ../code/guardiansPulse.r; renders DT tables
                           #   and plotly trend charts; mirrors LeagueEval/server.R

code/
  guardiansPulse.r         # data pipeline (analogue of inSeasonPulse.r)
  run_guardians_pulse.sh   # launchd wrapper (analogue of run_inseason_pulse.sh)
  daflFunctions.r          # add: resolveGuardiansAffiliates(), pullOrgStats(),
                           #      pullOrgTransactions(), milbHotScores()

code/DAFL.db               # add tables: GuardiansRoster, GuardiansStats,
                           #             GuardiansTransactions, GuardiansHotscore
```

### Runtime model

- **Local launch:** `cd Guardians; Rscript -e 'shiny::runApp()'` (same as LeagueEval).
- **Startup:** `server.R` calls `source("../code/guardiansPulse.r")`. The pulse checks today's snapshot in `DAFL.db`; if present and `DAFL_FORCE_REFRESH != 1`, exits in <5s after hydrating globals from DB. Otherwise pulls upstream and writes today's snapshot.
- **Daily refresh:** `code/run_guardians_pulse.sh` runs from launchd (~6am after games settle), writes today's snapshot to `DAFL.db`.
- **In-app refresh:** Settings modal exposes a `Refresh Data` button that sets `DAFL_FORCE_REFRESH=1` and re-sources `guardiansPulse.r` (pattern from `LeagueEval/server.R:140`).

## Data sources

### MLB Stats API via `baseballr::mlb_*` (primary)

- `mlb_team_affiliates()` → resolve Guardians org → affiliate `teamId`s. Cached weekly; hardcoded fallback table in `guardiansPulse.r` for resilience if the call ever fails mid-season.
- `mlb_team_roster(team_id = <affiliate>, season = cyear, roster_type = "fullSeason")` → today's roster for each level.
- `mlb_stats(stats = "season", group = "hitting"|"pitching", season = cyear, sportId = N, teamId = <affiliate>)` → season-to-date aggregated stats. `sportId`: 1=MLB, 11=AAA, 12=AA, 13=A+, 14=A, 16=Rookie/complex, 17=DSL.
- `mlb_transactions(start_date, end_date)` → org-filtered transaction feed for the Risers & Transactions tab.

### FanGraphs via `baseballr::fg_*` (rolling windows + scouting)

- `fg_milb_batter_game_logs(playerid, year)` / `fg_milb_pitcher_game_logs(playerid, year)` → compute trailing 7/14/30-day windows for HotScore. Only called for players with ≥10 PA / 5 IP in last 30 days (to keep daily load manageable).
- `fg_team_depth_chart()` for the MLB depth-chart panel.
- `getFGProspects()` (already in `daflFunctions.r`) → FV grades, tool grades, top-100 rank. Joined on FG playerid.

### Data freshness

- Daily snapshot written ~6am local. Stats columns can be up to ~24h stale.
- Transaction feed pulls last 30 days each run; upserts on `txn_id` keep duplicates out.
- App shows "Last refresh: YYYY-MM-DD HH:MM" in the header.

## SQLite schema

All tables live in the existing `code/DAFL.db`. Upserts are idempotent on the primary key, so re-running the pulse on the same day is safe.

```sql
GuardiansRoster
  snapshot_date  DATE        -- today's date when written
  mlb_id         INTEGER     -- canonical id across all sources
  fg_id          TEXT        -- nullable; only when matched on FanGraphs
  player         TEXT
  pos            TEXT
  level          TEXT        -- MLB / AAA / AA / A+ / A / ACL / DSL
  team_id        INTEGER     -- affiliate id
  age            REAL
  PRIMARY KEY (snapshot_date, mlb_id)

GuardiansStats
  snapshot_date  DATE
  mlb_id         INTEGER
  level          TEXT
  -- hitter columns
  pa, ab, h, hr, r, rbi, sb, bb, k  INTEGER
  avg, obp, slg, woba              REAL
  -- pitcher columns
  ip                              REAL
  w, l, sv, hld, so, bb_p         INTEGER
  era, fip, k9, bb9, whip         REAL
  PRIMARY KEY (snapshot_date, mlb_id)

GuardiansTransactions
  txn_id         TEXT PRIMARY KEY
  txn_date       DATE
  mlb_id         INTEGER
  player         TEXT
  type           TEXT        -- call-up / option / DFA / IL placement / IL activation / trade / sign / release
  from_team_id   INTEGER
  to_team_id     INTEGER
  description    TEXT

GuardiansHotscore
  snapshot_date  DATE
  mlb_id         INTEGER
  level          TEXT        -- the level the score is scoped to
  window_days    INTEGER     -- 7 / 14 / 30
  hotscore       REAL        -- z-score within (level, window_days, role) cohort
  PRIMARY KEY (snapshot_date, mlb_id, window_days)
```

### HotScore semantics

- Z-scored against peers **at the same level** (cohort = all players at that level in the requested window). An A+ player is compared to A+ players; an MLB player to MLB players. Avoids the "A-ball stud looks better than Ramírez" distortion that org-only scoring would create.
- Computed per role (hitters vs pitchers separately).
- Window options: 7 / 14 / 30 days. The Hot/Cold tab lets the user pick.

## Pipeline order (`guardiansPulse.r`)

1. Read `leagueEvalSettings`-style config (none needed v1; reserve for future).
2. Resolve org affiliate IDs (cached weekly; fallback table on failure).
3. For each affiliate `team_id`:
   - `mlb_team_roster` → upsert `GuardiansRoster` rows for today.
   - `mlb_stats` (hitting + pitching) → upsert `GuardiansStats` rows for today.
4. `mlb_transactions(today-30, today)` → upsert `GuardiansTransactions`.
5. For each player with sufficient recent activity (≥10 PA or ≥5 IP in last 30d):
   - `fg_milb_*_game_logs` (skip MLB players — use existing `Allhitters`/`Allpitchers` cache for them).
   - Compute 7/14/30-day windows → z-score within level → upsert `GuardiansHotscore`.
6. Diff today's `GuardiansRoster` vs yesterday's → flag promotions/demotions for Risers tab.
7. Hydrate globals consumed by `server.R`:
   - `gRoster` — today's roster joined with `GuardiansStats`.
   - `gHot` — `GuardiansHotscore` joined with player + level metadata.
   - `gTxn` — transactions feed for the last 14 days (filtered to org).
   - `gIL` — current IL state, computed by walking forward `GuardiansTransactions`.
   - `gProspects` — `getFGProspects()` filtered to Guardians.
   - `gDepth` — `fg_team_depth_chart()` for the MLB roster.
   - `gTrend` — historical `GuardiansHotscore` for trend plots.

## UI layout

### Tab 1 — Depth Chart (default landing)

Two-pane layout:

- **Left pane: org tree.** One card per level (MLB, AAA, AA, A+, A, ACL, DSL). Each card lists players sorted by position. One-line stat per player:
  - Hitter: `Name (Pos) — .AVG / HR / wOBA`
  - Pitcher: `Name (Pos) — ERA / K/9 / FIP`
  - Players whose level changed in the last 7 days get a ⬆ / ⬇ badge.
- **Right pane: MLB depth chart panel.** From `fg_team_depth_chart()` — shows projected MLB playing-time split by position.

### Tab 2 — Hot / Cold

Controls:
- `selectInput` window: 7 / 14 / 30 days.
- `radioButtons` role: Hitters / Pitchers / All.
- `selectInput` level filter: All / MLB / AAA / AA / A+ / A / ACL / DSL.

Single DT table:

```
Player | Lvl | Pos | Age | Window line | Δ vs season | HotScore
```

- Sorted by HotScore desc.
- Color-coded HotScore column (green for positive, red for negative).
- Window line is `HR / AVG / wOBA` for hitters, `K/9 / ERA / FIP` for pitchers.

### Tab 3 — Player Detail

`selectizeInput` searches the entire org (`gRoster$player`).

On select, renders a `playerSnapshot`-style UI (pattern from `LeagueEval/server.R:837`):

- **Header card:** name, position(s), age, level, MLB team / org, FV (if prospect), top-100 rank.
- **Hero row:** season triple-slash or pitching slash line.
- **Level-by-level table:** current-season stats split by level (for players who moved).
- **Game-log table:** last 30 games, sortable / filterable.
- **Scouting grade panel:** Hit / Game / Raw / Spd or FB / SL / CB / CH / CMD when FG has them.
- **Trend chart:** `plotly` line chart of HotScore over time (from `GuardiansHotscore` history) — only for players with ≥10 history rows.

### Tab 4 — Risers & Transactions

Three stacked sections:

1. **Risers** — players with HotScore positive ≥3 consecutive daily snapshots, plus level-promotions detected from `GuardiansRoster` diffs in the last 7 days.
2. **Recent Transactions** — `gTxn` rendered as a DT table, default window 14 days, filterable by type (call-up / option / DFA / IL / trade / sign / release).
3. **IL board** — current IL stints across the org, computed by walking forward `GuardiansTransactions`. Columns: Player, Level, Placed, Expected return (if known), Type.

### Tab 5 — Settings (modal)

- `Refresh Data` button → sets `DAFL_FORCE_REFRESH=1` and re-sources the pulse.
- Last-refresh timestamp display.

## Error handling

- Each upstream call wrapped in `tryCatch`. On failure: log to stderr, keep yesterday's data in globals, surface "Last successful snapshot: <date>" in the app header.
- Affiliate-ID resolution failures → fall back to hardcoded ID table baked into `guardiansPulse.r`.
- All DB writes use `INSERT OR REPLACE` so partial pulse runs don't corrupt state.

## Edge cases

- **Mid-season trades into/out of the org.** A player no longer in the org gets their last snapshot stamped and stops appearing in today's roster, but remains queryable in Player Detail. Symmetric for new acquisitions.
- **Position-player-pitching / two-way players.** Treat each appearance under the role the game-log reports. Display both lines on player detail if both exist.
- **DSL / complex coverage gaps.** If `mlb_stats(sportId=16/17)` returns empty for a level, the depth-chart card still shows roster but stat columns read "—".
- **Spring training (before Opening Day).** No regular-season stats yet → all tabs degrade to "Roster only" mode. Detected by checking whether any affiliate has a current-season game.
- **Off-season (post-Sep, pre-Mar).** Show "Final regular-season snapshot from YYYY-MM-DD" banner; data is whatever the last daily pulse captured.

## Tests / validation

- **Smoke test:** `Rscript code/guardiansPulse.r` runs cleanly with empty DB tables (cold start) and idempotently when re-run same day.
- **Affiliate resolution:** unit-test `resolveGuardiansAffiliates()` returns a non-empty list with at least MLB + AAA + AA + A+ + A.
- **HotScore:** spot-check a known recent stretch (e.g., a Columbus hitter on a 7-day tear) — confirm HotScore > 0 in the table.
- **App launch:** confirm Shiny app starts, all four tabs render, and Player Detail loads at least one MLB and one MiLB player.

## Open questions deferred to implementation

- Exact 2026 affiliate `teamId` mapping — to be verified during implementation via `mlb_team_affiliates()`.
- Whether `mlb_stats(sportId=17)` (DSL) returns usable rows — handle empty case as documented above either way.
- Whether `fg_milb_*_game_logs` rate-limits cause a problem for ~200 players (likely fine, but add a 200ms sleep between calls just in case).
