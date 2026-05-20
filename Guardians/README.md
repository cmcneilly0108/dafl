# Cleveland Guardians Tracker

Personal Shiny app that follows the entire Cleveland Guardians organization
(MLB through DSL).

## Launch

```bash
cd Guardians
Rscript -e 'shiny::runApp(launch.browser = TRUE)'
```

## Tabs

- **Depth Chart** — one card per level (MLB / AAA / AA / A+ / A / ACL / DSL)
  with each player's season stat line, plus the FanGraphs MLB depth chart on
  the right.
- **Hot / Cold** — z-scored leaderboard within each level. Window: 7 / 14 / 30
  days. Filterable by role and level.
- **Player Detail** — full season line plus a HotScore trend chart (once 5+
  daily snapshots have accumulated).
- **Risers & Transactions** — players on positive HotScore streaks or recent
  promotions; last-14d transactions; current IL board.

## Data

Sources:
- `baseballr::mlb_rosters`, `mlb_stats`, `mlb_people`
- MLB Stats API direct (transactions endpoint)
- `baseballr::fg_milb_batter_game_logs`, `fg_milb_pitcher_game_logs`
- `getFGProspects()` (existing) for FV grades
- `baseballr::get_chadwick_lu()` for the mlb_id ↔ fg_id crosswalk

Daily snapshots are written to 4 tables in `code/DAFL.db`. A Refresh
button in the Settings modal forces a re-pull.

## Scheduled refresh

`code/run_guardians_pulse.sh` is the launchd wrapper. Add an entry to
`~/Library/LaunchAgents` to run it daily (e.g., 6am) alongside the existing
`run_inseason_pulse.sh` schedule.

## Design / plan

- Spec: `docs/superpowers/specs/2026-05-20-guardians-tracker-design.md`
- Plan: `docs/superpowers/plans/2026-05-20-guardians-tracker.md`
