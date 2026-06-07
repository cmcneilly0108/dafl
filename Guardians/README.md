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

`code/run_guardians_pulse.sh` is the launchd wrapper. It runs daily at **3am**
via the user LaunchAgent `~/Library/LaunchAgents/com.dafl.guardianspulse.plist`,
alongside the other `com.dafl.*` jobs. `guardiansPulse.r` pulls upstream only
when the day's snapshot doesn't yet exist, so the 3am run fetches fresh data;
re-running it later the same day just re-hydrates from `DAFL.db`.

Logs: per-run files in `logs/guardians_pulse_*.log`; launchd stdout/stderr in
`logs/launchd_guardians_{stdout,stderr}.log`.

To (re)install the job after editing the plist:

```bash
launchctl unload ~/Library/LaunchAgents/com.dafl.guardianspulse.plist 2>/dev/null
launchctl load -w ~/Library/LaunchAgents/com.dafl.guardianspulse.plist
launchctl list | grep guardians   # confirm it's registered
```

## Design / plan

- Spec: `docs/superpowers/specs/2026-05-20-guardians-tracker-design.md`
- Plan: `docs/superpowers/plans/2026-05-20-guardians-tracker.md`
