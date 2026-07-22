---
name: dafl-team-name-change
description: Use when a DAFL fantasy team has been renamed in CBS (LeagueEval shows NA tiers, inSeasonPulse inner_join drops a team, or user reports "team name change")
---

# DAFL Team Name Change

## Overview

DAFL maps full CBS team names to short nicknames via `data/nicknames.csv`. When a manager renames their team in CBS, the new full name won't match any row in `nicknames.csv`, breaking the inner_join in `code/inSeasonPulse.r` and causing the LeagueEval Category Status tab to return NA tiers for that team.

The fix is a one-line update to `data/nicknames.csv`.

## How to detect which team changed

Compare current full team names in `overall.csv` (top 13 rows, `Team` column) against the `Team` column in `data/nicknames.csv`. The new name appears in `overall.csv` but not in `nicknames.csv`; the old name is the reverse.

```bash
# Quick visual diff: full team names in latest standings vs the mapping
head -14 overall.csv | tail -13 | cut -d, -f2
cut -d, -f1 data/nicknames.csv | tail -n +2
```

## How to update

1. Edit `data/nicknames.csv` — replace the old `FullName,Short` row with the new full name. Decide with the user whether to:
   - **Keep the old short nick** — preserves continuity with historical rows in `DAFLWeeklyStandings.csv` (those rows are not retroactively updated, but going forward the same short identifies the franchise across the rename).
   - **Use a new short nick** — if the new name is thematically very different and continuity isn't valued. Pick something derivable from the new full name (existing pattern: "Hogan's Heroes" → "Heroes", "Crap Shooters" → "Shooters").

   Always confirm the short with the user before writing.

2. No other file needs editing:
   - The `Avail` field is derived at runtime via `str_sub(Team, 1, 6)` in `code/daflFunctions.r:23`.
   - Historical rows in `DAFLWeeklyStandings.csv` are a snapshot per week and are not rewritten.

## Verification

After editing, the next run of `code/inSeasonPulse.r` will inner-join cleanly (line 110), and the LeagueEval Category Status tab will return real tiers for the renamed team instead of NA.

## Common mistakes

- **Editing the `Avail` column** — there is no `Avail` column in `nicknames.csv`. It is computed in `daflFunctions.r`. Do not add one.
- **Renaming the short to match the historical short of an unrelated team** — short nicks must be unique across the file (they become the join key in `cstand`).
- **Picking a short without asking** — the user has opinions about nicknames; always confirm.

## Quick reference

| File | Role |
|------|------|
| `data/nicknames.csv` | The mapping. Edit this. |
| `overall.csv` | Current CBS standings (full names). Source of truth for "what changed". |
| `code/inSeasonPulse.r:110` | Inner-joins `cstand` with `nicks`; silently drops unmapped teams. |
| `code/daflFunctions.r:22-23` | Loads `nicks`, derives `Avail`. |
| `LeagueEval/server.R` (tier lookup) | Maps full → short via `nicks` for `cstand` lookup. |
| `DAFLWeeklyStandings.csv` | Historical weekly snapshots; not rewritten on rename. |
