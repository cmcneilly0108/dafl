---
name: dafl-missing-player
description: Use when a specific DAFL player is missing from LeagueEval (not appearing in tier tables, FA pool, or category status), or when a player has duplicate rows in mymaster.csv (sa-prefix and numeric playerid for the same human)
---

# DAFL Missing Player / Master Duplicates

## Overview

Projections use FanGraphs numeric playerids (e.g. `35101`). `mymaster.csv` maps each CBS player (by `cbs_name` + `MLB`) to a playerid. When master holds an outdated FanGraphs prospect-board id (`sa\d+` prefix) instead of the real numeric id, the inner_join in `code/inSeasonPulse.r:172/203` drops that player and they vanish from LeagueEval.

Common shape: **two rows for the same player** — one with the right `cbs_name` but a `sa…` placeholder id, and one with the right numeric id but a name variant CBS doesn't use ("Sam"/"Samuel", accent/no-accent, "Davidjohn"/"DJ", etc.). Both rows are individually broken; merging fixes it. Sam Antonacci (2026-05-09) is the canonical example.

## How to diagnose

| Script | When to run |
|--------|-------------|
| `code/checkMissingPlayers.r` | Rostered CBS player missing from LeagueEval. Reports CBS-rostered players that won't survive the projection inner_join. |
| `scripts/findMasterDupes.js` | Scan ALL of `mymaster.csv` for duplicate-player rows (rostered + free agents). Flags SA+NUM pairs as the Antonacci pattern. |
| `scripts/mergeMasterDupes.js` | Auto-fix high-confidence SA+NUM pairs in bulk. |

For a single named player: grep `mymaster.csv` for the surname; check the active projection JSON (picked by `leagueEvalSettings.json:projSource` — `atcHROS.json`/`steamerHROS.json`/`batxHROS.json`) for the player's PlayerName and playerid. Compare against master.

## How to fix

**Always back up first** (`cp mymaster.csv mymaster.csv.bak-$(date +%Y%m%d-%H%M%S)`) — destructive ops on this file have bitten the user before.

**Single player:** in master, change the row whose `cbs_name` matches CBS exactly to use the projection's numeric playerid; delete any duplicate row for the same player.

**Bulk:** run `scripts/mergeMasterDupes.js`. It keeps numeric playerids, resolves `cbs_name` against `AllHitters.csv`/`AllPitchers01.csv` (CBS strips accents and uses team-specific spelling), falls back to no-accent + casual first name when the player isn't in CBS, and skips ambiguous candidates via `SKIP_KEYS` (e.g. `smith|1997|TEX` — Josh H. Smith is a real MLB player distinct from the prospect "Josh Smith"). Always review the diff after.

## Common mistakes

- **No backup.** Always copy `mymaster.csv` with a timestamp first.
- **Losing CRLF.** `mymaster.csv` is CRLF. The Edit tool may normalize to LF on multi-line edits — verify with `head -3 mymaster.csv | od -c | head -5` and look for `\r\n`.
- **Wrong `cbs_name` on merge.** Default to whatever's in `AllHitters.csv`/`AllPitchers01.csv` for that team. CBS varies — sometimes "Sam"/"TJ", sometimes "Davidjohn"/"A.J." (with periods). Don't assume; check.
- **Auto-merging genuinely-different humans.** Same surname + birth_year + team can be two real prospects. Only merge SA+NUM pairs with close-variant names; verify on FanGraphs if unsure.
- **`Player` and `cbs_name` columns drift apart** in some rows (e.g. one accented, one not, on the same row). On merge, set both to the same resolved value. The merger does this.

## Quick reference

| File | Role |
|------|------|
| `mymaster.csv` | CBS-name → playerid map. CRLF-encoded. |
| `code/inSeasonPulse.r:172,203` | Inner-joins CBS players with projections via `playerid`. Silently drops mismatches. |
| `atcHROS.json` / `steamerHROS.json` / `batxHROS.json` | FanGraphs projections. Source of truth for numeric playerid. |
| `leagueEvalSettings.json` | `projSource` selects active projection. |
| `AllHitters.csv` / `AllPitchers01.csv` | CBS exports. Source of truth for `cbs_name` spelling. |
| `code/checkMissingPlayers.r` | Diagnostic for rostered missing players. Run from `code/`. Output → `../missing_rostered_players.csv`. |
| `scripts/findMasterDupes.js` | Scan master for duplicate-player rows (Node). |
| `scripts/mergeMasterDupes.js` | Auto-merge confident SA+NUM pairs (Node). |
| `code/daflFunctions.r:664-705` | `read.fg()` — loads projections; rewrites unmatched playerids to `str_c(Player, pTeam)`. |
