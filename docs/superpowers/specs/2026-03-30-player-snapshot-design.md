# Player Snapshot — Design Spec

## Problem
During a live auction draft, when a player is nominated you need to quickly assess: what are they projected to do, how many teams need their position, who can outbid you, and who else is available at that position. Currently this requires hopping between 4+ tabs (Search, Positional Pressure, Nominations, Hitters/Pitchers).

## Solution
Add a snapshot detail panel to the existing Search tab. The search table stays as-is. When you click a player row, a card-style snapshot panel renders below the table with everything you need to decide on a bid.

## Scope — V1
Five data sections, plus a header. No new data sources — assembles existing reactives.

**Cut from V1:** cross-projection comparison (ATC vs Steamer vs BAT X), prospect info (FV/level/tools), owner detail card (already visible in search table).

## Layout

### Trigger
- `input$searchTable_rows_selected` drives the snapshot
- Clicking a row shows the snapshot; clicking the same row deselects and hides it
- `uiOutput("playerSnapshot")` renders below the search table

### Snapshot Panel Structure
```
┌─────────────────────────────────────────────────────────────┐
│ HEADER: Player Name | Pos (posEl) | MLB | Age | Owner/Sal  │
├──────────────────────────┬──────────────────────────────────┤
│ PROJECTED STATS          │ COMPETITION                      │
│ HR  RBI  R  SB  AVG      │ Your max bid: $XX                │
│ (or W  SO  ERA  SV  HLD) │ 3 teams can outbid you           │
│ DFL: $XX  SGP: X.X       │ - Team A ($45 max, needs SS)     │
│ ADP: XX   rankDiff: +XX  │ - Team B ($38 max, bench)        │
├──────────────────────────┼──────────────────────────────────┤
│ POSITIONAL CONTEXT        │ COMPARABLE PLAYERS               │
│ 6 teams need SS           │ Player B  $22  ADP 45            │
│ Avg $/Player: $14         │ Player C  $19  ADP 52            │
│ Top bidders:              │ Player D  $17  ADP 61            │
│ - Team X (Strong Buy $30) │ Player E  $15  ADP 70            │
│ - Team Y (Lean Buy $22)   │ Player F  $14  ADP 78            │
├──────────────────────────┴──────────────────────────────────┤
│ INJURY: IL-10 since 3/15 — Expected back 4/10              │
│ (only shown if player has injury data)                      │
└─────────────────────────────────────────────────────────────┘
```

## Data Flow

### 1. Player Lookup
Get selected playerid from `searchData_r()`. Look up the full record from `AllH_active()` or `AllP_active()` to determine hitter vs pitcher and get all stat columns.

### 2. Projected Stats
Pull directly from the matched player row:
- **Hitters:** pHR, pRBI, pR, pSB, pAVG
- **Pitchers:** pW, pSO, pERA, pSV, pHLD
- **Both:** pDFL, pSGP, pADP, rankDiff

### 3. Positional Context
Extract the positional need logic from `posNeed_r` into a shared helper function `getPositionalNeed(pos, protClean, pstandings, currentSummary, rhitters, rpitchers, targets)` that both the Positional Pressure tab and the snapshot can call.

For the snapshot, call it with the player's Pos and display:
- Count of teams that still need the position
- Average $/Player for those teams
- Top 5 teams sorted by market signal (Strong Buy first), showing team name (as roster link), market label, and max bid

### 4. Competition
Reuse the competition report logic inline. Given the selected playerid and `myTeam()`:
- Compute your max bid (CashLeft - (Needed - 1))
- For each other team with Needed > 0: compute their max bid, check if they need this position as a starter (using posThresholds)
- Only show teams that can outbid you
- Display: count headline, then each competitor with team name, max bid, reason (needs starter vs bench)

### 5. Comparable Players
Filter `AllH_avail()` or `AllP_avail()` (matching hitter/pitcher type) to:
- Same primary Pos as selected player
- pDFL within +/- $10 of selected player's pDFL
- Exclude the selected player
- Take top 5 by pDFL
- Show: Player name (fgLink), pDFL, pADP

### 6. Injury Status
Check `injOrig_full` for matching playerid. If found, show injury description and expected return date. If not found, omit the section entirely (no empty card).

## Implementation Notes

### Refactor: Extract positional need helper
The `posNeed_r` reactive (~80 lines) contains logic for computing which teams need a position, their market labels, cash left, max bids, and weakest stats. Extract the core computation into a function `getPositionalNeed(pos)` that accepts position and returns the need dataframe. Both `posNeed_r` and the snapshot call this function.

### Files Modified
- `LiveDraftTool/server.R` — add `playerSnapshot` renderer, extract `getPositionalNeed`, add competition logic inline
- `LiveDraftTool/ui.R` — add `uiOutput("playerSnapshot")` below searchTable, rename tab from "Search" to "Player Snapshot"

### No changes to
- `draftGuide.r` — no new data needed
- `daflFunctions.r` — no new helper functions needed
- `fgFetchAll.js` — no new data sources

## Verification
1. Load the app, go to Player Snapshot tab
2. Search table works as before (filters, target stars, team links)
3. Click a hitter row — snapshot appears with hitting stats, positional context for their Pos, competition info, comparables, injury if applicable
4. Click a pitcher row — snapshot shows pitching stats, same contextual panels
5. Click same row again — snapshot disappears
6. Click a free agent vs a rostered player — both work, header shows owner status
7. Positional Pressure tab still works (uses same extracted helper)
