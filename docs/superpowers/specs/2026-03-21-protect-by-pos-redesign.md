# Protect by Pos Tab Redesign

## Purpose

Replace the current "Protect by Pos" tab with a position-centric intelligence view. For a selected position, show which teams still need to fill that position, their financial situation, market label, and statistical weaknesses — a pivoted view of Rosters data organized by position instead of by team.

## Primary Use Case

During the live draft, quickly assess the competitive landscape at each position: who still needs players there, who has money to spend, and what stats they're chasing. This informs nomination strategy and bidding decisions.

## UI Layout

### Sidebar (width = 2)

**Position dropdown** (`selectizeInput`): C, 1B, 2B, SS, 3B, OF, SP, MR, CL (replaces old `allpos` which had RP instead of MR/CL).

**Summary card** below the dropdown showing aggregates for the selected position:
- Protected: total players protected at this position across all teams
- Teams Need: count of teams that haven't met their threshold
- Avg Salary: average salary of protected players at this position
- Avg Value: average pDFL of protected players at this position

### Main Panel

**Header**: "Teams That Need {Position}" (dynamic based on dropdown selection).

**Single DT table** with one row per team that still needs the selected position. Columns:

| Column | Description |
|--------|-------------|
| Team | Team name |
| Still Need | How many more slots the team needs at this position |
| Market | Categorical label: Strong Buy / Lean Buy / Neutral / Wait / Full |
| Cash Left | Position-adjusted budget remaining (hitting budget for C/1B/2B/SS/3B/OF, pitching budget for SP/MR/CL) |
| $/Player | DPP — dollars per player remaining from pstandings |
| Weakest Stats | Bottom 2-3 stat categories below 65% of goal, color-coded |

**Table is sorted** by market label priority (Strong Buy > Lean Buy > Neutral > Wait > Full), then $/Player descending within each tier.

## Position Thresholds

A team "still needs" a position based on how many they have protected vs the required count:

| Position | Required |
|----------|----------|
| OF | 3 |
| SP | 5 |
| C, 1B, 2B, SS, 3B, MR, CL | 1 |

`Still Need = max(0, required - count_protected_at_position)`. Teams with Still Need = 0 are excluded from the table.

## Market Label Computation

Uses the same DPP-ratio logic already in `nomTargets_r()`:

```
leagueAvgDPP = sum(CashLeft of all other teams with Needed > 0) / sum(Needed of those teams)
ratio = team_DPP / leagueAvgDPP
```

| Ratio | Label |
|-------|-------|
| >= 1.3 | Strong Buy |
| >= 1.0 | Lean Buy |
| >= 0.8 | Neutral |
| < 0.8 | Wait |
| Needed <= 0 | Full |

Color coding matches existing Nominations tab: Strong Buy/Lean Buy = `#d4edda`, Neutral = `#fff3cd`, Wait = `#f8d7da`, Full = `#e9ecef`.

## Weakest Stats Column

Computed per team using `calcGoals()` from `daflFunctions.r`. Stats are filtered by position type:

- **Hitter positions** (C, 1B, 2B, SS, 3B, OF): only HR, RBI, R, SB
- **Pitcher positions** (SP, MR, CL): only W, K, SV, HLD

Display the bottom 2-3 categories where `pc` (percent of goal collected) is below 0.65. Format as inline text: `"SB 32%, R 58%"`.

Color coding within the cell:
- Red (`#e74c3c`): below 50% of goal
- Orange (`#f39c12`): 50–65% of goal

If a team has no categories below 65%, show "On track" in green.

## Data Flow

All data sources already exist as reactives in `server.R`:

1. `protClean_r()` — combined roster of all protected players with Pos, Team, Salary, pDFL
2. `pstandings_r()` — team-level standings with CashLeft, Needed, DPP
3. `currentSummary_r()` — team budget split by hitting/pitching with `salleft`
4. `calcGoals(rp, rh, targets, team)` — stat goal completion per team
5. `rhitters_r()` / `rpitchers_r()` — reactive roster data for hitters/pitchers

New reactive needed: `posNeed_r()` that, for the selected position:
1. Counts each team's protected players at that position
2. Computes Still Need based on position thresholds
3. Filters to teams with Still Need > 0
4. Joins market label (DPP ratio computation)
5. Joins position-adjusted cash left from `currentSummary_r()`
6. Computes weakest stats via `calcGoals()` for each team, filtered to relevant stat categories
7. Formats weakest stats as HTML text with color coding

## Changes Summary

### ui.R
- Update position choices: replace `'SP','RP'` with `'SP','MR','CL'`
- Replace `mainPanel` contents: remove `allpos`, `uniquePos`, `tNeed`, `posProtect` outputs
- Add `uiOutput("posSummaryCard")` to sidebar
- Add single `DT::dataTableOutput("posNeedTable")` to main panel with dynamic header

### server.R
- Remove: `posProtect()`, `uniqueProtect()`, `teamsInterested()` functions
- Remove: `output$allpos`, `output$uniquePos`, `output$tNeed`, `output$posProtect` renderers
- Update: `updateSelectizeInput` for `e4` to use new position list
- Add: `posNeed_r()` reactive
- Add: `output$posSummaryCard` renderUI
- Add: `output$posNeedTable` DT::renderDataTable with market label color styling

### No changes to
- `daflFunctions.r` — `calcGoals()` is used as-is
- Other tabs — no cross-tab dependencies affected
