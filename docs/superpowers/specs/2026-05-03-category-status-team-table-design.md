# Category Status — Team-oriented detail table

## Context

The LeagueEval Shiny app's **Category Status** tab currently shows only
`catSummary`: a league-wide table of category-level `pvp` / `pvm` / `opportunity`
values. To act on that information, you have to mentally cross-reference your
team's standings (Statistical Surplus tab) and your roster (My Team tab). This
spec adds a team-oriented view that consolidates: what tier am I in for each
category, and which of my players are projected to drive each category.

## User-facing change

The Category Status tab gains:

1. A team selector (`selectizeInput`) at the top, defaulting to **Liquor Crickets**.
2. A new **Team Category Detail** table (this spec) immediately below the selector.
3. The existing **Points by Category** (`catSummary`) table, moved below the new table.

The selector controls only the new table. `catSummary` remains league-wide.

## Team Category Detail table

### Shape

- **Rows:** the 10 scoring categories — HR, RBI, R, SB, BA, W, K, SV, HD, ERA.
- **Columns:**
  1. **Category** — short label.
  2. **Tier** — one of `High` / `Medium` / `Low` with the team's value inline.
     Examples: `Medium (45)`, `High (.281)`, `Low (3.85)`. Background color
     matches Statistical Surplus: `#d4edda` green / `#fff3cd` yellow / `#f8d7da`
     red.
  3. **Top 4 Contributors** — comma-separated list of up to 4 players from the
     selected team, formatted `Name (Pos, value)`.

### Tier rule

Identical to Statistical Surplus:
- Rank teams in `cstand` for each category. ERA ranked low-to-high; all others
  high-to-low.
- Rank 1–4 → `High`, 5–9 → `Medium`, 10–13 → `Low`.
- The team's value is read from `cstand` and formatted: `BA` to 3 decimals,
  `ERA` to 2 decimals, others rounded to integer.

### Top 4 contributor logic

For each category, filter to the selected team and sort:

| Category | Source frame | Sort key (desc) | Filter |
|---|---|---|---|
| HR | `AllH` | `pHR` | — |
| RBI | `AllH` | `pRBI` | — |
| R | `AllH` | `pR` | — |
| SB | `AllH` | `pSB` | — |
| BA | `AllH` | `(pAVG - 0.250) * pAB` | `pAB > 0` |
| W | `AllP` | `pW` | — |
| K | `AllP` | `pSO` | — |
| SV | `AllP` | `pSV` | — |
| HD | `AllP` | `pHLD` | — |
| ERA | `AllP` | `(4.00 - pERA) * pIP / 9` | `pIP > 0` |

The 0.250 / 4.00 thresholds are league-average-adjacent baselines for
"contribution above replacement" — the goal is to rank players who actually move
the needle, not players with thin samples (a 1-AB .500 hitter or a 2-IP 0.00 ERA
reliever).

If `pAB` or `pIP` is not present in `AllH` / `AllP` (because `read.fg` only
prefixes columns from the projection JSON), fall back to `pAVG` desc for BA and
`pERA` asc for ERA. The implementation must verify column presence before
choosing the formula and document the fallback in a comment.

### Player display format

`{Name} ({Pos}, {value})` where:

- `Name` — `Player` field from `AllH` / `AllP` (already display-cleaned).
- `Pos` — primary position. For hitters use `Position` if present, else `Pos`.
  For pitchers use `Pos` (which is already SP / MR / CL after the
  `Allpitchers$Pos` derivation in `inSeasonPulse.r:180`).
- `value` — formatted to match the category:
  - `BA`: 3 decimals (`.281`)
  - `ERA`: 2 decimals (`3.42`)
  - All others: rounded integer.
  - For BA and ERA the value shown is the player's projected rate
    (`pAVG` / `pERA`), not the volume-weighted sort key.

If fewer than 4 players qualify (e.g. team has 3 starters projected for saves),
show whatever is available; do not pad with empty cells.

### Reactivity

The output reacts to:
- `input$teamSelect` — re-rendering when the user picks a different team.
- `rv$refreshCount` — so projection-source swaps (ATC ↔ Steamer ↔ BAT X)
  refresh the table, mirroring every other reactive output in `server.R`.

## Implementation notes

### `LeagueEval/ui.R`

Replace the current `tabPanel("Category Status", ...)` with a `sidebarLayout`
containing the team selector in the sidebar and both tables stacked in the main
panel (new table on top, `catSummary` below). Keep `mainPanel` width consistent
with the rest of the app.

### `LeagueEval/server.R`

Add a new `output$teamCatDetail` `renderDataTable`. Structure parallels
`output$statSurplus` (same `cats` list, same tier thresholds, same color
formatting) but iterates a single team rather than producing per-tier columns.

Both `AllH` and `AllP` are already loaded as session globals (set by
`inSeasonPulse.r` and re-bound by the projection switcher at
`server.R:69-78`), so no new data plumbing is needed.

### What stays the same

- `catSummary` content and styling — only its position on the tab changes.
- `cstand` ranking — reused as-is.
- Statistical Surplus — untouched; this is a separate, complementary view.

## Out of scope

- Changing the categories list (no FIP, no QS, etc.).
- Showing player projections for free agents on this tab.
- Trading suggestions or category-shopping recommendations — those live on the
  Dumpers / Desperate / Trades tabs.
- Editing the H/M/L thresholds.

## Success criteria

- Selecting a team updates the table within one render cycle.
- Switching projection source (Settings modal) updates both tables on the tab.
- Tier values exactly match Statistical Surplus for the same team.
- Top-4 lists for counting categories are stable across reloads (no random tie
  breaks). Ties are broken by player name ascending.
- BA / ERA top-4 lists exclude players with no projected playing time.
