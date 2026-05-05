# Trade Evaluator tab

## Context

The LeagueEval Shiny app has tabs for evaluating individual teams (By Team, Category Status), the league as a whole (Standings, Surplus), and players who might be available (Dumpers, Desperate). What's missing is a way to evaluate a *specific hypothetical trade*: pick two teams, pick the players each side is sending, and see what changes.

This spec adds a **Trade Eval** tab that does exactly that — counting-stat deltas, rate-stat (BA/ERA) deltas, and pDFL delta — using the projection columns already on `AllH` / `AllP`.

## User-facing change

A new top-level navbar tab `"Trade Eval"`, placed between `"Surplus"` and `"Prospects"` in `LeagueEval/ui.R`.

Layout:

```
sidebarLayout (sidebar width = 2):
  sidebarPanel:
    selectizeInput  "tradeTeamA"   "Team A"
    selectizeInput  "tradeTeamB"   "Team B"
  mainPanel:
    fluidRow:
      column(6): h4(Team A name) + DT::dataTableOutput("tradeRosterA")
      column(6): h4(Team B name) + DT::dataTableOutput("tradeRosterB")
    br()
    h3("Trade Summary")
    DT::dataTableOutput("tradeSummary")
```

Both selectors start empty (no default team) so the page does not auto-populate. Team list is `sort(unique(AllH$Team))` minus `"Free Agent"`.

## Roster tables (`tradeRosterA` / `tradeRosterB`)

### Shape

- **Rows:** every player on the selected team's roster — combined hitters + pitchers in one table per side. Sorted by `-pDFL`.
- **Columns** (visible): `Player | Pos | pDFL | Salary | Contract | hotscore | Injury`.
- **Hidden columns** carried for the summary computation:
  - For hitters: `pHR`, `pRBI`, `pR`, `pSB`, `pAVG`, `pAB`.
  - For pitchers: `pW`, `pSO`, `pSV`, `pHLD`, `pERA`, `pIP`.
  - Plus a `.kind` column (`"H"` or `"P"`) so the summary code can route rows to the right category math.

The hidden columns ride along on the reactive data frame; only the visible columns get rendered by `datatable(...)`. This avoids re-querying `AllH`/`AllP` from the summary reactive.

### Selection

`selection = list(mode = 'multiple')` on each table. The selected row indices come back via `input$tradeRosterA_rows_selected` / `input$tradeRosterB_rows_selected` (Shiny convention).

### Formatting

`formatCurrency(c('pDFL','Salary'))` and `formatRound('hotscore', 2)` to match the styling on other tabs.

## Trade Summary table (`tradeSummary`)

### Shape

One row per scoring category plus a final pDFL row. Three columns:

| Column | Meaning |
|---|---|
| Category | `HR`, `RBI`, `R`, `SB`, `BA`, `W`, `K`, `SV`, `HD`, `ERA`, `pDFL` |
| Team A Δ | Net change to Team A from this trade |
| Team B Δ | Net change to Team B from this trade |

The table is always rendered (even with zero rows selected, in which case all deltas are `0`). This keeps the layout stable while the user iterates.

### Counting categories — symmetric

For HR, RBI, R, SB, W, K, SV, HD:

```
outA = sum of pXXX over selA  (players A is sending out)
inA  = sum of pXXX over selB  (players A is receiving)
deltaA = inA - outA
deltaB = -deltaA
```

Pitchers contribute `0` to hitter cats and vice versa (hidden columns are NA for the wrong type — treat NA as 0 in these sums).

### Rate categories — asymmetric

BA and ERA depend on the team's existing volume, so each side is computed independently against its own roster.

**BA** — projected team batting average:

```
BA(team) = sum(pAVG * pAB) / sum(pAB)   over team's hitters
```

For Team A:
- `before` = BA over A's current hitters (from rosterA()).
- `after`  = BA over (A's hitters minus selA-hitters) plus (B's hitters in selB).
- `deltaA` = after - before.

Symmetrically for Team B with the sides swapped. Drop rows with `pAB <= 0` or NA from both sums. If a team has zero pAB after the trade, render the cell as `"—"`.

**ERA** — same shape with pIP:

```
ERA(team) = sum(pERA * pIP) / sum(pIP)   over team's pitchers
```

Drop rows with `pIP <= 0` or NA. Render as `"—"` if zero pIP after the trade.

This volume-weighting matches what the Category Status top table uses for its Top-4 contributor sort (`(4.00 - pERA) * pIP / 9`, `(pAVG - 0.250) * pAB`) — the same data shape, different aggregation.

### pDFL — symmetric

```
deltaA = sum(pDFL of selB) - sum(pDFL of selA)
deltaB = -deltaA
```

### Display formatting

- Counting cats: rounded integer with explicit sign (`+12`, `-3`, `0`).
- BA: 3 decimals with sign (`+0.004`, `-0.012`, or `"—"`).
- ERA: 2 decimals with sign (`-0.18`, `+0.05`, or `"—"`). Note: a *negative* ERA delta is *good* — leave that interpretation to the reader; do not flip signs.
- pDFL: `formatCurrency` with sign (`+$12`, `-$5`).
- All non-zero deltas should render with explicit `+` for positives so the direction is unambiguous at a glance.

### Reactivity

The summary recomputes when any of these change:

- `input$tradeTeamA`, `input$tradeTeamB`
- `input$tradeRosterA_rows_selected`, `input$tradeRosterB_rows_selected`
- `rv$refreshCount` (so Refresh Data and projection-source swaps invalidate the cache, matching every other reactive output in `server.R`)

## Edge cases

| Case | Behavior |
|---|---|
| Only one (or neither) team picked | Summary table not rendered — `req(input$tradeTeamA, input$tradeTeamB)` halts the reactive. The whole "Trade Summary" section is empty until both are chosen. (Showing a one-sided "delta" against a phantom Team B would mislead.) |
| Same team picked on both sides | Render summary as a single row reading "Pick two different teams". Don't disable the selectors — let the user adjust freely. |
| Both teams picked, no rows selected on either side | Show all 11 rows with `0` deltas. Stable layout, no flicker. |
| Team has no projected pAB / pIP after trade | Render BA / ERA cell as `"—"` for that team only. |
| Missing projection columns on a player | Treat NAs as 0 for counting sums; drop NA-volume rows from rate-stat means. After `bind_rows(hitters, pitchers)` the union of stat columns is on every row — pitcher rows carry NAs for `pHR`/`pRBI`/etc. and hitter rows carry NAs for `pW`/`pSO`/etc., which is what the NA-as-0 sum behavior counts on. |
| Player appears in both selections | Cannot happen — each table is filtered to one team's roster. No special handling needed. |

## Implementation notes

### `LeagueEval/ui.R`

Insert a new `tabPanel("Trade Eval", ...)` between the `"Surplus"` and `"Prospects"` panels (currently at lines 119 and 155). Use `sidebarLayout` matching the structure of the existing `"By Team"` and `"Category Status"` panels.

### `LeagueEval/server.R`

Add three reactives and one rendered output, near the existing Category Status block (around line 244). Skeleton:

```r
# Trade Eval — roster reactives carry hidden projection columns
rosterA <- reactive({ rv$refreshCount; buildRoster(input$tradeTeamA) })
rosterB <- reactive({ rv$refreshCount; buildRoster(input$tradeTeamB) })

output$tradeRosterA <- DT::renderDataTable({ ... })  # visible cols only
output$tradeRosterB <- DT::renderDataTable({ ... })

output$tradeSummary <- DT::renderDataTable({
  rv$refreshCount
  if (sameTeamGuard()) return(warningTable())
  selA <- rosterA()[input$tradeRosterA_rows_selected, ]
  selB <- rosterB()[input$tradeRosterB_rows_selected, ]
  buildSummary(rosterA(), rosterB(), selA, selB)
})
```

`buildRoster()` and `buildSummary()` should be defined as plain functions inside `shinyServer(...)` (not exported) — short enough to stay inline (~80 lines total). No new files. No changes to `inSeasonPulse.r`.

### Team list

`teamsForTrade <- sort(setdiff(unique(AllH$Team), "Free Agent"))` populated via `updateSelectizeInput()` in the same place the existing `teams` list is wired up (server.R:135 and :167).

### What stays the same

- Every other tab is untouched.
- `AllH` / `AllP` / `cstand` / `nicks` globals are read-only from this code.
- The projection-source switcher already invalidates `rv$refreshCount` (server.R settings observer), so the new tab inherits cache invalidation for free.

## Out of scope

- Adding free-agent pickup simulation (single-team add/drop). Different feature.
- Tier / standings impact ("if I make this trade, do I move from Medium to High in HR?"). Possible v2 addition; the data is already in `cstand` but recomputing post-trade tiers requires pulling all 13 teams' projections back through the ranking logic, which is enough complexity to defer.
- Per-player running totals shown next to checkboxes ("if I add this guy, +22 HR"). Polish for later.
- Persisting trade scenarios to disk.
- AI-generated trade analysis (the ProtectionTrades tab already does that for protection lists; not in scope here).
- Editing categories or weights.

## Success criteria

- Tab loads with both selectors empty and no errors in the R console.
- Selecting a team populates that side's roster table sorted by `-pDFL`, with Salary and Contract visible.
- With no rows selected on either side, summary table shows all 11 rows with `0` deltas.
- Trading one hitter from each side: counting-stat rows mirror exactly (`+x` / `-x`); BA row may show different magnitudes for each team; pitcher cats stay at 0.
- Trading one pitcher from each side: pitcher counting rows mirror; ERA row may differ in magnitude per team; hitter cats stay at 0.
- Picking the same team on both sides surfaces the warning row instead of computing a self-trade.
- Refresh Data click re-pulls roster contents from the (possibly swapped) projection globals.
- Settings projection switch (ATC ↔ Steamer ↔ BAT X) updates both rosters and the summary.
