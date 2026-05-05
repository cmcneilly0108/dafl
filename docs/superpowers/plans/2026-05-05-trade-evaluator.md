# Trade Evaluator Tab Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a new "Trade Eval" tab to the LeagueEval Shiny app that lets the user pick two teams, multi-select players from each side, and see counting-stat / rate-stat / pDFL deltas live.

**Architecture:** Single Shiny tab. UI in `LeagueEval/ui.R`, all logic in `LeagueEval/server.R`. Reads existing `AllH` / `AllP` globals; no changes to data pipeline. Three reactives (`rosterA`, `rosterB`, summary) + five outputs (two team-name labels, two roster tables, one summary table).

**Tech Stack:** R, Shiny, DT, dplyr.

**Spec:** `docs/superpowers/specs/2026-05-05-trade-evaluator-design.md`.

---

## File Structure

- **Modify** `LeagueEval/ui.R` — insert one `tabPanel("Trade Eval", ...)` between the existing `"Surplus"` (lines 119–154) and `"Prospects"` (line 155) tabPanels.
- **Modify** `LeagueEval/server.R` — add the Trade Eval reactives and outputs (~120 lines) immediately above the `# Category Status — per-team detail (new)` block at line 244. Add `updateSelectizeInput` calls for the new selectors in two places (line 135 inside the refresh handler, line 167 in the initial setup) where the other team-selector wiring already lives.

No new files. The LeagueEval app has no testthat infrastructure; verification is manual against the running app, scripted in Task 5.

## Reference

For convenience while implementing:

- Hidden projection columns to carry on **hitters**: `pHR, pRBI, pR, pSB, pAVG, pAB`
- Hidden projection columns to carry on **pitchers**: `pW, pSO, pSV, pHLD, pERA, pIP`
- Visible roster columns: `Player, Pos, pDFL, Salary, Contract, hotscore, Injury`
- Summary categories order: `HR, RBI, R, SB, BA, W, K, SV, HD, ERA, pDFL`
- Volume-weighted rates: `BA = Σ(pAVG·pAB)/Σ(pAB)` over hitters with `pAB > 0`; `ERA = Σ(pERA·pIP)/Σ(pIP)` over pitchers with `pIP > 0`.
- Counting deltas are symmetric (`deltaB = -deltaA`); rate deltas are computed independently per side.
- `teams` is already defined at `LeagueEval/server.R:7` as `sort(unique(RTot$Team))` — these are the 13 fantasy teams (RTot doesn't include Free Agent), so we can reuse it.

---

### Task 1: Add the Trade Eval tab shell to ui.R

**Goal:** Tab is visible in the navbar; layout is empty but well-structured; app still loads without errors.

**Files:**
- Modify: `LeagueEval/ui.R` (insert new tabPanel between line 154 closing `),` and line 155 `tabPanel("Prospects", ...`)

- [ ] **Step 1: Insert new tabPanel**

After the closing `),` of the Surplus tabPanel (line 154) and before `tabPanel("Prospects", ...` (line 155), insert exactly:

```r
    tabPanel("Trade Eval",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput('tradeTeamA', 'Team A', choices = NULL),
                 selectizeInput('tradeTeamB', 'Team B', choices = NULL),
                 width = 2
               ),
               mainPanel(
                 fluidRow(
                   column(6, h4(textOutput('tradeTeamAName')),
                          DT::dataTableOutput('tradeRosterA')),
                   column(6, h4(textOutput('tradeTeamBName')),
                          DT::dataTableOutput('tradeRosterB'))
                 ),
                 br(),
                 h3('Trade Summary'),
                 DT::dataTableOutput('tradeSummary')
               )
             )
    ),
```

- [ ] **Step 2: Verify ui.R parses by launching the app**

Run `shiny::runApp('LeagueEval')` from R (or click "Run App" in RStudio with `LeagueEval/ui.R` open).

Expected:
- App starts with no parse errors.
- "Trade Eval" tab appears in the navbar between "Surplus" and "Prospects".
- Clicking it shows the sidebar with two empty selectors and an empty main panel (it will fill in once Tasks 2–4 are done).

If you see an "unexpected token" error, the most common cause is a missing or extra comma between tabPanels. Check that the line above the inserted block ends with `),` and that the inserted block also ends with `),`.

- [ ] **Step 3: Commit**

```bash
git add LeagueEval/ui.R
git commit -m "feat: add Trade Eval tab shell to LeagueEval UI"
```

---

### Task 2: Wire up team selectors and name labels in server.R

**Goal:** Both selectors populate with the 13 fantasy teams (Free Agent already excluded by virtue of using `teams`). Both default to empty. Picking a team updates the column header above its roster table.

**Files:**
- Modify: `LeagueEval/server.R:135` (refresh handler, add two updateSelectizeInput calls after the existing teamSelect update)
- Modify: `LeagueEval/server.R:167` (initial setup, same)
- Modify: `LeagueEval/server.R` (new textOutputs above the Category Status block at line 244)

- [ ] **Step 1: Add updateSelectizeInput calls in the refresh handler**

In the `observeEvent(input$refreshBtn, ...)` block at line 129–144, find line 135:

```r
      updateSelectizeInput(session, 'teamSelect', choices = teams, selected = 'Liquor Crickets')
```

Immediately after that line, add:

```r
      updateSelectizeInput(session, 'tradeTeamA',
                           choices = c('Pick a team' = '', teams),
                           selected = '')
      updateSelectizeInput(session, 'tradeTeamB',
                           choices = c('Pick a team' = '', teams),
                           selected = '')
```

- [ ] **Step 2: Add updateSelectizeInput calls in the initial setup**

Find line 167:

```r
  updateSelectizeInput(session, 'teamSelect', choices = teams, selected = 'Liquor Crickets')
```

Immediately after it, add the same two updates (without the leading whitespace difference — this is outside the observer so just two-space indent):

```r
  updateSelectizeInput(session, 'tradeTeamA',
                       choices = c('Pick a team' = '', teams),
                       selected = '')
  updateSelectizeInput(session, 'tradeTeamB',
                       choices = c('Pick a team' = '', teams),
                       selected = '')
```

- [ ] **Step 3: Add textOutputs for team-name labels**

Find the `# Category Status — per-team detail (new)` comment at line 244. Immediately above that comment, add:

```r
# Trade Eval — team-name labels
  output$tradeTeamAName <- renderText({ input$tradeTeamA })
  output$tradeTeamBName <- renderText({ input$tradeTeamB })

```

- [ ] **Step 4: Verify in the running app**

Reload the app and click "Trade Eval".

Expected:
- Both selectors show the placeholder "Pick a team".
- Clicking either drops down a list of 13 fantasy team names. "Free Agent" is **not** in the list.
- Selecting a team on side A populates the `<h4>` heading above the (still-empty) Team A roster column with that team's name. Same for side B.

- [ ] **Step 5: Commit**

```bash
git add LeagueEval/server.R
git commit -m "feat: wire Trade Eval team selectors and name labels"
```

---

### Task 3: Roster reactives and rendered roster tables

**Goal:** Picking a team shows that team's full roster (hitters + pitchers combined) sorted by `-pDFL`, with Salary and Contract visible. Multi-select rows enabled.

**Files:**
- Modify: `LeagueEval/server.R` (extend the Trade Eval block above the Category Status section)

- [ ] **Step 1: Add the buildRoster helper and roster reactives**

Immediately below the two `output$tradeTeamXName` renderText lines from Task 2, add:

```r
  # Trade Eval — roster builder. Carries hidden projection columns (pHR/pAVG/pAB
  # for hitters; pW/pERA/pIP/etc. for pitchers) so the summary reactive can
  # compute deltas without re-querying AllH / AllP.
  buildRoster <- function(teamName) {
    if (is.null(teamName) || teamName == '') return(NULL)
    hCols <- c('Player','Pos','pDFL','Salary','Contract','hotscore','Injury',
               'pHR','pRBI','pR','pSB','pAVG','pAB')
    pCols <- c('Player','Pos','pDFL','Salary','Contract','hotscore','Injury',
               'pW','pSO','pSV','pHLD','pERA','pIP')
    hOnly <- AllH %>% filter(Team == teamName)
    hOnly <- hOnly[, intersect(hCols, colnames(hOnly)), drop = FALSE]
    if (nrow(hOnly) > 0) hOnly$.kind <- 'H'
    pOnly <- AllP %>% filter(Team == teamName)
    pOnly <- pOnly[, intersect(pCols, colnames(pOnly)), drop = FALSE]
    if (nrow(pOnly) > 0) pOnly$.kind <- 'P'
    bind_rows(hOnly, pOnly) %>% arrange(-pDFL)
  }

  rosterA <- reactive({ rv$refreshCount; buildRoster(input$tradeTeamA) })
  rosterB <- reactive({ rv$refreshCount; buildRoster(input$tradeTeamB) })
```

- [ ] **Step 2: Add the renderRoster helper and two outputs**

Immediately below the two reactives, add:

```r
  renderRoster <- function(roster) {
    req(roster)
    visibleCols <- intersect(
      c('Player','Pos','pDFL','Salary','Contract','hotscore','Injury'),
      colnames(roster)
    )
    display <- roster[, visibleCols, drop = FALSE]
    datatable(display,
              selection = list(mode = 'multiple'),
              options = list(pageLength = 30, paging = FALSE,
                             searching = FALSE, info = FALSE),
              rownames = FALSE) %>%
      formatCurrency(intersect(c('pDFL','Salary'), visibleCols)) %>%
      formatRound(intersect('hotscore', visibleCols), 2)
  }

  output$tradeRosterA <- DT::renderDataTable({ renderRoster(rosterA()) })
  output$tradeRosterB <- DT::renderDataTable({ renderRoster(rosterB()) })
```

- [ ] **Step 3: Verify in the running app**

Reload the app, click "Trade Eval", pick "Liquor Crickets" on Team A.

Expected:
- Left side shows the full Liquor Crickets roster (hitters + pitchers combined), sorted by pDFL descending.
- Visible columns: Player, Pos, pDFL, Salary, Contract, hotscore, Injury — in that order.
- pDFL and Salary formatted as currency (e.g., `$23`); hotscore rounded to 2 decimals.
- Clicking a row highlights it; ctrl-click adds another (multi-select works).
- Pick a different team on Team B; both tables show side-by-side, no overlap, each scrolls independently if rosters are long.
- Switching teams on either side updates only that side's table.

- [ ] **Step 4: Commit**

```bash
git add LeagueEval/server.R
git commit -m "feat: add Trade Eval roster reactives and tables"
```

---

### Task 4: Trade summary table

**Goal:** A summary table appears below the rosters with one row per category (HR, RBI, R, SB, BA, W, K, SV, HD, ERA) plus a final pDFL row. With no rows selected, all deltas are zero. Selecting rows updates deltas live.

**Files:**
- Modify: `LeagueEval/server.R` (extend the Trade Eval block, after the roster outputs from Task 3)

- [ ] **Step 1: Add summary helpers**

Immediately below the two `output$tradeRosterX` lines from Task 3, add:

```r
  # Trade Eval — summary helpers
  catCounting <- list(
    list(name='HR',  col='pHR'),  list(name='RBI', col='pRBI'),
    list(name='R',   col='pR'),   list(name='SB',  col='pSB'),
    list(name='W',   col='pW'),   list(name='K',   col='pSO'),
    list(name='SV',  col='pSV'),  list(name='HD',  col='pHLD')
  )

  sum0 <- function(x) sum(x, na.rm = TRUE)

  weightedRate <- function(rows, rateCol, volCol) {
    if (!rateCol %in% colnames(rows) || !volCol %in% colnames(rows)) return(NA_real_)
    v <- rows[[volCol]]; r <- rows[[rateCol]]
    ok <- !is.na(v) & !is.na(r) & v > 0
    if (!any(ok)) return(NA_real_)
    sum(r[ok] * v[ok]) / sum(v[ok])
  }

  rateDelta <- function(fullRoster, outgoing, incoming, rateCol, volCol) {
    before <- weightedRate(fullRoster, rateCol, volCol)
    kept   <- fullRoster %>% filter(!Player %in% outgoing$Player)
    after  <- weightedRate(bind_rows(kept, incoming), rateCol, volCol)
    if (is.na(before) || is.na(after)) return(NA_real_)
    after - before
  }

  fmtCount <- function(x) {
    if (is.na(x) || x == 0) '0'
    else if (x > 0) sprintf('+%d', round(x))
    else sprintf('%d', round(x))
  }
  fmtBA <- function(x) {
    if (is.na(x)) '—'
    else if (x == 0) '0.000'
    else if (x > 0) sprintf('+%.3f', x)
    else sprintf('%.3f', x)
  }
  fmtERA <- function(x) {
    if (is.na(x)) '—'
    else if (x == 0) '0.00'
    else if (x > 0) sprintf('+%.2f', x)
    else sprintf('%.2f', x)
  }
  fmtDFL <- function(x) {
    if (is.na(x) || x == 0) '$0'
    else if (x > 0) sprintf('+$%d', round(x))
    else sprintf('-$%d', round(abs(x)))
  }
```

- [ ] **Step 2: Add the summary output**

Immediately below the helpers, add:

```r
  output$tradeSummary <- DT::renderDataTable({
    rv$refreshCount
    req(input$tradeTeamA, input$tradeTeamB)

    if (input$tradeTeamA == input$tradeTeamB) {
      return(datatable(
        data.frame(Note = "Pick two different teams"),
        options = list(paging = FALSE, info = FALSE, searching = FALSE,
                       ordering = FALSE, dom = 't'),
        rownames = FALSE))
    }

    rA <- rosterA(); rB <- rosterB()
    selA <- if (length(input$tradeRosterA_rows_selected) > 0)
              rA[input$tradeRosterA_rows_selected, , drop = FALSE]
            else rA[0, , drop = FALSE]
    selB <- if (length(input$tradeRosterB_rows_selected) > 0)
              rB[input$tradeRosterB_rows_selected, , drop = FALSE]
            else rB[0, , drop = FALSE]

    countingRows <- lapply(catCounting, function(c) {
      out <- if (c$col %in% colnames(selA)) sum0(selA[[c$col]]) else 0
      inc <- if (c$col %in% colnames(selB)) sum0(selB[[c$col]]) else 0
      deltaA <- inc - out
      data.frame(Category = c$name,
                 A = fmtCount(deltaA),
                 B = fmtCount(-deltaA),
                 stringsAsFactors = FALSE)
    })

    baA <- rateDelta(rA, selA, selB, 'pAVG', 'pAB')
    baB <- rateDelta(rB, selB, selA, 'pAVG', 'pAB')
    baRow <- data.frame(Category = 'BA',
                        A = fmtBA(baA), B = fmtBA(baB),
                        stringsAsFactors = FALSE)

    eraA <- rateDelta(rA, selA, selB, 'pERA', 'pIP')
    eraB <- rateDelta(rB, selB, selA, 'pERA', 'pIP')
    eraRow <- data.frame(Category = 'ERA',
                         A = fmtERA(eraA), B = fmtERA(eraB),
                         stringsAsFactors = FALSE)

    outDFL <- if ('pDFL' %in% colnames(selA)) sum0(selA$pDFL) else 0
    incDFL <- if ('pDFL' %in% colnames(selB)) sum0(selB$pDFL) else 0
    dflDelta <- incDFL - outDFL
    dflRow <- data.frame(Category = 'pDFL',
                         A = fmtDFL(dflDelta), B = fmtDFL(-dflDelta),
                         stringsAsFactors = FALSE)

    df <- do.call(rbind, c(countingRows[1:4], list(baRow),
                           countingRows[5:8], list(eraRow, dflRow)))
    names(df) <- c('Category', input$tradeTeamA, input$tradeTeamB)

    datatable(df,
              options = list(paging = FALSE, info = FALSE, searching = FALSE,
                             ordering = FALSE, dom = 't'),
              rownames = FALSE)
  })
```

- [ ] **Step 3: Verify zero-state in the running app**

Reload the app. Pick "Liquor Crickets" on side A and any other team (e.g., the one whose name sorts first alphabetically) on side B. Don't select any roster rows.

Expected: summary table appears below the rosters with 11 rows in this order: HR, RBI, R, SB, BA, W, K, SV, HD, ERA, pDFL. Both team columns are headed with the actual team names. All deltas read `0` / `0.000` / `0.00` / `$0`.

- [ ] **Step 4: Verify counting symmetry**

Click one hitter row on side A and one hitter row on side B (any two players, doesn't matter who).

Expected:
- HR / RBI / R / SB rows show mirrored values: if A's column shows `+12`, B's shows `-12`, etc.
- W / K / SV / HD rows stay at `0` (no pitchers were traded).
- pDFL row mirrors as `+$x` / `-$x` (or `$0` if both pDFLs happen to match).

- [ ] **Step 5: Verify rate asymmetry (BA)**

With the same hitter pair selected, look at the BA row.

Expected: both columns are non-zero (or `0.000` if the trade is exactly volume-neutral, which is rare). The two magnitudes need not match — each team's BA delta is computed against its own pAB volume.

The ERA row stays at `0.00` because no pitchers were traded.

- [ ] **Step 6: Verify rate asymmetry (ERA)**

Clear the hitter selections. Click one pitcher row on side A and one on side B.

Expected: ERA row now shows non-zero deltas, potentially asymmetric. BA row goes back to `0.000`. Pitcher counting rows (W, K, SV, HD) mirror; hitter counting rows are `0`.

- [ ] **Step 7: Verify same-team guard**

Set both selectors to the same team (e.g., both to "Liquor Crickets").

Expected: the entire summary table collapses to a single row with the column header `Note` and the value `Pick two different teams`. The roster tables on each side still render (showing the same team's roster).

Switch one selector back to a different team — summary returns to the 11-row format.

- [ ] **Step 8: Commit**

```bash
git add LeagueEval/server.R
git commit -m "feat: add Trade Eval summary table with counting + rate + pDFL deltas"
```

---

### Task 5: Full feature verification

**Goal:** Run through the remaining success criteria from the spec to confirm the feature is shippable.

**Files:** none (manual verification only)

- [ ] **Step 1: Cold-load smoke test**

Stop the Shiny process, restart R, source the app fresh (`shiny::runApp('LeagueEval')`), click "Trade Eval".

Expected: tab loads, both selectors empty (showing "Pick a team" placeholder), no errors in the R console, no warnings about missing inputs.

- [ ] **Step 2: Refresh button**

With a trade selected (one player from each side), click the green "Refresh Data" button at top-right of the navbar.

Expected:
- The "Refreshing data..." notification appears.
- Once it disappears, both roster tables re-render and the summary recomputes.
- Layout/columns identical; numeric values may shift slightly if underlying projections changed.
- Selectors retain their values; row selections may be cleared (acceptable — Shiny re-renders the DT).

- [ ] **Step 3: Projection switch**

Click "Settings" → toggle from ATC to Steamer (or whichever isn't currently active). Close modal.

Expected: roster tables and summary update. pDFL deltas in particular should change because pDFL is projection-source-dependent. The team list itself is unchanged.

- [ ] **Step 4: Cross-tab non-interference**

Click over to the "Category Status" tab and pick a team there. Come back to "Trade Eval".

Expected: Trade Eval state (selected teams, selected rows) is preserved during the session. Category Status still works exactly as before — its team selector is independent of the Trade Eval selectors.

- [ ] **Step 5: Empty selection stability**

With both teams picked, click on already-selected rows to deselect them all (or use the table's clear selection).

Expected: summary still shows all 11 rows with zero deltas — no flicker, no missing rows, no error notification.

- [ ] **Step 6: Sanity-check ground truth**

Pick two teams. Pick a single hitter from side A and look up that hitter's `pHR` value via the "Category Status" tab's Top 4 Contributors row for HR (those rows display `Name (Pos, integer pHR)`). Then pick a single hitter from side B and check that hitter's pHR similarly.

Expected: the HR row in the summary should read `<pHR_B - pHR_A>` for column A and the negation for column B. Eyeball-verifiable arithmetic.

- [ ] **Step 7: Done**

If all checks pass, the feature is ready. If any check fails, capture which step and what was observed vs. expected, then return to the systematic-debugging skill rather than fixing forward.

---

## Notes for the implementer

- **Don't add tests under `tests/`.** This codebase has no test infrastructure. Adding testthat just for this feature is overkill.
- **Don't refactor the existing `output$catSummary` block** I touched in the previous session. The Trade Eval reactives sit above it and are independent.
- **Don't change `inSeasonPulse.r`.** All Trade Eval logic lives in `LeagueEval/server.R`; the data globals (`AllH`, `AllP`, `teams`, `rv$refreshCount`) are read-only from Trade Eval's perspective.
- **If you can't run the Shiny app** (e.g., subagent without an R environment), say so — don't claim verification passed. The user can run it manually.
