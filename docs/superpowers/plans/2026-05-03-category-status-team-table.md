# Category Status Team-Oriented Detail Table — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a team selector and per-team category breakdown table (tier + top 4 contributors) to the LeagueEval Category Status tab.

**Architecture:** The new `output$teamCatDetail` reactive in `LeagueEval/server.R` reads `input$teamSelect` and `rv$refreshCount`, computes a tier per category from the existing `cstand` standings frame, and pulls top-4 contributors from the `AllH` / `AllP` globals. UI changes restructure the Category Status `tabPanel` in `LeagueEval/ui.R` into a `sidebarLayout` with the selector in the sidebar and two stacked tables in the main panel. No new data plumbing — all source frames already exist as session globals.

**Tech Stack:** R, Shiny, DT (DataTables wrapper), dplyr, sprintf.

**Spec:** `docs/superpowers/specs/2026-05-03-category-status-team-table-design.md`

---

## File Structure

- **Modify** `LeagueEval/server.R` — add `output$teamCatDetail` and helpers near the existing `output$catSummary` block (around line 242). Logic stays inline alongside the other category outputs, mirroring the structure of `output$statSurplus` (line 261).
- **Modify** `LeagueEval/ui.R` — replace the existing `tabPanel("Category Status", ...)` (line 104) with a `sidebarLayout` version.

No new files. Existing pattern is "all server logic in `server.R`, all UI in `ui.R`" — follow it.

---

## Task 1: Verify projection volume columns exist on AllH / AllP

**Files:**
- Read-only check, no edits.

**Why:** The BA contributor sort key uses `pAB` and the ERA sort uses `pIP`. The spec requires falling back to raw `pAVG` desc / `pERA` asc if those volume columns aren't present. We need to know the truth before writing the implementation, so the right path can be coded directly without dead branches.

- [ ] **Step 1: Run a column check from the LeagueEval directory**

```powershell
$rscript = "C:\Program Files\R\R-4.4.2\bin\Rscript.exe"
& $rscript -e "setwd('code'); source('./inSeasonPulse.r'); cat('AllH cols with pA:', paste(grep('^pA', colnames(AllH), value=TRUE), collapse=', '), '\n'); cat('AllP cols with pI:', paste(grep('^pI', colnames(AllP), value=TRUE), collapse=', '), '\n')"
```

Expected (one possibility): `AllH cols with pA: pAB, pAVG, ...` and `AllP cols with pI: pIP`.

If `pAB` / `pIP` are absent, both BA and ERA contributor functions fall back to the rate-only path. Note the result before continuing.

- [ ] **Step 2: Record the outcome**

Write a one-line note to yourself: `pAB present: yes/no | pIP present: yes/no`. The implementation in Task 2 references this; if both are present, the volume-weighted formulas are used; if either is absent, that category falls back.

---

## Task 2: Add the new reactive output to server.R

**Files:**
- Modify: `LeagueEval/server.R` — insert a new `output$teamCatDetail` block immediately after the existing `output$catSummary` block (currently at lines 242–247).

- [ ] **Step 1: Open `LeagueEval/server.R` and locate the Category Status block**

The existing block:

```r
# Category Status
  output$catSummary <- DT::renderDataTable({
    rv$refreshCount
    datatable(catSummary,options = list(pageLength = 20)) %>%
      formatRound(c('pvp','pvm','opportunity'),2)
  })
```

- [ ] **Step 2: Insert the new output immediately above the existing `catSummary` output**

Replace the four-line block above with:

```r
# Category Status — per-team detail (new)
  output$teamCatDetail <- DT::renderDataTable({
    rv$refreshCount
    team <- input$teamSelect
    if (is.null(team) || team == '') return(NULL)

    cats <- list(
      list(name = "HR",  col = "HR",  reverse = FALSE, kind = "H",   sortKey = "pHR"),
      list(name = "RBI", col = "RBI", reverse = FALSE, kind = "H",   sortKey = "pRBI"),
      list(name = "R",   col = "R",   reverse = FALSE, kind = "H",   sortKey = "pR"),
      list(name = "SB",  col = "SB",  reverse = FALSE, kind = "H",   sortKey = "pSB"),
      list(name = "BA",  col = "BA",  reverse = FALSE, kind = "BA"),
      list(name = "W",   col = "W",   reverse = FALSE, kind = "P",   sortKey = "pW"),
      list(name = "K",   col = "K",   reverse = FALSE, kind = "P",   sortKey = "pSO"),
      list(name = "SV",  col = "S",   reverse = FALSE, kind = "P",   sortKey = "pSV"),
      list(name = "HD",  col = "HD",  reverse = FALSE, kind = "P",   sortKey = "pHLD"),
      list(name = "ERA", col = "ERA", reverse = TRUE,  kind = "ERA")
    )

    fmtVal <- function(catName, x) {
      if (is.na(x))                  ""
      else if (catName == "BA")      sprintf("%.3f", x)
      else if (catName == "ERA")     sprintf("%.2f", x)
      else                           as.character(round(x))
    }

    # Tier for the selected team in one category. cstand is a global from
    # inSeasonPulse.r with one row per team and columns matching `col`.
    computeTier <- function(catCol, reverse) {
      v <- suppressWarnings(as.numeric(cstand[[catCol]]))
      ord <- if (reverse) order(v, na.last = TRUE) else order(-v, na.last = TRUE)
      teamsRanked <- cstand$Team[ord]
      r <- which(teamsRanked == team)
      if (length(r) == 0) return(list(tier = NA_character_, value = NA_real_))
      list(tier  = if (r <= 4) 'High' else if (r <= 9) 'Medium' else 'Low',
           value = v[ord][r])
    }

    # Position display for a hitter row — prefer Position (eligibility-derived)
    # if present, fall back to Pos.
    hitterPos <- function(df) {
      if ("Position" %in% colnames(df)) df$Position else df$Pos
    }

    # Top 4 hitters for a counting category (HR / RBI / R / SB).
    topHitters <- function(sortKey) {
      df <- AllH %>% filter(Team == team)
      if (!sortKey %in% colnames(df) || nrow(df) == 0) return(character(0))
      df <- df %>% arrange(desc(.data[[sortKey]]), Player) %>% head(4)
      mapply(function(p, ps, v) sprintf("%s (%s, %d)", p, ps, round(v)),
             df$Player, hitterPos(df), df[[sortKey]],
             USE.NAMES = FALSE)
    }

    # Top 4 hitters for BA — volume-weighted (pAVG - 0.250) * pAB. Falls back
    # to pAVG desc when pAB is missing from the projection.
    topBA <- function() {
      df <- AllH %>% filter(Team == team)
      if (nrow(df) == 0 || !"pAVG" %in% colnames(df)) return(character(0))
      if ("pAB" %in% colnames(df)) {
        df <- df %>% filter(pAB > 0) %>%
          mutate(.k = (pAVG - 0.250) * pAB) %>%
          arrange(desc(.k), Player) %>% head(4)
      } else {
        df <- df %>% arrange(desc(pAVG), Player) %>% head(4)
      }
      mapply(function(p, ps, avg) sprintf("%s (%s, %.3f)", p, ps, avg),
             df$Player, hitterPos(df), df$pAVG, USE.NAMES = FALSE)
    }

    # Top 4 pitchers for a counting category (W / K / SV / HD).
    topPitchers <- function(sortKey) {
      df <- AllP %>% filter(Team == team)
      if (!sortKey %in% colnames(df) || nrow(df) == 0) return(character(0))
      df <- df %>% arrange(desc(.data[[sortKey]]), Player) %>% head(4)
      mapply(function(p, ps, v) sprintf("%s (%s, %d)", p, ps, round(v)),
             df$Player, df$Pos, df[[sortKey]],
             USE.NAMES = FALSE)
    }

    # Top 4 pitchers for ERA — volume-weighted (4.00 - pERA) * pIP / 9. Falls
    # back to pERA asc when pIP is missing.
    topERA <- function() {
      df <- AllP %>% filter(Team == team)
      if (nrow(df) == 0 || !"pERA" %in% colnames(df)) return(character(0))
      if ("pIP" %in% colnames(df)) {
        df <- df %>% filter(pIP > 0) %>%
          mutate(.k = (4.00 - pERA) * pIP / 9) %>%
          arrange(desc(.k), Player) %>% head(4)
      } else {
        df <- df %>% arrange(pERA, Player) %>% head(4)
      }
      mapply(function(p, ps, era) sprintf("%s (%s, %.2f)", p, ps, era),
             df$Player, df$Pos, df$pERA, USE.NAMES = FALSE)
    }

    rows <- lapply(cats, function(c) {
      t <- computeTier(c$col, c$reverse)
      tierLabel <- if (is.na(t$tier)) ""
                   else sprintf("%s (%s)", t$tier, fmtVal(c$name, t$value))
      contribs <- switch(c$kind,
                         "H"   = topHitters(c$sortKey),
                         "P"   = topPitchers(c$sortKey),
                         "BA"  = topBA(),
                         "ERA" = topERA(),
                         character(0))
      data.frame(
        Category   = c$name,
        Tier       = tierLabel,
        TierBg     = if (is.na(t$tier)) "" else t$tier,
        Contributors = paste(contribs, collapse = ", "),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })
    df <- do.call(rbind, rows)
    # Rename to display label only at the end (commas/keys clean above).
    names(df)[names(df) == "Contributors"] <- "Top 4 Contributors"

    datatable(df,
              options = list(paging = FALSE, info = FALSE, searching = FALSE,
                             ordering = FALSE, autoWidth = FALSE,
                             columnDefs = list(
                               list(targets = which(names(df) == "TierBg") - 1, visible = FALSE),
                               list(targets = 0, width = "60px"),
                               list(targets = 1, width = "180px")
                             )),
              rownames = FALSE, escape = FALSE) %>%
      formatStyle('Tier',
                  valueColumns = 'TierBg',
                  backgroundColor = styleEqual(
                    c('High', 'Medium', 'Low'),
                    c('#d4edda', '#fff3cd', '#f8d7da')
                  ))
  })

# Category Status — league-wide opportunity (existing)
  output$catSummary <- DT::renderDataTable({
    rv$refreshCount
    datatable(catSummary,options = list(pageLength = 20)) %>%
      formatRound(c('pvp','pvm','opportunity'),2)
  })
```

Notes about the `formatStyle` choice: DT's `styleEqual` matches whole-cell strings, but the `Tier` column contains parenthesized values like `"High (45)"`. The hidden `TierBg` column carries the bare `"High"`/`"Medium"`/`"Low"` and is used as `valueColumns` so the green/yellow/red color is applied based on the bare value while the parenthesized version is what the user sees.

- [ ] **Step 3: Save and re-source the app file (R syntax-check by load only)**

```powershell
$rscript = "C:\Program Files\R\R-4.4.2\bin\Rscript.exe"
& $rscript -e "tryCatch({ p <- parse('LeagueEval/server.R'); cat('parse OK,', length(p), 'top-level expressions\n') }, error = function(e) cat('PARSE ERROR:', conditionMessage(e), '\n'))"
```

Expected: `parse OK, N top-level expressions` (no parse error). If a parse error fires, fix the offending line before continuing.

- [ ] **Step 4: Commit**

```powershell
git add LeagueEval/server.R
git commit -m "feat: add per-team category detail output for LeagueEval Category Status tab"
```

---

## Task 3: Wire the team selector and new table into ui.R

**Files:**
- Modify: `LeagueEval/ui.R` — replace the existing `tabPanel("Category Status", ...)` block at lines 104–109.

- [ ] **Step 1: Open `LeagueEval/ui.R` and locate the Category Status tab**

Existing block:

```r
    tabPanel("Category Status",
             mainPanel(
               h2("Points by Category"),
               DT::dataTableOutput("catSummary")
             )
    ),
```

- [ ] **Step 2: Replace it with the sidebar-layout version**

```r
    tabPanel("Category Status",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput(
                   'teamSelect',
                   'Team',
                   choices  = teams,
                   selected = if ('Liquor Crickets' %in% teams) 'Liquor Crickets' else teams[1]
                 ),
                 width = 2
               ),
               mainPanel(
                 h2("Team Category Detail"),
                 DT::dataTableOutput("teamCatDetail"),
                 br(),
                 h2("Points by Category"),
                 DT::dataTableOutput("catSummary")
               )
             )
    ),
```

`teams` is the global `sort(unique(RTot$Team))` defined at `server.R:7`; it is in scope when `ui.R` is evaluated because Shiny sources both files into the same R environment at app startup.

- [ ] **Step 3: Parse-check `ui.R`**

```powershell
$rscript = "C:\Program Files\R\R-4.4.2\bin\Rscript.exe"
& $rscript -e "tryCatch({ p <- parse('LeagueEval/ui.R'); cat('parse OK,', length(p), 'top-level expressions\n') }, error = function(e) cat('PARSE ERROR:', conditionMessage(e), '\n'))"
```

Expected: `parse OK, N top-level expressions`.

- [ ] **Step 4: Commit**

```powershell
git add LeagueEval/ui.R
git commit -m "feat: add team selector to LeagueEval Category Status tab"
```

---

## Task 4: Manual smoke test

**Files:** None — runtime verification.

The LeagueEval app has no automated test harness. The verification steps below cover the spec's success criteria (selector works, projection swap re-renders, tier values match Statistical Surplus, top-4 lists exclude no-playing-time players for rate stats).

- [ ] **Step 1: Launch the app**

```powershell
$rscript = "C:\Program Files\R\R-4.4.2\bin\Rscript.exe"
& $rscript -e "shiny::runApp('LeagueEval', launch.browser = TRUE, port = 4001)"
```

Expected: a browser opens with the LeagueEval app loaded at `http://localhost:4001`.

- [ ] **Step 2: Navigate to the Category Status tab**

Click the **Category Status** tab. Expected:
- A team selector on the left (sidebar) labeled "Team", defaulting to "Liquor Crickets".
- A "Team Category Detail" heading and table on the right, with 10 rows (HR, RBI, R, SB, BA, W, K, SV, HD, ERA).
- Below it, the existing "Points by Category" table.

- [ ] **Step 3: Cross-check tiers against the Statistical Surplus tab**

Open the **Surplus → Statistical** sub-tab in another browser tab (or remember the values). For Liquor Crickets, every category's tier (`High` / `Medium` / `Low`) on the new table must match the column the team appears in on Statistical Surplus. Spot-check 3 categories minimum (one each: counting hitter, BA, ERA).

- [ ] **Step 4: Switch teams**

Pick a different team in the selector (e.g. "Plankton"). Expected:
- Both the tier column and the contributors column update in one render cycle.
- The "Points by Category" table below does NOT change (it's league-wide).

- [ ] **Step 5: Switch projection source**

Click the Settings (⚙) button → change projection from ATC → Steamer. Expected:
- A "Projection: Steamer" notification appears.
- The Team Category Detail table re-renders with potentially different contributor names / values (different projection produces different `pHR` etc.).
- Tier column may also change because `cstand` rankings depend on projected values flowed through standings.

- [ ] **Step 6: Verify rate-stat filtering**

Pick a team you know has a starter with very low ERA on a tiny sample (e.g., a recently called-up reliever). Confirm the BA and ERA top-4 lists are dominated by players with real volume, not low-sample fluke rates. (If `pAB` / `pIP` were unavailable per Task 1, this check is moot — instead just confirm the lists render.)

- [ ] **Step 7: Verify color coding**

Tier cells should be colored:
- `High (...)` → light green (`#d4edda`)
- `Medium (...)` → light yellow (`#fff3cd`)
- `Low (...)` → light red (`#f8d7da`)

Same colors as Statistical Surplus.

- [ ] **Step 8: Stop the app**

Close the browser tab and `Ctrl+C` in the PowerShell session.

If any step fails, fix the underlying issue and re-run from Step 1. If everything passes, this task is complete.

---

## Self-Review Checklist (post-plan)

- [x] Spec coverage: every column / formula / styling rule from the spec appears in Task 2's code or Task 3's UI.
- [x] No placeholders: each step has runnable code or a concrete observable expectation.
- [x] Type consistency: function names (`computeTier`, `topHitters`, `topBA`, `topPitchers`, `topERA`, `hitterPos`, `fmtVal`) are defined once and referenced consistently. Column names (`Category`, `Tier`, `TierBg`, `Contributors` → `Top 4 Contributors`) match between assembly and rendering.
- [x] Hidden styling column (`TierBg`) is created by Task 2 and hidden via `columnDefs` in the same block — no cross-task references.

---

## Out of scope

Already excluded by spec, restated for plan-readers:
- No new categories, no FIP / QS / etc.
- No free-agent contributors.
- No trade suggestions.
- No threshold tuning.
