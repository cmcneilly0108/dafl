# Protect by Pos Tab Redesign — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the bare "Protect by Pos" tab with a position-centric intelligence view showing which teams need the selected position, their market label, budget, and statistical weaknesses.

**Architecture:** Single new reactive `posNeed_r()` aggregates data from existing reactives (`protClean_r()`, `pstandings_r()`, `currentSummary_r()`, `calcGoals()`). The UI is a sidebar with position dropdown + summary card, and a main panel with one DT table. Old helper functions and outputs are removed.

**Tech Stack:** R / Shiny / DT / dplyr (all already in use)

**Spec:** `docs/superpowers/specs/2026-03-21-protect-by-pos-redesign.md`

---

### Task 1: Update position constants in server.R

**Files:**
- Modify: `LiveDraftTool/server.R:13` — `allpos` constant

- [ ] **Step 1: Change `allpos` to use MR/CL instead of RP**

Replace line 13 in `LiveDraftTool/server.R`:

```r
# OLD:
allpos <- c(hpos,list('SP','RP'))

# NEW:
allpos <- c(hpos,list('SP','MR','CL'))
```

- [ ] **Step 2: Commit**

```bash
git add LiveDraftTool/server.R
git commit -m "feat: update allpos to use MR/CL instead of RP"
```

---

### Task 2: Update ui.R — new Protect by Pos tab layout

**Files:**
- Modify: `LiveDraftTool/ui.R:116-129` — Protect by Pos tabPanel

- [ ] **Step 1: Replace the Protect by Pos tabPanel**

Replace lines 116–129 in `LiveDraftTool/ui.R`:

```r
# OLD:
                   tabPanel("Protect by Pos",
                            sidebarLayout(fluid=FALSE,
                                          sidebarPanel(
                                            selectizeInput(
                                              'e4', 'Select Position', choices=NULL)
                                            ,width=2),
                                          mainPanel(
                                            h2(textOutput("allpos")),
                                            h2(textOutput("uniquePos")),
                                            DT::dataTableOutput("tNeed"),
                                            DT::dataTableOutput("posProtect")
                                          )
                            )
                   ),

# NEW:
                   tabPanel("Protect by Pos",
                            sidebarLayout(fluid=FALSE,
                                          sidebarPanel(
                                            selectizeInput(
                                              'e4', 'Select Position', choices=NULL),
                                            uiOutput("posSummaryCard"),
                                            width=2),
                                          mainPanel(
                                            h3(textOutput("posNeedHeader")),
                                            DT::dataTableOutput("posNeedTable")
                                          )
                            )
                   ),
```

- [ ] **Step 2: Commit**

```bash
git add LiveDraftTool/ui.R
git commit -m "feat: update Protect by Pos tab layout with summary card and single table"
```

---

### Task 3: Remove old helper functions from server.R

**Files:**
- Modify: `LiveDraftTool/server.R:1004-1029` — remove `posProtect()`, `uniqueProtect()`, `teamsInterested()`

- [ ] **Step 1: Delete the three old functions**

Delete lines 1004–1029 in `LiveDraftTool/server.R` (the `posProtect`, `uniqueProtect`, and `teamsInterested` function definitions). These are:

```r
# DELETE ALL OF THIS:
  posProtect <- function(pos) {
    pc <- protClean_r()
    filter(pc, Pos == pos) %>%
      select(Player, Team, Age, pDFL, Salary, Contract, rankDiff, Skew = pSkew) %>% arrange(Team)
  }

  uniqueProtect <- function(pos) {
    pc <- protClean_r()
    res <- filter(pc, Pos == pos) %>% select(Team) %>% unique() %>% nrow()
    paste("Unique Teams = ", res)
  }

  teamsInterested <- function(pos) {
    pc <- protClean_r()
    cs <- currentSummary_r()
    allteams <- data.frame(Team = teams)
    have <- filter(pc, Pos == pos) %>% select(Team) %>% unique()
    need <- anti_join(allteams, have, by = 'Team')
    need <- inner_join(need, cs, by = 'Team')
    if (pos %in% c('SP','RP')) {
      need <- filter(need, group == 'pitching')
    } else {
      need <- filter(need, group == 'hitting')
    }
    arrange(need, -salleft)
  }
```

- [ ] **Step 2: Commit**

```bash
git add LiveDraftTool/server.R
git commit -m "refactor: remove old posProtect/uniqueProtect/teamsInterested helpers"
```

---

### Task 4: Remove old output renderers from server.R

**Files:**
- Modify: `LiveDraftTool/server.R:1326-1350` — remove old `output$allpos`, `output$uniquePos`, `output$posProtect`, `output$tNeed`

Note: line numbers will have shifted after Task 3 deletions. Find these by searching for `# Protect by position` comment.

- [ ] **Step 1: Delete the old output renderers**

Delete the block starting at `# Protect by position` comment through the `output$tNeed` renderDataTable (approximately lines 1326–1350 before Task 3, search for exact location):

```r
# DELETE ALL OF THIS:
  # Protect by position
  updateSelectizeInput(session, 'e4', choices = allpos, selected = 'OF')
  output$allpos <- renderText({ input$e4 })
  output$uniquePos <- renderText({
    req(input$e4)
    uniqueProtect(input$e4)
  })

  output$posProtect <- DT::renderDataTable({
    req(input$e4)
    datatable(posProtect(input$e4),
              options = list(pageLength = 20, autoWidth = FALSE,
                             info = FALSE), filter = 'top') %>%
      formatCurrency('pDFL') %>%
      formatRound(c('Age','rankDiff'), 0) %>%
      formatRound('Skew', 3)
  })

  output$tNeed <- DT::renderDataTable({
    req(input$e4)
    datatable(teamsInterested(input$e4),
              options = list(pageLength = 20, autoWidth = FALSE,
                             paging = FALSE, searching = FALSE, info = FALSE)) %>%
      formatRound('salleft', 0)
  })
```

- [ ] **Step 2: Commit**

```bash
git add LiveDraftTool/server.R
git commit -m "refactor: remove old Protect by Pos output renderers"
```

---

### Task 5: Add `posNeed_r()` reactive

**Files:**
- Modify: `LiveDraftTool/server.R` — add new reactive after the `ppp_r()` reactive (around line 620, after Task 3/4 deletions)

- [ ] **Step 1: Add the `posNeed_r()` reactive**

Insert this after the `ppp_r()` reactive definition (search for `# --- ppp: protected by position ---` and place it after that block):

```r
  # --- posNeed: position intelligence table ---
  posNeed_r <- reactive({
    req(input$e4)
    pos <- input$e4
    pc <- protClean_r()
    ps <- pstandings_r()
    cs <- currentSummary_r()
    rh <- rhitters_r()
    rp <- rpitchers_r()

    # Position thresholds
    posThresholds <- c('OF' = 3, 'SP' = 5)
    threshold <- ifelse(pos %in% names(posThresholds), posThresholds[pos], 1)

    # Count protected per team at this position (using Pos column only)
    posCounts <- pc %>% filter(Pos == pos) %>%
      group_by(Team) %>% summarize(have = n(), .groups = 'drop')

    # All teams, join counts, compute Still Need
    allteams <- data.frame(Team = teams, stringsAsFactors = FALSE)
    need <- left_join(allteams, posCounts, by = 'Team') %>%
      mutate(have = replace_na(have, 0),
             StillNeed = pmax(0, threshold - have)) %>%
      filter(StillNeed > 0)

    if (nrow(need) == 0) return(data.frame(
      Team = character(), StillNeed = integer(), Market = character(),
      CashLeft = numeric(), DPP = numeric(), WeakestStats = character(),
      stringsAsFactors = FALSE
    ))

    # Market label from pstandings DPP ratio
    need <- left_join(need, ps %>% select(Team, CashLeft_total = CashLeft, Needed, DPP), by = 'Team')

    need$Market <- sapply(seq_len(nrow(need)), function(i) {
      tm <- need$Team[i]
      tmRow <- need[i, ]
      others <- ps %>% filter(Team != tm, Needed > 0)
      leagueAvgDPP <- if (sum(others$Needed) > 0) sum(others$CashLeft) / sum(others$Needed) else 0
      ratio <- if (leagueAvgDPP > 0 && tmRow$Needed > 0) tmRow$DPP / leagueAvgDPP else NA
      case_when(
        is.na(ratio) || tmRow$Needed <= 0 ~ "Full",
        ratio >= 1.3 ~ "Strong Buy",
        ratio >= 1.0 ~ "Lean Buy",
        ratio >= 0.8 ~ "Neutral",
        TRUE ~ "Wait"
      )
    })

    # Position-adjusted cash left from currentSummary
    isHitterPos <- pos %in% c('C','1B','2B','SS','3B','OF')
    csGroup <- if (isHitterPos) 'hitting' else 'pitching'
    csSub <- cs %>% filter(group == csGroup) %>% select(Team, salleft)
    need <- left_join(need, csSub, by = 'Team')
    need$CashLeft <- round(need$salleft, 0)

    # Weakest stats per team
    hitterStats <- c('HR','RBI','R','SB')
    pitcherStats <- c('W','K','SV','HLD')
    relevantStats <- if (isHitterPos) hitterStats else pitcherStats

    need$WeakestStats <- sapply(need$Team, function(tm) {
      goals <- calcGoals(rp, rh, targets, tm)
      goals <- goals %>% filter(statistic %in% relevantStats, pc < 0.65) %>%
        arrange(pc) %>% head(3)
      if (nrow(goals) == 0) return('<span style="color:#2ecc71;">On track</span>')
      paste(sapply(seq_len(nrow(goals)), function(j) {
        pct <- round(goals$pc[j] * 100)
        color <- if (goals$pc[j] < 0.50) '#e74c3c' else '#f39c12'
        paste0('<span style="color:', color, ';">', goals$statistic[j], ' ', pct, '%</span>')
      }), collapse = ', ')
    })

    # Sort by market label priority, then DPP descending
    statusOrd <- c("Strong Buy" = 1, "Lean Buy" = 2, "Neutral" = 3, "Wait" = 4, "Full" = 5)
    need <- need %>%
      mutate(ord = statusOrd[Market]) %>%
      arrange(ord, -DPP) %>%
      select(Team, StillNeed, Market, CashLeft, DPP, WeakestStats) %>%
      dplyr::rename(`Still Need` = StillNeed, `$/Player` = DPP, `Cash Left` = CashLeft,
                     `Weakest Stats` = WeakestStats)
    need
  })
```

- [ ] **Step 2: Verify the reactive compiles**

Run the app briefly to check for syntax errors:
```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL/LiveDraftTool && Rscript -e "shiny::runApp('.', port=7654, launch.browser=FALSE)" &
sleep 5 && kill %1
```

If it fails, check the error and fix.

- [ ] **Step 3: Commit**

```bash
git add LiveDraftTool/server.R
git commit -m "feat: add posNeed_r() reactive for position intelligence table"
```

---

### Task 6: Add new output renderers

**Files:**
- Modify: `LiveDraftTool/server.R` — add outputs where the old `# Protect by position` block was removed

- [ ] **Step 1: Add the new outputs**

Insert these where the old Protect by Position renderers were removed (search for `# Static outputs that don't change with drafting` and place before it):

```r
  # Protect by position — new intelligence view
  updateSelectizeInput(session, 'e4', choices = allpos, selected = 'OF')

  output$posNeedHeader <- renderText({
    req(input$e4)
    paste("Teams That Need", input$e4)
  })

  output$posSummaryCard <- renderUI({
    req(input$e4)
    pos <- input$e4
    pc <- protClean_r()
    atPos <- pc %>% filter(Pos == pos)
    nProtected <- nrow(atPos)
    posThresholds <- c('OF' = 3, 'SP' = 5)
    threshold <- ifelse(pos %in% names(posThresholds), posThresholds[pos], 1)
    posCounts <- atPos %>% group_by(Team) %>% summarize(have = n(), .groups = 'drop')
    allteams <- data.frame(Team = teams, stringsAsFactors = FALSE)
    nTeamsNeed <- left_join(allteams, posCounts, by = 'Team') %>%
      mutate(have = replace_na(have, 0)) %>%
      filter(have < threshold) %>% nrow()
    avgSal <- if (nProtected > 0) round(mean(atPos$Salary, na.rm = TRUE)) else 0
    avgVal <- if (nProtected > 0) round(mean(atPos$pDFL, na.rm = TRUE)) else 0

    tags$div(style = "margin-top:15px; background:#f8f9fa; border-radius:6px; padding:12px;",
      tags$div(style = "font-weight:bold; margin-bottom:8px; border-bottom:1px solid #dee2e6; padding-bottom:6px;",
               "Position Summary"),
      tags$table(class = "table table-condensed", style = "margin-bottom:0; font-size:12px;",
        tags$tr(tags$td("Protected"), tags$td(style = "text-align:right; font-weight:bold;", nProtected)),
        tags$tr(tags$td("Teams Need"), tags$td(style = "text-align:right; font-weight:bold; color:#e74c3c;", nTeamsNeed)),
        tags$tr(tags$td("Avg Salary"), tags$td(style = "text-align:right; font-weight:bold;", paste0("$", avgSal))),
        tags$tr(tags$td("Avg Value"), tags$td(style = "text-align:right; font-weight:bold;", paste0("$", avgVal)))
      )
    )
  })

  output$posNeedTable <- DT::renderDataTable({
    req(input$e4)
    data <- posNeed_r()
    datatable(data, escape = FALSE, rownames = FALSE,
              options = list(pageLength = 20, autoWidth = FALSE,
                             paging = FALSE, searching = FALSE, info = FALSE,
                             ordering = FALSE)) %>%
      formatRound('$/Player', 0) %>%
      formatStyle('Market',
                  backgroundColor = styleEqual(
                    c('Strong Buy', 'Lean Buy', 'Neutral', 'Wait', 'Full'),
                    c('#d4edda', '#d4edda', '#fff3cd', '#f8d7da', '#e9ecef')))
  })
```

Note: `escape = FALSE` is critical — it allows the HTML color spans in `Weakest Stats` to render.

- [ ] **Step 2: Commit**

```bash
git add LiveDraftTool/server.R
git commit -m "feat: add Protect by Pos summary card and intelligence table renderers"
```

---

### Task 7: Smoke test the full app

**Files:** None — this is a manual verification step.

- [ ] **Step 1: Launch the app**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL/LiveDraftTool && Rscript -e "shiny::runApp('.', port=7654)"
```

- [ ] **Step 2: Verify the Protect by Pos tab**

Check all of these in the browser:

1. Position dropdown shows: C, 1B, 2B, SS, 3B, OF, SP, MR, CL
2. Selecting OF shows teams needing outfielders (Still Need accounts for 3 required)
3. Selecting SP shows teams needing starters (Still Need accounts for 5 required)
4. Selecting C shows teams needing a catcher (Still Need uses threshold of 1)
5. Summary card updates with Protected count, Teams Need count, Avg Salary, Avg Value
6. Market labels are color-coded (green for Strong Buy/Lean Buy, yellow Neutral, red Wait)
7. Weakest Stats shows only hitter stats (HR, RBI, R, SB) for hitter positions
8. Weakest Stats shows only pitcher stats (W, K, SV, HLD) for pitcher positions
9. Weakest Stats colors: red for <50%, orange for 50-65%, green "On track" if all >=65%
10. Table sorts by market tier then $/Player descending
11. Other tabs still work (Draft, Nominations, Rosters, Hitters, Pitchers, etc.)

- [ ] **Step 3: Fix any issues found, then commit**

```bash
git add LiveDraftTool/server.R LiveDraftTool/ui.R
git commit -m "fix: address smoke test issues in Protect by Pos tab"
```

Only commit this step if fixes were needed. Skip if everything passed.
