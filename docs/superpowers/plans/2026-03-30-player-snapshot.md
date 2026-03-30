# Player Snapshot Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a snapshot detail panel below the Search table that shows projected stats, positional context, competition, comparables, and injury status when a player row is clicked.

**Architecture:** The search table gains row selection. A single `renderUI` assembles five data sections by reusing existing reactives (AllH_active, pstandings_r, protClean_r, currentSummary_r, injOrig_full). Positional need logic is extracted into a shared helper so both the Positional Pressure tab and snapshot use the same code.

**Tech Stack:** R/Shiny, DT, htmltools

---

### Task 1: Add uiOutput to UI and enable row selection on search table

**Files:**
- Modify: `LiveDraftTool/ui.R:262-266`
- Modify: `LiveDraftTool/server.R:370-384` (searchTable renderer)

- [ ] **Step 1: Update ui.R — rename tab and add snapshot output**

Change the Search tab panel from:
```r
tabPanel("Search",
         verticalLayout(
           DT::dataTableOutput("searchTable")
         )
)
```
to:
```r
tabPanel("Player Snapshot",
         verticalLayout(
           DT::dataTableOutput("searchTable"),
           uiOutput("playerSnapshot")
         )
)
```

- [ ] **Step 2: Enable row selection on the search table**

In `server.R`, the `searchTable` datatable call currently has no `selection` parameter. Add `selection = 'single'`:

```r
dt <- datatable(data, selection = 'single',
          options = list(pageLength = 25, autoWidth = FALSE, info = FALSE),
          filter = 'top', escape = FALSE) %>%
```

- [ ] **Step 3: Commit**

```bash
git add LiveDraftTool/ui.R LiveDraftTool/server.R
git commit -m "feat: rename Search tab to Player Snapshot and add selection + uiOutput"
```

---

### Task 2: Extract positional need helper function

**Files:**
- Modify: `LiveDraftTool/server.R` — add `getPositionalNeed()` helper, refactor `posNeed_r` to use it

- [ ] **Step 1: Add the helper function**

Add this function right before the existing `posNeed_r` reactive (around line 618). It takes a position string and the needed reactive values as arguments, and returns the need dataframe (without teamLink applied — the caller decides on formatting):

```r
  # --- Shared helper: compute which teams need a position ---
  getPositionalNeed <- function(pos, pc, ps, cs, rh, rp) {
    threshold <- ifelse(pos %in% names(posThresholds), posThresholds[pos], 1)

    posCounts <- pc %>% filter(Pos == pos) %>%
      group_by(Team) %>% summarize(have = n(), .groups = 'drop')

    allteams <- data.frame(Team = teams, stringsAsFactors = FALSE)
    need <- left_join(allteams, posCounts, by = 'Team') %>%
      mutate(have = replace_na(have, 0),
             StillNeed = pmax(0, threshold - have)) %>%
      filter(StillNeed > 0)

    if (nrow(need) == 0) return(data.frame(
      Team = character(), StillNeed = integer(), Market = character(),
      CashLeft = numeric(), MaxBid = numeric(), DPP = numeric(),
      WeakestStats = character(),
      stringsAsFactors = FALSE
    ))

    need <- left_join(need, ps %>% select(Team, Needed, DPP, CashLeft), by = 'Team')

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

    isHitterPos <- pos %in% c('C','1B','2B','SS','3B','OF')
    csGroup <- if (isHitterPos) 'hitting' else 'pitching'
    csSub <- cs %>% filter(group == csGroup) %>% select(Team, salleft, needed)
    fullBudget <- if (isHitterPos) cap * (1 - hpratio) else cap * hpratio
    fullNeeded <- if (isHitterPos) nhitters else npitchers
    need <- left_join(need, csSub, by = 'Team', suffix = c('', '.cs'))
    need$CashLeft <- round(replace_na(need$salleft, fullBudget), 0)
    groupNeeded <- replace_na(need$needed.cs, fullNeeded)
    need$MaxBid <- pmax(1, need$CashLeft - pmax(0, groupNeeded - 1))

    hitterStats <- c('HR','RBI','R','SB')
    pitcherStats <- c('W','K','SV','HLD')
    relevantStats <- if (isHitterPos) hitterStats else pitcherStats

    need$WeakestStats <- sapply(need$Team, function(tm) {
      goals <- calcGoals(rp, rh, targets, tm)
      goals <- goals %>% filter(statistic %in% relevantStats, pc < 0.75) %>%
        arrange(pc) %>% head(3)
      if (nrow(goals) == 0) return('<span style="color:#2ecc71;">On track</span>')
      paste(sapply(seq_len(nrow(goals)), function(j) {
        pct <- round(goals$pc[j] * 100)
        color <- if (goals$pc[j] < 0.50) '#e74c3c' else if (goals$pc[j] < 0.65) '#f39c12' else '#c5a000'
        paste0('<span style="color:', color, ';">', goals$statistic[j], ' ', pct, '%</span>')
      }), collapse = ', ')
    })

    statusOrd <- c("Strong Buy" = 1, "Lean Buy" = 2, "Neutral" = 3, "Wait" = 4, "Full" = 5)
    need %>%
      mutate(ord = statusOrd[Market]) %>%
      arrange(ord, -DPP) %>%
      select(Team, StillNeed, Market, CashLeft, MaxBid, DPP, WeakestStats)
  }
```

- [ ] **Step 2: Refactor posNeed_r to use the helper**

Replace the body of `posNeed_r` with:

```r
  posNeed_r <- reactive({
    req(input$e4)
    need <- getPositionalNeed(input$e4, protClean_r(), pstandings_r(),
                              currentSummary_r(), rhitters_r(), rpitchers_r())
    if (nrow(need) == 0) return(data.frame(
      Team = character(), `Still Need` = integer(), Market = character(),
      `Cash Left` = numeric(), `Max Bid` = numeric(), `$/Player` = numeric(),
      `Weakest Stats` = character(),
      stringsAsFactors = FALSE, check.names = FALSE
    ))
    need %>%
      mutate(Team = teamLink(Team)) %>%
      dplyr::rename(`Still Need` = StillNeed, `$/Player` = DPP, `Cash Left` = CashLeft,
                     `Max Bid` = MaxBid, `Weakest Stats` = WeakestStats)
  })
```

- [ ] **Step 3: Verify Positional Pressure tab still works**

Reload the app. Navigate to Positional Pressure tab, select a position. Confirm the table renders with the same data as before (team links, market labels, max bid, weakest stats).

- [ ] **Step 4: Commit**

```bash
git add LiveDraftTool/server.R
git commit -m "refactor: extract getPositionalNeed helper for reuse by Player Snapshot"
```

---

### Task 3: Build the Player Snapshot renderer

**Files:**
- Modify: `LiveDraftTool/server.R` — add `output$playerSnapshot` renderUI

- [ ] **Step 1: Add the snapshot renderer**

Add this right after the `searchTable` renderer (after line 384):

```r
  # --- Player Snapshot detail panel ---
  output$playerSnapshot <- renderUI({
    sel <- input$searchTable_rows_selected
    if (is.null(sel) || length(sel) == 0) return(NULL)

    data <- searchData_r()
    if (sel > nrow(data)) return(NULL)
    pid <- as.character(data$playerid[sel])

    # Look up full player record
    ah <- AllH_active()
    ap <- AllP_active()
    playerH <- ah %>% filter(playerid == pid)
    playerP <- ap %>% filter(playerid == pid)
    isHitter <- nrow(playerH) > 0
    if (!isHitter && nrow(playerP) == 0) return(tags$div(style = "color:gray; padding:12px;", "Player not found in projection data."))
    player <- if (isHitter) playerH[1,] else playerP[1,]

    playerName <- player$Player
    playerPos <- player$Pos
    posElStr <- if (!is.null(player$posEl) && !is.na(player$posEl)) paste0(" (", player$posEl, ")") else ""
    playerAge <- round(player$Age)
    playerMLB <- player$MLB
    ownerRow <- data[sel, ]
    ownerStr <- if (!is.na(ownerRow$Owner) && ownerRow$Owner != "Free Agent") {
      paste0(ownerRow$Owner, " — $", ownerRow$Salary)
    } else {
      "Free Agent"
    }

    # --- Header ---
    headerUI <- tags$div(
      style = "padding:10px 14px; background:#2c3e50; color:white; border-radius:6px 6px 0 0; font-size:16px;",
      tags$strong(playerName),
      tags$span(style = "margin-left:12px;", paste0(playerPos, posElStr)),
      tags$span(style = "margin-left:12px;", playerMLB),
      tags$span(style = "margin-left:12px;", paste0("Age ", playerAge)),
      tags$span(style = "float:right; font-size:14px;", ownerStr)
    )

    # --- Stats Card ---
    if (isHitter) {
      statsUI <- tags$div(
        style = "padding:10px 14px; background:#f8f9fa; border-left:1px solid #ddd; border-right:1px solid #ddd;",
        tags$div(style = "display:flex; gap:20px; flex-wrap:wrap; align-items:center;",
          tags$span(tags$strong("DFL: "), paste0("$", round(player$pDFL))),
          tags$span(tags$strong("SGP: "), round(player$pSGP, 2)),
          tags$span(tags$strong("ADP: "), round(player$pADP)),
          tags$span(tags$strong("Rank Diff: "), ifelse(!is.na(player$rankDiff), sprintf("%+d", round(player$rankDiff)), "—")),
          tags$span(style = "margin-left:20px; color:#666;", "|"),
          tags$span(tags$strong("HR: "), round(player$pHR)),
          tags$span(tags$strong("RBI: "), round(player$pRBI)),
          tags$span(tags$strong("R: "), round(player$pR)),
          tags$span(tags$strong("SB: "), round(player$pSB)),
          tags$span(tags$strong("AVG: "), sprintf("%.3f", player$pAVG))
        )
      )
    } else {
      statsUI <- tags$div(
        style = "padding:10px 14px; background:#f8f9fa; border-left:1px solid #ddd; border-right:1px solid #ddd;",
        tags$div(style = "display:flex; gap:20px; flex-wrap:wrap; align-items:center;",
          tags$span(tags$strong("DFL: "), paste0("$", round(player$pDFL))),
          tags$span(tags$strong("SGP: "), round(player$pSGP, 2)),
          tags$span(tags$strong("ADP: "), round(player$pADP)),
          tags$span(tags$strong("Rank Diff: "), ifelse(!is.na(player$rankDiff), sprintf("%+d", round(player$rankDiff)), "—")),
          tags$span(style = "margin-left:20px; color:#666;", "|"),
          tags$span(tags$strong("W: "), round(player$pW)),
          tags$span(tags$strong("SO: "), round(player$pSO)),
          tags$span(tags$strong("ERA: "), sprintf("%.2f", player$pERA)),
          tags$span(tags$strong("SV: "), round(player$pSV)),
          tags$span(tags$strong("HLD: "), round(player$pHLD))
        )
      )
    }

    # --- Competition Card ---
    ps <- pstandings_r()
    pc <- protClean_r()
    me <- ps %>% filter(Team == myTeam())
    if (nrow(me) > 0 && me$Needed > 0) {
      myMaxBid <- me$CashLeft - (me$Needed - 1)
      otherTeams <- ps %>% filter(Team != myTeam(), Needed > 0)
      competitors <- lapply(seq_len(nrow(otherTeams)), function(i) {
        tm <- otherTeams$Team[i]
        tmRow <- otherTeams[i,]
        theirMaxBid <- tmRow$CashLeft - (tmRow$Needed - 1)
        if (theirMaxBid <= myMaxBid) return(NULL)
        theirPosCounts <- pc %>% filter(Team == tm, Pos == playerPos) %>% nrow()
        threshold <- ifelse(playerPos %in% names(posThresholds), posThresholds[playerPos], 1)
        needsStarter <- theirPosCounts < threshold
        hasBenchRoom <- tmRow$Needed > 0
        if (!needsStarter && !hasBenchRoom) return(NULL)
        reason <- if (needsStarter) paste0("needs ", playerPos) else "bench"
        data.frame(Team = tm, MaxBid = theirMaxBid, Reason = reason, stringsAsFactors = FALSE)
      })
      competitors <- bind_rows(competitors)
      nComp <- nrow(competitors)
      compColor <- if (nComp <= 1) "#2ecc71" else if (nComp <= 3) "#f39c12" else "#e74c3c"
      compHeadline <- paste0(nComp, " team", if (nComp != 1) "s", " can outbid you")
      if (nComp > 0) {
        competitors <- competitors %>% arrange(-MaxBid)
        compLines <- lapply(seq_len(nComp), function(j) {
          tags$div(style = "font-size:12px; padding:1px 0;",
            paste0(competitors$Team[j], " ($", competitors$MaxBid[j], " max, ", competitors$Reason[j], ")"))
        })
      } else {
        compLines <- list(tags$div(style = "font-size:12px; color:#2ecc71;", "No one can outbid you!"))
      }
      competitionUI <- tags$div(
        tags$div(style = "font-size:11px; color:#888;", paste0("Your max bid: $", myMaxBid)),
        tags$div(style = paste0("margin-top:4px; padding:6px; border-radius:4px; background:", compColor, "15;"),
          tags$strong(style = paste0("color:", compColor, "; font-size:13px;"), compHeadline),
          tags$div(style = "margin-top:4px;", compLines)
        )
      )
    } else {
      competitionUI <- tags$div(style = "color:#888; font-size:12px;", "Select your team in Settings to see competition.")
    }

    # --- Positional Context Card ---
    need <- getPositionalNeed(playerPos, pc, ps, currentSummary_r(), rhitters_r(), rpitchers_r())
    if (nrow(need) > 0) {
      avgDPP <- round(mean(need$DPP, na.rm = TRUE))
      needTop <- head(need, 5)
      needLines <- lapply(seq_len(nrow(needTop)), function(j) {
        mktColor <- switch(needTop$Market[j],
          "Strong Buy" = "#155724", "Lean Buy" = "#155724",
          "Neutral" = "#856404", "Wait" = "#721c24", "Full" = "#666", "#666")
        tags$div(style = "font-size:12px; padding:1px 0;",
          paste0(needTop$Team[j], " — "),
          tags$span(style = paste0("color:", mktColor, ";"), needTop$Market[j]),
          paste0(" ($", needTop$MaxBid[j], " max)"))
      })
      posContextUI <- tags$div(
        tags$strong(style = "font-size:13px;", paste0(nrow(need), " teams need ", playerPos)),
        tags$div(style = "font-size:11px; color:#888;", paste0("Avg $/Player: $", avgDPP)),
        tags$div(style = "margin-top:4px;", needLines)
      )
    } else {
      posContextUI <- tags$div(style = "color:#2ecc71; font-size:13px;",
                               paste0("No teams need ", playerPos, " as a starter"))
    }

    # --- Comparables Card ---
    if (isHitter) {
      comps <- AllH_avail() %>% filter(Pos == playerPos, playerid != pid,
                                        pDFL >= player$pDFL - 10, pDFL <= player$pDFL + 10) %>%
        arrange(-pDFL) %>% head(5)
    } else {
      comps <- AllP_avail() %>% filter(Pos == playerPos, playerid != pid,
                                        pDFL >= player$pDFL - 10, pDFL <= player$pDFL + 10) %>%
        arrange(-pDFL) %>% head(5)
    }
    if (nrow(comps) > 0) {
      compRows <- lapply(seq_len(nrow(comps)), function(j) {
        tags$div(style = "font-size:12px; padding:1px 0;",
          tags$span(style = "display:inline-block; width:180px;", comps$Player[j]),
          tags$span(style = "display:inline-block; width:70px;", paste0("$", round(comps$pDFL[j]))),
          tags$span(paste0("ADP ", round(comps$pADP[j]))))
      })
      comparablesUI <- tags$div(
        tags$strong(style = "font-size:13px;", "Comparable Players"),
        tags$div(style = "margin-top:4px;", compRows)
      )
    } else {
      comparablesUI <- tags$div(style = "color:#888; font-size:12px;", "No comparable players at this position.")
    }

    # --- Injury line ---
    injData <- injOrig_full %>% filter(playerid == pid)
    injUI <- if (nrow(injData) > 0) {
      tags$div(style = "padding:6px 14px; background:#fff3cd; border-radius:0 0 6px 6px; border:1px solid #ddd; border-top:none; font-size:12px;",
        tags$strong("Injury: "), injData$Injury[1],
        tags$span(style = "margin-left:12px;", tags$strong("Status: "), injData$status[1]),
        tags$span(style = "margin-left:12px;", tags$strong("Update: "), injData$`Latest Update`[1])
      )
    } else {
      NULL
    }

    # --- Assemble ---
    cardStyle <- "border:1px solid #ddd; padding:10px 14px;"
    tags$div(style = "margin-top:16px; border-radius:6px; overflow:hidden;",
      headerUI,
      statsUI,
      tags$div(style = "display:grid; grid-template-columns:1fr 1fr; border-left:1px solid #ddd; border-right:1px solid #ddd;",
        tags$div(style = cardStyle, competitionUI),
        tags$div(style = cardStyle, posContextUI)
      ),
      tags$div(style = "border:1px solid #ddd; border-top:none; padding:10px 14px;",
        comparablesUI
      ),
      injUI
    )
  })
```

- [ ] **Step 2: Verify the snapshot works**

Reload the app. Go to Player Snapshot tab.
1. Click a hitter row — snapshot appears with hitting stats, competition, positional context, comparables
2. Click a pitcher row — snapshot shows pitching stats
3. Click the same row again — snapshot disappears
4. Click a player with injury data — injury line appears at bottom
5. Check that the Positional Pressure tab still works correctly

- [ ] **Step 3: Commit**

```bash
git add LiveDraftTool/server.R
git commit -m "feat: add Player Snapshot detail panel with stats, competition, position context, and comparables"
```
