# Research Tab Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a Research tab to LiveDraftTool that scrapes baseball articles, uses Claude to extract player recommendations, matches them to the DAFL player pool, filters to free agents, and displays with full stats and targeting support.

**Architecture:** User pastes a URL, we scrape with `rvest`, send article text to `callClaudeAPI()` for structured player extraction (names + summaries + tags), match returned names to `AllH_avail()`/`AllP_avail()` reactive pools using exact + fuzzy matching, then display in split Hitters/Pitchers DT tables with targeting.

**Tech Stack:** R Shiny, rvest, jsonlite, httr, callClaudeAPI (Claude Sonnet), DT

**Spec:** `docs/superpowers/specs/2026-03-23-research-tab-design.md`

---

### Task 1: Add shinyjs dependency and Research tab UI

**Files:**
- Modify: `LiveDraftTool/ui.R` (add shinyjs, add Research tab before "My Targets")
- Modify: `LiveDraftTool/server.R:3` (add shinyjs library)

- [ ] **Step 1: Install shinyjs if not present**

Run: `Rscript -e "if (!require('shinyjs')) install.packages('shinyjs')"`

- [ ] **Step 2: Add `library("shinyjs")` to ui.R**

At the top of `ui.R`, after `library("DT")` (line 2), add:

```r
library("shinyjs")
```

- [ ] **Step 3: Add `useShinyjs()` inside the navbarPage**

Inside the `navbarPage()` call, after the `theme = bs_theme(bootswatch = "flatly"),` line (line 5 of ui.R), add as the first element:

```r
    useShinyjs(),
```

- [ ] **Step 4: Add `library(shinyjs)` to server.R**

At the top of `server.R`, after `library(ggplot2)` (line 3), add:

```r
library(shinyjs)
```

- [ ] **Step 5: Add the Research tabPanel to ui.R**

Insert before the "My Targets" `tabPanel` (line 230). The new tab goes between "Injured" and "My Targets":

```r
                   tabPanel("Research",
                            sidebarLayout(fluid = FALSE,
                              sidebarPanel(
                                textInput('researchUrl', 'Article URL',
                                          placeholder = 'https://www.fangraphs.com/...'),
                                actionButton('analyzeBtn', 'Analyze Article',
                                             class = 'btn-primary',
                                             style = 'width:100%; margin-bottom:10px;'),
                                uiOutput('researchStatus'),
                                tags$hr(),
                                actionButton('targetResBtn', 'Toggle Target',
                                             class = 'btn-info btn-sm',
                                             style = 'width:100%;'),
                                width = 3
                              ),
                              mainPanel(
                                tabsetPanel(id = 'researchTab', type = 'tabs',
                                  tabPanel('Hitters',
                                           DT::dataTableOutput('researchH')),
                                  tabPanel('Pitchers',
                                           DT::dataTableOutput('researchP'))
                                ),
                                uiOutput('researchUnmatched')
                              )
                            )
                   ),
```

- [ ] **Step 6: Verify the app still loads**

Run: `cd /Users/cmcneilly/Dropbox/Personal/DAFL/LiveDraftTool && Rscript -e "shiny::runApp('.', launch.browser=FALSE, port=7654)" &`

Check that the app starts without parse errors. Kill the process after confirming. The Research tab will show empty tables — that's expected.

- [ ] **Step 7: Commit**

```bash
git add LiveDraftTool/ui.R LiveDraftTool/server.R
git commit -m "feat: add shinyjs and Research tab UI to LiveDraftTool"
```

---

### Task 2: Add article scraping and LLM extraction logic

**Files:**
- Modify: `LiveDraftTool/server.R` (add before the closing `})` at line 2291)

- [ ] **Step 1: Add reactive values for research state**

Add to the existing `rv <- reactiveValues(...)` block (around line 76-110 in server.R). Add these fields inside the existing `reactiveValues()` call:

```r
    researchH = data.frame(),
    researchP = data.frame(),
    researchUnmatched = character(0),
    researchTitle = ""
```

- [ ] **Step 2: Add the article scraping and LLM extraction `observeEvent`**

Insert before the closing `})` of `shinyServer` (before line 2291):

```r
  # --- Research tab: article scraping + LLM extraction ---
  observeEvent(input$analyzeBtn, {
    url <- trimws(input$researchUrl)
    if (url == "" || !grepl("^https?://", url)) {
      showNotification("Please enter a valid URL", type = "warning")
      return()
    }

    # Disable button during processing
    shinyjs::disable("analyzeBtn")
    showNotification("Fetching article...", type = "message", duration = NULL, id = "researchMsg")

    tryCatch({
      # Step 1: Scrape article
      page <- rvest::read_html(url)
      articleText <- page %>%
        rvest::html_nodes("p, h1, h2, h3, h4, li") %>%
        rvest::html_text2() %>%
        paste(collapse = "\n\n")

      # Smart truncation: keep headings + first content after each
      if (nchar(articleText) > 12000) {
        articleText <- substr(articleText, 1, 12000)
      }

      # Extract title
      pageTitle <- tryCatch(
        page %>% rvest::html_node("title") %>% rvest::html_text2(),
        error = function(e) "Unknown Article"
      )
      sourceDomain <- gsub("^https?://([^/]+).*", "\\1", url)

      # Step 2: Call Claude API
      removeNotification("researchMsg")
      showNotification("Analyzing with Claude...", type = "message", duration = NULL, id = "researchMsg")

      prompt <- paste0(
        'You are a baseball fantasy analyst assistant. Extract all baseball players ',
        'mentioned in the following article. For each player the author is highlighting ',
        'as a target, sleeper, breakout, value pick, or otherwise recommending, return ',
        'a JSON array with these fields:\n\n',
        '- full_name: the player\'s full name (first and last)\n',
        '- summary: one sentence describing why the author thinks this player is interesting\n',
        '- tags: comma-separated list from these options: Sleeper, Breakout, Bounce-back, ',
        'Value, Upside, Buy-low, Sell-high, Injury-risk, Closer, Holds, Steals, Power, ',
        'AVG, Pitching, Strikeouts, Saves, Speed, Ratios\n\n',
        'Only include players the author is specifically recommending or discussing ',
        'positively. Skip players mentioned only in passing or as comparisons.\n\n',
        'Return ONLY the JSON array, no other text. Example:\n',
        '[{"full_name": "Luis Arraez", "summary": "Hitting .340 in spring with strong ',
        'lineup protection boosting BA and R upside", "tags": "Sleeper, AVG, Value"}]\n\n',
        'Article text:\n', articleText
      )

      response <- callClaudeAPI(prompt)

      # Step 3: Parse response
      if (!grepl("^\\s*\\[", response)) {
        # Might be an error string — retry with simplified prompt
        retryPrompt <- paste0(
          'Return a JSON array of objects with fields: full_name, summary, tags. ',
          'Example: [{"full_name":"Mike Trout","summary":"Still elite","tags":"Power"}]. ',
          'Extract players recommended in this article:\n\n',
          substr(articleText, 1, 4000)
        )
        response <- callClaudeAPI(retryPrompt)
        if (!grepl("^\\s*\\[", response)) {
          removeNotification("researchMsg")
          showNotification(paste0("Claude API error: ", substr(response, 1, 200)), type = "error", duration = 10)
          shinyjs::enable("analyzeBtn")
          return()
        }
      }

      extracted <- tryCatch(
        jsonlite::fromJSON(response),
        error = function(e) {
          removeNotification("researchMsg")
          showNotification(paste0("Failed to parse Claude response: ", e$message), type = "error", duration = 10)
          shinyjs::enable("analyzeBtn")
          return(NULL)
        }
      )

      if (is.null(extracted) || nrow(extracted) == 0) {
        removeNotification("researchMsg")
        showNotification("No players found in this article", type = "warning")
        rv$researchH <- data.frame()
        rv$researchP <- data.frame()
        rv$researchUnmatched <- character(0)
        rv$researchTitle <- pageTitle
        shinyjs::enable("analyzeBtn")
        return()
      }

      # Step 4: Match to player pools
      removeNotification("researchMsg")
      showNotification("Matching players...", type = "message", duration = NULL, id = "researchMsg")

      availH <- AllH_avail()
      availP <- AllP_avail()
      allAvail <- bind_rows(
        availH %>% mutate(poolType = "H"),
        availP %>% mutate(poolType = "P")
      )
      cleanNames <- tolower(allAvail$Player)

      matchedRows <- list()
      unmatched <- character(0)

      for (i in seq_len(nrow(extracted))) {
        fname <- extracted$full_name[i]
        fnameL <- tolower(fname)

        # Exact match
        exactIdx <- which(cleanNames == fnameL)
        if (length(exactIdx) > 0) {
          row <- allAvail[exactIdx[1], ]
          row$Tags <- extracted$tags[i]
          row$Summary <- extracted$summary[i]
          row$fuzzy <- FALSE
          matchedRows <- c(matchedRows, list(row))
          next
        }

        # Fuzzy match
        fuzzyIdx <- agrep(fnameL, cleanNames, max.distance = 0.15, ignore.case = TRUE)
        if (length(fuzzyIdx) > 0) {
          row <- allAvail[fuzzyIdx[1], ]
          row$Tags <- extracted$tags[i]
          row$Summary <- extracted$summary[i]
          row$fuzzy <- TRUE
          matchedRows <- c(matchedRows, list(row))
        } else {
          unmatched <- c(unmatched, fname)
        }
      }

      if (length(matchedRows) == 0) {
        removeNotification("researchMsg")
        showNotification("No matched free agents found", type = "warning")
        rv$researchH <- data.frame()
        rv$researchP <- data.frame()
        rv$researchUnmatched <- unmatched
        rv$researchTitle <- pageTitle
        shinyjs::enable("analyzeBtn")
        return()
      }

      matched <- bind_rows(matchedRows)

      # Add fuzzy prefix
      matched$Player <- ifelse(matched$fuzzy,
                               paste0("~ ", matched$Player),
                               matched$Player)

      # Split into H and P
      mH <- matched %>% filter(poolType == "H") %>%
        mutate(Player = fgLink(Player, playerid)) %>%
        arrange(-pDFL) %>%
        select(Player, Pos, Tags, Summary, Age, DFL = pDFL, SGP = pSGP, ADP = pADP,
               HR = pHR, RBI = pRBI, R = pR, SB = pSB, AVG = pAVG,
               Injury, Expected.Return, playerid)

      mP <- matched %>% filter(poolType == "P") %>%
        mutate(Player = fgLink(Player, playerid)) %>%
        arrange(-pDFL) %>%
        select(Player, Pos, Tags, Summary, Age, DFL = pDFL, SGP = pSGP, ADP = pADP,
               W = pW, SO = pSO, ERA = pERA, SV = pSV, HLD = pHLD, `K/9` = `pK/9`,
               Injury, Expected.Return, playerid)

      rv$researchH <- mH
      rv$researchP <- mP
      rv$researchUnmatched <- unmatched
      rv$researchTitle <- paste0(pageTitle, " (", sourceDomain, ")")

      removeNotification("researchMsg")
      showNotification(paste0("Found ", nrow(matched), " free agent(s) from article"), type = "message")
      shinyjs::enable("analyzeBtn")

    }, error = function(e) {
      removeNotification("researchMsg")
      showNotification(paste0("Error: ", e$message), type = "error", duration = 10)
      shinyjs::enable("analyzeBtn")
    })
  })
```

- [ ] **Step 3: Commit**

```bash
git add LiveDraftTool/server.R
git commit -m "feat: add article scraping and LLM extraction for Research tab"
```

---

### Task 3: Add Research tab DT rendering and status display

**Files:**
- Modify: `LiveDraftTool/server.R` (add before the closing `})`)

- [ ] **Step 1: Add the DT render outputs and status UI**

Insert in server.R before the closing `})`, after the analyzeBtn observeEvent:

```r
  # --- Research tab: render tables ---
  output$researchH <- DT::renderDataTable({
    df <- rv$researchH
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Message = "No hitters found. Paste an article URL and click Analyze."),
                       options = list(dom = 't'), selection = 'none'))
    }
    datatable(df, selection = 'single', escape = FALSE,
              options = list(pageLength = 20, columnDefs = list(
                list(visible = FALSE, targets = which(names(df) == "playerid") - 1)
              ))) %>%
      formatCurrency('DFL') %>%
      formatRound(c('SGP', 'AVG'), 3) %>%
      formatRound(c('Age', 'HR', 'RBI', 'R', 'SB'), 0)
  })

  output$researchP <- DT::renderDataTable({
    df <- rv$researchP
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Message = "No pitchers found. Paste an article URL and click Analyze."),
                       options = list(dom = 't'), selection = 'none'))
    }
    datatable(df, selection = 'single', escape = FALSE,
              options = list(pageLength = 20, columnDefs = list(
                list(visible = FALSE, targets = which(names(df) == "playerid") - 1)
              ))) %>%
      formatCurrency('DFL') %>%
      formatRound(c('SGP', 'ERA', 'K/9'), 3) %>%
      formatRound(c('Age', 'W', 'SO', 'SV', 'HLD'), 0)
  })

  # --- Research tab: status display ---
  output$researchStatus <- renderUI({
    title <- rv$researchTitle
    nH <- nrow(rv$researchH)
    nP <- nrow(rv$researchP)
    if (title == "" && nH == 0 && nP == 0) return(NULL)
    tags$div(style = "margin-top:10px; font-size:13px; line-height:1.6;",
      tags$strong(title),
      tags$br(),
      paste0(nH, " hitter(s), ", nP, " pitcher(s) found")
    )
  })

  # --- Research tab: unmatched players ---
  output$researchUnmatched <- renderUI({
    um <- rv$researchUnmatched
    if (length(um) == 0) return(NULL)
    tags$div(style = "margin-top:10px; font-size:12px; color:#888;",
      tags$em(paste0("Could not match: ", paste(um, collapse = ", ")))
    )
  })
```

- [ ] **Step 2: Commit**

```bash
git add LiveDraftTool/server.R
git commit -m "feat: add Research tab table rendering and status display"
```

---

### Task 4: Add targeting support for Research tab

**Files:**
- Modify: `LiveDraftTool/server.R` (add after the research render outputs)

- [ ] **Step 1: Add the target toggle observeEvent**

Insert after the research render outputs, following the exact same pattern as the Prospects tab targeting (server.R lines 287-316):

```r
  # --- Target toggle (Research tab — one button, checks active subtab) ---
  observeEvent(input$targetResBtn, {
    tab <- input$researchTab
    if (!is.null(tab) && tab == "Pitchers") {
      sel <- input$researchP_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        showNotification("Select a player row first", type = "warning")
        return()
      }
      data <- rv$researchP
      pid <- as.character(data$playerid[sel])
      pName <- data$Player[sel]
    } else {
      sel <- input$researchH_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        showNotification("Select a player row first", type = "warning")
        return()
      }
      data <- rv$researchH
      pid <- as.character(data$playerid[sel])
      pName <- data$Player[sel]
    }
    if (pid %in% rv$targets) {
      rv$targets <- rv$targets[rv$targets != pid]
      showNotification(paste0("Removed target: ", pName), type = "message")
    } else {
      rv$targets <- c(rv$targets, pid)
      showNotification(paste0("Added target: ", pName), type = "message")
    }
    write.csv(data.frame(playerid = rv$targets, stringsAsFactors = FALSE), targetFile, row.names = FALSE)
  })
```

- [ ] **Step 2: Commit**

```bash
git add LiveDraftTool/server.R
git commit -m "feat: add targeting support for Research tab"
```

---

### Task 5: Manual integration test

- [ ] **Step 1: Start the app**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL/LiveDraftTool
Rscript -e "shiny::runApp('.', launch.browser=TRUE, port=7654)"
```

- [ ] **Step 2: Test happy path**

1. Navigate to the Research tab
2. Paste a FanGraphs article URL (e.g., a sleeper picks article)
3. Click "Analyze Article"
4. Verify: button disables, notifications show progress
5. Verify: hitters and pitchers appear in their respective sub-tabs
6. Verify: Tags and Summary columns are populated
7. Verify: fuzzy-matched players show "~ " prefix
8. Verify: unmatched players listed below the tables
9. Select a row, click "Toggle Target", verify notification
10. Switch to "My Targets" tab, verify the player appears

- [ ] **Step 3: Test error cases**

1. Empty URL — should show "Please enter a valid URL"
2. Invalid URL (e.g., "not-a-url") — should show error notification
3. URL with no baseball content — should show "No players found"

- [ ] **Step 4: Commit any fixes**

```bash
git add LiveDraftTool/
git commit -m "fix: address issues found during Research tab integration testing"
```
