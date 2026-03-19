# Create Protection Lists Tab — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a "Create Protection Lists" tab to the ProtectionTrades Shiny app that lets users select up to 12 players per team and save them to `{cyear}ProtectionLists.csv`.

**Architecture:** New tab in existing navbarPage UI with sidebar (team selector, counter, submit) and main panel (DT with row selection). Server logic handles pre-selection from existing CSV, validation, and file I/O. One small upstream change: keep `playerid` in `rpreds` so we can join back to roster data for output. (Note: spec said "No Changes To protectionList.r" but `playerid` is dropped at line 246 and is needed for pre-selection matching — this one-line change is unavoidable.)

**Tech Stack:** R, Shiny, DT, shinyjs, dplyr

**Spec:** `docs/superpowers/specs/2026-03-19-create-protection-lists-tab-design.md`

---

### Task 1: Retain `playerid` in `rpreds`

**Files:**
- Modify: `code/protectionList.r:246`

Currently line 246 drops `playerid`:
```r
rpreds <- select(rpreds,-playerid)
```

The HTML Player link already embeds `playerid`, so the column was considered redundant. But we need it for joining to roster data on submit. Keeping it is harmless — existing DT renders won't show it unless explicitly selected.

- [ ] **Step 1: Comment out the playerid removal**

In `code/protectionList.r`, change line 246 from:
```r
rpreds <- select(rpreds,-playerid)
```
to:
```r
# Keep playerid for protection list CSV joins
# rpreds <- select(rpreds,-playerid)
```

- [ ] **Step 2: Verify protectionList.r still runs**

Run in R console from the `code/` directory:
```r
source("protectionList.r")
"playerid" %in% names(rpreds)  # Should be TRUE
```
Expected: TRUE, no errors.

- [ ] **Step 3: Verify existing tabs still work**

Launch the app and confirm Overview, by Team, Best Pitchers, Best Hitters tabs all render correctly. The `playerid` column won't appear in existing tables because: `pullPlayers()` uses `Player:Expected.Return` range which starts after `playerid` in column order; `bh()`/`bp()` use `Pos:netValue` which also excludes it. Verify visually that no `playerid` column shows in any existing tab.

- [ ] **Step 4: Commit**

```bash
git add code/protectionList.r
git commit -m "Keep playerid in rpreds for protection list CSV joins"
```

---

### Task 2: Add `shinyjs` and the new tab UI

**Files:**
- Modify: `ProtectionTrades/ui.R`

- [ ] **Step 1: Add shinyjs library and useShinyjs()**

At the top of `ui.R`, add `library("shinyjs")` after the existing library calls. Add `shinyjs::useShinyjs()` as the first element inside the `navbarPage()`.

The updated file top should look like:
```r
library("bslib")
library("DT")
library("shinyjs")


shinyUI(navbarPage("Offseason Trade Evaluator, v2.0",
        theme = bs_theme(bootswatch = "flatly"),
        shinyjs::useShinyjs(),
```

- [ ] **Step 2: Add the Create Protection Lists tabPanel**

Insert a new `tabPanel` after the "Best Hitters" tab (before the closing `)` of `navbarPage`):

```r
        tabPanel("Create Protection Lists",
                 sidebarLayout(
                   sidebarPanel(
                     selectizeInput('protTeam', 'Select Team', choices = NULL),
                     h4(textOutput("protCounter")),
                     actionButton("protSubmit", "Submit Protection List",
                                  icon = icon("save"),
                                  class = "btn-success",
                                  style = "margin-top: 15px;"),
                     width = 3
                   ),
                   mainPanel(
                     DT::dataTableOutput("protTable"),
                     width = 9
                   )
                 )
        )
```

- [ ] **Step 3: Verify the app launches without errors**

Run the app. The new tab should appear. The table and dropdown will be empty until server logic is added. No errors on launch.

- [ ] **Step 4: Commit**

```bash
git add ProtectionTrades/ui.R
git commit -m "Add Create Protection Lists tab UI with shinyjs"
```

---

### Task 3: Server logic — team data display and pre-selection

**Files:**
- Modify: `ProtectionTrades/server.R:202-312` (inside `shinyServer`)

- [ ] **Step 1: Initialize the team dropdown**

After the existing `updateSelectizeInput` for `e1` (line 270), add:

```r
  updateSelectizeInput(session, 'protTeam', choices = teams, selected = 'Liquor Crickets')
```

- [ ] **Step 2: Add reactive for protection table data**

Add this block inside `shinyServer`, after the `updateSelectizeInput` calls:

```r
  # --- Create Protection Lists tab ---

  # Build the protection list file path
  protFilePath <- paste0("../", cyear, "ProtectionLists.csv")

  # Reactive: players for selected team (for DT display)
  protPlayers <- reactive({
    req(input$protTeam)
    rv$rpreds %>%
      filter(Team == input$protTeam) %>%
      mutate(PlayerName = gsub("<[^>]+>", "", Player)) %>%
      select(playerid, PlayerName, Pos, Age, Salary, Contract, pDFL, netValue) %>%
      arrange(-netValue)
  })

  # Reactive: pre-selected row indices from existing CSV
  protPreSelected <- reactive({
    req(input$protTeam)
    if (!file.exists(protFilePath)) return(integer(0))
    existing <- read.csv(protFilePath, stringsAsFactors = FALSE)
    teamExisting <- existing %>% filter(Team == input$protTeam)
    if (nrow(teamExisting) == 0) return(integer(0))
    players <- protPlayers()
    which(players$playerid %in% teamExisting$playerid)
  })
```

- [ ] **Step 3: Render the DataTable with row selection**

```r
  output$protTable <- DT::renderDataTable({
    players <- protPlayers()
    displayDf <- players %>% select(-playerid)
    datatable(displayDf,
              selection = list(mode = 'multiple', selected = protPreSelected()),
              options = list(pageLength = 30, paging = FALSE, searching = FALSE, info = FALSE)) %>%
      formatCurrency(c('pDFL', 'netValue')) %>%
      formatRound('Age', 0)
  })
```

- [ ] **Step 4: Verify team selection loads players with pre-selection**

Launch the app, go to "Create Protection Lists" tab. Select a team. Players should appear in the table. If the team has entries in the existing `{cyear}ProtectionLists.csv`, those rows should be pre-highlighted.

- [ ] **Step 5: Commit**

```bash
git add ProtectionTrades/server.R
git commit -m "Add protection list table with team filter and pre-selection"
```

---

### Task 4: Server logic — counter, validation, and submit

**Files:**
- Modify: `ProtectionTrades/server.R`

- [ ] **Step 1: Add the selection counter output**

```r
  output$protCounter <- renderText({
    sel <- input$protTable_rows_selected
    n <- length(sel)
    paste0(n, " / 12 selected")
  })
```

- [ ] **Step 2: Add shinyjs disable/enable logic for submit button**

```r
  observe({
    sel <- input$protTable_rows_selected
    if (length(sel) > 12 || length(sel) == 0) {
      shinyjs::disable("protSubmit")
    } else {
      shinyjs::enable("protSubmit")
    }
  })
```

- [ ] **Step 3: Add the submit handler**

```r
  observeEvent(input$protSubmit, {
    sel <- input$protTable_rows_selected
    if (length(sel) > 12) {
      showNotification("Cannot protect more than 12 players.", type = "error")
      return()
    }
    if (length(sel) == 0) {
      showNotification("No players selected.", type = "warning")
      return()
    }

    # Get selected playerids
    players <- protPlayers()
    selectedIds <- players$playerid[sel]

    # Read full roster to get output columns matching Rosters.csv schema
    rosterFile <- paste0("../", cyear, "Rosters.csv")
    rosters <- read.csv(rosterFile, stringsAsFactors = FALSE)
    selectedRoster <- rosters %>% filter(playerid %in% selectedIds)

    # Safety check: ensure join actually matched
    if (nrow(selectedRoster) == 0) {
      showNotification("Error: could not match selected players to roster file.", type = "error")
      return()
    }

    # Read existing protection list (or create empty)
    if (file.exists(protFilePath)) {
      existingProt <- read.csv(protFilePath, stringsAsFactors = FALSE)
      # Remove this team's old entries
      existingProt <- existingProt %>% filter(Team != input$protTeam)
    } else {
      existingProt <- rosters[0, ]  # Empty df with same columns
    }

    # Append new selections and write
    updatedProt <- rbind(existingProt, selectedRoster)
    write.csv(updatedProt, protFilePath)

    showNotification(
      paste0("Saved ", length(sel), " players for ", input$protTeam, "!"),
      type = "message", duration = 4
    )
  })
```

- [ ] **Step 4: Test the full flow**

1. Launch app → Create Protection Lists tab
2. Select a team → players load, pre-selections highlight
3. Select >12 rows → Submit button disables
4. Select 1-12 rows → Submit button enables
5. Click Submit → notification appears, check `{cyear}ProtectionLists.csv` has the correct rows
6. Switch to a different team, submit there → verify first team's rows are preserved
7. Re-select the first team → verify pre-selection matches what was just saved

- [ ] **Step 5: Commit**

```bash
git add ProtectionTrades/server.R
git commit -m "Add protection list submit with validation and file I/O"
```
