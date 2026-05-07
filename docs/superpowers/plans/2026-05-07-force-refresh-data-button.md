# Force Refresh Data Button + Move to Settings Modal — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the LeagueEval "Refresh Data" button force refetch of all four upstream data sources (bypassing the staleness checks) and relocate it from the top nav into the Settings modal.

**Architecture:** Use a `DAFL_FORCE_REFRESH` environment variable as the cross-source signal. The Shiny refresh handler sets it before sourcing `inSeasonPulse.r` and clears it via `on.exit`. Inside `inSeasonPulse.r`, four staleness guards consult `forceRefresh` and skip the cache when the flag is set. UI moves the button into the Settings `modalDialog`; the handler closes the modal on click.

**Tech Stack:** R, Shiny, bslib, shinyjs. No new dependencies.

**Spec:** `docs/superpowers/specs/2026-05-07-force-refresh-data-button-design.md`

---

## File Structure

| File | Role | Change |
| --- | --- | --- |
| `code/inSeasonPulse.r` | Pipeline that fetches and assembles all data | Add `forceRefresh` flag + 4 guard updates |
| `LeagueEval/server.R` | Shiny server: settings modal + refresh handler | Add Refresh button to modal; update handler with env var + `on.exit` + `removeModal` |
| `LeagueEval/ui.R` | Shiny UI: top nav header | Remove `refreshBtn` from header |

No new files. No tests (this is a Shiny app with no automated test suite — verification is manual via the running app).

---

## Task 1: Add `forceRefresh` flag and update staleness guards in `inSeasonPulse.r`

**Files:**
- Modify: `code/inSeasonPulse.r:47-68` and `code/inSeasonPulse.r:296`, `code/inSeasonPulse.r:312`

This task adds the env-var read once and threads it through all four guards. Doing all four in one commit keeps the flag's definition and uses together — easier to review than four micro-commits.

- [ ] **Step 1: Add the `forceRefresh` definition**

In `code/inSeasonPulse.r`, immediately after the `projFiles <- list(...)` block (currently ends at line 46) and before the `# Update data files` comment, insert:

```r

# When set (e.g. by Shiny's Refresh Data button), bypass file-age cache
# checks below and refetch from every upstream source.
forceRefresh <- nchar(Sys.getenv("DAFL_FORCE_REFRESH")) > 0

```

- [ ] **Step 2: Update the projection + salary guard**

Find this block (currently at line 58):

```r
if (projMissing || any(projAges > 10)) {
  # Use Playwright-based fetch for FanGraphs (bypasses Cloudflare)
  system(str_c("node ../scripts/fgFetchInSeason.js ", cyear))
  system("bash ../scripts/salaryinfo.sh")
}
```

Replace with:

```r
if (forceRefresh || projMissing || any(projAges > 10)) {
  # Use Playwright-based fetch for FanGraphs (bypasses Cloudflare)
  system(str_c("node ../scripts/fgFetchInSeason.js ", cyear))
  system("bash ../scripts/salaryinfo.sh")
}
```

- [ ] **Step 3: Update the CBS guard**

Find this block (currently at line 66):

```r
if (is.na(cbsAge) || cbsAge > 10) {
  system("node ../scripts/cbsFetch.js")
}
```

Replace with:

```r
if (forceRefresh || is.na(cbsAge) || cbsAge > 10) {
  system("node ../scripts/cbsFetch.js")
}
```

- [ ] **Step 4: Update the injuries guard**

Find this block (currently at line 296):

```r
if (injAge < 20) {
  cat("Using cached injuries file (", injAge, " hours old)\n")
  injOrig <- read.csv("../latestInjuries.csv", stringsAsFactors = FALSE)
  injOrig <- injOrig %>% rename(`Latest Update` = `Latest.Update`, `Injury / Surgery Date` = `Injury...Surgery.Date`)
} else {
  cat("Injuries file is", injAge, "hours old, fetching fresh data...\n")
  tryCatch({
    injOrig <- getInjuriesAPI()
  }, error = function(e) {
    cat("Error fetching injuries:", e$message, "\n")
    cat("Falling back to cached injuries file\n")
    injOrig <<- read.csv("../latestInjuries.csv", stringsAsFactors = FALSE)
    injOrig <<- injOrig %>% rename(`Latest Update` = `Latest.Update`, `Injury / Surgery Date` = `Injury...Surgery.Date`)
  })
}
```

Replace the `if (injAge < 20)` line with:

```r
if (!forceRefresh && injAge < 20) {
```

Leave the rest of the block unchanged — including the fallback `tryCatch` that re-reads the cached CSV if the API call fails. (Force-refresh means "don't *skip* the fetch", not "fail loudly if the fetch breaks".)

- [ ] **Step 5: Update the Stuff+ guard**

Find this block (currently at line 312):

```r
if (stuffAge < 20) {
  cat("Using cached Stuff+ file (", stuffAge, " hours old)\n")
  stuff <- read.csv("../latestStuff.csv", stringsAsFactors = FALSE) %>%
    rename(`Pitching+` = `Pitching.`)
} else {
```

Replace the `if (stuffAge < 20)` line with:

```r
if (!forceRefresh && stuffAge < 20) {
```

Leave the rest of the block unchanged.

- [ ] **Step 6: Smoke-check that the file still parses**

Run from the repo root:

```bash
Rscript -e 'parse(file = "code/inSeasonPulse.r"); cat("OK\n")'
```

Expected output: `OK` (the file parses successfully).

If R is not on PATH on this Windows machine and the command errors with "Rscript not found", skip this check — the next manual run of the app will surface any syntax error. Do not invent a different verification.

- [ ] **Step 7: Commit**

```bash
git add code/inSeasonPulse.r
git commit -m "feat: add DAFL_FORCE_REFRESH flag to bypass staleness checks

When set, forces refetch of FanGraphs projections, salary info,
CBS endpoints, injuries, and Stuff+ regardless of cached file age."
```

---

## Task 2: Update Shiny refresh handler — env var, on.exit, removeModal

**Files:**
- Modify: `LeagueEval/server.R:128-150`

- [ ] **Step 1: Replace the refresh handler**

Find this block in `LeagueEval/server.R` (currently lines 128-150):

```r
# --- Refresh Data ---
  observeEvent(input$refreshBtn, {
    showNotification("Refreshing data... this may take a minute", type = "message", duration = NULL, id = "refreshMsg")
    tryCatch({
      source("../code/inSeasonPulse.r", local = globalenv())
      teams <<- sort(unique(RTot$Team))
      updateSelectizeInput(session, 'e1', choices = teams, selected = 'Liquor Crickets')
      updateSelectizeInput(session, 'teamSelect', choices = teams, selected = 'Liquor Crickets')
      updateSelectizeInput(session, 'tradeTeamA',
                           choices = c('Pick a team' = '', teams),
                           selected = '')
      updateSelectizeInput(session, 'tradeTeamB',
                           choices = c('Pick a team' = '', teams),
                           selected = '')
      updateSelectizeInput(session, 'choice', choices = trending$Player, server = TRUE)
      rv$refreshCount <- rv$refreshCount + 1
      removeNotification("refreshMsg")
      showNotification("Data refreshed!", type = "message")
    }, error = function(e) {
      removeNotification("refreshMsg")
      showNotification(paste0("Refresh failed: ", e$message), type = "error", duration = 10)
    })
  })
```

Replace with:

```r
# --- Refresh Data ---
  observeEvent(input$refreshBtn, {
    removeModal()
    showNotification("Refreshing data... this may take a minute", type = "message", duration = NULL, id = "refreshMsg")
    Sys.setenv(DAFL_FORCE_REFRESH = "1")
    on.exit(Sys.unsetenv("DAFL_FORCE_REFRESH"), add = TRUE)
    tryCatch({
      source("../code/inSeasonPulse.r", local = globalenv())
      teams <<- sort(unique(RTot$Team))
      updateSelectizeInput(session, 'e1', choices = teams, selected = 'Liquor Crickets')
      updateSelectizeInput(session, 'teamSelect', choices = teams, selected = 'Liquor Crickets')
      updateSelectizeInput(session, 'tradeTeamA',
                           choices = c('Pick a team' = '', teams),
                           selected = '')
      updateSelectizeInput(session, 'tradeTeamB',
                           choices = c('Pick a team' = '', teams),
                           selected = '')
      updateSelectizeInput(session, 'choice', choices = trending$Player, server = TRUE)
      rv$refreshCount <- rv$refreshCount + 1
      removeNotification("refreshMsg")
      showNotification("Data refreshed!", type = "message")
    }, error = function(e) {
      removeNotification("refreshMsg")
      showNotification(paste0("Refresh failed: ", e$message), type = "error", duration = 10)
    })
  })
```

The four added/changed lines:
- `removeModal()` — closes the Settings modal immediately on click.
- `Sys.setenv(DAFL_FORCE_REFRESH = "1")` — signal force-refresh to `inSeasonPulse.r`.
- `on.exit(Sys.unsetenv("DAFL_FORCE_REFRESH"), add = TRUE)` — clear the flag when the observer body exits, even if `source()` errors. `add = TRUE` is defensive in case Shiny's wrapper has registered other on.exit handlers.

- [ ] **Step 2: Commit**

```bash
git add LeagueEval/server.R
git commit -m "feat(LeagueEval): force refresh on Refresh Data click

Sets DAFL_FORCE_REFRESH before sourcing inSeasonPulse.r so all four
staleness checks are bypassed. Closes Settings modal immediately on
click. Clears the env var via on.exit so command-line sourcing of
inSeasonPulse.r continues to honor staleness checks."
```

---

## Task 3: Add Refresh Data button to Settings modal

**Files:**
- Modify: `LeagueEval/server.R:45-58`

- [ ] **Step 1: Update the Settings modal definition**

Find this block in `LeagueEval/server.R` (currently lines 45-58):

```r
  # --- Settings modal: projection source ---
  observeEvent(input$settingsBtn, {
    showModal(modalDialog(
      title = "Settings",
      size = "s",
      easyClose = TRUE,
      radioButtons('projSource', 'Projection System',
                   choices = c('ATC' = 'atc',
                               'Steamer' = 'steamer',
                               'THE BAT X' = 'batx'),
                   selected = isolate(projSource()),
                   inline = TRUE),
      footer = modalButton("Close")
    ))
  })
```

Replace with:

```r
  # --- Settings modal: projection source + Refresh Data ---
  observeEvent(input$settingsBtn, {
    showModal(modalDialog(
      title = "Settings",
      size = "s",
      easyClose = TRUE,
      radioButtons('projSource', 'Projection System',
                   choices = c('ATC' = 'atc',
                               'Steamer' = 'steamer',
                               'THE BAT X' = 'batx'),
                   selected = isolate(projSource()),
                   inline = TRUE),
      tags$hr(),
      actionButton('refreshBtn', 'Refresh Data',
                   class = 'btn-success btn-sm',
                   icon = icon('refresh')),
      footer = modalButton("Close")
    ))
  })
```

The two added lines (`tags$hr()` and the `actionButton`) sit between the radios and the `footer`. The button id stays `refreshBtn` so the existing observer (modified in Task 2) fires unchanged.

- [ ] **Step 2: Commit**

```bash
git add LeagueEval/server.R
git commit -m "feat(LeagueEval): add Refresh Data button to Settings modal"
```

---

## Task 4: Remove Refresh Data button from top nav

**Files:**
- Modify: `LeagueEval/ui.R:16-25`

- [ ] **Step 1: Replace the header block**

Find this block in `LeagueEval/ui.R` (currently lines 16-25):

```r
    header = tagList(
      shinyjs::useShinyjs(),
      tags$div(style = "position:absolute; right:15px; top:8px; z-index:1000; display:flex; gap:8px;",
        actionButton('settingsBtn', 'Settings',
                     class = 'btn-default btn-sm'),
        actionButton('refreshBtn', 'Refresh Data',
                      class = 'btn-success btn-sm',
                      icon = icon('refresh'))
      )
    ),
```

Replace with:

```r
    header = tagList(
      shinyjs::useShinyjs(),
      tags$div(style = "position:absolute; right:15px; top:8px; z-index:1000;",
        actionButton('settingsBtn', 'Settings',
                     class = 'btn-default btn-sm')
      )
    ),
```

Changes:
- `refreshBtn` actionButton removed from the header.
- The flex layout (`display:flex; gap:8px;`) is no longer needed since only one button remains. The `tags$div` wrapper is kept so the `position:absolute` right-anchored placement is preserved.

- [ ] **Step 2: Commit**

```bash
git add LeagueEval/ui.R
git commit -m "feat(LeagueEval): remove Refresh Data from top nav (now in Settings)"
```

---

## Task 5: Manual verification (run the app)

This is a Shiny app — there is no automated test suite. Verify manually.

- [ ] **Step 1: Start the app**

In RStudio, open `LeagueEval/ui.R` or `LeagueEval/server.R` and click **Run App**, or from a terminal at the repo root:

```bash
Rscript -e 'shiny::runApp("LeagueEval", launch.browser = TRUE)'
```

Wait until the browser opens and the app's tabs are visible.

- [ ] **Step 2: Verify the top nav**

In the running app:
- Top-right corner shows a single grey **Settings** button.
- There is **no** green **Refresh Data** button in the top nav.

If a Refresh Data button still shows in the top nav, Task 4 was not applied — re-check `LeagueEval/ui.R`.

- [ ] **Step 3: Verify the Settings modal layout**

Click **Settings** in the top right. The modal opens and shows, top-to-bottom:
1. Title: "Settings"
2. "Projection System" radios: ATC / Steamer / THE BAT X
3. A horizontal rule
4. A green **Refresh Data** button with a refresh icon
5. Footer: a "Close" button

If the Refresh button does not appear inside the modal, Task 3 was not applied — re-check `LeagueEval/server.R` Settings modal block.

- [ ] **Step 4: Verify modal close + refresh notification**

Click **Refresh Data** inside the modal:
- The modal closes immediately.
- A blue/grey toast notification appears at the bottom right: **"Refreshing data... this may take a minute"**.

If the modal stays open, the `removeModal()` line in Task 2 was not applied.

- [ ] **Step 5: Verify a true refetch happened**

Watch the R console (RStudio's Console pane, or the terminal running `runApp`) while the refresh runs. **You should NOT see** these lines:

```
Using cached injuries file ( N hours old)
Using cached Stuff+ file ( N hours old)
```

You **should** see fetch activity — typically `Injuries file is N hours old, fetching fresh data...` and the fgFetchInSeason / cbsFetch scripts running (they print their own progress to stdout).

If you see "Using cached" messages, `forceRefresh` is not being honored — re-check Task 1's guard updates and Task 2's `Sys.setenv` line.

- [ ] **Step 6: Verify completion**

When the pipeline finishes:
- The "Refreshing data..." notification disappears.
- A green **"Data refreshed!"** notification appears.
- Tables across tabs (Standings, By Team, etc.) reflect the new data.

- [ ] **Step 7: Verify command-line sourcing still respects staleness**

This confirms the env var doesn't leak. From the repo root in a *fresh* R session (not the Shiny app's session):

```bash
Rscript -e 'setwd("code"); source("inSeasonPulse.r")'
```

Watch the output. If the cached files are <20 hours old, you should see lines like `Using cached injuries file ( N hours old)` — i.e., the staleness checks are still active when no env var is set.

If you instead see fresh fetches running unconditionally, the `forceRefresh` flag is somehow being read as TRUE from a clean shell — double-check the `nchar(Sys.getenv(...)) > 0` definition in Task 1 Step 1 (an empty env var should yield `nchar == 0`).

- [ ] **Step 8: No commit**

This task only verifies; it does not change code. If issues were found and fixed, commit those fixes inside the relevant Task 1–4 commit boundaries (or as a fixup commit referencing the originating task).

---

## Self-Review Notes

- Spec coverage: each spec section has a corresponding task — Task 1 covers the env-var mechanism + four guard updates; Task 2 covers the handler changes (env var, on.exit, removeModal, error handling preserved); Task 3 covers Settings modal addition; Task 4 covers UI relocation; Task 5 covers the test plan from the spec.
- No placeholders. Every code block is the literal final code.
- The button id `refreshBtn` is consistent across Tasks 2, 3, and 4 (Task 4 removes it from one place; Task 3 adds it to another; Task 2 wires its handler).
- `DAFL_FORCE_REFRESH` is the env-var name used identically in Task 1 (read) and Task 2 (set + clear).
