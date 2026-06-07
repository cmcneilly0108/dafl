# Guardians Clickable Player Names Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make every player name across all tabs of the Guardians Tracker Shiny app a clickable link that opens the Player Detail tab with that player loaded.

**Architecture:** Add an `id` to the `navbarPage` so the server can switch tabs. Each linked name fires `Shiny.setInputValue('gPlayerClick', '<name>', {priority:'event'})` via an inline `onclick`; one server observer sets the Player Detail search picker and switches tabs. A shared `playerLink()` helper renders a name as an anchor only when it exists in `gRoster$player`, else as plain escaped text.

**Tech Stack:** R, Shiny, DT, htmltools. No unit-test harness exists for these reactive outputs — verification is manual against the running app (Playwright via the webapp-testing skill), per the spec.

**Spec:** `docs/superpowers/specs/2026-06-07-guardians-clickable-player-names-design.md`

---

## File Structure

- `Guardians/ui.R` — add `id = "gNav"` to `navbarPage`.
- `Guardians/server.R` — add `jsStr()` + `playerLink()` helpers and the
  `gPlayerClick` observer; wrap name columns in every table; make SVG diamond
  text, SP/RP columns, and the Bench line clickable.

No new files. All changes are localized to the two app files.

---

### Task 1: Navbar id, shared helpers, and click observer

This is the foundation every later task depends on. Nothing is clickable yet
after this task, but the plumbing is in place.

**Files:**
- Modify: `Guardians/ui.R` (the `navbarPage(...)` call)
- Modify: `Guardians/server.R` (top of `shinyServer` function body)

- [ ] **Step 1: Give the navbar an id**

In `Guardians/ui.R`, the `navbarPage` currently starts:

```r
  navbarPage(
    theme = bs_theme(bootswatch = "flatly"),
    "Cleveland Guardians Tracker",
```

Change it to add an `id`:

```r
  navbarPage(
    id = "gNav",
    theme = bs_theme(bootswatch = "flatly"),
    "Cleveland Guardians Tracker",
```

- [ ] **Step 2: Add the `jsStr()` and `playerLink()` helpers**

In `Guardians/server.R`, immediately after the `rv <- reactiveValues(refreshCount = 0)`
line (currently line 13) and before the `seasonLineHtml` helper, insert:

```r
  # Escape a string for safe embedding inside a single-quoted JS string in an
  # inline onclick attribute (e.g. names like O'Brien). Backslash first.
  jsStr <- function(s) {
    s <- gsub("\\\\", "\\\\\\\\", s)
    s <- gsub("'", "\\\\'", s)
    gsub("[\r\n]+", " ", s)
  }

  # Render a player name as a link that opens the Player Detail tab with that
  # player loaded. Returns an <a> only when the name exists in the current
  # roster; otherwise the HTML-escaped `display` text (no dead links). Vectorized
  # so it can map a whole DT column. `display` lets callers show a suffix while
  # linking on the bare name.
  playerLink <- function(name, display = name) {
    vapply(seq_along(name), function(i) {
      nm <- name[i]; dp <- display[i]
      esc <- htmltools::htmlEscape(dp, attribute = FALSE)
      if (is.na(nm) || !(nm %in% gRoster$player)) return(esc)
      sprintf('<a href="#" style="cursor:pointer;" onclick="Shiny.setInputValue(\'gPlayerClick\', \'%s\', {priority:\'event\'}); return false;">%s</a>',
              jsStr(nm), esc)
    }, character(1))
  }
```

- [ ] **Step 3: Add the click observer**

In `Guardians/server.R`, add this observer just after the `playerLink` helper
(it can live anywhere in the server body, but keeping it near the helper is
clearest):

```r
  # Any clicked player name lands here: set the Player Detail picker and switch
  # to that tab. gPlayerPick is a server-side selectize; selected = sets it.
  observeEvent(input$gPlayerClick, {
    updateSelectizeInput(session, 'gPlayerPick', selected = input$gPlayerClick)
    updateNavbarPage(session, 'gNav', selected = "Player Detail")
  })
```

- [ ] **Step 4: Smoke-check that the app still loads**

Run:

```bash
cd Guardians && Rscript -e 'parse("ui.R"); parse("server.R"); cat("parse OK\n")'
```

Expected: `parse OK` (no syntax errors). This only checks the files parse; full
behavior is verified in Task 6.

- [ ] **Step 5: Commit**

```bash
git add Guardians/ui.R Guardians/server.R
git commit -m "feat(guardians): navbar id, playerLink helper, click observer

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 2: Link names in the escape=FALSE DT tables

Hot/Cold, the Depth Chart roster table, and both Prospects tables already render
with `escape = FALSE`, so wrapping the name column is all that's needed.

**Files:**
- Modify: `Guardians/server.R` (`gHotTable`, `gDepthDiamond` roster df,
  `gProspectsH`, `gProspectsP`)

- [ ] **Step 1: Hot/Cold table**

In `output$gHotTable`, the pipeline ends with a `select(Player = player, ...)`
producing `df`, then `datatable(df, ...)`. Immediately before that `datatable(`
call, add:

```r
    df$Player <- playerLink(df$Player)
```

- [ ] **Step 2: Depth Chart roster table**

In `output$gDepthDiamond`, after the `df <- df[order(df$Pos, df$Player), ]` line
and before `statTable <- DT::datatable(df, ...)`, add:

```r
    df$Player <- playerLink(df$Player)
```

- [ ] **Step 3: Prospects Hitters table**

In `output$gProspectsH`, after the `df <- df %>% select(... Player = Name, ...)`
block and before the `datatable(df, ...)` call, add:

```r
    df$Player <- playerLink(df$Player)
```

- [ ] **Step 4: Prospects Pitchers table**

In `output$gProspectsP`, after its `select(... Player = Name, ...)` block and
before the `datatable(df, ...)` call, add:

```r
    df$Player <- playerLink(df$Player)
```

- [ ] **Step 5: Parse check**

```bash
cd Guardians && Rscript -e 'parse("server.R"); cat("parse OK\n")'
```

Expected: `parse OK`.

- [ ] **Step 6: Commit**

```bash
git add Guardians/server.R
git commit -m "feat(guardians): link player names in Hot/Cold, Depth roster, Prospects

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 3: Link names in Risers / Transactions / IL (switch to escape=FALSE)

These three tables currently render with the default `escape = TRUE`. Switch each
to `escape = FALSE`, wrap the name column with `playerLink()`, and HTML-escape
their free-text columns so only intended HTML renders.

**Files:**
- Modify: `Guardians/server.R` (`gRisers`, `gTxnTable`, `gILTable`)

- [ ] **Step 1: Risers table**

In `output$gRisers`, the success path builds `out` and ends with:

```r
    datatable(out, options = list(pageLength = 15, dom = 'tip', autoWidth = FALSE),
              rownames = FALSE)
```

Replace that final `datatable(...)` call with:

```r
    out$Reason <- htmltools::htmlEscape(out$Reason)
    out$Player <- playerLink(out$Player)
    datatable(out, options = list(pageLength = 15, dom = 'tip', autoWidth = FALSE),
              rownames = FALSE, escape = FALSE)
```

(Leave the early-return "No risers right now." datatable untouched.)

- [ ] **Step 2: Transactions table**

In `output$gTxnTable`, the success path is:

```r
    df <- gTxn %>%
      select(Date = txn_date, Player = player, Type = type,
             From = from_team_id, To = to_team_id, Description = description)
    datatable(df,
              options = list(pageLength = 25, filter = 'top', autoWidth = FALSE),
              filter = 'top', rownames = FALSE)
```

Replace it with:

```r
    df <- gTxn %>%
      select(Date = txn_date, Player = player, Type = type,
             From = from_team_id, To = to_team_id, Description = description)
    df$Description <- htmltools::htmlEscape(df$Description)
    df$Player <- playerLink(df$Player)
    datatable(df,
              options = list(pageLength = 25, filter = 'top', autoWidth = FALSE),
              filter = 'top', rownames = FALSE, escape = FALSE)
```

- [ ] **Step 3: IL table**

In `output$gILTable`, the success path is:

```r
    df <- gIL %>%
      mutate(Age = ifelse(is.na(age), NA_real_, round(age, 0))) %>%
      select(Player = player, Pos = pos, Age, Status = status,
             Injury = injury, `Latest Update` = update) %>%
      arrange(Player)
    datatable(df,
              options = list(pageLength = 25, autoWidth = FALSE,
                             filter = 'top'),
              filter = 'top', rownames = FALSE)
```

Replace it with:

```r
    df <- gIL %>%
      mutate(Age = ifelse(is.na(age), NA_real_, round(age, 0))) %>%
      select(Player = player, Pos = pos, Age, Status = status,
             Injury = injury, `Latest Update` = update) %>%
      arrange(Player)
    df$Injury <- htmltools::htmlEscape(df$Injury)
    df$`Latest Update` <- htmltools::htmlEscape(df$`Latest Update`)
    df$Player <- playerLink(df$Player)
    datatable(df,
              options = list(pageLength = 25, autoWidth = FALSE,
                             filter = 'top'),
              filter = 'top', rownames = FALSE, escape = FALSE)
```

- [ ] **Step 4: Parse check**

```bash
cd Guardians && Rscript -e 'parse("server.R"); cat("parse OK\n")'
```

Expected: `parse OK`.

- [ ] **Step 5: Commit**

```bash
git add Guardians/server.R
git commit -m "feat(guardians): link player names in Risers, Transactions, IL

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 4: Make Depth Chart SVG diamond names clickable

The diamond renders player names as SVG `<text>` nodes inside the nested
`playersAt()` function. Add `cursor:pointer` styling and the inline `onclick`
when the name resolves to a roster row.

**Files:**
- Modify: `Guardians/server.R` (`playersAt()` inside `gDepthDiamond`)

- [ ] **Step 1: Update the name-rendering loop**

In `output$gDepthDiamond`, inside `playersAt()`, the current `names <- ...` block
is:

```r
      names <- paste(vapply(seq_along(pl), function(j) {
        sprintf('<text x="%d" y="%d" text-anchor="middle" fill="#ffffff" font-size="12" stroke="#0F223E" stroke-width="0.3" paint-order="stroke">%s</text>',
                x, y + (j - 1) * step, esc(pl[j]))
      }, character(1)), collapse = "")
```

Replace it with:

```r
      names <- paste(vapply(seq_along(pl), function(j) {
        nm <- pl[j]
        clickable <- nm %in% gRoster$player
        onclick <- if (clickable) sprintf(' style="cursor:pointer;" onclick="Shiny.setInputValue(\'gPlayerClick\', \'%s\', {priority:\'event\'}); return false;"', jsStr(nm)) else ""
        sprintf('<text x="%d" y="%d" text-anchor="middle" fill="#ffffff" font-size="12" stroke="#0F223E" stroke-width="0.3" paint-order="stroke"%s>%s</text>',
                x, y + (j - 1) * step, onclick, esc(nm))
      }, character(1)), collapse = "")
```

- [ ] **Step 2: Parse check**

```bash
cd Guardians && Rscript -e 'parse("server.R"); cat("parse OK\n")'
```

Expected: `parse OK`.

- [ ] **Step 3: Commit**

```bash
git add Guardians/server.R
git commit -m "feat(guardians): clickable player names in depth chart diamond

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 5: Link SP/RP columns and the Bench line

These render player names as plain text with a suffix baked in
(`"Name (4.50 ERA)"`, `"Name (1B)"`). Restructure the grouping loop to keep the
bare name and its suffix separate so the name can be linked and the suffix
appended as plain text.

**Files:**
- Modify: `Guardians/server.R` (`gDepthDiamond`: grouping loop, sort loop,
  `pitcherCol()`, `sideUI`, `benchUI`)

- [ ] **Step 1: Replace the accumulator initialization**

In `output$gDepthDiamond`, the current initializers are:

```r
    pitchers    <- list(SP = character(0), RP = character(0))
    pitchersKey <- list(SP = numeric(0),   RP = numeric(0))
    bench <- character(0)
```

Replace with:

```r
    pitchersNm  <- list(SP = character(0), RP = character(0))
    pitchersSuf <- list(SP = character(0), RP = character(0))
    pitchersKey <- list(SP = numeric(0),   RP = numeric(0))
    benchNm  <- character(0)
    benchSuf <- character(0)
```

- [ ] **Step 2: Update the pitcher branch of the loop**

The current pitcher branch ends with:

```r
        pitchers[[bucket]]    <- c(pitchers[[bucket]],    paste0(nm, era))
        pitchersKey[[bucket]] <- c(pitchersKey[[bucket]], key)
```

Replace those two lines with:

```r
        pitchersNm[[bucket]]  <- c(pitchersNm[[bucket]],  nm)
        pitchersSuf[[bucket]] <- c(pitchersSuf[[bucket]], era)
        pitchersKey[[bucket]] <- c(pitchersKey[[bucket]], key)
```

- [ ] **Step 3: Update the bench branch of the loop**

The current bench branch (the final `else`) is:

```r
      } else {
        bench <- c(bench, paste0(nm, " (", p, ")"))
      }
```

Replace with:

```r
      } else {
        benchNm  <- c(benchNm,  nm)
        benchSuf <- c(benchSuf, paste0(" (", p, ")"))
      }
```

- [ ] **Step 4: Update the pitcher sort loop**

The current sort loop for pitchers is:

```r
    for (bucket in names(pitchers)) {
      ord <- order(pitchersKey[[bucket]], decreasing = TRUE)
      pitchers[[bucket]] <- pitchers[[bucket]][ord]
    }
```

Replace with:

```r
    for (bucket in names(pitchersNm)) {
      ord <- order(pitchersKey[[bucket]], decreasing = TRUE)
      pitchersNm[[bucket]]  <- pitchersNm[[bucket]][ord]
      pitchersSuf[[bucket]] <- pitchersSuf[[bucket]][ord]
    }
```

- [ ] **Step 5: Rewrite `pitcherCol()` to link names**

The current `pitcherCol` is:

```r
    pitcherCol <- function(label, names) {
      if (length(names) == 0) {
        return(tagList(
          tags$h5(label, style = "margin-bottom:4px;"),
          tags$div(style = "color:#888; font-style:italic; font-size:13px;",
                   "—")
        ))
      }
      tagList(
        tags$h5(paste0(label, " (", length(names), ")"),
                style = "margin-bottom:4px;"),
        tags$div(style = "font-size:13px;",
                 do.call(tagList,
                         lapply(names, function(x) tags$div(style="padding:2px 0;", x))))
      )
    }
```

Replace with:

```r
    pitcherCol <- function(label, nms, sufs) {
      if (length(nms) == 0) {
        return(tagList(
          tags$h5(label, style = "margin-bottom:4px;"),
          tags$div(style = "color:#888; font-style:italic; font-size:13px;",
                   "—")
        ))
      }
      tagList(
        tags$h5(paste0(label, " (", length(nms), ")"),
                style = "margin-bottom:4px;"),
        tags$div(style = "font-size:13px;",
                 do.call(tagList,
                         lapply(seq_along(nms), function(i)
                           tags$div(style = "padding:2px 0;",
                                    HTML(paste0(playerLink(nms[i]), sufs[i]))))))
      )
    }
```

- [ ] **Step 6: Update `sideUI` to pass name + suffix vectors**

The current `sideUI` is:

```r
    sideUI <- fluidRow(
      column(width = 6, pitcherCol("SP", pitchers$SP)),
      column(width = 6, pitcherCol("RP", pitchers$RP))
    )
```

Replace with:

```r
    sideUI <- fluidRow(
      column(width = 6, pitcherCol("SP", pitchersNm$SP, pitchersSuf$SP)),
      column(width = 6, pitcherCol("RP", pitchersNm$RP, pitchersSuf$RP))
    )
```

- [ ] **Step 7: Rewrite `benchUI` to link names**

The current `benchUI` is:

```r
    benchUI <- if (length(bench) > 0) {
      tags$div(style = "margin-top:14px;",
               tags$strong("Bench: "),
               tags$span(paste(bench, collapse = "  ·  ")))
    } else NULL
```

Replace with:

```r
    benchUI <- if (length(benchNm) > 0) {
      items <- vapply(seq_along(benchNm),
                      function(i) paste0(playerLink(benchNm[i]), benchSuf[i]),
                      character(1))
      tags$div(style = "margin-top:14px;",
               tags$strong("Bench: "),
               HTML(paste(items, collapse = "  ·  ")))
    } else NULL
```

- [ ] **Step 8: Parse check**

```bash
cd Guardians && Rscript -e 'parse("server.R"); cat("parse OK\n")'
```

Expected: `parse OK`.

- [ ] **Step 9: Commit**

```bash
git add Guardians/server.R
git commit -m "feat(guardians): link player names in SP/RP columns and Bench line

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 6: Manual verification against the running app

There is no automated test harness for these reactive outputs. Verify behavior
by driving the running app, per the spec's testing section.

**Files:** none (verification only)

- [ ] **Step 1: Launch the app**

Use the webapp-testing skill / Playwright. Start the app on a fixed port:

```bash
cd Guardians && Rscript -e 'shiny::runApp(port = 7654, launch.browser = FALSE)'
```

Then drive `http://127.0.0.1:7654` with Playwright.

- [ ] **Step 2: Verify each tab navigates**

For each location below, click a player name and confirm the app switches to the
**Player Detail** tab with that exact player loaded and their card rendered:

- Depth Chart — a name in the SVG diamond
- Depth Chart — a name in the SP or RP column
- Depth Chart — a name in the Bench line
- Depth Chart — a name in the roster stat table
- Hot / Cold table
- Prospects → Hitters
- Prospects → Pitchers
- Risers
- Transactions
- IL

- [ ] **Step 3: Verify the matched-only rule**

On Prospects, find a row whose player is not on the current roster (no link —
plain text). Confirm it is not clickable. If every prospect happens to be on the
roster, note that and skip.

- [ ] **Step 4: Verify apostrophe names**

If the roster contains a name with an apostrophe, click it and confirm it
navigates correctly (the `jsStr()` escape). If none exists, note and skip.

- [ ] **Step 5: Report results**

Summarize which locations passed. If any failed, capture the console error /
screenshot and fix before considering the feature complete.
