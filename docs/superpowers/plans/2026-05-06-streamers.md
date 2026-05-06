# Streamers Tab Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a new "Streamers" tab to the LeagueEval Shiny app that surfaces FAs hot in a specific scoring category over the last 14 days, by exposing per-stat z-scores that `hotScores()` already computes but discards.

**Architecture:** Five-line widening of `hotScores()` in `daflFunctions.r` (gated by a new `withZ = FALSE` parameter for backward compatibility), one-token change at each of two `inSeasonPulse.r` call sites, then one new tab in `LeagueEval/ui.R` and two new reactives in `LeagueEval/server.R`.

**Tech Stack:** R, Shiny, DT, dplyr.

**Spec:** `docs/superpowers/specs/2026-05-06-streamers-design.md`.

---

## File Structure

- **Modify** `code/daflFunctions.r:911-973` — add `withZ = FALSE` parameter to `hotScores()`; widen the final `select` calls to include per-stat z-scores when `withZ = TRUE`.
- **Modify** `code/inSeasonPulse.r:247` and `code/inSeasonPulse.r:945` — pass `withZ = TRUE` so per-stat z-scores ride along on the existing `left_join` into `AllH` / `AllP`.
- **Modify** `LeagueEval/ui.R` — insert one `tabPanel("Streamers", ...)` between the existing `"Injured"` (lines 201-204) and `"My Targets"` (lines 205-210) tabPanels.
- **Modify** `LeagueEval/server.R` — add two `DT::renderDataTable` blocks (`output$streamersHitters`, `output$streamersPitchers`) immediately above the `# My Targets` block at line 708.

No new files. No test framework in this codebase — verification is sourcing R files in a fresh session and launching the Shiny app.

## Reference

Hitter per-stat z-scores: `zHR, zR, zRBI, zSB, zxH`
Pitcher per-stat z-scores: `zW, zSO, zHLD, zSV, zxER`

Hitter visible columns:
```
Target | Player | Pos | Team | AB | HR | R | RBI | SB | AVG | zHR | zR | zRBI | zSB | zxH | pDFL
```

Pitcher visible columns:
```
Target | Player | Pos | Team | INN | W | K | S | HD | ERA | zW | zSO | zSV | zHLD | zxER | pDFL
```

The `markTargets()` helper at `LeagueEval/server.R:99-103` adds `Target` (HTML star span) and `isTarget` columns onto a data frame keyed by `playerid`.

---

### Task 1: Add `withZ` parameter to `hotScores()`

**Goal:** `hotScores()` returns the existing two-column result by default, but when `withZ = TRUE` the per-stat z-scores are included in each data frame. Existing callers in `faabAnalysis.r`, `draftGuideLive.r`, `inSeasonPulse.r`, and `LiveDraftTool/server.R` continue to work unchanged.

**Files:**
- Modify: `code/daflFunctions.r:911` (signature)
- Modify: `code/daflFunctions.r:965-972` (final select)

- [ ] **Step 1: Update the function signature**

In `code/daflFunctions.r` at line 911, change:

```r
hotScores <- function(toph,topp,tm=FALSE) {
```

to:

```r
hotScores <- function(toph,topp,tm=FALSE,withZ=FALSE) {
```

- [ ] **Step 2: Widen the final `select` calls**

In `code/daflFunctions.r`, replace the existing `if (tm) { ... } else { ... }` block (lines 965-972) with the version below. The new block adds the `withZ` axis on top of the existing `tm` axis, producing four cases.

Locate this existing code:

```r
  if (tm) {
    bhitters <- select(ih2,playerid,zScore,Team)
    bpitchers <- select(ip2,playerid,zScore,Team)

  } else {
    bhitters <- select(ih2,playerid,zScore)
    bpitchers <- select(ip2,playerid,zScore)
  }
```

Replace with:

```r
  hZcols <- c('zHR','zR','zRBI','zSB','zxH')
  pZcols <- c('zW','zSO','zHLD','zSV','zxER')
  if (tm) {
    if (withZ) {
      bhitters  <- select(ih2, playerid, Team, zScore, all_of(hZcols))
      bpitchers <- select(ip2, playerid, Team, zScore, all_of(pZcols))
    } else {
      bhitters  <- select(ih2, playerid, zScore, Team)
      bpitchers <- select(ip2, playerid, zScore, Team)
    }
  } else {
    if (withZ) {
      bhitters  <- select(ih2, playerid, zScore, all_of(hZcols))
      bpitchers <- select(ip2, playerid, zScore, all_of(pZcols))
    } else {
      bhitters  <- select(ih2, playerid, zScore)
      bpitchers <- select(ip2, playerid, zScore)
    }
  }
```

- [ ] **Step 3: Verify the function still parses by sourcing it**

From `code/` as the working directory, run in R:

```r
setwd("code")  # if not already there
source("daflFunctions.r")
```

Expected: no errors. The script sources cleanly. (Other warnings about overwritten functions or package masking are not relevant.)

- [ ] **Step 4: Smoke-test `withZ = TRUE` returns the new columns**

In R, with `daflFunctions.r` sourced:

```r
# Tiny synthetic data frames with all columns hotScores() reads
toph <- data.frame(
  playerid = c("p1","p2","p3"),
  Team = c("A","B","C"),
  AB = c(40, 30, 50),
  H = c(12, 8, 14),
  HR = c(2, 1, 3), R = c(8, 4, 10), RBI = c(7, 5, 9), SB = c(3, 1, 0),
  AVG = c(.300, .267, .280),
  stringsAsFactors = FALSE
)
topp <- data.frame(
  playerid = c("q1","q2","q3"),
  Team = c("A","B","C"),
  INN = c(15, 20, 12), ER = c(4, 3, 5),
  W = c(2, 1, 0), K = c(15, 20, 10), HD = c(0, 0, 0), S = c(0, 0, 0),
  ERA = c(2.40, 1.35, 3.75),
  stringsAsFactors = FALSE
)

r <- hotScores(toph, topp, withZ = TRUE)
colnames(r[[1]])  # hitters
colnames(r[[2]])  # pitchers
```

Expected output:
```
[1] "playerid" "zScore"   "zHR"      "zR"       "zRBI"     "zSB"      "zxH"
[1] "playerid" "zScore"   "zW"       "zSO"      "zHLD"     "zSV"      "zxER"
```

If you see column names with `.x`/`.y` suffixes, the function ran but the inner `mutate` re-used a name that already existed on the input data frame. Inspect `colnames(toph)` and `colnames(topp)` for the synthetic data — none of `zHR`, `zR`, etc. should be present.

- [ ] **Step 5: Smoke-test `withZ = FALSE` (default) is unchanged**

In R:

```r
r2 <- hotScores(toph, topp)
colnames(r2[[1]])
colnames(r2[[2]])
```

Expected:
```
[1] "playerid" "zScore"
[1] "playerid" "zScore"
```

- [ ] **Step 6: Commit**

```bash
git add code/daflFunctions.r
git commit -m "feat: add withZ parameter to hotScores for per-stat z-scores"
```

---

### Task 2: Wire the per-stat z-scores into `AllH` / `AllP`

**Goal:** Both `hotScores()` call sites in `inSeasonPulse.r` pass `withZ = TRUE`. After sourcing `inSeasonPulse.r`, `colnames(AllH)` includes `zHR, zR, zRBI, zSB, zxH` and `colnames(AllP)` includes `zW, zSO, zHLD, zSV, zxER`.

**Files:**
- Modify: `code/inSeasonPulse.r:247`
- Modify: `code/inSeasonPulse.r:945`

- [ ] **Step 1: Pre-flight check — confirm no name collisions on `AllH` / `AllP`**

Before changing the call sites, source the existing pipeline once and inspect what columns `AllH` / `AllP` carry today. From R, with `code/` as the working directory:

```r
source("inSeasonPulse.r")
intersect(colnames(AllH), c("zHR","zR","zRBI","zSB","zxH"))
intersect(colnames(AllP), c("zW","zSO","zHLD","zSV","zxER"))
```

Expected: both `intersect` calls return `character(0)` — no collisions.

If either returns non-empty: stop and revisit. The widened join will produce `.x`/`.y` suffixes and the downstream `select` in Task 4/5 will silently miss the columns. If this happens, append a step to rename or drop the colliding column before the join.

- [ ] **Step 2: Update the inline call site at line 247**

In `code/inSeasonPulse.r` at line 247, change:

```r
r <- hotScores(toph,topp)
```

to:

```r
r <- hotScores(toph,topp,withZ=TRUE)
```

- [ ] **Step 3: Update the refresh-handler call site at line 945**

In `code/inSeasonPulse.r` at line 945, change:

```r
  r <- hotScores(Allhitters, Allpitchers)
```

to:

```r
  r <- hotScores(Allhitters, Allpitchers, withZ = TRUE)
```

- [ ] **Step 4: Re-source the pipeline and verify the new columns are on `AllH` / `AllP`**

In a fresh R session, with `code/` as the working directory:

```r
source("inSeasonPulse.r")
intersect(colnames(AllH), c("zHR","zR","zRBI","zSB","zxH"))
intersect(colnames(AllP), c("zW","zSO","zHLD","zSV","zxER"))
head(AllH[, c("Player","HR","SB","zHR","zSB","hotscore")], 5)
head(AllP[, c("Player","W","K","zW","zSO","hotscore")], 5)
```

Expected:
- Both `intersect` calls return all five names.
- The `head` calls show real numeric values for the z-columns (mostly small numbers, mean ≈ 0). No `NA` for players who actually appear in `Allhitters` / `Allpitchers`. (Players who didn't play in the last 14 days will have `NA` z-scores via the `left_join` — that is expected and matches the existing `hotscore = NA` behavior.)

- [ ] **Step 5: Commit**

```bash
git add code/inSeasonPulse.r
git commit -m "feat: carry per-stat z-scores onto AllH and AllP"
```

---

### Task 3: Add the Streamers tab shell to `ui.R`

**Goal:** "Streamers" tab is visible in the navbar between "Injured" and "My Targets". Layout shows the FA-only checkbox plus two empty section headers and table outputs. App still loads without parse errors.

**Files:**
- Modify: `LeagueEval/ui.R` — insert new `tabPanel` between line 204 (closing `)),` of the `"Injured"` panel) and line 205 (opening of `"My Targets"` panel).

- [ ] **Step 1: Insert the new tabPanel**

In `LeagueEval/ui.R`, after the closing `)),` of the `"Injured"` panel (line 204) and before `tabPanel("My Targets", ...` (line 205), insert exactly:

```r
    tabPanel("Streamers",
             verticalLayout(
               h2("Streamers — last 14 days"),
               checkboxInput('faStreamers', 'Free Agents Only', value = TRUE),
               h3("Hitters"),
               DT::dataTableOutput("streamersHitters"),
               br(),
               h3("Pitchers"),
               DT::dataTableOutput("streamersPitchers")
             )),
```

- [ ] **Step 2: Verify ui.R parses by launching the app**

From R, with the project root as the working directory:

```r
shiny::runApp('LeagueEval')
```

Expected:
- App starts with no parse errors.
- "Streamers" tab appears in the navbar between "Injured" and "My Targets".
- Clicking it shows the page heading "Streamers — last 14 days", the "Free Agents Only" checkbox (checked by default), an h3 "Hitters" header, an empty rectangle where the table will render (DT shows "No data available in table" when the output is unbound), an h3 "Pitchers" header, and another empty rectangle.

If you see an "unexpected token" error, the most common cause is a missing or extra comma between tabPanels. Confirm the line above the inserted block ends with `)),` and the inserted block also ends with `)),`.

- [ ] **Step 3: Commit**

```bash
git add LeagueEval/ui.R
git commit -m "feat: add Streamers tab shell to LeagueEval UI"
```

---

### Task 4: Implement the hitters Streamers table

**Goal:** Picking the "Streamers" tab renders a DT table of hitters from `AllH` with the columns specified in the spec. The FA toggle filters to `Team == 'Free Agent'` (default ON). Star column is interactive: clicking it adds/removes the player from `rv$targets`. Sorting any z-column reorders the table.

**Files:**
- Modify: `LeagueEval/server.R` — add `output$streamersHitters` block immediately above the `# My Targets` comment at line 708.

- [ ] **Step 1: Insert the hitter renderer**

In `LeagueEval/server.R`, immediately above the line that reads `# My Targets` at line 708, insert:

```r
# Streamers — hitters
  output$streamersHitters <- DT::renderDataTable({
    rv$refreshCount
    rv$targets  # react to target changes
    df <- AllH %>%
      select(playerid, Player, Pos, Team, AB, HR, R, RBI, SB, AVG,
             zHR, zR, zRBI, zSB, zxH, pDFL)
    if (isTRUE(input$faStreamers)) df <- filter(df, Team == 'Free Agent')
    df <- df %>% arrange(desc(pDFL))
    ff <- markTargets(df, isolate(rv$targets)) %>%
      select(Target, Player, Pos, Team, AB, HR, R, RBI, SB, AVG,
             zHR, zR, zRBI, zSB, zxH, pDFL,
             -playerid, -isTarget)
    datatable(ff,
              options = list(pageLength = 25, autoWidth = FALSE, info = FALSE),
              filter = 'top', escape = FALSE) %>%
      formatCurrency('pDFL') %>%
      formatRound(c('zHR','zR','zRBI','zSB','zxH'), 2) %>%
      formatRound('AVG', 3) %>%
      formatRound('AB', 0)
  })

```

(Leave one blank line between this block and the existing `# My Targets` block.)

- [ ] **Step 2: Verify the table renders**

In R, run `shiny::runApp('LeagueEval')` and click the "Streamers" tab.

Expected:
- The "Hitters" section now shows a DT table.
- Default sort is by pDFL descending — top rows are the highest-projection FAs.
- Per-column DT filter row is visible at the top.
- Columns are: Target, Player, Pos, Team, AB, HR, R, RBI, SB, AVG, zHR, zR, zRBI, zSB, zxH, pDFL.
- `Team` column shows "Free Agent" on every row (because the FA checkbox defaults to ON).
- Clicking the "zSB" column header re-sorts the table; the top row should be a player with conspicuously high recent SB count.
- Star icons render in the Target column. Clicking one toggles between filled (★) and outline (☆); the toast notification "Added target: …" or "Removed target: …" appears.

- [ ] **Step 3: Verify the FA toggle works**

Uncheck "Free Agents Only".

Expected: the table now includes rostered players (Team is something other than "Free Agent" on most rows). Re-check the box: returns to FA-only.

- [ ] **Step 4: Commit**

```bash
git add LeagueEval/server.R
git commit -m "feat: add Streamers hitters table"
```

---

### Task 5: Implement the pitchers Streamers table

**Goal:** Pitchers section of the Streamers tab renders symmetric to the hitters table, using `AllP` and the pitcher z-score columns.

**Files:**
- Modify: `LeagueEval/server.R` — add `output$streamersPitchers` block immediately below the hitters block from Task 4 and above the `# My Targets` comment.

- [ ] **Step 1: Insert the pitcher renderer**

In `LeagueEval/server.R`, immediately below the `output$streamersHitters` closing `})` from Task 4, and immediately above the `# My Targets` comment, insert:

```r
# Streamers — pitchers
  output$streamersPitchers <- DT::renderDataTable({
    rv$refreshCount
    rv$targets  # react to target changes
    df <- AllP %>%
      select(playerid, Player, Pos, Team, INN, W, K, S, HD, ERA,
             zW, zSO, zSV, zHLD, zxER, pDFL)
    if (isTRUE(input$faStreamers)) df <- filter(df, Team == 'Free Agent')
    df <- df %>% arrange(desc(pDFL))
    ff <- markTargets(df, isolate(rv$targets)) %>%
      select(Target, Player, Pos, Team, INN, W, K, S, HD, ERA,
             zW, zSO, zSV, zHLD, zxER, pDFL,
             -playerid, -isTarget)
    datatable(ff,
              options = list(pageLength = 25, autoWidth = FALSE, info = FALSE),
              filter = 'top', escape = FALSE) %>%
      formatCurrency('pDFL') %>%
      formatRound(c('zW','zSO','zSV','zHLD','zxER'), 2) %>%
      formatRound('ERA', 2) %>%
      formatRound(c('INN','W','K','S','HD'), 0)
  })

```

- [ ] **Step 2: Verify the pitchers table renders**

In R, run `shiny::runApp('LeagueEval')` and click the "Streamers" tab.

Expected:
- "Pitchers" section now shows a DT table.
- Default sort is by pDFL descending.
- Columns are: Target, Player, Pos, Team, INN, W, K, S, HD, ERA, zW, zSO, zSV, zHLD, zxER, pDFL.
- Pos values are `SP` / `MR` / `CL` (set by `inSeasonPulse.r:180`).
- Sorting by `zSV` should put closers with high recent saves at the top.
- Star toggle works on this table too.

- [ ] **Step 3: Verify the FA toggle drives both tables**

Toggle "Free Agents Only" off, then on.

Expected: both the hitters and pitchers tables update together.

- [ ] **Step 4: Commit**

```bash
git add LeagueEval/server.R
git commit -m "feat: add Streamers pitchers table"
```

---

## Self-Review

Spec coverage:
- "Tab placement between Injured and My Targets" → Task 3.
- "Vertical layout, FA checkbox default on" → Task 3.
- "`hotScores()` exposes per-stat z-scores via `withZ`" → Task 1.
- "Both `inSeasonPulse.r` call sites updated" → Task 2.
- "Hitter columns + formatting" → Task 4.
- "Pitcher columns + formatting" → Task 5.
- "Default sort by `-pDFL`" → Tasks 4 and 5.
- "Star/target column via `markTargets()`" → Tasks 4 and 5.
- "Defensive check for column collisions on `AllH` / `AllP`" → Task 2 Step 1 (pre-flight).
- "Out of scope: trend, position-eligibility, min-AB floor" → not implemented.

No spec gaps.
