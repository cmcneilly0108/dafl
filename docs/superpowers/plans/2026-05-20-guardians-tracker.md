# Guardians Tracker Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a new standalone Shiny app `Guardians/` that lets the user follow the entire Cleveland Guardians organization (MLB through DSL) with daily snapshots, a hot/cold board (z-scored within each level), player detail pages, and a transactions feed.

**Architecture:** New Shiny app sibling to `LeagueEval/`. A new `code/guardiansPulse.r` pipeline pulls roster + season stats via `baseballr::mlb_*`, rolling-window game logs via `baseballr::fg_milb_*`, transactions via `baseballr::mlb_transactions`, and FanGraphs prospect FV via existing `getFGProspects()`. Daily snapshots upsert into 4 new tables in the existing `code/DAFL.db`. A `run_guardians_pulse.sh` launchd wrapper runs the pulse each morning. The Shiny UI sources the pulse at startup (fast no-op when today's snapshot exists).

**Tech Stack:** R, Shiny, DT, plotly, baseballr, RSQLite, dplyr, jsonlite.

**Spec:** `docs/superpowers/specs/2026-05-20-guardians-tracker-design.md`

---

## File Structure

**Create:**
- `code/guardiansPulse.r` — data pipeline orchestrator
- `code/run_guardians_pulse.sh` — launchd wrapper
- `Guardians/ui.R` — Shiny UI
- `Guardians/server.R` — Shiny server

**Modify:**
- `code/daflFunctions.r` — add helpers: `initGuardiansDB()`, `resolveGuardiansAffiliates()`, `pullGuardiansRoster()`, `pullGuardiansStats()`, `pullGuardiansTransactions()`, `pullGuardiansGameLogs()`, `computeGuardiansHotscore()`

**Touched at runtime (no source changes):**
- `code/DAFL.db` — new tables: `GuardiansRoster`, `GuardiansStats`, `GuardiansTransactions`, `GuardiansHotscore`

---

## Task 1: Create DB schema initializer

**Files:**
- Modify: `code/daflFunctions.r` (append at end of file)

- [ ] **Step 1: Add `initGuardiansDB()` to `code/daflFunctions.r`**

Append at the end of the file:

```r
# ============================================================
# Guardians Tracker — DB schema + helpers (see docs/superpowers/specs/2026-05-20-guardians-tracker-design.md)
# ============================================================

# Create the four Guardians-tracker tables if they don't exist.
# Idempotent: safe to call on every pulse run.
initGuardiansDB <- function(dbPath = "DAFL.db") {
  conn <- dbConnect(RSQLite::SQLite(), dbPath)
  on.exit(dbDisconnect(conn))

  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS GuardiansRoster (
      snapshot_date TEXT NOT NULL,
      mlb_id        INTEGER NOT NULL,
      fg_id         TEXT,
      player        TEXT,
      pos           TEXT,
      level         TEXT,
      team_id       INTEGER,
      age           REAL,
      PRIMARY KEY (snapshot_date, mlb_id)
    )")

  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS GuardiansStats (
      snapshot_date TEXT NOT NULL,
      mlb_id        INTEGER NOT NULL,
      level         TEXT,
      role          TEXT,
      pa INTEGER, ab INTEGER, h INTEGER, hr INTEGER,
      r INTEGER, rbi INTEGER, sb INTEGER, bb INTEGER, k INTEGER,
      avg REAL, obp REAL, slg REAL, woba REAL,
      ip REAL, w INTEGER, l INTEGER, sv INTEGER, hld INTEGER,
      so INTEGER, bb_p INTEGER,
      era REAL, fip REAL, k9 REAL, bb9 REAL, whip REAL,
      PRIMARY KEY (snapshot_date, mlb_id)
    )")

  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS GuardiansTransactions (
      txn_id        TEXT PRIMARY KEY,
      txn_date      TEXT NOT NULL,
      mlb_id        INTEGER,
      player        TEXT,
      type          TEXT,
      from_team_id  INTEGER,
      to_team_id    INTEGER,
      description   TEXT
    )")

  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS GuardiansHotscore (
      snapshot_date TEXT NOT NULL,
      mlb_id        INTEGER NOT NULL,
      level         TEXT,
      role          TEXT,
      window_days   INTEGER NOT NULL,
      hotscore      REAL,
      PRIMARY KEY (snapshot_date, mlb_id, window_days)
    )")

  invisible(TRUE)
}
```

- [ ] **Step 2: Add `library("RSQLite")` to the top of `code/daflFunctions.r` if absent**

Check `code/daflFunctions.r:5-12` — the library block. If `library("RSQLite")` is not there, add it after `library("httr")`.

- [ ] **Step 3: Verify schema initializer runs cleanly**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL/code
Rscript -e 'source("daflFunctions.r"); initGuardiansDB(); cat("OK\n")'
```

Expected last line: `OK`

- [ ] **Step 4: Verify the tables exist in DAFL.db**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL/code
sqlite3 DAFL.db ".tables" | tr ' ' '\n' | grep '^Guardians'
```

Expected output (4 lines):
```
GuardiansHotscore
GuardiansRoster
GuardiansStats
GuardiansTransactions
```

- [ ] **Step 5: Commit**

```bash
git add code/daflFunctions.r
git commit -m "feat(guardians): add DB schema initializer for tracker tables"
```

---

## Task 2: Resolve Guardians org affiliate IDs

**Files:**
- Modify: `code/daflFunctions.r` (append after `initGuardiansDB`)

- [ ] **Step 1: Add `resolveGuardiansAffiliates()` to `code/daflFunctions.r`**

Append after the `initGuardiansDB` function:

```r
# Hardcoded fallback table — the Guardians org affiliates as of 2026.
# Used when mlb_team_affiliates() fails. team_id values are MLB's stable
# affiliate IDs (verify with mlb_team_affiliates(team_ids = 114) on first run).
.guardiansAffiliatesFallback <- function() {
  data.frame(
    level    = c("MLB",     "AAA",      "AA",     "A+",         "A",        "ACL",          "DSL"),
    sport_id = c(1L,        11L,        12L,      13L,          14L,        16L,            17L),
    team_id  = c(114L,      445L,       402L,     437L,         538L,       5454L,          4189L),
    name     = c("Cleveland", "Columbus", "Akron", "Lake County","Lynchburg","ACL Guardians","DSL Guardians"),
    stringsAsFactors = FALSE
  )
}

# Resolve Guardians org → affiliate IDs. Caches result in
# `../data/guardiansAffiliates.csv` for one week. Falls back to the
# hardcoded table if both the cache and the API fail.
resolveGuardiansAffiliates <- function(cachePath = "../data/guardiansAffiliates.csv",
                                       maxAgeDays = 7) {
  cacheAgeDays <- if (file.exists(cachePath)) {
    as.numeric(difftime(Sys.time(), file.info(cachePath)$mtime, units = "days"))
  } else Inf
  if (cacheAgeDays < maxAgeDays) {
    return(read.csv(cachePath, stringsAsFactors = FALSE))
  }
  out <- tryCatch({
    af <- baseballr::mlb_team_affiliates(team_ids = 114)
    if (is.null(af) || nrow(af) == 0) stop("empty affiliates from baseballr")
    # baseballr returns columns like sport_id, team_id, team_name, sport_name.
    # Map to our schema. We always include MLB (114) as the parent row.
    parent <- data.frame(level = "MLB", sport_id = 1L, team_id = 114L,
                         name = "Cleveland", stringsAsFactors = FALSE)
    sportToLevel <- c(`11` = "AAA", `12` = "AA", `13` = "A+", `14` = "A",
                      `16` = "ACL", `17` = "DSL")
    children <- data.frame(
      level    = unname(sportToLevel[as.character(af$sport_id)]),
      sport_id = as.integer(af$sport_id),
      team_id  = as.integer(af$team_id),
      name     = af$team_name,
      stringsAsFactors = FALSE
    )
    children <- children[!is.na(children$level), ]
    rbind(parent, children)
  }, error = function(e) {
    warning("resolveGuardiansAffiliates: API failed (", e$message,
            "); using hardcoded fallback")
    .guardiansAffiliatesFallback()
  })
  tryCatch(write.csv(out, cachePath, row.names = FALSE),
           error = function(e) NULL)
  out
}
```

- [ ] **Step 2: Verify it returns a non-empty data frame**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL/code
Rscript -e 'suppressMessages(source("daflFunctions.r")); af <- resolveGuardiansAffiliates(); print(af); stopifnot(nrow(af) >= 5)'
```

Expected: a data frame with at least 5 rows including at minimum MLB, AAA, AA, A+, A; final exit code 0.

- [ ] **Step 3: Commit**

```bash
git add code/daflFunctions.r data/guardiansAffiliates.csv
git commit -m "feat(guardians): resolve org affiliate IDs with hardcoded fallback"
```

---

## Task 3: Pull org roster

**Files:**
- Modify: `code/daflFunctions.r` (append after `resolveGuardiansAffiliates`)

- [ ] **Step 1: Add `pullGuardiansRoster()` to `code/daflFunctions.r`**

```r
# Pull today's roster for every Guardians affiliate. Returns a data frame
# with one row per (player, level). team_id is the affiliate id; level is
# MLB / AAA / AA / A+ / A / ACL / DSL.
pullGuardiansRoster <- function(affiliates = resolveGuardiansAffiliates(),
                                season = cyear) {
  rosters <- lapply(seq_len(nrow(affiliates)), function(i) {
    af <- affiliates[i, ]
    tryCatch({
      r <- baseballr::mlb_team_roster(team_id = af$team_id,
                                      season = as.integer(season),
                                      roster_type = "fullSeason")
      if (is.null(r) || nrow(r) == 0) return(NULL)
      data.frame(
        mlb_id  = as.integer(r$person_id),
        player  = r$person_full_name,
        pos     = r$position_abbreviation,
        level   = af$level,
        team_id = af$team_id,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      warning("pullGuardiansRoster: ", af$level, " (", af$team_id, ") failed: ",
              e$message)
      NULL
    })
  })
  do.call(rbind, rosters)
}
```

- [ ] **Step 2: Verify the roster pull returns rows**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL/code
Rscript -e 'suppressMessages(source("daflFunctions.r")); r <- pullGuardiansRoster(); print(head(r)); cat("\nlevels:", paste(unique(r$level), collapse=", "), "\n"); cat("rows:", nrow(r), "\n"); stopifnot(nrow(r) >= 30, "MLB" %in% r$level)'
```

Expected: a head() table with mlb_id/player/pos/level/team_id columns, MLB present in levels, at least 30 rows total.

- [ ] **Step 3: Commit**

```bash
git add code/daflFunctions.r
git commit -m "feat(guardians): pull org roster via baseballr::mlb_team_roster"
```

---

## Task 4: Pull org season stats

**Files:**
- Modify: `code/daflFunctions.r` (append after `pullGuardiansRoster`)

- [ ] **Step 1: Add `pullGuardiansStats()` to `code/daflFunctions.r`**

```r
# Pull season-to-date hitting and pitching stats for every Guardians affiliate.
# Returns a data frame keyed by mlb_id with a `role` column ("H"/"P"). When an
# affiliate has no games yet (spring training, DSL not yet active) the call
# returns NULL for that level and is dropped.
pullGuardiansStats <- function(affiliates = resolveGuardiansAffiliates(),
                               season = cyear) {
  pullOne <- function(af, group) {
    tryCatch({
      s <- baseballr::mlb_stats(stats = "season",
                                group = group,
                                season = as.integer(season),
                                sport_id = af$sport_id,
                                team_id = af$team_id)
      if (is.null(s) || nrow(s) == 0) return(NULL)
      s$level    <- af$level
      s$role     <- if (group == "hitting") "H" else "P"
      s$mlb_id   <- as.integer(s$player_id)
      s
    }, error = function(e) {
      warning("pullGuardiansStats: ", af$level, "/", group, " failed: ", e$message)
      NULL
    })
  }
  hitRows <- lapply(seq_len(nrow(affiliates)), function(i) pullOne(affiliates[i, ], "hitting"))
  pitRows <- lapply(seq_len(nrow(affiliates)), function(i) pullOne(affiliates[i, ], "pitching"))

  # baseballr's column names vary by group; coerce to our schema. Missing
  # columns become NA so the bind_rows below stays well-formed.
  normH <- function(df) {
    if (is.null(df)) return(NULL)
    safe <- function(col) if (col %in% names(df)) df[[col]] else NA
    data.frame(
      mlb_id = df$mlb_id, level = df$level, role = "H",
      pa = safe("plate_appearances"), ab = safe("at_bats"),
      h  = safe("hits"), hr = safe("home_runs"),
      r  = safe("runs"), rbi = safe("rbi"),
      sb = safe("stolen_bases"), bb = safe("base_on_balls"),
      k  = safe("strike_outs"),
      avg = suppressWarnings(as.numeric(safe("avg"))),
      obp = suppressWarnings(as.numeric(safe("obp"))),
      slg = suppressWarnings(as.numeric(safe("slg"))),
      woba = NA_real_,  # not in mlb_stats response; left for FG join later
      ip = NA_real_, w = NA_integer_, l = NA_integer_,
      sv = NA_integer_, hld = NA_integer_,
      so = NA_integer_, bb_p = NA_integer_,
      era = NA_real_, fip = NA_real_, k9 = NA_real_,
      bb9 = NA_real_, whip = NA_real_,
      stringsAsFactors = FALSE
    )
  }
  normP <- function(df) {
    if (is.null(df)) return(NULL)
    safe <- function(col) if (col %in% names(df)) df[[col]] else NA
    data.frame(
      mlb_id = df$mlb_id, level = df$level, role = "P",
      pa = NA_integer_, ab = NA_integer_, h = NA_integer_, hr = NA_integer_,
      r = NA_integer_, rbi = NA_integer_, sb = NA_integer_, bb = NA_integer_,
      k = NA_integer_, avg = NA_real_, obp = NA_real_, slg = NA_real_, woba = NA_real_,
      ip  = suppressWarnings(as.numeric(safe("innings_pitched"))),
      w   = safe("wins"), l = safe("losses"),
      sv  = safe("saves"), hld = safe("holds"),
      so  = safe("strike_outs"), bb_p = safe("base_on_balls"),
      era = suppressWarnings(as.numeric(safe("era"))),
      fip = NA_real_,
      k9  = suppressWarnings(as.numeric(safe("strikeouts_per9inn"))),
      bb9 = suppressWarnings(as.numeric(safe("walks_per9inn"))),
      whip = suppressWarnings(as.numeric(safe("whip"))),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, c(lapply(hitRows, normH), lapply(pitRows, normP)))
}
```

- [ ] **Step 2: Verify the stats pull returns rows**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL/code
Rscript -e 'suppressMessages(source("daflFunctions.r")); s <- pullGuardiansStats(); cat("rows:", nrow(s), "\nlevels:", paste(unique(s$level), collapse=", "), "\nroles:", paste(unique(s$role), collapse=", "), "\n"); print(head(s[s$role=="H" & s$level=="MLB", c("mlb_id","level","role","ab","hr","avg")])); stopifnot(nrow(s) > 0)'
```

Expected: levels include at least MLB; both `H` and `P` roles present; non-zero AB / HR / AVG visible for MLB hitters.

- [ ] **Step 3: Commit**

```bash
git add code/daflFunctions.r
git commit -m "feat(guardians): pull org season stats via baseballr::mlb_stats"
```

---

## Task 5: Pull org transactions

**Files:**
- Modify: `code/daflFunctions.r` (append after `pullGuardiansStats`)

- [ ] **Step 1: Add `pullGuardiansTransactions()` to `code/daflFunctions.r`**

```r
# Pull the last `lookbackDays` of transactions for the Guardians org.
# Returns one row per transaction matching our schema. Filters by team_id
# matching any of the affiliate ids in either from_team_id or to_team_id.
pullGuardiansTransactions <- function(affiliates = resolveGuardiansAffiliates(),
                                      lookbackDays = 30) {
  endDate   <- as.character(Sys.Date())
  startDate <- as.character(Sys.Date() - lookbackDays)
  txns <- tryCatch({
    baseballr::mlb_transactions(start_date = startDate, end_date = endDate)
  }, error = function(e) {
    warning("pullGuardiansTransactions failed: ", e$message); NULL
  })
  if (is.null(txns) || nrow(txns) == 0) {
    return(data.frame(
      txn_id = character(0), txn_date = character(0),
      mlb_id = integer(0), player = character(0), type = character(0),
      from_team_id = integer(0), to_team_id = integer(0),
      description = character(0),
      stringsAsFactors = FALSE
    ))
  }
  affIds <- as.integer(affiliates$team_id)
  hit <- (txns$from_team_id %in% affIds) | (txns$to_team_id %in% affIds)
  txns <- txns[hit, , drop = FALSE]
  if (nrow(txns) == 0) {
    return(data.frame(
      txn_id = character(0), txn_date = character(0),
      mlb_id = integer(0), player = character(0), type = character(0),
      from_team_id = integer(0), to_team_id = integer(0),
      description = character(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    txn_id       = as.character(txns$transaction_id),
    txn_date     = as.character(txns$date),
    mlb_id       = as.integer(txns$person_id),
    player       = txns$person_full_name,
    type         = txns$type_desc,
    from_team_id = as.integer(txns$from_team_id),
    to_team_id   = as.integer(txns$to_team_id),
    description  = txns$description,
    stringsAsFactors = FALSE
  )
}
```

- [ ] **Step 2: Verify transactions pull returns a well-formed data frame**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL/code
Rscript -e 'suppressMessages(source("daflFunctions.r")); t <- pullGuardiansTransactions(); cat("rows:", nrow(t), "\ncols:", paste(names(t), collapse=", "), "\n"); print(head(t)); stopifnot(all(c("txn_id","txn_date","mlb_id","player","type","from_team_id","to_team_id","description") %in% names(t)))'
```

Expected: a data frame with all 8 columns. Row count may be 0 in off-season — that's OK, only the column schema is enforced.

- [ ] **Step 3: Commit**

```bash
git add code/daflFunctions.r
git commit -m "feat(guardians): pull org transactions via baseballr::mlb_transactions"
```

---

## Task 6: Pull recent game logs

**Files:**
- Modify: `code/daflFunctions.r` (append after `pullGuardiansTransactions`)

- [ ] **Step 1: Add `pullGuardiansGameLogs()` to `code/daflFunctions.r`**

```r
# Pull last-30-day game logs for one player (used by HotScore). Tries FG MiLB
# endpoint first (covers all MiLB levels). Returns a data frame with at least
# `date`, `g` (games), and the role-specific counting stats. On any failure
# returns NULL so the caller can skip the player without crashing.
pullGuardiansGameLogs <- function(fg_id, role, season = cyear) {
  if (is.na(fg_id) || !nzchar(as.character(fg_id))) return(NULL)
  tryCatch({
    if (role == "H") {
      df <- baseballr::fg_milb_batter_game_logs(playerid = fg_id,
                                                year = as.integer(season))
    } else {
      df <- baseballr::fg_milb_pitcher_game_logs(playerid = fg_id,
                                                 year = as.integer(season))
    }
    if (is.null(df) || nrow(df) == 0) return(NULL)
    # Standardise the date column name across baseballr versions.
    dateCol <- intersect(c("Date", "date", "game_date"), names(df))[1]
    if (is.na(dateCol)) return(NULL)
    df$gl_date <- suppressWarnings(as.Date(df[[dateCol]]))
    df <- df[!is.na(df$gl_date) & df$gl_date >= Sys.Date() - 30, , drop = FALSE]
    if (nrow(df) == 0) return(NULL)
    df
  }, error = function(e) {
    warning("pullGuardiansGameLogs(", fg_id, "/", role, ") failed: ", e$message)
    NULL
  })
}
```

- [ ] **Step 2: Verify the function returns NULL gracefully on a bogus id**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL/code
Rscript -e 'suppressMessages(source("daflFunctions.r")); g <- pullGuardiansGameLogs(NA, "H"); stopifnot(is.null(g)); cat("NA→NULL OK\n"); g2 <- pullGuardiansGameLogs("0", "H"); stopifnot(is.null(g2)); cat("bogus id→NULL OK\n")'
```

Expected output:
```
NA→NULL OK
bogus id→NULL OK
```

- [ ] **Step 3: Commit**

```bash
git add code/daflFunctions.r
git commit -m "feat(guardians): pull last-30-day MiLB game logs per player"
```

---

## Task 7: Compute HotScore per level/window

**Files:**
- Modify: `code/daflFunctions.r` (append after `pullGuardiansGameLogs`)

- [ ] **Step 1: Add `computeGuardiansHotscore()` to `code/daflFunctions.r`**

```r
# Compute a daily hot score per (player, level, window). Cohort = all players
# at the same level (so an A+ player is scored against A+ peers).
#
# Args:
#   roster: data.frame with columns mlb_id, fg_id, level, role
#   logsByPlayer: named list of game-log data frames keyed by mlb_id (as char)
#   windows: integer vector of window sizes in days, e.g. c(7, 14, 30)
#
# Returns: long data.frame (mlb_id, level, role, window_days, hotscore)
computeGuardiansHotscore <- function(roster, logsByPlayer, windows = c(7, 14, 30)) {
  perPlayerWindow <- function(playerRow, win) {
    pid <- as.character(playerRow$mlb_id)
    logs <- logsByPlayer[[pid]]
    if (is.null(logs)) return(NULL)
    sub <- logs[logs$gl_date >= Sys.Date() - win, , drop = FALSE]
    if (nrow(sub) == 0) return(NULL)
    # Per-game core metric. Hitters: OPS-style ((H+BB)/PA * 1.2 + TB/AB).
    # Pitchers: K - BB - 2*HR (per-game). Both crude but cohort-relative.
    if (playerRow$role == "H") {
      pa <- sum(suppressWarnings(as.numeric(sub$PA)), na.rm = TRUE)
      ab <- sum(suppressWarnings(as.numeric(sub$AB)), na.rm = TRUE)
      h  <- sum(suppressWarnings(as.numeric(sub$H)),  na.rm = TRUE)
      bb <- sum(suppressWarnings(as.numeric(sub$BB)), na.rm = TRUE)
      tb <- sum(suppressWarnings(as.numeric(sub$TB)), na.rm = TRUE)
      if (pa < 5 || ab == 0) return(NULL)
      metric <- ((h + bb) / pa) * 1.2 + (tb / ab)
    } else {
      ip <- sum(suppressWarnings(as.numeric(sub$IP)), na.rm = TRUE)
      so <- sum(suppressWarnings(as.numeric(sub$SO)), na.rm = TRUE)
      bb <- sum(suppressWarnings(as.numeric(sub$BB)), na.rm = TRUE)
      hr <- sum(suppressWarnings(as.numeric(sub$HR)), na.rm = TRUE)
      if (ip < 3) return(NULL)
      metric <- (so - bb - 2 * hr) / ip
    }
    data.frame(mlb_id = playerRow$mlb_id, level = playerRow$level,
               role = playerRow$role, window_days = win, metric = metric,
               stringsAsFactors = FALSE)
  }

  rows <- list()
  for (i in seq_len(nrow(roster))) {
    for (w in windows) {
      r <- perPlayerWindow(roster[i, ], w)
      if (!is.null(r)) rows[[length(rows) + 1]] <- r
    }
  }
  if (length(rows) == 0) {
    return(data.frame(mlb_id = integer(0), level = character(0),
                      role = character(0), window_days = integer(0),
                      hotscore = numeric(0), stringsAsFactors = FALSE))
  }
  raw <- do.call(rbind, rows)
  # Z-score WITHIN (level, role, window_days). At least 3 players needed for
  # a meaningful sd; otherwise hotscore = 0.
  raw$hotscore <- NA_real_
  groups <- split(seq_len(nrow(raw)), list(raw$level, raw$role, raw$window_days),
                  drop = TRUE)
  for (idx in groups) {
    vals <- raw$metric[idx]
    if (length(vals) >= 3 && sd(vals, na.rm = TRUE) > 0) {
      raw$hotscore[idx] <- (vals - mean(vals, na.rm = TRUE)) / sd(vals, na.rm = TRUE)
    } else {
      raw$hotscore[idx] <- 0
    }
  }
  raw[, c("mlb_id", "level", "role", "window_days", "hotscore")]
}
```

- [ ] **Step 2: Verify with a synthetic roster + logs (no network needed)**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL/code
Rscript -e '
suppressMessages(source("daflFunctions.r"))
roster <- data.frame(
  mlb_id = 1:5, fg_id = as.character(1:5),
  level = rep("AAA", 5), role = rep("H", 5), stringsAsFactors = FALSE
)
mkLog <- function(h, bb, tb, ab, pa, n=10) data.frame(
  gl_date = Sys.Date() - seq_len(n) + 1,
  PA = rep(pa, n), AB = rep(ab, n), H = rep(h, n),
  BB = rep(bb, n), TB = rep(tb, n))
logs <- list(
  "1" = mkLog(0, 0, 0, 4, 4),     # cold
  "2" = mkLog(1, 0, 1, 4, 4),
  "3" = mkLog(2, 0, 3, 4, 4),
  "4" = mkLog(3, 1, 6, 4, 5),
  "5" = mkLog(4, 1, 9, 4, 5)      # hot
)
hs <- computeGuardiansHotscore(roster, logs, windows = c(7))
print(hs)
stopifnot(nrow(hs) == 5)
stopifnot(hs$hotscore[hs$mlb_id == 5] > hs$hotscore[hs$mlb_id == 1])
cat("HotScore ordering OK\n")
'
```

Expected last line: `HotScore ordering OK`. Hottest player (id=5) has the highest score; coldest (id=1) the lowest.

- [ ] **Step 3: Commit**

```bash
git add code/daflFunctions.r
git commit -m "feat(guardians): compute hotscore z-scored within (level,role,window)"
```

---

## Task 8: Pipeline orchestrator `guardiansPulse.r`

**Files:**
- Create: `code/guardiansPulse.r`

- [ ] **Step 1: Create `code/guardiansPulse.r`**

```r
# Guardians Tracker — daily data pipeline. See:
#   docs/superpowers/specs/2026-05-20-guardians-tracker-design.md
#
# Reads/writes code/DAFL.db (tables GuardiansRoster/Stats/Transactions/Hotscore).
# Hydrates globals (gRoster, gHot, gTxn, gIL, gProspects, gDepth, gTrend) for
# the Shiny app in Guardians/server.R.
#
# If today's snapshot already exists in DAFL.db and DAFL_FORCE_REFRESH is not
# set, the upstream pulls are skipped and globals are hydrated from the DB.

library("dplyr")
library("lubridate")
library("jsonlite")
library("RSQLite")
library("baseballr")

source("./daflFunctions.r")

today <- as.character(Sys.Date())
forceRefresh <- nchar(Sys.getenv("DAFL_FORCE_REFRESH")) > 0
dbPath <- "DAFL.db"

initGuardiansDB(dbPath)

conn <- dbConnect(RSQLite::SQLite(), dbPath)

# Today's snapshot present?
haveToday <- as.integer(dbGetQuery(conn,
  "SELECT COUNT(*) FROM GuardiansRoster WHERE snapshot_date = ?",
  params = list(today))[[1]]) > 0

if (forceRefresh || !haveToday) {
  cat("[guardians] Fetching upstream (force=", forceRefresh, ", haveToday=", haveToday, ")\n", sep = "")

  affiliates <- resolveGuardiansAffiliates()
  roster <- pullGuardiansRoster(affiliates)
  stats  <- pullGuardiansStats(affiliates)
  txns   <- pullGuardiansTransactions(affiliates)

  # Attach fg_id and role via existing prospects + Allhitters/Allpitchers caches.
  fgMap <- tryCatch({
    h <- getFGProspects(pos = "bat")
    p <- getFGProspects(pos = "pit")
    rbind(
      data.frame(mlb_id = suppressWarnings(as.integer(h$MLBAMID)),
                 fg_id = as.character(h$PlayerId), stringsAsFactors = FALSE),
      data.frame(mlb_id = suppressWarnings(as.integer(p$MLBAMID)),
                 fg_id = as.character(p$PlayerId), stringsAsFactors = FALSE)
    )
  }, error = function(e) {
    warning("getFGProspects failed: ", e$message); data.frame()
  })
  if (nrow(fgMap) > 0) {
    fgMap <- fgMap[!is.na(fgMap$mlb_id), ]
    roster <- left_join(roster, fgMap, by = "mlb_id")
  } else {
    roster$fg_id <- NA_character_
  }

  # Age (years, computed against opening day) — not in mlb_team_roster output.
  # mlb_people gives birth_date; cheap enough to call for the union of ids.
  ages <- tryCatch({
    people <- baseballr::mlb_people(person_ids = unique(roster$mlb_id))
    data.frame(mlb_id = as.integer(people$id),
               age = as.numeric(difftime(Sys.Date(),
                                         as.Date(people$birth_date),
                                         units = "days")) / 365.25,
               stringsAsFactors = FALSE)
  }, error = function(e) {
    warning("mlb_people failed: ", e$message); data.frame()
  })
  if (nrow(ages) > 0) roster <- left_join(roster, ages, by = "mlb_id")
  else roster$age <- NA_real_

  roster$snapshot_date <- today
  roster <- roster[, c("snapshot_date","mlb_id","fg_id","player","pos","level","team_id","age")]
  stats$snapshot_date  <- today

  # Idempotent upserts. SQLite REPLACE works because PK is (snapshot_date, mlb_id).
  dbExecute(conn, "DELETE FROM GuardiansRoster WHERE snapshot_date = ?", params = list(today))
  dbWriteTable(conn, "GuardiansRoster", roster, append = TRUE)
  dbExecute(conn, "DELETE FROM GuardiansStats  WHERE snapshot_date = ?", params = list(today))
  dbWriteTable(conn, "GuardiansStats", stats, append = TRUE)

  if (nrow(txns) > 0) {
    # Upsert by txn_id — delete then insert.
    placeholders <- paste(rep("?", nrow(txns)), collapse = ",")
    dbExecute(conn, paste0("DELETE FROM GuardiansTransactions WHERE txn_id IN (", placeholders, ")"),
              params = as.list(txns$txn_id))
    dbWriteTable(conn, "GuardiansTransactions", txns, append = TRUE)
  }

  # HotScore: pull game logs for players with fg_id, compute, upsert.
  eligible <- roster[!is.na(roster$fg_id) & nzchar(roster$fg_id), ]
  # Restrict to roughly active players to keep the daily load sane.
  # `role` is inferred from position (TWP/SP/RP/CL/MR/P → P; otherwise H).
  pitchPos <- c("P","SP","RP","CL","MR","TWP")
  eligible$role <- ifelse(eligible$pos %in% pitchPos, "P", "H")
  logsByPlayer <- list()
  for (i in seq_len(nrow(eligible))) {
    pid <- as.character(eligible$mlb_id[i])
    logsByPlayer[[pid]] <- pullGuardiansGameLogs(eligible$fg_id[i], eligible$role[i])
    Sys.sleep(0.2)  # gentle rate-limit
  }
  hs <- computeGuardiansHotscore(eligible[, c("mlb_id","fg_id","level","role")],
                                 logsByPlayer, windows = c(7, 14, 30))
  if (nrow(hs) > 0) {
    hs$snapshot_date <- today
    hs <- hs[, c("snapshot_date","mlb_id","level","role","window_days","hotscore")]
    dbExecute(conn, "DELETE FROM GuardiansHotscore WHERE snapshot_date = ?", params = list(today))
    dbWriteTable(conn, "GuardiansHotscore", hs, append = TRUE)
  }
} else {
  cat("[guardians] Today's snapshot present; skipping upstream\n")
}

# Hydrate globals for server.R. Always reads from DB so app start is uniform.
gRoster <- dbGetQuery(conn, "SELECT * FROM GuardiansRoster WHERE snapshot_date = ?",
                     params = list(today))
gStats  <- dbGetQuery(conn, "SELECT * FROM GuardiansStats  WHERE snapshot_date = ?",
                     params = list(today))
gHot    <- dbGetQuery(conn, "SELECT * FROM GuardiansHotscore WHERE snapshot_date = ?",
                     params = list(today))
gTxn    <- dbGetQuery(conn,
  "SELECT * FROM GuardiansTransactions WHERE txn_date >= date(?, '-14 days') ORDER BY txn_date DESC",
  params = list(today))
gTrend  <- dbGetQuery(conn, "SELECT * FROM GuardiansHotscore ORDER BY snapshot_date")

# Compute current IL board by walking the full transactions table forward.
allTxn <- dbGetQuery(conn, "SELECT * FROM GuardiansTransactions ORDER BY txn_date")
gIL <- if (nrow(allTxn) > 0) {
  ils <- allTxn[grepl("IL|Injured List|Disabled", allTxn$type, ignore.case = TRUE), ]
  acts <- allTxn[grepl("Activated", allTxn$type, ignore.case = TRUE), ]
  onIL <- ils[!ils$mlb_id %in% acts$mlb_id[acts$txn_date > ils$txn_date], ]
  onIL[, c("txn_date","mlb_id","player","type","description")]
} else {
  data.frame()
}

# Prospect FV + tool grades, filtered to Guardians org.
gProspects <- tryCatch({
  h <- getFGProspects(pos = "bat"); p <- getFGProspects(pos = "pit")
  bind_rows(
    h %>% filter(Org == "CLE") %>% mutate(role = "H"),
    p %>% filter(Org == "CLE") %>% mutate(role = "P")
  )
}, error = function(e) data.frame())

# FG MLB depth chart (if available — for the right pane of the Depth Chart tab).
gDepth <- tryCatch({
  baseballr::fg_team_depth_chart(team = "CLE")
}, error = function(e) data.frame())

dbDisconnect(conn)
cat("[guardians] Pulse complete: ", nrow(gRoster), " roster rows, ",
    nrow(gStats), " stat rows, ", nrow(gTxn), " txns (14d), ",
    nrow(gHot), " hot scores\n", sep = "")
```

- [ ] **Step 2: Run the pulse end-to-end (cold start)**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL/code
Rscript guardiansPulse.r 2>&1 | tail -20
```

Expected: last line reads `[guardians] Pulse complete: <N> roster rows, <M> stat rows, ...` with `N >= 30`.

- [ ] **Step 3: Re-run and verify it skips upstream**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL/code
Rscript guardiansPulse.r 2>&1 | grep -E "skipping upstream|complete"
```

Expected output (2 lines):
```
[guardians] Today's snapshot present; skipping upstream
[guardians] Pulse complete: <N> roster rows, ...
```

- [ ] **Step 4: Commit**

```bash
git add code/guardiansPulse.r
git commit -m "feat(guardians): daily pulse pipeline writes snapshots to DAFL.db"
```

---

## Task 9: launchd wrapper

**Files:**
- Create: `code/run_guardians_pulse.sh`

- [ ] **Step 1: Create the wrapper**

```bash
#!/bin/bash

# Script to run guardiansPulse.r via cron/launchd.
# Mirrors code/run_inseason_pulse.sh.

cd "/Users/cmcneilly/Dropbox/Personal/DAFL/code"

LOG_FILE="../logs/guardians_pulse_$(date +%Y%m%d_%H%M%S).log"
mkdir -p "../logs"

echo "=== Starting guardiansPulse.r at $(date) ===" >> "$LOG_FILE"

Rscript guardiansPulse.r >> "$LOG_FILE" 2>&1

if [ $? -eq 0 ]; then
    echo "=== guardiansPulse.r completed successfully at $(date) ===" >> "$LOG_FILE"
    exit 0
else
    echo "=== guardiansPulse.r failed at $(date) ===" >> "$LOG_FILE"
    exit 1
fi
```

- [ ] **Step 2: Make it executable and verify exit code**

```bash
chmod +x /Users/cmcneilly/Dropbox/Personal/DAFL/code/run_guardians_pulse.sh
/Users/cmcneilly/Dropbox/Personal/DAFL/code/run_guardians_pulse.sh
echo "exit=$?"
tail -3 /Users/cmcneilly/Dropbox/Personal/DAFL/logs/guardians_pulse_*.log 2>/dev/null | tail -3
```

Expected: `exit=0` and the last log line includes `completed successfully`.

- [ ] **Step 3: Commit**

```bash
git add code/run_guardians_pulse.sh
git commit -m "feat(guardians): add launchd wrapper for daily pulse"
```

---

## Task 10: Shiny app skeleton

**Files:**
- Create: `Guardians/ui.R`
- Create: `Guardians/server.R`

- [ ] **Step 1: Create `Guardians/ui.R`**

```r
library("shiny")
library("plotly")
library("bslib")
library("DT")

shinyUI(
  navbarPage(
    theme = bs_theme(bootswatch = "flatly"),
    "Cleveland Guardians Tracker",
    header = tagList(
      tags$div(style = "position:absolute; right:15px; top:8px; z-index:1000;",
        actionButton('gSettingsBtn', 'Settings', class = 'btn-default btn-sm')
      )
    ),
    tabPanel("Depth Chart",
      fluidRow(
        column(width = 8, h3("Org by Level"), uiOutput("gOrgTree")),
        column(width = 4, h3("MLB Depth Chart"), DT::dataTableOutput("gDepthChart"))
      )
    ),
    tabPanel("Hot / Cold",
      sidebarLayout(
        sidebarPanel(
          selectInput("gHotWindow", "Window (days)", choices = c(7, 14, 30), selected = 14),
          radioButtons("gHotRole", "Role", choices = c("Hitters" = "H", "Pitchers" = "P", "All" = "A"),
                       selected = "A", inline = TRUE),
          selectInput("gHotLevel", "Level", choices = c("All","MLB","AAA","AA","A+","A","ACL","DSL"),
                      selected = "All"),
          width = 3
        ),
        mainPanel(DT::dataTableOutput("gHotTable"), width = 9)
      )
    ),
    tabPanel("Player Detail",
      sidebarLayout(
        sidebarPanel(selectizeInput("gPlayerPick", "Search player",
                                    choices = NULL, options = list(placeholder = "type a name…")),
                     width = 3),
        mainPanel(uiOutput("gPlayerCard"), width = 9)
      )
    ),
    tabPanel("Risers & Transactions",
      h3("Risers"), DT::dataTableOutput("gRisers"),
      tags$hr(),
      h3("Recent Transactions (last 14 days)"), DT::dataTableOutput("gTxnTable"),
      tags$hr(),
      h3("Current IL"), DT::dataTableOutput("gILTable")
    )
  )
)
```

- [ ] **Step 2: Create `Guardians/server.R`**

```r
# Guardians Tracker — Shiny server. Sources the pulse script which hydrates
# globals (gRoster, gStats, gHot, gTxn, gIL, gProspects, gDepth, gTrend).

setwd("../code/")
source("./guardiansPulse.r")

library("dplyr")
library("DT")
library("plotly")

shinyServer(function(input, output, session) {

  rv <- reactiveValues(refreshCount = 0)

  # --- Settings modal: Refresh button ---
  observeEvent(input$gSettingsBtn, {
    showModal(modalDialog(
      title = "Settings",
      size = "s", easyClose = TRUE,
      tags$p(paste0("Latest snapshot: ", today)),
      actionButton('gRefreshBtn', 'Refresh Data',
                   class = 'btn-success btn-sm', icon = icon('refresh')),
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$gRefreshBtn, {
    removeModal()
    showNotification("Refreshing Guardians data…", type = "message",
                     duration = NULL, id = "gRefreshMsg")
    Sys.setenv(DAFL_FORCE_REFRESH = "1")
    on.exit(Sys.unsetenv("DAFL_FORCE_REFRESH"), add = TRUE)
    tryCatch({
      source("../code/guardiansPulse.r", local = globalenv())
      rv$refreshCount <- rv$refreshCount + 1
      removeNotification("gRefreshMsg")
      showNotification("Refreshed!", type = "message")
    }, error = function(e) {
      removeNotification("gRefreshMsg")
      showNotification(paste0("Refresh failed: ", e$message),
                       type = "error", duration = 15)
    })
  })

  # Placeholder outputs — populated by later tasks.
  output$gOrgTree     <- renderUI({ tags$div("Depth chart coming soon.") })
  output$gDepthChart  <- DT::renderDataTable({ datatable(data.frame()) })
  output$gHotTable    <- DT::renderDataTable({ datatable(data.frame()) })
  output$gPlayerCard  <- renderUI({ tags$div("Pick a player.") })
  output$gRisers      <- DT::renderDataTable({ datatable(data.frame()) })
  output$gTxnTable    <- DT::renderDataTable({ datatable(data.frame()) })
  output$gILTable     <- DT::renderDataTable({ datatable(data.frame()) })

  # Populate the player picker once the pulse globals are available.
  updateSelectizeInput(session, 'gPlayerPick',
                       choices = sort(unique(gRoster$player)),
                       server = TRUE)
})
```

- [ ] **Step 3: Launch the app and confirm tabs render**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL/Guardians
Rscript -e 'shiny::runApp(launch.browser = FALSE, port = 7321)' &
APP_PID=$!
sleep 8
curl -sf http://127.0.0.1:7321/ > /tmp/g-index.html && grep -c "Cleveland Guardians Tracker" /tmp/g-index.html
kill $APP_PID 2>/dev/null
```

Expected: a non-zero count (the page title appears in the HTML).

- [ ] **Step 4: Commit**

```bash
git add Guardians/ui.R Guardians/server.R
git commit -m "feat(guardians): Shiny app skeleton with 4 tabs + settings modal"
```

---

## Task 11: Depth Chart tab

**Files:**
- Modify: `Guardians/server.R` (replace `output$gOrgTree` and `output$gDepthChart` placeholders)

- [ ] **Step 1: Replace the `output$gOrgTree` placeholder in `Guardians/server.R`**

Find the line:

```r
  output$gOrgTree     <- renderUI({ tags$div("Depth chart coming soon.") })
```

Replace with:

```r
  # One card per level. Inside each card, players sorted by position and one-
  # line stat. ⬆/⬇ badge if the player's level changed in the last 7 days.
  output$gOrgTree <- renderUI({
    rv$refreshCount
    if (nrow(gRoster) == 0) {
      return(tags$div(style = "color:#888; font-style:italic;",
                      "No roster snapshot available."))
    }
    levels <- c("MLB","AAA","AA","A+","A","ACL","DSL")
    statByPid <- setNames(split(gStats, gStats$mlb_id), nm = NULL)

    # Recent level changes (last 7 days) — query DB through the global trend
    # snapshot is not enough; do a fresh query through the conn-less helper.
    recentMoves <- tryCatch({
      conn2 <- dbConnect(RSQLite::SQLite(), "../code/DAFL.db")
      on.exit(dbDisconnect(conn2))
      dbGetQuery(conn2, "
        SELECT mlb_id, level, snapshot_date FROM GuardiansRoster
        WHERE snapshot_date >= date(?, '-7 days')
        ORDER BY mlb_id, snapshot_date",
        params = list(as.character(Sys.Date())))
    }, error = function(e) data.frame())
    moveBadge <- function(pid) {
      if (nrow(recentMoves) == 0) return("")
      hist <- recentMoves[recentMoves$mlb_id == pid, ]
      if (nrow(hist) < 2) return("")
      levOrder <- c("DSL"=1,"ACL"=2,"A"=3,"A+"=4,"AA"=5,"AAA"=6,"MLB"=7)
      first <- levOrder[hist$level[1]]; last <- levOrder[hist$level[nrow(hist)]]
      if (is.na(first) || is.na(last) || first == last) return("")
      if (last > first) " ⬆" else " ⬇"
    }

    levelCard <- function(lvl) {
      sub <- gRoster[gRoster$level == lvl, ]
      if (nrow(sub) == 0) {
        return(tags$div(class = "card", style = "margin-bottom:10px; padding:8px; border:1px solid #ddd; border-radius:4px;",
                        tags$h4(lvl), tags$div(style="color:#888;","No roster.")))
      }
      sub <- sub %>% arrange(pos, player)
      rows <- lapply(seq_len(nrow(sub)), function(i) {
        pid <- sub$mlb_id[i]
        st <- gStats[gStats$mlb_id == pid, ]
        line <- if (nrow(st) > 0) {
          if (st$role[1] == "H" && !is.na(st$avg[1])) {
            sprintf(" — .%s / %d HR / %.3f OBP",
                    sub("^0\\.", "", sprintf("%.3f", st$avg[1])),
                    ifelse(is.na(st$hr[1]), 0, as.integer(st$hr[1])),
                    ifelse(is.na(st$obp[1]), 0, st$obp[1]))
          } else if (st$role[1] == "P" && !is.na(st$era[1])) {
            sprintf(" — %.2f ERA / %.1f K/9 / %.2f WHIP",
                    st$era[1],
                    ifelse(is.na(st$k9[1]), 0, st$k9[1]),
                    ifelse(is.na(st$whip[1]), 0, st$whip[1]))
          } else ""
        } else ""
        tags$div(style = "font-size:13px; padding:2px 0;",
                 tags$strong(sub$player[i]),
                 tags$span(style="color:#888;", paste0(" (", sub$pos[i], ")")),
                 tags$span(line),
                 tags$span(style="color:#27ae60;", moveBadge(pid)))
      })
      tags$div(class = "card", style = "margin-bottom:10px; padding:8px; border:1px solid #ddd; border-radius:4px;",
               tags$h4(paste0(lvl, " (", nrow(sub), ")")),
               do.call(tagList, rows))
    }

    do.call(tagList, lapply(levels, levelCard))
  })
```

- [ ] **Step 2: Replace `output$gDepthChart` placeholder**

Find:

```r
  output$gDepthChart  <- DT::renderDataTable({ datatable(data.frame()) })
```

Replace with:

```r
  output$gDepthChart <- DT::renderDataTable({
    rv$refreshCount
    if (!is.data.frame(gDepth) || nrow(gDepth) == 0) {
      return(datatable(data.frame(Note = "FG depth chart not available"),
                       options = list(dom = 't'), selection = 'none', rownames = FALSE))
    }
    datatable(gDepth,
              options = list(pageLength = 30, dom = 't', autoWidth = FALSE),
              rownames = FALSE)
  })
```

- [ ] **Step 3: Launch and visually confirm**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL/Guardians
Rscript -e 'shiny::runApp(launch.browser = FALSE, port = 7321)' &
APP_PID=$!
sleep 8
curl -sf http://127.0.0.1:7321/ > /tmp/g-index.html
grep -c "MLB" /tmp/g-index.html
kill $APP_PID 2>/dev/null
```

Expected: count >= 1 (the "MLB" level card renders into the HTML). For a thorough check, the user can open the URL in a browser and inspect the cards.

- [ ] **Step 4: Commit**

```bash
git add Guardians/server.R
git commit -m "feat(guardians): depth chart tab with per-level cards + MLB FG panel"
```

---

## Task 12: Hot/Cold tab

**Files:**
- Modify: `Guardians/server.R` (replace `output$gHotTable` placeholder)

- [ ] **Step 1: Replace `output$gHotTable` placeholder**

Find:

```r
  output$gHotTable    <- DT::renderDataTable({ datatable(data.frame()) })
```

Replace with:

```r
  output$gHotTable <- DT::renderDataTable({
    rv$refreshCount
    req(input$gHotWindow, input$gHotRole, input$gHotLevel)
    win <- as.integer(input$gHotWindow)
    df <- gHot %>% filter(window_days == win)
    if (input$gHotRole != "A") df <- df %>% filter(role == input$gHotRole)
    if (input$gHotLevel != "All") df <- df %>% filter(level == input$gHotLevel)
    if (nrow(df) == 0) {
      return(datatable(data.frame(Note = "No hot/cold data for this filter"),
                       options = list(dom = 't'), selection = 'none', rownames = FALSE))
    }
    # Join in player name + a short window-line from gStats for context.
    df <- df %>%
      left_join(gRoster %>% select(mlb_id, player, pos, age), by = "mlb_id") %>%
      left_join(gStats  %>% select(mlb_id, avg, hr, obp, era, k9, whip),
                by = "mlb_id") %>%
      mutate(WindowLine = ifelse(role == "H",
                  sprintf(".%s / %d HR / %.3f OBP",
                          sub("^0\\.", "", sprintf("%.3f", ifelse(is.na(avg), 0, avg))),
                          ifelse(is.na(hr), 0, as.integer(hr)),
                          ifelse(is.na(obp), 0, obp)),
                  sprintf("%.2f ERA / %.1f K/9 / %.2f WHIP",
                          ifelse(is.na(era), 0, era),
                          ifelse(is.na(k9), 0, k9),
                          ifelse(is.na(whip), 0, whip)))) %>%
      arrange(desc(hotscore)) %>%
      select(Player = player, Lvl = level, Pos = pos, Age = age,
             `Window line` = WindowLine, HotScore = hotscore)

    datatable(df,
              options = list(pageLength = 25, autoWidth = FALSE),
              rownames = FALSE) %>%
      formatRound("HotScore", 2) %>%
      formatRound("Age", 0) %>%
      formatStyle("HotScore",
                  backgroundColor = styleInterval(c(-0.5, 0.5),
                                                  c("#f8d7da", "#ffffff", "#d4edda")))
  })
```

- [ ] **Step 2: Launch and verify the tab renders**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL/Guardians
Rscript -e 'shiny::runApp(launch.browser = FALSE, port = 7321)' &
APP_PID=$!
sleep 8
curl -sf 'http://127.0.0.1:7321/?_inputs_&gHotWindow=14&gHotRole=A&gHotLevel=All' >/tmp/g-hot.html
grep -c "HotScore" /tmp/g-hot.html
kill $APP_PID 2>/dev/null
```

Expected: count >= 1 (the table header serialises into the HTML).

- [ ] **Step 3: Commit**

```bash
git add Guardians/server.R
git commit -m "feat(guardians): hot/cold board with window + level filters"
```

---

## Task 13: Player Detail tab

**Files:**
- Modify: `Guardians/server.R` (replace `output$gPlayerCard` placeholder)

- [ ] **Step 1: Replace `output$gPlayerCard` placeholder**

Find:

```r
  output$gPlayerCard  <- renderUI({ tags$div("Pick a player.") })
```

Replace with:

```r
  output$gPlayerCard <- renderUI({
    rv$refreshCount
    nm <- input$gPlayerPick
    if (is.null(nm) || nm == "") {
      return(tags$div(style = "color:#888; padding:12px; font-style:italic;",
                      "Type a name in the search box to see player details."))
    }
    row <- gRoster %>% filter(player == nm)
    if (nrow(row) == 0) return(tags$div("Player not found in current roster."))
    row <- row[1, ]
    st <- gStats %>% filter(mlb_id == row$mlb_id)
    pros <- gProspects %>% filter(Name == nm)

    # Header
    headerUI <- tags$div(
      style = "padding:12px 16px; background:#0F223E; color:white; border-radius:6px 6px 0 0;",
      tags$div(style = "font-size:20px; font-weight:bold;", row$player),
      tags$div(style = "font-size:14px; margin-top:4px; color:#bdc3c7;",
               paste0(row$pos, "  |  ", row$level, "  |  Age ",
                      ifelse(is.na(row$age), "?", round(row$age, 1)),
                      if (nrow(pros) > 0 && "FV" %in% names(pros) && !is.na(pros$FV[1]))
                        paste0("  |  FV ", pros$FV[1]) else "",
                      if (nrow(pros) > 0 && "Top.100" %in% names(pros) && !is.na(pros$Top.100[1]))
                        paste0("  |  #", pros$Top.100[1], " overall") else ""))
    )

    # Hero line — current season slash or pitching summary
    heroUI <- if (nrow(st) > 0 && st$role[1] == "H") {
      tags$div(style = "padding:12px 16px; background:#f8f9fa; border:1px solid #ddd; border-top:none; font-size:18px;",
        tags$strong(sprintf(".%s / .%s / .%s",
              sub("^0\\.", "", sprintf("%.3f", ifelse(is.na(st$avg[1]), 0, st$avg[1]))),
              sub("^0\\.", "", sprintf("%.3f", ifelse(is.na(st$obp[1]), 0, st$obp[1]))),
              sub("^0\\.", "", sprintf("%.3f", ifelse(is.na(st$slg[1]), 0, st$slg[1]))))),
        tags$span(style = "margin-left:16px; color:#666; font-size:14px;",
                  sprintf("%d HR · %d RBI · %d R · %d SB",
                          ifelse(is.na(st$hr[1]), 0, as.integer(st$hr[1])),
                          ifelse(is.na(st$rbi[1]), 0, as.integer(st$rbi[1])),
                          ifelse(is.na(st$r[1]), 0, as.integer(st$r[1])),
                          ifelse(is.na(st$sb[1]), 0, as.integer(st$sb[1])))))
    } else if (nrow(st) > 0 && st$role[1] == "P") {
      tags$div(style = "padding:12px 16px; background:#f8f9fa; border:1px solid #ddd; border-top:none; font-size:18px;",
        tags$strong(sprintf("%.2f ERA · %.2f WHIP · %.1f K/9",
              ifelse(is.na(st$era[1]),  0, st$era[1]),
              ifelse(is.na(st$whip[1]), 0, st$whip[1]),
              ifelse(is.na(st$k9[1]),   0, st$k9[1]))),
        tags$span(style = "margin-left:16px; color:#666; font-size:14px;",
                  sprintf("%d W · %d SV · %d HLD · %d K",
                          ifelse(is.na(st$w[1]), 0, as.integer(st$w[1])),
                          ifelse(is.na(st$sv[1]), 0, as.integer(st$sv[1])),
                          ifelse(is.na(st$hld[1]), 0, as.integer(st$hld[1])),
                          ifelse(is.na(st$so[1]), 0, as.integer(st$so[1])))))
    } else {
      tags$div(style = "padding:12px 16px; background:#f8f9fa; border:1px solid #ddd; border-top:none; color:#888;",
              "No season stats yet for this player.")
    }

    # Trend plot — HotScore over snapshot_date (14d window) from gTrend
    trendDf <- gTrend %>%
      filter(mlb_id == row$mlb_id, window_days == 14) %>%
      mutate(snapshot_date = as.Date(snapshot_date)) %>%
      arrange(snapshot_date)
    trendUI <- if (nrow(trendDf) >= 5) {
      tags$div(style = "padding:12px 16px; border:1px solid #ddd; border-top:none;",
        tags$strong("HotScore trend (14-day window)"),
        plotly::plotlyOutput(session$ns("gPlayerTrend"), height = 220))
    } else {
      tags$div(style = "padding:12px 16px; border:1px solid #ddd; border-top:none; color:#888; font-size:13px;",
        "Not enough history yet for a trend chart (need 5+ daily snapshots).")
    }

    tags$div(style = "border-radius:6px; overflow:hidden;",
             headerUI, heroUI, trendUI)
  })

  # Trend plot output — paired with the renderUI above. plotly works when the
  # output is referenced inside a UI that's already on the page; renderUI
  # registers the placeholder div, and renderPlotly fills it.
  output$gPlayerTrend <- plotly::renderPlotly({
    nm <- input$gPlayerPick
    if (is.null(nm) || nm == "") return(NULL)
    row <- gRoster %>% filter(player == nm)
    if (nrow(row) == 0) return(NULL)
    trendDf <- gTrend %>%
      filter(mlb_id == row$mlb_id[1], window_days == 14) %>%
      mutate(snapshot_date = as.Date(snapshot_date)) %>%
      arrange(snapshot_date)
    if (nrow(trendDf) < 5) return(NULL)
    plotly::plot_ly(trendDf, x = ~snapshot_date, y = ~hotscore,
                    type = "scatter", mode = "lines+markers",
                    line = list(width = 3), marker = list(size = 8))
  })
```

Note: the `session$ns()` wrapping is harmless here (root namespace) and keeps the pattern reusable if the UI is later modularised.

- [ ] **Step 2: Launch and confirm a known player renders**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL/Guardians
Rscript -e 'shiny::runApp(launch.browser = FALSE, port = 7321)' &
APP_PID=$!
sleep 8
curl -sf 'http://127.0.0.1:7321/?gPlayerPick=José+Ramírez' > /tmp/g-player.html
grep -c "Ramírez\|Ramirez" /tmp/g-player.html
kill $APP_PID 2>/dev/null
```

Expected: count >= 1 (the player's name appears on the page). If José Ramírez is no longer on the roster when this runs, substitute another MLB Guardian (e.g. Steven Kwan).

- [ ] **Step 3: Commit**

```bash
git add Guardians/server.R
git commit -m "feat(guardians): player detail tab with header / hero line / trend"
```

---

## Task 14: Risers & Transactions tab

**Files:**
- Modify: `Guardians/server.R` (replace `output$gRisers`, `output$gTxnTable`, `output$gILTable` placeholders)

- [ ] **Step 1: Replace `output$gRisers`**

Find:

```r
  output$gRisers      <- DT::renderDataTable({ datatable(data.frame()) })
```

Replace with:

```r
  # Risers: players whose 14d HotScore has been > 0 for 3+ consecutive
  # snapshots, OR whose level moved up in the last 7 days.
  output$gRisers <- DT::renderDataTable({
    rv$refreshCount
    if (nrow(gTrend) == 0) {
      return(datatable(data.frame(Note = "Not enough history yet for risers."),
                       options = list(dom = 't'), selection = 'none', rownames = FALSE))
    }
    # Streaks on 14d HotScore
    t14 <- gTrend %>%
      filter(window_days == 14) %>%
      mutate(snapshot_date = as.Date(snapshot_date)) %>%
      arrange(mlb_id, snapshot_date)
    streaks <- t14 %>%
      group_by(mlb_id) %>%
      summarise(latest = tail(snapshot_date, 1),
                streak = {
                  s <- rev(hotscore > 0)
                  rl <- rle(s)
                  if (length(rl$values) == 0 || !isTRUE(rl$values[1])) 0L
                  else as.integer(rl$lengths[1])
                },
                .groups = "drop") %>%
      filter(streak >= 3, latest >= Sys.Date() - 1) %>%
      left_join(gRoster %>% select(mlb_id, player, pos, level), by = "mlb_id") %>%
      mutate(reason = paste0(streak, " consecutive positive HotScores")) %>%
      select(Player = player, Pos = pos, Lvl = level, Reason = reason)

    # Promotions in the last 7 days (level went up)
    promos <- tryCatch({
      conn3 <- dbConnect(RSQLite::SQLite(), "../code/DAFL.db")
      on.exit(dbDisconnect(conn3))
      hist <- dbGetQuery(conn3, "
        SELECT mlb_id, player, level, snapshot_date FROM GuardiansRoster
        WHERE snapshot_date >= date(?, '-7 days')
        ORDER BY mlb_id, snapshot_date",
        params = list(as.character(Sys.Date())))
      levOrder <- c("DSL"=1,"ACL"=2,"A"=3,"A+"=4,"AA"=5,"AAA"=6,"MLB"=7)
      hist %>%
        group_by(mlb_id) %>%
        summarise(player = tail(player, 1),
                  from = head(level, 1), to = tail(level, 1),
                  .groups = "drop") %>%
        filter(!is.na(levOrder[to]), !is.na(levOrder[from]),
               levOrder[to] > levOrder[from]) %>%
        mutate(Player = player, Pos = NA_character_, Lvl = to,
               Reason = paste0("Promoted ", from, " → ", to)) %>%
        select(Player, Pos, Lvl, Reason)
    }, error = function(e) data.frame(Player = character(), Pos = character(),
                                       Lvl = character(), Reason = character()))

    out <- bind_rows(streaks, promos) %>%
      distinct(Player, .keep_all = TRUE)
    if (nrow(out) == 0) {
      return(datatable(data.frame(Note = "No risers right now."),
                       options = list(dom = 't'), selection = 'none', rownames = FALSE))
    }
    datatable(out, options = list(pageLength = 15, dom = 'tip', autoWidth = FALSE),
              rownames = FALSE)
  })
```

- [ ] **Step 2: Replace `output$gTxnTable`**

Find:

```r
  output$gTxnTable    <- DT::renderDataTable({ datatable(data.frame()) })
```

Replace with:

```r
  output$gTxnTable <- DT::renderDataTable({
    rv$refreshCount
    if (nrow(gTxn) == 0) {
      return(datatable(data.frame(Note = "No transactions in the last 14 days."),
                       options = list(dom = 't'), selection = 'none', rownames = FALSE))
    }
    df <- gTxn %>%
      select(Date = txn_date, Player = player, Type = type,
             From = from_team_id, To = to_team_id, Description = description)
    datatable(df,
              options = list(pageLength = 25, filter = 'top', autoWidth = FALSE),
              filter = 'top', rownames = FALSE)
  })
```

- [ ] **Step 3: Replace `output$gILTable`**

Find:

```r
  output$gILTable     <- DT::renderDataTable({ datatable(data.frame()) })
```

Replace with:

```r
  output$gILTable <- DT::renderDataTable({
    rv$refreshCount
    if (nrow(gIL) == 0) {
      return(datatable(data.frame(Note = "No active IL placements."),
                       options = list(dom = 't'), selection = 'none', rownames = FALSE))
    }
    df <- gIL %>%
      select(Date = txn_date, Player = player, Type = type, Notes = description)
    datatable(df,
              options = list(pageLength = 15, autoWidth = FALSE),
              rownames = FALSE)
  })
```

- [ ] **Step 4: Launch and verify the tab renders without errors**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL/Guardians
Rscript -e 'shiny::runApp(launch.browser = FALSE, port = 7321)' &
APP_PID=$!
sleep 8
curl -sf http://127.0.0.1:7321/ > /tmp/g-rt.html
grep -c "Risers\|Transactions\|IL" /tmp/g-rt.html
kill $APP_PID 2>/dev/null
```

Expected: count >= 3 (the three section headers appear in the HTML).

- [ ] **Step 5: Commit**

```bash
git add Guardians/server.R
git commit -m "feat(guardians): risers, transactions, and IL board"
```

---

## Task 15: README / how to launch + add to launchd

**Files:**
- Modify: `code/run_data_loads.sh` (only if pulse should run from the daily loader; otherwise skip)
- Create: `Guardians/README.md`

- [ ] **Step 1: Create `Guardians/README.md`**

```markdown
# Cleveland Guardians Tracker

Personal Shiny app that follows the entire Cleveland Guardians organization
(MLB through DSL).

## Launch

```bash
cd Guardians
Rscript -e 'shiny::runApp(launch.browser = TRUE)'
```

## Tabs

- **Depth Chart** — one card per level (MLB / AAA / AA / A+ / A / ACL / DSL)
  with each player's season stat line, plus the FanGraphs MLB depth chart on
  the right.
- **Hot / Cold** — z-scored leaderboard within each level. Window: 7 / 14 / 30
  days. Filterable by role and level.
- **Player Detail** — full season line plus a HotScore trend chart (once 5+
  daily snapshots have accumulated).
- **Risers & Transactions** — players on positive HotScore streaks or recent
  promotions; last-14d transactions; current IL board.

## Data

Sources:
- `baseballr::mlb_team_roster`, `mlb_stats`, `mlb_transactions`, `mlb_people`
- `baseballr::fg_milb_batter_game_logs`, `fg_milb_pitcher_game_logs`
- `baseballr::fg_team_depth_chart`
- `getFGProspects()` (existing) for FV grades

Daily snapshots are written to the 4 tables in `code/DAFL.db`. A Refresh
button in the Settings modal forces a re-pull.

## Scheduled refresh

`code/run_guardians_pulse.sh` is the launchd wrapper. Add an entry to
`~/Library/LaunchAgents` to run it daily (e.g., 6am) alongside the existing
`run_inseason_pulse.sh` schedule.

## Design / plan

- Spec: `docs/superpowers/specs/2026-05-20-guardians-tracker-design.md`
- Plan: `docs/superpowers/plans/2026-05-20-guardians-tracker.md`
```

- [ ] **Step 2: Commit**

```bash
git add Guardians/README.md
git commit -m "docs(guardians): README with launch instructions and tab summary"
```

---

## Final verification

- [ ] **Step 1: Run the pulse cold-start one more time and confirm**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL/code
sqlite3 DAFL.db "DELETE FROM GuardiansRoster WHERE snapshot_date = date('now'); DELETE FROM GuardiansStats WHERE snapshot_date = date('now'); DELETE FROM GuardiansHotscore WHERE snapshot_date = date('now');"
Rscript guardiansPulse.r 2>&1 | tail -3
```

Expected: `[guardians] Pulse complete: ...` with non-zero counts.

- [ ] **Step 2: Launch the app for manual smoke test**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL/Guardians
Rscript -e 'shiny::runApp(launch.browser = TRUE)'
```

Manual checks (open in browser):
1. Depth Chart tab loads with one card per level.
2. Hot / Cold tab loads, change Window to 7 — table updates.
3. Player Detail — type a name, header + hero line render.
4. Risers & Transactions — all three sections show either content or "no data" messages.
5. Settings → Refresh Data — completes without error.

## Self-Review

**Spec coverage check:**
- Goal (org tracking app) → Tasks 8–14 ✓
- Architecture/file layout → Tasks 1–10 ✓
- MLB Stats API sources → Tasks 3–5 ✓
- FanGraphs game logs → Task 6 ✓
- FV grades + depth chart → Task 8 (hydrates `gProspects`, `gDepth`), Task 13 (renders FV in header) ✓
- SQLite schema → Task 1 ✓
- Pipeline order → Task 8 ✓
- HotScore z-scored within level → Task 7 ✓
- All 4 UI tabs → Tasks 11–14 ✓
- Settings modal + Refresh → Task 10 ✓
- Daily launchd → Task 9 ✓
- Error handling (tryCatch on each upstream) → Tasks 3–8 ✓
- Edge cases (mid-season trades, empty DSL, spring training) → handled by snapshot model and the "no data" fallback panels in Tasks 11–14 ✓

**Type/name consistency check:** the column names used in DB writes (Task 1 schema, Task 8 writes) match the column names used in DB reads (Task 8 hydrates `gRoster`, `gStats`, etc.) and the columns referenced by all four UI tabs (`player`, `level`, `pos`, `mlb_id`, `age`, `hr`, `avg`, `obp`, `slg`, `era`, `k9`, `whip`, `hotscore`, `window_days`, `role`, `txn_date`, `from_team_id`, `to_team_id`). All consistent.

**Placeholder scan:** no TODO/TBD; every code step shows complete code; every verification step has a concrete command and expected output.
