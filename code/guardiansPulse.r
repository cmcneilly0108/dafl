# Guardians Tracker — daily data pipeline. See:
#   docs/superpowers/specs/2026-05-20-guardians-tracker-design.md
#
# Reads/writes code/DAFL.db (tables GuardiansRoster/Stats/Transactions/Hotscore).
# Hydrates globals (gRoster, gStats, gHot, gTxn, gIL, gProspects, gDepth, gTrend) for
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

  # Level rank used for dedup'ing players who appear on multiple affiliate rosters.
  levelOrder <- c("MLB" = 1L, "AAA" = 2L, "AA" = 3L, "A+" = 4L, "A" = 5L, "ACL" = 6L, "DSL" = 7L)

  affiliates <- resolveGuardiansAffiliates()
  roster <- pullGuardiansRoster(affiliates)
  stats  <- pullGuardiansStats(affiliates)
  txns   <- pullGuardiansTransactions(affiliates)

  # Attach fg_id via Chadwick crosswalk (key_mlbam → key_fangraphs).
  # getFGProspects does not carry an MLBAM column, so we build the mapping
  # from the Chadwick register which has both key_mlbam and key_fangraphs.
  fgMap <- tryCatch({
    lu <- baseballr::get_chadwick_lu()
    lu <- lu[!is.na(lu$key_mlbam) & !is.na(lu$key_fangraphs) &
               nzchar(as.character(lu$key_fangraphs)), ]
    df <- data.frame(mlb_id = as.integer(lu$key_mlbam),
                     fg_id  = as.character(lu$key_fangraphs),
                     stringsAsFactors = FALSE)
    # Keep only the roster players to avoid inflating the join.
    df <- df[df$mlb_id %in% roster$mlb_id, ]
    # Deduplicate: one fg_id per mlb_id (keep first).
    df[!duplicated(df$mlb_id), ]
  }, error = function(e) {
    warning("Chadwick crosswalk failed: ", e$message); data.frame()
  })
  if (nrow(fgMap) > 0) {
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

  # Players on multiple affiliate rosters appear more than once.
  # Keep the highest-level assignment per player so the PK (snapshot_date, mlb_id)
  # stays unique. Level order: MLB > AAA > AA > A+ > A > ACL > DSL.
  roster$levelRank <- levelOrder[roster$level]
  roster$levelRank[is.na(roster$levelRank)] <- 99L
  roster <- roster[order(roster$mlb_id, roster$levelRank), ]
  roster <- roster[!duplicated(roster$mlb_id), ]
  roster$levelRank <- NULL

  roster <- roster[, c("snapshot_date","mlb_id","fg_id","player","pos","level","team_id","age")]
  stats$snapshot_date  <- today

  # Idempotent upserts. SQLite REPLACE works because PK is (snapshot_date, mlb_id).
  dbExecute(conn, "DELETE FROM GuardiansRoster WHERE snapshot_date = ?", params = list(today))
  dbWriteTable(conn, "GuardiansRoster", roster, append = TRUE)
  # Stats may have duplicate mlb_id rows (player on multiple levels, or
  # position players who pitched mop-up innings). PK is (snapshot_date, mlb_id),
  # so dedup. Prefer H (hitter) by default — position players whose only "P"
  # row is a token mop-up inning would otherwise overwrite their real batting
  # line. For pure pitchers, the "H" row's pa/ab will be NA/0, so fall back
  # to the P row in that case.
  stats$rolePref <- ifelse(stats$role == "H" &
                             (is.na(stats$pa) | stats$pa == 0L), 3L,
                           ifelse(stats$role == "H", 1L, 2L))
  stats$levelRank <- levelOrder[stats$level]
  stats$levelRank[is.na(stats$levelRank)] <- 99L
  stats <- stats[order(stats$mlb_id, stats$levelRank, stats$rolePref), ]
  stats <- stats[!duplicated(stats$mlb_id), ]
  stats$levelRank <- NULL; stats$rolePref <- NULL

  dbExecute(conn, "DELETE FROM GuardiansStats  WHERE snapshot_date = ?", params = list(today))
  dbWriteTable(conn, "GuardiansStats", stats, append = TRUE)

  if (nrow(txns) > 0) {
    # The raw transactions feed can return the same txn_id more than once
    # (e.g., trades involving multiple players share a txn_id). Keep the first
    # occurrence, then upsert by txn_id — delete then insert.
    txns <- txns[!duplicated(txns$txn_id), ]
    placeholders <- paste(rep("?", nrow(txns)), collapse = ",")
    dbExecute(conn, paste0("DELETE FROM GuardiansTransactions WHERE txn_id IN (", placeholders, ")"),
              params = as.list(txns$txn_id))
    dbWriteTable(conn, "GuardiansTransactions", txns, append = TRUE)
  }

  # League-wide HotScore: pull league season stats per level, z-score each
  # Guardian against the full cohort at their level. Drops the rolling-window
  # concept — single season-to-date hotscore per (player, level).
  leagueStats <- tryCatch(pullLeagueStats(affiliates),
                          error = function(e) {
                            warning("pullLeagueStats failed: ", e$message)
                            list(H = NULL, P = NULL)
                          })

  # Roster -> single role assignment per player (pos infers H vs P).
  pitchPos <- c("P","SP","RP","CL","MR","TWP")
  rosterForHS <- roster
  rosterForHS$role <- ifelse(rosterForHS$pos %in% pitchPos, "P", "H")
  hs <- computeGuardiansHotscore(rosterForHS[, c("mlb_id","level","role")],
                                 leagueStats)
  if (nrow(hs) > 0) {
    hs$snapshot_date <- today
    hs <- hs[, c("snapshot_date","mlb_id","level","role","hotscore")]
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
  # IL placements come through as type="Status Change" — search descriptions.
  # Match "injured list" / "IL" / "Disabled List" with a phrase that implies
  # *placement* (not transfer or activation).
  ils  <- allTxn[grepl("placed.*(injured list|disabled list|IL\\b)|transferred.*injured list",
                        allTxn$description, ignore.case = TRUE), ]
  acts <- allTxn[grepl("activated.*(injured list|disabled list|IL\\b)|reinstated",
                        allTxn$description, ignore.case = TRUE), ]
  if (nrow(ils) == 0) {
    data.frame(txn_date = character(0), mlb_id = integer(0),
               player = character(0), type = character(0),
               description = character(0), stringsAsFactors = FALSE)
  } else {
    # Per-player: still on IL iff most recent IL placement is AFTER most recent activation.
    latestIL  <- tapply(ils$txn_date,  ils$mlb_id, max)
    latestAct <- tapply(acts$txn_date, acts$mlb_id, max)
    matched   <- latestAct[names(latestIL)]
    stillOn   <- names(latestIL)[is.na(matched) | latestIL > matched]
    stillIds  <- suppressWarnings(as.integer(stillOn))
    onIL <- ils[ils$mlb_id %in% stillIds, , drop = FALSE]
    onIL <- onIL[order(onIL$mlb_id, onIL$txn_date, decreasing = TRUE), , drop = FALSE]
    onIL <- onIL[!duplicated(onIL$mlb_id), , drop = FALSE]
    onIL[, c("txn_date","mlb_id","player","type","description")]
  }
} else {
  data.frame(txn_date = character(0), mlb_id = integer(0),
             player = character(0), type = character(0),
             description = character(0), stringsAsFactors = FALSE)
}

# Prospect FV + tool grades, filtered to Guardians org.
# On any failure, return a typed empty data frame so server.R's
# `gProspects %>% filter(Name == ...)` doesn't crash with "object 'Name' not found".
gProspects <- tryCatch({
  h <- getFGProspects(pos = "bat"); p <- getFGProspects(pos = "pit")
  if (is.null(h) && is.null(p)) stop("both prospects pulls returned NULL")
  bind_rows(
    if (!is.null(h)) h %>% filter(Org == "CLE") %>% mutate(role = "H") else NULL,
    if (!is.null(p)) p %>% filter(Org == "CLE") %>% mutate(role = "P") else NULL
  )
}, error = function(e) {
  warning("gProspects failed: ", e$message)
  data.frame(Name = character(0), FV = character(0),
             Top.100 = integer(0), role = character(0),
             stringsAsFactors = FALSE)
})

# FG MLB depth chart — not exposed by baseballr 1.6.0. Left as an empty
# data frame placeholder; the UI degrades gracefully ("not available").
# Future: implement via direct FG scraping if desired.
gDepth <- data.frame()

dbDisconnect(conn)
cat("[guardians] Pulse complete: ", nrow(gRoster), " roster rows, ",
    nrow(gStats), " stat rows, ", nrow(gTxn), " txns (14d), ",
    nrow(gHot), " hot scores\n", sep = "")
