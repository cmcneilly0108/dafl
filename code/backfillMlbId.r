#!/usr/bin/env Rscript
# One-time backfill: add an `mlb_id` column to mymaster.csv via the Chadwick
# crosswalk, so Baseball Savant player links work downstream. Numeric FanGraphs
# playerids map to an MLBAM id; "sa"/"cbs" synthetic ids stay NA.
#
# Run from code/:  Rscript backfillMlbId.r
# Going forward, buildMaster.Rmd maintains the column on every rebuild.

suppressMessages(library(dplyr))
source("./daflFunctions.r")   # provides fgToMlbCrosswalk()

# Back up first (repo convention: mymaster.csv.bak-<timestamp>).
stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
bak <- paste0("../mymaster.csv.bak-", stamp, "-pre-mlbid")
file.copy("../mymaster.csv", bak, overwrite = FALSE)
cat("Backed up mymaster.csv ->", bak, "\n")

# Read raw, dropping the leading write.csv row-index column so the round-trip
# matches buildMaster's format (write.csv re-adds a fresh index).
m <- read.csv("../mymaster.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
if ("X" %in% names(m)) m$X <- NULL

cat("Fetching Chadwick crosswalk (this can take a minute)...\n")
xw <- fgToMlbCrosswalk()
m$mlb_id <- xw$mlb_id[match(as.character(m$playerid), xw$fg_id)]

write.csv(m, "../mymaster.csv")

n <- sum(!is.na(m$mlb_id)); tot <- nrow(m)
cat(sprintf("mlb_id backfilled: %d of %d rows (%.1f%%)\n", n, tot, 100 * n / tot))
