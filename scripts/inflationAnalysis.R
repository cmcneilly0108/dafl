library(dplyr)
library(stringr)
library(jsonlite)

parseDraftHitters <- function(filepath, year) {
  lines <- readLines(filepath, warn=FALSE)
  results <- list()
  section <- NA
  for (line in lines) {
    fields <- strsplit(line, ",")[[1]]
    if (length(fields) == 0) next
    first <- trimws(fields[1])
    if (length(fields) >= 2 && trimws(fields[2]) == "" &&
        !grepl("^(Avail|Batters|Pitchers|$)", first) && !grepl("TOTALS", first)) next
    if (first == "Batters") { section <- "Hitter"; next }
    if (first == "Pitchers") { section <- "Pitcher"; next }
    if (first == "Avail" || first == "" || grepl("TOTALS", line)) next
    if (section == "Hitter" && length(fields) >= 4) {
      salary <- suppressWarnings(as.numeric(trimws(fields[4])))
      player <- trimws(fields[2])
      player <- gsub("\\s+(C|1B|2B|3B|SS|OF|DH|P|U)\\s*[|].*$", "", player)
      player <- trimws(player)
      if (!is.na(salary) && salary > 0) {
        results[[length(results)+1]] <- data.frame(Year=year, Player=player, Salary=salary, stringsAsFactors=FALSE)
      }
    }
  }
  if (length(results) > 0) bind_rows(results) else data.frame()
}

# Load year-matched ATC projections
loadATC <- function(year) {
  fn <- paste0("atcH", year, ".json")
  if (!file.exists(fn)) { cat("Missing:", fn, "\n"); return(NULL) }
  df <- read_json(fn, simplifyVector = TRUE)
  # Normalize playerid column
  if ("playerid" %in% names(df)) {
    # good
  } else if ("playerids" %in% names(df)) {
    df <- df %>% rename(playerid = playerids)
  }
  df$playerid <- as.character(df$playerid)
  df$PlayerName <- trimws(df$PlayerName)
  df %>% select(Player = PlayerName, playerid, HR, RBI, R, SB, AVG)
}

# Load projections and run the DAFL valuation to get pDFL
# We need daflFunctions for the valuation formula
oldwd <- getwd()
setwd("code")
suppressMessages(suppressWarnings(source("daflFunctions.r", local = TRUE)))
setwd(oldwd)

# Parse drafts and match to year-specific projections
years <- c("2023", "2024", "2025")
draftFiles <- list("2023"="data/2023DraftResults.csv", "2024"="data/2024DraftResults.csv", "2025"="data/2025DraftResults.csv")

allMatched <- list()
for (yr in years) {
  drafts <- parseDraftHitters(draftFiles[[yr]], yr)
  proj <- loadATC(yr)
  if (is.null(proj)) next

  # Match by player name
  matched <- inner_join(drafts, proj, by = "Player", relationship = "many-to-many")
  # Dedupe — keep first match per player per year
  matched <- matched %>% distinct(Player, Year, .keep_all = TRUE)
  cat(yr, ": matched", nrow(matched), "hitters\n")
  allMatched[[yr]] <- matched
}

all <- bind_rows(allMatched)
cat("\nTotal matched:", nrow(all), "hitters across", length(years), "drafts\n\n")

# Use salary as value proxy — inflation = how much more they paid relative to median for that salary tier
# Since we don't have pDFL per year, use rank-based approach:
# For each year, rank players by projected stats, rank by salary, compare
# OR simpler: correlate raw stats with salary directly and look at residuals

# Simple regression: Salary ~ HR + RBI + R + SB + AVG
cat("=== LINEAR REGRESSION: Salary ~ Stats ===\n")
fit <- lm(Salary ~ HR + RBI + R + SB + AVG, data = all)
cat(capture.output(summary(fit)), sep = "\n")

# Inflation = actual salary minus regression-predicted salary
all$predicted <- predict(fit, all)
all$inflation <- all$Salary - all$predicted

cat("\n\n=== WHICH STATS ARE OVERPAID? (Regression coefficients) ===\n")
coefs <- summary(fit)$coefficients
for (stat in c("HR", "RBI", "R", "SB", "AVG")) {
  cat(sprintf("  %s: $%.2f per unit (p=%.4f) %s\n",
    stat, coefs[stat, "Estimate"], coefs[stat, "Pr(>|t|)"],
    ifelse(coefs[stat, "Pr(>|t|)"] < 0.05, "*", "")))
}

cat("\n=== CORRELATION: Stat vs Salary ===\n")
cat(sprintf("  HR:  r = %.3f\n", cor(all$HR, all$Salary, use="complete")))
cat(sprintf("  RBI: r = %.3f\n", cor(all$RBI, all$Salary, use="complete")))
cat(sprintf("  R:   r = %.3f\n", cor(all$R, all$Salary, use="complete")))
cat(sprintf("  SB:  r = %.3f\n", cor(all$SB, all$Salary, use="complete")))
cat(sprintf("  AVG: r = %.3f\n", cor(all$AVG, all$Salary, use="complete")))

cat("\n=== AVG SALARY BY STAT QUARTILE ===\n")
for (stat in c("HR","RBI","R","SB","AVG")) {
  q75 <- quantile(all[[stat]], 0.75, na.rm=TRUE)
  top <- all %>% filter(.data[[stat]] >= q75)
  bot <- all %>% filter(.data[[stat]] < q75)
  cat(sprintf("  %s: Top25%%: avg salary $%.1f | Bottom75%%: $%.1f | Diff: $%.1f\n",
    stat, mean(top$Salary, na.rm=TRUE), mean(bot$Salary, na.rm=TRUE),
    mean(top$Salary, na.rm=TRUE) - mean(bot$Salary, na.rm=TRUE)))
}

cat("\n=== MOST OVERPAID (vs regression model) ===\n")
top15 <- all %>% arrange(-inflation) %>% head(15)
for (i in seq_len(nrow(top15))) {
  cat(sprintf("  %s (%s): Paid $%d, Model $%.0f, +$%.0f | HR:%d R:%d RBI:%d SB:%d AVG:%.3f\n",
    top15$Player[i], top15$Year[i], top15$Salary[i], top15$predicted[i], top15$inflation[i],
    round(top15$HR[i]), round(top15$R[i]), round(top15$RBI[i]), round(top15$SB[i]), top15$AVG[i]))
}

cat("\n=== MOST UNDERPAID (vs regression model) ===\n")
bot15 <- all %>% arrange(inflation) %>% head(15)
for (i in seq_len(nrow(bot15))) {
  cat(sprintf("  %s (%s): Paid $%d, Model $%.0f, %+.0f | HR:%d R:%d RBI:%d SB:%d AVG:%.3f\n",
    bot15$Player[i], bot15$Year[i], bot15$Salary[i], bot15$predicted[i], bot15$inflation[i],
    round(bot15$HR[i]), round(bot15$R[i]), round(bot15$RBI[i]), round(bot15$SB[i]), bot15$AVG[i]))
}

cat("\n=== STAT PROFILE: Overpaid ($10+ over model) vs Underpaid ($10+ under) ===\n")
over <- all %>% filter(inflation >= 10)
under <- all %>% filter(inflation <= -10)
cat(sprintf("  Overpaid  (n=%d): HR=%.0f RBI=%.0f R=%.0f SB=%.0f AVG=%.3f AvgSalary=$%.0f\n",
  nrow(over), mean(over$HR), mean(over$RBI), mean(over$R), mean(over$SB), mean(over$AVG), mean(over$Salary)))
cat(sprintf("  Underpaid (n=%d): HR=%.0f RBI=%.0f R=%.0f SB=%.0f AVG=%.3f AvgSalary=$%.0f\n",
  nrow(under), mean(under$HR), mean(under$RBI), mean(under$R), mean(under$SB), mean(under$AVG), mean(under$Salary)))
