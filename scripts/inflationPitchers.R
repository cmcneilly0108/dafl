library(dplyr)
library(stringr)
library(jsonlite)

parseDraftPitchers <- function(filepath, year) {
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
    if (section == "Pitcher" && length(fields) >= 4) {
      salary <- suppressWarnings(as.numeric(trimws(fields[4])))
      player <- trimws(fields[2])
      player <- gsub("\\s+P\\s*[|].*$", "", player)
      player <- trimws(player)
      if (!is.na(salary) && salary > 0) {
        results[[length(results)+1]] <- data.frame(Year=year, Player=player, Salary=salary, stringsAsFactors=FALSE)
      }
    }
  }
  if (length(results) > 0) bind_rows(results) else data.frame()
}

loadATCP <- function(year) {
  fn <- paste0("atcP", year, ".json")
  if (!file.exists(fn)) { cat("Missing:", fn, "\n"); return(NULL) }
  df <- read_json(fn, simplifyVector = TRUE)
  if ("playerid" %in% names(df) && "playerids" %in% names(df)) {
    df$playerids <- NULL
  } else if ("playerids" %in% names(df)) {
    df <- df %>% rename(playerid = playerids)
  }
  df$playerid <- as.character(df$playerid)
  df$PlayerName <- trimws(df$PlayerName)
  df %>% select(Player = PlayerName, playerid, W, SO, ERA, SV, HLD, IP, BB)
}

years <- c("2023", "2024", "2025")
draftFiles <- list("2023"="data/2023DraftResults.csv", "2024"="data/2024DraftResults.csv", "2025"="data/2025DraftResults.csv")

allMatched <- list()
for (yr in years) {
  drafts <- parseDraftPitchers(draftFiles[[yr]], yr)
  proj <- loadATCP(yr)
  if (is.null(proj)) next
  matched <- inner_join(drafts, proj, by = "Player", relationship = "many-to-many") %>%
    distinct(Player, Year, .keep_all = TRUE)
  cat(yr, ": matched", nrow(matched), "pitchers\n")
  allMatched[[yr]] <- matched
}

all <- bind_rows(allMatched)
cat("\nTotal matched:", nrow(all), "pitchers across", length(years), "drafts\n\n")

# Derive K/9 and BB/9
all$K9 <- all$SO / pmax(all$IP, 1) * 9
all$BB9 <- all$BB / pmax(all$IP, 1) * 9

cat("=== CORRELATION: Stat vs Salary ===\n")
cat(sprintf("  W:   r = %.3f\n", cor(all$W, all$Salary, use="complete")))
cat(sprintf("  SO:  r = %.3f\n", cor(all$SO, all$Salary, use="complete")))
cat(sprintf("  ERA: r = %.3f\n", cor(all$ERA, all$Salary, use="complete")))
cat(sprintf("  SV:  r = %.3f\n", cor(all$SV, all$Salary, use="complete")))
cat(sprintf("  HLD: r = %.3f\n", cor(all$HLD, all$Salary, use="complete")))
cat(sprintf("  IP:  r = %.3f\n", cor(all$IP, all$Salary, use="complete")))
cat(sprintf("  K/9: r = %.3f\n", cor(all$K9, all$Salary, use="complete")))

cat("\n=== LINEAR REGRESSION: Salary ~ Stats ===\n")
fit <- lm(Salary ~ W + SO + ERA + SV + HLD + IP, data = all)
cat(capture.output(summary(fit)), sep = "\n")

cat("\n\n=== WHICH STATS ARE OVERPAID? (Regression coefficients) ===\n")
coefs <- summary(fit)$coefficients
for (stat in c("W", "SO", "ERA", "SV", "HLD", "IP")) {
  cat(sprintf("  %s: $%.2f per unit (p=%.4f) %s\n",
    stat, coefs[stat, "Estimate"], coefs[stat, "Pr(>|t|)"],
    ifelse(coefs[stat, "Pr(>|t|)"] < 0.05, "*", "")))
}

cat("\n=== AVG SALARY BY STAT QUARTILE ===\n")
for (stat in c("W","SO","ERA","SV","HLD","IP","K9")) {
  q75 <- quantile(all[[stat]], 0.75, na.rm=TRUE)
  if (stat == "ERA") {
    # For ERA, "top" is low ERA
    q25 <- quantile(all[[stat]], 0.25, na.rm=TRUE)
    top <- all %>% filter(.data[[stat]] <= q25)
    bot <- all %>% filter(.data[[stat]] > q25)
    cat(sprintf("  %s: Best25%%: avg salary $%.1f | Rest: $%.1f | Diff: $%.1f\n",
      stat, mean(top$Salary, na.rm=TRUE), mean(bot$Salary, na.rm=TRUE),
      mean(top$Salary, na.rm=TRUE) - mean(bot$Salary, na.rm=TRUE)))
  } else {
    top <- all %>% filter(.data[[stat]] >= q75)
    bot <- all %>% filter(.data[[stat]] < q75)
    cat(sprintf("  %s: Top25%%: avg salary $%.1f | Bottom75%%: $%.1f | Diff: $%.1f\n",
      stat, mean(top$Salary, na.rm=TRUE), mean(bot$Salary, na.rm=TRUE),
      mean(top$Salary, na.rm=TRUE) - mean(bot$Salary, na.rm=TRUE)))
  }
}

all$predicted <- predict(fit, all)
all$inflation <- all$Salary - all$predicted

cat("\n=== MOST OVERPAID PITCHERS (vs regression model) ===\n")
top15 <- all %>% arrange(-inflation) %>% head(15)
for (i in seq_len(nrow(top15))) {
  t <- top15[i, ]
  cat(sprintf("  %s (%s): Paid $%d, Model $%.0f, +$%.0f | W:%d SO:%d ERA:%.2f SV:%d HLD:%d IP:%.0f\n",
    t$Player, t$Year, t$Salary, t$predicted, t$inflation,
    round(t$W), round(t$SO), t$ERA, round(t$SV), round(t$HLD), t$IP))
}

cat("\n=== MOST UNDERPAID PITCHERS (vs regression model) ===\n")
bot15 <- all %>% arrange(inflation) %>% head(15)
for (i in seq_len(nrow(bot15))) {
  t <- bot15[i, ]
  cat(sprintf("  %s (%s): Paid $%d, Model $%.0f, %+.0f | W:%d SO:%d ERA:%.2f SV:%d HLD:%d IP:%.0f\n",
    t$Player, t$Year, t$Salary, t$predicted, t$inflation,
    round(t$W), round(t$SO), t$ERA, round(t$SV), round(t$HLD), t$IP))
}

cat("\n=== STAT PROFILE: Overpaid ($5+ over model) vs Underpaid ($5+ under) ===\n")
over <- all %>% filter(inflation >= 5)
under <- all %>% filter(inflation <= -5)
cat(sprintf("  Overpaid  (n=%d): W=%.0f SO=%.0f ERA=%.2f SV=%.0f HLD=%.0f IP=%.0f AvgSalary=$%.0f\n",
  nrow(over), mean(over$W), mean(over$SO), mean(over$ERA), mean(over$SV), mean(over$HLD), mean(over$IP), mean(over$Salary)))
cat(sprintf("  Underpaid (n=%d): W=%.0f SO=%.0f ERA=%.2f SV=%.0f HLD=%.0f IP=%.0f AvgSalary=$%.0f\n",
  nrow(under), mean(under$W), mean(under$SO), mean(under$ERA), mean(under$SV), mean(under$HLD), mean(under$IP), mean(under$Salary)))

# SP vs RP analysis
cat("\n=== SP vs RP INFLATION ===\n")
all$role <- ifelse(all$IP >= 100, "SP", "RP")
for (r in c("SP", "RP")) {
  sub <- all %>% filter(role == r)
  cat(sprintf("  %s (n=%d): Avg salary $%.1f, Avg model $%.1f, Avg inflation $%.1f\n",
    r, nrow(sub), mean(sub$Salary), mean(sub$predicted), mean(sub$inflation)))
}
