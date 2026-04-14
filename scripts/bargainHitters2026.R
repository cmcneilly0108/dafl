library(dplyr)
library(stringr)
library(jsonlite)

# Source draftGuide for available hitters and projections
oldwd <- getwd()
setwd("code")
suppressMessages(suppressWarnings(source("draftGuide.r", local = TRUE)))
setwd(oldwd)

# Get available hitters (not protected)
avail <- AllH %>%
  filter(!playerid %in% protected$playerid) %>%
  select(Player, playerid, Pos, MLB, Age, pDFL, pHR, pRBI, pR, pSB, pAVG, pSGP)

# Build the regression model from historical data (same as inflationAnalysis.R)
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

loadATC <- function(year) {
  fn <- paste0("atcH", year, ".json")
  if (!file.exists(fn)) return(NULL)
  df <- read_json(fn, simplifyVector = TRUE)
  if ("playerid" %in% names(df) && "playerids" %in% names(df)) {
    df$playerids <- NULL
  } else if ("playerids" %in% names(df)) {
    df <- df %>% rename(playerid = playerids)
  }
  df$playerid <- as.character(df$playerid)
  df$PlayerName <- trimws(df$PlayerName)
  df %>% select(Player = PlayerName, playerid, HR, RBI, R, SB, AVG)
}

# Build historical model
years <- c("2023", "2024", "2025")
draftFiles <- list("2023"="data/2023DraftResults.csv", "2024"="data/2024DraftResults.csv", "2025"="data/2025DraftResults.csv")
allMatched <- list()
for (yr in years) {
  drafts <- parseDraftHitters(draftFiles[[yr]], yr)
  proj <- loadATC(yr)
  if (is.null(proj)) next
  matched <- inner_join(drafts, proj, by = "Player", relationship = "many-to-many") %>%
    distinct(Player, Year, .keep_all = TRUE)
  allMatched[[yr]] <- matched
}
hist <- bind_rows(allMatched)

# Fit model: what does the league typically pay given stats?
fit <- lm(Salary ~ HR + RBI + R + SB + AVG, data = hist)

# Predict what the league would pay for each available 2026 hitter
avail$predictedSalary <- predict(fit, newdata = data.frame(
  HR = avail$pHR, RBI = avail$pRBI, R = avail$pR, SB = avail$pSB, AVG = avail$pAVG
))

# Bargain score: how much less they're likely to actually cost vs what the model says
# Players the model says are expensive but whose profile (speed, low AVG) suggests discount
avail$bargainGap <- avail$pDFL - avail$predictedSalary
# Also factor in: high SB players are underpriced historically
avail$speedBonus <- ifelse(avail$pSB >= 15, 3, ifelse(avail$pSB >= 10, 1.5, 0))
avail$bargainScore <- avail$bargainGap + avail$speedBonus

# Filter to meaningful players and sort
bargains <- avail %>%
  filter(pDFL >= 3) %>%
  arrange(-bargainScore) %>%
  head(20)

cat("=== TOP 20 BARGAIN HITTERS FOR 2026 DRAFT ===\n\n")
cat(sprintf("%-25s %4s %6s %6s %4s %4s %4s %4s %5s  %s\n",
  "Player", "Pos", "pDFL", "Likely", "HR", "R", "RBI", "SB", "AVG", "Why"))
cat(paste(rep("-", 120), collapse=""), "\n")

for (i in seq_len(nrow(bargains))) {
  b <- bargains[i, ]

  # Generate reason
  reasons <- c()
  if (b$pSB >= 20) reasons <- c(reasons, paste0("elite speed (", round(b$pSB), " SB) consistently underpriced"))
  else if (b$pSB >= 10) reasons <- c(reasons, paste0("good speed (", round(b$pSB), " SB) undervalued by league"))
  if (b$pAVG < 0.250) reasons <- c(reasons, paste0("low AVG (", sprintf("%.3f", b$pAVG), ") suppresses bidding"))
  if (b$pDFL >= 15 && b$predictedSalary < b$pDFL * 0.7) reasons <- c(reasons, "projected value exceeds what league typically pays for this profile")
  if (b$pHR >= 25 && b$pAVG < 0.255) reasons <- c(reasons, "power without AVG — league discounts this combo")
  if (length(reasons) == 0) reasons <- "stat profile suggests league undervalues relative to projected value"

  reason <- reasons[1]

  cat(sprintf("%-25s %4s  $%-4.0f  $%-4.0f %4d %4d %4d %4d %5.3f  %s\n",
    b$Player, b$Pos, b$pDFL, max(1, round(b$predictedSalary)),
    round(b$pHR), round(b$pR), round(b$pRBI), round(b$pSB), b$pAVG, reason))
}
