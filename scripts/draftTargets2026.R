library(dplyr)
library(stringr)
library(jsonlite)

# Source draftGuide for available players and projections
oldwd <- getwd()
setwd("code")
suppressMessages(suppressWarnings(source("draftGuide.r", local = TRUE)))
setwd(oldwd)

# Available hitters (not protected)
availH <- AllH %>%
  filter(!playerid %in% protected$playerid) %>%
  mutate(playerid = as.character(playerid))

# Available pitchers
availP <- AllP %>%
  filter(!playerid %in% protected$playerid) %>%
  mutate(playerid = as.character(playerid))

# ============================================
# 1. BIGGEST pDFL vs ADP GAPS
# ============================================
cat("=== 1. PROJECTION vs ADP GAPS (projections love them, drafters don't) ===\n")
cat("   Players where pDFL ranks them much higher than their ADP\n\n")

gapH <- availH %>%
  filter(pDFL >= 5, !is.na(pADP), pADP > 0) %>%
  mutate(
    dflRank = rank(-pDFL),
    adpRank = pADP,
    rankGap = adpRank - dflRank
  ) %>%
  arrange(-rankGap) %>%
  head(10)

cat("HITTERS:\n")
cat(sprintf("%-25s %4s %6s %6s %6s  %s\n", "Player", "Pos", "pDFL", "ADP", "Gap", "Profile"))
for (i in seq_len(nrow(gapH))) {
  g <- gapH[i, ]
  profile <- paste0("HR:", round(g$pHR), " R:", round(g$pR), " RBI:", round(g$pRBI), " SB:", round(g$pSB), " AVG:", sprintf("%.3f", g$pAVG))
  cat(sprintf("%-25s %4s  $%-4.0f  %4.0f  %+5.0f  %s\n",
    g$Player, g$Pos, g$pDFL, g$pADP, g$rankGap, profile))
}

gapP <- availP %>%
  filter(pDFL >= 5, !is.na(pADP), pADP > 0) %>%
  mutate(
    dflRank = rank(-pDFL),
    adpRank = pADP,
    rankGap = adpRank - dflRank
  ) %>%
  arrange(-rankGap) %>%
  head(10)

cat("\nPITCHERS:\n")
cat(sprintf("%-25s %4s %6s %6s %6s  %s\n", "Player", "Pos", "pDFL", "ADP", "Gap", "Profile"))
for (i in seq_len(nrow(gapP))) {
  g <- gapP[i, ]
  profile <- paste0("W:", round(g$pW), " SO:", round(g$pSO), " ERA:", sprintf("%.2f", g$pERA), " SV:", round(g$pSV), " HLD:", round(g$pHLD))
  cat(sprintf("%-25s %4s  $%-4.0f  %4.0f  %+5.0f  %s\n",
    g$Player, g$Pos, g$pDFL, g$pADP, g$rankGap, profile))
}

# ============================================
# 2. BREAKOUT CANDIDATES — young + big projection
# ============================================
cat("\n\n=== 2. BREAKOUT CANDIDATES (age 25 or under with strong projections) ===\n\n")

breakH <- availH %>%
  filter(Age <= 25, pDFL >= 8) %>%
  arrange(-pDFL) %>%
  head(10)

cat("HITTERS:\n")
cat(sprintf("%-25s %4s %4s %6s %6s  %s\n", "Player", "Pos", "Age", "pDFL", "ADP", "Profile"))
for (i in seq_len(nrow(breakH))) {
  b <- breakH[i, ]
  profile <- paste0("HR:", round(b$pHR), " R:", round(b$pR), " RBI:", round(b$pRBI), " SB:", round(b$pSB), " AVG:", sprintf("%.3f", b$pAVG))
  cat(sprintf("%-25s %4s  %3.0f  $%-4.0f  %4.0f  %s\n",
    b$Player, b$Pos, b$Age, b$pDFL, b$pADP, profile))
}

breakP <- availP %>%
  filter(Age <= 25, pDFL >= 5) %>%
  arrange(-pDFL) %>%
  head(10)

cat("\nPITCHERS:\n")
cat(sprintf("%-25s %4s %4s %6s %6s  %s\n", "Player", "Pos", "Age", "pDFL", "ADP", "Profile"))
for (i in seq_len(nrow(breakP))) {
  b <- breakP[i, ]
  profile <- paste0("W:", round(b$pW), " SO:", round(b$pSO), " ERA:", sprintf("%.2f", b$pERA))
  cat(sprintf("%-25s %4s  %3.0f  $%-4.0f  %4.0f  %s\n",
    b$Player, b$Pos, b$Age, b$pDFL, b$pADP, profile))
}

# ============================================
# 3. POSITIONAL SCARCITY
# ============================================
cat("\n\n=== 3. POSITIONAL SCARCITY (where talent dries up fastest) ===\n\n")

positions <- c("C", "1B", "2B", "SS", "3B", "OF")
cat(sprintf("%-4s %6s %6s %6s %8s  %s\n", "Pos", "#Avail", "Top$", "2nd$", "Dropoff", "Best Available"))
for (pos in positions) {
  posPlayers <- availH %>%
    filter(Pos == pos | str_detect(posEl, fixed(pos))) %>%
    arrange(-pDFL)
  n <- nrow(posPlayers)
  top1 <- if (n >= 1) posPlayers$pDFL[1] else 0
  top2 <- if (n >= 2) posPlayers$pDFL[2] else 0
  top3 <- if (n >= 3) posPlayers$pDFL[3] else 0
  dropoff <- top1 - top3
  bestName <- if (n >= 1) posPlayers$Player[1] else "None"
  aboveFive <- sum(posPlayers$pDFL >= 5)
  cat(sprintf("%-4s    %3d   $%-3.0f   $%-3.0f    $%-4.0f  %s (%d above $5)\n",
    pos, n, top1, top2, dropoff, bestName, aboveFive))
}

# Dollar guys who might outperform
cat("\n--- BEST $1 SLEEPERS BY POSITION ---\n")
for (pos in positions) {
  posPlayers <- availH %>%
    filter(Pos == pos | str_detect(posEl, fixed(pos)), pDFL >= 1, pDFL <= 6) %>%
    arrange(-pDFL) %>%
    head(3)
  if (nrow(posPlayers) > 0) {
    names <- paste0(posPlayers$Player, " ($", round(posPlayers$pDFL), ")", collapse = ", ")
    cat(sprintf("  %s: %s\n", pos, names))
  }
}

cat("\n--- PITCHER ROLES ---\n")
for (role in c("SP", "MR", "CL")) {
  rolePlayers <- availP %>%
    filter(Pos == role) %>%
    arrange(-pDFL)
  n <- nrow(rolePlayers)
  top1 <- if (n >= 1) rolePlayers$pDFL[1] else 0
  aboveFive <- sum(rolePlayers$pDFL >= 5)
  bestName <- if (n >= 1) rolePlayers$Player[1] else "None"
  cat(sprintf("  %s: %d available, %d above $5, best: %s ($%.0f)\n",
    role, n, aboveFive, bestName, top1))
}
