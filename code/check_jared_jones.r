# Verification: confirm Jared Jones appears exactly once on Liquor
# Crickets after addPlayerid overrides + master backfill.
# Minimal slice of inSeasonPulse — no network fetches.

if (basename(getwd()) != "code") setwd("code")
library("zoo")  # for na.locf, used by getSalary
source("./daflFunctions.r")

projFiles <- list(atc = list(h = "../atcHROS.json", p = "../atcPROS.json"))
pitchers <- read.fg(projFiles$atc$p)
hitters  <- read.fg(projFiles$atc$h)
hitters$Pos <- replace(hitters$Pos, is.na(hitters$Pos), 'DH')
hitters  <- select(hitters, -Player, -MLB, -Pos)
pitchers <- select(pitchers, -Player, -MLB, -Pos)

Allhitters <- read.cbs("../AllHitters.csv")
Allpitchers <- read.cbs("../AllPitchers01.csv") %>% rename(INN = INNs)

AllH <- inner_join(Allhitters, hitters, by = c('playerid'), copy = FALSE)
AllP <- inner_join(Allpitchers, pitchers, by = c('playerid'), copy = FALSE)

# Apply salary join (the place the dup actually surfaced)
sal <- getSalary()
AllP_sal <- AllP %>% left_join(sal, by = 'playerid')
AllH_sal <- AllH %>% left_join(sal, by = 'playerid')

cat("\n=== Allhitters Jared Jones (post addPlayerid) ===\n")
print(Allhitters %>% filter(Player == 'Jared Jones') %>% select(Player, Pos, MLB, Team, playerid))

cat("\n=== Allpitchers Jared Jones ===\n")
print(Allpitchers %>% filter(Player == 'Jared Jones') %>% select(Player, Pos, MLB, Team, playerid))

cat("\n=== sal rows for the two Jared Jones playerids ===\n")
print(sal %>% filter(playerid %in% c('27863', 'sa_jaredjones_1b_pit')))

cat("\n=== AllP+sal Jared Jones rows on Liquor Crickets ===\n")
print(AllP_sal %>% filter(Player == 'Jared Jones', Team == 'Liquor Crickets') %>%
        select(Player, Pos, MLB, Team, playerid, Salary, Contract))

cat("\n=== AllH+sal Jared Jones rows (should be empty) ===\n")
print(AllH_sal %>% filter(Player == 'Jared Jones') %>%
        select(Player, Pos, MLB, Team, playerid, Salary, Contract))

cat("\n=== Regression check: Liquor Crickets duplicate-player count ===\n")
dups_lc <- bind_rows(
  AllH_sal %>% filter(Team == 'Liquor Crickets') %>% select(Player, playerid),
  AllP_sal %>% filter(Team == 'Liquor Crickets') %>% select(Player, playerid)
) %>% count(Player, playerid) %>% filter(n > 1)
if (nrow(dups_lc) == 0) cat("  no duplicates\n") else print(dups_lc)

cat("\n=== Regression check: Hogan's Heroes duplicate-player count ===\n")
dups_hh <- bind_rows(
  AllH_sal %>% filter(Team == "Hogan's Heroes") %>% select(Player, playerid),
  AllP_sal %>% filter(Team == "Hogan's Heroes") %>% select(Player, playerid)
) %>% count(Player, playerid) %>% filter(n > 1)
if (nrow(dups_hh) == 0) cat("  no duplicates\n") else print(dups_hh)
