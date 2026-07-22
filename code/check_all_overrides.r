# Verify all 6 cbs_multi_pos cases got distinct playerids after the override change.

if (basename(getwd()) != "code") setwd("code")
library("zoo")
source("./daflFunctions.r")

Allhitters  <- read.cbs("../AllHitters.csv")
Allpitchers <- read.cbs("../AllPitchers01.csv") %>% rename(INN = INNs)
combined <- bind_rows(
  Allhitters  %>% select(Player, Pos, MLB, Team, playerid),
  Allpitchers %>% select(Player, Pos, MLB, Team, playerid)
)

ambig <- c("Alan Garcia", "Eduardo Garcia", "Jared Jones",
           "Jose Ruiz", "Luis Gonzalez")
cat("\n=== combined CBS rows for ambiguous names, grouped ===\n")
for (n in ambig) {
  cat("\n--", n, "--\n")
  print(combined %>% filter(Player == n) %>% arrange(MLB, Pos))
}

cat("\n=== sanity: distinct (Player, MLB, Pos) -> distinct playerid count ===\n")
chk <- combined %>%
  group_by(Player, MLB, Pos) %>%
  summarise(n_ids = n_distinct(playerid), ids = paste(unique(playerid), collapse=","), .groups="drop") %>%
  filter(n_ids > 1)
if (nrow(chk) == 0) cat("  every (name, team, pos) maps to one playerid -- OK\n") else print(chk)

cat("\n=== sanity: every override row maps at least one CBS row ===\n")
ov <- read.csv("../data/playerOverrides.csv", stringsAsFactors = FALSE)
for (i in seq_len(nrow(ov))) {
  r <- ov[i, ]
  hits <- combined %>% filter(Player == r$cbs_name, MLB == r$mlb_team, Pos == r$cbs_pos) %>% nrow()
  cat(sprintf("  %s / %s / %s -> %s : %d CBS row(s)\n",
              r$cbs_name, r$mlb_team, r$cbs_pos, r$playerid, hits))
}
