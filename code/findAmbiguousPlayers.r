# Diagnostic: scan CBS rosters + mymaster.csv for cases where the
# (Player, MLB) join key in addPlayerid() is ambiguous or missing.
#
# Outputs:
#   - data/ambiguousPlayers.csv    one row per detected issue
#   - stdout summary               counts per kind + cbs_multi_pos table
#
# Run from project root or code/ — the script auto-detects.

library("dplyr")
library("stringr")

# --- locate project root ---
if (basename(getwd()) == "code") setwd("..")
stopifnot(file.exists("mymaster.csv"))

# --- parsing helpers (duplicates the parsing block in read.cbs to
# avoid coupling the diagnostic to addPlayerid's join behavior) ---
stripName <- function(n) {
  nm <- strsplit(n, "[ |]+")[[1]]
  paste(head(nm, -2), collapse = " ")
}
pullPos <- function(n) {
  n <- str_trim(n)
  p <- str_match(n, ".+ .+ ([^|]+) .+")[, 2]
  p <- ifelse(p == "P", "RP", p)
  ifelse(p %in% c("CF", "RF", "LF"), "OF", p)
}
pullMLB <- function(n) {
  n <- str_trim(n)
  str_match(n, ".+ .+ .+ (.+)")[, 2]
}

remapMLB <- function(x) {
  x <- replace(x, x == "WAS", "WSN")
  x <- replace(x, x == "CWS", "CHW")
  x <- replace(x, x == "TB",  "TBR")
  x <- replace(x, x == "KC",  "KCR")
  x <- replace(x, x == "SD",  "SDP")
  x <- replace(x, x == "SF",  "SFG")
  x
}

parseCbs <- function(fn) {
  if (!file.exists(fn)) {
    message("skipping (missing): ", fn)
    return(tibble(
      cbs_name = character(), mlb_team = character(),
      cbs_pos = character(), source = character()
    ))
  }
  df <- read.csv(fn, skip = 1, stringsAsFactors = FALSE, encoding = "UTF-8")
  df <- mutate(df, Player = str_replace(Player, "&#149;", "|"))
  df <- mutate(df, Pos = pullPos(Player), MLB = pullMLB(Player))
  df <- filter(df, !is.na(MLB))
  df$Player <- unlist(lapply(df$Player, stripName))
  df$MLB <- remapMLB(df$MLB)
  tibble(
    cbs_name = df$Player,
    mlb_team = df$MLB,
    cbs_pos  = df$Pos,
    source   = basename(fn)
  )
}

# --- load CBS rows ---
cbs_files <- c(
  "AllHitters.csv",
  "AllPitchers01.csv",
  "AllHYTD.csv",
  "AllPYTD02.csv",
  "poselig.csv"
)
cbs <- bind_rows(lapply(cbs_files, parseCbs))

# --- load master ---
master <- read.csv("mymaster.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

# --- detection rule 1: cbs_multi_pos ---
# Same (cbs_name, mlb_team) appears in CBS files with 2+ distinct cbs_pos.
multi_pos <- cbs %>%
  distinct(cbs_name, mlb_team, cbs_pos, source) %>%
  group_by(cbs_name, mlb_team) %>%
  summarise(
    n_pos   = n_distinct(cbs_pos),
    details = paste(sort(unique(paste0(cbs_pos, "@", source))), collapse = ","),
    .groups = "drop"
  ) %>%
  filter(n_pos >= 2) %>%
  mutate(kind = "cbs_multi_pos") %>%
  select(cbs_name, mlb_team, kind, details)

# --- detection rule 2: master_multi_row ---
master_dups <- master %>%
  group_by(cbs_name, MLB) %>%
  summarise(
    n_rows  = n(),
    details = paste(sort(playerid), collapse = ","),
    .groups = "drop"
  ) %>%
  filter(n_rows >= 2) %>%
  mutate(kind = "master_multi_row") %>%
  rename(mlb_team = MLB) %>%
  select(cbs_name, mlb_team, kind, details)

# --- detection rule 3: unmatched ---
# CBS row whose (cbs_name, mlb_team) has zero matching rows in master.
unmatched <- cbs %>%
  distinct(cbs_name, mlb_team, source) %>%
  anti_join(master, by = c("cbs_name" = "cbs_name", "mlb_team" = "MLB")) %>%
  group_by(cbs_name, mlb_team) %>%
  summarise(
    details = paste(sort(unique(source)), collapse = ","),
    .groups = "drop"
  ) %>%
  mutate(kind = "unmatched") %>%
  select(cbs_name, mlb_team, kind, details)

# --- combine + write ---
out <- bind_rows(multi_pos, master_dups, unmatched) %>%
  arrange(kind, cbs_name)

dir.create("data", showWarnings = FALSE)
write.csv(out, "data/ambiguousPlayers.csv", row.names = FALSE)

# --- stdout summary ---
cat("\n=== ambiguousPlayers summary ===\n")
print(out %>% count(kind))
cat("\n--- cbs_multi_pos (most likely to need an override) ---\n")
print(out %>% filter(kind == "cbs_multi_pos"), n = Inf)
cat("\nWrote", nrow(out), "rows to data/ambiguousPlayers.csv\n")
