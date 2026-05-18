# Identify CBS-rostered players that don't survive into LeagueEval's AllH/AllP.
#
# Two failure modes:
#   1) "no master entry"           - CBS player not in mymaster.csv at all
#   2) "master playerid mismatch"  - master row exists but its playerid is not
#                                    in the active projection, so the inner_join
#                                    in inSeasonPulse.r:172 / :203 drops them
#
# Run from the code/ directory:
#   Rscript checkMissingPlayers.r
#
# Output: ../missing_rostered_players.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(jsonlite)
})

source("./daflFunctions.r")  # loads master, pullPos, pullMLB, stripName, read.fg

# --- Active projection (matches inSeasonPulse.r logic) ---
leSettingsFile <- "../leagueEvalSettings.json"
leSettings <- tryCatch({
  if (file.exists(leSettingsFile)) jsonlite::fromJSON(leSettingsFile) else list()
}, error = function(e) list())
activeProj <- if (!is.null(leSettings$projSource) &&
                  leSettings$projSource %in% c('atc','steamer','batx')) {
  leSettings$projSource
} else 'atc'

projFiles <- list(
  atc     = list(h = "../atcHROS.json",     p = "../atcPROS.json"),
  steamer = list(h = "../steamerHROS.json", p = "../steamerPROS.json"),
  batx    = list(h = "../batxHROS.json",    p = "../batxPROS.json")
)

# --- Read CBS files like read.cbs() does, but skip addPlayerid so we keep
# original Player/MLB even when the master mapping is missing. ---
read_cbs_raw <- function(fn) {
  df <- read.csv(fn, skip = 1, stringsAsFactors = FALSE, encoding = "UTF-8")
  df <- df %>%
    mutate(Player = str_replace(Player, '&#149;', '|'),
           Pos    = pullPos(Player),
           MLB    = pullMLB(Player)) %>%
    filter(!is.na(MLB))
  df$Player <- unlist(lapply(df$Player, stripName))
  df$MLB <- replace(df$MLB, df$MLB == 'WAS', 'WSN')
  df$MLB <- replace(df$MLB, df$MLB == 'CWS', 'CHW')
  df$MLB <- replace(df$MLB, df$MLB == 'TB',  'TBR')
  df$MLB <- replace(df$MLB, df$MLB == 'KC',  'KCR')
  df$MLB <- replace(df$MLB, df$MLB == 'SD',  'SDP')
  df$MLB <- replace(df$MLB, df$MLB == 'SF',  'SFG')
  df$Team <- ifelse(str_detect(df$Avail, 'W '), 'Free Agent', df$Avail)
  df %>% select(Player, MLB, Pos, Team)
}

cbsH <- read_cbs_raw("../AllHitters.csv")
cbsP <- read_cbs_raw("../AllPitchers01.csv")

# --- Master lookups (Player+MLB primary, Player-only fallback) ---
m_full <- master %>%
  select(playerid, Player = cbs_name, MLB) %>%
  group_by(Player, MLB) %>%
  summarise(master_playerid_full = paste(unique(playerid), collapse = ';'),
            .groups = 'drop')

m_name <- master %>%
  select(playerid, Player = cbs_name) %>%
  group_by(Player) %>%
  summarise(master_playerid_name = paste(unique(playerid), collapse = ';'),
            .groups = 'drop')

attach_master <- function(cbs) {
  cbs %>%
    left_join(m_full, by = c('Player', 'MLB')) %>%
    left_join(m_name, by = 'Player') %>%
    mutate(master_playerid = case_when(
      !is.na(master_playerid_full) ~ master_playerid_full,
      !is.na(master_playerid_name) ~ master_playerid_name,
      TRUE                         ~ NA_character_
    )) %>%
    select(-master_playerid_full, -master_playerid_name)
}

cbsH2 <- attach_master(cbsH)
cbsP2 <- attach_master(cbsP)

# --- Load projections two ways ---
# 1. via read.fg (matches inSeasonPulse.r join semantics: playerids not in
#    master get rewritten to str_c(Player, pTeam), e.g. "Jared JonesPIT")
# 2. raw JSON, so we have the projection's original playerid for diagnosis
hitters_proj  <- read.fg(projFiles[[activeProj]]$h)
pitchers_proj <- read.fg(projFiles[[activeProj]]$p)

# Alternate projections — players covered by either are filtered out, since
# the user can switch projection in LeagueEval to surface them.
load_alt_ids <- function(slot) {
  alt_files <- projFiles[setdiff(names(projFiles), activeProj)]
  list(
    h = unlist(lapply(alt_files, function(x) unique(read.fg(x$h)$playerid))),
    p = unlist(lapply(alt_files, function(x) unique(read.fg(x$p)$playerid)))
  )
}
alt_ids <- load_alt_ids()

# Normalize a name for fuzzy comparison: strip accents, lowercase, collapse
# whitespace, drop punctuation. Catches "José Ramírez" vs "Jose Ramirez" and
# "Hye Seong Kim" vs "Hyeseong Kim".
normalize_name <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- str_replace_all(x, "[^a-z0-9]", "")
  x
}

read_proj_raw <- function(fn) {
  raw <- jsonlite::read_json(fn, simplifyVector = TRUE)
  if ("playerids" %in% colnames(raw) && !"playerid" %in% colnames(raw)) {
    raw <- raw %>% rename(playerid = playerids)
  }
  raw %>%
    transmute(Player = PlayerName,
              proj_name_key = normalize_name(PlayerName),
              proj_playerid = as.character(playerid),
              proj_team = Team) %>%
    group_by(proj_name_key) %>%
    summarise(proj_player    = paste(unique(Player),        collapse = ';'),
              proj_playerids = paste(unique(proj_playerid), collapse = ';'),
              proj_teams     = paste(unique(proj_team),     collapse = ';'),
              .groups = 'drop')
}

projH_byname <- read_proj_raw(projFiles[[activeProj]]$h)
projP_byname <- read_proj_raw(projFiles[[activeProj]]$p)

# --- Classify rostered CBS players that wouldn't survive the inner_join ---
any_id_in <- function(id_string, id_set) {
  if (is.na(id_string)) return(FALSE)
  ids <- strsplit(id_string, ';', fixed = TRUE)[[1]]
  any(ids %in% id_set)
}

classify <- function(cbs2, proj_ids_after_readfg, proj_byname, kind) {
  cbs2 %>%
    filter(Team != 'Free Agent') %>%
    rowwise() %>%
    mutate(in_projection = any_id_in(master_playerid, proj_ids_after_readfg)) %>%
    ungroup() %>%
    filter(!in_projection) %>%
    mutate(name_key = normalize_name(Player)) %>%
    left_join(proj_byname, by = c('name_key' = 'proj_name_key')) %>%
    transmute(
      kind            = kind,
      Player, MLB,
      CBS_Pos         = Pos,
      CBS_Team        = Team,
      master_playerid,
      proj_player_normalized = proj_player,
      proj_playerid_byname   = proj_playerids,
      proj_team_byname       = proj_teams,
      failure_mode = case_when(
        is.na(master_playerid)         ~ 'no master entry',
        !is.na(proj_playerid_byname)   ~ 'master playerid mismatch (name found in projection)',
        TRUE                           ~ 'not in projection at all'
      )
    )
}

missH <- classify(cbsH2, hitters_proj$playerid,  projH_byname, 'hitter')
missP <- classify(cbsP2, pitchers_proj$playerid, projP_byname, 'pitcher')

raw_result <- bind_rows(missH, missP) %>%
  arrange(failure_mode, kind, CBS_Team, Player) %>%
  distinct()

# Filter out players covered by Steamer or BAT X (any master id matches an
# alternate projection's id set for the same kind). User can surface those by
# switching the LeagueEval projection.
covered_by_alt <- function(master_pid, kind) {
  if (is.na(master_pid)) return(FALSE)
  ids <- strsplit(master_pid, ';', fixed = TRUE)[[1]]
  alt <- if (kind == 'hitter') alt_ids$h else alt_ids$p
  any(ids %in% alt)
}

result <- raw_result %>%
  rowwise() %>%
  mutate(in_alt_projection = covered_by_alt(master_playerid, kind)) %>%
  ungroup() %>%
  filter(!in_alt_projection) %>%
  select(-in_alt_projection)

dropped <- nrow(raw_result) - nrow(result)

out_path <- "../missing_rostered_players.csv"
write.csv(result, out_path, row.names = FALSE)

cat(sprintf("\nProjection: %s\n", activeProj))
cat(sprintf("%d rostered CBS players missing from LeagueEval (%d additional covered by Steamer/BAT X, suppressed)\n\n",
            nrow(result), dropped))
cat("Breakdown by failure mode:\n")
print(result %>% count(failure_mode, kind), row.names = FALSE)
cat("\n")
print(as.data.frame(result), row.names = FALSE)
cat(sprintf("\nWrote: %s\n", out_path))
