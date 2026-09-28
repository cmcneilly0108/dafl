# Lineup audit: where did each team's hitters' at-bats go?
#   Rscript lineupAudit.r [year]            (defaults to cyear)
#
# Rebuilds every hitter's day-by-day fantasy status (Active / Bench / IR) from the
# CBS transaction log ({year}all.csv), lines it up with MLB game logs from
# FanGraphs, and splits each rostered hitter's MLB at-bats into:
#   active      - in the lineup, counted for the team
#   benchPre    - on the bench before the draft (only protected players are
#                 rostered then, so these are keepers left out of the lineup)
#   bench       - on the bench after the draft (lineup choice or roster crunch)
#   ir          - on the fantasy IR while playing in MLB (slow activation)
# Keepers and trade arrivals start in the lineup; signings start on the bench. "active" is
# checked against CBS's accrued AB ({year}Accrued.csv) as a sanity test.
# Output: ../{year}lineupAudit.xlsx (team summary + per-player detail)

library("dplyr")
library("stringr")
library("tidyr")
library("jsonlite")
library("openxlsx")

source("./daflFunctions.r")

args <- commandArgs(trailingOnly = TRUE)
year <- if (length(args) > 0) args[1] else cyear
yy <- substr(year, 3, 4)

# Same mid-season renames as faabAnalysis.r: preseason name -> end-of-season name
allRenames <- list(
  "2020" = c("Kirby and the 10:15 Crew" = "Alex and the Bangers",
             "Neon Tetras" = "The Johnson Treatment",
             "Soft Tossers" = "Kandahar"),
  "2021" = c("The Johnson Treatment" = "Neon Tetras"),
  "2022" = c("Alex and the Bangers" = "Chamomile and Oxy",
             "Fluffy the Destroyer" = "Heinous Fuckery",
             "Kandahar" = "Pearl Harbor"),
  "2026" = c("SNACKTIME" = "No Ohtanis",
             "Monochromatic Wombats" = "Doug's Fluffy Glasses",
             "Neon Giraffes" = "Banana Cry"))
teamRenames <- if (is.null(allRenames[[year]])) c() else allRenames[[year]]
renameTeams <- function(t) {
  t <- as.character(t)
  i <- t %in% names(teamRenames)
  t[i] <- unname(teamRenames[t[i]])
  t
}

mdy <- function(s) as.Date(str_trim(str_extract(s, "[0-9]+/[0-9]+/[0-9]+")), format = "%m/%d/%y")

# ---- Transactions -> one row per player move
tx <- read.csv(str_c("../", year, "all.csv"), stringsAsFactors = FALSE)
# Multi-player transactions list one move per line - but some saved files (2024)
# lost the line breaks ("BenchedJason Adam P | SD - Activated"), so re-insert a
# break after every action. Explicit position list so "Moved to 2B" doesn't eat
# the next player's initial.
txTeams <- unique(c(tx$Team, names(teamRenames), unname(teamRenames)))
teamAlt <- paste(str_replace_all(txTeams, "([.$()\\[\\]^*+?{}|\\\\])", "\\\\\\1"), collapse = "|")
posAlt <- "IR|SS|OF|DH|SP|RP|MI|CI|LF|CF|RF|1B|2B|3B|C|U|P"
actionRe <- str_c("(Activated and Moved to (?:", posAlt, ")|Benched and Moved to (?:", posAlt,
                  ")|Moved to (?:", posAlt, ")|Activated|Benched|Dropped|Signed for \\$[0-9.]+|",
                  "Traded [Ff]rom (?:", teamAlt, "))")
tx$Players <- str_replace_all(tx$Players, actionRe, "\\1\n")

tx <- tx %>% filter(!is.na(Players), Players != "") %>%
  mutate(stamp = as.POSIXct(str_extract(Date, "[0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+ [AP]M"),
                            format = "%m/%d/%y %I:%M %p"),
         eff = mdy(Effective), Team = renameTeams(Team)) %>%
  filter(!is.na(eff)) %>%
  separate_rows(Players, sep = "\n") %>%
  mutate(Players = str_trim(Players)) %>%
  filter(str_detect(Players, " - ")) %>%
  mutate(raw = str_trim(str_extract(Players, "^.*?(?= - )")),
         action = str_trim(str_extract(Players, "(?<= - ).*$")),
         isPitcher = str_detect(raw, " P \\| | P\\|| SP \\| | RP \\| "),
         MLB = normMlbTeam(str_trim(str_extract(raw, "(?<=\\| ?)[A-Z]+$"))),
         Player = str_trim(str_replace(raw, " [A-Z0-9,]+ ?\\| ?[A-Z]+$", "")))

# Map each move to a status change (NA = no change, e.g. "Moved to 2B")
statusOf <- function(a) case_when(
  str_detect(a, "^Activated") ~ "active",
  str_detect(a, "^Benched") ~ "bench",
  str_detect(a, "Moved to IR") ~ "ir",
  str_detect(a, "^Dropped") ~ "gone",
  str_detect(a, "^Signed") ~ "bench",
  str_detect(a, regex("^Traded from", ignore_case = TRUE)) ~ "active",
  TRUE ~ NA_character_)
moves <- tx %>% mutate(status = statusOf(action)) %>% filter(!is.na(status)) %>%
  select(Team, Player, MLB, isPitcher, eff, stamp, status, action)

# A trade also removes the player from the team he left
tradedAway <- tx %>% filter(str_detect(action, regex("^Traded from", ignore_case = TRUE))) %>%
  transmute(Team = renameTeams(str_trim(str_remove(action, regex("^Traded from ", ignore_case = TRUE)))),
            Player, MLB, isPitcher, eff, stamp, status = "gone")
moves <- bind_rows(moves, tradedAway)

seasonStart <- min(moves$eff)
# The draft: the busiest effective date for "Signed" moves after opening day
signedDates <- tx %>% filter(str_detect(action, "^Signed"), eff > seasonStart) %>% count(eff)
draftDate <- if (nrow(signedDates) > 0) signedDates$eff[which.max(signedDates$n)] else seasonStart

# Before the draft, stats count for every player on the post-draft roster (CBS
# credits draft picks back to opening day), whatever their lineup status - so
# draft signings are treated as joining on opening day, and pre-draft days are
# bucketed "active" below. Lineup moves only matter from the draft on.
moves <- moves %>% mutate(eff = if_else(status == "bench" & eff <= draftDate &
                                          str_detect(tx$action[match(paste(Team, Player, eff), paste(tx$Team, tx$Player, tx$eff))], "^Signed") %in% TRUE,
                                        seasonStart, eff))

# Protected players start the season in the active lineup (CBS default) without
# a move, and traded players arrive in the lineup; signings start on the bench
prot <- read.csv(str_c("../data/", year, "ProtectionLists.csv"), stringsAsFactors = FALSE) %>%
  transmute(Team = renameTeams(Team), Player = str_trim(Player), MLB = normMlbTeam(MLB),
            isPitcher = Pos %in% c("P","SP","RP","MR","CL"),
            eff = seasonStart - 1, stamp = as.POSIXct(NA), status = "active")
moves <- bind_rows(prot, moves)

# ---- FanGraphs ids via mymaster: normalized name + MLB team, else a unique
# normalized name (older CBS names drop "Jr." - "Fernando Tatis", "Juan Soto")
m <- master %>% transmute(key = normPlayerName(cbs_name), MLB = normMlbTeam(MLB), playerid = as.character(playerid)) %>%
  filter(!is.na(playerid), playerid != "")
byNameTeam <- m %>% distinct(key, MLB, .keep_all = TRUE)
byName <- m %>% group_by(key) %>% filter(n_distinct(playerid) == 1) %>% slice(1) %>% ungroup() %>%
  select(key, pidByName = playerid)
ids <- moves %>% distinct(Player, MLB) %>% mutate(key = normPlayerName(Player)) %>%
  left_join(byNameTeam, by = c("key","MLB")) %>%
  left_join(byName, by = "key") %>%
  mutate(playerid = coalesce(playerid, pidByName)) %>% select(-pidByName, -key)
# A player's MLB team can change mid-season; take any id found for the name
ids <- ids %>% group_by(Player) %>% mutate(playerid = first(na.omit(playerid))) %>% ungroup()
moves <- left_join(moves, ids, by = c("Player","MLB")) %>%
  group_by(Player) %>% mutate(playerid = first(na.omit(playerid))) %>% ungroup()
unmapped <- moves %>% filter(is.na(playerid)) %>% distinct(Player)
if (nrow(unmapped) > 0) message(nrow(unmapped), " players without a FanGraphs id (skipped): ",
                                paste(head(unmapped$Player, 10), collapse = ", "))
# Game logs need a numeric FanGraphs id (prospect "sa" ids have no MLB log)
odd <- moves %>% filter(!is.na(playerid), !str_detect(playerid, "^[0-9]+$")) %>% distinct(Player, playerid)
if (nrow(odd) > 0) message(nrow(odd), " players with a non-FanGraphs id (skipped): ",
                           paste(head(odd$Player, 10), collapse = ", "))
moves <- filter(moves, !is.na(playerid), str_detect(playerid, "^[0-9]+$"))
playerNames <- moves %>% distinct(playerid, Player) %>% group_by(playerid) %>% slice(1) %>% ungroup()

# ---- Game logs (cached). Pitching logs need position=P; IP is in thirds (5.2)
glDir <- str_c("../data/gamelogs/", year)
dir.create(glDir, recursive = TRUE, showWarnings = FALSE)
gameLog <- function(pid, pitcher) {
  f <- file.path(glDir, str_c(pid, if (pitcher) "_P" else "", ".json"))
  if (!file.exists(f)) {
    res <- curl::curl_fetch_memory(str_c("https://www.fangraphs.com/api/players/game-log?playerid=",
                                         pid, "&position=", if (pitcher) "P" else "", "&type=0&season=", year))
    if (res$status_code != 200) return(NULL)
    writeBin(res$content, f)
    Sys.sleep(0.3)
  }
  g <- tryCatch(fromJSON(f)$mlb, error = function(e) NULL)
  need <- if (pitcher) c("Date","IP") else c("Date","AB")
  if (!is.data.frame(g) || nrow(g) == 0 || !all(need %in% names(g))) return(NULL)
  g <- mutate(g, date = as.Date(substr(gsub("<[^>]+>", "", Date), 1, 10)))
  g <- if (pitcher) {
    transmute(g, date, IP = floor(as.numeric(IP)) + round((as.numeric(IP) %% 1) * 10) / 3,
              W = as.numeric(W), K = as.numeric(SO), S = as.numeric(SV), HD = as.numeric(HLD), ER = as.numeric(ER))
  } else {
    transmute(g, date, AB = as.numeric(AB), H = as.numeric(H), HR = as.numeric(HR), R = as.numeric(R),
              RBI = as.numeric(RBI), SB = as.numeric(SB))
  }
  filter(g, !is.na(date), format(date, "%Y") == year)
}

# CBS accrued totals per team, for the sanity check
pl <- read.csv(str_c("../", year, "Accrued.csv"), header = FALSE, stringsAsFactors = FALSE, fill = TRUE)
pl <- pl %>% mutate(Team = ifelse(V2 == "" & !(V1 %in% c("Batters","Pitchers")), V1, NA),
                    porh = ifelse(V1 %in% c("Batters","Pitchers"), V1, NA)) %>%
  fill(Team, porh) %>% filter(str_detect(V2, "\\|"))

# ---- One side (hitters or pitchers): status on each game date = last move
# effective on or before that date; stats bucketed by that status
auditSide <- function(pitcher) {
  mv <- filter(moves, isPitcher == pitcher)
  statCols <- if (pitcher) c("IP","W","K","S","HD","ER") else c("AB","H","HR","R","RBI","SB")
  vol <- statCols[1]
  pids <- unique(mv$playerid)
  message("Game logs: ", length(pids), if (pitcher) " pitchers" else " hitters", " (cached in ", glDir, ")")
  logs <- bind_rows(lapply(pids, function(p) { g <- gameLog(p, pitcher); if (!is.null(g)) mutate(g, playerid = p) }))
  events <- mv %>% select(Team, playerid, eff, stamp, status)
  statusAt <- logs %>%
    inner_join(events, by = "playerid", relationship = "many-to-many") %>%
    filter(eff <= date) %>%
    group_by(Team, playerid, date) %>% arrange(eff, stamp, .by_group = TRUE) %>% slice_tail(n = 1) %>% ungroup() %>%
    filter(status != "gone") %>%
    mutate(bucket = case_when(date < draftDate ~ "active",
                              status == "active" ~ "active",
                              status == "ir" ~ "ir",
                              TRUE ~ "bench")) %>%
    left_join(playerNames, by = "playerid")

  detail <- statusAt %>% group_by(Team, Player, bucket) %>%
    summarize(across(all_of(statCols), sum), .groups = "drop")
  accCol <- if (pitcher) "V9" else "V8"
  accrued <- pl %>% filter(porh == if (pitcher) "Pitchers" else "Batters") %>%
    group_by(Team) %>% summarize(accrued = sum(as.numeric(.data[[accCol]]), na.rm = TRUE))

  showCols <- if (pitcher) c("IP","W","K","S","HD") else c("AB","HR","R","RBI","SB")
  team <- detail %>% group_by(Team, bucket) %>% summarize(across(all_of(showCols), sum), .groups = "drop") %>%
    complete(Team, bucket = c("active","bench","ir"), fill = setNames(as.list(rep(0, length(showCols))), showCols)) %>%
    pivot_wider(names_from = bucket, values_from = all_of(showCols), names_glue = "{.value}_{bucket}") %>%
    left_join(accrued, by = "Team")
  team$rostered <- team[[str_c(vol, "_active")]] + team[[str_c(vol, "_bench")]] + team[[str_c(vol, "_ir")]]
  team <- team %>% mutate(lineupEff = .data[[str_c(vol, "_active")]] / rostered,
                          checkVsAccrued = .data[[str_c(vol, "_active")]] / accrued) %>%
    select(Team, rostered, lineupEff, starts_with(str_c(vol, "_")), ends_with("_bench"), ends_with("_ir"),
           accrued, checkVsAccrued) %>%
    rename_with(~ str_replace(., "^rostered$", str_c("rostered", vol)), everything()) %>%
    rename_with(~ str_replace(., "^accrued$", str_c("accrued", vol)), everything()) %>%
    arrange(lineupEff)

  players <- detail %>% select(Team, Player, bucket, all_of(showCols)) %>%
    pivot_wider(names_from = bucket, values_from = all_of(showCols), values_fill = 0, names_glue = "{.value}_{bucket}")
  for (b in c("active","bench","ir")) if (!(str_c(vol, "_", b) %in% names(players))) players[[str_c(vol, "_", b)]] <- 0
  players$lost <- players[[str_c(vol, "_bench")]] + players[[str_c(vol, "_ir")]]
  players <- players %>% rename_with(~ str_replace(., "^lost$", str_c("lost", vol)), everything()) %>%
    arrange(Team, -.data[[str_c("lost", vol)]])

  list(team = team, players = players, statusAt = statusAt, logs = logs)
}

hit <- auditSide(FALSE)
pit <- auditSide(TRUE)

# ---- Weekly lineup regret (hitters, hindsight). For each team-week after the
# draft, swap benched hitters into the lineup where they'd have out-produced a
# starter they could replace (shared position eligibility, plus one U swap
# against the weakest starter), greedily from the best bench week down. Values
# use the season review's dollar scale (seasonScores in faabAnalysis.r) so
# regret is in DFL dollars. Hindsight upper bound: the week's results weren't
# known when the lineup was set - compare teams against each other.
accH <- pl %>% filter(porh == "Batters") %>%
  transmute(Team, Player = unlist(lapply(V2, stripName)), elig = V5,
            AB = as.numeric(V8), H = as.numeric(V9), HR = as.numeric(V10), R = as.numeric(V11),
            RBI = as.numeric(V12), SB = as.numeric(V13)) %>% filter(AB > 0)
accP <- pl %>% filter(porh == "Pitchers") %>%
  transmute(INN = as.numeric(V9), ER = as.numeric(V8), W = as.numeric(V10), S = as.numeric(V11),
            HD = as.numeric(V13)) %>% filter(INN > 0)
lgAvg <- sum(accH$H) / sum(accH$AB)
zH <- with(accH, HR/sd(HR) + R/sd(R) + RBI/sd(RBI) + SB/sd(SB) + (H - AB*lgAvg)/sd(H - AB*lgAvg))
# Pitcher side of the dollar scale. The accrued view has no K, so its z-sum is
# stood in for by the other four categories x 5/4 - this only sizes the dollar
# scale (a few percent), not the comparison between teams
lgEra <- 9 * sum(accP$ER) / sum(accP$INN)
zP <- with(accP, W/sd(W) + S/sd(S) + HD/sd(HD) + (INN*lgEra/9 - ER)/sd(INN*lgEra/9 - ER))
dollarsPerZ <- 300 * n_distinct(pl$Team) / (sum(zH) + sum(zP) * 5/4)
sdH <- with(accH, c(HR = sd(HR), R = sd(R), RBI = sd(RBI), SB = sd(SB), xH = sd(H - AB*lgAvg)))
hDollars <- function(AB, H, HR, R, RBI, SB)
  (HR/sdH["HR"] + R/sdH["R"] + RBI/sdH["RBI"] + SB/sdH["SB"] + (H - AB*lgAvg)/sdH["xH"]) * dollarsPerZ

eligOf <- accH %>% distinct(Team, Player, elig)
weeks <- hit$statusAt %>% filter(date >= draftDate, bucket != "ir") %>%
  mutate(week = date - (as.integer(format(date, "%u")) - 1)) %>%
  group_by(Team, Player, week) %>%
  summarize(started = sum(AB[bucket == "active"]) >= sum(AB[bucket == "bench"]),
            AB = sum(AB), H = sum(H), HR = sum(HR), R = sum(R), RBI = sum(RBI), SB = sum(SB), .groups = "drop") %>%
  mutate(value = unname(hDollars(AB, H, HR, R, RBI, SB))) %>%
  left_join(eligOf, by = c("Team","Player")) %>%
  mutate(elig = coalesce(elig, "U"))

posSet <- function(e) setdiff(str_trim(str_split(e, ",")[[1]]), "U")
weekRegret <- function(d) {
  if (!any(d$started) || all(d$started)) return(tibble(gain = 0, swaps = 0, detail = ""))
  swapList <- character(0)
  a2 <- d %>% filter(started) %>% arrange(value); b2 <- d %>% filter(!started) %>% arrange(-value)
  used <- rep(FALSE, nrow(a2)); benchUsed <- rep(FALSE, nrow(b2)); gain <- 0
  for (i in seq_len(nrow(b2))) {
    pb <- posSet(b2$elig[i])
    ok <- which(!used & sapply(a2$elig, function(e) length(intersect(pb, posSet(e))) > 0))
    if (length(ok) > 0 && b2$value[i] > a2$value[ok[1]]) {
      j <- ok[1]; used[j] <- TRUE; benchUsed[i] <- TRUE
      gain <- gain + b2$value[i] - a2$value[j]
      swapList <- c(swapList, str_c(b2$Player[i], " for ", a2$Player[j]))
    }
  }
  # One U swap: best unused bench bat vs weakest unused starter
  bi <- which(!benchUsed)[1]; aj <- which(!used)[1]
  if (!is.na(bi) && !is.na(aj) && b2$value[bi] > a2$value[aj]) {
    gain <- gain + b2$value[bi] - a2$value[aj]
    swapList <- c(swapList, str_c(b2$Player[bi], " for ", a2$Player[aj], " (U)"))
  }
  tibble(gain = gain, swaps = length(swapList), detail = paste(swapList, collapse = "; "))
}
regretWeeks <- weeks %>% group_by(Team, week) %>% group_modify(~ weekRegret(.x)) %>% ungroup()
regretTeams <- regretWeeks %>% group_by(Team) %>%
  summarize(weeks = n(), regretDFL = sum(gain), weeksWithRegret = sum(gain > 0), swaps = sum(swaps),
            avgPerWeek = mean(gain), .groups = "drop") %>%
  arrange(-regretDFL)

# ---- Lineup changes (hitters): how many, and did they help? A change is a
# bench->active or active->bench move after the draft (IR returns, signings and
# drops excluded). For each team and effective date with equal numbers moved in
# and out (a clean swap), compare what the activated and benched hitters did the
# next 7 days (swapGain > 0 = the change beat standing pat), and what they had
# done the prior 14 days (per week) - activating hot players who then cool off
# shows as a big prev->next drop for "in" against a rebound for "out".
prevStatus <- moves %>% filter(!isPitcher) %>% arrange(Team, playerid, eff, stamp) %>%
  group_by(Team, playerid) %>% mutate(prev = lag(status)) %>% ungroup()
changes <- prevStatus %>%
  filter(eff >= draftDate, str_detect(coalesce(action, ""), "^(Activated|Benched)"),
         (status == "active" & prev == "bench") | (status == "bench" & prev == "active")) %>%
  mutate(dir = ifelse(status == "active", "in", "out"))
hLogs <- hit$logs
windowValue <- function(pid, from, to) {
  g <- hLogs[hLogs$playerid == pid & hLogs$date >= from & hLogs$date <= to, ]
  c(value = unname(hDollars(sum(g$AB), sum(g$H), sum(g$HR), sum(g$R), sum(g$RBI), sum(g$SB))), G = nrow(g))
}
if (nrow(changes) > 0) {
  wv <- t(mapply(function(p, e) c(windowValue(p, e, e + 6), windowValue(p, e - 14, e - 1)),
                 changes$playerid, changes$eff))
  changes$nextVal <- wv[, 1]; changes$nextG <- wv[, 2]; changes$prevVal <- wv[, 3] / 2
}
swapDates <- changes %>% group_by(Team, eff) %>%
  summarize(nIn = sum(dir == "in"), nOut = sum(dir == "out"),
            inNext = sum(nextVal[dir == "in"]), outNext = sum(nextVal[dir == "out"]),
            inPrev = sum(prevVal[dir == "in"]), outPrev = sum(prevVal[dir == "out"]),
            inG = sum(nextG[dir == "in"]), outG = sum(nextG[dir == "out"]), .groups = "drop") %>%
  filter(nIn == nOut, nIn > 0) %>%
  mutate(swapGain = inNext - outNext)
nWeeks <- as.numeric(max(hLogs$date) - draftDate) / 7
changeTeams <- changes %>% group_by(Team) %>% summarize(moves = n(), .groups = "drop") %>%
  left_join(swapDates %>% group_by(Team) %>%
              summarize(swaps = sum(nIn), swapGainDFL = sum(swapGain), swapWinRate = mean(swapGain > 0),
                        gamesEdge = sum(inG - outG) / sum(nIn),
                        inPrev = sum(inPrev) / sum(nIn), inNext = sum(inNext) / sum(nIn),
                        outPrev = sum(outPrev) / sum(nIn), outNext = sum(outNext) / sum(nIn), .groups = "drop"),
            by = "Team") %>%
  mutate(movesPerWeek = moves / nWeeks,
         # how much more the activated players cooled off than the benched ones
         hotChase = (inPrev - inNext) - (outPrev - outNext)) %>%
  arrange(-moves)

wb <- createWorkbook()
headerStyle <- createStyle(halign = "CENTER", textDecoration = "Bold")
pct <- createStyle(numFmt = "0%")
addSheet <- function(name, df, pctCols = c("lineupEff","checkVsAccrued")) {
  addWorksheet(wb, name)
  writeData(wb, name, df, headerStyle = headerStyle)
  cols <- which(names(df) %in% pctCols)
  if (length(cols)) addStyle(wb, name, pct, rows = 2:(nrow(df) + 1), cols = cols, gridExpand = TRUE)
  setColWidths(wb, name, cols = 1:ncol(df), widths = "auto")
}
addSheet("Hitters", hit$team)
addSheet("Pitchers", pit$team)
addSheet("Lineup Regret", regretTeams %>% mutate(across(c(regretDFL, avgPerWeek), ~ round(., 1))))
addSheet("Regret by Week", regretWeeks %>% filter(gain > 0) %>% arrange(Team, week) %>%
           mutate(gain = round(gain, 1), week = format(week, "%Y-%m-%d")) %>% rename(weekOf = week))
addSheet("Lineup Changes", changeTeams %>% mutate(across(where(is.numeric), ~ round(., 2))), pctCols = "swapWinRate")
addSheet("Hitter Detail", hit$players)
addSheet("Pitcher Detail", pit$players %>% mutate(across(starts_with("IP_") | starts_with("lostIP"), ~ round(., 1))))
saveWorkbook(wb, str_c("../", year, "lineupAudit.xlsx"), overwrite = TRUE)
message("Draft date ", draftDate, "; wrote ../", year, "lineupAudit.xlsx")
print(as.data.frame(hit$team %>% select(Team, lineupEff, checkVsAccrued) %>%
  left_join(pit$team %>% select(Team, pLineupEff = lineupEff, pCheck = checkVsAccrued), by = "Team") %>%
  left_join(regretTeams %>% select(Team, regretDFL, weeksWithRegret), by = "Team") %>%
  mutate(across(where(is.numeric), ~ round(., 2)))))
