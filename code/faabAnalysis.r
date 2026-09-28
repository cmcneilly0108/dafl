# Bugs
#   - topFAABers - what even is this?
#   - injury columns - bring this back or remove it

# Create accrued file
#   http://dafl.baseball.cbssports.com/stats/stats-main/team:all/ytd:f/accrued/
#   2024Accrued.csv
# FanGraphs season totals - fgBatting{year}.json / fgPitching{year}.json (lineup
#   efficiency; past-season K estimate). Written by scripts/fgFetchInSeason.js, or:
#   https://www.fangraphs.com/api/leaders/major-league/data?pos=all&stats=bat&lg=all&season={year}&season1={year}&ind=0&qual=0&type=0&month=0&pageitems=3000&rost=0
#   (stats=pit for pitching)
# Accrued strikeouts - the accrued view's pitching "SO" column is shutouts, not K
#   https://dafl.baseball.cbssports.com/stats/stats-main/team:all/ytd:f/standard
#   {year}AccruedStd.csv
# Trades file!
#   https://dafl.baseball.cbssports.com/transactions/all/trades/
#   2019trades.csv
# Draft results - {year}DraftRosters.csv (written at draft time) is used when present.
#   Older years: week 1 rosters exported to data/{year}DraftResults.csv
#   https://dafl.baseball.cbssports.com/stats/stats-main/team:all/period-1:p/salary%20info/
#   !!!Need to manually fix Avail column!!!
# Injuries file
#   all transactions file?
#   https://dafl.baseball.cbssports.com/transactions/all/all/
#   2024all.csv 
# Copy over {year}ProtectionLists.csv to data/ folder
#   Team renames during the season go in teamRenames below
# Draftguide.xlsx
#   Needs the PRESEASON version saved as {year}draftGuide.xlsx - draftGuide.xlsx
#   gets regenerated in-season with ROS projections, so copy it right after the draft
#   (2026's was rebuilt after the fact from FanGraphs' frozen preseason ATC projections)

library("openxlsx")
library("stringr")
library("dplyr")
library("XML")
library("ggplot2")
library("reshape2")
library("tidyr")
library("splitstackshape")
library("zoo")

source("./daflFunctions.r")

cleanRosters <- function(pl) {
  colnames(pl) <- c('Avail','Player','E1','Pos','E2','Salary','Contract','S1','S2','S3','S4','S5','S6','Rank')
  players <- select(pl,-Rank,-E1,-E2) %>%
    filter(!(Player %in% c('Player','TOTALS')))
  #players <- mutate(players,porh=ifelse((Avail %in% c('Batters','Pitchers')),Avail,NA)) %>% 
  #  fill(porh) %>% filter(!(Avail %in% c('Batters','Pitchers')))
  players <- mutate(players,porh=ifelse((Avail %in% c('Batters','Pitchers')),Avail,NA)) %>% 
    fill(porh) %>% filter(!(Avail %in% c('Batters','Pitchers')))
  players <- mutate(players,Team=ifelse((Player %in% c('')),Avail,NA)) %>% 
    fill(Team) %>% filter(!(Player %in% c('')))
  # players <- mutate(players,Avail=ifelse((str_length(S1)==0),Player,NA)) %>% fill(Avail) %>%
  #   filter(!(str_length(S1)==0))
  # CBS uses SD/KC/TB/...; mymaster uses SDP/KCR/TBR - an unnormalized team misses
  # the Player+MLB match and falls back to name-only, fanning out to every namesake
  players <- mutate(players, MLB = normMlbTeam(pullMLB(Player)))
  players$Player <- unlist(lapply(players$Player,stripName))
  players <- players %>% mutate(.rid = row_number()) %>% addPlayerid()
  # Same name + MLB team in mymaster (e.g. two Jose Ramirez, CLE) still yields
  # several rows per CBS row: keep the id whose master Pos matches, then a real
  # FG id over an sa prospect id
  mPos <- master %>% select(playerid, mPos = Pos) %>% distinct(playerid, .keep_all = TRUE)
  players %>% left_join(mPos, by = 'playerid') %>%
    group_by(.rid) %>%
    arrange(desc(mPos %in% Pos), str_starts(playerid, 'sa'), .by_group = TRUE) %>%
    slice(1) %>% ungroup() %>% select(-.rid, -mPos) %>%
    # Unmatched players (e.g. older CBS names like "Fernando Tatis") would have
    # NA ids, which join to each other and multiply rows - give each its own
    mutate(playerid = ifelse(is.na(playerid), str_c("cbs_", Player, "_", Team), playerid)) %>%
    # Two namesakes mapped to one id on the same team (2022's two Luis Garcia
    # pitchers) would also multiply rows - suffix the extras
    group_by(playerid, Team, porh) %>%
    mutate(playerid = if (n() > 1) str_c(playerid, "_", row_number()) else playerid) %>%
    ungroup()
}

getWeek1 <- function(fn) {
  # Add Salary, Contract to players
  s <- read.csv(fn,header=FALSE,stringsAsFactors=FALSE, encoding="UTF-8")
  colnames(s) <- c('Avail','Player','Pos','Salary','Contract','Rank')
  
  sal <- select(s,-Rank) %>%
    filter(!(Avail %in% c('Batters','Pitchers','Avail'))) %>%
    filter(!(Player %in% c('TOTALS')))
  sal <- mutate(sal,Team = ifelse(str_length(lag(Pos))==0,lag(Avail),NA)) %>% filter(str_length(Pos)>0)
  sal$Team <- na.locf(sal$Team)
  sal <- mutate(sal, MLB = pullMLB(Player))
  sal$Player <- unlist(lapply(sal$Player,stripName))
  sal$Salary <- as.integer(sal$Salary)
  sal$Contract <- as.integer(sal$Contract)
  sal <- addPlayerid(sal) %>% select(playerid,Team) %>% distinct()
}

# Pitcher strikeouts accrued to each fantasy team, from the ytd:f/standard view.
# Same Team/Batters/Pitchers section layout as the accrued export; columns are
# looked up by header name.
getAccruedK <- function(fn) {
  s <- read.csv(fn, header=FALSE, stringsAsFactors=FALSE, fill=TRUE,
                col.names=paste0('V',1:25), colClasses='character')
  hdr <- which(s$V1 == 'Avail' & s$V2 == 'Player')
  pHdr <- hdr[which(s$V1[hdr-1] == 'Pitchers')][1]
  kCol <- str_c('V', which(unlist(s[pHdr,]) == 'K')[1])
  s <- mutate(s, Team = ifelse(V2 == '' & !(V1 %in% c('Batters','Pitchers')), V1, NA),
                 porh = ifelse(V1 %in% c('Batters','Pitchers'), V1, NA)) %>%
    fill(Team, porh) %>%
    filter(porh == 'Pitchers', str_detect(V2, '\\|'))
  data.frame(Team = s$Team,
             MLB = normMlbTeam(pullMLB(s$V2)),
             Player = unlist(lapply(s$V2, stripName)),
             accK = as.integer(s[[kCol]]),
             stringsAsFactors = FALSE)
}

# Season value scores. Same categories as hotScores() in daflFunctions.r
# (kept local so other reports are unaffected), with these changes:
#   - counting stats are scored from zero (x/sd) rather than from the worst
#     player, so there's no flat per-player bonus for churning through players
#   - AVG/ERA are scored against the AB/IP-weighted league rate rather than an
#     unweighted mean of player rates (inflated by tiny-sample blowups), so a
#     player who hurt his team's ratio comes out negative there
#   - holds count as a full category (hotScores weights them 0.6)
# A team's summed score is then a weighted sum of its category totals.
seasonScores <- function(h, p) {
  h <- filter(h, AB > 0)
  p <- filter(p, INN > 0)
  lgAvg <- sum(h$H) / sum(h$AB)
  lgEra <- 9 * sum(p$ER) / sum(p$INN)
  h <- mutate(h, xH = H - AB * lgAvg)
  p <- mutate(p, xER = INN * lgEra / 9 - ER)
  h <- mutate(h, zScore = HR/sd(HR) + R/sd(R) + RBI/sd(RBI) + SB/sd(SB) + xH/sd(xH))
  p <- mutate(p, zScore = W/sd(W) + K/sd(K) + HD/sd(HD) + S/sd(S) + xER/sd(xER))
  list(select(h, playerid, zScore, Team), select(p, playerid, zScore, Team))
}

# Season to analyze: Rscript faabAnalysis.r [year] - defaults to cyear
args <- commandArgs(trailingOnly = TRUE)
year <- if (length(args) > 0) args[1] else cyear

# Teams renamed during the season: preseason name (protection lists, draft
# rosters) -> end-of-season CBS name (Accrued, trades, standings). Without this
# their protected/drafted players are misclassified as faab. Mappings come from
# which accrued roster the preseason team's protected players ended up on.
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

# Prospects protected/drafted under an sa id may have a real FG id by season's
# end. Where (playerid, Team) no longer matches the accrued rosters, adopt the
# accrued id for the same Player + Team.
syncIds <- function(df, players) {
  cur <- players %>% select(Player, Team, curId = playerid) %>%
    group_by(Player, Team) %>% filter(n() == 1) %>% ungroup()
  df %>% left_join(cur, by = c('Player','Team')) %>%
    mutate(playerid = ifelse(!is.na(curId) & !paste(playerid, Team) %in% paste(players$playerid, players$Team),
                             curId, playerid)) %>%
    select(-curId)
}


# http://dafl.baseball.cbssports.com/stats/stats-main/team:all/ytd:f/accrued/
#pl <- read.csv("2019Accrued.csv",header=FALSE,stringsAsFactors=FALSE)
pl <- read.csv(str_c("../",year,"Accrued.csv"),header=FALSE,stringsAsFactors=FALSE)
# Raw CBS exports end each header row with a comma, adding an empty 15th column
if (ncol(pl) > 14) {
  stopifnot(all(is.na(pl[[15]]) | pl[[15]] == ""))
  pl <- pl[, 1:14]
}
players <- cleanRosters(pl)

hitters <- filter(players,porh=='Batters') %>%  select(Player,Pos,Salary,Contract,S1,S2,S3,S4,S5,S6,Team,playerid)
colnames(hitters) <- c('Player','Pos','Salary','Contract','AB','H','HR','R','RBI','SB','Team','playerid')
hitters <- mutate(hitters,AB=as.integer(AB),H=as.integer(H),HR=as.integer(HR),R=as.integer(R),
                  RBI=as.integer(RBI),SB=as.integer(SB),AVG=H/AB)
pitchers <- filter(players,porh=='Pitchers') %>%  select(Player,Pos,Salary,Contract,S1,S2,S3,S4,S5,S6,Team,playerid)
colnames(pitchers) <- c('Player','Pos','Salary','Contract','ER','INN','W','S','K','HD','Team','playerid')
pitchers <- mutate(pitchers,ER=as.integer(ER),INN=as.numeric(INN),W=as.integer(W),S=as.integer(S),
                  K=as.integer(K),HD=as.integer(HD),ERA=(ER/INN)*9)
# The accrued view's "K" slot is really SO (shutouts) - take K from the standard view
kFile <- str_c("../",year,"AccruedStd.csv")
if (file.exists(kFile)) {
  # Join on Team + Player only: addPlayerid can rewrite MLB to mymaster's team
  kdf <- getAccruedK(kFile) %>% select(-MLB)
  stopifnot(!anyDuplicated(kdf[, c('Team','Player')]))
  pKeys <- filter(players, porh=='Pitchers') %>% select(playerid, Team, Player) %>%
    left_join(kdf, by = c('Team','Player')) %>% select(playerid, Team, accK)
  pitchers <- left_join(pitchers, pKeys, by = c('playerid','Team')) %>%
    mutate(K = coalesce(accK, 0L)) %>% select(-accK)
  print(str_c("Accrued K: ", sum(!is.na(pKeys$accK)), " of ", nrow(pKeys), " pitchers matched"))
} else if (file.exists(str_c("../fgPitching",year,".json"))) {
  # Past seasons: CBS no longer serves team-accrued K, so prorate each pitcher's
  # MLB season K by the share of his innings accrued to the fantasy team.
  # FG IP is in thirds notation (59.2 = 59 2/3); accrued INN is decimal.
  fgp <- fromJSON(str_c("../fgPitching",year,".json"))$data %>%
    transmute(playerid = as.character(playerid), seasonK = SO,
              seasonIP = floor(IP) + round((IP %% 1) * 10) / 3) %>%
    distinct(playerid, .keep_all = TRUE)
  pitchers <- left_join(pitchers, fgp, by = 'playerid') %>%
    mutate(K = ifelse(is.na(seasonK) | seasonIP == 0, 0L,
                      as.integer(round(seasonK * pmin(1, INN / seasonIP))))) %>%
    select(-seasonK, -seasonIP)
  print(str_c("Estimated K from FanGraphs season totals: ", sum(pitchers$K > 0), " of ",
              sum(pitchers$INN > 0), " pitchers with innings"))
} else {
  warning(str_c(kFile, " missing - strikeouts are scored as shutouts"))
}

# hotscores
r <- seasonScores(hitters,pitchers)
oh <- r[[1]]
op <- r[[2]]

# convert to DFL
tz <- sum(oh$zScore) + sum(op$zScore)
nteams <- n_distinct(players$Team)
td <- 300*nteams
tratio <- td/tz
oh$DFL <- oh$zScore * tratio
op$DFL <- op$zScore * tratio

hitters <- inner_join(hitters,oh,by=c('playerid','Team'))
pitchers <- inner_join(pitchers,op,by=c('playerid','Team'))

RH <- hitters %>% group_by(Team) %>% summarize(hDFL = sum(DFL))
RP <- pitchers %>% group_by(Team) %>% summarize(piDFL = sum(DFL))
RTot <- inner_join(RH,RP,by=c('Team')) %>%
  mutate(tDFL = hDFL + piDFL,hRank = rank(-hDFL),pRank = rank(-piDFL)) %>%
  arrange(-tDFL)


# Now create the asrc field
# Load ProtectionList file - 'protect'
# Load DraftRecap file - 'draft'
# Everything else - 'faab'
prot <- read.csv(str_c("../data/",year,"ProtectionLists.csv"),stringsAsFactors=FALSE)
prot$Team <- renameTeams(prot$Team)
prot$playerid <- as.character(prot$playerid)
prot <- syncIds(prot, players)
protFull <- prot %>% mutate(asrc='protect')
protThin <- protFull %>% select(playerid,Team,asrc)
hitters <- left_join(hitters,protThin,by=c('playerid','Team'))
pitchers <- left_join(pitchers,protThin,by=c('playerid','Team'))
# then do anti-join with larger prot file - doesn't work - hitter/pitcher split
# fill rest with blanks
# row bind
#hitters <- anti_join(protFullhitters,prot,by=c('playerid','Team'))
#pitchers <- full_join(pitchers,prot,by=c('playerid','Team'))
# Protection lists use P through 2025, SP/MR/CL (sometimes RP) from 2026
pitcherPos <- c("P","SP","RP","MR","CL")
protFP <- protFull %>% filter(Pos %in% pitcherPos)
protFH <- protFull %>% filter(!(Pos %in% pitcherPos))
protFP <- anti_join(protFP,pitchers,by=c('playerid','Team'))
protFH <- anti_join(protFH,hitters,by=c('playerid','Team'))
protFHres <- protFH %>% mutate(Salary=as.character(Salary),Contract=as.character(Contract),AB=0,H=0,R=0,RBI=0,SB=0,AVG=0,zScore=0,DFL=0) %>% select(Player,Pos,Salary,Contract,AB,H,R,RBI,SB,Team,playerid,AVG,zScore,DFL,asrc)
hitters <- bind_rows(hitters,protFHres)
protFPres <- protFP %>% mutate(Salary=as.character(Salary),Contract=as.character(Contract),ER=0,INN=0,W=0,S=0,K=0,HD=0,ERA=0,zScore=0,DFL=0) %>% select(Player,Pos,Salary,Contract,ER,INN,W,S,K,HD,Team,playerid,ERA,zScore,DFL,asrc)
pitchers <- bind_rows(pitchers,protFPres)


#draft <- read.csv(str_c("../data/",year,"DraftResults.csv"),stringsAsFactors=FALSE)
draftRostersFile <- str_c("../",year,"DraftRosters.csv")
# Older seasons saved the draft as name-only files (no playerid); match those to
# the accrued rosters by Player + Team
draftByName <- list("2020" = "../data/2020DraftRecap.csv",
                    "2021" = "../data/2021DraftResults2.csv")
if (!is.null(draftByName[[year]])) {
  dn <- read.csv(draftByName[[year]],stringsAsFactors=FALSE) %>%
    mutate(Player=ifelse(str_detect(Player,'\\|'), unlist(lapply(Player,stripName)), str_trim(Player)),
           Team=renameTeams(Team))
  cur <- players %>% select(Player, Team, playerid) %>%
    group_by(Player, Team) %>% filter(n() == 1) %>% ungroup()
  draft <- inner_join(dn, cur, by=c('Player','Team')) %>%
    select(playerid,Team,dSal=Salary) %>% distinct()
  print(str_c("Draft file matched ", nrow(draft), " of ", nrow(dn), " rows to accrued rosters"))
} else if (file.exists(draftRostersFile)) {
  # Post-draft rosters (protected + drafted); protect wins in the coalesce below
  draft <- read.csv(draftRostersFile,stringsAsFactors=FALSE) %>%
    mutate(playerid=as.character(playerid),Team=renameTeams(Team)) %>%
    syncIds(players) %>% select(playerid,Team,dSal=Salary) %>% distinct()
} else if (file.exists(str_c("../data/",year,"DraftResults.csv"))) {
  draftFile <- str_c("../data/",year,"DraftResults.csv")
  if ("playerid" %in% names(read.csv(draftFile, nrows=1))) {
    # Already-processed post-draft rosters (2021): same shape as DraftRosters
    draft <- read.csv(draftFile,stringsAsFactors=FALSE) %>%
      mutate(playerid=as.character(playerid),Team=renameTeams(Team)) %>%
      syncIds(players) %>% select(playerid,Team,dSal=Salary) %>% distinct()
  } else {
    draft <- getWeek1(draftFile) %>% mutate(dSal=NA_integer_)
  }
} else {
  warning(str_c("No draft results for ", year, " - drafted players will count as faab"))
  draft <- data.frame(playerid=character(0), Team=character(0), dSal=integer(0))
}
draft$Team <- renameTeams(draft$Team)
draft <- draft %>% mutate(dft='draft') %>% select(playerid,Team,dft,dSal)
hitters <- left_join(hitters,draft,by=c('playerid','Team'))
pitchers <- left_join(pitchers,draft,by=c('playerid','Team'))


hitters$asrc <- coalesce(hitters$asrc,hitters$dft,"faab")
pitchers$asrc <- coalesce(pitchers$asrc,pitchers$dft,"faab")


# Load trades file
# http://dafl.baseball.cbssports.com/transactions/all/trades/?print_rows=9999
trades <- read.csv(str_c("../",year,"trades.csv"),stringsAsFactors=FALSE)
if (!("Players" %in% names(trades)) && all(c("Team","Player","Effective","fTeam") %in% names(trades))) {
  # Already-parsed trades file (2024)
  trades <- select(trades,Team,Player,Traded=Effective,fTeam)
} else {
  # Split rows with multiple players (separated by newlines) into separate rows
  trades <- cSplit(trades,"Players",sep="\n",direction="long")

  # Parse the Players column to extract player name and from team
  # Format: "Player Name POS | TEAM - Traded from Team Name"
  trades <- trades %>%
    filter(str_detect(Players,'Traded')) %>%  # Keep only 'Traded' rows
    mutate(
      # Extract player name (everything before " - Traded from")
      Player = str_extract(Players, regex("^[^-]+(?= - Traded from)", ignore_case=TRUE)) %>% str_trim(),
      # Extract the from team (everything after "Traded from ")
      fTeam = str_extract(Players, regex("(?<=Traded from ).*$", ignore_case=TRUE)) %>% str_trim()
    )

  # Remove position and team info from Player name (e.g., "P | SF")
  trades$Player <- str_replace(trades$Player, " [A-Z0-9,]+\\s*\\|\\s*[A-Z]+\\s*$", "")

  trades <- select(trades,Team,Player,Traded=Effective,fTeam)
}

# Filter again to remove Benched and Moved rows
trades <- filter(trades,!str_detect(fTeam,'Benched'))
trades <- filter(trades,!str_detect(fTeam,'Moved'))

hitters <- left_join(hitters,trades,by=c('Team','Player'))
pitchers <- left_join(pitchers,trades,by=c('Team','Player'))

# Try not overwriting protect/draft for traded away players
#hitters$asrc <- ifelse(is.na(hitters$Traded),hitters$asrc,'trade')
#pitchers$asrc <- ifelse(is.na(pitchers$Traded),pitchers$asrc,'trade')
hitters$asrc <- ifelse((!is.na(hitters$Traded) & hitters$asrc=="faab"),'trade',hitters$asrc)
pitchers$asrc <- ifelse((!is.na(pitchers$Traded) & pitchers$asrc=="faab"),'trade',pitchers$asrc)

# Lets add injuries!
allFile <- str_c("../",year,"all.csv")
if (file.exists(allFile)) {
  injured <- read.csv(allFile,stringsAsFactors=FALSE)
  injured <- filter(injured,str_detect(Players,'IR'))
  numinj <- injured %>% count(Team) %>% rename(injured = n)
} else {
  numinj <- data.frame(Team=character(0), injured=integer(0))
}
#numinj <- numinj %>% mutate(irank = rank(injured))

# Create traded away value
taH <- filter(hitters,!is.na(fTeam)) %>% group_by(fTeam) %>% 
  summarize(taway =sum(DFL)) %>% select(Team=fTeam,taway)
taP <- filter(pitchers,!is.na(fTeam)) %>% group_by(fTeam) %>% summarize(taway =sum(DFL)) %>% select(Team=fTeam,taway)
ta <- rbind(taH,taP) %>% group_by(Team) %>% summarize(taway =sum(taway))

# CBS leaves Salary blank for some drafted players in the Accrued export -
# fall back to their draft-day salary
hitters <- mutate(hitters, Salary = coalesce(as.integer(Salary), as.integer(dSal)))
pitchers <- mutate(pitchers, Salary = coalesce(as.integer(Salary), as.integer(dSal)))


hitters <- mutate(hitters, Value = DFL - Salary)
pitchers <- mutate(pitchers, Value = DFL - Salary)
hitters <- arrange(hitters,-Value)
pitchers <- arrange(pitchers,-Value)


#Load DAFL standings file
if (year == cyear) {
  standings <- read.csv("../DAFLWeeklyStandings.csv",stringsAsFactors=FALSE)
  standings$Rank <- as.numeric(str_extract(standings$Rank,'[0-9]+'))
  final <- filter(standings,Week==max(Week)) %>% select(Actual=Rank,Short=Team)
  nicks <- read.csv("../data/nicknames.csv",stringsAsFactors=FALSE)
  fstand <- inner_join(final,nicks,by=c('Short')) %>% select(-Short)
} else {
  # Past seasons. data/fs{year}.csv holds saved final standings: use it when its
  # names cover every team (exact or unique-prefix match - some are truncated,
  # e.g. "Liquor Cricket"). Otherwise (short nicknames) use the final ranks
  # recorded in the original season review.
  accTeams <- unique(players$Team)
  fsFile <- str_c("../data/fs",year,".csv")
  fstand <- NULL
  if (file.exists(fsFile)) {
    fs <- read.csv(fsFile,stringsAsFactors=FALSE)
    fs <- data.frame(Actual=as.numeric(fs[[1]]), fsTeam=fs$Team, stringsAsFactors=FALSE)
    fs$Team <- sapply(fs$fsTeam, function(t) {
      m <- accTeams[accTeams == t | startsWith(accTeams, t)]
      if (length(m) == 1) m else NA_character_
    })
    if (all(accTeams %in% fs$Team)) fstand <- fs %>% filter(!is.na(Team)) %>% select(Team, Actual)
  }
  if (is.null(fstand)) {
    fstand <- read.xlsx(str_c("../data/seasonReviewBackup/",year,"seasonReview.xlsx"),1) %>%
      select(Team, Actual) %>% filter(!is.na(Actual))
  }
}


# Create final data frame
srcH <- select(hitters,Team,asrc,DFL,Salary)
srcP <- select(pitchers,Team,asrc,DFL,Salary)
src <- rbind(srcH,srcP)
#f <- src %>% group_by(Team,asrc) %>% summarize(srcDFL=sum(DFL))
#seasonResults <- dcast(f,Team ~ asrc)
f2 <- src %>% group_by(Team,asrc) %>% summarize(Sal=sum(Salary),DFL=sum(DFL))
meltf <- melt(f2, id.vars=c('Team','asrc'))
seasonResults <- dcast(meltf,Team ~ asrc + variable)
seasonResults <- select(seasonResults,-any_of(c('trade_Sal','faab_Sal')))
# A season with no trades (or no draft file) has no rows for that source
for (col in c('protect_DFL','protect_Sal','draft_DFL','draft_Sal','faab_DFL','trade_DFL')) {
  if (!(col %in% names(seasonResults))) seasonResults[[col]] <- if (endsWith(col,'_Sal')) NA_real_ else 0
}

seasonResults$protect_DFL <- replace(seasonResults$protect_DFL,is.na(seasonResults$protect_DFL),0)
seasonResults$faab_DFL <- replace(seasonResults$faab_DFL,is.na(seasonResults$faab_DFL),0)
seasonResults$trade_DFL <- replace(seasonResults$trade_DFL,is.na(seasonResults$trade_DFL),0)
seasonResults <- left_join(seasonResults,ta) %>% mutate(tradeValue = trade_DFL-taway)

seasonResults <- left_join(seasonResults,fstand)

seasonResults <- seasonResults %>% replace_na(list(tradeValue=0))
seasonResults <- mutate(seasonResults,overall = draft_DFL+faab_DFL+protect_DFL+trade_DFL,
                        pratio = protect_DFL/protect_Sal,
                        dratio = draft_DFL/draft_Sal, drank = rank(-dratio),
                        frank = rank(-faab_DFL),prank = rank(-pratio),trank = rank(-tradeValue)) %>%
  select(Team,Actual,overall,protect_DFL,prank,protect_Sal,pratio,draft_DFL,drank,draft_Sal,dratio,faab_DFL,frank,trade_DFL,trank,tradeValue) %>% arrange(-overall)

# add injured data -  mutate(irank = rank(injured))
seasonResults <- left_join(seasonResults,numinj)
seasonResults <- seasonResults %>% replace_na(list(injured=0)) %>%  mutate(irank = rank(injured))

# Lineup efficiency. CBS only accrues stats while a player is in the active
# lineup, so for whole-season players (protected/drafted and still on the team
# at season's end) MLB season stats minus accrued stats = production left on the
# bench (or on the fantasy IL after returning). hLineupEff / pLineupEff are the
# share of those players' MLB AB / IP that counted; benchDFL values what didn't,
# using the same scoring as seasonScores(). Upper bound: starting a benched
# player means sitting someone else.
fgbFile <- str_c("../fgBatting",year,".json"); fgpFile <- str_c("../fgPitching",year,".json")
if (file.exists(fgbFile) && file.exists(fgpFile)) {
  fgb <- fromJSON(fgbFile)$data %>%
    transmute(playerid=as.character(playerid), sAB=AB, sH=H, sHR=HR, sR=R, sRBI=RBI, sSB=SB) %>%
    distinct(playerid, .keep_all=TRUE)
  fgp <- fromJSON(fgpFile)$data %>%
    transmute(playerid=as.character(playerid), sIP=floor(IP)+round((IP %% 1)*10)/3,
              sER=ER, sW=W, sK=SO, sS=SV, sHD=HLD) %>%
    distinct(playerid, .keep_all=TRUE)
  endOwner <- players %>% select(playerid, Team, Avail)
  h <- filter(hitters, AB > 0); p <- filter(pitchers, INN > 0)
  lgAvg <- sum(h$H)/sum(h$AB); lgEra <- 9*sum(p$ER)/sum(p$INN)
  sdH <- c(HR=sd(h$HR), R=sd(h$R), RBI=sd(h$RBI), SB=sd(h$SB), xH=sd(h$H - h$AB*lgAvg))
  sdP <- c(W=sd(p$W), K=sd(p$K), HD=sd(p$HD), S=sd(p$S), xER=sd(p$INN*lgEra/9 - p$ER))
  hVal <- function(AB,H,HR,R,RBI,SB) unname((HR/sdH["HR"] + R/sdH["R"] + RBI/sdH["RBI"] + SB/sdH["SB"] + (H-AB*lgAvg)/sdH["xH"]) * tratio)
  pVal <- function(IP,ER,W,K,S,HD) unname((W/sdP["W"] + K/sdP["K"] + HD/sdP["HD"] + S/sdP["S"] + (IP*lgEra/9-ER)/sdP["xER"]) * tratio)
  wholeSeason <- function(df) df %>% filter(asrc %in% c("protect","draft")) %>%
    inner_join(endOwner, by=c("playerid","Team")) %>% filter(Avail == Team)
  benchH <- wholeSeason(h) %>% inner_join(fgb, by="playerid") %>% filter(sAB >= AB) %>%
    mutate(benchDFL = hVal(sAB,sH,sHR,sR,sRBI,sSB) - hVal(AB,H,HR,R,RBI,SB))
  benchP <- wholeSeason(p) %>% inner_join(fgp, by="playerid") %>% filter(sIP >= INN - 0.5) %>%
    mutate(benchDFL = pVal(sIP,sER,sW,sK,sS,sHD) - pVal(INN,ER,W,K,S,HD))
  # Value-weighted versions: share of the counting-category value (HR/R/RBI/SB,
  # W/K/S/HD) those players produced that counted - an AB by a star weighs more
  # than an AB by a backup. Ratio categories are left out (can go negative).
  hCount <- function(HR,R,RBI,SB) unname(HR/sdH["HR"] + R/sdH["R"] + RBI/sdH["RBI"] + SB/sdH["SB"])
  pCount <- function(W,K,S,HD) unname(W/sdP["W"] + K/sdP["K"] + S/sdP["S"] + HD/sdP["HD"])
  benchH <- benchH %>% mutate(accV = hCount(HR,R,RBI,SB), seaV = hCount(sHR,sR,sRBI,sSB))
  benchP <- benchP %>% mutate(accV = pCount(W,K,S,HD), seaV = pCount(sW,sK,sS,sHD))
  lineup <- full_join(
    benchH %>% group_by(Team) %>% summarize(hLineupEff = sum(AB)/sum(sAB), hValueEff = sum(accV)/sum(seaV),
                                            hBench = sum(benchDFL)),
    benchP %>% group_by(Team) %>% summarize(pLineupEff = sum(INN)/sum(sIP), pValueEff = sum(accV)/sum(seaV),
                                            pBench = sum(benchDFL)),
    by = "Team") %>%
    transmute(Team, hLineupEff, hValueEff, pLineupEff, pValueEff, benchDFL = coalesce(hBench,0) + coalesce(pBench,0))
  seasonResults <- left_join(seasonResults, lineup, by = "Team")
} else {
  warning(str_c(fgbFile, " or ", fgpFile, " missing - no lineup efficiency columns"))
}


# Top FAAB
tfh <- hitters %>% filter(asrc=="faab") %>% select(Player,Pos,Team,DFL)
tfp <- pitchers %>% filter(asrc=="faab") %>% select(Player,Pos,Team,DFL)

topfaab <- bind_rows(tfh,tfp) %>% arrange(-DFL)


# Create averages data points
avpratio <- mean(seasonResults$pratio, na.rm=TRUE)
avdratio <- mean(seasonResults$dratio, na.rm=TRUE)
avfaab <- mean(seasonResults$faab_DFL)
avtrade <- mean(seasonResults$trade_DFL)

# Create protection ratio against preseason predicted numbers
dgFile <- str_c("../",year,"draftGuide.xlsx")
if (file.exists(dgFile)) {
  fcast <- read.xlsx(dgFile,1)
  fcast <- select(fcast,Team,projectedValue=TotalValue)
  fcast$Team <- renameTeams(fcast$Team)
  s2 <- left_join(seasonResults,fcast,by='Team') %>% mutate(projRatio = protect_DFL/projectedValue)
  avprotect <- mean(s2$projRatio, na.rm=TRUE)

  # Draft vs Projection: drafted players' season value against their preseason
  # projected value (DFL) on the guide's position sheets. Drafted players the
  # guide didn't list (deep $1 picks) are left out of both sides.
  posSheets <- intersect(c("C","1B","2B","SS","3B","OF","DH","Other","SP","MR","CL"), getSheetNames(dgFile))
  proj <- bind_rows(lapply(posSheets, function(sh) {
    d <- read.xlsx(dgFile, sh)
    if (is.null(d) || nrow(d) == 0 || !all(c('Player','MLB','DFL') %in% names(d))) return(NULL)
    transmute(d, Player = as.character(Player), MLB = normMlbTeam(MLB), projDFL = as.numeric(DFL))
  })) %>% filter(!is.na(projDFL)) %>% group_by(Player, MLB) %>%
    summarize(projDFL = max(projDFL), .groups = 'drop')
  uniqueNames <- proj %>% group_by(Player) %>% filter(n() == 1) %>% ungroup() %>% select(Player, projDFL)
  pMLB <- players %>% select(playerid, Team, MLB) %>% distinct(playerid, Team, .keep_all = TRUE)
  drafted <- bind_rows(select(hitters, Player, Team, playerid, asrc, DFL),
                       select(pitchers, Player, Team, playerid, asrc, DFL)) %>%
    filter(asrc == 'draft') %>% left_join(pMLB, by = c('playerid','Team')) %>%
    left_join(proj, by = c('Player','MLB')) %>%
    left_join(uniqueNames %>% rename(projByName = projDFL), by = 'Player') %>%
    mutate(projDFL = coalesce(projDFL, projByName))
  print(str_c("Draft vs Projection: ", sum(!is.na(drafted$projDFL)), " of ", nrow(drafted),
              " drafted players found in the draft guide"))
  dproj <- drafted %>% filter(!is.na(projDFL)) %>% group_by(Team) %>%
    summarize(dRatio = sum(DFL) / sum(projDFL))
  avdproj <- mean(dproj$dRatio, na.rm=TRUE)
} else {
  avprotect <- NA
  avdproj <- NA
}

# Function to rank drafted players by DFL value for any team
getDraftedPlayersRanked <- function(team_name, hitters_df = hitters, pitchers_df = pitchers) {
  # Filter drafted hitters for the specified team (Contract = 1 only, not protected players)
  team_hitters <- hitters_df %>%
    filter(Team == team_name & !is.na(dft) & Contract == 1) %>%
    select(Player, Pos, Salary, Contract, DFL, Value) %>%
    distinct()

  # Filter drafted pitchers for the specified team (Contract = 1 only, not protected players)
  team_pitchers <- pitchers_df %>%
    filter(Team == team_name & !is.na(dft) & Contract == 1) %>%
    select(Player, Pos, Salary, Contract, DFL, Value) %>%
    distinct()

  # Combine and sort by Value (descending)
  drafted_players <- bind_rows(team_hitters, team_pitchers) %>%
    distinct() %>%
    arrange(desc(Value))

  # Add rank column
  drafted_players$Rank <- 1:nrow(drafted_players)

  return(drafted_players)
}


aggStats <- tibble(Statistic=c("Protection ROI","Protection vs Projection","Draft ROI","Draft vs Projection","FAAB Value","Trade Value"),
                   Value=c(avpratio,avprotect,avdratio,avdproj,avfaab,avtrade),
                   Description=c(
                     "League average of each team's season value from protected players divided by their total salary. Above 1 means keepers returned more than they cost.",
                     "League average of each team's season value from protected players divided by the value the preseason draft guide projected for them (TotalValue). Below 1 means keepers underperformed their projections.",
                     "League average of each team's season value from players acquired at the draft divided by their draft salaries. Above 1 means draft picks returned more than they cost.",
                     "League average of each team's season value from drafted players divided by the value the preseason draft guide projected for them (position sheets). Drafted players the guide didn't list are excluded. Diagnostic for the projection model: the guide's position-sheet values are not on a consistent dollar scale across years (or between hitters and pitchers), so compare with care.",
                     "League average, per team, of total season value (in DFL dollars) produced by free-agent (FAAB) pickups while on that team.",
                     "League average, per team, of total season value (in DFL dollars) produced by players acquired in trades after the trade. Does not subtract the value of players traded away (see tradeValue on the valueByAcq tab)."))


#Create xlsx with tabbed data
review <- createWorkbook()
headerStyle <- createStyle(halign = "CENTER", textDecoration = "Bold")
csRatioColumn <- createStyle(numFmt = "##0.000")
csMoneyColumn <- createStyle(numFmt = "CURRENCY")

addWorksheet(review,'valueByAcq')
writeData(review,'valueByAcq',seasonResults,headerStyle = headerStyle)
addStyle(review, 'valueByAcq',style = csMoneyColumn,rows = 2:20, cols =3:4,gridExpand = TRUE)
addStyle(review, 'valueByAcq',style = csMoneyColumn,rows = 2:20, cols = 6,gridExpand = TRUE)
addStyle(review, 'valueByAcq',style = csMoneyColumn,rows = 2:20, cols = 8,gridExpand = TRUE)
addStyle(review, 'valueByAcq',style = csMoneyColumn,rows = 2:20, cols = 10,gridExpand = TRUE)
addStyle(review, 'valueByAcq',style = csMoneyColumn,rows = 2:20, cols = 12,gridExpand = TRUE)
addStyle(review, 'valueByAcq',style = csMoneyColumn,rows = 2:20, cols = 14,gridExpand = TRUE)
addStyle(review, 'valueByAcq',style = csMoneyColumn,rows = 2:20, cols = 16,gridExpand = TRUE)
addStyle(review, 'valueByAcq',style = csRatioColumn,rows = 2:20, cols = 7,gridExpand = TRUE)
addStyle(review, 'valueByAcq',style = csRatioColumn,rows = 2:20, cols = 11,gridExpand = TRUE)
for (nm in intersect(c('hLineupEff','hValueEff','pLineupEff','pValueEff'), names(seasonResults)))
  addStyle(review, 'valueByAcq',style = createStyle(numFmt = "0%"),rows = 2:20, cols = which(names(seasonResults) == nm),gridExpand = TRUE)
if ('benchDFL' %in% names(seasonResults))
  addStyle(review, 'valueByAcq',style = csMoneyColumn,rows = 2:20, cols = which(names(seasonResults) == 'benchDFL'),gridExpand = TRUE)

setColWidths(review, 'valueByAcq', cols = 1:25, widths = "auto")

addWorksheet(review,'topFAABers')
writeData(review,'topFAABers',topfaab,headerStyle = headerStyle)
addStyle(review, 'topFAABers',style = csMoneyColumn,rows = 2:250, cols = 4,gridExpand = TRUE)
setColWidths(review, 'topFAABers', cols = 1:25, widths = "auto")

addWorksheet(review,'Summary Stats')
writeData(review,'Summary Stats',aggStats,headerStyle = headerStyle)
addStyle(review, 'Summary Stats',style = csRatioColumn,rows = 2:20, cols = 2,gridExpand = TRUE)
setColWidths(review, 'Summary Stats', cols = 1:2, widths = "auto")
setColWidths(review, 'Summary Stats', cols = 3, widths = 90)
addStyle(review, 'Summary Stats',style = createStyle(wrapText = TRUE, valign = "top"),rows = 2:7, cols = 3,gridExpand = TRUE)

saveWorkbook(review,str_c("../",year,"seasonReview.xlsx"),overwrite = TRUE)

