library(tidyr)
library(baseballr)
library(RSQLite)

glogs <- fg_batter_game_logs(playerid = 19755, year = 2024)


# What about storing the 2-week data everyday in a DB and then generate my charts from there?

conn <- dbConnect(RSQLite::SQLite(), "DAFL.db")

# Player, pDFL, hotscore, date -from htrend.csv
htrend <- AllH %>% select(playerid,Player,hotscore) %>% mutate(Date=today())
ptrend <- AllP %>% select(playerid,Player,hotscore) %>% mutate(Date=today())
alltrend <- rbind(htrend,ptrend)

#dbWriteTable(conn,"Trending",alltrend,append=TRUE)

dbGetQuery(conn, "SELECT count(*) FROM Trending")
df <- dbGetQuery(conn, "SELECT * FROM Trending")


trowcount <- dbGetQuery(conn, "SELECT count(*) FROM Trending where Date = ?",params = c(today()))
asnum <- as.numeric(trowcount[[1]])
if (asnum == 0) {
  dbWriteTable(conn,"Trending",alltrend,append=TRUE)
}

