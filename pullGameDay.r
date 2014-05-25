# Personal BB data DB
# Scrape from GameDay
# Populate SQLite table
# Decide what to store in DB

# Want to calculate player data YTD, last 14 days
# Update automatically via cron job

# Check Events, make sure no new ones

# test pull
library(pitchRx)
dat <- scrape(start="2013-06-01", end="2013-06-01")

hitter <- dat$atbat
hitter$x1B <- with(hitter,ifelse(event=='Single',1,0))
hitter$x2B <- with(hitter,ifelse(event=='Double',1,0))
hitter$x3B <- with(hitter,ifelse(event=='Triple',1,0))
hitter$HR <- with(hitter,ifelse(event=='Home Run',1,0))
hitter$BB <- with(hitter,ifelse(event=='Walk' | event=='Intent Walk',1,0))
hitter$K <- with(hitter,ifelse(event=='Strikeout',1,0))
hitter$H <- ifelse(hitter$x1B == 1 | hitter$x2B == 1 | hitter$x3B == 1 | hitter$HR == 1,1,0)
hitter$AB <- ifelse(hitter$event %in% c('Walk','Sac Bunt','Sac Fly','Hit By Pitch','Intent Walk','Runner Out'),0,1)

hTots <- hitter %.% group_by(batter) %.% summarize(AB=sum(AB),x1B=sum(x1B),x2B=sum(x2B),x3B=sum(x3B),
                                                   HR=sum(HR),BB=sum(BB),K=sum(K),H=sum(H))
hTots$id <- as.character(hTots$batter)

runner <- dat$runner
runner$id <- as.character(runner$id)
runner$SB <- with(runner,ifelse(event=='Stolen Base 3B' | event=='Stolen Base 2B' | event=='Stolen Base Home',1,0))
runner$CS <- with(runner,ifelse(event=='Caught Stealing 3B' | event=='Caught Stealing 2B' | event=='Caught Stealing Home',1,0))
runner$rbi[is.na(runner$rbi)] <- 0
runner$score[is.na(runner$score)] <- 0
runner$RBI <- with(runner,ifelse(rbi=='T',1,0))
runner$R <- with(runner,ifelse(score=='T',1,0))

rTots <- runner %.% group_by(id) %.% summarize(RBI=sum(RBI),R=sum(R),SB=sum(SB),CS=sum(CS))

dailyH <- inner_join(hTots,rTots,by=c('id'))

# pitching stats - GS, IP, W, S, ER, QS?, HLD
# done - K, BB
# at home, grab a day's worth of data and write to csv file