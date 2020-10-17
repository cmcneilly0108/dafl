# Create accrued file
#   http://dafl.baseball.cbssports.com/stats/stats-main/team:all/ytd:f/accrued/
#   2019Accrued.csv
# Remember to update the week 1 rosters file
#   http://dafl.baseball.cbssports.com/stats/stats-main/team:all/period-1:f/salary%20info/
#   2019DraftResults.csv
# Trades file!
#   http://dafl.baseball.cbssports.com/transactions/all/trades/?print_rows=9999
#   2019trades.csv


library("openxlsx")
library("stringr")
library("dplyr")
library("XML")
library("ggplot2")
library("reshape2")
library("tidyr")
library("splitstackshape")

source("./daflFunctions.r")

cleanRosters <- function(pl) {
  colnames(pl) <- c('Avail','Player','E1','Pos','E2','Salary','Contract','S1','S2','S3','S4','S5','S6','Rank','E3')
  players <- select(pl,-Rank,-E1,-E2,-E3) %>%
    filter(!(Player %in% c('Player','TOTALS')))
  #players <- mutate(players,porh=ifelse((Avail %in% c('Batters','Pitchers')),Avail,NA)) %>% 
  #  fill(porh) %>% filter(!(Avail %in% c('Batters','Pitchers')))
  players <- mutate(players,porh=ifelse((Avail %in% c('Batters','Pitchers')),Avail,NA)) %>% 
    fill(porh) %>% filter(!(Avail %in% c('Batters','Pitchers')))
  players <- mutate(players,Team=ifelse((Player %in% c('')),Avail,NA)) %>% 
    fill(Team) %>% filter(!(Player %in% c('')))
  # players <- mutate(players,Avail=ifelse((str_length(S1)==0),Player,NA)) %>% fill(Avail) %>%
  #   filter(!(str_length(S1)==0))
  players <- mutate(players, MLB = pullMLB(Player))
  players$Player <- unlist(lapply(players$Player,stripName))
  players <- addPlayerid(players)
}

year <- "2020"


# http://dafl.baseball.cbssports.com/stats/stats-main/team:all/ytd:f/accrued/
#pl <- read.csv("2019Accrued.csv",header=FALSE,stringsAsFactors=FALSE)
pl <- read.csv(str_c(year,"Accrued.csv"),header=FALSE,stringsAsFactors=FALSE)
players <- cleanRosters(pl)

hitters <- filter(players,porh=='Batters') %>%  select(Player,Pos,Salary,Contract,S1,S2,S3,S4,S5,S6,Team,playerid)
colnames(hitters) <- c('Player','Pos','Salary','Contract','AB','H','HR','R','RBI','SB','Team','playerid')
hitters <- mutate(hitters,AB=as.integer(AB),H=as.integer(H),HR=as.integer(HR),R=as.integer(R),
                  RBI=as.integer(RBI),SB=as.integer(SB),AVG=H/AB)
pitchers <- filter(players,porh=='Pitchers') %>%  select(Player,Pos,Salary,Contract,S1,S2,S3,S4,S5,S6,Team,playerid)
colnames(pitchers) <- c('Player','Pos','Salary','Contract','ER','INN','W','S','K','HD','Team','playerid')
pitchers <- mutate(pitchers,ER=as.integer(ER),INN=as.numeric(INN),W=as.integer(W),S=as.integer(S),
                  K=as.integer(K),HD=as.integer(HD),ERA=(ER/INN)*9)

# hotscores
r <- hotScores(hitters,pitchers,tm=TRUE)
oh <- r[[1]]
op <- r[[2]]

# convert to DFL
tz <- sum(oh$zScore) + sum(op$zScore)
td <- 300*16
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

# Week 1 rosters
# http://dafl.baseball.cbssports.com/stats/stats-main/team:all/period-1:f/salary%20info/
drosters <- read.csv(str_c(year,"DraftResults.csv"),header=FALSE,stringsAsFactors=FALSE)
colnames(drosters) <- c('Avail','Player','Pos','Salary','Contract','Rank','E1')
drosters <- select(drosters,-Rank,-E1) %>%
  filter(!(Player %in% c('Player','TOTALS')))
drosters <- mutate(drosters,porh=ifelse((Avail %in% c('Batters','Pitchers')),Avail,NA)) %>% 
  fill(porh) %>% filter(!(Avail %in% c('Batters','Pitchers')))
drosters <- mutate(drosters,Team=ifelse((Player %in% c('')),Avail,NA)) %>% 
  fill(Team) %>% filter(!(Player %in% c('')))
drosters <- mutate(drosters, MLB = pullMLB(Player))
drosters$Player <- unlist(lapply(drosters$Player,stripName))
drosters <- addPlayerid(drosters)
drosters <- mutate(drosters,asrc='protect',Salary=as.integer(Salary),Contract=as.integer(Contract))
dc <- select(drosters,playerid,asrc)

# join and create w1 - which players were on week 1 roster?
hitters <- left_join(hitters,dc)
hitters$asrc <- replace(hitters$asrc,is.na(hitters$asrc),'faab')
hitters$asrc <- ifelse(hitters$asrc=='protect' & hitters$Contract==1,'draft',hitters$asrc)
pitchers <- left_join(pitchers,dc)
pitchers$asrc <- replace(pitchers$asrc,is.na(pitchers$asrc),'faab')
pitchers$asrc <- ifelse(pitchers$asrc=='protect' & pitchers$Contract==1,'draft',pitchers$asrc)

# Load trades file
# http://dafl.baseball.cbssports.com/transactions/all/trades/?print_rows=9999
trades <- read.csv(str_c(year,"trades.csv"),stringsAsFactors=FALSE)

# new - need to filter for only 'Traded' rows
trades <- filter(trades,str_detect(Players,'Traded'))

trades <- cSplit(trades,"Players",sep="\n",direction="long")
trades$Player <- unlist(lapply(trades$Players,swapName3))

trades$fTeam <- unlist(lapply(trades$Players,tradeFrom))
trades <- select(trades,Team,Player,Traded=Effective,fTeam)
hitters <- left_join(hitters,trades,by=c('Team','Player'))
pitchers <- left_join(pitchers,trades,by=c('Team','Player'))
hitters$asrc <- ifelse(is.na(hitters$Traded),hitters$asrc,'trade')
pitchers$asrc <- ifelse(is.na(pitchers$Traded),pitchers$asrc,'trade')

# Lets add injuries!
injured <- read.csv(str_c(year,"trades.csv"),stringsAsFactors=FALSE)
injured <- filter(injured,str_detect(Players,'Injured'))
numinj <- injured %>% count(Team) %>% rename(injured = n)
#numinj <- numinj %>% mutate(irank = rank(injured))

# Create traded away value
taH <- filter(hitters,!is.na(fTeam)) %>% group_by(fTeam) %>% 
  summarize(taway =sum(DFL)) %>% select(Team=fTeam,taway)
taP <- filter(pitchers,!is.na(fTeam)) %>% group_by(fTeam) %>% summarize(taway =sum(DFL)) %>% select(Team=fTeam,taway)
ta <- rbind(taH,taP) %>% group_by(Team) %>% summarize(taway =sum(taway))

hitters <- mutate(hitters, Salary = as.integer(Salary))
pitchers <- mutate(pitchers, Salary = as.integer(Salary))

dr2 <- select(drosters,Player,Team,Salary,Contract)
hitters <- left_join(hitters,dr2,by=c('Player','Team'))
hitters <- mutate(hitters,Salary=ifelse((is.na(Salary.y) | Salary.y == 1),Salary.x,Salary.y))
hitters <- mutate(hitters,Contract=ifelse(is.na(Contract.y),Contract.x,Contract.y))
pitchers <- left_join(pitchers,dr2,by=c('Player','Team'))
pitchers <- mutate(pitchers,Salary=ifelse((is.na(Salary.y) | Salary.y == 1),Salary.x,Salary.y))
pitchers <- mutate(pitchers,Contract=ifelse(is.na(Contract.y),Contract.x,Contract.y))

hitters <- mutate(hitters, Value = DFL - Salary)
pitchers <- mutate(pitchers, Value = DFL - Salary)
hitters <- arrange(hitters,-Value)
pitchers <- arrange(pitchers,-Value)


#Load DAFL standings file
standings <- read.csv("DAFLWeeklyStandings.csv",stringsAsFactors=FALSE)
standings$Rank <- as.numeric(str_extract(standings$Rank,'[0-9]+'))
final <- filter(standings,Week==max(Week)) %>% select(Actual=Rank,Short=Team)

nicks <- read.csv("nicknames.csv",stringsAsFactors=FALSE)
fstand <- inner_join(final,nicks,by=c('Short')) %>% select(-Short) 


# Create final data frame
srcH <- select(hitters,Team,asrc,DFL,Salary)
srcP <- select(pitchers,Team,asrc,DFL,Salary)
src <- rbind(srcH,srcP)
#f <- src %>% group_by(Team,asrc) %>% summarize(srcDFL=sum(DFL))
#seasonResults <- dcast(f,Team ~ asrc)
f2 <- src %>% group_by(Team,asrc) %>% summarize(Sal=sum(Salary),DFL=sum(DFL))
meltf <- melt(f2, id.vars=c('Team','asrc'))
seasonResults <- dcast(meltf,Team ~ asrc + variable)
seasonResults <- select(seasonResults,-trade_Sal,-faab_Sal)

seasonResults$protect_DFL <- replace(seasonResults$protect_DFL,is.na(seasonResults$protect_DFL),0)
seasonResults$faab_DFL <- replace(seasonResults$faab_DFL,is.na(seasonResults$faab_DFL),0)
seasonResults$trade_DFL <- replace(seasonResults$trade_DFL,is.na(seasonResults$trade_DFL),0)
seasonResults <- left_join(seasonResults,ta) %>% mutate(tradeValue = trade_DFL-taway)

seasonResults <- left_join(seasonResults,fstand)

seasonResults <- seasonResults %>% replace_na(list(tradeValue=0))
seasonResults <- mutate(seasonResults,overall = draft_DFL+faab_DFL+protect_DFL+trade_DFL,
                        drank = rank(-draft_DFL), pratio = protect_DFL/protect_Sal,
                        dratio = draft_DFL/draft_Sal,
                        frank = rank(-faab_DFL),prank = rank(-protect_DFL),trank = rank(-tradeValue)) %>%
  select(Team,Actual,overall,protect_DFL,prank,protect_Sal,pratio,draft_DFL,drank,draft_Sal,dratio,faab_DFL,frank,trade_DFL,trank,tradeValue) %>% arrange(-overall)

# add injured data -  mutate(irank = rank(injured))
seasonResults <- left_join(seasonResults,numinj)
seasonResults <- seasonResults %>% replace_na(list(injured=0)) %>%  mutate(irank = rank(injured))

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

setColWidths(review, 'valueByAcq', cols = 1:25, widths = "auto")

saveWorkbook(review,str_c(year,"seasonReview.xlsx"),overwrite = TRUE)


# Deep Dive
lcp <- filter(pitchers, Team=='Liquor Crickets') %>% 
  select(Player,Pos,Salary=Salary.x,Contract,DFL,asrc) %>% 
  mutate(Value = DFL - Salary) %>% arrange(-Value)

# BUG - no K's in pitcher file!!!
# BUG - pitcher DFL values don't make sense.  Hopefully because of above

# # Read in draftAnalysis
# da <- read.xlsx("draftAnalysis.xlsx",2)
# da <- select(da,Team=Team,prep=tDFL)
# seasonResults2 <- inner_join(seasonResults,da)
# da <- read.xlsx("draftAnalysis.xlsx",3)
# da <- select(da,Team=Team,pred=tDFL)
# seasonResults2 <- inner_join(seasonResults2,da)
#
# seasonResults2 <- mutate(seasonResults2,deltap = protect - prep,deltad = draft-pred) %>%
#   select(Team,overall,protect,prank,prep,deltap,draft,drank,pred,deltad,faab,frank,trade,trank) %>% arrange(-overall)
#
