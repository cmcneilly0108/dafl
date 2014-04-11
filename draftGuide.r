# Working

library("xlsx")
library("stringr")
library("dplyr")

swapName <- function(n){
  comma <- str_locate(n,',')
  ln <- str_sub(n,1,comma-1)
  fn <- str_sub(n,comma+2,-1)
  nn <- str_join(fn,ln,sep=" ",collapse=NULL)
  nn[1] 
}

#Load Rotoworld data, convert name to chars, and remove some columns
hitters <- read.xlsx("projections_ALLHIT_rotoworld140328.xlsx",1)
hitters$Player <- as.character(hitters$Player)
pitchers <- read.xlsx("projections_ALLPIT_rotoworld140328.xlsx",1)
pitchers$Player <- as.character(pitchers$Player)
hitters <- hitters[,c(1,2,3,4,5,9,10,11,12,13,14,15,19,20)]
pitchers <- pitchers[,c(1,2,3,4,5,9,10,11,12,13,15,16,17)]

#Load protection spreadsheet from Craig, re-arrange Name so I can match
protects <- read.xlsx("2014+All+Protection+Lists.xls",1)
protects$Name <- as.character(protects$Name)
protects$Player <- unlist(lapply(protects$Name,swapName))

#remove protected players from available lists
aHitters <- hitters[!(hitters$Player %in% protects$Player),]
aPitchers <- pitchers[!(pitchers$Player %in% protects$Player),]

#Create separate tabs by position
a1B <- filter(aHitters, grepl("1B",Pos))
aC <- filter(aHitters, grepl("C",Pos))
a2B <- filter(aHitters, grepl("2B",Pos))
aSS <- filter(aHitters, grepl("SS",Pos))
a3B <- filter(aHitters, grepl("3B",Pos))
aOF <- filter(aHitters, grepl("OF",Pos))
aDH <- filter(aHitters, Pos == 'DH')
aSP <- filter(aPitchers, grepl("S",Pos))
aRP <- filter(aPitchers, grepl("R",Pos))

# Forecast each teams totals - run a standings analysis plus additional category of $$
proTH <- inner_join(protects,hitters,by=c('Player'),copy=FALSE)
proTP <- inner_join(protects,pitchers,by=c('Player'),copy=FALSE)

# Create Cricket tab with forecasts
myHitters <- proTH %.% filter(Team == 'Liquor Crickets')
myPitchers <- proTP %.% filter(Team == 'Liquor Crickets')
myTeam <- rbind_all(list(myHitters,myPitchers))
myTeam <- select(myTeam,c(Name,Contract,Salary,AB,HR,RBI,R,SB,AVG,W,IP,K,HLD,SV,ERA,DFL))


# Group and Summarize each team's protection list
ltothits <- proTH %.% group_by(Team) %.% summarize(HR = sum(HR), RBI = sum(RBI), 
                                                   R = sum(R), SB = sum(SB),  AVG = sum(H)/sum(AB))
ltotpits <- proTP %.% group_by(Team) %.% summarize(W = sum(W), K = sum(K), 
                                                   SV = sum(SV), H = sum(HLD),  ERA = sum(IP * ERA)/sum(IP))

ltotpits$pW <- rank(ltotpits$W)
ltotpits$pK <- rank(ltotpits$K)
ltotpits$pSV <- rank(ltotpits$SV)
ltotpits$pH <- rank(ltotpits$H)
ltotpits$pERA <- rank(ltotpits$ERA)

ltothits$pHR <- rank(ltothits$HR)
ltothits$pRBI <- rank(ltothits$RBI)
ltothits$pR <- rank(ltothits$R)
ltothits$pSB <- rank(ltothits$SB)
ltothits$pAVG <- rank(ltothits$AVG)

lStandings <- inner_join(ltothits,ltotpits,by=c('Team'),copy=FALSE)

tSal <- protects %.% group_by(Team) %.% summarize(dLeft = 260 - sum(Salary))
lStandings$dLeft <- tSal$dLeft
lStandings$pLeft <- rank(lStandings$dLeft)

lStandings$tPoints <- with(lStandings, pHR + pRBI + pR + pSB + pAVG + pW + pK + pH + pSV + pERA + pLeft)
leagueProjection <- arrange(lStandings,desc(tPoints))
leagueProjection <- select(leagueProjection,-c(pHR,pRBI,pR,pSB,pAVG,pW,pSV,pH,pK,pERA,pLeft))

# Analyze my needs based on winning numbers from last year
# Load goals, compare my totals to goals, bar chart
goals2013 <- read.csv(file="2013goals.csv")
goals2013$Category <- as.character(goals2013$Category)

currentLC <- leagueProjection %.% filter(Team == 'Liquor Crickets') %.%
  select(-c(Team,dLeft,tPoints))
currentLC <- as.data.frame(t(currentLC))
currentLC$Category <- rownames(currentLC)
colnames(currentLC) <- c('Current','Category')
myGoals <- inner_join(goals2013,currentLC)
myGoals$PctCmp <- with(myGoals,Current/Goal)
myGoals$StillNeed <- with(myGoals,Goal-Current)
myGoals <- arrange(myGoals,PctCmp)

#Create xlsx with tabbed data
available <- createWorkbook()
wslp <- createSheet(wb=available,sheetName='Projected Standings')
wsmt <- createSheet(wb=available,sheetName='My Team')
wsmg <- createSheet(wb=available,sheetName='My Needs')
wsc <- createSheet(wb=available,sheetName='C')
ws1B <- createSheet(wb=available,sheetName='1B')
ws2B <- createSheet(wb=available,sheetName='2B')
wsSS <- createSheet(wb=available,sheetName='SS')
ws3B <- createSheet(wb=available,sheetName='3B')
wsOF <- createSheet(wb=available,sheetName='OF')
wsDH <- createSheet(wb=available,sheetName='DH')
wsSP <- createSheet(wb=available,sheetName='SP')
wsRP <- createSheet(wb=available,sheetName='RP')

addDataFrame(x=leagueProjection,sheet=wslp)
addDataFrame(x=myTeam,sheet=wsmt)
addDataFrame(x=myGoals,sheet=wsmg)
addDataFrame(x=aC,sheet=wsc)
addDataFrame(x=a1B,sheet=ws1B)
addDataFrame(x=a2B,sheet=ws2B)
addDataFrame(x=aSS,sheet=wsSS)
addDataFrame(x=a3B,sheet=ws3B)
addDataFrame(x=aOF,sheet=wsOF)
addDataFrame(x=aDH,sheet=wsDH)
addDataFrame(x=aSP,sheet=wsSP)
addDataFrame(x=aRP,sheet=wsRP)

saveWorkbook(available,"2004DAFLDraft.xlsx")

# Ideas
# Sheets for my team and projected results
