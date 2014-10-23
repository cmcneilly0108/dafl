# 2 projection files - fangraphs
# 2 FA files - cbssports
# 1 prospects file - fangraphs
# update Week
# update Pitchers YTD totals (for holds)
# update fangraphs bullpen URL

#TBD
# Automatically download projection files
# Better tracking of prospects
# Find opportunities/threats for points - find slopes of pre and post competitors, lowest slopes are easiest/most dangerous

library("xlsx")
library("stringr")
library("dplyr")
library("XML")
library("ggplot2")
library("reshape2")


source("./daflFunctions.r")


# Data that needs to be update manually
Week <- 25
tWeeks <-27
bp <- "http://www.fangraphs.com/fantasy/bullpen-report-september-17-2014/"
ytdf <- "AllP20140918.csv"
#prospectf <- "prospects0801.csv"
# End manual update data


#Create Charts
standings <- read.csv("DAFLWeeklyStandings.csv")
standings$Rank <- as.numeric(str_extract(standings$Rank,'[0-9]+'))
leaders <- standings %>% filter(Week == max(Week), Rank <= 5 | Team == 'Cricket') %>% select(Team)
#l2 <- ifelse('Cricket' %in% leaders$Team,leaders$Team,append(leaders$Team,'Cricket'))
# add category rank columns
s2 <- standings %>% group_by(Week) %>% mutate(rHR = rank(HR),rR = rank(R),rSB = rank(SB),
                                              rRBI = rank(RBI),rBA = rank(BA),rW = rank(W)
                                              ,rS = rank(S),rHD = rank(HD),rK = rank(K)
                                              ,rERA = rank(-ERA))
s2 <- mutate(s2,TP=rHR+rR+rSB+rRBI+rBA+rW+rS+rHD+rK+rERA)
# create line graph
g1 <- ggplot(data=filter(s2,Team %in% leaders$Team), 
             aes(x=Week, y=TP, group=Team, shape=Team, color=Team)) + geom_line(size=1.2) + 
  geom_point(size=4) + labs(title='Top 5 plus Crickets',y='Total Points')
s3 <- melt(s2,c('Team','Week'))
g2 <- ggplot(data=filter(s3,Team=='Cricket',variable %in% c('rHR','rR','rRBI','rBA','rSB')), 
             aes(x=Week, y=value, group=variable, shape=variable,color=variable)) + 
  geom_line(size=1.2) + geom_point(size=4) + labs(title='Crickets Hitting by Week',y='Points')
g3 <- ggplot(data=filter(s3,Team=='Cricket',variable %in% c('rW','rK','rS','rHD','rERA')), 
             aes(x=Week, y=value, group=variable, shape=variable,color=variable)) + 
  geom_line(size=1.2) + geom_point(size=4) + labs(title='Crickets Pitching by Week',y='Points')
pdf("DAFLcharts.pdf")
print(g1)
print(g2)
print(g3)
dev.off()


# Year End Totals
sTots <- list()

l1 <- loadPast2()
eras <- l1[[1]]
avgs <- l1[[2]]
r3 <- l1[[3]]
# Load Master file
master <- read.csv("master_14.csv",stringsAsFactors=FALSE)
master <- rename(master,playerid=fg_id,Pos = mlb_pos,MLB=mlb_team,Player=mlb_name)


#Load Steamer rest of season projections
hitters <- read.fg("steamerHROS.csv")
hitters$pSGP <- hitSGP(hitters)
hitters <- select(hitters,-Player,-MLB,-Pos)


pitchers <- read.fg("steamerPROS.csv")
pitchers <- select(pitchers,-Player,-MLB,-Pos)

#Load All Players - Extract Free Agents
Allhitters <- read.cbs("AllHitters.csv")

AllH <- inner_join(Allhitters,hitters,by=c('playerid'),copy=FALSE)

Allpitchers <- read.cbs("AllPitchers.csv")
Allpitchers$Pos <- with(Allpitchers,ifelse(Pos=='SP','SP',ifelse(S>HD,'CL',ifelse(HD>0,'MR','SP'))))

ytdp <- read.cbs(ytdf)
ytdp <- select(ytdp,playerid,HD) %>% rename(yHLD = HD)


AllP <- inner_join(Allpitchers,pitchers,by=c('playerid'),copy=FALSE)
AllP <- left_join(AllP,ytdp,by=c('playerid'),copy=FALSE)
# give 60/40 weight to YTD/3WKS
AllP$pHLD <- with(AllP,round(((HD/4)*(tWeeks-Week)*.4)+((yHLD/Week)*(tWeeks-Week)*.6)),0)
AllP$pSGP <- pitSGPh(AllP)
  
# GENERATE DFL dollar values for all players
#Set parameters
nteams <- 15
tdollars <- (nteams * (260+50)) * (1-(Week/tWeeks))
# 63/37 split - just guessing
pdollars <- round(tdollars*0.37)
hdollars <- tdollars - pdollars
# 13/12 hitters/pitchers based on rosters on 5/29/14
nhitters <- 12
npitchers <- 13
thitters <- (nhitters * nteams) + 40
tpitchers <- (npitchers * nteams) + 40
# Only value a certain number of players
bhitters <- filter(AllH,rank(-pSGP) <= thitters)
hitSGP <- round(sum(bhitters$pSGP))
bpitchers <- filter(AllP,rank(-pSGP) <= tpitchers)
pitSGP <- round(sum(bpitchers$pSGP))
hsgpd <- hdollars/hitSGP
psgpd <- pdollars/pitSGP
# Create dollar amounts
bhitters$pDFL <- bhitters$pSGP * hsgpd
bpitchers$pDFL <- bpitchers$pSGP * psgpd
bhitters <- select(bhitters,playerid,pDFL)
bpitchers <- select(bpitchers,playerid,pDFL)
# find min $, subtract from everyone, then multiply everyone by %diff
# Normalize for auction - three iterations
hmin <- min(bhitters$pDFL) - 1
hlost <- hmin * thitters
bhitters$pDFL <- (bhitters$pDFL - hmin) * (hdollars/(hdollars - hlost))
hmin <- min(bhitters$pDFL) - 1
hlost <- hmin * thitters
bhitters$pDFL <- (bhitters$pDFL - hmin) * (hdollars/(hdollars - hlost))
hmin <- min(bhitters$pDFL) - 1
hlost <- hmin * thitters
bhitters$pDFL <- (bhitters$pDFL - hmin) * (hdollars/(hdollars - hlost))

pmin <- min(bpitchers$pDFL) - 1
plost <- pmin * tpitchers
bpitchers$pDFL <- (bpitchers$pDFL - pmin) * (pdollars/(pdollars - plost))
pmin <- min(bpitchers$pDFL) - 1
plost <- pmin * tpitchers
bpitchers$pDFL <- (bpitchers$pDFL - pmin) * (pdollars/(pdollars - plost))
pmin <- min(bpitchers$pDFL) - 1
plost <- pmin * tpitchers
bpitchers$pDFL <- (bpitchers$pDFL - pmin) * (pdollars/(pdollars - plost))


# Incorporate scores back into AllH, AllP
AllH <- left_join(AllH,bhitters,by=c('playerid'))
AllP <- left_join(AllP,bpitchers,by=c('playerid'))
AllH$pDFL <- replace(AllH$pDFL,is.na(AllH$pDFL),0)
AllP$pDFL <- replace(AllP$pDFL,is.na(AllP$pDFL),0)


# Create Free Agents
FAH <- filter(AllH,Team == 'Free Agent')
FAH <- select(FAH,-Team)
FAP <- filter(AllP,Team == 'Free Agent')
FAP <- select(FAP,-Team)

#Load Current Roster
myteam <- pullTeam('Liquor Crickets')
mh <- myteam[[1]]
mp <- myteam[[2]]
  
# Create worksheets
allsp <- FAP %>% arrange(-pDFL,-pSGP) %>% filter(pHLD==0,pSV==0, pGS > 0) %>%
  select(Player,Pos,pDFL,pSGP,Rank,pW,pSO,pERA,pK.9,pFIP,pGS,W,K,S,HD,ERA)

allClosers <- FAP %>% arrange(-pSV,-S,-pDFL) %>% filter(pSV>0) %>%
  select(Player,Pos,pDFL,pSGP,Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pFIP,W,K,S,HD,ERA)

allHolds <- FAP %>% filter(pHLD>0, pK.9 > 8.0, pBB.9 < 3.5) %>% 
  arrange(-pHLD,-pDFL) %>%
  select(Player,Pos,pDFL,pSGP, Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pBB.9,W,K,S,HD,ERA)

TopFAH <- group_by(FAH,Pos) %>% arrange(Pos,-pDFL,-pSGP) %>% filter(rank(-pSGP) <= 5) %>%
  select(Player,Pos,pDFL,pSGP, Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)
      
# Closer report
c <- readHTMLTable(bp,stringsAsFactors=F)

f <- lapply(c,function(x) {is.data.frame(x) && ncol(x) == 5})
c2 <- c[unlist(f)]
crep <- c2[[1]]

colnames(crep) <- c(' ','Closer','First','Second','DL/Minors')
crep <- crep[-1,]

#crep <- readHTMLTable(bp, header=T, which=15,stringsAsFactors=F)
t <- data.frame(crep$Closer,10)
t2 <- data.frame(crep$First,5)
t3 <- data.frame(crep$Second,2)
colnames(t) <- c('Player','Score')
colnames(t2) <- c('Player','Score')
colnames(t3) <- c('Player','Score')
crep <- rbind_list(t,t2,t3)
crep$Player <- enc2native(crep$Player)
availCL <- inner_join(crep,FAP,by=c('Player'),copy=FALSE) %>% arrange(-pDFL) %>% 
  select(Player,pDFL,pSGP, Score,Rank,pSV,pHLD,pW,pSO,pERA,pK.9,pBB.9,pGS,W,K,S,HD,ERA)


# TALENT Tab - Calculate total SGPs per team, rank
nicks <- read.csv("nicknames.csv")
st <- filter(standings,Week == max(Week)) %>% mutate(Short = Team, Actual = Rank) %>% 
  select(Short,Actual) %>% inner_join(nicks,by=c('Short'))

RH <- filter(AllH,Team != 'Free Agent') %>% group_by(Team) %>% summarize(hDFL = sum(pDFL))
RP <- filter(AllP,Team != 'Free Agent') %>% group_by(Team) %>% summarize(piDFL = sum(pDFL))
RTot <- inner_join(RH,RP,by=c('Team')) %>% 
  mutate(tDFL = hDFL + piDFL,hRank = rank(-hDFL),pRank = rank(-piDFL)) %>% 
  inner_join(st,by=c('Team')) %>%
  select(Team,hDFL,hRank,piDFL,pRank,tDFL,Actual) %>% arrange(-tDFL)


#Load minor league stats
mhitters <- read.csv("minHitters.csv")
mhitters$Player <- as.character(mhitters$Name)
FAPHp <- inner_join(mhitters,FAH,by=c('Player'),copy=FALSE) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,Team,Age,Pos,pDFL,pSGP, Rank,pHR,pRBI,pR,pSB,pAVG,HR.x,RBI.x,R.x,SB.x,AVG)

mpitchers <- read.csv("minPitchers.csv")
mpitchers$Player <- as.character(mpitchers$Name)
FAPPp <- inner_join(mpitchers,FAP,by=c('Player'),copy=FALSE) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,Team,Age,Pos,pDFL,pSGP,Rank,pW,pSO,pERA,pK.9,pFIP,pGS,W.x,SO,SV,ERA.x)


#Create xlsx with tabbed data
wkly <- createWorkbook()
tabs <- list()
tabs[[length(tabs)+1]] <- list('Talent',RTot)
tabs[[length(tabs)+1]] <- list('My Hitters',mh)
tabs[[length(tabs)+1]] <- list('My Pitchers',mp)
tabs[[length(tabs)+1]] <- list('Top Hitters',TopFAH)
tabs[[length(tabs)+1]] <- list('SP',allsp)
tabs[[length(tabs)+1]] <- list('Cl',allClosers)
tabs[[length(tabs)+1]] <- list('FanCl',availCL)
tabs[[length(tabs)+1]] <- list('Hld',allHolds)
tabs[[length(tabs)+1]] <- list('PP - P',FAPPp)
tabs[[length(tabs)+1]] <- list('PP - H',FAPHp)

lapply(tabs,addSheet,wkly)
saveWorkbook(wkly,"weeklyUpdate.xlsx")


#ad hoc queries

# For a position, who has surplus?
f <- AllH %>% filter(Pos == 'C',pSGP > 5) %>% group_by(Team) %>% summarize(nGood = length(Team))
f2 <- AllH %>% filter(Pos == '1B') %>% group_by(Team) %>% summarize(nTotal = length(Team))
ff <- left_join(f2,f,by=c('Team')) %>% arrange(-nGood,-nTotal)

f <- AllP %>% filter(pSV > 10) %>% group_by(Team) %>% summarize(nGood = length(Team)) %>% arrange(-nGood)

#Find out what a team has that I can use
pullTeam('clowndog & banjo')[[1]]

#Find players by position who can help immediately
FAH %>% filter(Pos == 'C',pSGP > 2, pAVG > 0.25) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,Pos,pDFL,pSGP, Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)

FAH %>% filter(pSGP > 8, BA > 0.26) %>% arrange(-pSGP) %>% 
  select(Player,Pos,pSGP, Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)

# Filters by strong K's and low BB's
FAP %>% filter(pHLD>10, pK.9 > 8.0, pBB.9 < 3.5) %>% arrange(-pDFL,-pHLD,-pSGP) %>%
  select(Player,Pos,pDFL,pSGP, Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pBB.9,W,K,S,HD,ERA)

# Top FA in a stat
FAH %>% arrange(-pDFL,-pSGP) %>% filter(pHR > 9, pDFL > 5) %>%
  select(Player,Pos,pDFL,pSGP, Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)

FAP %>% arrange(-pDFL,-pSGP) %>% filter(QS>1,pBB.9 < 3.5) %>%
select(Player,Pos,pDFL,pSGP, Rank,pW,pSO,pERA,pK.9,pBB.9,pGS,W,K,S,HD,ERA)

#game <- "http://www.baseball-reference.com/boxes/TBA/TBA201405220.shtml"
#c <- readHTMLTable(game,stringASFactors=F)

#li <- AllP %>% filter(Team != 'Free Agent') %>% group_by(Team,Pos) %>% 
#  summarize(Count = length(Pos))

