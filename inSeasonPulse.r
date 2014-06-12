# 2 projection files - fangraphs
# 2 FA files - cbssports
# 1 prospects file - rotoworld
# update Week
# update ttWx
# update fangraphs bullpen URL

#TBD
# Remove gVAL
# Automatically download projection files
# Address position scarcity in $$ - too many holds guys
# Better tracking of prospects

library("xlsx")
library("stringr")
library("dplyr")
library("XML")
library(ggplot2)


source("./daflFunctions.r")


# Data that needs to be update manually
Week <- 11
bp <- "http://www.fangraphs.com/fantasy/bullpen-report-june-11-2014/"
ytdf <- "AllP20140612.csv"

# End manual update data


# Year End Totals
sTots <- list()

l1 <- loadPast2()
eras <- l1[[1]]
avgs <- l1[[2]]
r3 <- l1[[3]]

#Load Steamer rest of season projections
hitters <- read.csv("steamerHROS.csv")
hitters$SGP <- hitSGP(hitters)
colnames(hitters) <- str_join('p',colnames(hitters))
hitters$Player <- as.character(hitters$pName)

pitchers <- read.csv("steamerPROS.csv")
colnames(pitchers) <- str_join('p',colnames(pitchers))
pitchers$Player <- as.character(pitchers$pName)

#Load All Players - Extract Free Agents
Allhitters <- read.csv("AllHitters.csv",skip=1)
Allhitters$Player <- as.character(Allhitters$Player)
Allhitters <- mutate(Allhitters, MLB = pullMLB(Player))
Allhitters <- mutate(Allhitters, Pos = pullPos(Player))
Allhitters$Player <- unlist(lapply(Allhitters$Player,swapName2))

AllH <- inner_join(Allhitters,hitters,by=c('Player'),copy=FALSE)

Allpitchers <- read.csv("AllPitchers.csv",skip=1)
Allpitchers$Player <- as.character(Allpitchers$Player)
Allpitchers$Team <- as.character(Allpitchers$Team)
Allpitchers <- mutate(Allpitchers, Pos = pullPos(Player))
Allpitchers <- mutate(Allpitchers, MLB = pullMLB(Player))
Allpitchers$Player <- unlist(lapply(Allpitchers$Player,swapName2))
Allpitchers$Pos <- with(Allpitchers,ifelse(Pos=='SP','SP',ifelse(S>HD,'CL',ifelse(HD>0,'MR','SP'))))

ytdp <- read.csv(ytdf,skip=1)
ytdp$Player <- as.character(ytdp$Player)
ytdp$Team <- as.character(ytdp$Team)
ytdp <- mutate(ytdp, Pos = pullPos(Player))
ytdp <- mutate(ytdp, MLB = pullMLB(Player))
ytdp$Player <- unlist(lapply(ytdp$Player,swapName2))
ytdp <- select(ytdp,Player,MLB,HD)
colnames(ytdp) <- c('Player','MLB','yHLD')



AllP <- inner_join(Allpitchers,pitchers,by=c('Player'),copy=FALSE)
AllP <- left_join(AllP,ytdp,by=c('Player','MLB'),copy=FALSE)
# give 60/40 weight to YTD/3WKS
AllP$pHLD <- with(AllP,round(((HD/4)*(30-Week)*.4)+((yHLD/Week)*(30-Week)*.6)),0)
AllP$pSGP <- pitSGPh(AllP)
#AllP2 <- pitSGPhALL(AllP)
  
# Generate expected values
AllH$gHR <- with(AllH,pHR/getd('HR'))
AllH$gRBI <- with(AllH,pRBI/getd('RBI'))
AllH$gR <- with(AllH,pR/getd('R'))
AllH$gSB <- with(AllH,pSB/getd('SB'))
#Assumes all hitters are playing full time - 
AllH$gAVG <- with(AllH,(pAVG - getd('AVG'))*(30 - Week)/270)
AllH$gVAL <- with(AllH,gHR+gRBI+gR+gSB+gAVG)

AllP$gW <- with(AllP,pW/getd('W'))
AllP$gK <- with(AllP,pSO/getd('K'))
AllP$gSV <- with(AllP,pSV/getd('SV'))
AllP$gHLD <- with(AllP,pHLD/getd('HLD'))
AllP$gERA <- with(AllP,(getd('ERA') - pERA)*(30 - Week)/350)
AllP$gVAL <- with(AllP,gW+gK+gERA+gSV+gHLD)

# GENERATE DFL dollar values for all players
#Set parameters
nteams <- 15
tdollars <- (nteams * (260+50)) * (1-(Week/30))
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
bhitters <- select(bhitters,Player,MLB,pDFL)
bpitchers <- select(bpitchers,Player,MLB,pDFL)
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
AllH <- left_join(AllH,bhitters,by=c('Player','MLB'))
AllP <- left_join(AllP,bpitchers,by=c('Player','MLB'))
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
allsp <- FAP %.% arrange(-pDFL,-pSGP) %.% filter(pHLD==0,pSV==0, pGS > 0) %.%
  select(Player,Pos,pDFL,pSGP,gVAL,Rank,pW,pSO,pERA,pK.9,pFIP,pGS,W,K,S,HD,ERA)

allClosers <- FAP %.% arrange(-S,-pSV,-pDFL) %.% filter(pSV>0) %.%
  select(Player,Pos,pDFL,pSGP,gVAL,Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pFIP,W,K,S,HD,ERA)

allHolds <- FAP %.% filter(pHLD>0, pK.9 > 8.0, pBB.9 < 3.5) %.% 
  arrange(-pHLD,-pDFL) %.%
  select(Player,Pos,pDFL,pSGP,gVAL,Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pBB.9,W,K,S,HD,ERA)

TopFAH <- group_by(FAH,Pos) %>% arrange(Pos,-pDFL,-pSGP) %>% filter(rank(-pDFL,-pSGP) <= 5) %>%
  select(Player,Pos,pDFL,pSGP,gVAL,Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)
      
# Create available prospect lists
#prospect <- read.csv("prospects0424.csv",sep='\t',header=FALSE)
#prospect <- prospect[,c('V1','V3','V5','V7','V8','V9')]
prospect <- read.csv("prospects0610.csv")
prospect <- select(prospect,Rank,Player,Team,Position,ETA,Notes)
colnames(prospect) <- c('Rank','Player','Team','Pos','Arrival','Notes')
prospect$Player <- str_trim(as.character(prospect$Player))
FAHp <- inner_join(prospect,FAH,by=c('Player'),copy=FALSE) %.% arrange(-pDFL) %.% 
  select(Rank.x,Player,Team,Pos.x,Arrival,Notes,pDFL,pSGP,gVAL)
FAPp <- inner_join(prospect,FAP,by=c('Player'),copy=FALSE) %.% arrange(-pDFL) %.% 
  select(Rank.x,Player,Team,Pos.x,Arrival,Notes,pDFL,gVAL)

# Closer report
# For MacOS
c <- readHTMLTable(bp,stringASFactors=F)
# For Linux
#c <- readHTMLTable(bp,header=T,stringASFactors=F)

f <- lapply(c,function(x) {ncol(x) == 5})
c2 <- c[unlist(f)]
crep <- c2[[1]]

# for MacOS
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
availCL <- inner_join(crep,FAP,by=c('Player'),copy=FALSE) %.% arrange(-pDFL) %.% 
  select(Player,pDFL,pSGP,gVAL,Score,Rank,pSV,pHLD,pW,pSO,pERA,pK.9,pBB.9,pGS,W,K,S,HD,ERA)

# Calculate total SGPs per team, rank
RH <- filter(AllH,Team != 'Free Agent') %>% group_by(Team) %>% summarize(hDFL = sum(pDFL))
RP <- filter(AllP,Team != 'Free Agent') %>% group_by(Team) %>% summarize(piDFL = sum(pDFL))
RTot <- inner_join(RH,RP,by=c('Team')) %>% 
  mutate(tDFL = hDFL + piDFL,hRank = rank(-hDFL),pRank = rank(-piDFL)) %>% 
  select(Team,hDFL,hRank,piDFL,pRank,tDFL) %>% arrange(-tDFL)

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
tabs[[length(tabs)+1]] <- list('Prospect - P',FAPp)
tabs[[length(tabs)+1]] <- list('Prospect - H',FAHp)

lapply(tabs,addSheet)
saveWorkbook(wkly,"weeklyUpdate.xlsx")

#Create Charts
standings <- read.csv("DAFLWeeklyStandings.csv")
standings$Rank <- as.numeric(str_extract(standings$Rank,'[0-9]+'))
# add category rank columns
s2 <- standings %>% group_by(Week) %>% mutate(rHR = rank(HR),rR = rank(R),rSB = rank(SB),
                                              rRBI = rank(RBI),rBA = rank(BA),rW = rank(W)
                                              ,rS = rank(S),rHD = rank(HD),rK = rank(K)
                                              ,rERA = rank(-ERA))
s2 <- mutate(s2,TP=rHR+rR+rSB+rRBI+rBA+rW+rS+rHD+rK+rERA)
# create line graph
g1 <- ggplot(data=filter(s2,Team %in% c('Cricket','Justice','Fluffy','Tetras')), 
             aes(x=Week, y=TP, group=Team, shape=Team, color=Team)) + geom_line(size=1.2) + 
  geom_point(size=4) + labs(title='Top 4 plus Crickets',y='Total Points')
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

#ad hoc queries

# For a position, who has surplus?
f <- AllH %.% filter(Pos == '1B',pSGP > 13) %.% group_by(Team) %.% summarize(nGood = length(Team))
f2 <- AllH %.% filter(Pos == '1B') %.% group_by(Team) %.% summarize(nTotal = length(Team))
ff <- left_join(f2,f,by=c('Team')) %.% arrange(-nGood,-nTotal)

f <- AllP %>% filter(pSV > 10) %>% group_by(Team) %>% summarize(nGood = length(Team)) %>% arrange(-nGood)

#Find out what a team has that I can use
pullTeam('clowndog & banjo')[[1]]

#Find players by position who can help immediately
FAH %.% filter(Pos == 'OF',pSGP > 10, BA > 0.25) %.% arrange(-pSGP) %.% 
  select(Player,Pos,pSGP,gVAL,Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)

FAH %.% filter(pSGP > 8, BA > 0.26) %.% arrange(-pSGP) %.% 
  select(Player,Pos,pSGP,gVAL,Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)

# Filters by strong K's and low BB's
newHolds <- FAP %.% filter(pHLD>0, pK.9 > 8.0, pBB.9 < 3.5) %.% arrange(-pHLD,-pSGP) %.%
  select(Player,Pos,pSGP,gVAL,Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pBB.9,W,K,S,HD,ERA)

# Top FA in a stat
FAH %.% arrange(-pHR,-pSGP) %.% filter(pHR > 10, pSGP > 8) %.%
  select(Player,Pos,pSGP,gVAL,Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)

FAP %.% arrange(-pSGP) %.% filter(QS>1,pBB.9 < 3.5) %.%
select(Player,Pos,pSGP,gVAL,Rank,pW,pSO,pERA,pK.9,pBB.9,pGS,W,K,S,HD,ERA)

#game <- "http://www.baseball-reference.com/boxes/TBA/TBA201405220.shtml"
#c <- readHTMLTable(game,stringASFactors=F)

#li <- AllP %>% filter(Team != 'Free Agent') %>% group_by(Team,Pos) %>% 
#  summarize(Count = length(Pos))

