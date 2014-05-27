# 2 projection files - fangraphs
# 2 FA files - cbssports
# 1 prospects file - rotoworld
# update Week
# update ttWx
# update fangraphs bullpen URL

#TBD
# Remove gVAL, wVAL
# Incorporate holds into SGP calculation
# Automatically download projection files

library("xlsx")
library("stringr")
library("dplyr")
library("XML")


source("./daflFunctions.r")


# Data that needs to be update manually
Week <- 9
bp <- "http://www.fangraphs.com/fantasy/bullpen-report-may-25-2014/"


ttLabels <- c('AVG','HR','R','RBI','SB','ERA','HLD','K','SV','W')
ttW1 <- c(.229,3,25,21,3,2.01,1,58,1,4)
ttW2 <- c(.239,7,55,37,3,2.73,1,102,1,7)
ttW3 <- c(.247,14,79,62,6,2.80,4,154,2,8)
ttW4 <- c(.248,16,105,78,11,2.88,4,212,7,10)
ttW5 <- c(.248,23,138,105,20,3.337,5,270,10,13)
ttW6 <- c(.244,26,160,121,29,3.307,6,312,13,14)
ttW7 <- c(.244,30,184,150,32,3.381,8,378,13,19)
ttW8 <- c(.251,40,212,181,36,3.426,8,404,14,20)

tt <- as.list(ttW8)
names(tt) <- ttLabels
# End manual update data


# Year End Totals
sTots <- list()

l1 <- loadPast()
r2 <- l1[[1]]
r3 <- l1[[2]]


# Get statistical goals
#results <- read.xlsx("DAFLSGP.xlsx",2)
#r3 <- results %.% group_by(Category) %.% summarize(ad = mean(Top))
#getd <- function(c) {
#  r3[r3$Category==c,'ad']
#}

# % of each stat I still need
pTots <- list()
pTots$HR <- 1-tt$HR/getd('HR')
pTots$RBI <-  1-tt$RBI/getd('RBI')
pTots$R <-  1-tt$R/getd('R')
pTots$SB <-  1-tt$SB/getd('SB')
pTots$HLD <-  1-tt$HLD/getd('HLD')
pTots$SV <-  1-tt$SV/getd('SV')
pTots$K <-  1-tt$K/getd('K')
pTots$W <-  1-tt$W/getd('W')
#pTots$AVG <- .291
#pTots$ERA <- 3.277

#Load Steamer rest of season projections
hitters <- read.csv("steamerHROS.csv")
hitters$SGP <- hitSGP(hitters)
colnames(hitters) <- str_join('p',colnames(hitters))
hitters$Player <- as.character(hitters$pName)

pitchers <- read.csv("steamerPROS.csv")
pitchers$SGP <- pitSGP(pitchers)
colnames(pitchers) <- str_join('p',colnames(pitchers))
pitchers$Player <- as.character(pitchers$pName)

#Load All Players - Extract Free Agents
Allhitters <- read.csv("AllHitters.csv",skip=1)
Allhitters$Player <- as.character(Allhitters$Player)
Allhitters <- mutate(Allhitters, Pos = pullPos(Player))
Allhitters$Player <- unlist(lapply(Allhitters$Player,swapName2))

AllH <- inner_join(Allhitters,hitters,by=c('Player'),copy=FALSE)

Allpitchers <- read.csv("AllPitchers.csv",skip=1)
Allpitchers$Player <- as.character(Allpitchers$Player)
Allpitchers <- mutate(Allpitchers, Pos = pullPos(Player))
Allpitchers$Player <- unlist(lapply(Allpitchers$Player,swapName2))

AllP <- inner_join(Allpitchers,pitchers,by=c('Player'),copy=FALSE)
AllP$pHLD <- with(AllP,(HD/3)*(30-Week))

# Generate expected values
AllH$gHR <- with(AllH,pHR/getd('HR'))
AllH$gRBI <- with(AllH,pRBI/getd('RBI'))
AllH$gR <- with(AllH,pR/getd('R'))
AllH$gSB <- with(AllH,pSB/getd('SB'))
#Assumes all hitters are playing full time - 
AllH$gAVG <- with(AllH,(pAVG - getd('AVG'))*(30 - Week)/270)
AllH$gVAL <- with(AllH,gHR+gRBI+gR+gSB+gAVG)
AllH$wVAL <- with(AllH,(gHR*pTots$HR)+(gRBI*pTots$RBI)+(gR*pTots$R)
                 +(gSB*pTots$SB))

AllP$gW <- with(AllP,pW/getd('W'))
AllP$gK <- with(AllP,pSO/getd('K'))
AllP$gSV <- with(AllP,pSV/getd('SV'))
AllP$gHLD <- with(AllP,pHLD/getd('HLD'))
AllP$gERA <- with(AllP,(getd('ERA') - pERA)*(30 - Week)/350)
AllP$gVAL <- with(AllP,gW+gK+gERA+gSV+gHLD)
AllP$wVAL <- with(AllP,(gW*pTots$W)+(gK*pTots$K)+(gSV*pTots$SV)+(gHLD*pTots$HLD))

# Create Free Agents
FAH <- filter(AllH,Team == 'Free Agent')
FAH <- select(FAH,-Team)
FAP <- filter(AllP,Team == 'Free Agent')
FAP <- select(FAP,-Team)

#Load Current Roster
myteam <- pullTeam('Liquor Crickets')
mh <- myteam[[1]]
mp <- myteam[[2]]
  
#dollar values?
#myHros$dVAL <- myHros$gVAL * 260
#myPros$dVAL <- myPros$gVAL * 260

allsp <- FAP %.% arrange(-pSGP) %.% filter(pHLD==0,pSV==0) %.%
  select(Player,Pos,pSGP,gVAL,wVAL,Rank,pW,pSO,pERA,pK.9,pFIP,pGS,W,K,S,HD,ERA)

allClosers <- FAP %.% arrange(-S,-pSV,-pSGP) %.% filter(pSV>0) %.%
  select(Player,Pos,pSGP,gVAL,wVAL,Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pFIP,W,K,S,HD,ERA)

allHolds <- FAP %.% filter(pHLD>0, pK.9 > 8.0, pBB.9 < 3.5) %.% 
  arrange(-pHLD,-pSGP) %.%
  select(Player,Pos,pSGP,gVAL,wVAL,Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pBB.9,W,K,S,HD,ERA)

  

TopFAH <- FAH %.% group_by(Pos)  %.% arrange(Pos,-pSGP) %.% filter(rank(-gVAL) <= 5)
 
TopFAH <- select(TopFAH,Player,Pos,pSGP,gVAL,Rank,wVAL,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)
  
#TopFAP <- FAP %.% arrange(Pos,-pSGP) %.% 
#  group_by(Pos) %.% filter(rank(-gVAL) <= 5)
#TopFAP <- select(TopFAP,Player,Pos,pSGP,gVAL,Rank,wVAL,pW,pSO,pSV,pHLD,pERA,pK.9,pFIP,W,K,S,HD,ERA)
    
# Create available prospect lists
#prospect <- read.csv("prospects0424.csv",sep='\t',header=FALSE)
#prospect <- prospect[,c('V1','V3','V5','V7','V8','V9')]
prospect <- read.csv("prospects0523.csv")
prospect <- select(prospect,Rank,Player,Team,Position,ETA,Notes)
colnames(prospect) <- c('Rank','Player','Team','Pos','Arrival','Notes')
prospect$Player <- str_trim(as.character(prospect$Player))
FAHp <- inner_join(prospect,FAH,by=c('Player'),copy=FALSE) %.% arrange(-pSGP) %.% 
  select(Rank.x,Player,Team,Pos.x,Arrival,Notes,pSGP,gVAL)
FAPp <- inner_join(prospect,FAP,by=c('Player'),copy=FALSE) %.% arrange(-pSGP) %.% 
  select(Rank.x,Player,Team,Pos.x,Arrival,Notes,pSGP,gVAL)

# Closer report
c <- readHTMLTable(bp, header=T,stringASFactors=F)
ncol(c[[15]]) == 5
f <- lapply(c,function(x) {ncol(x) == 5})
c2 <- c[unlist(f)]
crep <- c2[[1]]
#crep <- readHTMLTable(bp, header=T, which=15,stringsAsFactors=F)
t <- data.frame(crep$Closer,10)
t2 <- data.frame(crep$First,5)
t3 <- data.frame(crep$Second,2)
colnames(t) <- c('Player','Score')
colnames(t2) <- c('Player','Score')
colnames(t3) <- c('Player','Score')
crep <- rbind_list(t,t2,t3)
availCL <- inner_join(crep,FAP,by=c('Player'),copy=FALSE) %.% arrange(-pSGP) %.% 
  select(Player,pSGP,gVAL,Score,Rank,pSV,pHLD,pW,pSO,pERA,pK.9,pBB.9,pGS,W,K,S,HD,ERA)

# Calculate total SGPs per team, rank
RH <- filter(AllH,Team != 'Free Agent') %>% group_by(Team) %>% summarize(hSGP = sum(pSGP))
RP <- filter(AllP,Team != 'Free Agent') %>% group_by(Team) %>% summarize(piSGP = sum(pSGP))
RTot <- inner_join(RH,RP,by=c('Team')) %>% mutate(tSGP = hSGP + piSGP,hRank = rank(-hSGP),pRank = rank(-piSGP)) %>% 
  select(Team,hSGP,hRank,piSGP,pRank,tSGP) %>% arrange(-tSGP)

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
  select(Player,Pos,pSGP,gVAL,Rank,wVAL,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)

FAH %.% filter(pSGP > 8, BA > 0.26) %.% arrange(-pSGP) %.% 
  select(Player,Pos,pSGP,gVAL,Rank,wVAL,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)

# Filters by strong K's and low BB's
newHolds <- FAP %.% filter(pHLD>0, pK.9 > 8.0, pBB.9 < 3.5) %.% arrange(-pHLD,-pSGP) %.%
  select(Player,Pos,pSGP,gVAL,wVAL,Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pBB.9,W,K,S,HD,ERA)

# Top FA in a stat
FAH %.% arrange(-pHR,-pSGP) %.% filter(pHR > 10, pSGP > 8) %.%
  select(Player,Pos,pSGP,gVAL,Rank,wVAL,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)

FAP %.% arrange(-pSGP) %.% filter(QS>1,pBB.9 < 3.5) %.%
select(Player,Pos,pSGP,gVAL,wVAL,Rank,pW,pSO,pERA,pK.9,pBB.9,pGS,W,K,S,HD,ERA)

#game <- "http://www.baseball-reference.com/boxes/TBA/TBA201405220.shtml"
#c <- readHTMLTable(game,stringASFactors=F)
