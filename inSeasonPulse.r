# 2 projection files - fangraphs
# 2 FA files - cbssports
# 1 prospects file - rotoworld
# update Week
# update ttWx

#TBD
# Revisit gVAL for AVG and ERA - is it reasonable?
# Take team record into consideration when forcasting holds
# Get year end totals from csv file - currently hardcoded
# Automatically download projection files
# convert gVAL to dollar values

library("xlsx")
library("stringr")
library("dplyr")

# Data that needs to be update manually
Week <- 4

ttLabels <- c('AVG','HR','R','RBI','SB','ERA','HLD','K','SV','W')
ttW1 <- c(.229,3,25,21,3,2.01,1,58,1,4)
ttW2 <- c(.239,7,55,37,3,2.73,1,102,1,7)
ttW3 <- c(.247,14,79,62,6,2.80,4,154,2,8)
ttW4 <- c(.250,16,103,78,11,3.04,4,192,7,9)

tt <- as.list(ttW4)
names(tt) <- ttLabels
# End manual update data


swapName2 <- function(n){
  comma <- str_locate(n,',')
  ln <- str_sub(n,1,comma-1)
  rest <- str_sub(n,comma+2,-1)
  space <- str_locate(rest,' ')
  fn <- str_sub(rest,1,space-1)
  nn <- str_join(fn,ln,sep=" ",collapse=NULL)
  nn[1] 
}

pullPos <- function(n){
  n <- str_trim(n)
  p <- str_match(n,".+, .+ (.+) .+")
  p <- p[,2]
  p <- ifelse((p =='P'),'RP',p)
  ifelse((p %in% c('CF','RF','LF')),'OF',p)
}

pullTeam <- function(tn){
  tH <- filter(AllH,Team == tn)
  tH <- select(tH,-Team)
  tP <- filter(AllP,Team == tn)
  tP <- select(tP,-Team)
  tP <- tP %.% arrange(-gVAL) %.% 
    select(Player,Pos,gVAL,Rank,wVAL,pW,pSO,pHLD,pSV,pERA,pK.9,pFIP,W,K,HD,S,ERA)
  tH <- tH %.% arrange(-gVAL) %.%
    select(Player,Pos,gVAL,Rank,wVAL,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)
  list(tH,tP)
}

addSheet <- function(l){
  sht <- createSheet(wb=wkly,sheetName=l[[1]])
  addDataFrame(x=l[[2]],sheet=sht)
}

# Year End Totals
# c(209,735,739,159,42,90,1191,96)
# c('HR','RBI','R','SB','HLD','SV','K','W')
sTots <- list()
sTots$HR <- 209
sTots$RBI <- 735
sTots$R <- 739
sTots$SB <- 159
sTots$HLD <- 42
sTots$SV <- 90
sTots$K <- 1191
sTots$W <- 96
sTots$AVG <- .291
sTots$ERA <- 3.277

pTots <- list()
pTots$HR <- 1-tt$HR/sTots$HR
pTots$RBI <-  1-tt$RBI/sTots$RBI
pTots$R <-  1-tt$R/sTots$R
pTots$SB <-  1-tt$SB/sTots$SB
pTots$HLD <-  1-tt$HLD/sTots$HLD
pTots$SV <-  1-tt$SV/sTots$SV
pTots$K <-  1-tt$K/sTots$K
pTots$W <-  1-tt$W/sTots$W
#pTots$AVG <- .291
#pTots$ERA <- 3.277

#Load Steamer rest of season projections
# TASK - get data dynamically
hitters <- read.csv("steamerHROS.csv")
colnames(hitters) <- str_join('p',colnames(hitters))
hitters$Player <- as.character(hitters$pName)
pitchers <- read.csv("steamerPROS.csv")
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
AllP$pHLD <- with(AllP,(HD/2)*(30-Week))

# Generate expected values
AllH$gHR <- with(AllH,pHR/sTots$HR)
AllH$gRBI <- with(AllH,pRBI/sTots$RBI)
AllH$gR <- with(AllH,pR/sTots$R)
AllH$gSB <- with(AllH,pSB/sTots$SB)
#Assumes all hitters are playing full time - 
AllH$gAVG <- with(AllH,(pAVG - sTots$AVG)*(30 - Week)/270)
AllH$gVAL <- with(AllH,gHR+gRBI+gR+gSB+gAVG)
AllH$wVAL <- with(AllH,(gHR*pTots$HR)+(gRBI*pTots$RBI)+(gR*pTots$R)
                 +(gSB*pTots$SB))

AllP$gW <- with(AllP,pW/sTots$W)
AllP$gK <- with(AllP,pSO/sTots$K)
AllP$gSV <- with(AllP,pSV/sTots$SV)
AllP$gHLD <- with(AllP,pHLD/sTots$HLD)
AllP$gERA <- with(AllP,(sTots$ERA - pERA)*(30 - Week)/350)
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

allsp <- FAP %.% arrange(-gVAL) %.% filter(pHLD==0,pSV==0) %.%
  select(Player,Pos,gVAL,wVAL,Rank,pW,pSO,pERA,pK.9,pFIP,pGS,W,K,S,HD,ERA)

allClosers <- FAP %.% arrange(-S,-pSV,-gVAL) %.% filter(pSV>0) %.%
  select(Player,Pos,gVAL,wVAL,Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pFIP,W,K,S,HD,ERA)

allHolds <- FAP %.% arrange(-pHLD,-gVAL) %.% filter(pHLD>0) %.%
  select(Player,Pos,gVAL,wVAL,Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pFIP,W,K,S,HD,ERA)
  

TopFAH <- FAH %.% group_by(Pos)  %.% arrange(Pos,-gVAL) %.% filter(rank(-gVAL) <= 5)
 
TopFAH <- select(TopFAH,Player,Pos,gVAL,Rank,wVAL,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)
  
TopFAP <- FAP %.% arrange(Pos,-gVAL) %.% 
  group_by(Pos) %.% filter(rank(-gVAL) <= 5)

TopFAP <- select(TopFAP,Player,Pos,gVAL,Rank,wVAL,pW,pSO,pSV,pHLD,pERA,pK.9,pFIP,W,K,S,HD,ERA)
    
# Create available prospect lists
#prospect <- read.csv("prospects0424.csv",sep='\t',header=FALSE)
#prospect <- prospect[,c('V1','V3','V5','V7','V8','V9')]
prospect <- read.csv("prospects0424.csv")
prospect <- select(prospect,Rank,Player,Team,Position,ETA,Notes)
colnames(prospect) <- c('Rank','Player','Team','Pos','Arrival','Notes')
prospect$Player <- str_trim(as.character(prospect$Player))
FAHp <- inner_join(prospect,FAH,by=c('Player'),copy=FALSE) %.% arrange(Rank.x) %.% 
  select(Rank.x,Player,Team,Pos.x,Arrival,Notes,gVAL)
FAPp <- inner_join(prospect,FAP,by=c('Player'),copy=FALSE) %.% arrange(Rank.x) %.% 
  select(Rank.x,Player,Team,Pos.x,Arrival,Notes,gVAL)


#Create xlsx with tabbed data
wkly <- createWorkbook()
tabs <- list()
tabs[[length(tabs)+1]] <- list('My Hitters',mh)
tabs[[length(tabs)+1]] <- list('My Pitchers',mp)
tabs[[length(tabs)+1]] <- list('Top Hitters',TopFAH)
tabs[[length(tabs)+1]] <- list('Top Pitchers',TopFAP)
tabs[[length(tabs)+1]] <- list('SP',allsp)
tabs[[length(tabs)+1]] <- list('Cl',allClosers)
tabs[[length(tabs)+1]] <- list('Hld',allHolds)
tabs[[length(tabs)+1]] <- list('Prospect - P',FAPp)
tabs[[length(tabs)+1]] <- list('Prospect - H',FAHp)

lapply(tabs,addSheet)
saveWorkbook(wkly,"weeklyUpdate.xlsx")

#ad hoc queries

# For a position, who has surplus?
f <- AllH %.% filter(Pos == 'SS',gVAL > 0.32) %.% group_by(Team) %.% summarize(nGood = length(Team))
f2 <- AllH %.% filter(Pos == 'SS') %.% group_by(Team) %.% summarize(nTotal = length(Team))
ff <- inner_join(f,f2,by=c('Team')) %.% arrange(-nGood,-nTotal)

#Find out what a team has that I can use
pullTeam('clowndog & banjo')[[1]]

#Find players by position who can help immediately
FAH %.% filter(Pos == 'OF',gVAL > 0.15, BA > 0.25) %.% arrange(-gVAL) %.% 
  select(Player,Pos,gVAL,Rank,wVAL,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)

FAH %.% filter(gVAL > 0.19, BA > 0.26) %.% arrange(-gVAL) %.% 
  select(Player,Pos,gVAL,Rank,wVAL,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)

