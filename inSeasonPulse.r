# 2 projection files - fangraphs
# 2 FA files - cbssports
# 1 roster file - cbssports - make sure to use scoring option
# 1 prospects file - rotoworld
# update Week
# update ttWx

#TBD
# Get year end totals from csv file - currently hardcoded
# Automatically download projection files
# convert gVAL to dollar values

library("xlsx")
library("stringr")
library("dplyr")

# Data that needs to be update manually
Week <- 3

ttLabels <- c('AVG','HR','R','RBI','SB','ERA','HLD','K','SV','W')
ttW1 <- c(.229,3,25,21,3,2.01,1,58,1,4)
ttW2 <- c(.239,7,55,37,3,2.73,1,102,1,7)
ttW3 <- c(.239,11,68,50,5,2.44,2,134,2,8)

tt <- as.list(ttW3)
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
#hitters <- read.csv("zipsHROS.csv")
colnames(hitters) <- str_join('p',colnames(hitters))
hitters$Player <- as.character(hitters$pName)
pitchers <- read.csv("steamerPROS.csv")
colnames(pitchers) <- str_join('p',colnames(pitchers))
#pitchers <- read.csv("zipsPROS.csv")
pitchers$Player <- as.character(pitchers$pName)

#Load Free Agents
FAhitters <- read.csv("FAHitters.csv",skip=1)
FAhitters$Player <- as.character(FAhitters$Player)
FAhitters <- mutate(FAhitters, Pos = pullPos(Player))


FApitchers <- read.csv("FAPitchers.csv",skip=1)
FApitchers$Player <- as.character(FApitchers$Player)
FApitchers <- mutate(FApitchers, Pos = pullPos(Player))

FAhitters$Player <- unlist(lapply(FAhitters$Player,swapName2))
FApitchers$Player <- unlist(lapply(FApitchers$Player,swapName2))


FAH <- inner_join(FAhitters,hitters,by=c('Player'),copy=FALSE)
FAP <- inner_join(FApitchers,pitchers,by=c('Player'),copy=FALSE)

# Rough projection of holds - maintain the rate you're at
FAP$pHLD <- with(FAP,(HD/2)*(30-Week))
#FAP$SV <- with(FAP,(S/2)*(30-Week))

#Load Current Roster
myhitters <- read.csv("LCRoster.csv",skip=2,nrows=13)
myhitters$Player <- as.character(myhitters$Player)
mypitchers <- read.csv("LCRoster.csv",skip=18,nrows=14)
mypitchers$Player <- as.character(mypitchers$Player)
myhitters$Player <- unlist(lapply(myhitters$Player,swapName2))
mypitchers$Player <- unlist(lapply(mypitchers$Player,swapName2))

myHros <- inner_join(myhitters,hitters,by=c('Player'),copy=FALSE)
myPros <- inner_join(mypitchers,pitchers,by=c('Player'),copy=FALSE)

myPros$pHLD <- with(myPros,(HD/Week)*(30-Week))

myHros$gHR <- with(myHros,pHR/sTots$HR)
myHros$gRBI <- with(myHros,pRBI/sTots$RBI)
myHros$gR <- with(myHros,pR/sTots$R)
myHros$gSB <- with(myHros,pSB/sTots$SB)
myHros$gAVG <- with(myHros,(pAVG - sTots$AVG)*(30 - Week)/270)
myHros$gVAL <- with(myHros,gHR+gRBI+gR+gSB+gAVG)

myHros$wVAL <- with(myHros,(gHR*pTots$HR)+(gRBI*pTots$RBI)+(gR*pTots$R)
                 +(gSB*pTots$SB))

myPros$gW <- with(myPros,pW/sTots$W)
myPros$gK <- with(myPros,pSO/sTots$K)
myPros$gSV <- with(myPros,pSV/sTots$SV)
myPros$gHLD <- with(myPros,pHLD/sTots$HLD)
myPros$gERA <- with(myPros,(sTots$ERA - pERA)*(30 - Week)/350)

myPros$gVAL <- with(myPros,gW+gK+gERA+gSV+gHLD)
myPros$wVAL <- with(myPros,(gW*pTots$W)+(gK*pTots$K)+(gSV*pTots$SV)+(gHLD*pTots$HLD))

#dollar values?
myHros$dVAL <- myHros$gVAL * 260
myPros$dVAL <- myPros$gVAL * 260

FAH$gHR <- with(FAH,pHR/sTots$HR)
FAH$gRBI <- with(FAH,pRBI/sTots$RBI)
FAH$gR <- with(FAH,pR/sTots$R)
FAH$gSB <- with(FAH,pSB/sTots$SB)
FAH$gAVG <- with(FAH,(pAVG - sTots$AVG)*(30 - Week)/270)
FAH$gVAL <- with(FAH,gHR+gRBI+gR+gSB+gAVG)

FAH$wVAL <- with(FAH,(gHR*pTots$HR)+(gRBI*pTots$RBI)+(gR*pTots$R)
                 +(gSB*pTots$SB))


FAP$gW <- with(FAP,pW/sTots$W)
FAP$gK <- with(FAP,pSO/sTots$K)
FAP$gSV <- with(FAP,pSV/sTots$SV)
FAP$gHLD <- with(FAP,pHLD/sTots$HLD)
FAP$gERA <- with(FAP,(sTots$ERA - pERA)*(30 - Week)/350)
FAP$gVAL <- with(FAP,gW+gK+gERA+gSV+gHLD)

FAP$wVAL <- with(FAP,(gW*pTots$W)+(gK*pTots$K)+(gSV*pTots$SV)+(gHLD*pTots$HLD))

allsp <- FAP %.% arrange(-gVAL) %.% filter(pHLD==0,pSV==0) %.%
  select(Player,Pos,gVAL,wVAL,Rank,pW,pSO,pERA,pK.9,pFIP,pGS,W,K,S,HD,ERA)
#  filter(GS.y>0)

allClosers <- FAP %.% arrange(-S,-pSV,-gVAL) %.% filter(pSV>0) %.%
  select(Player,Pos,gVAL,wVAL,Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pFIP,W,K,S,HD,ERA)
  

allHolds <- FAP %.% arrange(-pHLD,-gVAL) %.% filter(pHLD>0) %.%
  select(Player,Pos,gVAL,wVAL,Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pFIP,W,K,S,HD,ERA)
  

TopFAH <- FAH %.% arrange(Pos,-gVAL) %.% 
  group_by(Pos) %.% filter(rank(-gVAL) <= 5)

fix(TopFAH)
 
TopFAH <- select(TopFAH,Player,Pos,gVAL,Rank,wVAL,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)
  
TopFAP <- FAP %.% arrange(Pos,-gVAL) %.% 
  group_by(Pos) %.% filter(rank(-gVAL) <= 5)

TopFAP <- select(TopFAP,Player,Pos,gVAL,Rank,wVAL,pW,pSO,pSV,pHLD,pERA,pK.9,pFIP,W,K,S,HD,ERA)
  
mp <- myPros %.% arrange(gVAL) %.% 
  select(Player,gVAL,Rank,wVAL,pW,pSO,pHLD,pSV,pERA,pK.9,pFIP,W,K,HD,S,ERA)
mh <- myHros %.% arrange(gVAL) %.%
  select(Player,gVAL,Rank,wVAL,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)

# Create available prospect lists
prospect <- read.csv("prospects0417.csv",sep='\t',header=FALSE)
prospect <- prospect[,c('V1','V3','V5','V7','V8','V9')]
colnames(prospect) <- c('Rank','Player','Team','Pos','Arrival','Notes')
prospect$Player <- str_trim(as.character(prospect$Player))
FAHp <- inner_join(prospect,FAH,by=c('Player'),copy=FALSE) %.% arrange(Rank.x) %.% 
  select(Rank.x,Player,Team,Pos.x,Arrival,Notes,gVAL)
FAPp <- inner_join(prospect,FAP,by=c('Player'),copy=FALSE) %.% arrange(Rank.x) %.% 
  select(Rank.x,Player,Team,Pos.x,Arrival,Notes,gVAL)



#Create xlsx with tabbed data
wkly <- createWorkbook()
wmh <- createSheet(wb=wkly,sheetName='My Hitters')
wmp <- createSheet(wb=wkly,sheetName='My Pitchers')
wfah <- createSheet(wb=wkly,sheetName='Top Hitters')
wfap <- createSheet(wb=wkly,sheetName='Top Pitchers')
wfasp <- createSheet(wb=wkly,sheetName='SP')
wfacl <- createSheet(wb=wkly,sheetName='Cl')
wfahld <- createSheet(wb=wkly,sheetName='Hld')
wfpp <- createSheet(wb=wkly,sheetName='Prospect - P')
wfph <- createSheet(wb=wkly,sheetName='Prospect - H')

addDataFrame(x=mh,sheet=wmh)
addDataFrame(x=mp,sheet=wmp)
addDataFrame(x=TopFAH,sheet=wfah)
addDataFrame(x=TopFAP,sheet=wfap)
addDataFrame(x=allsp,sheet=wfasp)
addDataFrame(x=allClosers,sheet=wfacl)
addDataFrame(x=allHolds,sheet=wfahld)
addDataFrame(x=FAPp,sheet=wfpp)
addDataFrame(x=FAHp,sheet=wfph)

saveWorkbook(wkly,"weeklyUpdate.xlsx")


