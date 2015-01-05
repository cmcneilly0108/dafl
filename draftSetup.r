
library("xlsx")
library("stringr")
library("dplyr")
library("XML")
library("ggplot2")
library("reshape2")
library("googleVis")

source("./daflFunctions.r")

#Load steamer data
hitters <- read.fg("steamerH2015.csv")
hitters$pSGP <- hitSGP(hitters)

pitchers <- read.fg("steamerP2015.csv")
pitchers <- predictHolds(pitchers)
pitchers$pSGP <- pitSGP(pitchers)

#Generate dollars
nlist <- preDollars(hitters,pitchers)
thitters <- nlist[[1]]
tpitchers <- nlist[[2]]

thitters <- rename(thitters,oDFL=pDFL)
tpitchers <- rename(tpitchers,oDFL=pDFL)

# Incorporate scores back into AllH, AllP
AllH <- left_join(hitters,thitters,by=c('playerid'))
AllP <- left_join(pitchers,tpitchers,by=c('playerid'))
AllH$oDFL <- replace(AllH$oDFL,is.na(AllH$oDFL),0)
AllP$oDFL <- replace(AllP$oDFL,is.na(AllP$oDFL),0)

#Read in protections, calculate pDFL
protected <- read.csv("2014fakeprotected.csv",stringsAsFactors=FALSE)

#Generate pDFL for best players
nlist <- preDollars(hitters,pitchers,protected)
thitters <- nlist[[1]]
tpitchers <- nlist[[2]]

# Incorporate scores back into AllH, AllP
AllH <- left_join(AllH,thitters,by=c('playerid'))
AllP <- left_join(AllP,tpitchers,by=c('playerid'))
AllH$pDFL <- replace(AllH$pDFL,is.na(AllH$pDFL),0)
AllP$pDFL <- replace(AllP$pDFL,is.na(AllP$pDFL),0)
AllH$Pos <- with(AllH,ifelse(Pos %in% c('LF','CF','RF'),'OF',Pos))
AllP$Pos <- with(AllP,ifelse(pSV>pHLD,'CL',ifelse(pHLD>pW,'MR','SP')))
# Remove protected players
AvH <- anti_join(AllH,protected,by=c('Player'),copy=FALSE) %>% arrange(-pDFL)
AvP <- anti_join(AllP,protected,by=c('Player'),copy=FALSE) %>% arrange(-pDFL)

# Create protected lists
PH <- inner_join(AllH,protected,by=c('Player'),copy=FALSE)
PH <- rename(PH,pDFL=pDFL.y,Pos=Pos.x) %>% select(Player,Team,MLB,Pos,Contract,Salary,DFL=pDFL,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
PP <- inner_join(AllP,protected,by=c('Player'),copy=FALSE)
PP <- rename(PP,pDFL=pDFL.y,Pos=Pos.y) %>% select(Player,Team,MLB,Pos,Contract,Salary,DFL=pDFL,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD)


avbyPos <- function(p) {
  if (p %in% c('SP','MR','CL')) {
    AvP %>% filter(Pos==p) %>% select(Player,MLB,DFL=pDFL,oDFL,SGP=pSGP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD)
  } else {
    AvH %>% filter(Pos==p) %>% select(Player,MLB,DFL=pDFL,oDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
  }
}

moveHitter <- function(pid,sal,tm) {
  pl <- filter(AvH,playerid==pid)
  AvH <<- filter(AvH,playerid != pid)
  pl$Team <- tm
  pl$Contract <- 1
  pl$Salary <- sal
  pl <- select(pl,Player,Team,MLB,Contract,Salary,DFL=oDFL,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
  PH <<- rbind(PH,pl)
}

movePitcher <- function(pid,sal,tm) {
  pl <- filter(AvP,playerid==pid)
  AvP <<- filter(AvP,playerid != pid)
  pl$Team <- tm
  pl$Contract <- 1
  pl$Salary <- sal
  pl <- select(pl,Player,Team,MLB,Contract,Salary,DFL=oDFL,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
  PP <<- rbind(PP,pl)
}

