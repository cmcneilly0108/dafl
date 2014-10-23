# For 2015
# Retrofit inSeasonPulse to use new/changed functions
# Add in multiple position eligibility - check cbs
# Create holds projections - add in fangraphs closer report
# Create prospect report
# Create 1st week stats collector - who had hot 1st week? - CBS-filter free agents, calculate SGPs, convert to DFL


library("xlsx")
library("stringr")
library("dplyr")
library("XML")
library("ggplot2")
library("reshape2")

source("./daflFunctions.r")

# Year End Totals
sTots <- list()

l1 <- loadPast2()
eras <- l1[[1]]
avgs <- l1[[2]]
r3 <- l1[[3]]
# Load Master file
master <- read.csv("master_14.csv",stringsAsFactors=FALSE)
master <- rename(master,playerid=fg_id,Pos = mlb_pos,MLB=mlb_team,Player=mlb_name)



#Load protection list
protected <- read.csv("2014fakeprotected.csv",stringsAsFactors=FALSE)

pstandings <- protected %>% group_by(Team) %>% filter(rank(-Value) < 13,DollarRate > 1.3 | Value > 5) %>% 
  summarize(NumProtected = length(Team),
            Spent = sum(Salary),
            TotalValue = sum(pDFL),
            MoneyEarned = TotalValue - Spent,
            VPPlayer = TotalValue/NumProtected,
            DPRemaining = (260-sum(Salary))/(25-NumProtected),
            FullValue = TotalValue + (260-sum(Salary)),
            DollarValue = TotalValue/Spent) %>%
  arrange(-FullValue)
#This works, but only because I manually created the fake file.  Change up to merge with current projections.

#Load steamer projection data
hitters <- read.fg("steamerH2014.csv")
hitters$pSGP <- hitSGP(hitters)

pitchers <- read.fg("steamerP2014.csv")

#Need to predict holds
# Step 2 - copy over previous year's totals
lyp <- read.cbs("AllP2013.csv")
lyp <- select(lyp,playerid,lyHLD=HD)

pitchers <- left_join(pitchers,lyp,by=c('playerid'))
pitchers$pHLD <- pitchers$lyHLD

# Step 3 - use last year's totals plus fangraphs projected role

pitchers$pSGP <- pitSGPh(pitchers)

#Generate pDFL for best players
nlist <- preDollars(hitters,pitchers,protected)
thitters <- nlist[[1]]
tpitchers <- nlist[[2]]

# Incorporate scores back into AllH, AllP
AllH <- left_join(hitters,thitters,by=c('playerid'))
AllP <- left_join(pitchers,tpitchers,by=c('playerid'))
AllH$pDFL <- replace(AllH$pDFL,is.na(AllH$pDFL),0)
AllP$pDFL <- replace(AllP$pDFL,is.na(AllP$pDFL),0)

# Remove protected players
AllH <- anti_join(AllH,protected,by=c('Player'),copy=FALSE) %>% arrange(-pDFL)
AllP <- anti_join(AllP,protected,by=c('Player'),copy=FALSE) %>% arrange(-pDFL)

# Bucket pitchers by role
AllP$Pos <- with(AllP,ifelse(pSV>pHLD,'CL',ifelse(pHLD>pW,'MR','SP')))

#Create separate tabs by position
pc <- AllH %>% filter(Pos == 'C',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
p1b <- AllH %>% filter(Pos == '1B',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
p2b <- AllH %>% filter(Pos == '2B',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
pss <- AllH %>% filter(Pos == 'SS',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
p3b <- AllH %>% filter(Pos == '3B',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
pdh <- AllH %>% filter(Pos == 'DH',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
pof <- AllH %>% filter(Pos %in% c('OF','LF','CF','RF'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
pna <- AllH %>% filter(is.na(Pos),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)


psp <- AllP %>% filter(Pos=='SP',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,DFL=pDFL,SGP=pSGP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD)
pcl <- AllP %>% filter(Pos=='CL',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,DFL=pDFL,SGP=pSGP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD)
pmr <- AllP %>% filter(Pos=='MR',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,DFL=pDFL,SGP=pSGP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD)

# Create spreadsheet
draft <- createWorkbook()
tabs <- list()
tabs[[length(tabs)+1]] <- list('Early Standings',pstandings)
tabs[[length(tabs)+1]] <- list('C',pc)
tabs[[length(tabs)+1]] <- list('1B',p1b)
tabs[[length(tabs)+1]] <- list('2B',p2b)
tabs[[length(tabs)+1]] <- list('SS',pss)
tabs[[length(tabs)+1]] <- list('3B',p3b)
tabs[[length(tabs)+1]] <- list('OF',pof)
tabs[[length(tabs)+1]] <- list('DH',pdh)
tabs[[length(tabs)+1]] <- list('Other',pna)
tabs[[length(tabs)+1]] <- list('SP',psp)
tabs[[length(tabs)+1]] <- list('MR',pmr)
tabs[[length(tabs)+1]] <- list('CL',pcl)

lapply(tabs,addSheet,draft)
saveWorkbook(draft,"draftGuide.xlsx")


#GARBAGE from here down
# Forecast each teams totals - run a standings analysis plus additional category of $$
#proTH <- inner_join(protects,hitters,by=c('Player'),copy=FALSE)
#proTP <- inner_join(protects,pitchers,by=c('Player'),copy=FALSE)

# Create Cricket tab with forecasts
#myHitters <- proTH %.% filter(Team == 'Liquor Crickets')
#myPitchers <- proTP %.% filter(Team == 'Liquor Crickets')
#myTeam <- rbind_all(list(myHitters,myPitchers))
#myTeam <- select(myTeam,c(Name,Contract,Salary,AB,HR,RBI,R,SB,AVG,W,IP,K,HLD,SV,ERA,DFL))


# Group and Summarize each team's protection list
#ltothits <- proTH %.% group_by(Team) %.% summarize(HR = sum(HR), RBI = sum(RBI), 
#                                                   R = sum(R), SB = sum(SB),  AVG = sum(H)/sum(AB))
#ltotpits <- proTP %.% group_by(Team) %.% summarize(W = sum(W), K = sum(K), 
#                                                   SV = sum(SV), H = sum(HLD),  ERA = sum(IP * ERA)/sum(IP))

#ltotpits$pW <- rank(ltotpits$W)
#ltotpits$pK <- rank(ltotpits$K)
#ltotpits$pSV <- rank(ltotpits$SV)
#ltotpits$pH <- rank(ltotpits$H)
#ltotpits$pERA <- rank(ltotpits$ERA)

#ltothits$pHR <- rank(ltothits$HR)
#ltothits$pRBI <- rank(ltothits$RBI)
#ltothits$pR <- rank(ltothits$R)
#ltothits$pSB <- rank(ltothits$SB)
#ltothits$pAVG <- rank(ltothits$AVG)

#lStandings <- inner_join(ltothits,ltotpits,by=c('Team'),copy=FALSE)

#tSal <- protects %.% group_by(Team) %.% summarize(dLeft = 260 - sum(Salary))
#lStandings$dLeft <- tSal$dLeft
#lStandings$pLeft <- rank(lStandings$dLeft)

#lStandings$tPoints <- with(lStandings, pHR + pRBI + pR + pSB + pAVG + pW + pK + pH + pSV + pERA + pLeft)
#leagueProjection <- arrange(lStandings,desc(tPoints))
#leagueProjection <- select(leagueProjection,-c(pHR,pRBI,pR,pSB,pAVG,pW,pSV,pH,pK,pERA,pLeft))

# Analyze my needs based on winning numbers from last year
# Load goals, compare my totals to goals, bar chart
#goals2013 <- read.csv(file="2013goals.csv")
#goals2013$Category <- as.character(goals2013$Category)

#currentLC <- leagueProjection %.% filter(Team == 'Liquor Crickets') %.%
#  select(-c(Team,dLeft,tPoints))
#currentLC <- as.data.frame(t(currentLC))
#currentLC$Category <- rownames(currentLC)
#colnames(currentLC) <- c('Current','Category')
#myGoals <- inner_join(goals2013,currentLC)
#myGoals$PctCmp <- with(myGoals,Current/Goal)
#myGoals$StillNeed <- with(myGoals,Goal-Current)
#myGoals <- arrange(myGoals,PctCmp)

