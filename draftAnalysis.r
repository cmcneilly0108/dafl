# For 2015
# Use better holds predictor
# Calculate standings based on projected stats
# inSeasonAnalysis - how can this be done?


library("xlsx")
library("stringr")
library("dplyr")
library("XML")
library("ggplot2")
library("reshape2")

source("./daflFunctions.r")

# Week 1 rosters
rosters <- read.cbs("2014DraftResults.csv")
#split into P,H tables
rHitters <- filter(rosters,Pos != 'SP' & Pos != 'RP') 
rPitchers <- filter(rosters,Pos == 'SP' | Pos == 'RP')


#Load steamer data
hitters <- read.fg("steamerH2014.csv") %>% select(-Player)
hitters$pSGP <- hitSGP(hitters)
pitchers <- read.fg("steamerP2014.csv") %>% select(-Player)
#Need to predict holds
lyp <- read.cbs("AllP2013.csv")
lyp <- select(lyp,playerid,lyHLD=HD)

pitchers <- left_join(pitchers,lyp,by=c('playerid'))
pitchers$pHLD <- pitchers$lyHLD
pitchers$pSGP <- pitSGPh(pitchers)

#Generate dollars
nlist <- preDollars(hitters,pitchers)
thitters <- nlist[[1]]
tpitchers <- nlist[[2]]

# Incorporate scores back into AllH, AllP
AllH <- left_join(hitters,thitters,by=c('playerid'))
AllP <- left_join(pitchers,tpitchers,by=c('playerid'))
AllH$pDFL <- replace(AllH$pDFL,is.na(AllH$pDFL),0)
AllP$pDFL <- replace(AllP$pDFL,is.na(AllP$pDFL),0)

#merge with steamer
AllH <- left_join(rHitters,AllH,by=c('playerid'),copy=FALSE)
AllP <- left_join(rPitchers,AllP,by=c('playerid'),copy=FALSE)
AllH <- select(AllH,-Pos.x) %>% rename(Pos=Pos.y)
AllP <- select(AllP,-Pos.x) %>% rename(Pos=Pos.y)

AllH$Value <- AllH$pDFL - AllH$Salary
AllP$Value <- AllP$pDFL - AllP$Salary
AllH$pDFL <- with(AllH,ifelse(is.na(pDFL),0,pDFL))
AllP$pDFL <- with(AllP,ifelse(is.na(pDFL),0,pDFL))

RH <- AllH %>% group_by(Team) %>% summarize(hDFL = sum(pDFL))
RP <- AllP %>% group_by(Team) %>% summarize(piDFL = sum(pDFL))
RTot <- inner_join(RH,RP,by=c('Team')) %>% 
  mutate(tDFL = hDFL + piDFL,hRank = rank(-hDFL),pRank = rank(-piDFL)) %>% 
  select(Team,hDFL,hRank,piDFL,pRank,tDFL) %>% arrange(-tDFL)

RH <- AllH %>% filter(Contract == 1) %>% group_by(Team) %>% summarize(hDFL = sum(pDFL))
RP <- AllP %>% filter(Contract == 1) %>% group_by(Team) %>% summarize(piDFL = sum(pDFL))
RAuction <- inner_join(RH,RP,by=c('Team')) %>% 
  mutate(tDFL = hDFL + piDFL,hRank = rank(-hDFL),pRank = rank(-piDFL)) %>% 
  select(Team,hDFL,hRank,piDFL,pRank,tDFL) %>% arrange(-tDFL)

RH <- AllH %>% filter(Contract > 1) %>% group_by(Team) %>% summarize(hDFL = sum(pDFL))
RP <- AllP %>% filter(Contract > 1) %>% group_by(Team) %>% summarize(piDFL = sum(pDFL))
RProtect <- inner_join(RH,RP,by=c('Team')) %>% 
  mutate(tDFL = hDFL + piDFL,hRank = rank(-hDFL),pRank = rank(-piDFL)) %>% 
  select(Team,hDFL,hRank,piDFL,pRank,tDFL) %>% arrange(-tDFL)

RH <- AllH %>% group_by(Team) %>% summarize(hSpent = sum(Salary))
RP <- AllP %>% group_by(Team) %>% summarize(pSpent = sum(Salary))
Splits <- inner_join(RH,RP,by=c('Team')) %>% 
  mutate(Ratio = hSpent/(hSpent+pSpent),hRank = rank(-hSpent),pRank = rank(-pSpent)) %>% 
  select(Team,hSpent,hRank,pSpent,pRank,Ratio) %>% arrange(-Ratio)

RH <- select(AllH,Team,Player,Pos,Contract,Salary,pDFL) %>% filter(Team=='Liquor Crickets')
RP <- select(AllP,Team,Player,Pos,Contract,Salary,pDFL) %>% filter(Team=='Liquor Crickets')
lc <- rbind(RH,RP)

# Create spreadsheet
draft <- createWorkbook()
tabs <- list()
tabs[[length(tabs)+1]] <- list('StandingsByDFL',RTot)
tabs[[length(tabs)+1]] <- list('SByProtect',RProtect)
tabs[[length(tabs)+1]] <- list('SByAuction',RAuction)
tabs[[length(tabs)+1]] <- list('HP Splits',Splits)

lapply(tabs,addSheet,draft)
saveWorkbook(draft,"draftAnalysis.xlsx")

