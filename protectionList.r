#Generate based on 2014 projections

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


#Load steamer data
hitters <- read.fg("steamerH2014.csv")
hitters$pSGP <- hitSGP(hitters)

pitchers <- read.fg("steamerP2014.csv")
#Need to predict holds
lyp <- read.cbs("AllP2013.csv")
lyp <- select(lyp,playerid,lyHLD=HD)

pitchers <- left_join(pitchers,lyp,by=c('playerid'))
pitchers$pHLD <- pitchers$lyHLD


pitchers$pSGP <- pitSGPh(pitchers)

#Load 2013 final rosters
rosters <- read.xlsx("2013+Season+Ending+Rosters.xlsx",1)
rosters$Player <- as.character(rosters$Name..Qual.Pos.)
rosters$Player <- unlist(lapply(rosters$Player,swapName2))
rosters <- filter(rosters,X2014.Salary != 'NA')
rosters$X2014.Salary <- as.numeric(as.character(rosters$X2014.Salary))
#split into P,H tables
rHitters <- filter(rosters,Pos != 'P')
rPitchers <- filter(rosters,Pos == 'P')

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
AllH <- inner_join(rHitters,AllH,by=c('Player'),copy=FALSE)
AllP <- inner_join(rPitchers,AllP,by=c('Player'),copy=FALSE)
AllH <- select(AllH,-Pos.x) %>% rename(Pos=Pos.y)
AllP <- select(AllP,-Pos.x) %>% rename(Pos=Pos.y)

AllH$Value <- AllH$pDFL - AllH$X2014.Salary
AllP$Value <- AllP$pDFL - AllP$X2014.Salary

rpreds <- rbind(select(AllH,Team,Player,Pos,X2014.Contract,X2014.Salary,pDFL,Value),
            select(AllP,Team,Player,Pos,X2014.Contract,X2014.Salary,pDFL,Value))

rpreds$DollarRate <- rpreds$pDFL/rpreds$X2014.Salary
rpreds <- rename(rpreds,Salary=X2014.Salary,Contract=X2014.Contract)

#lc <- filter(rpreds,Team == 'Liquor Crickets') %>% arrange(-Value)
lc <- filter(rpreds,Team == 'Liquor Crickets',DollarRate > 1.3 | Value > 5) %>% arrange(-DollarRate)

# Logic - filter to players that either have a valueRate > 1.3 or totalValue > 5, sort by Value descending
totals2 <- rpreds %>% group_by(Team) %>% filter(rank(-Value) < 13,DollarRate > 1.3 | Value > 5) %>% 
  summarize(NumProtected = length(Team),
            Spent = sum(Salary),
            TotalValue = sum(pDFL),
            MoneyEarned = TotalValue - Spent,
            VPPlayer = TotalValue/NumProtected,
            DPRemaining = (260-sum(Salary))/(25-NumProtected),
            DollarValue = TotalValue/Spent) %>%
  arrange(-MoneyEarned)

prosters <- rpreds %>% group_by(Team) %>% filter(rank(-Value) < 13,DollarRate > 1.3 | Value > 5)
write.csv(prosters,"2014fakeprotected.csv")
# Need to give more weight to spending more
# sum(Salary) * DollarRate = probably too much?  log?