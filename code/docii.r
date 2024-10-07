

library("openxlsx")
library("stringr")
library("dplyr")
library("XML")
library("ggplot2")
library("reshape2")
library("lubridate")
library("zoo")
library("xml2")
library("rvest")
library("jsonlite")
library("tidyr")


source("./daflFunctions.r")

# STEP 1 - Use September data as the predictor of playoff performance

# hitting last 30
# https://www.fangraphs.com/leaders/major-league?pos=all&stats=bat&lg=all&qual=y&type=0&season=2024&month=1000&season1=2024&ind=0&startdate=2024-09-01&enddate=2024-11-01&team=0
# re-pull on Sunday


hitters <- read.csv("../dociH.csv",stringsAsFactors=FALSE)
pitchers <- read.csv("../dociP.csv",stringsAsFactors=FALSE)

#strip out non-playoff players
playoffTeams <- c('NYY','CLE','HOU','BAL','DET','KCR','LAD','PHI','MIL','SDP','NYM','ATL')

h2 <- filter(hitters,Team %in% playoffTeams)
h2 <- h2 %>% rename(playerid=PlayerId,pHR=HR,pR=R,pRBI=RBI,pSB=SB,pAVG=AVG,pH=H,pAB=AB)
p2 <- filter(pitchers,Team %in% playoffTeams)
p2 <- p2 %>% rename(playerid=PlayerId,pW=W,pSO=SO,pHLD=HLD,pSV=SV,pERA=ERA,pIP=IP,pER=ER)

#nlist <- preLPP(h2,p2,data.frame(),1,50,21)
nlist <- dociiDollars(h2,p2)

bhitters <- nlist[[1]]
bpitchers <- nlist[[2]]


# Incorporate scores back into AllH, AllP
AllH <- left_join(h2,bhitters,by=c('playerid'),relationship = "many-to-many")
AllP <- left_join(p2,bpitchers,by=c('playerid'),relationship = "many-to-many")
AllH <- rename(AllH,pDFL=zDFL)
AllP <- rename(AllP,pDFL=zDFL)
AllH$pDFL <- replace(AllH$pDFL,is.na(AllH$pDFL),0)
AllP$pDFL <- replace(AllP$pDFL,is.na(AllP$pDFL),0)

AllH <- AllH %>% select(Name,Team,pDFL,pHR,pRBI,pSB,pAVG) %>% arrange(-pDFL)
AllP <- AllP %>% select(Name,Team,pDFL,pW,pSO,pSV,pERA) %>% arrange(-pDFL)

AllH <- mutate(AllH,eRank=row_number())
AllP <- mutate(AllP,eRank=row_number())

# STEP 2 - Use odds from Fangraphs to gauge likelihood of reaching each round
#         multiply number of games in each round

# Now use odds
# https://www.fangraphs.com/standings/playoff-odds

odds <- read.csv('../dociiOdds.csv')

AllH2 <- left_join(AllH,odds)
AllH2 <- AllH2 %>% mutate(dDFL = 3*pDFL*WC + 5*pDFL*LDS + 7*pDFL*LCS + 7*pDFL*WS)
AllH2 <- AllH2 %>% select(-WC,-LDS,-LCS,-WS) %>% arrange(-dDFL)

AllP2 <- left_join(AllP,odds)
AllP2 <- AllP2 %>% mutate(dDFL = 3*pDFL*WC + 5*pDFL*LDS + 7*pDFL*LCS + 7*pDFL*WS)
AllP2 <- AllP2 %>% select(-WC,-LDS,-LCS,-WS) %>% arrange(-dDFL)

AllH2 <- mutate(AllH2,delta=eRank-row_number())
AllP2 <- mutate(AllP2,delta=eRank-row_number())


# vulture wins are a problem, or maybe not - relievers should get more wins
# maybe keep original rank order so I can see where maybe I can wait for the next pick
# create a formatted report


# STEP 3 - Strip out claimed players after each round
# http://craigandmary.com/DOCI/DOCIStandings.php

taken <- read.csv("../2024DOCIRosters.csv")
taken <- select(taken,Name=Player.Name)
AllH2 <- anti_join(AllH2,taken)
AllP2 <- anti_join(AllP2,taken)

write.csv(AllH2,"../dociHRank.csv")
write.csv(AllP2,"../dociPRank.csv")
