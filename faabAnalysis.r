
library("xlsx")
library("stringr")
library("dplyr")
library("XML")
library("ggplot2")
library("reshape2")

source("./daflFunctions.r")

# Load accrued stats
hitters <- read.inseasonrecap("2014AccruedStats.xlsx",2)
hitters$pSGP <- hitSGP(hitters)

pitchers <- read.inseasonrecap("2014AccruedStats.xlsx",3)
pitchers <- rename(pitchers,pSV=pS,pHLD=pHD,pIP=pINN)
pitchers$pSGP <- pitSGP(pitchers)

#Generate dollars
nlist <- postDollars(hitters,pitchers)
AllH <- nlist[[1]]
AllP <- nlist[[2]]

RH <- AllH %>% group_by(pTeam) %>% summarize(hDFL = sum(pDFL))
RP <- AllP %>% group_by(pTeam) %>% summarize(piDFL = sum(pDFL))
RTot <- inner_join(RH,RP,by=c('pTeam')) %>% 
  mutate(tDFL = hDFL + piDFL,hRank = rank(-hDFL),pRank = rank(-piDFL)) %>% 
  arrange(-tDFL)

# Week 1 rosters
drosters <- read.cbs("2014DraftResults.csv")
drosters <- mutate(drosters,asrc='protect')
dc <- select(drosters,playerid,asrc)

# join and create w1 - which players were on week 1 roster?
AllH <- left_join(AllH,dc)
AllH$asrc <- replace(AllH$asrc,is.na(AllH$asrc),'faab')
AllH$asrc <- ifelse(AllH$asrc=='protect' & AllH$pContract==1,'draft',AllH$asrc)
AllP <- left_join(AllP,dc)
AllP$asrc <- replace(AllP$asrc,is.na(AllP$asrc),'faab')
AllP$asrc <- ifelse(AllP$asrc=='protect' & AllP$pContract==1,'draft',AllP$asrc)

# Load trades file
trades <- read.csv("2014trades.csv")
trades$Player <- unlist(lapply(trades$Players,swapName3))
trades <- select(trades,pTeam=Team,Player,Traded=Effective)
AllH <- left_join(AllH,trades,by=c('pTeam','Player'))
AllP <- left_join(AllP,trades,by=c('pTeam','Player'))
AllH$asrc <- ifelse(is.na(AllH$Traded),AllH$asrc,'trade')
AllP$asrc <- ifelse(is.na(AllP$Traded),AllP$asrc,'trade')

srcH <- select(AllH,pTeam,asrc,pDFL)
srcP <- select(AllP,pTeam,asrc,pDFL)
src <- rbind(srcH,srcP)
f <- src %>% group_by(pTeam,asrc) %>% summarize(srcDFL=sum(pDFL))
seasonResults <- dcast(f,pTeam ~ asrc)
seasonResults$protect <- replace(seasonResults$protect,is.na(seasonResults$protect),0)
seasonResults$faab <- replace(seasonResults$faab,is.na(seasonResults$faab),0)
seasonResults$trade <- replace(seasonResults$trade,is.na(seasonResults$trade),0)
seasonResults <- mutate(seasonResults,overall = draft+faab+protect+trade,drank = rank(-draft),
                        frank = rank(-faab),prank = rank(-protect),trank = rank(-trade)) %>%
  select(pTeam,overall,protect,prank,draft,drank,faab,frank,trade,trank) %>% arrange(-overall)

# Read in draftAnalysis
da <- read.xlsx("draftAnalysis.xlsx",2)
da <- select(da,pTeam=Team,prep=tDFL)
seasonResults2 <- inner_join(seasonResults,da)
da <- read.xlsx("draftAnalysis.xlsx",3)
da <- select(da,pTeam=Team,pred=tDFL)
seasonResults2 <- inner_join(seasonResults2,da)

seasonResults2 <- mutate(seasonResults2,deltap = protect - prep,deltad = draft-pred) %>% 
  select(pTeam,overall,protect,prank,prep,deltap,draft,drank,pred,deltad,faab,frank,trade,trank) %>% arrange(-overall)

# Create spreadsheet
review <- createWorkbook()
tabs <- list()
tabs[[length(tabs)+1]] <- list('Overview',seasonResults)
tabs[[length(tabs)+1]] <- list('Detail',seasonResults2)

lapply(tabs,addSheet,review)
saveWorkbook(review,"2014seasonReview.xlsx")
