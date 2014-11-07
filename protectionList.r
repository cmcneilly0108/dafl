# TBD
# Evaluate combined projections - when they have 2015 available

library("xlsx")
library("stringr")
library("dplyr")
library("XML")
library("ggplot2")
library("reshape2")

source("./daflFunctions.r")



#Load steamer data
hitters <- read.fg("steamerH2015.csv")
hitters$pSGP <- hitSGP(hitters)

pitchers <- read.fg("steamerP2015.csv")
pitchers <- predictHolds(pitchers)
pitchers$pSGP <- pitSGPh(pitchers)

#Load 2013 final rosters
rosters <- read.cbs("2015RostersU.csv")
#split into P,H tables
rHitters <- filter(rosters,Pos != 'SP' & Pos != 'RP') 
rPitchers <- filter(rosters,Pos == 'SP' | Pos == 'RP')

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
rhitters <- inner_join(rHitters,AllH,by=c('playerid'),copy=FALSE)
rpitchers <- inner_join(rPitchers,AllP,by=c('playerid'),copy=FALSE)
rhitters <- select(rhitters,-Pos.x,-Player.x,-MLB.x) %>% rename(Pos=Pos.y,Player=Player.y,MLB=MLB.y)
rpitchers <- select(rpitchers,-Pos.x,-Player.x,-MLB.x) %>% rename(Pos=Pos.y,Player=Player.y,MLB=MLB.y)
rhitters$Value <- rhitters$pDFL - rhitters$Salary
rpitchers$Value <- rpitchers$pDFL - rpitchers$Salary

# Bucket pitchers by role
rpitchers$Pos <- with(rpitchers,ifelse(pSV>pHLD,'CL',ifelse(pHLD>pW,'MR','SP')))
AllP$Pos <- with(AllP,ifelse(pSV>pHLD,'CL',ifelse(pHLD>pW,'MR','SP')))


rpreds <- rbind(select(rhitters,Team,Player,Pos,Contract,Salary,pDFL,Value,s1=pHR,s2=pRBI,s3=pR,s4=pSB),
            select(rpitchers,Team,Player,Pos,Contract,Salary,pDFL,Value,s1=pW,s2=pSO,s3=pHLD,s4=pSV))

rpreds$DollarRate <- rpreds$pDFL/rpreds$Salary
rpreds <- rename(rpreds,Salary=Salary,Contract=Contract)

#lc <- filter(rpreds,Team == 'Liquor Crickets') %>% arrange(-Value)
lcp <- filter(rpreds,Team == 'Liquor Crickets',DollarRate > 1.3 | Value > 5) %>% arrange(-Value) %>%
  filter(rank(-Value) < 13)
lc <- filter(rpreds,Team == 'Liquor Crickets') %>% arrange(-Value)

# Logic - filter to players that either have a valueRate > 1.3 or totalValue > 5, sort by Value descending
totals <- rpreds %>% group_by(Team) %>% filter(rank(-Value) < 13,DollarRate > 1.1 | Value > 2) %>% 
  summarize(NumProtected = length(Team),
            Spent = sum(Salary),
            TotalValue = sum(pDFL),
            MoneyEarned = TotalValue - Spent,
            VPPlayer = TotalValue/NumProtected,
            DPRemaining = (260-sum(Salary))/(25-NumProtected),
            DollarValue = TotalValue/Spent) %>%
  arrange(-MoneyEarned)

prosters <- rpreds %>% group_by(Team) %>% filter(rank(-Value) < 13,DollarRate > 1.1 | Value > 2) %>%
  arrange(Team,-Value)
write.csv(prosters,"2014fakeprotected.csv")
# Need to give more weight to spending more
# sum(Salary) * DollarRate = probably too much?  log?

# Add in position eligibility based on 20 games
pedf <- read.xlsx("2014 Position Counts.xlsx",1)
pedf <- rename(pedf,Player=PLAYER,MLB=Team)
pedf$Player <- unlist(lapply(pedf$Player,swapName))
pedf <- mutate(pedf,posEl = ifelse(X1B>19,',1B',''))
pedf <- mutate(pedf,posEl = ifelse(X2B>19,str_c(posEl,',2B'),posEl))
pedf <- mutate(pedf,posEl = ifelse(SS>19,str_c(posEl,',SS'),posEl))
pedf <- mutate(pedf,posEl = ifelse(X3B>19,str_c(posEl,',3B'),posEl))
pedf <- mutate(pedf,posEl = ifelse(OF>19,str_c(posEl,',OF'),posEl))
pedf <- mutate(pedf,posEl = ifelse(C>19,str_c(posEl,',C'),posEl))
pedf <- mutate(pedf,posEl = ifelse(DH>19,str_c(posEl,',DH'),posEl))
pedf$posEl <- str_sub(pedf$posEl,2)
m2 <- select(master,-Pos)
p2 <- inner_join(pedf, m2,by=c('Player','MLB'))
p3 <- anti_join(pedf, m2,by=c('Player','MLB'))
# Merge rest with only name
p4 <- inner_join(p3, m2,by=c('Player'))
p4 <- select(p4,-MLB.x) %>% rename(MLB=MLB.y) 
pedf <- rbind(p2,p4) %>% select(playerid,posEl)
# Add column into AllH
AllH <- left_join(AllH,pedf,by=c('playerid')) 


#Create separate tabs by position
pc <- AllH %>% filter(Pos == 'C' | str_detect(posEl,'C'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,posEl,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
p1b <- AllH %>% filter(Pos == '1B' | str_detect(posEl,'1B'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,posEl,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
p2b <- AllH %>% filter(Pos == '2B' | str_detect(posEl,'2B'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,posEl,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
pss <- AllH %>% filter(Pos == 'SS' | str_detect(posEl,'SS'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,posEl,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
p3b <- AllH %>% filter(Pos == '3B' | str_detect(posEl,'3B'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,posEl,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
pdh <- AllH %>% filter(Pos == 'DH' | str_detect(posEl,'DH'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,posEl,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
pof <- AllH %>% filter(Pos == 'OF' | str_detect(posEl,'OF'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,posEl,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
pna <- AllH %>% filter(is.na(Pos),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)


psp <- AllP %>% filter(Pos=='SP',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,DFL=pDFL,SGP=pSGP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD)
pcl <- AllP %>% filter(Pos=='CL',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,DFL=pDFL,SGP=pSGP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD)
pmr <- AllP %>% filter(Pos=='MR',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,DFL=pDFL,SGP=pSGP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD)




# Create spreadsheet
protect <- createWorkbook()
tabs <- list()
tabs[[length(tabs)+1]] <- list('LeagueSummary',totals)
tabs[[length(tabs)+1]] <- list('AllCrickets',lc)
tabs[[length(tabs)+1]] <- list('ProjectedCrickets',lcp)
tabs[[length(tabs)+1]] <- list('AllRosters',prosters)
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



lapply(tabs,addSheet,protect)
saveWorkbook(protect,"protectionAnalysis.xlsx")

