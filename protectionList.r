

library("xlsx")
library("stringr")
library("dplyr")
library("XML")
library("ggplot2")
library("reshape2")
library("lubridate")

source("./daflFunctions.r")

fd <- file.info("steamerH2015.csv")$mtime
cd <- Sys.time()
dt <- difftime(cd, fd, units = "hours")
if (dt > 10) {
  system("./pullSteamer.sh")
}

#Load steamer data
hitters <- read.fg("steamerH2015.csv")
hitters$pSGP <- hitSGP(hitters)

pitchers <- read.fg("steamerP2015.csv")
pitchers <- predictHolds(pitchers)
pitchers$pSGP <- pitSGP(pitchers)

#Load 2014 final rosters
#rosters <- read.cbs("2015RostersU.csv")

#official file
rosters <- read.cbs("2014 Season Ending Rosters.csv")

#split into P,H tables
rHitters <- filter(rosters,Pos != 'SP' & Pos != 'RP') 
rPitchers <- filter(rosters,Pos == 'SP' | Pos == 'RP')

nteams <- 15
tdollars <- nteams * 260
pdollars <- round(tdollars*0.36)
hdollars <- tdollars - pdollars
nhitters <- 12
npitchers <- 13
chitters <- (nhitters * nteams)
cpitchers <- (npitchers * nteams)


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
  select(Player,MLB,posEl,Age,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
pc <- mutate(pc,RPV = (SGP - aRPV(pc))/aRPV(pc))
p1b <- AllH %>% filter(Pos == '1B' | str_detect(posEl,'1B'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,posEl,Age,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
p1b <- mutate(p1b,RPV = (SGP - aRPV(p1b))/aRPV(p1b))
p2b <- AllH %>% filter(Pos == '2B' | str_detect(posEl,'2B'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,posEl,Age,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
p2b <- mutate(p2b,RPV = (SGP - aRPV(p2b))/aRPV(p2b))
pss <- AllH %>% filter(Pos == 'SS' | str_detect(posEl,'SS'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,posEl,Age,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
pss <- mutate(pss,RPV = (SGP - aRPV(pss))/aRPV(pss))
p3b <- AllH %>% filter(Pos == '3B' | str_detect(posEl,'3B'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,posEl,Age,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
p3b <- mutate(p3b,RPV = (SGP - aRPV(p3b))/aRPV(p3b))
pdh <- AllH %>% filter(Pos == 'DH' | str_detect(posEl,'DH'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,posEl,Age,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
pdh <- mutate(pdh,RPV = (SGP - aRPV(pdh))/aRPV(pdh))
pof <- AllH %>% filter(Pos == 'OF' | str_detect(posEl,'OF'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,posEl,Age,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
pof <- mutate(pof,RPV = (SGP - aRPV(pof,45))/aRPV(pof,45))
pna <- AllH %>% filter(is.na(Pos),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,Age,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)


psp <- AllP %>% filter(Pos=='SP',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,Age,DFL=pDFL,SGP=pSGP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD)
psp <- mutate(psp,RPV = (SGP - aRPV(psp,120))/aRPV(psp,120))
pcl <- AllP %>% filter(Pos=='CL',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,Age,DFL=pDFL,SGP=pSGP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD)
pcl <- mutate(pcl,RPV = (SGP - aRPV(pcl))/aRPV(pcl))
pmr <- AllP %>% filter(Pos=='MR',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  select(Player,MLB,Age,DFL=pDFL,SGP=pSGP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD)
pmr <- mutate(pmr,RPV = (SGP - aRPV(pmr))/aRPV(pmr))


rpreds <- rbind(select(rhitters,Team,Player,Pos,Age,Contract,Salary,pDFL,Value,s1=pHR,s2=pRBI,s3=pR,s4=pSB),
            select(rpitchers,Team,Player,Pos,Age,Contract,Salary,pDFL,Value,s1=pW,s2=pSO,s3=pHLD,s4=pSV))

rpreds$DollarRate <- rpreds$pDFL/rpreds$Salary
rpreds <- rename(rpreds,Salary=Salary,Contract=Contract)

prosters <- rpreds %>% group_by(Team) %>% filter(rank(-Value) < 13,Value > 1) %>%
  arrange(Team,-Value)

ohi <- 0
opi <- 0
nhi <- 1
npi <- 1
ct <- 0
prosters2 <- prosters
while ((ohi != nhi | opi != npi) & ct < 8) {
  ct <- ct + 1
  inf <- calcInflation(prosters2)
  ohi <- nhi
  opi <- npi
  nhi <- inf[[1]]
  npi <- inf[[2]]
  rhitters2 <- rhitters
  rpitchers2 <- rpitchers
  rhitters2$pDFL <- (rhitters2$pDFL * nhi)
  rpitchers2$pDFL <- (rpitchers2$pDFL * npi)
  
  pmin <- min(rpitchers2$pDFL) - 1
  plost <- pmin * cpitchers
  rpitchers2$pDFL <- (rpitchers2$pDFL - pmin) * (pdollars/(pdollars - plost))
  hmin <- min(rhitters2$pDFL) - 1
  hlost <- hmin * chitters
  rhitters2$pDFL <- (rhitters2$pDFL - hmin) * (hdollars/(hdollars - hlost))
  
  rhitters2$Value <- rhitters2$pDFL - rhitters2$Salary
  rpitchers2$Value <- rpitchers2$pDFL - rpitchers2$Salary
  rpreds2 <- rbind(select(rhitters2,Team,Player,Pos,Age,Contract,Salary,pDFL,Value,s1=pHR,s2=pRBI,s3=pR,s4=pSB),
                   select(rpitchers2,Team,Player,Pos,Age,Contract,Salary,pDFL,Value,s1=pW,s2=pSO,s3=pHLD,s4=pSV))
  rpreds2$DollarRate <- rpreds2$pDFL/rpreds2$Salary
  prosters2 <- rpreds2 %>% group_by(Team) %>% filter(rank(-Value) < 13,Value > 1) %>%
    arrange(Team,-Value)
  print(nhi)
  print(npi)
}
rpreds <- rpreds2
prosters <- prosters2

#lc <- filter(rpreds,Team == 'Liquor Crickets') %>% arrange(-Value)
lcp <- filter(rpreds,Team == 'Liquor Crickets',Value > 1) %>% arrange(-Value) %>%
  filter(rank(-Value) < 13)
lc <- filter(rpreds,Team == 'Liquor Crickets') %>% arrange(-Value)

# Logic - filter to players that either have a valueRate > 1.3 or totalValue > 5, sort by Value descending
totals <- rpreds %>% group_by(Team) %>% filter(rank(-Value) < 13,Value > 1) %>% 
  summarize(NumProtected = length(Team),
            Spent = sum(Salary),
            TotalValue = sum(pDFL),
            MoneyEarned = TotalValue - Spent,
            VPPlayer = TotalValue/NumProtected,
            DPRemaining = (260-sum(Salary))/(25-NumProtected),
            DollarValue = TotalValue/Spent) %>%
  arrange(-MoneyEarned)

write.csv(prosters,"2014fakeprotected.csv")




bh <- as.data.frame(prosters) %>% filter(Value>20) %>% arrange(-Value)
bp <- as.data.frame(prosters) %>% filter(Pos=='SP',Value>7) %>% arrange(-Value) 

# Create Trends tab
targets <- data.frame()
targets <- rbind(targets,c(2014,pgoals('fs2014.csv')))
targets <- rbind(targets,c(2013,pgoals('fs2013.csv')))
targets <- rbind(targets,c(2012,pgoals('fs2012.csv')))
colnames(targets) <- c('year','HR','RBI','R','SB','AVG','W','K','SV','HLD','ERA')


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
tabs[[length(tabs)+1]] <- list('BP',bp)
tabs[[length(tabs)+1]] <- list('BH',bh)
tabs[[length(tabs)+1]] <- list('Targets',targets)



lapply(tabs,addSheet,protect)
saveWorkbook(protect,"protectionAnalysis.xlsx")

# some code to remember later
#sp <- inner_join(gleft,pitchers,by=c('Player')) %>% select(Player,playerid=playerid.y)
#sh <- inner_join(gleft,hitters,by=c('Player')) %>% select(Player,playerid=playerid.y)
#rooks <- rbind(sh,sp)
#write.csv(rooks,"2015RookieIDs.csv")



