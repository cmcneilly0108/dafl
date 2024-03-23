# BUG - not removing Wyatt Langford from prospects page

# For live draft, create CSV with all players and teams, free agents have Team = 'Free Agent'
# Then update draftguide to remove those rows - line 149 is when protected are removed


library("openxlsx")
library("stringr")
library("dplyr")
library("XML")
#library("ggplot2")
library("reshape2")
library("lubridate")
library("xml2")
library("rvest")
library("tidyr")
library("jsonlite")
source("./daflFunctions.r")

src <- 'atc'
#src <- 'steamer'
computer <- 'mac'
#computer <- 'windows'


#positionElig <- str_c(as.character(cyear-1),'PosElig.csv',sep='')

predUpdate <- FALSE
#predUpdate <- TRUE
fd <- file.info(str_c("../steamerH",cyear,".json"))$mtime
cd <- Sys.time()
dt <- difftime(cd, fd, units = "hours")
if (dt > 14) {
  system("bash ../scripts/pullSteamer.sh")
  system("bash ../scripts/pullATC.sh")
  system("bash ../scripts/fgInj.sh")
  system("bash ../scripts/pullRRClosers.sh")
  predUpdate <- TRUE
}


#official file
protected <- read.csv(str_c('../',as.character(cyear),'ProtectionLists.csv',sep=''),stringsAsFactors=FALSE)
#protected <- read.csv("../2024fakeprotected.csv",stringsAsFactors=FALSE)
protected$playerid <- as.character(protected$playerid)


#split into P,H tables
rHitters <- filter(protected,(Pos != 'P' & Pos != 'SP' & Pos != 'MR' & Pos != 'CL' & Pos != 'RP') | is.na(Pos))
rPitchers <- filter(protected,Pos == 'P' |Pos == 'SP' | Pos == 'MR' | Pos == 'CL' | Pos == 'RP')

hitters <- NULL
pitchers <- NULL
if (src=='atc') {
  hitters <- read.fg(str_c('../atcH',as.character(cyear),'.json',sep=''))
  pitchers <- read.fg(str_c('../atcP',as.character(cyear),'.json',sep=''))
} else {
  #Load steamer projection data
  #hitters <- read.fg(str_c('../steamerH',as.character(cyear),'.csv',sep=''))
  #pitchers <- read.fg(str_c('../steamerP',as.character(cyear),'.csv',sep=''))
  hitters <- read.fg(str_c('../steamerH',as.character(cyear),'.json',sep=''))
  pitchers <- read.fg(str_c('../steamerP',as.character(cyear),'.json',sep=''))
}

#pitchers <- predictHolds(pitchers)
pitchers$pSGP <- pitSGP(pitchers)
hitters$pSGP <- hitSGP(hitters)

#Generate pDFL for best players - no protections!
#nlist <- preDollars(hitters,pitchers)
nlist <- preLPP(hitters,pitchers)
thitters <- nlist[[1]]
tpitchers <- nlist[[2]]

# Incorporate scores back into AllH, AllP
AllH <- left_join(hitters,thitters,by=c('playerid'),copy=FALSE)
AllP <- left_join(pitchers,tpitchers,by=c('playerid'),copy=FALSE)
AllH <- dplyr::rename(AllH,pDFL=zDFL)
AllP <- dplyr::rename(AllP,pDFL=zDFL)
AllH$pDFL <- replace(AllH$pDFL,is.na(AllH$pDFL),0)
AllP$pDFL <- replace(AllP$pDFL,is.na(AllP$pDFL),0)

#merge with steamer
rhitters <- left_join(rHitters,AllH,by=c('playerid'),copy=FALSE)
rpitchers <- left_join(rPitchers,AllP,by=c('playerid'),copy=FALSE)
rhitters <- select(rhitters,-Pos.x,-Player.x,Pos=Pos.y,Player=Player.y)
rpitchers <- select(rpitchers,-Pos.x,-Player.x,Pos=Pos.y,Player=Player.y)
#rhitters <- select(rhitters,-Pos.x,-Player.x) %>% dplyr::rename(Pos=Pos.y,Player=Player.y)
#rpitchers <- select(rpitchers,-Pos.x,-Player.x) %>% dplyr::rename(Pos=Pos.y,Player=Player.y)
rhitters$Value <- rhitters$pDFL - rhitters$Salary
rpitchers$Value <- rpitchers$pDFL - rpitchers$Salary

#Generate overall Rankings by position

# 1.  Separate by pos, assign rank
AllH$Pos <- with(AllH,ifelse(Pos %in% c('LF','CF','RF'),'OF',Pos))
hrank <- AllH %>% group_by(Pos) %>% mutate(orank = rank(-pDFL))
hrank <- select(hrank,playerid,orank)
AllP$Pos <- with(AllP,ifelse(pSV>pHLD,'CL',ifelse(pHLD>pW,'MR','SP')))
prank <- AllP %>% group_by(Pos) %>% mutate(orank = rank(-pDFL))
prank <- select(prank,playerid,orank)

protClean <- rbind(select(rhitters,Team,Player,Contract,Salary,pDFL,Age,Pos,playerid),
                   select(rpitchers,Team,Player,Contract,Salary,pDFL,Age,Pos,playerid))
protClean$pDFL <- replace(protClean$pDFL,is.na(protClean$pDFL),0)


# Create Pre-Draft Standings
pstandings <- protClean %>% group_by(Team) %>%
  summarize(Players = length(Team),
            Spent = sum(Salary),
            TotalValue = sum(pDFL),
            Earned = TotalValue - Spent,
            VPPlayer = TotalValue/Players,
            DPP = (260-sum(Salary))/(25-Players),
            FullValue = TotalValue + auctionROI*(260-sum(Salary)),
            ValueRatio = TotalValue/Spent) %>%
  arrange(-FullValue)
pstandings$zScore <- as.numeric(scale(pstandings$FullValue))


lc <- filter(protClean,Team == 'Liquor Crickets') %>% select(-Team) %>% arrange(-pDFL)


#Generate pDFL for best players
#nlist <- preDollars(hitters,pitchers,protected)
nlist <- preLPP(hitters,pitchers,protected)
thitters <- nlist[[1]]
tpitchers <- nlist[[2]]


# Incorporate scores back into AllH, AllP
AllH <- left_join(hitters,thitters,by=c('playerid'))
AllP <- left_join(pitchers,tpitchers,by=c('playerid'))
AllH <- dplyr::rename(AllH,pDFL=zDFL)
AllP <- dplyr::rename(AllP,pDFL=zDFL)
AllH$pDFL <- replace(AllH$pDFL,is.na(AllH$pDFL),0)
AllP$pDFL <- replace(AllP$pDFL,is.na(AllP$pDFL),0)


# Bucket pitchers by role
#AllP$Pos <- with(AllP,ifelse(pSV>pHLD,'CL',ifelse(pHLD>pW,'MR','SP')))
AllP$Pos <- with(AllP,ifelse(pSV>pHLD,'CL',ifelse(pHLD>pW,'MR','SP')))
AllH$Pos <- with(AllH,ifelse(Pos %in% c('LF','CF','RF'),'OF',Pos))


# Add in position eligibility based on 20 games
#pedf <- read.cbs(positionElig)
pedf <- read.cbs(str_c("../",cyear,"PosElig.csv"))
pedf <- dplyr::rename(pedf,posEl=Eligible) %>% select(playerid,posEl)
# Add column into AllH
AllH <- left_join(AllH,pedf,by=c('playerid'))

# Create DADP = DAFL ADP
dafladp <- rbind(thitters,tpitchers)
dafladp <- dafladp %>% mutate(dadp = rank(-zDFL)) %>% select(-zDFL)
AllH <- left_join(AllH,dafladp,by=c('playerid'))
AllP <- left_join(AllP,dafladp,by=c('playerid'))

lc <- left_join(lc,pedf,by=c('playerid')) %>% select(Player,Age,posEl,Salary,Contract,pDFL)
protected <- left_join(protected,pedf,by=c('playerid'))
protected$posEl <- replace_na(protected$posEl,'P')

# Add back in orank
AllH <- inner_join(AllH,hrank,by=c('playerid','Pos'),multiple="first")
AllP <- inner_join(AllP,prank,by=c('playerid','Pos'),multiple="first")


# Injuries data
if (computer=='mac')
{
  injOrig <- read.csv("../latestInjuries.csv",stringsAsFactors=FALSE)
  injOrig <- injOrig %>% rename(`Latest Update` = `Latest.Update`,`Injury / Surgery Date` = `Injury...Surgery.Date`)

} else {
  injOrig <- getInjuriesRS()
}

inj <- injOrig %>% select(Player,Injury,Expected.Return=`Latest Update`)

pscores <- bind_rows(thitters,tpitchers)
injOrig <- left_join(injOrig,pscores,by=c('playerid'))
injOrig <- injOrig %>% rename(pDFL=zDFL)
injOrig$pDFL <- replace(injOrig$pDFL,is.na(injOrig$pDFL),0)
injOrig <- injOrig %>% select(Player,MLB,Pos,pDFL,`Injury / Surgery Date`,Injury,Status,`Latest Update`)
injOrig <- arrange(injOrig,Pos,-pDFL)

# remove protected players
injOrig <- anti_join(injOrig,protected,by=c('Player'))

#fake out until injuries get updated
#inj <- data_frame(Player="temp",Injury="",Expected.Return="")

AllH <- left_join(AllH,inj,by=c('Player'))
AllP <- left_join(AllP,inj,by=c('Player'))

# Remove protected players
#AllH <- anti_join(AllH,protected,by=c('Player'),copy=FALSE) %>% arrange(-pDFL)
#AllP <- anti_join(AllP,protected,by=c('Player'),copy=FALSE) %>% arrange(-pDFL)
# Remove protected players
AllH <- anti_join(AllH,protected,by=c('playerid'),copy=FALSE) %>% arrange(-pDFL)
AllP <- anti_join(AllP,protected,by=c('playerid'),copy=FALSE) %>% arrange(-pDFL)


OFY <- filter(inj,str_detect(Expected.Return,fixed('60-day',ignore_case=TRUE)))
#OFY <- filter(inj,str_detect(Expected.Return,fixed('out for the season',ignore_case=TRUE)))
lc <- left_join(lc,inj,by=c('Player'))

# New Pitches - From FanGraphs - hasn't been updated since 2020
#BUG - strip (date)
# r <- read_html("https://www.fangraphs.com/fantasy/2020-new-pitch-tracker/")
# r2 <- html_node(r,".fullpostentry") %>% html_nodes("li") %>% html_text()
# #r3 <- str_match(r2,".+\\) (.+) – (.+)")
# r3 <- str_match(r2,"(.+) – (.+)")
# #r3 <- str_match(r2,"(.+) - (.+)")
# npitch <- as.data.frame(na.omit(r3),stringsAsFactors = FALSE) %>%
#   select(-V1) %>% dplyr::rename(Player=V2,Pitch=V3)
# npitch$Player <- stripDates(npitch$Player)

#AllP <- left_join(AllP,npitch,by=c('Player'))

# oAllH <- read.csv('../AllHPrev.csv',stringsAsFactors=FALSE)
# oAllP <- read.csv('../AllPPrev.csv',stringsAsFactors=FALSE)
# oAllH <- select(oAllH,playerid,oDFL=pDFL)
# oAllP <- select(oAllP,playerid,oDFL=pDFL)
# oAllH2 <- inner_join(oAllH,AllH,by=c('playerid')) %>% mutate(cDFL = pDFL - oDFL) %>% select(Player,cDFL,pDFL,Injury,Expected.Return)
# oAllP2 <- inner_join(oAllP,AllP,by=c('playerid')) %>% mutate(cDFL = pDFL - oDFL) %>% select(Player,cDFL,pDFL,Injury,Expected.Return)
# change <- rbind(oAllH2,oAllP2) %>% filter(abs(cDFL) > 1) %>% arrange(cDFL)



#prospects from FanGraphs
hplist <- getFGScouts("../fangraphs-the-board-dataH.json")
pplist <- getFGScouts("../fangraphs-the-board-dataP.json")
#proh <- right_join(AllH,hplist,by=c('playerid'))
proh <- inner_join(AllH,hplist,by=c('playerid'))
proh <- proh %>% filter(cFV > 45)
prospectH <- select(proh,Player=Player.y,MLB=Team,Current.Level=mlevel,Pos,Age=Age.x,DFL=pDFL,ADP=pADP,FV=cFV,Top.100=Ovr_Rank,Hit,Game,Raw,Spd) %>%
  arrange(desc(FV))
#prop <- right_join(AllP,pplist,by=c('playerid')) %>% filter(cFV > 45)
prop <- inner_join(AllP,pplist,by=c('playerid')) %>% filter(cFV > 45)
prospectP <- select(prop,Player=Player.y,MLB=Team,Current.Level=mlevel,Age=Age.x,DFL=pDFL,ADP=pADP,FV=cFV,Top.100=Ovr_Rank,FB,SL,CB,CH,CMD) %>%
  arrange(desc(FV))

# Roster Resource Closer report
# Need to update json file
rrc <- getRRClosers()
rrcAvail <- inner_join(rrc,AllP,by=c('playerid'))
rrcResults <- arrange(rrcAvail,-pDFL) %>%
  select(Player,MLB,Pos,pDFL,pSGP,Role,Tags,pADP,pSV,pHLD,pW,pSO,pERA,`pK/9`,`pBB/9`,Injury,Expected.Return)
rrcResults <- rrcResults %>% arrange(Role,-pDFL)



if (predUpdate==TRUE) {
  oAllH <- read.csv('../AllHPrev.csv',stringsAsFactors=FALSE)
  oAllP <- read.csv('../AllPPrev.csv',stringsAsFactors=FALSE)
  oAllH <- select(oAllH,playerid,oDFL=pDFL)
  oAllP <- select(oAllP,playerid,oDFL=pDFL)
  oAllH2 <- inner_join(oAllH,AllH,by=c('playerid')) %>% mutate(cDFL = pDFL - oDFL) %>% select(Player,cDFL,pDFL,Injury,Expected.Return)
  oAllP2 <- inner_join(oAllP,AllP,by=c('playerid')) %>% mutate(cDFL = pDFL - oDFL) %>% select(Player,cDFL,pDFL,Injury,Expected.Return)
  change <- rbind(oAllH2,oAllP2) %>% filter(abs(cDFL) > 1) %>% arrange(cDFL)
  
  write.csv(AllH,'../AllHPrev.csv')
  write.csv(AllP,'../AllPPrev.csv')
}

#Create separate tabs by position
pc <- AllH %>% filter(Pos == 'C' | str_detect(posEl,'C'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% dplyr::rename(DFL=pDFL,SGP=pSGP)
pc <- mutate(pc,RPV = (SGP - aRPV(pc,nrow(filter(pc,DFL>0))))/aRPV(pc,nrow(filter(pc,DFL>0))))
pc <- select(pc,Player,MLB,posEl,Age,DFL,RPV,SGP,orank,ADP=pADP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)

p1b <- AllH %>% filter(Pos == '1B' | str_detect(posEl,'1B'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% dplyr::rename(DFL=pDFL,SGP=pSGP)
p1b <- mutate(p1b,RPV = (SGP - aRPV(p1b,nrow(filter(p1b,DFL>0))))/aRPV(p1b,nrow(filter(p1b,DFL>0))))
p1b <- select(p1b,Player,MLB,posEl,Age,DFL,RPV,SGP,orank,ADP=pADP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)

p2b <- AllH %>% filter(Pos == '2B' | str_detect(posEl,'2B'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% dplyr::rename(DFL=pDFL,SGP=pSGP)
p2b <- mutate(p2b,RPV = (SGP - aRPV(p2b,nrow(filter(p2b,DFL>0))))/aRPV(p2b,nrow(filter(p2b,DFL>0))))
p2b <- select(p2b,Player,MLB,posEl,Age,DFL,RPV,SGP,orank,ADP=pADP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)

pss <- AllH %>% filter(Pos == 'SS' | str_detect(posEl,'SS'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% dplyr::rename(DFL=pDFL,SGP=pSGP)
pss <- mutate(pss,RPV = (SGP - aRPV(pss,nrow(filter(pss,DFL>0))))/aRPV(pss,nrow(filter(pss,DFL>0))))
pss <- select(pss,Player,MLB,posEl,Age,DFL,RPV,SGP,orank,ADP=pADP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)

p3b <- AllH %>% filter(Pos == '3B' | str_detect(posEl,'3B'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% dplyr::rename(DFL=pDFL,SGP=pSGP)
p3b <- mutate(p3b,RPV = (SGP - aRPV(p3b,nrow(filter(p3b,DFL>0))))/aRPV(p3b,nrow(filter(p3b,DFL>0))))
p3b <- select(p3b,Player,MLB,posEl,Age,DFL,RPV,SGP,orank,ADP=pADP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)

pdh <- AllH %>% filter((is.na(Pos) | Pos == 'DH' | Pos == 'P'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% dplyr::rename(DFL=pDFL,SGP=pSGP)
#pdh <- AllH %>% filter(Pos == 'DH',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% dplyr::rename(DFL=pDFL,SGP=pSGP)
pdh <- mutate(pdh,RPV = (SGP - aRPV(pdh,nrow(filter(pdh,DFL>0))))/aRPV(pdh,nrow(filter(pdh,DFL>0))))
pdh <- select(pdh,Player,MLB,posEl,Age,DFL,RPV,SGP,orank,ADP=pADP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)

pof <- AllH %>% filter(Pos == 'OF' | str_detect(posEl,'OF'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>%
  dplyr::rename(DFL=pDFL,SGP=pSGP)  %>% head(200)
pof <- mutate(pof,RPV = (SGP - aRPV(pof,nrow(filter(pof,DFL>0))))/aRPV(pof,nrow(filter(pof,DFL>0))))
pof <- select(pof,Player,MLB,posEl,Age,DFL,RPV,SGP,orank,ADP=pADP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)

pna <- AllH %>% filter(is.na(Pos) | Pos=='',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>%
  select(Player,MLB,Age,DFL=pDFL,SGP=pSGP,ADP=pADP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)



psp <- AllP %>% filter(Pos=='SP',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% dplyr::rename(DFL=pDFL,SGP=pSGP) %>% head(200)
psp <- mutate(psp,RPV = (SGP - aRPV(psp,nrow(filter(psp,DFL>0))))/aRPV(psp,nrow(filter(psp,DFL>0))))
psp <- select(psp,Player,MLB,Age,DFL,RPV,SGP,orank,ADP=pADP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD,Injury,Expected.Return)

pcl <- AllP %>% filter(Pos=='CL',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% dplyr::rename(DFL=pDFL,SGP=pSGP)
pcl <- mutate(pcl,RPV = (SGP - aRPV(pcl,nrow(filter(pcl,DFL>0))))/aRPV(pcl,nrow(filter(pcl,DFL>0))))
pcl <- select(pcl,Player,MLB,Age,DFL,RPV,SGP,orank,ADP=pADP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD,Injury,Expected.Return)

pmr <- AllP %>% filter(Pos=='MR',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% dplyr::rename(DFL=pDFL,SGP=pSGP) %>% head(200)
pmr <- mutate(pmr,RPV = (SGP - aRPV(pmr,nrow(filter(pmr,DFL>0))))/aRPV(pmr,nrow(filter(pmr,DFL>0))))
pmr <- select(pmr,Player,MLB,Age,DFL,RPV,SGP,orank,ADP=pADP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD,Injury,Expected.Return)



# Create percent against goals worksheet
# Step 1 - Create targets
targets <- data.frame()
targets <- rbind(targets,c(lastyear,pgoals('../data/fs2023.csv')))
colnames(targets) <- c('year','HR','RBI','R','SB','AVG','W','K','SV','HLD','ERA')
targets <- select(targets,-AVG,-ERA,-year)
targets <- melt(targets)
targets <- dplyr::rename(targets,statistic = variable, goal = value)
# Step 2 - summarize protected projections
#lcht <- rhitters %>% filter(Team == 'Liquor Crickets') %>%
#  summarize(HR = sum(pHR),RBI=sum(pRBI),R=sum(pR),SB=sum(pSB))
#lcht <- melt(lcht) %>% dplyr::rename(statistic = variable, collected = value)
#hg <- inner_join(lcht,targets) %>% mutate(needed=goal-collected,pc = (collected/goal)*100)

#lcpt <- rpitchers %>% filter(Team == 'Liquor Crickets') %>%
#  summarize(W = sum(pW),HLD=sum(pHLD),K=sum(pSO),SV=sum(pSV))
#lcpt <- melt(lcpt) %>% dplyr::rename(statistic = variable, collected = value)
#pg <- inner_join(lcpt,targets) %>% mutate(needed=goal-collected,pc = (collected/goal)*100)

#gmet <- rbind(hg,pg) %>% arrange(pc)

gmet <- calcGoals(rpitchers,rhitters,targets,'Liquor Crickets')
# csmet <- calcGoals(rpitchers,rhitters,targets,'Crap Shooters')
# bjmet <- calcGoals(rpitchers,rhitters,targets,'But Justice')
# fdmet <- calcGoals(rpitchers,rhitters,targets,'Fluffy the Destroyer')
# ntmet <- calcGoals(rpitchers,rhitters,targets,'Neon Tetras')

firstPos <- function (str) {
  #s <- str_split(str,',')[[1]][1]
  s <- str_sub(str,0,str_locate(str,',')[[1]]-1)
  s
}

#don't have poselig in protected
#protected$Position <- with(protected,ifelse(Pos %in% c('LF','CF','RF'),'OF',Pos))
protected <- mutate(protected,Position=firstPos(posEl))
# if Pos in C,2B,SS, use Pos, else use Position
#protected <- mutate(protected,Position=ifelse(Pos=='2B',Pos,posEl))
protected <- mutate(protected,Position=ifelse((Pos %in% c('SS','2B','C') &
                                               str_detect(posEl,Pos)==TRUE),Pos,Position))
ppp <- group_by(protected,Position) %>% summarize(Count=length(Position))

# Who will be bidding against me?
# at <- as.data.frame(unique(protected$Team))
# names(at) <- c('Team')
# ww1b <- filter(rhitters,Pos %in% c('LF','CF','RF')) %>% group_by(Team) %>%
#   summarize(Num=length(Team),tSal=sum(Salary),tDFL=sum(pDFL)) %>%
#   as.data.frame()
# ww1b <- left_join(at,ww1b)
# ww1b[is.na(ww1b)] <- 0
# ww1b <- arrange(ww1b,tDFL)


# Estimate how they will spend rest based on 60/40 split, 13/12 players
currentSummary <- pstandings %>% mutate(PlayersLeft = 25 - Players,DollarsLeft = 260 - Spent) %>%
  select(Team,DollarsLeft,ExpValue = FullValue,zScore,PlayersLeft)
#htots <- rhitters %>% count(Team) %>% mutate(Hneeded = 13 - n) %>% select(-n)
htots <- rhitters %>% group_by(Team) %>% summarise(needed = 13 - n(),salleft=(260*(1-hpratio))-sum(Salary))
htots$group <- 'hitting'
#ptots <- rpitchers %>% count(Team) %>% mutate(Pneeded = 12 - n) %>% select(-n)
ptots <- rpitchers %>% group_by(Team) %>% summarise(needed = 12 - n(),salleft=(260*hpratio)-sum(Salary))
ptots$group <- 'pitching'
#currentSummary <- inner_join(currentSummary,htots)
#currentSummary <- inner_join(currentSummary,ptots)
currentSummary <- bind_rows(htots,ptots) %>% arrange(group,-salleft) %>% select(Team,group,needed,salleft)

hitterTotal <- 14*13
pitcherTotal <- 14*12
hitterTaken <- nrow(rhitters)
pitcherTaken <- nrow(rpitchers)
hitterSpent <- sum(rhitters$Salary)
pitcherSpent <- sum(rpitchers$Salary)
hitterMoneyTotal <- 14*(260*(1-hpratio))
pitcherMoneyTotal <- 14*(260*hpratio)
rhitters$pDFL <- rhitters$pDFL %>% replace_na(0)
hpdfl <- sum(rhitters$pDFL)
ppdfl <- sum(rpitchers$pDFL)
hlv <- (filter(rhitters,pDFL < 2) %>% nrow())/hitterTotal
plv <- filter(rpitchers,pDFL < 2) %>% nrow()/pitcherTotal

# TBD - how many hitters/pitchers do teams end up with after the draft?
# TBD - how many low value players protected?
hpr <- hitterTaken/hitterTotal
hsr <- hitterSpent/hitterMoneyTotal
ppr <- pitcherTaken/pitcherTotal
psr <- pitcherSpent/pitcherMoneyTotal
hvr <- hpdfl/hitterMoneyTotal
pvr <- ppdfl/pitcherMoneyTotal

hnleft <- hitterTotal - hitterTaken
pnleft <- pitcherTotal - pitcherTaken

protectSummary <- data.frame(type=c("hitter","pitcher"),playersProt=c(hpr,ppr),dollarsSpent=c(hsr,psr),
                             ToFill=c(hnleft,pnleft),valueTaken=c(hvr,pvr))

# List of hitters to burn first
topHitters <- AllH %>% filter(pDFL > 20) %>% select(Player,MLB,posEl,Age,pDFL,ADP=pADP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)

# From Athletic article combining saves and holds
# savesholds <- read.csv('20athrelievers.csv',stringsAsFactors=FALSE)
# savesholds <- rename(savesholds,Player=Pitcher)
# savesholds <- inner_join(savesholds,AllP)
# savesholds <- select(savesholds,Player,MLB,Age,Rank,pDFL,ADP=pADP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD,Injury,Expected.Return,Pitch)


# Create spreadsheet
draft <- createWorkbook()
headerStyle <- createStyle(halign = "CENTER", textDecoration = "Bold")
csRatioColumn <- createStyle(numFmt = "##0.000")
csIntegerColumn <- createStyle(numFmt = "##0")
csMoneyColumn <- createStyle(numFmt = "CURRENCY")


addWorksheet(draft,'Early Standings')
writeData(draft,'Early Standings',pstandings,headerStyle = headerStyle)
addStyle(draft, 'Early Standings',style = csMoneyColumn,rows = 2:20, cols = 4:8,gridExpand = TRUE)
addStyle(draft, 'Early Standings',style = csRatioColumn,rows = 2:20, cols = 9:10,gridExpand = TRUE)
setColWidths(draft, 'Early Standings', cols = 1:10, widths = "auto")

addWorksheet(draft,'Crickets')
writeData(draft,'Crickets',lc,headerStyle = headerStyle)
addStyle(draft, 'Crickets',style = csIntegerColumn,rows = 2:20, cols = 2,gridExpand = TRUE)
addStyle(draft, 'Crickets',style = csMoneyColumn,rows = 2:20, cols = 4,gridExpand = TRUE)
addStyle(draft, 'Crickets',style = csMoneyColumn,rows = 2:20, cols = 6,gridExpand = TRUE)
setColWidths(draft, 'Crickets', cols = 1:10, widths = "auto")

addWorksheet(draft,'Goals')
writeData(draft,'Goals',gmet,headerStyle = headerStyle)
addStyle(draft, 'Goals',style = csRatioColumn,rows = 2:20, cols = 5,gridExpand = TRUE)
addStyle(draft, 'Goals',style = csIntegerColumn,rows = 2:20, cols = 2:4,gridExpand = TRUE)
setColWidths(draft, 'Goals', cols = 1:10, widths = "auto")

addWorksheet(draft,'Protected by Position')
writeData(draft,'Protected by Position',ppp,headerStyle = headerStyle)
addStyle(draft, 'Protected by Position',style = csRatioColumn,rows = 2:20, cols = 5,gridExpand = TRUE)

addWorksheet(draft,'Protected Breakdown')
writeData(draft,'Protected Breakdown',currentSummary,headerStyle = headerStyle)
addStyle(draft, 'Protected Breakdown',style = csIntegerColumn,rows = 2:40, cols = 3:4,gridExpand = TRUE)
setColWidths(draft, 'Protected Breakdown', cols = 1:20, widths = "auto")

addWorksheet(draft,'Auction Pool')
writeData(draft,'Auction Pool',protectSummary,headerStyle = headerStyle)
addStyle(draft, 'Auction Pool',style = csRatioColumn,rows = 2:20, cols = 2:5,gridExpand = TRUE)

addWorksheet(draft,'C')
writeData(draft,'C',pc,headerStyle = headerStyle)
addStyle(draft, 'C',style = csMoneyColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
addStyle(draft, 'C',style = csRatioColumn,rows = 2:200, cols = 6:7,gridExpand = TRUE)
addStyle(draft, 'C',style = csRatioColumn,rows = 2:200, cols = 14,gridExpand = TRUE)
addStyle(draft, 'C',style = csIntegerColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
addStyle(draft, 'C',style = csIntegerColumn,rows = 2:200, cols = 8:13,gridExpand = TRUE)
setColWidths(draft, 'C', cols = 1:20, widths = "auto")

addWorksheet(draft,'1B')
writeData(draft,'1B',p1b,headerStyle = headerStyle)
addStyle(draft, '1B',style = csMoneyColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
addStyle(draft, '1B',style = csRatioColumn,rows = 2:200, cols = 6:7,gridExpand = TRUE)
addStyle(draft, '1B',style = csRatioColumn,rows = 2:200, cols = 14,gridExpand = TRUE)
addStyle(draft, '1B',style = csIntegerColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
addStyle(draft, '1B',style = csIntegerColumn,rows = 2:200, cols = 8:13,gridExpand = TRUE)
setColWidths(draft, '1B', cols = 1:20, widths = "auto")

addWorksheet(draft,'2B')
writeData(draft,'2B',p2b,headerStyle = headerStyle)
addStyle(draft, '2B',style = csMoneyColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
addStyle(draft, '2B',style = csRatioColumn,rows = 2:200, cols = 6:7,gridExpand = TRUE)
addStyle(draft, '2B',style = csRatioColumn,rows = 2:200, cols = 14,gridExpand = TRUE)
addStyle(draft, '2B',style = csIntegerColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
addStyle(draft, '2B',style = csIntegerColumn,rows = 2:200, cols = 8:13,gridExpand = TRUE)
setColWidths(draft, '2B', cols = 1:20, widths = "auto")

addWorksheet(draft,'SS')
writeData(draft,'SS',pss,headerStyle = headerStyle)
addStyle(draft, 'SS',style = csMoneyColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
addStyle(draft, 'SS',style = csRatioColumn,rows = 2:200, cols = 6:7,gridExpand = TRUE)
addStyle(draft, 'SS',style = csRatioColumn,rows = 2:200, cols = 14,gridExpand = TRUE)
addStyle(draft, 'SS',style = csIntegerColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
addStyle(draft, 'SS',style = csIntegerColumn,rows = 2:200, cols = 8:13,gridExpand = TRUE)
setColWidths(draft, 'SS', cols = 1:20, widths = "auto")

addWorksheet(draft,'3B')
writeData(draft,'3B',p3b,headerStyle = headerStyle)
addStyle(draft, '3B',style = csMoneyColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
addStyle(draft, '3B',style = csRatioColumn,rows = 2:200, cols = 6:7,gridExpand = TRUE)
addStyle(draft, '3B',style = csRatioColumn,rows = 2:200, cols = 14,gridExpand = TRUE)
addStyle(draft, '3B',style = csIntegerColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
addStyle(draft, '3B',style = csIntegerColumn,rows = 2:200, cols = 8:13,gridExpand = TRUE)
setColWidths(draft, '3B', cols = 1:20, widths = "auto")

addWorksheet(draft,'OF')
writeData(draft,'OF',pof,headerStyle = headerStyle)
addStyle(draft, 'OF',style = csMoneyColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
addStyle(draft, 'OF',style = csRatioColumn,rows = 2:200, cols = 6:7,gridExpand = TRUE)
addStyle(draft, 'OF',style = csRatioColumn,rows = 2:200, cols = 14,gridExpand = TRUE)
addStyle(draft, 'OF',style = csIntegerColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
addStyle(draft, 'OF',style = csIntegerColumn,rows = 2:200, cols = 8:13,gridExpand = TRUE)
setColWidths(draft, 'OF', cols = 1:20, widths = "auto")

addWorksheet(draft,'DH')
writeData(draft,'DH',pdh,headerStyle = headerStyle)
addStyle(draft, 'DH',style = csMoneyColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
addStyle(draft, 'DH',style = csRatioColumn,rows = 2:200, cols = 6:7,gridExpand = TRUE)
addStyle(draft, 'DH',style = csRatioColumn,rows = 2:200, cols = 14,gridExpand = TRUE)
addStyle(draft, 'DH',style = csIntegerColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
addStyle(draft, 'DH',style = csIntegerColumn,rows = 2:200, cols = 8:13,gridExpand = TRUE)
setColWidths(draft, 'DH', cols = 1:20, widths = "auto")

addWorksheet(draft,'Other')
writeData(draft,'Other',pna,headerStyle = headerStyle)
addStyle(draft, 'Other',style = csMoneyColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
addStyle(draft, 'Other',style = csRatioColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
addStyle(draft, 'Other',style = csRatioColumn,rows = 2:200, cols = 11,gridExpand = TRUE)
addStyle(draft, 'Other',style = csIntegerColumn,rows = 2:200, cols = 3,gridExpand = TRUE)
addStyle(draft, 'Other',style = csIntegerColumn,rows = 2:200, cols = 6:10,gridExpand = TRUE)
setColWidths(draft, 'Other', cols = 1:20, widths = "auto")

addWorksheet(draft,'SP')
writeData(draft,'SP',psp,headerStyle = headerStyle)
addStyle(draft, 'SP',style = csMoneyColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
addStyle(draft, 'SP',style = csRatioColumn,rows = 2:200, cols = 5:6,gridExpand = TRUE)
addStyle(draft, 'SP',style = csIntegerColumn,rows = 2:200, cols = 3,gridExpand = TRUE)
addStyle(draft, 'SP',style = csIntegerColumn,rows = 2:200, cols = 7:10,gridExpand = TRUE)
addStyle(draft, 'SP',style = csRatioColumn,rows = 2:200, cols = 11,gridExpand = TRUE)
addStyle(draft, 'SP',style = csIntegerColumn,rows = 2:200, cols = 12:13,gridExpand = TRUE)
setColWidths(draft, 'SP', cols = 1:20, widths = "auto")

addWorksheet(draft,'MR')
writeData(draft,'MR',pmr,headerStyle = headerStyle)
addStyle(draft, 'MR',style = csMoneyColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
addStyle(draft, 'MR',style = csRatioColumn,rows = 2:200, cols = 5:6,gridExpand = TRUE)
addStyle(draft, 'MR',style = csIntegerColumn,rows = 2:200, cols = 3,gridExpand = TRUE)
addStyle(draft, 'MR',style = csIntegerColumn,rows = 2:200, cols = 7:10,gridExpand = TRUE)
addStyle(draft, 'MR',style = csRatioColumn,rows = 2:200, cols = 11,gridExpand = TRUE)
addStyle(draft, 'MR',style = csIntegerColumn,rows = 2:200, cols = 12:13,gridExpand = TRUE)
setColWidths(draft, 'MR', cols = 1:20, widths = "auto")

addWorksheet(draft,'CL')
writeData(draft,'CL',pcl,headerStyle = headerStyle)
addStyle(draft, 'CL',style = csMoneyColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
addStyle(draft, 'CL',style = csRatioColumn,rows = 2:200, cols = 5:6,gridExpand = TRUE)
addStyle(draft, 'CL',style = csIntegerColumn,rows = 2:200, cols = 3,gridExpand = TRUE)
addStyle(draft, 'CL',style = csIntegerColumn,rows = 2:200, cols = 7:10,gridExpand = TRUE)
addStyle(draft, 'CL',style = csRatioColumn,rows = 2:200, cols = 11,gridExpand = TRUE)
addStyle(draft, 'CL',style = csIntegerColumn,rows = 2:200, cols = 12:13,gridExpand = TRUE)
setColWidths(draft, 'CL', cols = 1:20, widths = "auto")

addWorksheet(draft,'RRClosers')
writeData(draft,'RRClosers',rrcResults,headerStyle = headerStyle)
addStyle(draft, 'RRClosers',style = csMoneyColumn,rows = 2:400, cols = 4,gridExpand = TRUE)
addStyle(draft, 'RRClosers',style = csRatioColumn,rows = 2:400, cols = 5,gridExpand = TRUE)
addStyle(draft, 'RRClosers',style = csIntegerColumn,rows = 2:400, cols = 8:12,gridExpand = TRUE)
addStyle(draft, 'RRClosers',style = csRatioColumn,rows = 2:400, cols = 13:15,gridExpand = TRUE)
setColWidths(draft, 'RRClosers', cols = 1:29, widths = "auto")

# # Athletic Article
# addWorksheet(draft,'SavesHolds')
# writeData(draft,'SavesHolds',savesholds,headerStyle = headerStyle)
# addStyle(draft, 'SavesHolds',style = csMoneyColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
# #addStyle(draft, 'SavesHolds',style = csRatioColumn,rows = 2:200, cols = 5:6,gridExpand = TRUE)
# setColWidths(draft, 'SavesHolds', cols = 1:20, widths = "auto")


# st <- list('5'=csMoneyColumn,'6'=csRatioColumn)
# tabs[[length(tabs)+1]] <- list('HitProspect',hp,st,c(2,14))
addWorksheet(draft,'HitProspect')
writeData(draft,'HitProspect',prospectH,headerStyle = headerStyle)
addStyle(draft, 'HitProspect',style = csMoneyColumn,rows = 2:200, cols = 6,gridExpand = TRUE)
addStyle(draft, 'HitProspect',style = csIntegerColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
addStyle(draft, 'HitProspect',style = csIntegerColumn,rows = 2:200, cols = 7,gridExpand = TRUE)
setColWidths(draft, 'HitProspect', cols = 1:20, widths = "auto")

# tabs[[length(tabs)+1]] <- list('PitProspect',pp,st,c(2,14))
addWorksheet(draft,'PitProspect')
writeData(draft,'PitProspect',prospectP,headerStyle = headerStyle)
addStyle(draft, 'PitProspect',style = csMoneyColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
addStyle(draft, 'PitProspect',style = csIntegerColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
addStyle(draft, 'PitProspect',style = csIntegerColumn,rows = 2:200, cols = 6,gridExpand = TRUE)
setColWidths(draft, 'PitProspect', cols = 1:20, widths = "auto")

# addWorksheet(draft,'OutForYear')
# writeData(draft,'OutForYear',OFY,headerStyle = headerStyle)
# setColWidths(draft, 'OutForYear', cols = 1:20, widths = "auto")

addWorksheet(draft,'TopHitters')
writeData(draft,'TopHitters',topHitters,headerStyle = headerStyle)
addStyle(draft, 'TopHitters',style = csMoneyColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
addStyle(draft, 'TopHitters',style = csRatioColumn,rows = 2:200, cols = 6,gridExpand = TRUE)
addStyle(draft, 'TopHitters',style = csRatioColumn,rows = 2:200, cols = 11,gridExpand = TRUE)
addStyle(draft, 'TopHitters',style = csIntegerColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
addStyle(draft, 'TopHitters',style = csIntegerColumn,rows = 2:200, cols = 6:10,gridExpand = TRUE)
setColWidths(draft, 'TopHitters', cols = 1:20, widths = "auto")

addWorksheet(draft,'Injured')
writeData(draft,'Injured',injOrig,headerStyle = headerStyle)
addStyle(draft, 'Injured',style = csMoneyColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
#addStyle(draft, 'Injured',style = csRatioColumn,rows = 2:200, cols = 6,gridExpand = TRUE)
#addStyle(draft, 'TopHitters',style = csRatioColumn,rows = 2:200, cols = 11,gridExpand = TRUE)
setColWidths(draft, 'Injured', cols = 1:20, widths = "auto")
# st <- list('5'=csPctColumn)
# tabs[[length(tabs)+1]] <- list('BJGoals',bjmet,st,c())
# 
# tabs[[length(tabs)+1]] <- list('FDGoals',fdmet,st,c())
# 
# tabs[[length(tabs)+1]] <- list('CSGoals',csmet,st,c())
# 
# st <- list('2'=csMoneyColumn,'3'=csMoneyColumn)
# tabs[[length(tabs)+1]] <- list('Recent Changes',change,st,c(2))
if (predUpdate==TRUE) {
  addWorksheet(draft,'Recent Changes')
  writeData(draft,'Recent Changes',change,headerStyle = headerStyle)
  addStyle(draft, 'Recent Changes',style = csMoneyColumn,rows = 2:200, cols = 2:3,gridExpand = TRUE)
  setColWidths(draft, 'Recent Changes', cols = 1:20, widths = "auto")
}

saveWorkbook(draft,"../draftGuide.xlsx",overwrite = TRUE)

# histograms
# hist(rhitters$Value)
# hist(rpitchers$Value)
hist(rhitters$pDFL)
hist(rpitchers$pDFL)
# hist(AllH$pDFL)
# hist(AllP$pDFL)
AP2 <- filter(AllP,pDFL > 0)
hist(AP2$pDFL)
AH2 <- filter(AllH,pDFL > 0)
hist(AH2$pDFL)


# create scenarios
# Step 1 - create random lineup
#    fill out dataframe

blanklineup <- data_frame(posid=c('C','1B','2B','SS','3B','OF1','OF2','OF3','U'),
                          pos=c('C','1B','2B','SS','3B','OF','OF','OF','U'),
                          playerid=NA,psalary=1,pHR=0,pRBI=0,pR=0,pSB=0)
blanklineup$playerid <- as.character(blanklineup$playerid)
poslist <- data_frame(posid=c('C','1B','2B','SS','3B','OF1','OF2','OF3','U'),
                      pos=c('C','1B','2B','SS','3B','OF','OF','OF','U'))

pickplayer <- function(pposid,ppos,maxSalary) {
  #m2sal <- ifelse((maxSalary>40),40,maxSalary)
  #rplayer <- AllH %>% filter(Pos == ppos | str_detect(posEl,ppos),pSGP > 0,pDFL <= m2sal) %>% sample_n(size=1)
  rplayer <- AllH %>% filter(Pos == ppos | str_detect(posEl,ppos),pSGP > 0,pDFL <= maxSalary) %>% sample_n(size=1)
  psalary <- round(rplayer$pDFL)
  psalary <- ifelse(psalary < 1,1,psalary)
  data_frame(posid=pposid,pos=ppos,lock=0,playerid=rplayer$playerid,psalary,pHR=rplayer$pHR,
             pRBI=rplayer$pRBI,pR=rplayer$pR,pSB=rplayer$pSB)
}

randomLineup <- function(odf,sal) {
  edf <- odf[is.na(odf$playerid),]
  fdf <- odf[!is.na(odf$playerid),]
  if (nrow(edf) == 0) {
    odf
  } else {
    rint <- sample.int(nrow(edf), 1)
    pickpos <- edf[rint,]
    # remove row from new_df
    smaller_df <- edf[-rint,]
    # call randomLineup on smaller df
    returned_df <- randomLineup(smaller_df,sal-1 - sum(fdf$psalary))
    # get back df and add new row back in
    mbid <- sal - sum(returned_df$psalary) - sum(fdf$psalary)
    #print(mbid)
    npid <- pickplayer(pickpos$posid,pickpos$pos,mbid)
    # still have to add back in non-NAs from first call
    rbind(fdf,returned_df,npid)
  }
}

rosterScore <- function(roster) {
  hrs <- (sum(roster[,5]))/targets[targets$statistic=='HR',]$goal
  hrs <- ifelse((hrs>1),1,hrs)
  rbis <- (sum(roster[,6]))/targets[targets$statistic=='RBI',]$goal
  rbis <- ifelse((rbis>1),1,rbis)
  rs <- (sum(roster[,7]))/targets[targets$statistic=='R',]$goal
  rs <- ifelse((rs>1),1,rs)
  sbs <- (sum(roster[,8]))/targets[targets$statistic=='SB',]$goal
  sbs <- ifelse((sbs>1),1,sbs)
  totalscore <- (hrs+rbis+rs+sbs)/4
  totalscore
}

generateGeneration <- function(slist,msal) {
  topr <- slist[[1]]
  toplock <- filter(topr,lock==1)
  topnolock <- filter(topr,lock ==0)
  # mate with next 4 - TBD
  
  # mutate - remove 3,2,1 players
  ct<- 1
  nk <- nrow(topnolock) - 3
  while (ct <= 10) {
    cr <- sample_n(topnolock,nk) %>% rbind(toplock)
    cr <- left_join(poslist,cr,by = c("posid", "pos"))
    cr <- randomLineup(cr,msal)
    slist <- append(slist,list(cr))
    ct <- ct + 1
  }
  
  ct<- 1
  nk <- nrow(topnolock) - 2
    while (ct <= 10) {
    cr <- sample_n(topnolock,nk) %>% rbind(toplock)
    cr <- left_join(poslist,cr,by = c("posid", "pos"))
    cr <- randomLineup(cr,msal)
    slist <- append(slist,list(cr))
    ct <- ct + 1
  }
  
  ct<- 1
  nk <- nrow(topnolock) - 1
  while (ct <= 10) {
    cr <- sample_n(topnolock,nk) %>% rbind(toplock)
    cr <- left_join(poslist,cr,by = c("posid", "pos"))
    cr <- randomLineup(cr,msal)
    slist <- append(slist,list(cr))
    ct <- ct + 1
  }
  
  wht <- lapply(slist,rosterScore)
  new_vector2 <- unlist(wht, use.names = FALSE)
  which.max(new_vector2)
  print(max(new_vector2))
  slist
}

# mcricks <- read.csv("../forecastCricks.csv")
# mcricks <- filter(mcricks,posid!="OF2")
# cr <- left_join(poslist,mcricks,by = c("posid", "pos"))
# mPay <- 166 - sum(mcricks$psalary)
# #cr <- randomLineup(cr,mPay)
# 
# #maxPay <- 166
# counter <- 1
# rosters <- list()
# 
# while (counter <= 30) {
#   #nr <- randomLineup(blanklineup,maxPay)
#   nr <- randomLineup(cr,maxPay)
#   rosters <- append(rosters,list(nr))
#   counter <- counter + 1
# }
# 
# wht <- lapply(rosters,rosterScore)
# new_vector <- unlist(wht, use.names = FALSE)
# #which.max(new_vector)
# print(max(new_vector))
# 
# #create new set of 30
# nextr <- list()
# 
# # add in best
# nextr <- append(nextr,list(rosters[[which.max(new_vector)]]))
# 
# ctr <- 1
# while (ctr <= 50) {
#   nround <- list()
#   nround <- generateGeneration(nextr,maxPay)
#   # BUG - genGen doubles my protected list - didn't double SS
#   ctr <- ctr + 1
#   nextr <- list()
#   wht <- lapply(nround,rosterScore)
#   new_vector <- unlist(wht, use.names = FALSE)
#   nextr <- append(nextr,list(nround[[which.max(new_vector)]]))
# }
# 
# myteam <- nround[[which.max(new_vector)]]
# mt4 <- left_join(myteam,AllH)
# sum(mt2$psalary)
pfix <- pedf %>% filter(str_starts(playerid,"cbs"))