
library("xlsx")
library("stringr")
library("dplyr")
library("XML")
library("ggplot2")
library("reshape2")
library("lubridate")

source("./daflFunctions.r")

predUpdate <- FALSE
fd <- file.info("steamerH2015.csv")$mtime
cd <- Sys.time()
dt <- difftime(cd, fd, units = "hours")
if (dt > 10) {
  system("./pullSteameru.sh")
  #predUpdate <- TRUE
}

#Load protection list
#protected <- read.csv("2014fakeprotected.csv",stringsAsFactors=FALSE)
#p2 <- read.xlsx("2015 Protection Lists.xlsx",1,stringsAsFactors=FALSE)

#merge with Master file
#m2 <- select(master,-Pos,-Player) %>% rename(Player=cbs_name)
#gname <- inner_join(protected, m2,by=c('Player')) %>% select(Team,Player,playerid,Pos,Contract,Salary)
#grest <- anti_join(protected, m2,by=c('Player'))
#gname$playerid <- ifelse(is.na(gname$playerid),gname$Player,gname$playerid)

#rooks <- read.csv('2015RookieIDs.csv',stringsAsFactors=FALSE) %>% select(-X)
#g2name <- inner_join(grest,rooks,by=c('Player')) %>% select(Team,Player,playerid,Pos,Contract,Salary)
#protected <- rbind(gname,g2name)

#official file
protected <- read.cbs("2015 Protection List.csv")


#split into P,H tables
rHitters <- filter(protected,(Pos != 'SP' & Pos != 'MR' & Pos != 'CL' & Pos != 'RP') | is.na(Pos))
rPitchers <- filter(protected,Pos == 'SP' | Pos == 'MR' | Pos == 'CL' | Pos == 'RP')

#Load steamer projection data
hitters <- read.fg("steamerH2015.csv")
#hitters <- read.fg("combinedH2015.csv")
hitters$pSGP <- hitSGP(hitters)

pitchers <- read.fg("steamerP2015.csv")
#pitchers <- read.fg("combinedP2015.csv")
pitchers <- predictHolds(pitchers)
pitchers$pSGP <- pitSGP(pitchers)

#Generate pDFL for best players - no protections!
#nlist <- preDollars(hitters,pitchers)
nlist <- preLPP(hitters,pitchers)
thitters <- nlist[[1]]
tpitchers <- nlist[[2]]

# Incorporate scores back into AllH, AllP
AllH <- left_join(hitters,thitters,by=c('playerid'))
AllP <- left_join(pitchers,tpitchers,by=c('playerid'))
AllH <- rename(AllH,pDFL=zDFL)
AllP <- rename(AllP,pDFL=zDFL)
AllH$pDFL <- replace(AllH$pDFL,is.na(AllH$pDFL),0)
AllP$pDFL <- replace(AllP$pDFL,is.na(AllP$pDFL),0)
#merge with steamer
rhitters <- left_join(rHitters,AllH,by=c('playerid'),copy=FALSE)
rpitchers <- left_join(rPitchers,AllP,by=c('playerid'),copy=FALSE)
rhitters <- select(rhitters,-Pos.x,-Player.x) %>% rename(Pos=Pos.y,Player=Player.y)
rpitchers <- select(rpitchers,-Pos.x,-Player.x) %>% rename(Pos=Pos.y,Player=Player.y)
rhitters$Value <- rhitters$pDFL - rhitters$Salary
rpitchers$Value <- rpitchers$pDFL - rpitchers$Salary

#Generate overall Rankings by position

# 1.  Separate by pos, assign rank
AllH$Pos <- with(AllH,ifelse(Pos %in% c('LF','CF','RF'),'OF',Pos))
hrank <- AllH %>% group_by(Pos) %>% mutate(orank = rank(-pDFL,-pSGP))
hrank <- select(hrank,playerid,orank)
AllP$Pos <- with(AllP,ifelse(pSV>pHLD,'CL',ifelse(pHLD>pW,'MR','SP')))
prank <- AllP %>% group_by(Pos) %>% mutate(orank = rank(-pDFL,-pSGP))
prank <- select(prank,playerid,orank)

protClean <- rbind(select(rhitters,Team,Player,Contract,Salary,pDFL),
                   select(rpitchers,Team,Player,Contract,Salary,pDFL))
protClean$pDFL <- replace(protClean$pDFL,is.na(protClean$pDFL),0)


# Create Pre-Draft Standings
pstandings <- protClean %>% group_by(Team) %>%
  summarize(Players = length(Team),
            Spent = sum(Salary),
            TotalValue = sum(pDFL),
            Earned = TotalValue - Spent,
            VPPlayer = TotalValue/Players,
            DPP = (260-sum(Salary))/(25-Players),
            FullValue = TotalValue + (260-sum(Salary)),
            ValueRatio = TotalValue/Spent) %>%
  arrange(-Earned)
pstandings$zScore <- as.numeric(scale(pstandings$Earned))

#This works, but only because I manually created the fake file.  Change up to merge with current projections.

lc <- filter(protClean,Team == 'Liquor Crickets') %>% select(-Team) %>% arrange(-pDFL)


#Generate pDFL for best players
#nlist <- preDollars(hitters,pitchers,protected)
nlist <- preLPP(hitters,pitchers,protected)
thitters <- nlist[[1]]
tpitchers <- nlist[[2]]

# Incorporate scores back into AllH, AllP
AllH <- left_join(hitters,thitters,by=c('playerid'))
AllP <- left_join(pitchers,tpitchers,by=c('playerid'))
AllH <- rename(AllH,pDFL=zDFL)
AllP <- rename(AllP,pDFL=zDFL)
AllH$pDFL <- replace(AllH$pDFL,is.na(AllH$pDFL),0)
AllP$pDFL <- replace(AllP$pDFL,is.na(AllP$pDFL),0)

# Remove protected players
AllH <- anti_join(AllH,protected,by=c('Player'),copy=FALSE) %>% arrange(-pDFL)
AllP <- anti_join(AllP,protected,by=c('Player'),copy=FALSE) %>% arrange(-pDFL)

# Bucket pitchers by role
#AllP$Pos <- with(AllP,ifelse(pSV>pHLD,'CL',ifelse(pHLD>pW,'MR','SP')))
AllP$Pos <- with(AllP,ifelse(pSV>pHLD,'CL',ifelse(pHLD>pW,'MR','SP')))
AllH$Pos <- with(AllH,ifelse(Pos %in% c('LF','CF','RF'),'OF',Pos))


# Add in position eligibility based on 20 games
pedf <- read.xlsx("2014 Position Counts.xlsx",1,stringsAsFactors=F)
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

# Add back in orank
AllH <- inner_join(AllH,hrank,by=c('playerid','Pos'))
AllP <- inner_join(AllP,prank,by=c('playerid','Pos'))


# Grab razzball $values - http://razzball.com/projections-grey/
# Download html page and save it because work is blocking the URL
test <- 0
if (test==1) {
  rb <- readHTMLTable("http://razzball.com/projections-grey/",stringsAsFactors=F)
  rb2 <- rb[[2]]
  colnames(rb2)[[1]] <- 'rbrank'
  colnames(rb2)[[6]] <- 'rbprice'
  rb2 <- select(rb2,Player=Name,MLB=Team,rbprice)
  write.csv(rb2,'rbresults.csv')
}
rb2 <- read.csv('rbresults.csv',stringsAsFactors=FALSE) %>% select(-MLB,-rbprice,rbrank=X)
AllH <- left_join(AllH,rb2,by=c('Player'))
AllP <- left_join(AllP,rb2,by=c('Player'))

# Grab CBS rankings - http://dafl.baseball.cbssports.com/players/rankings/top300/by_pos/standard
cbs <- readHTMLTable("cbs300.html",stringsAsFactors=F,skip=1)
c2 <- cbs[[3]] %>% head(300)
c2$Player <- unlist(lapply(c2$V2,stripName))
c2 <- rename(c2,cbsrank=V1) %>% select(-V2)
AllH <- left_join(AllH,c2,by=c('Player'))
AllP <- left_join(AllP,c2,by=c('Player'))

# Grab CBS injuries report - append to players, tab of draft for next year
injuries <- readHTMLTable("http://www.cbssports.com/mlb/injuries",skip=1,stringsAsFactors=F)
injuries <- injuries[-c(1,2)]
inj <- rbind_all(injuries) %>% select(-Updated,-Pos) %>% filter(!is.na(Player))
inj$Player <- str_replace(inj$Player,"Â."," ")
names(inj) <- sub(" ", ".", names(inj))
AllH <- left_join(AllH,inj,by=c('Player'))
AllP <- left_join(AllP,inj,by=c('Player'))
OFY <- filter(inj,str_detect(Expected.Return,ignore.case('out for season')))
lc <- left_join(lc,inj,by=c('Player'))


# Process new pitches file
con  <- file("2015newPitch0303.txt", open = "r")

npitch <- data.frame()
while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
  # do stuff
  p <- str_match(oneLine,".+\\) (.+) - (.+)")
  nr <- data.frame(Player=p[,2],Pitch=p[,3])
  npitch <- rbind(npitch,nr)
} 

close(con)
AllP <- left_join(AllP,npitch,by=c('Player'))

if (predUpdate==TRUE) {
  write.csv(AllH,'AllHPrev.csv')
  write.csv(AllP,'AllPPrev.csv')
}

oAllH <- read.csv('AllHPrev.csv',stringsAsFactors=FALSE)
oAllP <- read.csv('AllPPrev.csv',stringsAsFactors=FALSE)
oAllH <- select(oAllH,playerid,oDFL=pDFL)
oAllP <- select(oAllP,playerid,oDFL=pDFL)
oAllH2 <- inner_join(oAllH,AllH,by=c('playerid')) %>% mutate(cDFL = pDFL - oDFL) %>% select(Player,cDFL,pDFL,Injury,Expected.Return)
oAllP2 <- inner_join(oAllP,AllP,by=c('playerid')) %>% mutate(cDFL = pDFL - oDFL) %>% select(Player,cDFL,pDFL,Injury,Expected.Return)
change <- rbind(oAllH2,oAllP2) %>% filter(abs(cDFL) > 1) %>% arrange(cDFL)

#Create separate tabs by position
#pc <- AllH %>% filter(Pos == 'C' | str_detect(posEl,'C'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>%
#  select(Player,MLB,posEl,Age,DFL=pDFL,SGP=pSGP,orank,rbrank,cbsrank,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)
##pc <- mutate(pc,RPV = (SGP - aRPV(pc,nrow(filter(pc,DFL>0))))/aRPV(pc,nrow(filter(pc,DFL>0))))
pc <- AllH %>% filter(Pos == 'C' | str_detect(posEl,'C'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% rename(DFL=pDFL,SGP=pSGP)
pc <- mutate(pc,RPV = (SGP - aRPV(pc,nrow(filter(pc,DFL>0))))/aRPV(pc,nrow(filter(pc,DFL>0))))
pc <- select(pc,Player,MLB,posEl,Age,DFL,RPV,SGP,orank,rbrank,cbsrank,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)

p1b <- AllH %>% filter(Pos == '1B' | str_detect(posEl,'1B'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% rename(DFL=pDFL,SGP=pSGP)
p1b <- mutate(p1b,RPV = (SGP - aRPV(p1b,nrow(filter(p1b,DFL>0))))/aRPV(p1b,nrow(filter(p1b,DFL>0))))
p1b <- select(p1b,Player,MLB,posEl,Age,DFL,RPV,SGP,orank,rbrank,cbsrank,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)

p2b <- AllH %>% filter(Pos == '2B' | str_detect(posEl,'2B'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% rename(DFL=pDFL,SGP=pSGP)
p2b <- mutate(p2b,RPV = (SGP - aRPV(p2b,nrow(filter(p2b,DFL>0))))/aRPV(p2b,nrow(filter(p2b,DFL>0))))
p2b <- select(p2b,Player,MLB,posEl,Age,DFL,RPV,SGP,orank,rbrank,cbsrank,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)

pss <- AllH %>% filter(Pos == 'SS' | str_detect(posEl,'SS'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% rename(DFL=pDFL,SGP=pSGP)
pss <- mutate(pss,RPV = (SGP - aRPV(pss,nrow(filter(pss,DFL>0))))/aRPV(pss,nrow(filter(pss,DFL>0))))
pss <- select(pss,Player,MLB,posEl,Age,DFL,RPV,SGP,orank,rbrank,cbsrank,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)

p3b <- AllH %>% filter(Pos == '3B' | str_detect(posEl,'3B'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% rename(DFL=pDFL,SGP=pSGP)
p3b <- mutate(p3b,RPV = (SGP - aRPV(p3b,nrow(filter(p3b,DFL>0))))/aRPV(p3b,nrow(filter(p3b,DFL>0))))
p3b <- select(p3b,Player,MLB,posEl,Age,DFL,RPV,SGP,orank,rbrank,cbsrank,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)

pdh <- AllH %>% filter(Pos == 'DH',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% rename(DFL=pDFL,SGP=pSGP)
pdh <- mutate(pdh,RPV = (SGP - aRPV(pdh,nrow(filter(pdh,DFL>0))))/aRPV(pdh,nrow(filter(pdh,DFL>0))))
pdh <- select(pdh,Player,MLB,posEl,Age,DFL,RPV,SGP,orank,rbrank,cbsrank,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)

pof <- AllH %>% filter(Pos == 'OF' | str_detect(posEl,'OF'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% 
  rename(DFL=pDFL,SGP=pSGP)  %>% head(200)
pof <- mutate(pof,RPV = (SGP - aRPV(pof,nrow(filter(pof,DFL>0))))/aRPV(pof,nrow(filter(pof,DFL>0))))
pof <- select(pof,Player,MLB,posEl,Age,DFL,RPV,SGP,orank,rbrank,cbsrank,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)

pna <- AllH %>% filter(is.na(Pos) | Pos=='',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>%
  select(Player,MLB,Age,DFL=pDFL,SGP=pSGP,rbrank,cbsrank,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)



psp <- AllP %>% filter(Pos=='SP',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% rename(DFL=pDFL,SGP=pSGP) %>% head(200)
psp <- mutate(psp,RPV = (SGP - aRPV(psp,nrow(filter(psp,DFL>0))))/aRPV(psp,nrow(filter(psp,DFL>0))))
psp <- select(psp,Player,MLB,Age,DFL,RPV,SGP,orank,rbrank,cbsrank,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD,Injury,Expected.Return,Pitch)

pcl <- AllP %>% filter(Pos=='CL',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% rename(DFL=pDFL,SGP=pSGP)
pcl <- mutate(pcl,RPV = (SGP - aRPV(pcl,nrow(filter(pcl,DFL>0))))/aRPV(pcl,nrow(filter(pcl,DFL>0))))
pcl <- select(pcl,Player,MLB,Age,DFL,RPV,SGP,orank,rbrank,cbsrank,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD,Injury,Expected.Return,Pitch)

pmr <- AllP %>% filter(Pos=='MR',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% rename(DFL=pDFL,SGP=pSGP) %>% head(200)
pmr <- mutate(pmr,RPV = (SGP - aRPV(pmr,nrow(filter(pmr,DFL>0))))/aRPV(pmr,nrow(filter(pmr,DFL>0))))
pmr <- select(pmr,Player,MLB,Age,DFL,RPV,SGP,orank,rbrank,cbsrank,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD,Injury,Expected.Return,Pitch)

# Create prospect reports!!
# http://www.scoutingbook.com/prospects/matrix
prospects <- readHTMLTable("http://www.scoutingbook.com/prospects/matrix",stringsAsFactors=F)
prospects <- prospects[[1]]
prospects <- prospects %>% select(Player,Team,Position,SB)
prospects <- filter(prospects,SB!="")
prospects <- mutate(prospects,rookRank=as.numeric(SB)) %>% select(-SB) %>% rename(MLB=Team)
# Strip out weird character
prospects$Player <- str_replace(prospects$Player,"Â."," ")
prs <- select(prospects,Player,rookRank)
proh <- inner_join(AllH,prs,by=c('Player'))
prop <- inner_join(AllP,prs,by=c('Player'))

hp <- proh %>% filter(!is.na(rookRank)) %>% arrange(-pDFL,rookRank) %>%
  select(Player,MLB,rookRank,Age,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)
pp <- prop %>% filter(!is.na(rookRank)) %>% arrange(-pDFL,rookRank) %>%
  select(Player,MLB,rookRank,Age,DFL=pDFL,SGP=pSGP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD,Injury,Expected.Return)

# Create percent against goals worksheet
# Step 1 - Create targets
targets <- data.frame()
targets <- rbind(targets,c(2014,pgoals('fs2014.csv')))
colnames(targets) <- c('year','HR','RBI','R','SB','AVG','W','K','SV','HLD','ERA')
targets <- select(targets,-AVG,-ERA,-year)
targets <- melt(targets)
targets <- rename(targets,statistic = variable, goal = value)
# Step 2 - summarize protected projections
#lcht <- rhitters %>% filter(Team == 'Liquor Crickets') %>% 
#  summarize(HR = sum(pHR),RBI=sum(pRBI),R=sum(pR),SB=sum(pSB))
#lcht <- melt(lcht) %>% rename(statistic = variable, collected = value)
#hg <- inner_join(lcht,targets) %>% mutate(needed=goal-collected,pc = (collected/goal)*100)

#lcpt <- rpitchers %>% filter(Team == 'Liquor Crickets') %>% 
#  summarize(W = sum(pW),HLD=sum(pHLD),K=sum(pSO),SV=sum(pSV))
#lcpt <- melt(lcpt) %>% rename(statistic = variable, collected = value)
#pg <- inner_join(lcpt,targets) %>% mutate(needed=goal-collected,pc = (collected/goal)*100)

#gmet <- rbind(hg,pg) %>% arrange(pc)

gmet <- calcGoals(rpitchers,rhitters,targets,'Liquor Crickets')
ntmet <- calcGoals(rpitchers,rhitters,targets,'Neon Tetras')
csmet <- calcGoals(rpitchers,rhitters,targets,'Crap Shooters')
bjmet <- calcGoals(rpitchers,rhitters,targets,'But Justice')

protected$Position <- with(protected,ifelse(Pos %in% c('LF','CF','RF'),'OF',Pos))
ppp <- group_by(protected,Position) %>% summarize(Count=length(Position))

# Who will be bidding against me?
at <- as.data.frame(unique(protected$Team))
names(at) <- c('Team')
ww1b <- filter(rhitters,Pos %in% c('LF','CF','RF')) %>% group_by(Team) %>% 
  summarize(Num=length(Team),tSal=sum(Salary),tDFL=sum(pDFL)) %>%
  as.data.frame()
ww1b <- left_join(at,ww1b)
ww1b[is.na(ww1b)] <- 0
ww1b <- arrange(ww1b,tDFL)

# Create spreadsheet
draft <- createWorkbook()
csRatioColumn <- CellStyle(draft, dataFormat=DataFormat("#0.00")) 
csPctColumn <- CellStyle(draft, dataFormat=DataFormat("#0.00%")) 
csMoneyColumn <- CellStyle(draft, dataFormat=DataFormat("$#,##0.00;-$#,##0.00")) 
# st <- list('3'=csMoneyColumn,'4'=csMoneyColumn,'5'=csMoneyColumn,'6'=csMoneyColumn,'7'=csMoneyColumn,
#            '8'=csMoneyColumn,'9'=csRatioColumn,'10'=csRatioColumn)
# sht <- createSheet(wb=draft,sheetName='Test')
# addDataFrame(x=pstandings,sheet=sht,colStyle=st)
# setColumnWidth(sht, colIndex=2, colWidth= 1.4*max(length(pstandings[[2]])))
# saveWorkbook(draft,"test.xlsx")


tabs <- list()
st <- list('3'=csMoneyColumn,'4'=csMoneyColumn,'5'=csMoneyColumn,'6'=csMoneyColumn,'7'=csMoneyColumn,
           '8'=csMoneyColumn,'9'=csRatioColumn,'10'=csRatioColumn)
tabs[[length(tabs)+1]] <- list('Early Standings',pstandings,st,c(2))
st <- list('4'=csMoneyColumn)
tabs[[length(tabs)+1]] <- list('Crickets',lc,st,c(2,7))
st <- list('5'=csPctColumn)
tabs[[length(tabs)+1]] <- list('Goals',gmet,st,c())
tabs[[length(tabs)+1]] <- list('Percent Protected',ppp,st,c())
st <- list('5'=csMoneyColumn,'6'=csRatioColumn,'7'=csRatioColumn)
tabs[[length(tabs)+1]] <- list('C',pc,st,c(2,18))
tabs[[length(tabs)+1]] <- list('1B',p1b,st,c(2,18))
tabs[[length(tabs)+1]] <- list('2B',p2b,st,c(2,18))
tabs[[length(tabs)+1]] <- list('SS',pss,st,c(2,18))
tabs[[length(tabs)+1]] <- list('3B',p3b,st,c(2,18))
tabs[[length(tabs)+1]] <- list('OF',pof,st,c(2,18))
tabs[[length(tabs)+1]] <- list('DH',pdh,st,c(2))
#tabs[[length(tabs)+1]] <- list('Other',pna,st,c(2))
st <- list('4'=csMoneyColumn,'5'=csRatioColumn,'6'=csRatioColumn)
tabs[[length(tabs)+1]] <- list('SP',psp,st,c(2,17,18))
tabs[[length(tabs)+1]] <- list('MR',pmr,st,c(2,17,18))
tabs[[length(tabs)+1]] <- list('CL',pcl,st,c(2,17,18))
st <- list('5'=csMoneyColumn,'6'=csRatioColumn)
tabs[[length(tabs)+1]] <- list('HitProspect',hp,st,c(2,14))
tabs[[length(tabs)+1]] <- list('PitProspect',pp,st,c(2,14))
tabs[[length(tabs)+1]] <- list('OutForYear',OFY,st,c(2,3,4))
st <- list('5'=csPctColumn)
tabs[[length(tabs)+1]] <- list('NTGoals',ntmet,st,c())
tabs[[length(tabs)+1]] <- list('CSGoals',csmet,st,c())
tabs[[length(tabs)+1]] <- list('BJGoals',bjmet,st,c())
st <- list('2'=csMoneyColumn,'3'=csMoneyColumn)
tabs[[length(tabs)+1]] <- list('Recent Changes',change,st,c(2))

lapply(tabs,addSheet,draft)
saveWorkbook(draft,"draftGuide.xlsx")

