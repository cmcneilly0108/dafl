#BUG - Cricket protection list - both of my pitchers
#BUG - only 2 catchers


library("xlsx")
library("stringr")
library("dplyr")
library("XML")
library("ggplot2")
library("reshape2")

source("./daflFunctions.r")

system("./pullSteamer.sh")

#Load protection list
protected <- read.csv("2014fakeprotected.csv",stringsAsFactors=FALSE)

#merge with Master file - BUG
m2 <- select(master,-Pos,-Player) %>% rename(Player=cbs_name)
gname <- inner_join(protected, m2,by=c('Player')) %>% select(Team,Player,playerid,Pos,Contract,Salary)
grest <- anti_join(protected, m2,by=c('Player'))
#gname$playerid <- ifelse(is.na(gname$playerid),gname$Player,gname$playerid)

rooks <- read.csv('2015RookieIDs.csv',stringsAsFactors=FALSE) %>% select(-X)
g2name <- inner_join(grest,rooks,by=c('Player')) %>% select(Team,Player,playerid,Pos,Contract,Salary)
protected <- rbind(gname,g2name)

#split into P,H tables
rHitters <- filter(protected,Pos != 'SP' & Pos != 'RP') 
rPitchers <- filter(protected,Pos == 'SP' | Pos == 'RP')

#Load steamer projection data
hitters <- read.fg("steamerH2015.csv")
hitters$pSGP <- hitSGP(hitters)

pitchers <- read.fg("steamerP2015.csv")
pitchers <- predictHolds(pitchers)
pitchers$pSGP <- pitSGP(pitchers)

#Generate pDFL for best players - no protections!
nlist <- preDollars(hitters,pitchers)
thitters <- nlist[[1]]
tpitchers <- nlist[[2]]

# Incorporate scores back into AllH, AllP
AllH <- left_join(hitters,thitters,by=c('playerid'))
AllP <- left_join(pitchers,tpitchers,by=c('playerid'))
AllH$pDFL <- replace(AllH$pDFL,is.na(AllH$pDFL),0)
AllP$pDFL <- replace(AllP$pDFL,is.na(AllP$pDFL),0)
#merge with steamer
rhitters <- left_join(rHitters,AllH,by=c('playerid'),copy=FALSE)
rpitchers <- left_join(rPitchers,AllP,by=c('playerid'),copy=FALSE)
rhitters <- select(rhitters,-Pos.x,-Player.x) %>% rename(Pos=Pos.y,Player=Player.y)
rpitchers <- select(rpitchers,-Pos.x,-Player.x) %>% rename(Pos=Pos.y,Player=Player.y)
rhitters$Value <- rhitters$pDFL - rhitters$Salary
rpitchers$Value <- rpitchers$pDFL - rpitchers$Salary

rhitters <- select(rhitters,Team,Player,Salary,pDFL)
rpitchers <- select(rpitchers,Team,Player,Salary,pDFL)

protClean <- rbind(rhitters,rpitchers)
protClean$pDFL <- replace(protClean$pDFL,is.na(protClean$pDFL),0)


# Create Pre-Draft Standings
pstandings <- protClean %>% group_by(Team) %>%
  summarize(NumProtected = length(Team),
            Spent = sum(Salary),
            TotalValue = sum(pDFL),
            MoneyEarned = TotalValue - Spent,
            VPPlayer = TotalValue/NumProtected,
            DPRemaining = (260-sum(Salary))/(25-NumProtected),
            FullValue = TotalValue + (260-sum(Salary)),
            DollarValue = TotalValue/Spent) %>%
  arrange(-MoneyEarned)
#This works, but only because I manually created the fake file.  Change up to merge with current projections.

lc <- filter(protClean,Team == 'Liquor Crickets') %>% select(-Team) %>% arrange(-pDFL)


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
pna <- AllH %>% filter(is.na(Pos) | Pos=='',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>%
  select(Player,MLB,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)


psp <- AllP %>% filter(Pos=='SP',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>%
  select(Player,MLB,DFL=pDFL,SGP=pSGP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD)
pcl <- AllP %>% filter(Pos=='CL',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>%
  select(Player,MLB,DFL=pDFL,SGP=pSGP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD)
pmr <- AllP %>% filter(Pos=='MR',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>%
  select(Player,MLB,DFL=pDFL,SGP=pSGP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD)

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
AllH <- inner_join(AllH,prs,by=c('Player'))
AllP <- inner_join(AllP,prs,by=c('Player'))

hp <- AllH %>% filter(!is.na(rookRank)) %>% arrange(-pDFL,rookRank) %>%
  select(Player,MLB,rookRank,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
pp <- AllP %>% filter(!is.na(rookRank)) %>% arrange(-pDFL,rookRank) %>%
  select(Player,MLB,rookRank,DFL=pDFL,SGP=pSGP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD)



# Create spreadsheet
draft <- createWorkbook()
tabs <- list()
tabs[[length(tabs)+1]] <- list('Early Standings',pstandings)
tabs[[length(tabs)+1]] <- list('Crickets',lc)
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
tabs[[length(tabs)+1]] <- list('HitProspect',hp)
tabs[[length(tabs)+1]] <- list('PitProspect',pp)


lapply(tabs,addSheet,draft)
saveWorkbook(draft,"draftGuide.xlsx")
