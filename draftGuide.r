# For 2015
# Refactor - Consolidate pDFL calculations
# Refactor - constants
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

# Step 3 - use last year's totals plus fangraphs projected role
# Use last year's data, if now a closer, set to 0, if true setup - make sure to up number
# http://www.fangraphs.com/fantasy/bullpen-report-september-24-2014/
c <- readHTMLTable("http://www.fangraphs.com/fantasy/bullpen-report-september-24-2014/",stringsAsFactors=F)
f <- lapply(c,function(x) {is.data.frame(x) && ncol(x) == 5})
c2 <- c[unlist(f)]
crep <- c2[[1]]
colnames(crep) <- c(' ','Closer','First','Second','DL/Minors')
crep <- crep[-1,]
#crep <- readHTMLTable(bp, header=T, which=15,stringsAsFactors=F)
t <- data.frame(crep$Closer,10)
t2 <- data.frame(crep$First,5)
t3 <- data.frame(crep$Second,2)
colnames(t) <- c('Player','pRole')
colnames(t2) <- c('Player','pRole')
colnames(t3) <- c('Player','pRole')
crep <- rbind_list(t,t2,t3)
crep$Player <- iconv(crep$Player,'UTF-8','ASCII')
pitchers <- left_join(pitchers,crep,c('Player'))
pitchers$pRole <- ifelse(is.na(pitchers$pRole),0,pitchers$pRole)
pitchers$pHLD <- with(pitchers,ifelse(pRole==10,0,ifelse(pRole==5 & lyHLD < 25,25,lyHLD)))
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

# Create prospect reports!!
# http://www.scoutingbook.com/prospects/matrix
prospects <- readHTMLTable("http://www.scoutingbook.com/prospects/matrix",stringsAsFactors=F)
prospects <- prospects[[1]]
prospects <- prospects %>% select(Player,Team,Position,SB)
prospects <- filter(prospects,SB!="")
prospects <- mutate(prospects,rookRank=as.numeric(SB)) %>% select(-SB) %>% rename(MLB=Team)
# Strip out weird character
prospects$Player <- str_replace(prospects$Player,"Â."," ")
# merge with master, split hitters,pitchers, merge with projections, spit out report
m2 <- select(master,-Pos)
p2 <- inner_join(prospects, m2,by=c('Player','MLB'))
p3 <- anti_join(prospects, m2,by=c('Player','MLB'))
# Merge rest with only name
p4 <- inner_join(p3, m2,by=c('Player'))
p4 <- select(p4,-MLB.x) %>% rename(MLB=MLB.y) 
prospects <- rbind(p2,p4) %>% select(playerid,rookRank)
# Add column into AllH
AllH <- left_join(AllH,prospects,by=c('playerid')) 
AllP <- left_join(AllP,prospects,by=c('playerid')) 
hp <- AllH %>% filter(!is.na(rookRank)) %>% arrange(-pDFL,rookRank) %>% 
  select(Player,MLB,rookRank,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG)
pp <- AllP %>% filter(!is.na(rookRank)) %>% arrange(-pDFL,rookRank) %>% 
  select(Player,MLB,rookRank,DFL=pDFL,SGP=pSGP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD)



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
tabs[[length(tabs)+1]] <- list('HitProspect',hp)
tabs[[length(tabs)+1]] <- list('PitProspect',pp)


lapply(tabs,addSheet,draft)
saveWorkbook(draft,"draftGuide.xlsx")




