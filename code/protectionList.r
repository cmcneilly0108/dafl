# Generate new salary file
#    updatePreSeasonSalaries.Rmd
# Files to update each year
#    - Position Eligibility - line 110 - from cbs dafl
#    - cannot do this until CBS resets the league - need following year elig
# http://dafl.baseball.cbssports.com/stats/stats-main/all:C:1B:2B:3B:SS:OF:U/2021:p/PosElig/
#    - update loadPast with latest year
# fs2019.csv
#    - update predictHolds with latest bullpen report URL
# edit pullSteamer and pullATC shell files


library("openxlsx")
library("stringr")
library("reshape2")
library("lubridate")
library("zoo")
library("xml2")
library("dplyr")
library("rvest")
library("jsonlite")

source("./daflFunctions.r")

year <- "2024"
lastyear <- "2023"

fd <- file.info(str_c("../steamerH",year,".csv"))$mtime
cd <- Sys.time()
dt <- difftime(cd, fd, units = "hours")
if (dt > 20) {
  #system("./pullSteamer.sh")
  system("bash ../scripts/pullSteamer.sh")
  #system("bash ../scripts/pullMaster.sh")
  system("bash ../scripts/pullATC.sh")
}

#Load steamer data
#hitters <- read.fg(str_c("../steamerH",year,".csv"))
hitters <- read.fg(str_c("../atcH",year,".json"))
#hitters <- read.fg("atcH2020.csv")
hitters$pSGP <- hitSGP(hitters)

#pitchers <- read.fg(str_c("../steamerP",year,".csv"))
pitchers <- read.fg(str_c("../atcP",year,".json"))
#pitchers <- read.fg("atcP2020.csv")
#pitchers <- predictHolds(pitchers)
pitchers$pSGP <- pitSGP(pitchers)


#official file
rosters <- read.csv(str_c("../",year,"Rosters1.csv"), encoding="UTF-8")


#split into P,H tables
rHitters <- filter(rosters,Pos != 'P' & Pos != 'RP')
rPitchers <- filter(rosters,Pos == 'P' | Pos == 'RP')

nteams <- 14
tdollars <- nteams * 260
pdollars <- round(tdollars*0.4)
hdollars <- tdollars - pdollars
nhitters <- 13
npitchers <- 12
chitters <- (nhitters * nteams)
cpitchers <- (npitchers * nteams)


#Generate SGP dollars
#nlist <- preDollars(hitters,pitchers)
#hitters <- hitters[is.na(hitters)] <- 0
nlist <- preLPP(hitters,pitchers)
thitters <- nlist[[1]]
tpitchers <- nlist[[2]]

# Incorporate scores back into AllH, AllP
AllH <- left_join(hitters,thitters,by=c('playerid'))
AllP <- left_join(pitchers,tpitchers,by=c('playerid'))
AllH <- dplyr::rename(AllH,pDFL=zDFL)
AllP <- dplyr::rename(AllP,pDFL=zDFL)
AllH$pDFL <- replace(AllH$pDFL,is.na(AllH$pDFL),0)
AllP$pDFL <- replace(AllP$pDFL,is.na(AllP$pDFL),0)

# Standardize positions, calculate rank
AllH$Pos <- with(AllH,ifelse(Pos %in% c('LF','CF','RF'),'OF',Pos))
AllP$Pos <- with(AllP,ifelse(pSV>pHLD,'CL',ifelse(pHLD>pW,'MR','SP')))
AllH <- AllH %>% group_by(Pos) %>% dplyr::mutate(orank = rank(-pDFL)) %>% as.data.frame()
AllP <- AllP %>% group_by(Pos) %>% mutate(orank = rank(-pDFL)) %>% as.data.frame()

#th <- filter(AllH,Pos=="1B")
#t2 <- th %>% mutate(orank = rank(-pDFL,-pSGP))

#merge with steamer
rhitters <- inner_join(rHitters,AllH,by=c('playerid'),copy=FALSE)
rpitchers <- inner_join(rPitchers,AllP,by=c('playerid'),copy=FALSE)
rhitters <- select(rhitters,-Pos.x,-Player.x,-MLB.x) %>% dplyr::rename(Pos=Pos.y,Player=Player.y,MLB=MLB.y)
rpitchers <- select(rpitchers,-Pos.x,-Player.x,-MLB.x) %>% dplyr::rename(Pos=Pos.y,Player=Player.y,MLB=MLB.y)
rhitters$Value <- rhitters$pDFL - rhitters$Salary
rpitchers$Value <- rpitchers$pDFL - rpitchers$Salary

# Bucket pitchers by role
rpitchers$Pos <- with(rpitchers,ifelse(pSV>pHLD,'CL',ifelse(pHLD>pW,'MR','SP')))


# Add in position eligibility based on 20 games
pedf <- read.cbs(str_c("../",year,"PosElig.csv"))
pedf <- dplyr::rename(pedf,posEl=Eligible) %>% select(playerid,posEl)
AllH <- left_join(AllH,pedf,by=c('playerid'))

# Injuries data
inj <- getInjuries()

#fake out until injuries get updated
inj <- data_frame(Player="temp",Injury="",Expected.Return="")

AllH <- left_join(AllH,inj,by=c('Player'))
AllP <- left_join(AllP,inj,by=c('Player'))
rhitters <- left_join(rhitters,inj,by=c('Player'))
rpitchers <- left_join(rpitchers,inj,by=c('Player'))




rpreds <- rbind(select(rhitters,playerid,Team,Player,Pos,Age,Contract,Salary,pDFL,Value,s1=pHR,s2=pRBI,s3=pR,s4=pSB,Injury,Expected.Return),
            select(rpitchers,playerid,Team,Player,Pos,Age,Contract,Salary,pDFL,Value,s1=pW,s2=pSO,s3=pHLD,s4=pSV,Injury,Expected.Return))

#rpreds <- dplyr::rename(rpreds,Salary=Salary,Contract=Contract)

prosters <- rpreds %>% group_by(Team) %>% filter(rank(-Value) < 13,Value > 1) %>%
  arrange(Team,-Value)

# New approach
# Pass to preLPP with protection list
# Get new pDFLs, rerank, and update protection list
# iterate as long as overall value continues to increase
cvalue <- 0
ct <- 0
prosters2 <- prosters
nvalue <- sum(prosters2$Value)
print(nvalue)

#see if I can track only evaluated once
pjoin <- prosters2 %>% ungroup() %>% mutate(rdOne = TRUE) %>% select(playerid,rdOne)
rhitters <- left_join(rhitters,pjoin,by=c('playerid'))
rpitchers <- left_join(rpitchers,pjoin,by=c('playerid'))



while ((nvalue > (cvalue+1)) & ct < 20) {
  ct <- ct + 1
  cvalue <- nvalue
  nlist <- preLPP(hitters,pitchers,prosters2)
  ihitters <- nlist[[1]]
  ipitchers <- nlist[[2]]
  # update rhitters, rpitchers with new pDFL and Value scores
  rhitters <- left_join(rhitters,ihitters,by=c('playerid'))
  rhitters <- mutate(rhitters,pDFL = ifelse(is.na(zDFL),pDFL,zDFL),
                     rdOne = ifelse(is.na(zDFL),rdOne,FALSE)) %>% select(-zDFL)
  rpitchers <- left_join(rpitchers,ipitchers,by=c('playerid'))
  rpitchers <- mutate(rpitchers,pDFL = ifelse(is.na(zDFL),pDFL,zDFL),
                      rdOne = ifelse(is.na(zDFL),rdOne,FALSE)) %>% select(-zDFL)
  rpreds <- rbind(select(rhitters,Team,playerid,Player,Pos,Age,Contract,Salary,pDFL,Value,orank,rdOne,s1=pHR,s2=pRBI,s3=pR,s4=pSB,Injury,Expected.Return),
                  select(rpitchers,Team,playerid,Player,Pos,Age,Contract,Salary,pDFL,Value,orank,rdOne,s1=pW,s2=pSO,s3=pHLD,s4=pSV,Injury,Expected.Return))
  rpreds <- mutate(rpreds,Value = pDFL - Salary)
  prosters2 <- rpreds %>% group_by(Team) %>% filter(rank(-Value) < 13,Value > 1) %>%
    arrange(Team,-Value)
  if (nrow(filter(prosters2,rdOne==TRUE))) {prosters2 <- arrange(prosters2,rdOne) %>% head(-10)} 
  nvalue <- sum(prosters2$Value)
  print(nvalue)
}

prosters <- select(prosters2,playerid,Player,Pos,Team,Salary,Contract,orank)
write.csv(prosters,str_c("../",year,"fakeprotected.csv"))



lc2 <- filter(rpreds,Team == 'Liquor Crickets') %>% arrange(-Value) %>% 
  select(Player,Pos,Team,Salary,Contract,playerid,orank)
#prosters <- select(prosters,-playerid)
rpreds <- select(rpreds,-playerid)

#lc <- filter(rpreds,Team == 'Liquor Crickets') %>% arrange(-Value)
lcp <- filter(rpreds,Team == 'Liquor Crickets',Value > 1) %>% arrange(-Value) %>%
  filter(rank(-Value) < 13)
lc <- filter(rpreds,Team == 'Liquor Crickets') %>% arrange(-Value)


# Logic - filter to players that either have a valueRate > 1.3 or totalValue > 5, sort by Value descending
totals <- rpreds %>% group_by(Team) %>% filter(rank(-Value) < 13,Value > 1) %>%
  dplyr::summarize(NumProtected = length(Team),
            Spent = sum(Salary),
            TotalValue = sum(pDFL),
            MoneyEarned = TotalValue - Spent,
            VPPlayer = TotalValue/NumProtected,
            PostDraftEst = TotalValue + 1.05 * (260-sum(Salary)),
            ValueRatio = TotalValue/Spent) %>%
  arrange(-PostDraftEst)
totals$zScore <- as.numeric(scale(totals$PostDraftEst))

#Create separate tabs by position
pc <- AllH %>% filter(Pos == 'C' | str_detect(posEl,'C'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>%
  select(Player,MLB,posEl,Age,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)
pc <- mutate(pc,RPV = (SGP - aRPV(pc))/aRPV(pc))
p1b <- AllH %>% filter(Pos == '1B' | str_detect(posEl,'1B'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>%
  select(Player,MLB,posEl,Age,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)
p1b <- mutate(p1b,RPV = (SGP - aRPV(p1b,20))/aRPV(p1b,20))
p2b <- AllH %>% filter(Pos == '2B' | str_detect(posEl,'2B'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>%
  select(Player,MLB,posEl,Age,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)
p2b <- mutate(p2b,RPV = (SGP - aRPV(p2b))/aRPV(p2b))
pss <- AllH %>% filter(Pos == 'SS' | str_detect(posEl,'SS'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>%
  select(Player,MLB,posEl,Age,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)
pss <- mutate(pss,RPV = (SGP - aRPV(pss))/aRPV(pss))
p3b <- AllH %>% filter(Pos == '3B' | str_detect(posEl,'3B'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>%
  select(Player,MLB,posEl,Age,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)
p3b <- mutate(p3b,RPV = (SGP - aRPV(p3b))/aRPV(p3b))
pdh <- AllH %>% filter(Pos == 'DH' | str_detect(posEl,'DH'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>%
  select(Player,MLB,posEl,Age,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)
pdh <- mutate(pdh,RPV = (SGP - aRPV(pdh))/aRPV(pdh))
pof <- AllH %>% filter(Pos == 'OF' | str_detect(posEl,'OF'),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>%
  select(Player,MLB,posEl,Age,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)
pof <- mutate(pof,RPV = (SGP - aRPV(pof,60))/aRPV(pof,60))
pna <- AllH %>% filter(is.na(Pos),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>%
  select(Player,MLB,Age,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)


psp <- AllP %>% filter(Pos=='SP',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>%
  select(Player,MLB,Age,DFL=pDFL,SGP=pSGP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD,Injury,Expected.Return)
psp <- mutate(psp,RPV = (SGP - aRPV(psp,120))/aRPV(psp,120))
pcl <- AllP %>% filter(Pos=='CL',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>%
  select(Player,MLB,Age,DFL=pDFL,SGP=pSGP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD,Injury,Expected.Return)
pcl <- mutate(pcl,RPV = (SGP - aRPV(pcl))/aRPV(pcl))
pmr <- AllP %>% filter(Pos=='MR',pSGP > 0) %>% arrange(-pDFL,-pSGP) %>%
  select(Player,MLB,Age,DFL=pDFL,SGP=pSGP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD,Injury,Expected.Return)
pmr <- mutate(pmr,RPV = (SGP - aRPV(pmr))/aRPV(pmr))




bh <- as.data.frame(rpreds) %>% filter(Value>20,Pos!='SP') %>% arrange(-Value)
bp <- as.data.frame(rpreds) %>% filter((Pos=='SP' | Pos=='CL'),Value>7) %>% arrange(-Value)
bout <- as.data.frame(rpreds) %>% filter(Pos=='OF',s3>90,Salary<50) %>% arrange(-Value)

# Best, but not bargains
t1 <- rpreds %>% filter(orank < 6,Value > -5, Value < 5)
t2 <- rpreds %>% filter(Pos %in% c('OF','SP'),orank < 40,Value > -5, Value < 5)
fairpriced <- bind_rows(t1,t2) %>% arrange(orank)



# Create Trends tab
targets <- data.frame()
targets <- rbind(targets,c(2019,pgoals('../data/fs2019.csv')))
targets <- rbind(targets,c(2018,pgoals('../data/fs2018.csv')))
targets <- rbind(targets,c(2017,pgoals('../data/fs2017.csv')))
targets <- rbind(targets,c(2016,pgoals('../data/fs2016.csv')))
targets <- rbind(targets,c(2015,pgoals('../data/fs2015.csv')))
targets <- rbind(targets,c(2014,pgoals('../data/fs2014.csv')))
targets <- rbind(targets,c(2013,pgoals('../data/fs2013.csv')))
targets <- rbind(targets,c(2012,pgoals('../data/fs2012.csv')))
colnames(targets) <- c('year','HR','RBI','R','SB','AVG','W','K','SV','HLD','ERA')


# Create spreadsheet
protect <- createWorkbook()
headerStyle <- createStyle(halign = "CENTER", textDecoration = "Bold")
csRatioColumn <- createStyle(numFmt = "##0.00")
csMoneyColumn <- createStyle(numFmt = "CURRENCY")
# csBAColumn <- CellStyle(protect, dataFormat=DataFormat("##0.000"))
# csPctColumn <- CellStyle(protect, dataFormat=DataFormat("#0.00%"))


# tabs <- list()
# st <- list('3'=csMoneyColumn,'4'=csMoneyColumn,'5'=csMoneyColumn,'6'=csMoneyColumn,'7'=csMoneyColumn,'8'=csRatioColumn,'9'=csRatioColumn,'10'=csRatioColumn)
# tabs[[length(tabs)+1]] <- list('LeagueSummary',totals,st,c(2))
addWorksheet(protect,'LeagueSummary')
writeData(protect,'LeagueSummary',totals,headerStyle = headerStyle)
addStyle(protect, 'LeagueSummary',style = csMoneyColumn,rows = 2:20, cols = 4:7,gridExpand = TRUE)
addStyle(protect, 'LeagueSummary',style = csRatioColumn,rows = 2:20, cols = 8:9,gridExpand = TRUE)
setColWidths(protect, 'LeagueSummary', cols = 1:9, widths = "auto")


#addStyle(protect, 'LeagueSummary', headerStyle, rows = 1)
# st <- list('6'=csMoneyColumn,'7'=csMoneyColumn,'8'=csMoneyColumn)
# tabs[[length(tabs)+1]] <- list('AllCrickets',lc,st,c(2,3,14,15))
addWorksheet(protect,'AllCrickets')
writeData(protect,'AllCrickets',lc,headerStyle = headerStyle)
setColWidths(protect, 'AllCrickets', cols = 1:15, widths = "auto")
addStyle(protect, 'AllCrickets',style = csMoneyColumn,rows = 2:200, cols = 7:8,gridExpand = TRUE)

# tabs[[length(tabs)+1]] <- list('ProjectedCrickets',lcp,st,c(2,3,14,15))
addWorksheet(protect,'ProjectedCrickets')
writeData(protect,'ProjectedCrickets',lcp,headerStyle = headerStyle)
setColWidths(protect, 'ProjectedCrickets', cols = 1:15, widths = "auto")
addStyle(protect, 'ProjectedCrickets',style = csMoneyColumn,rows = 2:200, cols = 7:8,gridExpand = TRUE)

# tabs[[length(tabs)+1]] <- list('AllRosters',as.data.frame(prosters),st,c(2,3,14,15))
addWorksheet(protect,'AllRosters')
writeData(protect,'AllRosters',as.data.frame(prosters),headerStyle = headerStyle)
setColWidths(protect, 'AllRosters', cols = 1:15, widths = "auto")
addStyle(protect, 'AllRosters',style = csMoneyColumn,rows = 2:200, cols = 7:8,gridExpand = TRUE)

# st <- list('5'=csMoneyColumn,'6'=csMoneyColumn,'7'=csRatioColumn)
# tabs[[length(tabs)+1]] <- list('C',pc,st,c(2,3,14,15))
addWorksheet(protect,'C')
writeData(protect,'C',pc,headerStyle = headerStyle)
setColWidths(protect, 'C', cols = 1:15, widths = "auto")
addStyle(protect, 'C',style = csMoneyColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
addStyle(protect, 'C',style = csRatioColumn,rows = 2:200, cols = c(6,14),gridExpand = TRUE)

# tabs[[length(tabs)+1]] <- list('1B',p1b,st,c(2,3,14,15))
addWorksheet(protect,'1B')
writeData(protect,'1B',p1b,headerStyle = headerStyle)
setColWidths(protect, '1B', cols = 1:15, widths = "auto")
addStyle(protect, '1B',style = csMoneyColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
addStyle(protect, '1B',style = csRatioColumn,rows = 2:200, cols = c(6,14),gridExpand = TRUE)

# tabs[[length(tabs)+1]] <- list('2B',p2b,st,c(2,3,14,15))
addWorksheet(protect,'2B')
writeData(protect,'2B',p2b,headerStyle = headerStyle)
setColWidths(protect, '2B', cols = 1:15, widths = "auto")
addStyle(protect, '2B',style = csMoneyColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
addStyle(protect, '2B',style = csRatioColumn,rows = 2:200, cols = c(6,14),gridExpand = TRUE)

# tabs[[length(tabs)+1]] <- list('SS',pss,st,c(2,3,14,15))
addWorksheet(protect,'SS')
writeData(protect,'SS',pss,headerStyle = headerStyle)
setColWidths(protect, 'SS', cols = 1:15, widths = "auto")
addStyle(protect, 'SS',style = csMoneyColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
addStyle(protect, 'SS',style = csRatioColumn,rows = 2:200, cols = c(6,14),gridExpand = TRUE)


# tabs[[length(tabs)+1]] <- list('3B',p3b,st,c(2,3,14,15))
addWorksheet(protect,'3B')
writeData(protect,'3B',p3b,headerStyle = headerStyle)
setColWidths(protect, '3B', cols = 1:15, widths = "auto")
addStyle(protect, '3B',style = csMoneyColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
addStyle(protect, '3B',style = csRatioColumn,rows = 2:200, cols = c(6,14),gridExpand = TRUE)

# tabs[[length(tabs)+1]] <- list('OF',pof,st,c(2,3,14,15))
addWorksheet(protect,'OF')
writeData(protect,'OF',pof,headerStyle = headerStyle)
setColWidths(protect, 'OF', cols = 1:15, widths = "auto")
addStyle(protect, 'OF',style = csMoneyColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
addStyle(protect, 'OF',style = csRatioColumn,rows = 2:200, cols = c(6,14),gridExpand = TRUE)

# tabs[[length(tabs)+1]] <- list('DH',pdh,st,c(2,3,14,15))
addWorksheet(protect,'DH')
writeData(protect,'DH',pdh,headerStyle = headerStyle)
setColWidths(protect, 'DH', cols = 1:15, widths = "auto")
addStyle(protect, 'DH',style = csMoneyColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
addStyle(protect, 'DH',style = csRatioColumn,rows = 2:200, cols = c(6,14),gridExpand = TRUE)

# st <- list('4'=csMoneyColumn,'5'=csMoneyColumn,'6'=csRatioColumn)
# tabs[[length(tabs)+1]] <- list('Other',pna,st,c(2,3,14,15))
addWorksheet(protect,'Other')
writeData(protect,'Other',pna,headerStyle = headerStyle)
setColWidths(protect, 'Other', cols = 1:15, widths = "auto")
addStyle(protect, 'Other',style = csMoneyColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
addStyle(protect, 'Other',style = csRatioColumn,rows = 2:200, cols = c(6,14),gridExpand = TRUE)


# tabs[[length(tabs)+1]] <- list('SP',psp,st,c(2,3,14,15))
addWorksheet(protect,'SP')
writeData(protect,'SP',psp,headerStyle = headerStyle)
setColWidths(protect, 'SP', cols = 1:15, widths = "auto")
addStyle(protect, 'SP',style = csMoneyColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
addStyle(protect, 'SP',style = csRatioColumn,rows = 2:200, cols = c(5,13),gridExpand = TRUE)



# tabs[[length(tabs)+1]] <- list('MR',pmr,st,c(2,3,14,15))
addWorksheet(protect,'MR')
writeData(protect,'MR',pmr,headerStyle = headerStyle)
setColWidths(protect, 'MR', cols = 1:15, widths = "auto")
addStyle(protect, 'MR',style = csMoneyColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
addStyle(protect, 'MR',style = csRatioColumn,rows = 2:200, cols = c(5,13),gridExpand = TRUE)

# tabs[[length(tabs)+1]] <- list('CL',pcl,st,c(2,3,14,15))
addWorksheet(protect,'CL')
writeData(protect,'CL',pcl,headerStyle = headerStyle)
setColWidths(protect, 'CL', cols = 1:15, widths = "auto")
addStyle(protect, 'CL',style = csMoneyColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
addStyle(protect, 'CL',style = csRatioColumn,rows = 2:200, cols = c(5,13),gridExpand = TRUE)

# 
# st <- list('6'=csMoneyColumn,'7'=csMoneyColumn,'8'=csMoneyColumn)
# tabs[[length(tabs)+1]] <- list('BP',bp,st,c(2,3,14,15))
addWorksheet(protect,'BP')
writeData(protect,'BP',bp,headerStyle = headerStyle)
setColWidths(protect, 'BP', cols = 1:15, widths = "auto")
addStyle(protect, 'BP',style = csMoneyColumn,rows = 2:200, cols = 7:8,gridExpand = TRUE)

# tabs[[length(tabs)+1]] <- list('BH',bh,st,c(2,3,14,15))
addWorksheet(protect,'BH')
writeData(protect,'BH',bh,headerStyle = headerStyle)
setColWidths(protect, 'BH', cols = 1:15, widths = "auto")
addStyle(protect, 'BH',style = csMoneyColumn,rows = 2:200, cols = 7:8,gridExpand = TRUE)

# tabs[[length(tabs)+1]] <- list('BH',bh,st,c(2,3,14,15))
addWorksheet(protect,'FairPriced')
writeData(protect,'FairPriced',fairpriced,headerStyle = headerStyle)
setColWidths(protect, 'FairPriced', cols = 1:15, widths = "auto")
addStyle(protect, 'FairPriced',style = csMoneyColumn,rows = 2:200, cols = 7:8,gridExpand = TRUE)

# st <- list('6'=csBAColumn,'11'=csRatioColumn)
# tabs[[length(tabs)+1]] <- list('Targets',targets,st,c(2))
addWorksheet(protect,'Targets')
writeData(protect,'Targets',targets,headerStyle = headerStyle)
setColWidths(protect, 'Targets', cols = 1:15, widths = "auto")
#addStyle(protect, 'Targets',style = csRatioColumn,rows = 2:200, cols = c(6,14),gridExpand = TRUE)

saveWorkbook(protect,"../protectionAnalysis.xlsx",overwrite = TRUE)

#lapply(tabs,addSheet,protect)

#saveWorkbook(protect,"protectionAnalysis.xlsx",overwrite = TRUE)


bestp <- as.data.frame(rpreds) %>% filter(pDFL>14.5,Pos=='SP',Team!='Liquor Cricketsd') %>% arrange(Value)

# some code to remember later
#sp <- inner_join(gleft,pitchers,by=c('Player')) %>% select(Player,playerid=playerid.y)
#sh <- inner_join(gleft,hitters,by=c('Player')) %>% select(Player,playerid=playerid.y)
#rooks <- rbind(sh,sp)
#write.csv(rooks,"2015RookieIDs.csv")

# #playing around with z-score
# bhitters <- filter(AllH,pDFL > 0)
# bhitters$zHR <- scale(bhitters$pHR)
# 
# b1b <- filter(p1b,DFL>0)
# b1b$zScore <- scale(b1b$DFL)

# Code to take all the Won transactions and update the Salaries
# library("tidyr")
# library("splitstackshape")
# fabids <- read.csv("2015Transactions.csv",stringsAsFactors=FALSE)
# fabids <- cSplit(fabids,"Players",sep="\n",direction="long")
# fabids$Player <- unlist(lapply(fabids$Players,swapName3))
# fabids$fTeam <- unlist(lapply(fabids$Players,tradeFrom))
# fabids <- select(fabids,Team,Player,Acquired=Effective,fTeam)
# fabids <- filter(fabids,str_detect(fTeam,'Won'))
# fabids <- filter(fabids,!str_detect(fTeam,'\\$0'))
# fabids <- mutate(fabids,Salary=as.numeric(str_match(fTeam,"\\$([0-9])+")[,2]))
# fabids <- select(fabids,-fTeam)
# fabids <- fabids[!duplicated(fabids$Player),]
# f2 <- select(fabids,Player,NSalary = Salary)
# r2 <- read.csv("2015FinalRosters.csv")
# r3 <- left_join(r2,f2)
# r3$NSalary <- ifelse(is.na(r3$NSalary),r3$Salary,r3$NSalary)
# r4 <- select(r3,-Salary,Salary=NSalary)
# write.csv(r4,"2016Rosters2.csv")

# Find salary errors
# need to switch names from lastyr
# join and relabel columns
# delete if year hasn't increased by 1
# lastyr <- read.csv("2015 Protection List.csv",skip=1)
# lastyr <- dplyr::rename(lastyr,PContract=Contract,PSalary=Salary)
# lastyr <- select(lastyr,Player,PSalary,PContract)
# lastyr$Player <- unlist(lapply(lastyr$Player,swapName2))
# 
# thisyr <- read.csv("2016Rosters3.csv")
# thisyr <- select(thisyr,Player, Team, Salary, Contract)
# 
# check <- inner_join(thisyr,lastyr)
# check2 <- filter(check,Contract <= PContract)

# The function to generate protectable players for any team
cs <- filter(rpreds,Team == 'Crap Shooters') %>% arrange(-Value)
bd <- filter(rpreds,Team == 'Butterflies & Daisies') %>% arrange(-Value)
bj <- filter(rpreds,Team == 'But Justice') %>% arrange(-Value)
nt <- filter(rpreds,Team == 'Neon Tetras') %>% arrange(-Value)
cm <- filter(rpreds,Team == "Chris' Mom for $1") %>% arrange(-Value)
kc <- filter(rpreds,Team == 'Kirby and the 10:15 Crew') %>% arrange(-Value)
cb <- filter(rpreds,Team == 'clowndog & banjo') %>% arrange(-Value)
fd <- filter(rpreds,Team == 'Fluffy the Destroyer') %>% arrange(-Value)
st <- filter(rpreds,Team == 'Soft Tossers') %>% arrange(-Value)
hh <- filter(rpreds,Team == "Hogan's Heroes") %>% arrange(-Value)
ss <- filter(rpreds,Team == "Sad Sacks") %>% arrange(-Value)
nh <- filter(rpreds,Team == "Nacho Helmet") %>% arrange(-Value)

# find and fix roster file problems

#rosters <- read.csv(str_c("../",year,"Rosters1.csv"), encoding="UTF-8")
#r1 <- rosters %>% filter(str_detect(playerid,"cbs"))
