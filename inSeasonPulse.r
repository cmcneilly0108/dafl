# update Week
# update fangraphs bullpen URL
# update weekly standings file


library("xlsx")
library("stringr")
library("dplyr")
library("XML")
library("ggplot2")
library("reshape2")
library("lubridate")
library("zoo")

source("./daflFunctions.r")


fd <- file.info("steamerHROS.csv")$mtime
cd <- Sys.time()
dt <- difftime(cd, fd, units = "hours")
if (dt > 10) {
  system("./pullSteamerROS.sh")
  system("./pullCBS.sh")
}


# Data that needs to be updated manually
#aWeek <- 21
# 04/05/2015
aWeek <- as.integer((as.integer(today() - as.Date("2015-04-05"))-1)/7) + 1
tWeeks <-26
bp <- "http://www.fangraphs.com/fantasy/bullpen-report-august-25-2015/"
ytdf <- "AllPYTD.csv"
# End manual update data

#Load DAFL standings file
standings <- read.csv("DAFLWeeklyStandings.csv",stringsAsFactors=FALSE)
standings$Rank <- as.numeric(str_extract(standings$Rank,'[0-9]+'))
#Update DAFL standings file
dstand <- read.csv('overall.csv',stringsAsFactors=FALSE,nrows=15) %>% select(Rank,Team,Total)
ddeets <- read.csv('overall.csv',stringsAsFactors=FALSE,header=FALSE,skip=17)
ddeets <- select(ddeets,-V3,-V4,-V5)
# Need to convert format of columns

dfl <- split(ddeets, (0:nrow(ddeets) %/% 16))
dfl <- lapply(dfl,function(x) {colnames(x) = x[1, ]
                               x <- x[-1,]
                               x[[2]] <- as.numeric(x[[2]])
                               x})
dfl <- dfl[-11]
cstand <- Reduce(function(x, y) inner_join(x,y,by=c('Team')), c(list(dstand),dfl))
cstand$Week <- aWeek
nicks <- read.csv("nicknames.csv",stringsAsFactors=FALSE)
cstand <- inner_join(cstand,nicks,by=c('Team')) %>% select(-Team) %>% rename(Team=Short)

standings <- filter(standings,Week != aWeek)
standings <- rbind(standings,cstand)
write.csv(standings,"DAFLWeeklyStandings.csv",row.names=FALSE)

#Create Charts
leaders <- standings %>% filter(Week == max(Week), Rank <= 5 | Team == 'Cricket') %>% select(Team)
#l2 <- ifelse('Cricket' %in% leaders$Team,leaders$Team,append(leaders$Team,'Cricket'))
# add category rank columns
s2 <- standings %>% group_by(Week) %>% mutate(rHR = rank(HR),rR = rank(R),rSB = rank(SB),
                                              rRBI = rank(RBI),rBA = rank(BA),rW = rank(W)
                                              ,rS = rank(S),rHD = rank(HD),rK = rank(K)
                                              ,rERA = rank(-ERA))
s2 <- mutate(s2,TP=rHR+rR+rSB+rRBI+rBA+rW+rS+rHD+rK+rERA)
# create line graph
g1 <- ggplot(data=filter(s2,Team %in% leaders$Team),
             aes(x=Week, y=TP, group=Team, shape=Team, color=Team)) + geom_line(size=1.2) +
  geom_point(size=4) + labs(title='Top 5 plus Crickets',y='Total Points')
s3 <- melt(s2,c('Team','Week'))
g2 <- ggplot(data=filter(s3,Team=='Cricket',variable %in% c('rHR','rR','rRBI','rBA','rSB')),
             aes(x=Week, y=value, group=variable, shape=variable,color=variable)) +
  geom_line(size=1.2) + geom_point(size=4) + labs(title='Crickets Hitting by Week',y='Points')
g3 <- ggplot(data=filter(s3,Team=='Cricket',variable %in% c('rW','rK','rS','rHD','rERA')),
             aes(x=Week, y=value, group=variable, shape=variable,color=variable)) +
  geom_line(size=1.2) + geom_point(size=4) + labs(title='Crickets Pitching by Week',y='Points')
pdf("DAFLcharts.pdf")
print(g1)
print(g2)
print(g3)
dev.off()

#Load Steamer rest of season projections
hitters <- read.fg("steamerHROS.csv")
hitters$Pos <- replace(hitters$Pos,is.na(hitters$Pos),'DH')
hitters$pSGP <- hitSGP(hitters)
hitters <- select(hitters,-Player,-MLB,-Pos)


pitchers <- read.fg("steamerPROS.csv")
pitchers <- select(pitchers,-Player,-MLB,-Pos)

#Load All Players - Extract Free Agents
Allhitters <- read.cbs("AllHitters.csv")

AllH <- inner_join(Allhitters,hitters,by=c('playerid'),copy=FALSE)

Allpitchers <- read.cbs("AllPitchers.csv")
Allpitchers$Pos <- with(Allpitchers,ifelse(Pos=='SP','SP',ifelse(S>HD,'CL',ifelse(HD>0,'MR','SP'))))

ytdp <- read.cbs(ytdf) %>% filter(INN > 0)
ytdh <- read.cbs("AllHYTD.csv") %>% filter(AB > 0)
ytdp2 <- select(ytdp,playerid,HD) %>% rename(yHLD = HD)


AllP <- inner_join(Allpitchers,pitchers,by=c('playerid'),copy=FALSE)
AllP <- left_join(AllP,ytdp2,by=c('playerid'),copy=FALSE)
# give 60/40 weight to YTD/3WKS
AllP$pHLD <- with(AllP,round(((HD/2)*(tWeeks-aWeek)*.2)+((yHLD/aWeek)*(tWeeks-aWeek)*.8)),0)
AllP$pHLD <- ifelse(is.na(AllP$pHLD),0,AllP$pHLD)
AllP$pSGP <- pitSGP(AllP)

#Generate dollars
#nlist <- preDollars(AllH,AllP,data.frame(),(1-(aWeek/tWeeks)),50,40)
nlist <- preLPP(AllH,AllP,data.frame(),(1-(aWeek/tWeeks)),50,40)
bhitters <- nlist[[1]]
bpitchers <- nlist[[2]]

# Incorporate scores back into AllH, AllP
AllH <- left_join(AllH,bhitters,by=c('playerid'))
AllP <- left_join(AllP,bpitchers,by=c('playerid'))
AllH <- rename(AllH,pDFL=zDFL)
AllP <- rename(AllP,pDFL=zDFL)
AllH$pDFL <- replace(AllH$pDFL,is.na(AllH$pDFL),0)
AllP$pDFL <- replace(AllP$pDFL,is.na(AllP$pDFL),0)

# Generate recent zScores
toph <- Allhitters
topp <- Allpitchers
r <- hotScores(toph,topp)
oh <- r[[1]]
op <- r[[2]]

# Incorporate scores back into AllH, AllP
AllH <- left_join(AllH,oh,by=c('playerid'))
AllP <- left_join(AllP,op,by=c('playerid'))
AllH <- rename(AllH,hotscore=zScore)
AllP <- rename(AllP,hotscore=zScore)


# # Generate YTD zScores
r2 <- hotScores(ytdh,ytdp)
oh2 <- r2[[1]]
op2 <- r2[[2]]
AllH <- left_join(AllH,oh2,by=c('playerid'))
AllP <- left_join(AllP,op2,by=c('playerid'))
AllH <- rename(AllH,ytdscore=zScore)
AllP <- rename(AllP,ytdscore=zScore)


# # # Generate positional advantage zScores
r <- zScores(AllH,AllP)
oh3 <- r[[1]]
op3 <- r[[2]]
oh3 <- rename(oh3,pScore = zScore)
op3 <- rename(op3,pScore = zScore)
# Incorporate scores back into AllH, AllP
AllH <- left_join(AllH,oh3,by=c('playerid'))
AllP <- left_join(AllP,op3,by=c('playerid'))
AllH <- mutate(AllH,diffscore = pScore - ytdscore)
AllP <- mutate(AllP,diffscore = pScore - ytdscore)


# Grab CBS injuries report - append to players, tab of draft for next year
injuries <- readHTMLTable("http://www.cbssports.com/mlb/injuries",skip=1,stringsAsFactors=F)
injuries <- injuries[-c(1,2)]
inj <- rbind_all(injuries) %>% select(-Updated,-Pos) %>% filter(!is.na(Player))
inj$Player <- str_replace(inj$Player,"Â."," ")
names(inj) <- sub(" ", ".", names(inj))
AllH <- left_join(AllH,inj,by=c('Player'))
AllP <- left_join(AllP,inj,by=c('Player'))

# Add season standings and L10 for Holds guys
stand <- readHTMLTable("http://www.cbssports.com/mlb/standings",stringsAsFactors=F)
sdf <- lapply(stand,function(x) {is.data.frame(x) && ncol(x) == 15})
stand <- stand[unlist(sdf)]
stand <- do.call("rbind", stand)
colnames(stand) = stand[1, ]
stand <- mutate(stand,Season=str_c(W,L,sep='-'))
stand <- select(stand,Team,Season,L10) %>% filter(!str_detect(Team,'Team'))
stand$Team <- str_trim(str_replace(stand$Team,"\\(.+",""))
abbrev <- read.csv('teamAb.csv',stringsAsFactors=FALSE)
stand <- left_join(stand,abbrev) %>% select(-Team)
AllP <- left_join(AllP,stand,by=c('MLB'))



# Create Free Agents
FAH <- filter(AllH,Team == 'Free Agent')
FAH <- select(FAH,-Team)
FAP <- filter(AllP,Team == 'Free Agent')
FAP <- select(FAP,-Team)

#Load Current Roster
myteam <- pullTeam('Liquor Crickets')
mh <- myteam[[1]]
mp <- myteam[[2]]


# Create worksheets
allsp <- FAP %>% arrange(-pDFL,-pSGP) %>% filter(pHLD==0,pSV==0, pGS > 0) %>%
  select(Player,Pos,Age,pDFL,pSGP,Rank,pW,pSO,pERA,pK.9,pFIP,pGS,W,K,S,HD,ERA,hotscore,Injury,Expected.Return)

allClosers <- FAP %>% arrange(-pSV,-S,-pDFL) %>% filter(pSV>0) %>%
  select(Player,Pos,pDFL,pSGP,Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pFIP,W,K,S,HD,ERA,hotscore,Injury,Expected.Return)

#allHolds <- FAP %>% filter(pHLD>0, pK.9 > 8.0, pBB.9 < 3.5) %>%
allHolds <- FAP %>% filter(pHLD>0) %>%
  arrange(-pHLD,-pDFL) %>%
  select(Player,Pos,pDFL,pSGP, Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pBB.9,W,K,S,HD,ERA,hotscore,Injury,Expected.Return)

allHolds2 <- FAP %>% filter(pHLD>0,pK.9 > 9,pDFL > 0) %>%
  arrange(-pDFL) %>%
  select(Player,Pos,pDFL,pSGP, MLB, Season, L10, Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pBB.9,W,K,S,HD,ERA,hotscore,Injury,Expected.Return)

TopFAH <- group_by(FAH,Pos) %>% arrange(Pos,-pDFL,-pSGP) %>% filter(rank(-pSGP) <= 8) %>%
  select(Player,Pos,Age,pDFL,pSGP, Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA,hotscore,Injury,Expected.Return)

# Closer report
c <- readHTMLTable(bp,stringsAsFactors=F)

f <- lapply(c,function(x) {is.data.frame(x) && ncol(x) == 5})
c2 <- c[unlist(f)]
crep <- c2[[1]]

colnames(crep) <- c(' ','Closer','First','Second','DL/Minors')
crep <- crep[-1,]

#crep <- readHTMLTable(bp, header=T, which=15,stringsAsFactors=F)
t <- data.frame(crep$Closer,10,stringsAsFactors=FALSE)
t2 <- data.frame(crep$First,5,stringsAsFactors=FALSE)
t3 <- data.frame(crep$Second,2,stringsAsFactors=FALSE)
colnames(t) <- c('Player','Score')
colnames(t2) <- c('Player','Score')
colnames(t3) <- c('Player','Score')
crep <- rbind_list(t,t2,t3)

crep$Player <- iconv(crep$Player,'UTF-8','ASCII')
availCL <- inner_join(crep,FAP,by=c('Player'),copy=FALSE) %>% arrange(-pDFL) %>%
  select(Player,Pos,pDFL,pSGP, Score,Rank,pSV,pHLD,pW,pSO,pERA,pK.9,pBB.9,pGS,W,K,S,HD,ERA,hotscore,Injury,Expected.Return)


# TALENT Tab - Calculate total SGPs per team, rank
st <- filter(standings,Week == max(Week)) %>% mutate(Short = Team, Actual = Rank) %>%
  select(Short,Actual) %>% inner_join(nicks,by=c('Short'))

# Positional Strength Chart
RH2 <- filter(AllH,Team != 'Free Agent') %>% group_by(Team,Pos) %>% summarize(hDFL = sum(pDFL))
RP2 <- filter(AllP,Team != 'Free Agent') %>% group_by(Team,Pos) %>% summarize(piDFL = sum(pDFL))

RH <- filter(AllH,Team != 'Free Agent') %>% group_by(Team) %>% summarize(hDFL = sum(pDFL))
RP <- filter(AllP,Team != 'Free Agent') %>% group_by(Team) %>% summarize(piDFL = sum(pDFL))
RTot <- inner_join(RH,RP,by=c('Team')) %>%
  mutate(tDFL = hDFL + piDFL,hRank = rank(-hDFL),pRank = rank(-piDFL)) %>%
  inner_join(st,by=c('Team')) %>%
  select(Team,hDFL,hRank,piDFL,pRank,tDFL,Actual) %>% arrange(-tDFL)

RTot$zScore <- as.numeric(scale(RTot$tDFL))
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
AllHr <- inner_join(AllH,prs,by=c('Player'))
AllPr <- inner_join(AllP,prs,by=c('Player'))

hp <- AllHr %>% filter(!is.na(rookRank) & Team=='Free Agent') %>% arrange(-pDFL,rookRank) %>%
  select(Player,MLB,rookRank,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)
pp <- AllPr %>% filter(!is.na(rookRank) & Team=='Free Agent') %>% arrange(-pDFL,rookRank) %>%
  select(Player,MLB,rookRank,DFL=pDFL,SGP=pSGP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD,Injury,Expected.Return)


# Add Salary, Contract to players
# create cron job to pull file
s <- read.csv("salaryinfo.csv",header=FALSE,stringsAsFactors=FALSE)
colnames(s) <- c('Player','Pos','Salary','Contract','Rank','Extra')
sal <- select(s,-Rank,-Extra) %>% 
  filter(!(Player %in% c('Batters','Pitchers','Player','TOTALS')))
#sal <- mutate(sal,Team = lag(Player))
#sal$Team <- ifelse(str_detect(sal$Team,'|'),lag(sal$Team),sal$Team)
sal <- mutate(sal,Team = ifelse(str_length(lag(Pos))==0,lag(Player),NA)) %>% filter(str_length(Pos)>0)
#sal$Team <- ifelse(str_length(sal$Team)==0,lag(sal$Team),sal$Team)
sal$Team <- na.locf(sal$Team)
sal <- mutate(sal, MLB = pullMLB(Player))
sal$Player <- unlist(lapply(sal$Player,stripName))
sal$Salary <- as.integer(sal$Salary)
sal$Contract <- as.integer(sal$Contract)
sal <- addPlayerid(sal) %>% select(playerid,Salary,Contract)
AllH <- left_join(AllH,sal,by=c('playerid'))
AllP <- left_join(AllP,sal,by=c('playerid'))



#Create xlsx with tabbed data
wkly <- createWorkbook()
csRatioColumn <- CellStyle(wkly, dataFormat=DataFormat("##0.00"))
csPctColumn <- CellStyle(wkly, dataFormat=DataFormat("#0.00%"))
csMoneyColumn <- CellStyle(wkly, dataFormat=DataFormat("$#,##0.00;-$#,##0.00"))

tabs <- list()
st <- list('2'=csMoneyColumn,'4'=csMoneyColumn,'6'=csMoneyColumn,'8'=csRatioColumn)
tabs[[length(tabs)+1]] <- list('Talent',RTot,st,c(2))
st <- list('3'=csMoneyColumn,'4'=csRatioColumn,'16'=csRatioColumn)
tabs[[length(tabs)+1]] <- list('My Hitters',mh,st,c(2,18,19))
st <- list('3'=csMoneyColumn,'4'=csRatioColumn,'18'=csRatioColumn)
tabs[[length(tabs)+1]] <- list('My Pitchers',mp,st,c(2,20,21))
st <- list('4'=csMoneyColumn,'5'=csRatioColumn,'17'=csRatioColumn)
tabs[[length(tabs)+1]] <- list('Top Hitters',TopFAH,st,c(2,19,20))
st <- list('4'=csMoneyColumn,'5'=csRatioColumn,'18'=csRatioColumn)
tabs[[length(tabs)+1]] <- list('SP',allsp,st,c(2,21,22))
st <- list('3'=csMoneyColumn,'4'=csRatioColumn,'18'=csRatioColumn)
tabs[[length(tabs)+1]] <- list('Cl',allClosers,st,c(2,20,21))
st <- list('3'=csMoneyColumn,'4'=csRatioColumn,'20'=csRatioColumn)
tabs[[length(tabs)+1]] <- list('FanCl',availCL,st,c(2,22,23))
st <- list('3'=csMoneyColumn,'4'=csRatioColumn,'18'=csRatioColumn)
tabs[[length(tabs)+1]] <- list('Hld',allHolds,st,c(2,20,21))
st <- list('3'=csMoneyColumn,'4'=csRatioColumn,'21'=csRatioColumn)
tabs[[length(tabs)+1]] <- list('Hld2',allHolds2,st,c(2,23,24))
st <- list('4'=csMoneyColumn,'5'=csRatioColumn)
tabs[[length(tabs)+1]] <- list('Prosp P',pp,st,c(2,12,13))
tabs[[length(tabs)+1]] <- list('Prosp H',hp,st,c(2,12,13))

lapply(tabs,addSheet,wkly)
saveWorkbook(wkly,"weeklyUpdate.xlsx")


#ad hoc queries

# # For a position, who has surplus?
 f <- AllH %>% filter(Pos == '2B',pDFL > 8) %>% group_by(Team) %>% summarize(nGood = length(Team))
 f2 <- AllH %>% filter(Pos == '2B') %>% group_by(Team) %>% summarize(nTotal = length(Team))
 ff <- left_join(f2,f,by=c('Team')) %>% arrange(-nGood,-nTotal)
#
# f <- AllP %>% filter(pSV > 10) %>% group_by(Team) %>% summarize(nGood = length(Team)) %>% arrange(-nGood)
#
# #Find out what a team has that I can use
# pullTeam('clowndog & banjo')[[1]]
#
# #Find players by position who can help immediately
# FAH %>% filter(Pos == 'C',pSGP > 2, pAVG > 0.25) %>% arrange(-pDFL,-pSGP) %>%
#   select(Player,Pos,pDFL,pSGP, Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)
#
# FAH %>% filter(pSGP > 8, BA > 0.26) %>% arrange(-pSGP) %>%
#   select(Player,Pos,pSGP, Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)
#
# # Filters by strong K's and low BB's
# FAP %>% filter(pHLD>10, pK.9 > 8.0, pBB.9 < 3.5) %>% arrange(-pDFL,-pHLD,-pSGP) %>%
#   select(Player,Pos,pDFL,pSGP, Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pBB.9,W,K,S,HD,ERA)
#
# # Top FA in a stat
# FAH %>% arrange(-pDFL,-pSGP) %>% filter(pHR > 9, pDFL > 5) %>%
#   select(Player,Pos,pDFL,pSGP, Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)
#
# FAP %>% arrange(-pDFL,-pSGP) %>% filter(QS>1,pBB.9 < 3.5) %>%
# select(Player,Pos,pDFL,pSGP, Rank,pW,pSO,pERA,pK.9,pBB.9,pGS,W,K,S,HD,ERA)
#
# #game <- "http://www.baseball-reference.com/boxes/TBA/TBA201405220.shtml"
# #c <- readHTMLTable(game,stringASFactors=F)
#
# #li <- AllP %>% filter(Team != 'Free Agent') %>% group_by(Team,Pos) %>%
# #  summarize(Count = length(Pos))

# Problems - rosscore is only scoring top players, differences in sample size
#blh <- arrange(AllH,-diffscore) %>% filter(is.na(Injury),diffscore > 0)
#blp <- arrange(AllP,-diffscore) %>% filter(is.na(Injury),diffscore > 0)

#unPit <- arrange(AllP,-diffscore) %>% filter(!(Team %in% c('Free Agent','Liquor Crickets')),diffscore > 0) %>% select(Player, Team, Salary, Contract, pDFL, diffscore, hotscore, ytdscore,pScore)
#unHit <- arrange(AllH,-diffscore) %>% filter(!(Team %in% c('Free Agent','Liquor Crickets')),diffscore > 0) %>% select(Player, Team, Salary, Contract, pDFL, diffscore, hotscore, ytdscore,pScore)
