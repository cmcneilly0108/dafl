# Bad injury news
# Where is Steven Kwan?


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


source("./daflFunctions.r")

### Set variables ###


aWeek <- as.integer((as.integer(today() - as.Date("2022-04-05"))+1)/7) + 1
tWeeks <-30


# Update data files
fd <- file.info("../steamerHROS.csv")$mtime
cd <- Sys.time()
dt <- difftime(cd, fd, units = "hours")
if (dt > 12) {
  system("bash ../scripts/pullSteamerROS.sh")
  system("bash ../scripts/pullBatXROS.sh")
  system("bash ../scripts/pullSteamer.sh")
  system("bash ../scripts/pullCBS.sh")
  system("bash ../scripts/pullCBS2.sh")
  system("bash ../scripts/salaryinfo.sh")
  system("bash ../scripts/fgInj.sh")
}


###  Get all data ###


# Salaries
sal <- getSalary()
stand <- getMLBstandings()
inj <- getInjuries()
twostarts <- get2starts()

#Get latest bullpen report
#f <- read_html('http://www.fangraphs.com/fantasy/category/bullpen-report/')
#l <- xml_find_one(f,'//a[contains(@title,"Bullpen")]')
#l <- xml_find_first(f,'//a[contains(@title,"Bullpen")]')
#bp <- xml_attr(l,'href')
#bp <- 'https://fantasy.fangraphs.com/bullpen-report-june-9-2021/'


#Load/Update DAFL standings file
standings <- read.csv("../DAFLWeeklyStandings.csv",stringsAsFactors=FALSE)
standings$Rank <- as.numeric(str_extract(standings$Rank,'[0-9]+'))
#Update DAFL standings file
dstand <- read.csv('../overall.csv',stringsAsFactors=FALSE,nrows=14) %>% select(Rank,Team,Total)
ddeets <- read.csv('../overall.csv',stringsAsFactors=FALSE,header=FALSE,skip=16)
ddeets <- select(ddeets,-V3,-V4,-V5)
# Need to convert format of columns

dfl <- split(ddeets, (0:nrow(ddeets) %/% 15))
dfl <- lapply(dfl,function(x) {colnames(x) = x[1, ]
                               x <- x[-1,]
                               x[[2]] <- as.numeric(x[[2]])
                               x})
dfl <- dfl[-11]
cstand <- Reduce(function(x, y) inner_join(x,y,by=c('Team')), c(list(dstand),dfl))
cstand$Week <- aWeek
cstand <- inner_join(cstand,nicks,by=c('Team')) %>% select(-Team) %>% rename(Team=Short)

standings <- filter(standings,Week != aWeek)
standings <- bind_rows(standings,cstand)
write.csv(standings,"../DAFLWeeklyStandings.csv",row.names=FALSE)
#Load/Update DAFL standings file


#Create Charts
leaders <- standings %>% filter(Week == max(Week), Rank <= 5 | Team == 'Cricket') %>% select(Team)
#l2 <- ifelse('Cricket' %in% leaders$Team,leaders$Team,append(leaders$Team,'Cricket'))
# add category rank columns
s2 <- standings %>% group_by(Week) %>% mutate(rHR = rank(HR),rR = rank(R),rSB = rank(SB),
                                              rRBI = rank(RBI),rBA = rank(BA),rW = rank(W)
                                              ,rS = rank(S),rHD = rank(HD),rK = rank(K)
                                              ,rERA = rank(-ERA))
s2 <- mutate(s2,TP=rHR+rR+rSB+rRBI+rBA+rW+rS+rHD+rK+rERA) %>% select(-Avail)
# create line graph
g1 <- ggplot(data=filter(s2,Team %in% leaders$Team),
             aes(x=Week, y=TP, group=Team, shape=Team, color=Team)) + geom_line(size=1.2) +
  geom_point(size=4) + labs(title='Top 5 plus Crickets',y='Total Points')
s3 <- melt(s2,c('Team','Week'))
s3$value <- as.numeric(s3$value)
g2 <- ggplot(data=filter(s3,Team=='Cricket',variable %in% c('rHR','rR','rRBI','rBA','rSB')),
             aes(x=Week, y=value, group=variable, shape=variable,color=variable)) +
  geom_line(size=1.2) + geom_point(size=4) + labs(title='Crickets Hitting by Week',y='Points')
g3 <- ggplot(data=filter(s3,Team=='Cricket',variable %in% c('rW','rK','rS','rHD','rERA')),
             aes(x=Week, y=value, group=variable, shape=variable,color=variable)) +
  geom_line(size=1.2) + geom_point(size=4) + labs(title='Crickets Pitching by Week',y='Points')
pdf("../DAFLcharts.pdf")
print(g1)
print(g2)
print(g3)
dev.off()
#Create Charts

#Load Steamer rest of season projections
#for preseason
#hitters <- read.fg("steamerH2020.csv")
#pitchers <- read.fg("steamerP2020.csv")
# Once Season Starts

pitchers <- read.fg("../steamerPROS.csv")
#hitters <- read.fg("../steamerHROS.csv")
#pitchers <- read.fg("../batxPROS.csv")
hitters <- read.fg("../batxHROS.csv")


hitters$Pos <- replace(hitters$Pos,is.na(hitters$Pos),'DH')
hitters$pSGP <- hitSGP(hitters)
hitters <- select(hitters,-Player,-MLB,-Pos)

pitchers <- select(pitchers,-Player,-MLB,-Pos)


#Load CBS Data
Allhitters <- read.cbs("../AllHitters.csv")
# Allhitters - has 2 rows for ID=13145
# hitters - has 2 rows for ID=13145
AllH <- inner_join(Allhitters,hitters,by=c('playerid'),copy=FALSE)
# AllH - too many josh bell
Allpitchers <- read.cbs("../AllPitchers01.csv") %>% rename(INN = INNs)
Allpitchers2 <- read.cbs("../AllPitchers02.csv") %>% select(playerid,HD)
#Allpitchers <- left_join(Allpitchers,Allpitchers2,by=c("playerid"))
Allpitchers$HD <- Allpitchers2$HD
Allpitchers$Pos <- with(Allpitchers,ifelse(Pos=='SP','SP',ifelse(S>HD,'CL',ifelse(HD>0,'MR','SP'))))

# Create 2week leverage stat - lvg = (w+l+sv+bs+hld)/IP - or G
Allpitchers <- mutate(Allpitchers,LVG = (W+L+S+BS+HD)/APP)




#ytdp <- read.cbs("AllPYTD.csv") %>% filter(INN > 0)
#ytdh <- read.cbs("AllHYTD.csv") %>% filter(AB > 0)
#ytdp <- read.cbs("../AllPYTD.csv") 
ytdp <- read.cbs("../AllPYTD02.csv") %>% select(playerid,HD)
ytdp2 <- select(ytdp,playerid,HD) %>% rename(yHLD = HD)



#Allpitchers <- left_join(Allpitchers,Allpitchers2,by=c("playerid"))
ytdh <- read.cbs("../AllHYTD.csv") 


AllP <- inner_join(Allpitchers,pitchers,by=c('playerid'),copy=FALSE)
AllP <- left_join(AllP,ytdp2,by=c('playerid'),copy=FALSE)
# give 50/50 weight to 2weeks/ytd
AllP$pHLD <- with(AllP,round(((HD/2)*(tWeeks-aWeek)*.5)+((yHLD/aWeek)*(tWeeks-aWeek)*.5)),0)
# For short season, only use current year predictions
#AllP$pHLD <- with(AllP,round(((yHLD/aWeek)*(tWeeks-aWeek))))

AllP$pHLD <- ifelse(is.na(AllP$pHLD),0,AllP$pHLD)
AllP$pSGP <- pitSGP(AllP)


# Add in position eligibility based on 20 games
pedf <- read.cbs('../poselig.csv')
pedf <- dplyr::rename(pedf,posEl=Eligible) %>% select(playerid,posEl)
# Add column into AllH
AllH <- left_join(AllH,pedf,by=c('playerid'))
AllH <- mutate(AllH,Position=firstPos(posEl))
AllH <- mutate(AllH,Position=ifelse((Pos %in% c('SS','2B','C') &
                                                 str_detect(posEl,Pos)==TRUE),Pos,Position))

# duplicate problem - clean up before generating dollars
# later joins may create more later, but best to clean up before generating $$$
AllH <- distinct(AllH)
AllP <- distinct(AllP)

#Generate dollars
#nlist <- preLPP(AllH,AllP,data.frame(),(1-(aWeek/tWeeks)),50,40)
#nlist <- preLPP(AllH,AllP,data.frame(),1,50,40)
# Adjusted for short season
nlist <- preLPP(AllH,AllP,data.frame(),1,25,40)

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

## I forget what I was trying to do here, commented out
# # # Generate YTD zScores
# r2 <- hotScores(ytdh,ytdp)
# oh2 <- r2[[1]]
# op2 <- r2[[2]]
# AllH <- left_join(AllH,oh2,by=c('playerid'))
# AllP <- left_join(AllP,op2,by=c('playerid'))
# AllH <- rename(AllH,ytdscore=zScore)
# AllP <- rename(AllP,ytdscore=zScore)


# # # Generate positional advantage zScores
r <- zScores(AllH,AllP)
oh3 <- r[[1]]
op3 <- r[[2]]
oh3 <- rename(oh3,pScore = zScore)
op3 <- rename(op3,pScore = zScore)
# Incorporate scores back into AllH, AllP
AllH <- left_join(AllH,oh3,by=c('playerid'))
AllP <- left_join(AllP,op3,by=c('playerid'))
#AllH <- mutate(AllH,diffscore = pScore - ytdscore)
#AllP <- mutate(AllP,diffscore = pScore - ytdscore)

#inj <- getInjuries()
AllH <- AllH %>% addInjuries() %>% addSalary()
#AllP <- AllP %>% addInjuries() %>% addSalary() %>% addMLBstandings() %>% add2starts()
AllP <- AllP %>% addInjuries() %>% addSalary() %>% add2starts()
#AllP <- AllP %>% addInjuries() %>% addSalary()

# Need to join stand
AllP <- left_join(AllP,stand,by=c('MLB'))
#AllP$Season <- '5-5'
#AllP$L10 <- '5-5'

# #Add in the estOPS for both hitters and pitchers
# hev <- read.csv("../BSHes.csv",stringsAsFactors=FALSE)
# hev <- rename(hev,mlb_id=player_id)
# AllH <- left_join(AllH,hev,by="mlb_id") %>% 
#   mutate(est_ops = est_slg + est_woba, ops_delta = est_ops - slg - woba)
# pes <- read.csv("BSPes.csv",stringsAsFactors=FALSE)
# pes <- rename(pes,mlb_id=player_id)
# AllP <- left_join(AllP,pes,by="mlb_id") %>% 
#   mutate(est_ops = est_slg + est_woba, ops_delta = est_ops - slg - woba)
  


#Other team/pos messes things up

# Create Free Agents
FAH <- filter(AllH,Team == 'Free Agent')
FAH <- select(FAH,-Team)
FAP <- filter(AllP,Team == 'Free Agent')
FAP <- select(FAP,-Team)



###  Create Reports  ###


# Closer report
#page <- read_html(bp) %>% html_nodes("table") %>% .[[8]] %>% html_table(,header=TRUE,fill=TRUE)
#page <- read_html(bp) %>% html_nodes("table") %>% findHTMLTable(30)
#names(page)[1]<-"Team"
#names(page)[6]<-"Extra"
#crep <- select(page,-Extra)
#crep <- page
#crep <- getbpReport(bp)

#FAP <- left_join(FAP,crep,by=c('Player'),copy=FALSE)
#availCL <- arrange(FAP,-pDFL) %>% filter(!is.na(Score)) %>%
# select(Player,Pos,pDFL,pSGP, Score,Rank,pSV,pHLD,pW,pSO,pERA,pK.9,pBB.9,pGS,W,K,S,HD,ERA,hotscore,LVG,Injury,Expected.Return)

#New FG Roster Resource Closers page
rrc <- getRRClosers()
rrcAvail <- inner_join(rrc,FAP,by=c('playerid'))
rrcResults <- arrange(rrcAvail,-pDFL) %>%
  select(Player,Pos,pDFL,pSGP,Role,Tags,Rank,pSV,pHLD,pW,pSO,pERA,pK.9,pBB.9,pGS,W,K,S,HD,ERA,hotscore,LVG,MLB, Season, L10,Injury,Expected.Return)


# change filter conditions here, check if BPReport is working, see if I'm only downloading 2 weeks
#allHolds2 <- FAP %>% filter(pHLD>0,pK.9 > 9,pDFL > 0) %>%
allHolds2 <- FAP %>% filter(pHLD>0,pDFL > 0) %>%
  arrange(-pDFL) %>%
  select(Player,Pos,pDFL,pSGP, MLB, Season, L10, Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pBB.9,W,K,S,HD,ERA,hotscore,LVG,Injury,Expected.Return)



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

#hplist <- read.csv("../fangraphs-the-board-dataH.csv",stringsAsFactors = FALSE) %>%
#  rename(Player = Name,playerid = playerId)
hplist <- getFGScouts("../fangraphs-the-board-dataH.json")
#pplist <- read.csv("../fangraphs-the-board-dataP.csv",stringsAsFactors = FALSE) %>%
#  rename(Player = Name,playerid = playerId)
pplist <- getFGScouts("../fangraphs-the-board-dataP.json")
proh <- inner_join(FAH,hplist,by=c('playerid'))
prospectH <- select(proh,Player=Player.x,MLB=Team,Current.Level=mlevel,Pos,Age=Age.y,FV=cFV,DFL=pDFL,Top.100=Ovr_Rank,Hit,Game,Raw,Spd) %>%
  arrange(desc(FV),desc(DFL))
prop <- inner_join(FAP,pplist,by=c('playerid'))
prospectP <- select(prop,Player=Player.x,MLB=Team,Current.Level=mlevel,Age=Age.y,FV=cFV,DFL=pDFL,Top.100=Ovr_Rank,FB,SL,CB,CH,CMD) %>%
  arrange(desc(FV),desc(DFL))




#Load Current Roster
myteam <- pullTeam('Liquor Crickets')
mh <- myteam[[1]]
mp <- myteam[[2]]

#FAP$twostarts <- 'no'
FAP$est_ops <- 0
FAP$ops_delta <- 0
FAH$est_ops <- 0
FAH$ops_delta <- 0


# Create worksheets
allsp <- FAP %>% arrange(-pDFL,-pSGP) %>% filter(pGS > 0) %>%
  select(Player,Pos,Age,pDFL,est_ops,ops_delta,pSGP,Rank,pW,pSO,pERA,pK.9,pFIP,pGS,W,K,S,HD,ERA,hotscore,twostarts,Injury,Expected.Return)

allClosers <- FAP %>% arrange(-pSV,-S,-pDFL) %>% filter(pSV>0) %>%
  select(Player,Pos,pDFL,pSGP,Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pFIP,W,K,S,HD,ERA,hotscore,LVG,Injury,Expected.Return)

TopFAH <- group_by(FAH,Pos) %>% arrange(Pos,-pDFL,-pSGP) %>% filter(rank(-pSGP) <= 14) %>%
  select(Player,Pos,Age,pDFL,est_ops,ops_delta,pSGP,Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,AVG,hotscore,Injury,Expected.Return)

HotFAH <- group_by(FAH,Pos) %>% arrange(Pos,-hotscore) %>% filter(rank(-pSGP) <= 14) %>%
  select(Player,Pos,Age,pDFL,est_ops,ops_delta,pSGP,Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,AVG,hotscore,Injury,Expected.Return)

TopOF <- filter(FAH,Pos=='OF',pDFL>0) %>% arrange(Pos,-pDFL,-pSGP) %>%
  select(Player,Pos,Age,pDFL,pSGP, Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,AVG,hotscore,Injury,Expected.Return)

TopLeft <- filter(FAH,Pos %in% c('3B','2B'),pDFL>0) %>% arrange(Pos,-pDFL,-pSGP) %>%
  select(Player,Pos,Age,pDFL,pSGP, Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,AVG,hotscore,Injury,Expected.Return)

# Category Targets
# How did I decide on the weights???

res <- pvCat(cstand$HR,0.2,as.numeric(filter(cstand,Team=='Cricket')$HR))
pvResults <- data.frame(category='HR',pvp = res[[1]],pvm = res[[2]])


res <- pvCat(cstand$RBI,(1/18),as.numeric(filter(cstand,Team=='Cricket')$RBI))
pvResults2 <- data.frame(category='RBI', pvp = res[[1]],pvm = res[[2]])
pvResults <- rbind(pvResults,pvResults2)

res <- pvCat(cstand$SB,.2,as.numeric(filter(cstand,Team=='Cricket')$SB))
pvResults2 <- data.frame(category='SB', pvp = res[[1]],pvm = res[[2]])
pvResults <- rbind(pvResults,pvResults2)

res <- pvCat(cstand$R,(1/18),as.numeric(filter(cstand,Team=='Cricket')$R))
pvResults2 <- data.frame(category='R', pvp = res[[1]],pvm = res[[2]])
pvResults <- rbind(pvResults,pvResults2)



res <- pvCat(cstand$W,(1/6),as.numeric(filter(cstand,Team=='Cricket')$W))
pvResults2 <- data.frame(category='W', pvp = res[[1]],pvm = res[[2]])
pvResults <- rbind(pvResults,pvResults2)

res <- pvCat(cstand$HD,0.5,as.numeric(filter(cstand,Team=='Cricket')$HD))
pvResults2 <- data.frame(category='HD', pvp = res[[1]],pvm = res[[2]])
pvResults <- rbind(pvResults,pvResults2)

res <- pvCat(cstand$S,0.5,as.numeric(filter(cstand,Team=='Cricket')$S))
pvResults2 <- data.frame(category='S', pvp = res[[1]],pvm = res[[2]])
pvResults <- rbind(pvResults,pvResults2)

res <- pvCat(cstand$K,(1/6),as.numeric(filter(cstand,Team=='Cricket')$K))
pvResults2 <- data.frame(category='K', pvp = res[[1]],pvm = res[[2]])
pvResults <- rbind(pvResults,pvResults2)


pvResults <- arrange(pvResults,-pvp)

# Where to focus time
df <- cstand %>% arrange(W)
num <- which(df$Team=='Cricket')
myscores <- data_frame(category='W',score = num)
df <- cstand %>% arrange(K)
num <- which(df$Team=='Cricket')
myscores <- myscores %>% add_row(category='K',score=num)
df <- cstand %>% arrange(S)
num <- which(df$Team=='Cricket')
myscores <- myscores %>% add_row(category='S',score=num)
df <- cstand %>% arrange(HD)
num <- which(df$Team=='Cricket')
myscores <- myscores %>% add_row(category='HD',score=num)
df <- cstand %>% arrange(-ERA)
num <- which(df$Team=='Cricket')
myscores <- myscores %>% add_row(category='ERA',score=num)

df <- cstand %>% arrange(HR)
num <- which(df$Team=='Cricket')
myscores <- myscores %>% add_row(category='HR',score=num)
df <- cstand %>% arrange(RBI)
num <- which(df$Team=='Cricket')
myscores <- myscores %>% add_row(category='RBI',score=num)
df <- cstand %>% arrange(R)
num <- which(df$Team=='Cricket')
myscores <- myscores %>% add_row(category='R',score=num)
df <- cstand %>% arrange(SB)
num <- which(df$Team=='Cricket')
myscores <- myscores %>% add_row(category='SB',score=num)
df <- cstand %>% arrange(BA)
num <- which(df$Team=='Cricket')
myscores <- myscores %>% add_row(category='BA',score=num)

myscores <- arrange(myscores,score)



#Create xlsx with tabbed data
wkly <- createWorkbook()
headerStyle <- createStyle(halign = "CENTER", textDecoration = "Bold")
csRatioColumn <- createStyle(numFmt = "##0.000")
csMoneyColumn <- createStyle(numFmt = "CURRENCY")
csIntColumn <- createStyle(numFmt = "##0.0")


addWorksheet(wkly,'Talent')
writeData(wkly,'Talent',RTot,headerStyle = headerStyle)
addStyle(wkly, 'Talent',style = csMoneyColumn,rows = 2:20, cols = 2,gridExpand = TRUE)
addStyle(wkly, 'Talent',style = csMoneyColumn,rows = 2:20, cols = 4,gridExpand = TRUE)
addStyle(wkly, 'Talent',style = csMoneyColumn,rows = 2:20, cols = 6,gridExpand = TRUE)
addStyle(wkly, 'Talent',style = csRatioColumn,rows = 2:20, cols = 8,gridExpand = TRUE)
setColWidths(wkly, 'Talent', cols = 1:25, widths = "auto")

addWorksheet(wkly,'My Hitters')
writeData(wkly,'My Hitters',mh,headerStyle = headerStyle)
addStyle(wkly, 'My Hitters',style = csMoneyColumn,rows = 2:20, cols = 3,gridExpand = TRUE)
addStyle(wkly, 'My Hitters',style = csRatioColumn,rows = 2:20, cols = 4,gridExpand = TRUE)
addStyle(wkly, 'My Hitters',style = csRatioColumn,rows = 2:20, cols = 18,gridExpand = TRUE)
setColWidths(wkly, 'My Hitters', cols = 1:25, widths = "auto")

addWorksheet(wkly,'My Pitchers')
writeData(wkly,'My Pitchers',mp,headerStyle = headerStyle)
addStyle(wkly, 'My Pitchers',style = csMoneyColumn,rows = 2:20, cols = 3,gridExpand = TRUE)
addStyle(wkly, 'My Pitchers',style = csRatioColumn,rows = 2:20, cols = 4,gridExpand = TRUE)
addStyle(wkly, 'My Pitchers',style = csRatioColumn,rows = 2:20, cols = 20,gridExpand = TRUE)
setColWidths(wkly, 'My Pitchers', cols = 1:25, widths = "auto")

addWorksheet(wkly,'Top Hitters')
writeData(wkly,'Top Hitters',as.data.frame(TopFAH),headerStyle = headerStyle)
addStyle(wkly, 'Top Hitters',style = csMoneyColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
addStyle(wkly, 'Top Hitters',style = csRatioColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
addStyle(wkly, 'Top Hitters',style = csRatioColumn,rows = 2:200, cols = 6,gridExpand = TRUE)
addStyle(wkly, 'Top Hitters',style = csRatioColumn,rows = 2:200, cols = 7,gridExpand = TRUE)
addStyle(wkly, 'Top Hitters',style = csRatioColumn,rows = 2:200, cols = 19,gridExpand = TRUE)
setColWidths(wkly, 'Top Hitters', cols = 1:25, widths = "auto")

addWorksheet(wkly,'SP')
writeData(wkly,'SP',allsp,headerStyle = headerStyle)
addStyle(wkly, 'SP',style = csMoneyColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
addStyle(wkly, 'SP',style = csRatioColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
addStyle(wkly, 'SP',style = csRatioColumn,rows = 2:200, cols = 6,gridExpand = TRUE)
addStyle(wkly, 'SP',style = csRatioColumn,rows = 2:200, cols = 7,gridExpand = TRUE)
addStyle(wkly, 'SP',style = csRatioColumn,rows = 2:200, cols = 20,gridExpand = TRUE)
setColWidths(wkly, 'SP', cols = 1:25, widths = "auto")

addWorksheet(wkly,'Cl')
writeData(wkly,'Cl',allClosers,headerStyle = headerStyle)
addStyle(wkly, 'Cl',style = csMoneyColumn,rows = 2:200, cols = 3,gridExpand = TRUE)
addStyle(wkly, 'Cl',style = csRatioColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
addStyle(wkly, 'Cl',style = csRatioColumn,rows = 2:200, cols = 18:19,gridExpand = TRUE)
setColWidths(wkly, 'Cl', cols = 1:25, widths = "auto")

# addWorksheet(wkly,'FanCl')
# writeData(wkly,'FanCl',availCL,headerStyle = headerStyle)
# addStyle(wkly, 'FanCl',style = csMoneyColumn,rows = 2:200, cols = 3,gridExpand = TRUE)
# addStyle(wkly, 'FanCl',style = csRatioColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
# addStyle(wkly, 'FanCl',style = csRatioColumn,rows = 2:200, cols = 20:21,gridExpand = TRUE)
# setColWidths(wkly, 'FanCl', cols = 1:20, widths = "auto")

addWorksheet(wkly,'Hld')
writeData(wkly,'Hld',allHolds2,headerStyle = headerStyle)
addStyle(wkly, 'Hld',style = csMoneyColumn,rows = 2:200, cols = 3,gridExpand = TRUE)
addStyle(wkly, 'Hld',style = csRatioColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
addStyle(wkly, 'Hld',style = csRatioColumn,rows = 2:200, cols = 22:23,gridExpand = TRUE)
setColWidths(wkly, 'Hld', cols = 1:22, widths = "auto")

addWorksheet(wkly,'RRClosers')
writeData(wkly,'RRClosers',rrcResults,headerStyle = headerStyle)
addStyle(wkly, 'RRClosers',style = csMoneyColumn,rows = 2:200, cols = 3,gridExpand = TRUE)
addStyle(wkly, 'RRClosers',style = csRatioColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
addStyle(wkly, 'RRClosers',style = csRatioColumn,rows = 2:200, cols = 20:22,gridExpand = TRUE)
setColWidths(wkly, 'RRClosers', cols = 1:29, widths = "auto")

addWorksheet(wkly,'CTargets')
writeData(wkly,'CTargets',pvResults,headerStyle = headerStyle)
addStyle(wkly, 'CTargets',style = csRatioColumn,rows = 2:200, cols = 2:3,gridExpand = TRUE)
setColWidths(wkly, 'CTargets', cols = 1:20, widths = "auto")

addWorksheet(wkly,'CategoryPoints')
writeData(wkly,'CategoryPoints',myscores,headerStyle = headerStyle)
#addStyle(wkly, 'CTargets',style = csRatioColumn,rows = 2:200, cols = 2:3,gridExpand = TRUE)
setColWidths(wkly, 'CTargets', cols = 1:20, widths = "auto")

addWorksheet(wkly,'Prosp P')
writeData(wkly,'Prosp P',prospectP,headerStyle = headerStyle)
addStyle(wkly, 'Prosp P',style = csMoneyColumn,rows = 2:200, cols = 6,gridExpand = TRUE)
addStyle(wkly, 'Prosp P',style = csIntColumn,rows = 2:200, cols = 4,gridExpand = TRUE)
setColWidths(wkly, 'Prosp P', cols = 1:22, widths = "auto")

addWorksheet(wkly,'Prosp H')
writeData(wkly,'Prosp H',prospectH,headerStyle = headerStyle)
addStyle(wkly, 'Prosp H',style = csMoneyColumn,rows = 2:200, cols = 7,gridExpand = TRUE)
addStyle(wkly, 'Prosp H',style = csIntColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
setColWidths(wkly, 'Prosp H', cols = 1:22, widths = "auto")


saveWorkbook(wkly,"../weeklyUpdate.xlsx",overwrite = TRUE)

#ad hoc queries

# # For a position, who has surplus?
 f <- AllH %>% filter(Pos == 'OF',pDFL > 8) %>% group_by(Team) %>% summarize(nGood = length(Team))
# f2 <- AllH %>% filter(Pos == '2B') %>% group_by(Team) %>% summarize(nTotal = length(Team))
# ff <- left_join(f2,f,by=c('Team')) %>% arrange(-nGood,-nTotal)
#
f <- AllP %>% filter(pSV > 10) %>% group_by(Team) %>% summarize(nGood = length(Team)) %>% arrange(-nGood)
svRP <- AllP %>% filter(pSV > 10) %>% select(Player,Team,Salary,Contract,Pos,pDFL,pSGP,Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pFIP,W,K,S,HD,ERA,hotscore,Injury,Expected.Return)
#
svSP <- AllP %>% filter(pGS > 8, pDFL > 15) %>% select(Player,Team,Salary,Contract,Pos,pDFL,pSGP,Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pFIP,W,K,S,HD,ERA,hotscore,Injury,Expected.Return)

# #Find out what a team has that I can use
res <- pullTeam('Neon Tetras')[[1]]


# Add to hitter trend file
htrend <- mh %>% select(Player,hotscore) %>% mutate(Date=today())

write.table(htrend, "hTrend.csv", sep = ",", row.names=F, col.names = F, append = T)




#Load some team
#someteam <- pullTeam("But Justice")
#someteam <- pullTeam("Butterflies & Daisies")
#someteam <- pullTeam("clowndog & banjo")
#someteam <- pullTeam("Crap Shooters")
#someteam <- pullTeam("Dancing Homers")
#someteam <- pullTeam("East Lansing Laughing LLamas")
someteam <- pullTeam("Heinous Fuckery")
#someteam <- pullTeam("Hogan's Heroes")
#someteam <- pullTeam("Pearl Harbor")
#someteam <- pullTeam("Nacho Helmet")
#someteam <- pullTeam("Natural Catching Position")
#someteam <- pullTeam("Neon Tetras")
#someteam <- pullTeam("Sad Sacks")
#someteam <- pullTeam("Chamomile and Oxy")

sh <- someteam[[1]]
sp <- someteam[[2]]

takenP <- AllP %>% filter(Team != 'Free Agent') %>% arrange(-hotscore) %>%
  select(Player,Team,Pos,pDFL,pSGP,Rank,Salary,Contract,pW,pSO,pHLD,pSV,pERA,pK.9,pFIP,W,K,HD,S,ERA,hotscore,Injury,Expected.Return)
takenH <- AllH %>% filter(Team != 'Free Agent') %>% arrange(-hotscore) %>%
  select(Player,Team,Pos,pDFL,pSGP,Rank,Salary,Contract,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,AVG,hotscore,Injury,Expected.Return)

# Some queries to identify duplicate players
countPlayers <- function(tn){
  tH <- filter(AllH,Team == tn)
  hc <- nrow(tH)
  tP <- filter(AllP,Team == tn)
  pc <- nrow(tP)
  hc + pc
}
lst <- unique(AllH$Team)
lst2 <- lapply(lst,countPlayers)

teamSize <- data.frame(Team=lst)
teamSize$count <- lst2

# See lists by position
a1b <- AllH %>% filter(str_detect(posEl,"1B")) %>%
  select(Player,Pos,Age,pDFL,Team,pSGP,Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,AVG,hotscore,Injury,Expected.Return) %>%
  arrange(-pDFL)
a3b <- AllH %>% filter(str_detect(posEl,"3B")) %>%
  select(Player,Pos,Age,pDFL,Team,pSGP,Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,AVG,hotscore,Injury,Expected.Return) %>%
  arrange(-pDFL)
aof <- AllH %>% filter(str_detect(posEl,"OF")) %>%
  select(Player,Pos,Age,pDFL,Team,pSGP,Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,AVG,hotscore,Injury,Expected.Return) %>%
  arrange(-pDFL)
acl <- AllP %>% arrange(-pSV,-S,-pDFL) %>% filter(pSV>0) %>%
  select(Player,Pos,pDFL,Team,pSGP,Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pFIP,W,K,S,HD,ERA,hotscore,LVG,Injury,Expected.Return)
asp <- AllP %>% arrange(-pDFL) %>% filter(pDFL>15,pW>5,Salary>15) %>%
  select(Player,Pos,pDFL,Team,Salary,Contract,pSGP,Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pFIP,W,K,S,HD,ERA,hotscore,LVG,Injury,Expected.Return)

# Who needs an OF?
needsOF <- aof %>% filter(pDFL > 7) %>% group_by(Team) %>% 
  summarise(total = sum(pDFL)) %>% arrange(-total)
needsOF <- inner_join(needsOF,st) %>% select(-Avail,-Short)

# Let's play with htrend file
htrend <- read.csv("hTrend.csv")
htrend$date <- mdy(htrend$date)
#ggplot(data=htrend, aes(x=date, y=hotscore, group=Player)) +
#  geom_line(aes(color=Player))+
#  geom_point(aes(color=Player))

# Recently injured
newHurt <- AllH %>% mutate(idate=as_date(Injury,format="%m/%d")) %>% filter(!is.na(Injury) & idate > today()-7 & Team != "Free Agent") %>%
  select(Player,Pos,Age,pDFL,Team,hotscore,Injury,Expected.Return)
newHurt2 <- AllP %>% mutate(idate=as_date(Injury,format="%m/%d")) %>% filter(!is.na(Injury) & idate > today()-7 & Team != "Free Agent") %>%
  select(Player,Pos,Age,pDFL,Team,hotscore,Injury,Expected.Return)
newHurt <- bind_rows(newHurt,newHurt2) %>% arrange(Team)

# Big slumpers
newSlump <- AllH %>% filter(pDFL > 15 & hotscore < 3 & Team != "Free Agent") %>%
  select(Player,Pos,Age,pDFL,Team,hotscore,Injury,Expected.Return)
newSlump2 <- AllP %>% filter(pDFL > 8 & hotscore < 3 & Team != "Free Agent") %>%
  select(Player,Pos,Age,pDFL,Team,hotscore,Injury,Expected.Return)
newSlump <- bind_rows(newSlump,newSlump2) %>% arrange(Team)

problems <- bind_rows(newHurt, newSlump) %>% arrange(Team)

#Combing pvp, pvm with points
catSummary <- left_join(myscores,pvResults)
