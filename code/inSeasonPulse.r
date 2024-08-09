# ATC ROS exists


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
library("RSelenium")
library("netstat")
library("RSQLite")

source("./daflFunctions.r")

### Set variables ###


aWeek <- as.integer((as.integer(today() - as.Date("2024-03-31"))+1)/7) + 1
tWeeks <-30
#computer <- 'mac'
#computer <- 'windows'
computer <- Sys.info()['sysname']


# Update data files
fd <- file.info("../steamerHROS.json")$mtime
cd <- Sys.time()
dt <- as.integer(difftime(cd, fd, units = "hours"))
#dt <- 9
if (dt > 0) {
  system("bash ../scripts/pullSteamerROS.sh")
  system("bash ../scripts/pullBatXROS.sh")
  system("bash ../scripts/pullSteamer.sh")
  system("bash ../scripts/pullCBS.sh")
  system("bash ../scripts/pullCBS2.sh")
  system("bash ../scripts/salaryinfo.sh")
  # fgInj is only used for prospect json files.  Run manually
  system("bash ../scripts/fgInj.sh")
}


###  Get all data ###


# Salaries
sal <- getSalary()

# times out 8/15
stand <- getMLBstandings()
#inj <- getInjuriesFG()
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

dstandfull <- read.csv('../overall.csv',stringsAsFactors=FALSE,nrows=14) %>% select(-Rank,-X)

# Warning error here - look it up!
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
             aes(x=Week, y=TP, group=Team, shape=Team, color=Team)) + geom_line(linewidth=1.2) +
  geom_point(size=4) + labs(title='Top 5 plus Crickets',y='Total Points')
s3 <- melt(s2,c('Team','Week'))
s3$value <- as.numeric(s3$value)
g2 <- ggplot(data=filter(s3,Team=='Cricket',variable %in% c('rHR','rR','rRBI','rBA','rSB')),
             aes(x=Week, y=value, group=variable, shape=variable,color=variable)) +
  geom_line(linewidth=1.2) + geom_point(size=4) + labs(title='Crickets Hitting by Week',y='Points')
g3 <- ggplot(data=filter(s3,Team=='Cricket',variable %in% c('rW','rK','rS','rHD','rERA')),
             aes(x=Week, y=value, group=variable, shape=variable,color=variable)) +
  geom_line(linewidth=1.2) + geom_point(size=4) + labs(title='Crickets Pitching by Week',y='Points')
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

pitchers <- read.fg("../steamerPROS.json")
#pitchers <- read.fg("../batxPROS.json")

hitters <- read.fg("../batxHROS.json")
#hitters <- read.fg("../steamerHROS.json")


hitters$Pos <- replace(hitters$Pos,is.na(hitters$Pos),'DH')
hitters$pSGP <- hitSGP(hitters)
hitters <- select(hitters,-Player,-MLB,-Pos)

pitchers <- select(pitchers,-Player,-MLB,-Pos)

#Load CBS Data
Allhitters <- read.cbs("../AllHitters.csv")
#Allhitters <- distinct(Allhitters,playerid, .keep_all = TRUE)
# Allhitters - has 2 rows for ID=13145
# hitters - has 2 rows for ID=13145
AllH <- inner_join(Allhitters,hitters,by=c('playerid'),copy=FALSE)
# AllH - too many josh bell
Allpitchers <- read.cbs("../AllPitchers01.csv") %>% rename(INN = INNs)
Allpitchers2 <- read.cbs("../AllPitchers02.csv") %>% select(playerid,HD)
#Allpitchers <- left_join(Allpitchers,Allpitchers2,by=c("playerid"))
#Allpitchers$HD <- Allpitchers2$HD
#Allpitchers$Pos <- with(Allpitchers,ifelse(Pos=='SP','SP',ifelse(S>HD,'CL',ifelse(HD>0,'MR','SP'))))
#Allpitchers$Pos <- with(Allpitchers,ifelse(Pos=='SP','SP',ifelse(S>HD,'CL',ifelse(GS/APP>.5,'SP','MR'))))
Allpitchers$Pos <- with(Allpitchers,ifelse(S>HD,'CL',ifelse(GS/APP>.3,'SP','MR')))

# Create 2week leverage stat - lvg = (w+l+sv+bs+hld)/IP - or G
Allpitchers <- mutate(Allpitchers,LVG = (W+L+S+BS+HD)/APP)




#ytdp <- read.cbs("AllPYTD.csv") %>% filter(INN > 0)
#ytdh <- read.cbs("AllHYTD.csv") %>% filter(AB > 0)
#ytdp <- read.cbs("../AllPYTD.csv") 
ytdp <- read.cbs("../AllPYTD02.csv") %>% select(playerid,HD)
ytdp2 <- select(ytdp,playerid,HD) %>% rename(yHLD = HD)



#Allpitchers <- left_join(Allpitchers,Allpitchers2,by=c("playerid"))
ytdh <- read.cbs("../AllHYTD.csv") 

# clean up duplicates
AllpitchersD <- distinct(Allpitchers,playerid, .keep_all = TRUE)
pitchers <- distinct(pitchers,playerid, .keep_all = TRUE)

AllP <- inner_join(Allpitchers,pitchers,by=c('playerid'),copy=FALSE)
AllP <- left_join(AllP,ytdp2,by=c('playerid'),copy=FALSE,relationship = "many-to-many")
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
AllH <- left_join(AllH,pedf,by=c('playerid'),relationship = "many-to-many")
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
nlist <- preLPP(AllH,AllP,data.frame(),1,50,21)

bhitters <- nlist[[1]]
bpitchers <- nlist[[2]]

# Incorporate scores back into AllH, AllP
AllH <- left_join(AllH,bhitters,by=c('playerid'),relationship = "many-to-many")
AllP <- left_join(AllP,bpitchers,by=c('playerid'),relationship = "many-to-many")
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
AllH <- left_join(AllH,oh,by=c('playerid'),relationship = "many-to-many")
AllP <- left_join(AllP,op,by=c('playerid'),relationship = "many-to-many")
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
AllH <- left_join(AllH,oh3,by=c('playerid'),relationship = "many-to-many")
AllP <- left_join(AllP,op3,by=c('playerid'),relationship = "many-to-many")
#AllH <- mutate(AllH,diffscore = pScore - ytdscore)
#AllP <- mutate(AllP,diffscore = pScore - ytdscore)

#inj <- getInjuries()
#AllH <- AllH %>% addInjuriesFG() %>% addSalary()
AllH <- AllH %>% addSalary()
#AllP <- AllP %>% addInjuries() %>% addSalary() %>% addMLBstandings() %>% add2starts()
#AllP <- AllP %>% addInjuriesFG() %>% addSalary() %>% add2starts()
AllP <- AllP %>% addSalary()
#AllP <- AllP %>% addInjuries() %>% addSalary()

# New injury stuff
# Injuries data
fd <- file.info("../latestStuff.csv")$mtime
cd <- Sys.time()
dt <- as.integer(difftime(cd, fd, units = "hours"))
#dt <- 9
if ((computer!='Windows') | (dt < 20))
{
  injOrig <- read.csv("../latestInjuries.csv",stringsAsFactors=FALSE)
  injOrig <- injOrig %>% rename(`Latest Update` = `Latest.Update`,`Injury / Surgery Date` = `Injury...Surgery.Date`)
  
  stuff <- read.csv("../latestStuff.csv",stringsAsFactors=FALSE) %>%
    rename(`Pitching+`=`Pitching.`)
  
} else {
  rD <- rsDriver(browser="firefox",port=free_port(), 
                 chromever=NULL, verbose=F)
  remDr <- rD[["client"]]
  
  injOrig <- getInjuriesRS()
  stuff <- getStuffRS()

  remDr$close()
  system("taskkill /im java.exe /f")
  
}

# Week before draft - manual download file
# injOrig <- read.xlsx("../roster-resource-download.xlsx")
# injOrig <- rename(injOrig,Player=Name,MLB=Team)
# injOrig <- injOrig %>% rename(playerid = playerId,Injury = `Injury./.Surgery`,`Latest Update` = `Latest.Update`,`Injury / Surgery Date` = `Injury./.Surgery.Date`)


inj <- injOrig %>% select(Player,Injury,Expected.Return=`Latest Update`)
AllH <- left_join(AllH,inj,by=c('Player'),relationship = "many-to-many")
AllP <- left_join(AllP,inj,by=c('Player'),relationship = "many-to-many")


# Need to join stand
AllP <- left_join(AllP,stand,by=c('MLB'),relationship = "many-to-many")
AllP <- left_join(AllP,twostarts,relationship = "many-to-many")
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


# Get stuff+ data
#fn <- "https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=36&season=2024&month=0&season1=2024&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=2024-01-01&enddate=2024-12-31&sort=13,d&page=1_1000"
#fn <- "https://www.fangraphs.com/leaders/major-league?pos=all&stats=pit&lg=all&qual=0&type=36&season=2023&month=0&season1=2023&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=2023-01-01&enddate=2023-12-31&sort=13%2Cd&page=1_1000&pagenum=1&pageitems=2000000000"
# fn <- "https://www.fangraphs.com/leaders-legacy.aspx?pos=all&stats=pit&lg=all&qual=30&type=36&season=2023&month=0&season1=2023&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=2023-01-01&enddate=2023-12-31&page=1_1000"
# page <- read_html(fn) %>% html_nodes("table") %>% html_table()
# df <- page[[7]]
# df <- df %>% slice(-1) %>% select(-X1)
# names(df) <- df %>% slice(1) %>% unlist()
# df <- df %>% slice(-(1:2))
# #stuff <- df %>% select(Player=Name,MLB=Team,`Pitching+`)
# stuff <- df %>% select(Player=Name,`Pitching+`)
AllP <- left_join(AllP,stuff,relationship = "many-to-many")
#AllP$`Pitching+` <- 100
#AllP$twostarts <- 'no'

#All pitchers get a position
AllP <- AllP %>% mutate(Pos = ifelse(is.na(Pos),'SP',Pos))

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
  select(Player,Pos,pDFL,pSGP,Role,Tags,Rank,pSV,pHLD,pW,pSO,pERA,`pK/9`,`pBB/9`,pGS,W,K,S,HD,ERA,hotscore,LVG,MLB, Season, L10,Injury,Expected.Return,playerid)


# change filter conditions here, check if BPReport is working, see if I'm only downloading 2 weeks
#allHolds2 <- FAP %>% filter(pHLD>0,pK.9 > 9,pDFL > 0) %>%
allHolds2 <- FAP %>% filter(pHLD>0,pDFL > 0) %>%
  arrange(-pDFL) %>%
  select(Player,Pos,pDFL,pSGP, MLB, Season, L10, Rank,pW,pSO,pSV,pHLD,pERA,`pK/9`,`pBB/9`,W,K,S,HD,ERA,hotscore,LVG,Injury,Expected.Return)



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
#proh <- inner_join(FAH,hplist,by=c('playerid'))
proh <- right_join(Allhitters,hplist,by=c('playerid'),relationship = "many-to-many")
#df$birth_year <- replace(df$birth_year,is.na(df$birth_year),2010)

#prospectH <- select(proh,Player=Player.y,MLB=Team.y,Team=Team.x,Current.Level=mlevel,Pos,Age=Age.y,FV=cFV,DFL=pDFL,Top.100=Ovr_Rank,Hit,Game,Raw,Spd) %>%
#  arrange(desc(FV),desc(DFL))
prospectH <- select(proh,Player=Player.y,MLB=Team.y,Team=Team.x,Current.Level=mlevel,Pos,Age,FV=cFV,Top.100=Ovr_Rank,Hit,Game,Raw,Spd,playerid) %>%
  arrange(desc(FV),Top.100)
prop <- right_join(Allpitchers,pplist,by=c('playerid'),relationship = "many-to-many")
prospectP <- select(prop,Player=Player.y,MLB=Team.y,Team=Team.x,Current.Level=mlevel,Age,FV=cFV,Top.100=Ovr_Rank,FB,SL,CB,CH,CMD,playerid) %>%
  arrange(desc(FV),Top.100)

# which prospects are taken?
proth <- left_join(hplist,AllH,by=c('playerid'),na_matches="never",relationship = "many-to-many") 
aprospectH <- select(proth,Player=Player.x,MLB=Team.x,Team=Team.y,Current.Level=mlevel,Pos,Age=Age.y,FV=cFV,DFL=pDFL,Top.100=Ovr_Rank,Hit,Game,Raw,Spd) %>%
  arrange(desc(FV),desc(DFL))
protp <- left_join(pplist,AllP,by=c('playerid'),na_matches="never",relationship = "many-to-many") 
aprospectP <- select(protp,Player=Player.x,MLB=Team.x,Team=Team.y,,Current.Level=mlevel,Age=Age.y,FV=cFV,DFL=pDFL,Top.100=Ovr_Rank,FB,SL,CB,CH,CMD) %>%
  arrange(desc(FV),desc(DFL))


#Load Current Roster
myteam <- pullTeam('Liquor Crickets')
mh <- myteam[[1]]
mp <- myteam[[2]]

FAP$est_ops <- 0
FAP$ops_delta <- 0
FAH$est_ops <- 0
FAH$ops_delta <- 0


# Create worksheets
allsp <- FAP %>% arrange(-pDFL,-pSGP) %>% filter(pGS > 0) %>%
  select(Player,Pos,Age,pDFL,`Pitching+`,ops_delta,pSGP,Rank,pW,pSO,pERA,`pK/9`,pFIP,pGS,W,K,S,HD,ERA,hotscore,twostarts,Injury,Expected.Return)

allClosers <- FAP %>% arrange(-pSV,-S,-pDFL) %>% filter(pSV>0) %>%
  select(Player,Pos,pDFL,pSGP,Rank,pW,pSO,pSV,pHLD,pERA,`pK/9`,pFIP,W,K,S,HD,ERA,hotscore,LVG,Injury,Expected.Return)

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

res <- pvCat(cstand$HR,0.3,as.numeric(filter(cstand,Team=='Cricket')$HR))
pvResults <- data.frame(category='HR',pvp = res[[1]],pvm = res[[2]],opportunity=res[[3]])


res <- pvCat(cstand$RBI,0.3,as.numeric(filter(cstand,Team=='Cricket')$RBI))
pvResults2 <- data.frame(category='RBI', pvp = res[[1]],pvm = res[[2]],opportunity=res[[3]])
pvResults <- rbind(pvResults,pvResults2)

res <- pvCat(cstand$SB,0.3,as.numeric(filter(cstand,Team=='Cricket')$SB))
pvResults2 <- data.frame(category='SB', pvp = res[[1]],pvm = res[[2]],opportunity=res[[3]])
pvResults <- rbind(pvResults,pvResults2)

res <- pvCat(cstand$R,0.3,as.numeric(filter(cstand,Team=='Cricket')$R))
pvResults2 <- data.frame(category='R', pvp = res[[1]],pvm = res[[2]],opportunity=res[[3]])
pvResults <- rbind(pvResults,pvResults2)



res <- pvCat(cstand$W,0.3,as.numeric(filter(cstand,Team=='Cricket')$W))
pvResults2 <- data.frame(category='W', pvp = res[[1]],pvm = res[[2]],opportunity=res[[3]])
pvResults <- rbind(pvResults,pvResults2)

res <- pvCat(cstand$HD,0.3,as.numeric(filter(cstand,Team=='Cricket')$HD))
pvResults2 <- data.frame(category='HD', pvp = res[[1]],pvm = res[[2]],opportunity=res[[3]])
pvResults <- rbind(pvResults,pvResults2)

res <- pvCat(cstand$S,0.3,as.numeric(filter(cstand,Team=='Cricket')$S))
pvResults2 <- data.frame(category='S', pvp = res[[1]],pvm = res[[2]],opportunity=res[[3]])
pvResults <- rbind(pvResults,pvResults2)

res <- pvCat(cstand$K,0.3,as.numeric(filter(cstand,Team=='Cricket')$K))
pvResults2 <- data.frame(category='K', pvp = res[[1]],pvm = res[[2]],opportunity=res[[3]])
pvResults <- rbind(pvResults,pvResults2)


pvResults <- arrange(pvResults,-opportunity)

# Where to focus time
df <- cstand %>% arrange(W)
num <- which(df$Team=='Cricket')
#myscores <- data_frame(category='W',score = num)
myscores <- tibble(category='W',score = num)
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
#addStyle(wkly, 'SP',style = csRatioColumn,rows = 2:200, cols = 5,gridExpand = TRUE)
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
 f <- AllH %>% filter(Contract < 3,pDFL > 8) %>% group_by(Team) %>% summarize(nGood = length(Team))
# f2 <- AllH %>% filter(Pos == '2B') %>% group_by(Team) %>% summarize(nTotal = length(Team))
# ff <- left_join(f2,f,by=c('Team')) %>% arrange(-nGood,-nTotal)
#
f <- AllP %>% filter(pSV > 10) %>% group_by(Team) %>% summarize(nGood = length(Team)) %>% arrange(-nGood)
svRP <- AllP %>% filter(pSV > 10) %>% select(Player,Team,Salary,Contract,Pos,pDFL,pSGP,Rank,pW,pSO,pSV,pHLD,pERA,`pK/9`,pFIP,W,K,S,HD,ERA,hotscore,Injury,Expected.Return)
#
svSP <- AllP %>% filter(pGS > 8, pDFL > 15) %>% select(Player,Team,Salary,Contract,Pos,pDFL,pSGP,Rank,pW,pSO,pSV,pHLD,pERA,`pK/9`,pFIP,W,K,S,HD,ERA,hotscore,Injury,Expected.Return)

# #Find out what a team has that I can use
res <- pullTeam('Neon Tetras')[[1]]



# set up injOrig for reporting site
# AllH, AllP - ids plus pDFL, add pDFL
# Prune out taken players
# Select only fields that I want
dH <- AllH %>% select(playerid,pDFL,Team)
dP <- AllP %>% select(playerid,pDFL,Team)
dollars <- bind_rows(dH,dP)
injOrig <- left_join(injOrig,dollars,relationship = "many-to-many")
injOrig$pDFL[is.na(injOrig$pDFL)] <- 0
injOrig$Team[is.na(injOrig$Team)] <- 'Free Agent'
#injOrig <- injOrig %>% filter(Team=='Free Agent') %>% select(-X,-birth_year,-Team,-playerid)
injOrig <- injOrig %>% filter(Team=='Free Agent') %>% select(-X,-birth_year,-Team)

#Load some team
someteam <- pullTeam("But Justice")
#someteam <- pullTeam("Butterflies & Daisies")
#someteam <- pullTeam("clowndog & banjo")
#someteam <- pullTeam("Crap Shooters")
#someteam <- pullTeam("Dancing Homers")
#someteam <- pullTeam("East Lansing Laughing LLamas")
#someteam <- pullTeam("Heinous Fuckery")
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
  select(Player,Team,Pos,pDFL,pSGP,Rank,Salary,Contract,pW,pSO,pHLD,pSV,pERA,`pK/9`,pFIP,W,K,HD,S,ERA,hotscore,Injury,Expected.Return)
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
  select(Player,Pos,pDFL,Team,pSGP,Rank,pW,pSO,pSV,pHLD,pERA,`pK/9`,pFIP,W,K,S,HD,ERA,hotscore,LVG,Injury,Expected.Return)
asp <- AllP %>% arrange(-pDFL) %>% filter(pDFL>15,pW>5,Salary>15) %>%
  select(Player,Pos,pDFL,Team,Salary,Contract,pSGP,Rank,pW,pSO,pSV,pHLD,pERA,`pK/9`,pFIP,W,K,S,HD,ERA,hotscore,LVG,Injury,Expected.Return)

# Who needs an OF?
needsOF <- aof %>% filter(pDFL > 7) %>% group_by(Team) %>% 
  summarise(total = sum(pDFL)) %>% arrange(-total)
needsOF <- inner_join(needsOF,st) %>% select(-Avail,-Short)


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
catSummary <- left_join(myscores,pvResults,relationship = "many-to-many") %>% arrange(-opportunity)

# See if top 17 changes to strength of team predictions

RH <- filter(AllH,Team != 'Free Agent') %>% group_by(Team) %>% filter(rank(-pDFL) <= 9) %>% summarize(hDFL = sum(pDFL))
RP <- filter(AllP,Team != 'Free Agent') %>% group_by(Team) %>% filter(rank(-pDFL) <= 8)%>% summarize(piDFL = sum(pDFL))
RTotTop <- inner_join(RH,RP,by=c('Team')) %>%
  mutate(tDFL = hDFL + piDFL,hRank = rank(-hDFL),pRank = rank(-piDFL)) %>%
  inner_join(st,by=c('Team')) %>%
  select(Team,hDFL,hRank,piDFL,pRank,tDFL,Actual) %>% arrange(-tDFL)

RTotTop$zScore <- as.numeric(scale(RTotTop$tDFL))

# High pDFL players on shitty teams
# Find bottom half teams, top 10% of players

bottom <- RTot %>% filter(Actual > 7) %>% select(Team)
candH <- AllH %>% filter(Team %in% bottom$Team, Salary > 20) %>% select(Player, Team, pDFL, Salary, Contract)
candP <- AllP %>% filter(Team %in% bottom$Team, Salary > 20) %>% select(Player, Team, pDFL, Salary, Contract)
candTrades <- bind_rows(candH,candP) %>% arrange(Team,-pDFL)

youngStuds <- AllH %>% filter(Salary< 10, Age < 26) %>% 
  select(Player,Pos,Age,pDFL,Salary,Contract,pSGP,Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,AVG,hotscore,Injury,Expected.Return) %>% 
  arrange(-hotscore)
# 
# fn <- "https://www.fangraphs.com/prospects/the-board/2023-in-season-prospect-list/summary?filter=scoutgrade%7Cf%7CFV%7Cgt%7C50%7C50&pos=pit"
# fn <- "https://www.fangraphs.com/prospects/the-board/2023-in-season-prospect-list/summary?filter=scoutgrade%7Cf%7CFV%7Cgt%7C50%7C50&pos=bat"
# page <- read_html(fn) %>% html_nodes("table") %>% html_table()
# df <- page[[6]]
# df <- df %>% slice(-1) %>% select(-X1)
# names(df) <- df %>% slice(1) %>% unlist()
# df <- df %>% slice(-(1:2))
# stuff <- df %>% select(Player=Name,MLB=Team,`Pitching+`)

# check master file against CBS names
mhit <- missing.cbs("../AllHitters.csv")
mpit <- missing.cbs("../AllPitchers01.csv")


# What about storing the 2-week data everyday in a DB and then generate my charts from there?

# old htrend

# Add to hitter trend file
htrend <- mh %>% select(Player,hotscore) %>% mutate(Date=today())
#df <- mh %>% select(Player,hotscore) %>% mutate(Date=today())

write.table(htrend, "hTrend.csv", sep = ",", row.names=F, col.names = F, append = T)
# Let's play with htrend file
htrend <- read.csv("hTrend.csv")
htrend$date <- ymd(htrend$date)
#ggplot(data=htrend, aes(x=date, y=hotscore, group=Player)) +
#  geom_line(aes(color=Player))+
#  geom_point(aes(color=Player))



conn <- dbConnect(RSQLite::SQLite(), "DAFL.db")

# Player, pDFL, hotscore, date -from htrend.csv
htrend <- AllH %>% select(playerid,Player,hotscore) %>% mutate(Date=today())
ptrend <- AllP %>% select(playerid,Player,hotscore) %>% mutate(Date=today())
alltrend <- rbind(htrend,ptrend)

#dbWriteTable(conn,"Trending",alltrend,append=TRUE)

#trowcount <- dbGetQuery(conn, "SELECT count(*) FROM Trending")

trowcount <- dbGetQuery(conn, "SELECT count(*) FROM Trending where Date = ?",params = c(today()))
asnum <- as.numeric(trowcount[[1]])
if (asnum == 0) {
  dbWriteTable(conn,"Trending",alltrend,append=TRUE)
}

trending <- dbGetQuery(conn, "SELECT * FROM Trending")
trending$Date <- as.Date(trending$Date)
trending$hotscore <- as.numeric(trending$hotscore)





# playing with links
# https://www.fangraphs.com/players/trea-turner/16252/stats?position=SS
AllH <- AllH %>% mutate(fg=paste0("<a target = '_blank' href= '//www.fangraphs.com/players/abcd/",playerid,"/stats'>",Player,"</a>"))
AllH$Player <- AllH$fg
AllP <- AllP %>% mutate(fg=paste0("<a target = '_blank' href= '//www.fangraphs.com/players/abcd/",playerid,"/stats'>",Player,"</a>"))
AllP$Player <- AllP$fg

injOrig <- injOrig %>% mutate(fg=paste0("<a target = '_blank' href= '//www.fangraphs.com/players/abcd/",playerid,"/stats'>",Player,"</a>"))
injOrig$Player <- injOrig$fg
injOrig <- injOrig %>% select(-fg)

rrcResults <- rrcResults %>% mutate(fg=paste0("<a target = '_blank' href= '//www.fangraphs.com/players/abcd/",playerid,"/stats'>",Player,"</a>"))
rrcResults$Player <- rrcResults$fg
rrcResults <- rrcResults %>% select(-fg,-playerid)

prospectH <- prospectH %>% mutate(fg=paste0("<a target = '_blank' href= '//www.fangraphs.com/players/abcd/",playerid,"/stats'>",Player,"</a>"))
prospectH$Player <- prospectH$fg
prospectH <- prospectH %>% select(-fg,-playerid)

prospectP <- prospectP %>% mutate(fg=paste0("<a target = '_blank' href= '//www.fangraphs.com/players/abcd/",playerid,"/stats'>",Player,"</a>"))
prospectP$Player <- prospectP$fg
prospectP <- prospectP %>% select(-fg,-playerid)
