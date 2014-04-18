# 2 projection files
# 2 FA files
# 1 roster file
# update Week
# update ttWx

library("xlsx")
library("stringr")
library("dplyr")

# Data that needs to be update manually
Week <- 3

ttLabels <- c('AVG','HR','R','RBI','SB','ERA','HLD','K','SV','W')
ttW1 <- c(.229,3,25,21,3,2.01,1,58,1,4)
ttW2 <- c(.239,7,55,37,3,2.73,1,102,1,7)
ttW3 <- c(.239,11,68,50,5,2.44,2,134,2,8)

tt <- as.list(ttW3)
names(tt) <- ttLabels
# End manual update data


swapName2 <- function(n){
  comma <- str_locate(n,',')
  ln <- str_sub(n,1,comma-1)
  rest <- str_sub(n,comma+2,-1)
  space <- str_locate(rest,' ')
  fn <- str_sub(rest,1,space-1)
  nn <- str_join(fn,ln,sep=" ",collapse=NULL)
  nn[1] 
}

pullPos <- function(n){
  n <- str_trim(n)
  p <- str_match(n,".+, .+ (.+) .+")
  p <- p[,2]
  p <- ifelse((p =='P'),'RP',p)
  ifelse((p %in% c('CF','RF','LF')),'OF',p)
}

# Year End Totals
# c(209,735,739,159,42,90,1191,96)
# c('HR','RBI','R','SB','HLD','SV','K','W')
sTots <- list()
sTots$HR <- 209
sTots$RBI <- 735
sTots$R <- 739
sTots$SB <- 159
sTots$HLD <- 42
sTots$SV <- 90
sTots$K <- 1191
sTots$W <- 96
sTots$AVG <- .291
sTots$ERA <- 3.277

pTots <- list()
pTots$HR <- 1-tt$HR/sTots$HR
pTots$RBI <-  1-tt$RBI/sTots$RBI
pTots$R <-  1-tt$R/sTots$R
pTots$SB <-  1-tt$SB/sTots$SB
pTots$HLD <-  1-tt$HLD/sTots$HLD
pTots$SV <-  1-tt$SV/sTots$SV
pTots$K <-  1-tt$K/sTots$K
pTots$W <-  1-tt$W/sTots$W
#pTots$AVG <- .291
#pTots$ERA <- 3.277

#t <- as.data.frame(pTots)
#t <- as.data.frame(t(t))
#colnames(t) <- c('category','score')

#Load Steamer rest of season projections
# TASK - get data dynamically
hitters <- read.csv("steamerHROS.csv")
#hitters <- read.csv("zipsHROS.csv")
hitters$Player <- as.character(hitters$Name)
pitchers <- read.csv("steamerPROS.csv")
#pitchers <- read.csv("zipsPROS.csv")
pitchers$Player <- as.character(pitchers$Name)

#Load Free Agents
FAhitters <- read.csv("FAHitters.csv",skip=1)
FAhitters$Player <- as.character(FAhitters$Player)
FAhitters <- mutate(FAhitters, Pos = pullPos(Player))


FApitchers <- read.csv("FAPitchers.csv",skip=1)
FApitchers$Player <- as.character(FApitchers$Player)
FApitchers <- mutate(FApitchers, Pos = pullPos(Player))

FAhitters$Player <- unlist(lapply(FAhitters$Player,swapName2))
FApitchers$Player <- unlist(lapply(FApitchers$Player,swapName2))


FAH <- inner_join(FAhitters,hitters,by=c('Player'),copy=FALSE)
FAP <- inner_join(FApitchers,pitchers,by=c('Player'),copy=FALSE)

# Rough projection of holds - maintain the rate you're at
FAP$HLD <- with(FAP,(HD/2)*(30-Week))
#FAP$SV <- with(FAP,(S/2)*(30-Week))

#Load Current Roster
myhitters <- read.csv("LCRoster.csv",skip=2,nrows=13)
myhitters$Player <- as.character(myhitters$Player)
mypitchers <- read.csv("LCRoster.csv",skip=18,nrows=14)
mypitchers$Player <- as.character(mypitchers$Player)
myhitters$Player <- unlist(lapply(myhitters$Player,swapName2))
mypitchers$Player <- unlist(lapply(mypitchers$Player,swapName2))

myHros <- inner_join(myhitters,hitters,by=c('Player'),copy=FALSE)
myPros <- inner_join(mypitchers,pitchers,by=c('Player'),copy=FALSE)

myHros$gHR <- with(myHros,HR.y/sTots$HR)
myHros$gRBI <- with(myHros,RBI.y/sTots$RBI)
myHros$gR <- with(myHros,R.y/sTots$R)
myHros$gSB <- with(myHros,SB.y/sTots$SB)
myHros$gAVG <- with(myHros,(AVG - sTots$AVG)*(30 - Week)/270)
myHros$gVAL <- with(myHros,gHR+gRBI+gR+gSB+gAVG)

myHros$wVAL <- with(myHros,(gHR*pTots$HR)+(gRBI*pTots$RBI)+(gR*pTots$R)
                 +(gSB*pTots$SB))

myPros$gW <- with(myPros,W.y/sTots$W)
myPros$gK <- with(myPros,SO/sTots$K)
myPros$gSV <- with(myPros,SV/sTots$SV)
myPros$gERA <- with(myPros,(sTots$ERA - ERA.y)*(30 - Week)/350)
myPros$gVAL <- with(myPros,gW+gK+gERA+gSV)

myPros$wVAL <- with(myPros,(gW*pTots$W)+(gK*pTots$K)+(gSV*pTots$SV))

#dollar values?
myHros$dVAL <- myHros$gVAL * 260
myPros$dVAL <- myPros$gVAL * 260

arrange(FAP,-W.y)[1:10,c('Player','W.y')]
arrange(FAP,-SO)[1:10,c('Player','SO')]
arrange(FAP,-SV)[1:10,c('Player','SV')]
arrange(FAP,-HLD)[1:10,c('Player','HLD')]
arrange(FAP,-WAR)[1:10,c('Player','WAR')]
arrange(FAP,Rank)[1:10,c('Player','Rank','WAR')]

arrange(FAH,-HR.y)[1:10,c('Player','HR.y')]
arrange(FAH,-RBI.y)[1:10,c('Player','RBI.y')]
arrange(FAH,-R.y)[1:10,c('Player','R.y')]
arrange(FAH,-SB.y)[1:10,c('Player','SB.y')]
arrange(FAH,-WAR)[1:10,c('Player','WAR')]
arrange(FAH,Rank)[1:10,c('Player','Rank','WAR')]


FAH$gHR <- with(FAH,HR.y/sTots$HR)
FAH$gRBI <- with(FAH,RBI.y/sTots$RBI)
FAH$gR <- with(FAH,R.y/sTots$R)
FAH$gSB <- with(FAH,SB.y/sTots$SB)
FAH$gAVG <- with(FAH,(AVG - sTots$AVG)*(30 - Week)/270)
FAH$gVAL <- with(FAH,gHR+gRBI+gR+gSB+gAVG)

FAH$wVAL <- with(FAH,(gHR*pTots$HR)+(gRBI*pTots$RBI)+(gR*pTots$R)
                 +(gSB*pTots$SB))


FAP$gW <- with(FAP,W.y/sTots$W)
FAP$gK <- with(FAP,SO/sTots$K)
FAP$gSV <- with(FAP,SV/sTots$SV)
FAP$gHLD <- with(FAP,HLD/sTots$HLD)
FAP$gERA <- with(FAP,(sTots$ERA - ERA.y)*(30 - Week)/350)
FAP$gVAL <- with(FAP,gW+gK+gERA+gSV+gHLD)

FAP$wVAL <- with(FAP,(gW*pTots$W)+(gK*pTots$K)+(gSV*pTots$SV)+(gHLD*pTots$HLD))


arrange(FAP,-W.y)[1:10,c('Player','W.y','gVAL')]
arrange(FAP,-SO)[1:10,c('Player','SO','gVAL')]
arrange(FAP,-SV)[1:10,c('Player','SV','gVAL')]
arrange(FAP,ERA.y)[1:10,c('Player','ERA.y','gVAL')]
arrange(FAP,Rank)[1:10,c('Player','Rank','WAR','gVAL')]
arrange(FAP, -wVAL)[1:10,c('Player','Pos','gVAL','wVAL','Rank','W.y','SO','SV','HLD','ERA.y')]

allsp <- FAP %.% arrange(-gVAL) %.% 
  select(Player,Pos,gVAL,wVAL,Rank,W.y,SO,SV,HLD,ERA.y,GS.y) %.%
  filter(HLD==0,SV==0)
#  filter(GS.y>0)

allClosers <- FAP %.% arrange(-SV,-gVAL) %.% 
  select(Player,Pos,gVAL,wVAL,Rank,W.y,SO,SV,HLD,ERA.y) %.%
  filter(SV>0)



arrange(FAH,-HR.y)[1:10,c('Player','HR.y','gVAL')]
arrange(FAH,-RBI.y)[1:10,c('Player','RBI.y','gVAL')]
arrange(FAH,-R.y)[1:10,c('Player','R.y','gVAL')]
arrange(FAH,-SB.y)[1:10,c('Player','SB.y','gVAL')]
arrange(FAH,-AVG)[1:10,c('Player','AVG','gVAL')]
arrange(FAH,Rank)[1:10,c('Player','Rank','WAR','gVAL')]
arrange(FAH,-gVAL)[1:10,c('Player','Pos','gVAL','Rank','wVAL','HR.y','RBI.y','R.y','SB.y','AVG')]
arrange(FAH,-wVAL)[1:10,c('Player','Pos','gVAL','Rank','wVAL','HR.y','RBI.y','R.y','SB.y','AVG')]

TopFAH <- FAH %.% arrange(Pos,-gVAL) %.% 
  select(Player,Pos,gVAL,Rank,wVAL,HR.y,RBI.y,R.y,SB.y,AVG,HR.x,RBI.x,R.x,SB.x,BA) %.% 
  group_by(Pos) %.% filter(rank(-gVAL) <= 5)

TopFAP <- FAP %.% arrange(Pos,-gVAL) %.% 
  select(Player,Pos,gVAL,Rank,wVAL,W.y,SO,SV,HLD,ERA.y,W.x,K,S,HD,ERA.x) %.% 
  group_by(Pos) %.% filter(rank(-gVAL) <= 5)

mp <- arrange(myPros, gVAL)[,c('Player','gVAL','wVAL','Rank','W.y','SO','SV','ERA.y','W.x','K','S','ERA.x')]
mh <- arrange(myHros,gVAL)[,c('Player','gVAL','Rank','wVAL','HR.y','RBI.y','R.y','SB.y','AVG','HR.x','RBI.x','R.x','SB.x','BA')]

#Create xlsx with tabbed data
wkly <- createWorkbook()
wmh <- createSheet(wb=wkly,sheetName='My Hitters')
wmp <- createSheet(wb=wkly,sheetName='My Pitchers')
wfah <- createSheet(wb=wkly,sheetName='Top Hitters')
wfap <- createSheet(wb=wkly,sheetName='Top Pitchers')
wfasp <- createSheet(wb=wkly,sheetName='SP')
wfacl <- createSheet(wb=wkly,sheetName='Cl')

addDataFrame(x=mh,sheet=wmh)
addDataFrame(x=mp,sheet=wmp)
addDataFrame(x=TopFAH,sheet=wfah)
addDataFrame(x=TopFAP,sheet=wfap)
addDataFrame(x=allsp,sheet=wfasp)
addDataFrame(x=allClosers,sheet=wfacl)

saveWorkbook(wkly,"weeklyUpdate.xlsx")


#TBD
# Output a spreadsheet of all the tables
# Is zips much different than steamer?  Which is better?
# Automatically download projection files

prospect <- read.csv("prospects0417.csv",sep='\t',header=FALSE)
prospect <- prospect[,c('V1','V3','V7','V8','V9')]
colnames(prospect) <- c('Rank','Player','Pos','Arrival','Notes')
prospect$Player <- as.character(prospect$Player)
FAHp <- inner_join(prospect,FAH,by=c('Player'),copy=FALSE)
FAPp <- inner_join(prospect,FAP,by=c('Player'),copy=FALSE)

