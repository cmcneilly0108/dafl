# TBD
# Improve holds projections?

bullpen <- "http://www.fangraphs.com/fantasy/bullpen-report-april-25-2015/"

getd <- function(c) {
  as.numeric(unlist(r3[r3$Category==c,'ad']))
}

loadPast <- function() {
  f1 <- read.csv("fs2014.csv")
  res <- genDenoms(f1)
  eras <- f1$ERA
  avgs <- f1$AVG
  f1 <- read.csv("fs2013.csv")
  res <- rbind(res,genDenoms(f1))
  eras <- append(eras,f1$ERA)
  avgs <- append(avgs,f1$AVG)
    f1 <- read.csv("fs2012.csv")
  res <- rbind(res,genDenoms(f1))
  eras <- append(eras,f1$ERA)
  avgs <- append(avgs,f1$AVG)
  f1 <- read.csv("fs2011.csv")
  res <- rbind(res,genDenoms(f1))
  eras <- append(eras,f1$ERA)
  avgs <- append(avgs,f1$AVG)

  final <- group_by(res,Category) %>% summarize(ad = mean(denom))
  list(eras,avgs,final)
}

pgoals <- function(f) {
  f1 <- read.csv(f)
  avg <- f1$AVG[[match(12,rank(f1$AVG,ties.method='random'))]]
  hr <- f1$HR[[match(12,rank(f1$HR,ties.method='random'))]]
  rbi <- f1$RBI[[match(12,rank(f1$RBI,ties.method='random'))]]
  r <- f1$R[[match(12,rank(f1$R,ties.method='random'))]]
  sb <- f1$SB[[match(12,rank(f1$SB,ties.method='random'))]]

  w <- f1$W[[match(12,rank(f1$W,ties.method='random'))]]
  k <- f1$K[[match(12,rank(f1$K,ties.method='random'))]]
  sv <- f1$SV[[match(12,rank(f1$SV,ties.method='random'))]]
  hld <- f1$HLD[[match(12,rank(f1$HLD,ties.method='random'))]]
  era <- f1$ERA[[match(12,rank(-f1$ERA,ties.method='random'))]]

  list(hr,rbi,r,sb,avg,w,k,sv,hld,era)
}

hitSGP <- function(h) {
  atBats <- 510 * 9
  avgavg <- mean(avgs)
  hits <- round(atBats * avgavg)
  atBats <- atBats *8 / 9
  hits <- round(hits  *8 / 9)

  with(h,{pR/getd('R') + pHR/getd('HR') + pRBI/getd('RBI') + pSB/getd('SB') +
         (((hits + pH)/(atBats + pAB)) - avgavg)/getd('AVG')})
}

pitSGP <- function(p) {
  innpit <- (6 * 200) + (2 * 75)
  avgera <- mean(eras)
  eruns <-  (avgera/9) * innpit
  innpit <- innpit * 7/8
  eruns <- eruns * 7/8

  # I forget why I'm multiplying Holds by 0.2.  Were MR's getting too much $$$?
  hdiscount <- 0.2
  with(p,pW/getd('W') + pSO/getd('K') + pSV/getd('SV') + hdiscount*(pHLD/getd('HLD')) +
         ((avgera - ((eruns+pER) * (9/(innpit+pIP))))/getd('ERA'))
  )
}

swapName <- function(n){
  comma <- str_locate(n,',')
  ln <- str_sub(n,1,comma-1)
  fn <- str_sub(n,comma+2,-1)
  nn <- str_c(fn,ln,sep=" ",collapse=NULL)
  nn[1]
}

swapName2 <- function(n){
  comma <- str_locate(n,',')
  ln <- str_sub(n,1,comma-1)
  #rest <- str_sub(n,comma+2,-1)
  fn <- str_match(n,".+, (.+) [^|]+ .+")
  fn <- fn[,2]
  #space <- str_locate(rest,' ')
  #fn <- str_sub(rest,1,space-1)
  nn <- str_c(fn,ln,sep=" ",collapse=NULL)
  nn[1]
}

stripName <- function(n){
  #nm <- str_match(n,"(.+) .{1,2} |")
  #nm <- str_match(n,"(.+) [^|]+ .+")
  #nm <- str_match(n,"(.+)( [^ |]{1,2} )|")
  #nm[,2]
  nm <- strsplit(n, "[ |]+")[[1]]
  nm <- paste(head(nm,-2),collapse = ' ')
}

swapName3 <- function(n){
  # For trades file
  dash <- str_locate(n,'-')
  first <- str_sub(n,1,dash-1)
  swapName2(first)
}


pullPos <- function(n){
  n <- str_trim(n)
  p <- str_match(n,".+ .+ ([^|]+) .+")
  p <- p[,2]
  p <- ifelse((p =='P'),'RP',p)
  ifelse((p %in% c('CF','RF','LF')),'OF',p)
}

pullMLB <- function(n){
  n <- str_trim(n)
  p <- str_match(n,".+ .+ .+ (.+)")
  p <- p[,2]
}

pullTeam <- function(tn){
  tH <- filter(AllH,Team == tn)
  tH <- select(tH,-Team)
  tP <- filter(AllP,Team == tn)
  tP <- select(tP,-Team)
  tP <- tP %>% arrange(-hotscore) %>%
    select(Player,Pos,pDFL,pSGP,Rank,pW,pSO,pHLD,pSV,pERA,pK.9,pFIP,W,K,HD,S,ERA,hotscore,Injury,Expected.Return)
  tH <- tH %>% arrange(-hotscore) %>%
    select(Player,Pos,pDFL,pSGP,Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA,hotscore,Injury,Expected.Return)
  list(tH,tP)
}

addSheet <- function(l,w) {
  df <- l[[2]]
  csTableColNames <- CellStyle(w) + Alignment(wrapText=TRUE, h="ALIGN_CENTER")
  sht <- createSheet(wb=w,sheetName=l[[1]])
  addDataFrame(x=df,sheet=sht,colStyle=l[[3]],colnamesStyle=csTableColNames)
  cols <- l[[4]]
  lapply(cols,function(x){autoSizeColumn(sht, x)})
  printSetup(sht,landscape=TRUE,fitWidth=1,fitHeight=5)
}

# Create final stats file from past years total files
#x <- read.csv("2013FinalStandings.txt",skip=202,nrows=16)
#x <- select(x,-Points)
#f2013 <- inner_join(f2013,x,by=c('Team'))
#write.csv(f2013,"fs2013.csv")

getMult <- function(pts) {
  pR <- rank(pts)
  l <- lm(pR ~ pts, na.action=na.exclude)
  1/coefficients(l)[2]
}

genDenoms <- function(df) {
  f2 <- select(df,-X,-Team)
  v <- as.data.frame(apply(f2,2,getMult))
  colnames(v) <- c('denom')
  v$Category <- rownames(v)
  rownames(v) <- NULL
  v
}


preDollars <- function(ihitters,ipitchers,prot=data.frame(),ratio=1,dadj=0,padj=0) {
  # GENERATE DFL dollar values for all players
  #Set parameters
  nteams <- 15
  tdollars <- nteams * (260 +dadj) * ratio
  # 66/34 split - just guessing
  # books say 69/31, but that seems high for DAFL
  pdollars <- round(tdollars*0.36)
  hdollars <- tdollars - pdollars
  # 13/12 hitters/pitchers based on rosters on 5/29/14
  nhitters <- 12
  npitchers <- 13
  thitters <- (nhitters * nteams) + padj
  tpitchers <- (npitchers * nteams) + padj

  # Remove protected players and change counts and dollars
  if (nrow(prot)>0) {
    ih2 <- anti_join(ihitters,prot,by=c('Player'),copy=FALSE)
    ip2 <- anti_join(ipitchers,prot,by=c('Player'),copy=FALSE)
    tpitchers <- tpitchers - nrow(prot[prot$Pos %in% c('SP','MR','CL'),])
    thitters <- thitters - nrow(prot[!(prot$Pos %in% c('SP','MR','CL')),])
    pdollars <- pdollars - sum(prot[prot$Pos %in% c('SP','MR','CL'),'Salary'])
    hdollars <- hdollars - sum(prot[!(prot$Pos %in% c('SP','MR','CL')),'Salary'])
  } else {
    ih2 <- ihitters
    ip2 <- ipitchers
  }

  # Only value a certain number of players
  bhitters <- filter(ih2,rank(-pSGP) <= thitters)

  hitSGP <- round(sum(bhitters$pSGP))
  bpitchers <- filter(ip2,rank(-pSGP) <= tpitchers)
  pitSGP <- round(sum(bpitchers$pSGP))
  hsgpd <- hdollars/hitSGP
  psgpd <- pdollars/pitSGP
  # Create dollar amounts
  bhitters$pDFL <- bhitters$pSGP * hsgpd
  bpitchers$pDFL <- bpitchers$pSGP * psgpd
  # find min $, subtract from everyone, then multiply everyone by %diff
  # Normalize for auction - three iterations
  hmin <- min(bhitters$pDFL) - 1
  hlost <- hmin * thitters
  bhitters$pDFL <- (bhitters$pDFL - hmin) * (hdollars/(hdollars - hlost))
  hmin <- min(bhitters$pDFL) - 1
  hlost <- hmin * thitters
  bhitters$pDFL <- (bhitters$pDFL - hmin) * (hdollars/(hdollars - hlost))
  hmin <- min(bhitters$pDFL) - 1
  hlost <- hmin * thitters
  bhitters$pDFL <- (bhitters$pDFL - hmin) * (hdollars/(hdollars - hlost))

  # count C's
  bhitters <- arrange(bhitters,-pDFL)
  nc <- nrow(filter(bhitters,Pos=='C'))
  bh2 <- head(bhitters,-(nteams-nc))
  ac <- filter(ihitters,Pos=='C') %>% arrange(-pSGP)
  bh3 <- ac[nc+1:(nteams-nc),]
  bh3$pDFL <- 1
  bhitters <- rbind(bh2,bh3)



  bhitters <- select(bhitters,playerid,pDFL)
  bpitchers <- select(bpitchers,playerid,pDFL)

  pmin <- min(bpitchers$pDFL) - 1
  plost <- pmin * tpitchers
  bpitchers$pDFL <- (bpitchers$pDFL - pmin) * (pdollars/(pdollars - plost))
  pmin <- min(bpitchers$pDFL) - 1
  plost <- pmin * tpitchers
  bpitchers$pDFL <- (bpitchers$pDFL - pmin) * (pdollars/(pdollars - plost))
  pmin <- min(bpitchers$pDFL) - 1
  plost <- pmin * tpitchers
  bpitchers$pDFL <- (bpitchers$pDFL - pmin) * (pdollars/(pdollars - plost))

  list(bhitters,bpitchers)
}

preLPP <- function(ihitters,ipitchers,prot=data.frame(),ratio=1,dadj=0,padj=0) {
  #Set parameters
  nteams <- 15
  tdollars <- nteams * (260 +dadj) * ratio
  # 66/34 split - just guessing
  # books say 69/31, but that seems high for DAFL
  pdollars <- round(tdollars*0.36)
  hdollars <- tdollars - pdollars
  # 13/12 hitters/pitchers based on rosters on 5/29/14
  nhitters <- 12
  npitchers <- 13
  thitters <- (nhitters * nteams) + padj
  tpitchers <- (npitchers * nteams) + padj
  ct <- 0

  while (ct < 6) {
    toph <- head(ihitters,thitters)
    topp <- head(ipitchers,tpitchers)
    mHR <- mean(toph$pHR)
    sdHR <- sd(toph$pHR)
    mR <- mean(toph$pR)
    sdR <- sd(toph$pR)
    mSB <- mean(toph$pSB)
    sdSB <- sd(toph$pSB)
    mRBI <- mean(toph$pRBI)
    sdRBI <- sd(toph$pRBI)

    mW <- mean(topp$pW)
    sdW <- sd(topp$pW)
    mSO <- mean(topp$pSO)
    sdSO <- sd(topp$pSO)
    mHLD <- mean(topp$pHLD)
    sdHLD <- sd(topp$pHLD)
    mSV <- mean(topp$pSV)
    sdSV <- sd(topp$pSV)

    mAvg <- mean(toph$pAVG)
    sdAvg <- sd(toph$pAVG)
    ihitters <- mutate(ihitters,xH = pH-(pAB * mAvg))
    toph <- mutate(toph,xH = pH-(pAB * mAvg))
    mxH <- mean(toph$xH)
    sdxH <- sd(toph$xH)

    mERA <- mean(topp$pERA)
    sdERA <- sd(topp$pERA)
    ipitchers <- mutate(ipitchers,xER = (pIP * mERA/9)-pER)
    topp <- mutate(topp,xER = (pIP * mERA/9)-pER)
    mxER <- mean(topp$xER)
    sdxER <- sd(topp$xER)

    ihitters <- mutate(ihitters,zHR=(pHR-mHR)/sdHR,zR=(pR-mR)/sdR,zRBI=(pRBI-mRBI)/sdRBI,
                       zSB=(pSB-mSB)/sdSB,zxH=(xH-mxH)/sdxH)
    ihitters <- mutate(ihitters,zScore=zHR+zR+zRBI+zSB+zxH)
    ihitters <- arrange(ihitters,-zScore)

    ipitchers <- mutate(ipitchers,zW=(pW-mW)/sdW,zSO=(pSO-mSO)/sdSO,zHLD=(pHLD-mHLD)/sdHLD,
                       zSV=(pSV-mSV)/sdSV,zxER=(xER-mxER)/sdxER)
    ipitchers <- mutate(ipitchers,zScore=zW+zSO+(0.4*zHLD)+zSV+zxER)
    ipitchers <- arrange(ipitchers,-zScore)

    ct <- ct + 1
  }
  # Add the total thitter value to everyone
  ihitters <- head(ihitters,thitters)
  ihitters$zScore <- ihitters$zScore - last(ihitters$zScore)
  # Add pitchers
  ipitchers <- head(ipitchers,tpitchers)
  ipitchers$zScore <- ipitchers$zScore - last(ipitchers$zScore)

  # remove protected players
  if (nrow(prot)>0) {
    ih2 <- anti_join(ihitters,prot,by=c('Player'),copy=FALSE)
    ip2 <- anti_join(ipitchers,prot,by=c('Player'),copy=FALSE)
    tpitchers <- tpitchers - nrow(prot[prot$Pos %in% c('SP','MR','CL'),])
    thitters <- thitters - nrow(prot[!(prot$Pos %in% c('SP','MR','CL')),])
    pdollars <- pdollars - sum(prot[prot$Pos %in% c('SP','MR','CL'),'Salary'])
    hdollars <- hdollars - sum(prot[!(prot$Pos %in% c('SP','MR','CL')),'Salary'])
  } else {
    ih2 <- ihitters
    ip2 <- ipitchers
  }
  # Calculate DFL
  tvalue <- sum(ih2$zScore)
  ih2$zDFL <- (ih2$zScore / tvalue) * hdollars + 1
  tvalue <- sum(ip2$zScore)
  ip2$zDFL <- (ip2$zScore / tvalue) * pdollars + 1

  bhitters <- select(ih2,playerid,zDFL)
  bpitchers <- select(ip2,playerid,zDFL)

  list(bhitters,bpitchers)
}

postDollars <- function(ihitters,ipitchers) {
  # GENERATE DFL dollar values for all players
  #Set parameters
  nteams <- 15
  tdollars <- nteams * (260 + 75)
  # 66/34 split - just guessing
  # books say 69/31, but that seems high for DAFL
  pdollars <- round(tdollars*0.36)
  hdollars <- tdollars - pdollars
  # 13/12 hitters/pitchers based on rosters on 5/29/14

  # Only value a certain number of players
  hitSGP <- round(sum(ihitters$pSGP))
  pitSGP <- round(sum(ipitchers$pSGP))
  hsgpd <- hdollars/hitSGP
  psgpd <- pdollars/pitSGP
  # Create dollar amounts
  ihitters$pDFL <- ihitters$pSGP * hsgpd
  ipitchers$pDFL <- ipitchers$pSGP * psgpd

  list(ihitters,ipitchers)
}

calcInflation <- function(prot) {
  # GENERATE DFL dollar values for all players
  #Set parameters
  nteams <- 15
  tdollars <- nteams * 260
  # 66/34 split - just guessing
  # books say 69/31, but that seems high for DAFL
  pdollars <- round(tdollars*0.36)
  hdollars <- tdollars - pdollars
  # 13/12 hitters/pitchers based on rosters on 5/29/14
  nhitters <- 12
  npitchers <- 13
  thitters <- (nhitters * nteams)
  tpitchers <- (npitchers * nteams)
  oh <- hdollars/thitters
  op <- pdollars/tpitchers

  pp <- filter(prot,Pos %in% c('SP','MR','CL'))
  ph <- filter(prot,!(Pos %in% c('SP','MR','CL')))

  tpitchers <- tpitchers - nrow(pp)
  thitters <- thitters - nrow(ph)
  pdollars <- pdollars - sum(pp$Salary)
  hdollars <- hdollars - sum(ph$Salary)
  nh <- hdollars/thitters
  np <- pdollars/tpitchers

  list(nh/oh,np/op)
}


read.fg <- function(fn) {
  m2 <- select(master,playerid,Pos,MLB,birth_year)
  df <- read.csv(fn,stringsAsFactors=FALSE)
  colnames(df) <- str_c('p',colnames(df))
  df <- rename(df,playerid=pplayerid,Player=pName)
  df <- left_join(df,m2,by=c('playerid'),copy=FALSE)
  df$birth_year <- replace(df$birth_year,is.na(df$birth_year),2010)
  df <- mutate(df,Age=year(Sys.time())-birth_year)
  dfh <- anti_join(df,m2,by=c('playerid'),copy=FALSE)
  df$playerid <- ifelse(df$playerid %in% dfh$playerid,df$Player,df$playerid)
  df
}

read.cbs <- function(fn) {
  df <- read.csv(fn,skip=1,stringsAsFactors=FALSE)
  df <- mutate(df, Pos = pullPos(Player))
  df <- mutate(df, MLB = pullMLB(Player))
  df$Player <- unlist(lapply(df$Player,stripName))
  # Team abbreviations are not the same - find all discrepancies WAS->WSH
  df$MLB <- replace(df$MLB,df$MLB=='WAS','WSH')
  df$MLB <- replace(df$MLB,df$MLB=='CHW','CWS')
  addPlayerid(df)
}

addPlayerid <- function(df) {
  m2 <- select(master,-Pos,-Player) %>% rename(Player=cbs_name)
  # Merge with team
  gfull <- inner_join(df, m2,by=c('Player','MLB'))
  dfleft <- anti_join(df, m2,by=c('Player','MLB'))
  m2 <- anti_join(m2,df,by=c('Player','MLB'))

  # Merge rest with only name
  gname <- left_join(dfleft, m2,by=c('Player'))
  gname <- select(gname,-MLB.x) %>% rename(MLB=MLB.y)

  rooks <- read.csv('2015RookieIDs.csv',stringsAsFactors=FALSE) %>% select(-X)
  gname <- left_join(gname,rooks,by=c('Player'))

  gname <- rename(gname,playerid = playerid.x)

  #gname$playerid <- ifelse(is.na(gname$playerid),ifelse(is.na(gname$playerid.y),gname$Player,gname$playerid.y),
  #                         gname$playerid)
  gname <- select(gname,-playerid.y)

  final <- rbind(gfull,gname)
  final$playerid <- ifelse(is.na(final$playerid),final$Player,final$playerid)
  final$playerid <- ifelse(str_length(final$playerid)==0,final$Player,final$playerid)
  final
}


read.2014cbs <- function(fn) {
  df <- read.csv(fn,skip=1,stringsAsFactors=FALSE)
  df <- mutate(df, Pos = pullPos(Player))
  df <- mutate(df, MLB = pullMLB(Player))
  #df$Player <- unlist(lapply(df$Player,stripName))
  df$Player <- unlist(lapply(df$Player,swapName2))
  # Team abbreviations are not the same - find all discrepancies WAS->WSH
  df$MLB <- replace(df$MLB,df$MLB=='WAS','WSH')
  df$MLB <- replace(df$MLB,df$MLB=='CHW','CWS')
  addPlayerid(df)
}

read.inseasonrecap <- function(fn,pos) {
  m2 <- select(master,-Pos,-Player) %>% rename(Player=cbs_name)
  df <- read.xlsx(fn,pos,stringsAsFactors=FALSE)
  colnames(df) <- str_c('p',colnames(df))
  df <- rename(df,Player=pPlayer)
  df <- mutate(df, Pos = pullPos(Player))
  df <- mutate(df, MLB = pullMLB(Player))
  df$Player <- unlist(lapply(df$Player,swapName2))
  # Team abbreviations are not the same - find all discrepancies WAS->WSH
  df$MLB <- replace(df$MLB,df$MLB=='WAS','WSH')
  df$MLB <- replace(df$MLB,df$MLB=='CHW','CWS')
  # Merge with team
  gfull <- inner_join(df, m2,by=c('Player','MLB'))
  dfleft <- anti_join(df, m2,by=c('Player','MLB'))
  # Merge rest with only name
  gname <- left_join(dfleft, m2,by=c('Player'))
  gname <- select(gname,-MLB.x) %>% rename(MLB=MLB.y)
  gname$playerid <- ifelse(is.na(gname$playerid),gname$Player,gname$playerid)
  rbind(gfull,gname)
}

predictHolds <- function(pitchers) {
  #Need to predict holds
  # Step 2 - copy over previous year's totals
  lyp <- read.2014cbs("AllP2014.csv")
  lyp <- select(lyp,playerid,lyHLD=HD)
  pitchers <- left_join(pitchers,lyp,by=c('playerid'))

  # Step 3 - use last year's totals plus fangraphs projected role
  # Use last year's data, if now a closer, set to 0, if true setup - make sure to up number
  # http://www.fangraphs.com/fantasy/bullpen-report-september-24-2014/
  c <- readHTMLTable(bullpen,stringsAsFactors=F)
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
  pitchers$pHLD <- with(pitchers,ifelse((pRole==10 | pSV > 10 | pGS > 10),0,ifelse(pRole==5 & lyHLD < 25,25,lyHLD)))
  pitchers$pHLD <- ifelse(is.na(pitchers$pHLD),0,pitchers$pHLD)
  return(pitchers)
}

aRPV <- function(p,pl=15) {
  top <- filter(p,rank(-SGP) <= pl)
  median(top$SGP)
}

# Year End Totals
sTots <- list()

l1 <- loadPast()
eras <- l1[[1]]
avgs <- l1[[2]]
r3 <- l1[[3]]
# Load Master file
master <- read.csv("master.csv",stringsAsFactors=FALSE)
#master <- read.csv("master150801.csv",stringsAsFactors=FALSE)
master <- rename(master,playerid=fg_id,Pos = mlb_pos,MLB=mlb_team,Player=mlb_name)

calcGoals <- function(p,h,targets,t) {
  lcht <- h %>% filter(Team == t) %>%
    summarize(HR = sum(pHR),RBI=sum(pRBI),R=sum(pR),SB=sum(pSB))
  lcht <- melt(lcht) %>% rename(statistic = variable, collected = value)
  hg <- inner_join(lcht,targets) %>% mutate(needed=goal-collected,pc = (collected/goal))

  lcpt <- p %>% filter(Team == t) %>%
    summarize(W = sum(pW),HLD=sum(pHLD),K=sum(pSO),SV=sum(pSV))
  lcpt <- melt(lcpt) %>% rename(statistic = variable, collected = value)
  pg <- inner_join(lcpt,targets) %>% mutate(needed=goal-collected,pc = (collected/goal))

  gmet <- rbind(hg,pg) %>% arrange(pc)
}

hotScores <- function(toph,topp) {
  toph <- filter(toph,AB>0)
  topp <- filter(topp,INN>0)
  mHR <- mean(toph$HR)
  sdHR <- sd(toph$HR)
  mR <- mean(toph$R)
  sdR <- sd(toph$R)
  mSB <- mean(toph$SB)
  sdSB <- sd(toph$SB)
  mRBI <- mean(toph$RBI)
  sdRBI <- sd(toph$RBI)

  mW <- mean(topp$W)
  sdW <- sd(topp$W)
  mSO <- mean(topp$K)
  sdSO <- sd(topp$K)
  mHLD <- mean(topp$HD)
  sdHLD <- sd(topp$HD)
  mSV <- mean(topp$S)
  sdSV <- sd(topp$S)

  mAvg <- mean(toph$BA)
  sdAvg <- sd(toph$BA)
  toph <- mutate(toph,xH = H-(AB * mAvg))
  mxH <- mean(toph$xH)
  sdxH <- sd(toph$xH)

  mERA <- mean(topp$ERA)
  sdERA <- sd(topp$ERA)
  topp <- mutate(topp,xER = (INN * mERA/9)-(INN * ERA/9))
  mxER <- mean(topp$xER)
  sdxER <- sd(topp$xER)

  toph <- mutate(toph,zHR=(HR-mHR)/sdHR,zR=(R-mR)/sdR,zRBI=(RBI-mRBI)/sdRBI,
                     zSB=(SB-mSB)/sdSB,zxH=(xH-mxH)/sdxH)
  toph <- mutate(toph,zScore=zHR+zR+zRBI+zSB+zxH)
  toph <- arrange(toph,-zScore)

  topp <- mutate(topp,zW=(W-mW)/sdW,zSO=(K-mSO)/sdSO,zHLD=(HD-mHLD)/sdHLD,
                      zSV=(S-mSV)/sdSV,zxER=(xER-mxER)/sdxER)
  topp <- mutate(topp,zScore=zW+zSO+(0.4*zHLD)+zSV+zxER)
  topp <- arrange(topp,-zScore)

  # Add the total thitter value to everyone
  #toph <- head(toph,thitters)
  toph$zScore <- toph$zScore - last(toph$zScore)
  # Add pitchers
  #topp <- head(topp,tpitchers)
  topp$zScore <- topp$zScore - last(topp$zScore)

  ih2 <- toph
  ip2 <- topp

  bhitters <- select(ih2,playerid,zScore)
  bpitchers <- select(ip2,playerid,zScore)

  list(bhitters,bpitchers)
}


# This hangs and never completes
zScores <- function(toph,topp) {
  toph <- filter(toph,pAB>0)
  topp <- filter(topp,pIP>0)
  mHR <- mean(toph$pHR)
  sdHR <- sd(toph$pHR)
  mR <- mean(toph$pR)
  sdR <- sd(toph$pR)
  mSB <- mean(toph$pSB)
  sdSB <- sd(toph$pSB)
  mRBI <- mean(toph$pRBI)
  sdRBI <- sd(toph$pRBI)

  mW <- mean(topp$pW)
  sdW <- sd(topp$pW)
  mSO <- mean(topp$pSO)
  sdSO <- sd(topp$pSO)
  mHLD <- mean(topp$pHLD)
  sdHLD <- sd(topp$pHLD)
  mSV <- mean(topp$pSV)
  sdSV <- sd(topp$pSV)

  mAvg <- mean(toph$pAVG)
  sdAvg <- sd(toph$pAVG)
  toph <- mutate(toph,xH = pH-(pAB * mAvg))
  mxH <- mean(toph$xH)
  sdxH <- sd(toph$xH)

  mERA <- mean(topp$pERA)
  sdERA <- sd(topp$pERA)
  topp <- mutate(topp,xER = (pIP * mERA/9)-(pIP * pERA/9))
  mxER <- mean(topp$xER)
  sdxER <- sd(topp$xER)

  toph <- mutate(toph,zHR=(pHR-mHR)/sdHR,zR=(pR-mR)/sdR,zRBI=(pRBI-mRBI)/sdRBI,
                 zSB=(pSB-mSB)/sdSB,zxH=(xH-mxH)/sdxH)
  toph <- mutate(toph,zScore=zHR+zR+zRBI+zSB+zxH)
  toph <- arrange(toph,-zScore)


  topp <- mutate(topp,zW=(pW-mW)/sdW,zSO=(pSO-mSO)/sdSO,zHLD=(pHLD-mHLD)/sdHLD,
                 zSV=(pSV-mSV)/sdSV,zxER=(xER-mxER)/sdxER)
  topp <- mutate(topp,zScore=zW+zSO+(0.4*zHLD)+zSV+zxER)
  topp <- arrange(topp,-zScore)

  # Add the total thitter value to everyone
  #toph <- head(toph,thitters)
  toph$zScore <- toph$zScore - last(toph$zScore)
  # Add pitchers
  #topp <- head(topp,tpitchers)
  topp$zScore <- topp$zScore - last(topp$zScore)

  ih2 <- toph
  ip2 <- topp

  bhitters <- select(ih2,playerid,zScore)
  bpitchers <- select(ip2,playerid,zScore)

  list(bhitters,bpitchers)
}

zScoresST <- function(ihitters,ipitchers) {

  toph <- ihitters
  topp <- ipitchers
  mHR <- mean(toph$pHR)
  sdHR <- sd(toph$pHR)
  mR <- mean(toph$pR)
  sdR <- sd(toph$pR)
  mSB <- mean(toph$pSB)
  sdSB <- sd(toph$pSB)
  mRBI <- mean(toph$pRBI)
  sdRBI <- sd(toph$pRBI)

  mW <- mean(topp$pW)
  sdW <- sd(topp$pW)
  mSO <- mean(topp$pSO)
  sdSO <- sd(topp$pSO)
  mHLD <- mean(topp$pHLD)
  sdHLD <- sd(topp$pHLD)
  mSV <- mean(topp$pSV)
  sdSV <- sd(topp$pSV)

  mAvg <- mean(toph$pAVG)
  sdAvg <- sd(toph$pAVG)
  ihitters <- mutate(ihitters,xH = pH-(pAB * mAvg))
  toph <- mutate(toph,xH = pH-(pAB * mAvg))
  mxH <- mean(toph$xH)
  sdxH <- sd(toph$xH)

  mERA <- mean(topp$pERA)
  sdERA <- sd(topp$pERA)
  ipitchers <- mutate(ipitchers,xER = (pIP * mERA/9)-pER)
  topp <- mutate(topp,xER = (pIP * mERA/9)-pER)
  mxER <- mean(topp$xER)
  sdxER <- sd(topp$xER)

  ihitters <- mutate(ihitters,zHR=(pHR-mHR)/sdHR,zR=(pR-mR)/sdR,zRBI=(pRBI-mRBI)/sdRBI,
                     zSB=(pSB-mSB)/sdSB,zxH=(xH-mxH)/sdxH)
  ihitters <- mutate(ihitters,zScore=zHR+zR+zRBI+zSB+zxH)
  ihitters <- arrange(ihitters,-zScore)

  ipitchers <- mutate(ipitchers,zW=(pW-mW)/sdW,zSO=(pSO-mSO)/sdSO,zHLD=(pHLD-mHLD)/sdHLD,
                      zSV=(pSV-mSV)/sdSV,zxER=(xER-mxER)/sdxER)
  ipitchers <- mutate(ipitchers,zScore=zW+zSO+(0.4*zHLD)+zSV+zxER)
  ipitchers <- arrange(ipitchers,-zScore)

  # Add the total thitter value to everyone
  #toph <- head(toph,thitters)
  #toph$zScore <- toph$zScore - last(toph$zScore)
  # Add pitchers
  #topp <- head(topp,tpitchers)
  #topp$zScore <- topp$zScore - last(topp$zScore)

  bhitters <- select(ihitters,playerid,zScore)
  bpitchers <- select(ipitchers,playerid,zScore)

  list(bhitters,bpitchers)
}
