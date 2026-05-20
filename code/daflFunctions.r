# # TBD
# # Improve holds projections?
#

# Load required libraries
library("dplyr")
library("stringr")
library("lubridate")
library("jsonlite")
library("xml2")
library("rvest")
library("httr")
library("RSQLite")

# When bumping cyear/lastyear each season, also update YEAR in
# code/run_data_loads.sh (the daily launchd data-load job hardcodes the year).
cyear <- "2026"
lastyear <- "2025"
# it's been .75 for 2024 and 2025, but .89 in 2023
auctionROI <- 0.80
hpratio <- .38

# 
# bullpen <- "http://www.fangraphs.com/fantasy/bullpen-report-april-25-2015/"
nicks <- read.csv("../data/nicknames.csv",stringsAsFactors=FALSE)
nicks <- mutate(nicks,Avail=str_sub(Team,1,6))

getd <- function(c) {
  as.numeric(unlist(r3[r3$Category==c,'ad']))
}

# Skipping 2020 data since it would through off the averages
loadPast <- function() {
  f1 <- read.csv("../data/fs2025.csv")
  res <- genDenoms(f1)
  eras <- f1$ERA
  avgs <- f1$AVG
  f1 <- read.csv("../data/fs2023.csv")
  res <- rbind(res,genDenoms(f1))
  eras <- append(eras,f1$ERA)
  avgs <- append(avgs,f1$AVG)
  f1 <- read.csv("../data/fs2022.csv")
  res <- rbind(res,genDenoms(f1))
  eras <- append(eras,f1$ERA)
  avgs <- append(avgs,f1$AVG)
  f1 <- read.csv("../data/fs2021.csv")
  res <- rbind(res,genDenoms(f1))
  eras <- append(eras,f1$ERA)
  avgs <- append(avgs,f1$AVG)

  final <- group_by(res,Category) %>% dplyr::summarize(ad = mean(denom))
  list(eras,avgs,final)
}

pgoals <- function(f) {
  f1 <- read.csv(f, encoding="UTF-8")
  avg <- f1$AVG[[match(11,rank(f1$AVG,ties.method='random'))]]
  hr <- f1$HR[[match(11,rank(f1$HR,ties.method='random'))]]
  rbi <- f1$RBI[[match(11,rank(f1$RBI,ties.method='random'))]]
  r <- f1$R[[match(11,rank(f1$R,ties.method='random'))]]
  sb <- f1$SB[[match(11,rank(f1$SB,ties.method='random'))]]

  w <- f1$W[[match(11,rank(f1$W,ties.method='random'))]]
  k <- f1$K[[match(11,rank(f1$K,ties.method='random'))]]
  sv <- f1$SV[[match(11,rank(f1$SV,ties.method='random'))]]
  hld <- f1$HLD[[match(11,rank(f1$HLD,ties.method='random'))]]
  era <- f1$ERA[[match(11,rank(-f1$ERA,ties.method='random'))]]

  list(hr,rbi,r,sb,avg,w,k,sv,hld,era)
}

hitSGP <- function(h) {
#  atBats <- 510 * 9
# discount for partial season
  atBats <- 189 * 9
  avgavg <- mean(avgs)
  hits <- round(atBats * avgavg)
  atBats <- atBats *8 / 9
  hits <- round(hits  *8 / 9)

  with(h,{pR/getd('R') + pHR/getd('HR') + pRBI/getd('RBI') + pSB/getd('SB') +
         (((hits + pH)/(atBats + pAB)) - avgavg)/getd('AVG')})
  #with(h,{pR/getd('R')})
}

pitSGP <- function(p) {
#  innpit <- (6 * 200) + (2 * 75)
# discount for partial season
  innpit <- (6 * 74) + (2 * 28)
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

# swapName2 <- function(n){
#   comma <- str_locate(n,',')
#   ln <- str_sub(n,1,comma-1)
#   #rest <- str_sub(n,comma+2,-1)
#   fn <- str_match(n,".+, (.+) [^|]+ .+")
#   fn <- fn[,2]
#   #space <- str_locate(rest,' ')
#   #fn <- str_sub(rest,1,space-1)
#   nn <- str_c(fn,ln,sep=" ",collapse=NULL)
#   nn[1]
# }

stripName <- function(n){
  #nm <- str_match(n,"(.+) .{1,2} |")
  #nm <- str_match(n,"(.+) [^|]+ .+")
  #nm <- str_match(n,"(.+)( [^ |]{1,2} )|")
  #nm[,2]
  nm <- strsplit(n, "[ |]+")[[1]]
  nm <- paste(head(nm,-2),collapse = ' ')
}

# stripNameP <- function(nm){
#   #nm <- str_match(n,"(.+) .{1,2} |")
#   #nm <- str_match(n,"(.+) [^|]+ .+")
#   #nm <- str_match(n,"(.+)( [^ |]{1,2} )|")
#   #nm[,2]
#   nm <- strsplit(nm," \\(")[[1]][[1]]
#   #nm <- paste(head(nm,-2),collapse = ' ')
# }
# 
# swapName <- function(n){
#   dash <- str_locate(n,',')
#   dash <- dash[[1]]
#   last <- str_sub(n,1,dash-1)
#   first <- str_sub(n,dash+2)
#   paste(c(first,last),collapse = ' ')
# }

swapName3 <- function(n){
  # For trades file
  dash <- str_locate(n,'-')
  first <- str_sub(n,1,dash-1)[1]
  trimws(first)
  #stripName(first)
}

swapName4 <- function(n){
  # For trades file
  dash <- str_locate(n,'-')
  str_sub(n,1,dash-2)[[1]]
  #  first <- str_sub(n,1,dash-1)
#  stripName(first)
}

tradeFrom <- function(n){
  # For trades file
  #from <- str_locate(n,'from ')
  #team <- str_sub(n,from+5,length(n))
  team <- unlist(str_split(n,"- Traded from "))
  t2 <- team[length(team)]
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
  tP <- left_join(tP,rrc)
  tP <- select(tP,-Team)
  tP <- tP %>% arrange(-hotscore) %>%
    select(Player,Pos,Age,pDFL,pSGP,Rank,Salary,Contract,'Pitching+',pW,pSO,pHLD,pSV,pERA,`pK/9`,pFIP,W,K,HD,S,ERA,hotscore,twostarts,Injury,Expected.Return,Role,Tags)
  tH <- tH %>% arrange(-hotscore) %>%
    select(Player,Pos,Age,pDFL,pSGP,Rank,Salary,Contract,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,AVG,hotscore,Injury,Expected.Return)
  list(tH,tP)
}

# addSheet <- function(l,w) {
#   df <- l[[2]]
#   csTableColNames <- CellStyle(w) + Alignment(wrapText=TRUE, h="ALIGN_CENTER")
#   sht <- createSheet(wb=w,sheetName=l[[1]])
#   addDataFrame(x=df,sheet=sht,colStyle=l[[3]],colnamesStyle=csTableColNames)
#   cols <- l[[4]]
#   lapply(cols,function(x){autoSizeColumn(sht, x)})
#   printSetup(sht,landscape=TRUE,fitWidth=1,fitHeight=5)
# }

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

# 
# preDollars <- function(ihitters,ipitchers,prot=data.frame(),ratio=1,dadj=0,padj=0) {
#   # GENERATE DFL dollar values for all players
#   #Set parameters
#   nteams <- 16
#   tdollars <- nteams * (260 +dadj) * ratio
#   # 66/34 split - just guessing
#   # books say 69/31, but that seems high for DAFL
#   pdollars <- round(tdollars*hpratio)
#   hdollars <- tdollars - pdollars
#   # 13/12 hitters/pitchers based on rosters on 5/29/14
#   nhitters <- 12
#   npitchers <- 13
#   thitters <- (nhitters * nteams) + padj
#   tpitchers <- (npitchers * nteams) + padj
# 
#   # Remove protected players and change counts and dollars
#   if (nrow(prot)>0) {
#     ih2 <- anti_join(ihitters,prot,by=c('Player'),copy=FALSE)
#     ip2 <- anti_join(ipitchers,prot,by=c('Player'),copy=FALSE)
#     tpitchers <- tpitchers - nrow(prot[prot$Pos %in% c('SP','MR','CL'),])
#     thitters <- thitters - nrow(prot[!(prot$Pos %in% c('SP','MR','CL')),])
#     pdollars <- pdollars - sum(prot[prot$Pos %in% c('SP','MR','CL'),'Salary'])
#     hdollars <- hdollars - sum(prot[!(prot$Pos %in% c('SP','MR','CL')),'Salary'])
#   } else {
#     ih2 <- ihitters
#     ip2 <- ipitchers
#   }
# 
#   # Only value a certain number of players
#   bhitters <- filter(ih2,rank(-pSGP) <= thitters)
# 
#   hitSGP <- round(sum(bhitters$pSGP))
#   bpitchers <- filter(ip2,rank(-pSGP) <= tpitchers)
#   pitSGP <- round(sum(bpitchers$pSGP))
#   hsgpd <- hdollars/hitSGP
#   psgpd <- pdollars/pitSGP
#   # Create dollar amounts
#   bhitters$pDFL <- bhitters$pSGP * hsgpd
#   bpitchers$pDFL <- bpitchers$pSGP * psgpd
#   # find min $, subtract from everyone, then multiply everyone by %diff
#   # Normalize for auction - three iterations
#   hmin <- min(bhitters$pDFL) - 1
#   hlost <- hmin * thitters
#   bhitters$pDFL <- (bhitters$pDFL - hmin) * (hdollars/(hdollars - hlost))
#   hmin <- min(bhitters$pDFL) - 1
#   hlost <- hmin * thitters
#   bhitters$pDFL <- (bhitters$pDFL - hmin) * (hdollars/(hdollars - hlost))
#   hmin <- min(bhitters$pDFL) - 1
#   hlost <- hmin * thitters
#   bhitters$pDFL <- (bhitters$pDFL - hmin) * (hdollars/(hdollars - hlost))
# 
#   # count C's
#   bhitters <- arrange(bhitters,-pDFL)
#   nc <- nrow(filter(bhitters,Pos=='C'))
#   bh2 <- head(bhitters,-(nteams-nc))
#   ac <- filter(ihitters,Pos=='C') %>% arrange(-pSGP)
#   bh3 <- ac[nc+1:(nteams-nc),]
#   bh3$pDFL <- 1
#   bhitters <- rbind(bh2,bh3)
# 
# 
# 
#   bhitters <- select(bhitters,playerid,pDFL)
#   bpitchers <- select(bpitchers,playerid,pDFL)
# 
#   pmin <- min(bpitchers$pDFL) - 1
#   plost <- pmin * tpitchers
#   bpitchers$pDFL <- (bpitchers$pDFL - pmin) * (pdollars/(pdollars - plost))
#   pmin <- min(bpitchers$pDFL) - 1
#   plost <- pmin * tpitchers
#   bpitchers$pDFL <- (bpitchers$pDFL - pmin) * (pdollars/(pdollars - plost))
#   pmin <- min(bpitchers$pDFL) - 1
#   plost <- pmin * tpitchers
#   bpitchers$pDFL <- (bpitchers$pDFL - pmin) * (pdollars/(pdollars - plost))
# 
#   list(bhitters,bpitchers)
# }
# 
preLPP <- function(ihitters,ipitchers,prot=data.frame(),ratio=1,dadj=0,padj=0) {
  #Set parameters
  #Used by draftGuide.r
  #Used by protectionList.r
  #used by inSeasonPulse.r

  # 13/12 hitters/pitchers based on rosters on 5/29/14
  nhitters <- 13
  npitchers <- 12
  cap <- 260
# Setting for Protection and Draft - only focus on top 19 players
  # nhitters <- 10
  # npitchers <- 9
  # cap <- 254
  
  nteams <- 13
  tdollars <- nteams * (cap +dadj) * ratio

  # What happens when we lop of the last 5 spots
  # I can't remember why I'm doing this?
  #nhitters <- 10
  #npitchers <- 10
  #tdollars <- tdollars - 80


  pdollars <- round(tdollars*hpratio)
  hdollars <- tdollars - pdollars

  thitters <- (nhitters * nteams) + padj
  tpitchers <- (npitchers * nteams) + padj
  ct <- 0

  while (ct < 6) {
    toph <- head(ihitters,thitters)
    topp <- head(ipitchers,tpitchers)
    mHR <- mean(toph$pHR,na.rm = TRUE)
    sdHR <- sd(toph$pHR,na.rm = TRUE)
    mR <- mean(toph$pR,na.rm = TRUE)
    sdR <- sd(toph$pR,na.rm = TRUE)
    mSB <- mean(toph$pSB,na.rm = TRUE)
    sdSB <- sd(toph$pSB,na.rm = TRUE)
    mRBI <- mean(toph$pRBI,na.rm = TRUE)
    sdRBI <- sd(toph$pRBI,na.rm = TRUE)

    mW <- mean(topp$pW,na.rm = TRUE)
    sdW <- sd(topp$pW,na.rm = TRUE)
    mSO <- mean(topp$pSO,na.rm = TRUE)
    sdSO <- sd(topp$pSO,na.rm = TRUE)
    mHLD <- mean(topp$pHLD,na.rm = TRUE)
    sdHLD <- sd(topp$pHLD,na.rm = TRUE)
    sdHLD <- ifelse(sdHLD==0,1,sdHLD)
    mSV <- mean(topp$pSV,na.rm = TRUE)
    sdSV <- sd(topp$pSV,na.rm = TRUE)

    mAvg <- mean(toph$pAVG,na.rm = TRUE)
    sdAvg <- sd(toph$pAVG,na.rm = TRUE)
    ihitters <- mutate(ihitters,xH = pH-(pAB * mAvg))
    toph <- mutate(toph,xH = pH-(pAB * mAvg))
    mxH <- mean(toph$xH,na.rm = TRUE)
    sdxH <- sd(toph$xH,na.rm = TRUE)

    mERA <- mean(topp$pERA,na.rm = TRUE)
    sdERA <- sd(topp$pERA,na.rm = TRUE)
    ipitchers <- mutate(ipitchers,xER = (pIP * mERA/9)-pER)
    topp <- mutate(topp,xER = (pIP * mERA/9)-pER)
    mxER <- mean(topp$xER,na.rm = TRUE)
    sdxER <- sd(topp$xER,na.rm = TRUE)

    ihitters <- mutate(ihitters,zHR=(pHR-mHR)/sdHR,zR=(pR-mR)/sdR,zRBI=(pRBI-mRBI)/sdRBI,
                       zSB=(pSB-mSB)/sdSB,zxH=(xH-mxH)/sdxH)
    ihitters <- mutate(ihitters,zScore=zHR+zR+zRBI+zSB+(1.0*zxH))
    ihitters <- arrange(ihitters,-zScore)

    ipitchers <- mutate(ipitchers,zW=(pW-mW)/sdW,zSO=(pSO-mSO)/sdSO,zHLD=(pHLD-mHLD)/sdHLD,
                       zSV=(pSV-mSV)/sdSV,zxER=(xER-mxER)/sdxER)
    ipitchers <- mutate(ipitchers,zScore=zW+zSO+(0.5*zHLD)+zSV+zxER)
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
  ih2$zDFL <- (ih2$zScore / tvalue) * (hdollars - nrow(ih2)) + 1
  tvalue <- sum(ip2$zScore)
  ip2$zDFL <- (ip2$zScore / tvalue) * (pdollars - nrow(ip2)) + 1

  bhitters <- select(ih2,playerid,zDFL)
  bpitchers <- select(ip2,playerid,zDFL)

  list(bhitters,bpitchers)
}

preLPP2 <- function(ihitters,ipitchers,prot=data.frame(),ratio=1,dadj=0,padj=0) {
  # Same as preLPP but also returns inflation-adjusted zDFL for protected players
  hpratio <- 0.35
  nhitters <- 13
  npitchers <- 12
  nteams <- 13
  cap <- 260

  pdollars <- round((nteams * (cap + dadj) * ratio)*hpratio)
  hdollars <- (nteams * (cap + dadj) * ratio) - pdollars

  thitters <- (nhitters * nteams) + padj
  tpitchers <- (npitchers * nteams) + padj
  ct <- 0

  while (ct < 6) {
    toph <- head(ihitters,thitters)
    topp <- head(ipitchers,tpitchers)
    mHR <- mean(toph$pHR,na.rm = TRUE)
    sdHR <- sd(toph$pHR,na.rm = TRUE)
    mR <- mean(toph$pR,na.rm = TRUE)
    sdR <- sd(toph$pR,na.rm = TRUE)
    mSB <- mean(toph$pSB,na.rm = TRUE)
    sdSB <- sd(toph$pSB,na.rm = TRUE)
    mRBI <- mean(toph$pRBI,na.rm = TRUE)
    sdRBI <- sd(toph$pRBI,na.rm = TRUE)

    mW <- mean(topp$pW,na.rm = TRUE)
    sdW <- sd(topp$pW,na.rm = TRUE)
    mSO <- mean(topp$pSO,na.rm = TRUE)
    sdSO <- sd(topp$pSO,na.rm = TRUE)
    mHLD <- mean(topp$pHLD,na.rm = TRUE)
    sdHLD <- sd(topp$pHLD,na.rm = TRUE)
    sdHLD <- ifelse(sdHLD==0,1,sdHLD)
    mSV <- mean(topp$pSV,na.rm = TRUE)
    sdSV <- sd(topp$pSV,na.rm = TRUE)

    mAvg <- mean(toph$pAVG,na.rm = TRUE)
    sdAvg <- sd(toph$pAVG,na.rm = TRUE)
    ihitters <- mutate(ihitters,xH = pH-(pAB * mAvg))
    toph <- mutate(toph,xH = pH-(pAB * mAvg))
    mxH <- mean(toph$xH,na.rm = TRUE)
    sdxH <- sd(toph$xH,na.rm = TRUE)

    mERA <- mean(topp$pERA,na.rm = TRUE)
    sdERA <- sd(topp$pERA,na.rm = TRUE)
    ipitchers <- mutate(ipitchers,xER = (pIP * mERA/9)-pER)
    topp <- mutate(topp,xER = (pIP * mERA/9)-pER)
    mxER <- mean(topp$xER,na.rm = TRUE)
    sdxER <- sd(topp$xER,na.rm = TRUE)

    ihitters <- mutate(ihitters,zHR=(pHR-mHR)/sdHR,zR=(pR-mR)/sdR,zRBI=(pRBI-mRBI)/sdRBI,
                       zSB=(pSB-mSB)/sdSB,zxH=(xH-mxH)/sdxH)
    ihitters <- mutate(ihitters,zScore=zHR+zR+zRBI+zSB+(1.0*zxH))
    ihitters <- arrange(ihitters,-zScore)

    ipitchers <- mutate(ipitchers,zW=(pW-mW)/sdW,zSO=(pSO-mSO)/sdSO,zHLD=(pHLD-mHLD)/sdHLD,
                       zSV=(pSV-mSV)/sdSV,zxER=(xER-mxER)/sdxER)
    ipitchers <- mutate(ipitchers,zScore=zW+zSO+(0.5*zHLD)+zSV+zxER)
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
  ih2$zDFL <- (ih2$zScore / tvalue) * (hdollars - nrow(ih2)) + 1
  tvalue <- sum(ip2$zScore)
  ip2$zDFL <- (ip2$zScore / tvalue) * (pdollars - nrow(ip2)) + 1

  bhitters <- select(ih2,playerid,zDFL)
  bpitchers <- select(ip2,playerid,zDFL)

  # Calculate inflation-adjusted zDFL for protected players using same rates
  if (nrow(prot) > 0) {
    hRate <- (hdollars - nrow(ih2)) / sum(ih2$zScore)
    pRate <- (pdollars - nrow(ip2)) / sum(ip2$zScore)
    protH <- semi_join(ihitters, prot, by='Player')
    protP <- semi_join(ipitchers, prot, by='Player')
    protH$zDFL <- protH$zScore * hRate + 1
    protP$zDFL <- protP$zScore * pRate + 1
    bprot <- rbind(select(protH, playerid, zDFL), select(protP, playerid, zDFL)) %>%
      group_by(playerid) %>% summarise(zDFL = max(zDFL), .groups = 'drop')
  } else {
    bprot <- data.frame(playerid=character(), zDFL=numeric())
  }

  list(bhitters, bpitchers, bprot)
}

dociiDollars <- function(ihitters,ipitchers,prot=data.frame(),ratio=1,dadj=0,padj=0) {
  #Set parameters
  #Used by draftGuide.r
  #Used by protectionList.r
  #used by inSeasonPulse.r
  
  # 13/12 hitters/pitchers based on rosters on 5/29/14
  nhitters <- 8
  npitchers <- 8
  cap <- 100
  # Setting for Protection and Draft - only focus on top 19 players
  # nhitters <- 10
  # npitchers <- 9
  # cap <- 254
  
  nteams <- 14
  tdollars <- nteams * (cap +dadj) * ratio
  
  # What happens when we lop of the last 5 spots
  # I can't remember why I'm doing this?
  #nhitters <- 10
  #npitchers <- 10
  #tdollars <- tdollars - 80
  
  
  pdollars <- round(tdollars*hpratio)
  hdollars <- tdollars - pdollars
  
  thitters <- (nhitters * nteams) + padj
  tpitchers <- (npitchers * nteams) + padj
  ct <- 0
  
  while (ct < 6) {
    toph <- head(ihitters,thitters)
    topp <- head(ipitchers,tpitchers)
    mHR <- mean(toph$pHR,na.rm = TRUE)
    sdHR <- sd(toph$pHR,na.rm = TRUE)
    mR <- mean(toph$pR,na.rm = TRUE)
    sdR <- sd(toph$pR,na.rm = TRUE)
    mSB <- mean(toph$pSB,na.rm = TRUE)
    sdSB <- sd(toph$pSB,na.rm = TRUE)
    mRBI <- mean(toph$pRBI,na.rm = TRUE)
    sdRBI <- sd(toph$pRBI,na.rm = TRUE)
    
    mW <- mean(topp$pW,na.rm = TRUE)
    sdW <- sd(topp$pW,na.rm = TRUE)
    mSO <- mean(topp$pSO,na.rm = TRUE)
    sdSO <- sd(topp$pSO,na.rm = TRUE)
    mHLD <- mean(topp$pHLD,na.rm = TRUE)
    sdHLD <- sd(topp$pHLD,na.rm = TRUE)
    sdHLD <- ifelse(sdHLD==0,1,sdHLD)
    mSV <- mean(topp$pSV,na.rm = TRUE)
    sdSV <- sd(topp$pSV,na.rm = TRUE)
    
    mAvg <- mean(toph$pAVG,na.rm = TRUE)
    sdAvg <- sd(toph$pAVG,na.rm = TRUE)
    ihitters <- mutate(ihitters,xH = pH-(pAB * mAvg))
    toph <- mutate(toph,xH = pH-(pAB * mAvg))
    mxH <- mean(toph$xH,na.rm = TRUE)
    sdxH <- sd(toph$xH,na.rm = TRUE)
    
    mERA <- mean(topp$pERA,na.rm = TRUE)
    sdERA <- sd(topp$pERA,na.rm = TRUE)
    ipitchers <- mutate(ipitchers,xER = (pIP * mERA/9)-pER)
    topp <- mutate(topp,xER = (pIP * mERA/9)-pER)
    mxER <- mean(topp$xER,na.rm = TRUE)
    sdxER <- sd(topp$xER,na.rm = TRUE)
    
    ihitters <- mutate(ihitters,zHR=(pHR-mHR)/sdHR,zR=(pR-mR)/sdR,zRBI=(pRBI-mRBI)/sdRBI,
                       zSB=(pSB-mSB)/sdSB,zxH=(xH-mxH)/sdxH)
    ihitters <- mutate(ihitters,zScore=zHR+zRBI+zSB+(1.0*zxH))
    ihitters <- arrange(ihitters,-zScore)
    
    ipitchers <- mutate(ipitchers,zW=(pW-mW)/sdW,zSO=(pSO-mSO)/sdSO,zHLD=(pHLD-mHLD)/sdHLD,
                        zSV=(pSV-mSV)/sdSV,zxER=(xER-mxER)/sdxER)
    ipitchers <- mutate(ipitchers,zScore=zW+zSO+zSV+zxER)
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


calcInflation <- function(prot) {
  # GENERATE DFL dollar values for all players
  #Set parameters
  nteams <- 13
  tdollars <- nteams * 260
  # 66/34 split - just guessing
  # books say 69/31, but that seems high for DAFL
  pdollars <- round(tdollars*hpratio)
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
  #df <- read.csv(fn,stringsAsFactors=FALSE, encoding="UTF-8")
  df <- read_json(fn,simplifyVector = TRUE)
  # Should we be removing blank teams?  Free agents are removed, but maybe it breaks something else?
  #df <- filter(df,str_length(Team) > 0)
  if ("playerid" %in% colnames(df)) {
  } else {
    if ("playerids" %in% colnames(df)) {
      df <- df %>% rename(playerid=playerids)
    } else {
      df <- df %>% mutate(playerid = str_match(Name,"s?a?[0-9]+"))
    }
  }

  colnames(df) <- str_c('p',colnames(df))
  df <- dplyr::rename(df,playerid=pplayerid)
  df <- dplyr::rename(df,Player=pPlayerName)
  df <- df %>% mutate(playerid = as.character(playerid))
  df <- left_join(df,m2,by=c('playerid'),copy=FALSE)
  df$birth_year <- replace(df$birth_year,is.na(df$birth_year),2010)
  df$pTeam <- replace(df$pTeam,is.na(df$pTeam),"FA")
  df <- mutate(df,Age=year(Sys.time())-birth_year)
  dfh <- anti_join(df,m2,by=c('playerid'),copy=FALSE)
  df$playerid <- ifelse(df$playerid %in% dfh$playerid,str_c(df$Player,df$pTeam),df$playerid)
  df
}

read.fgOLD <- function(fn) {
  m2 <- select(master,playerid,Pos,MLB,birth_year)
  df <- read.csv(fn,stringsAsFactors=FALSE, encoding="UTF-8")
  #df <- read_json(fn,simplifyVector = TRUE)
  # Should we be removing blank teams?  Free agents are removed, but maybe it breaks something else?
  #df <- filter(df,str_length(Team) > 0)
  colnames(df) <- str_c('p',colnames(df))
  colnames(df)[1] <- 'Player'
  df <- dplyr::rename(df,playerid=pplayerid)
  df <- df %>% mutate(playerid = as.character(playerid))
  df <- left_join(df,m2,by=c('playerid'),copy=FALSE)
  df$birth_year <- replace(df$birth_year,is.na(df$birth_year),2010)
  df <- mutate(df,Age=year(Sys.time())-birth_year)
  dfh <- anti_join(df,m2,by=c('playerid'),copy=FALSE)
  df$playerid <- ifelse(df$playerid %in% dfh$playerid,str_c(df$Player,df$pTeam),df$playerid)
  df
}

# read.cbsOLD <- function(fn) {
#   df <- read.csv(fn,skip=1,stringsAsFactors=FALSE, encoding="UTF-8")
#   df <- mutate(df, Pos = pullPos(Player))
#   df <- mutate(df, MLB = pullMLB(Player))
#   df$Player <- unlist(lapply(df$Player,stripName))
#   # Team abbreviations are not the same - find all discrepancies WAS->WSH
#   df$MLB <- replace(df$MLB,df$MLB=='WAS','WSH')
#   df$MLB <- replace(df$MLB,df$MLB=='CHW','CWS')
# 
#   # Create Team column
#   df <- mutate(df,Avail=str_replace(Avail,'\\.\\.\\.',''))
#   df <- left_join(df,nicks,by=c('Avail'))
#   df <- mutate(df,Team=ifelse(is.na(Team),'Free Agent',Team)) %>% select(-Avail,-Short)
#   addPlayerid(df)
# }
# 
read.cbs <- function(fn) {
  df <- read.csv(fn,skip=1,stringsAsFactors=FALSE, encoding="UTF-8")
  df <- mutate(df,Player = str_replace(Player,'&#149;','|'))
  df <- mutate(df, Pos = pullPos(Player))
  df <- mutate(df, MLB = pullMLB(Player))
  df <- filter(df,!is.na(MLB))
  df$Player <- unlist(lapply(df$Player,stripName))
  # Team abbreviations are not the same - find all discrepancies WAS->WSH
  df$MLB <- replace(df$MLB,df$MLB=='WAS','WSN')
  df$MLB <- replace(df$MLB,df$MLB=='CWS','CHW')
  df$MLB <- replace(df$MLB,df$MLB=='TB','TBR')
  df$MLB <- replace(df$MLB,df$MLB=='KC','KCR')
  df$MLB <- replace(df$MLB,df$MLB=='SD','SDP')
  df$MLB <- replace(df$MLB,df$MLB=='SF','SFG')
  
  # Create Team column
  #df <- mutate(df,Avail=str_replace(Avail,'\\.\\.\\.',''))
  #df <- left_join(df,nicks,by=c('Avail'))
  df$Team <- df$Avail
  #df$Team <- str_replace(df$Team,'W','Free Agent')
  df$Team <- ifelse(str_detect(df$Team,'W '),'Free Agent',df$Team)
  df <- select(df,-Avail)
  addPlayerid(df)
}

missing.cbs <- function(fn) {
  df <- read.csv(fn,skip=1,stringsAsFactors=FALSE, encoding="UTF-8")
  df <- mutate(df,Player = str_replace(Player,'&#149;','|'))
  df <- mutate(df, Pos = pullPos(Player))
  df <- mutate(df, MLB = pullMLB(Player))
  df <- filter(df,!is.na(MLB))
  df$Player <- unlist(lapply(df$Player,stripName))
  # Team abbreviations are not the same - find all discrepancies WAS->WSH
  df$MLB <- replace(df$MLB,df$MLB=='WAS','WSN')
  df$MLB <- replace(df$MLB,df$MLB=='CWS','CHW')
  df$MLB <- replace(df$MLB,df$MLB=='TB','TBR')
  df$MLB <- replace(df$MLB,df$MLB=='KC','KCR')
  df$MLB <- replace(df$MLB,df$MLB=='SD','SDP')
  df$MLB <- replace(df$MLB,df$MLB=='SF','SFG')
  
  # Create Team column
  #df <- mutate(df,Avail=str_replace(Avail,'\\.\\.\\.',''))
  #df <- left_join(df,nicks,by=c('Avail'))
  df$Team <- df$Avail
  #df$Team <- str_replace(df$Team,'W','Free Agent')
  df$Team <- ifelse(str_detect(df$Team,'W '),'Free Agent',df$Team)
  df <- select(df,-Avail)
  m2 <- select(master,-Pos,-Player) %>% dplyr::rename(Player=cbs_name)
  dfleft <- anti_join(df, m2,by=c('Player','MLB'))
  dfleft
}

addPlayerid <- function(df) {
  m2 <- select(master,-Pos,-Player) %>% dplyr::rename(Player=cbs_name)
  # Merge with team
  gfull <- inner_join(df, m2,by=c('Player','MLB'),relationship = "many-to-many")
  dfleft <- anti_join(df, m2,by=c('Player','MLB'))
  m2 <- anti_join(m2,df,by=c('Player','MLB'))

  # Merge rest with only name
  gname <- left_join(dfleft, m2,by=c('Player'),relationship = "many-to-many")
  gname <- select(gname,-MLB.x) %>% dplyr::rename(MLB=MLB.y)

  final <- rbind(gfull,gname)
  final
  # see if we can only join on team
  #gfull
}

addPlayeridOnly <- function(df) {
  m2 <- select(master,-Pos,-Player) %>% dplyr::rename(Player=cbs_name)
  m2 <- select(m2,Player,MLB,playerid)
  # Merge with team
  final <- left_join(df, m2,by=c('Player'),relationship = "many-to-many")
  final$playerid <- ifelse(is.na(final$playerid),final$Player,final$playerid)
  final$playerid <- ifelse(str_length(final$playerid)==0,final$Player,final$playerid)
  final
}

# 
# read.2014cbs <- function(fn) {
#   df <- read.csv(fn,skip=1,stringsAsFactors=FALSE, encoding="UTF-8")
#   df <- mutate(df, Pos = pullPos(Player))
#   df <- mutate(df, MLB = pullMLB(Player))
#   #df$Player <- unlist(lapply(df$Player,stripName))
#   df$Player <- unlist(lapply(df$Player,swapName2))
#   # Team abbreviations are not the same - find all discrepancies WAS->WSH
#   df$MLB <- replace(df$MLB,df$MLB=='WAS','WSH')
#   df$MLB <- replace(df$MLB,df$MLB=='CHW','CWS')
#   addPlayerid(df)
# }
# 
# read.inseasonrecap <- function(fn,pos) {
#   m2 <- select(master,-Pos,-Player) %>% dplyr::rename(Player=cbs_name)
#   df <- read.xlsx(fn,pos,stringsAsFactors=FALSE)
#   colnames(df) <- str_c('p',colnames(df))
#   df <- dplyr::rename(df,Player=pPlayer)
#   df <- mutate(df, Pos = pullPos(Player))
#   df <- mutate(df, MLB = pullMLB(Player))
#   df$Player <- unlist(lapply(df$Player,swapName2))
#   # Team abbreviations are not the same - find all discrepancies WAS->WSH
#   df$MLB <- replace(df$MLB,df$MLB=='WAS','WSH')
#   df$MLB <- replace(df$MLB,df$MLB=='CHW','CWS')
#   # Merge with team
#   gfull <- inner_join(df, m2,by=c('Player','MLB'))
#   dfleft <- anti_join(df, m2,by=c('Player','MLB'))
#   # Merge rest with only name
#   gname <- left_join(dfleft, m2,by=c('Player'))
#   gname <- select(gname,-MLB.x) %>% dplyr::rename(MLB=MLB.y)
#   gname$playerid <- ifelse(is.na(gname$playerid),gname$Player,gname$playerid)
#   rbind(gfull,gname)
# }

predictHolds <- function(pitchers) {
  #Need to predict holds
  # Step 2 - copy over previous year's totals
  lyp <- read.cbs(str_c("../AllP",lastyear,".csv"))
  lyp <- select(lyp,playerid,lyHLD=HD)
  pitchers <- left_join(pitchers,lyp,by=c('playerid'))

  # Step 3 - use last year's totals plus fangraphs projected role
  # Use last year's data, if now a closer, set to 0, if true setup - make sure to up number
  # http://www.fangraphs.com/fantasy/bullpen-report-september-24-2014/

  # Commenting out dynamic URL finder during offseason
  #Get latest bullpen report
  # f <- read_html('http://www.fangraphs.com/fantasy/category/bullpen-report/')
  # l <- xml_find_one(f,'//a[contains(@title,"Bullpen")]')
  # bp <- xml_attr(l,'href')
  #bp <- 'https://fantasy.fangraphs.com/bullpen-report-september-26-2019/'
  #Get latest bullpen report
  f <- read_html('http://www.fangraphs.com/fantasy/category/bullpen-report/')
  #l <- xml_find_one(f,'//a[contains(@title,"Bullpen")]')
  l <- xml_find_first(f,'//a[contains(@title,"Bullpen")]')
  bp <- xml_attr(l,'href')
  
    crep <- getbpReport(bp)

  colnames(crep) <- c('Player','pRole')
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

# # Year End Totals
# sTots <- list()


# Load Past still being used by protectionList
l1 <- loadPast()
eras <- l1[[1]]
avgs <- l1[[2]]
r3 <- l1[[3]]

# Load Master file
# master <- read.csv("../master.csv",stringsAsFactors=FALSE, encoding="UTF-8")
# # Use fangraphs id as player_id
# master <- dplyr::rename(master,playerid=fg_id,Pos = mlb_pos,MLB=mlb_team,Player=mlb_name)
# #master <- mutate(master, birth_year = as.integer(str_sub(birth_date,1,4)))
# master <- mutate(master, birth_year = year(as.Date(birth_date, format="%m/%d/%Y")))
# # Try using mlb_id when fg_id is empty, which it sometimes is
# master$playerid <- ifelse(str_length(master$playerid)==0,master$mlb_id,master$playerid)
master <- read.csv("../mymaster.csv",stringsAsFactors=FALSE, encoding="UTF-8")


calcGoals <- function(p,h,targets,t) {
  lcht <- h %>% filter(Team == t) %>%
    summarize(HR = sum(pHR, na.rm=TRUE),RBI=sum(pRBI, na.rm=TRUE),R=sum(pR, na.rm=TRUE),SB=sum(pSB, na.rm=TRUE))
  lcht <- melt(lcht) %>% dplyr::rename(statistic = variable, collected = value)
  hg <- inner_join(lcht,targets) %>% mutate(needed=goal-collected,pc = (collected/goal))

  lcpt <- p %>% filter(Team == t) %>%
    summarize(W = sum(pW, na.rm=TRUE),HLD=sum(pHLD, na.rm=TRUE),K=sum(pSO, na.rm=TRUE),SV=sum(pSV, na.rm=TRUE))
  lcpt <- melt(lcpt) %>% dplyr::rename(statistic = variable, collected = value)
  pg <- inner_join(lcpt,targets) %>% mutate(needed=goal-collected,pc = (collected/goal))

  gmet <- rbind(hg,pg) %>% arrange(pc)
}

hotScores <- function(toph,topp,tm=FALSE,withZ=FALSE) {
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

  mAvg <- mean(toph$AVG)
  sdAvg <- sd(toph$AVG)
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
  topp <- mutate(topp,zScore=zW+zSO+(0.6*zHLD)+zSV+zxER)
#  topp <- mutate(topp,zScore=zW+zSO+(0.4*zHLD)+zSV+zxER)
  topp <- arrange(topp,-zScore)

  # Add the total thitter value to everyone
  #toph <- head(toph,thitters)
  toph$zScore <- toph$zScore - last(toph$zScore)
  # Add pitchers
  #topp <- head(topp,tpitchers)
  topp$zScore <- topp$zScore - last(topp$zScore)

  ih2 <- toph
  ip2 <- topp

  hZcols <- c('zHR','zR','zRBI','zSB','zxH')
  pZcols <- c('zW','zSO','zHLD','zSV','zxER')
  if (tm) {
    if (withZ) {
      bhitters  <- select(ih2, playerid, Team, zScore, all_of(hZcols))
      bpitchers <- select(ip2, playerid, Team, zScore, all_of(pZcols))
    } else {
      bhitters  <- select(ih2, playerid, zScore, Team)
      bpitchers <- select(ip2, playerid, zScore, Team)
    }
  } else {
    if (withZ) {
      bhitters  <- select(ih2, playerid, zScore, all_of(hZcols))
      bpitchers <- select(ip2, playerid, zScore, all_of(pZcols))
    } else {
      bhitters  <- select(ih2, playerid, zScore)
      bpitchers <- select(ip2, playerid, zScore)
    }
  }

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

getSalary <- function() {
  # Add Salary, Contract to players
  s <- read.csv("../salaryinfo.csv",header=FALSE,stringsAsFactors=FALSE, encoding="UTF-8")
  colnames(s) <- c('Avail','Player','Pos','Salary','Contract','Rank','Extra')

  sal <- select(s,-Rank,-Extra) %>%
    filter(!(Avail %in% c('Batters','Pitchers','Avail'))) %>%
    filter(!(Player %in% c('TOTALS')))
  sal <- mutate(sal,Team = ifelse(str_length(lag(Pos))==0,lag(Avail),NA)) %>% filter(str_length(Pos)>0)
  sal$Team <- na.locf(sal$Team)
  sal <- mutate(sal,Player = str_replace(Player,'&#149;','|'))
  sal <- mutate(sal, MLB = pullMLB(Player))
  sal$Player <- unlist(lapply(sal$Player,stripName))
  sal$Salary <- as.integer(sal$Salary)
  sal$Contract <- as.integer(sal$Contract)
  sal <- addPlayerid(sal) %>% select(playerid,Salary,Contract) %>% distinct()
}

createProtection <- function(fn) {
  # Add Salary, Contract to players
  s <- read.csv(fn,header=FALSE,stringsAsFactors=FALSE, encoding="UTF-8")
  colnames(s) <- c('Avail','Player','Pos','Salary','Contract','Rank','Extra')

  sal <- select(s,-Rank,-Extra) %>%
    filter(!(Avail %in% c('Batters','Pitchers','Avail'))) %>%
    filter(!(Player %in% c('TOTALS')))
  sal <- mutate(sal,Team = ifelse(str_length(lag(Pos))==0,lag(Avail),NA)) %>% filter(str_length(Pos)>0)
  sal$Team <- na.locf(sal$Team)
  sal <- mutate(sal, MLB = pullMLB(Pos))
  sal$Player <- unlist(lapply(sal$Player,stripName))
  sal$Salary <- as.integer(sal$Salary)
  sal$Contract <- as.integer(sal$Contract)
  sal <- sal %>% select(-MLB,-Avail)
  #sal$Contract <- sal$Contract + 1
  #sal <- filter(sal,Contract < 5)
  #sal <- mutate(sal,Salary=ifelse(Contract==3,Salary+5,ifelse(Contract==4,Salary+10,Salary)))
  #BUG - minor league players get deleted from here.
  #sal <- addPlayeridOnly(sal) %>% select(-Team,-Pos)
  sal <- addPlayeridOnly(sal)
}

addSalary <- function(df) {
  left_join(df,sal,by=c('playerid'))
}


getMLBstandings <- function() {
  page <- read_html("https://www.mlb.com/standings/")
  p2 <- page %>% html_nodes("table") %>% html_table()
  stand <- p2[[1]]

  teamCol <- names(stand)[1]
  stand <- mutate(stand,Season=paste(W,L,sep='-'), LongMLB = .data[[teamCol]])
  mlbmap <- read.csv("../data/MLBmap.csv",stringsAsFactors = FALSE)
  stand <- left_join(stand,mlbmap) 
  #%>% mutate(L10 = '0-0')
  stand <- stand %>% select(MLB,Season,L10)
}

# getMLBstandings <- function() {
#   df <- read_json("https://erikberg.com/mlb/standings.json",simplifyVector = TRUE)
#   #df <- read_json("https://www.mlb.com/standings",simplifyVector = TRUE)
#   df2 <- df[2]
#   df3 <- df2$standing
#   df3 <- mutate(df3,Season=paste(won,lost,sep='-'))
#   mlbmap <- read.csv("../data/MLBmap.csv",stringsAsFactors = FALSE)
#   df3 <- left_join(df3,mlbmap)
#   stand <- df3 %>% select(MLB,Season,L10 = last_ten)
# }


addMLBstandings <- function(df) {
 left_join(df,stand,by=c('MLB'))
}
 

# Legacy RSelenium globals and helpers - no longer needed with API approach
# rD <- NA
# remDr <- NA
#
# startRS <- function() {
#   rD <- rsDriver(browser="firefox",port=free_port(),
#                  chromever=NULL, verbose=F)
#   remDr <- rD[["client"]]
# }
#
# endRS <- function() {
#   remDr$close()
#   system("taskkill /im java.exe /f")
# }


# Load injuries from FanGraphs JSON (fetched by scripts/fgFetchAll.js)
# Falls back to direct API call if JSON file not found
getInjuriesAPI <- function() {
  cat("Loading injuries from FanGraphs...\n")

  currentYear <- as.integer(format(Sys.Date(), "%Y"))

  jsonFile <- "../latestInjuries.json"
  if (file.exists(jsonFile)) {
    inj <- fromJSON(jsonFile)
    cat("Loaded", nrow(inj), "injury records from", jsonFile, "\n")
  } else {
    cat("JSON file not found, trying API directly...\n")
    url <- paste0("https://www.fangraphs.com/api/roster-resource/injury-report/data?season=", currentYear)
    inj <- fromJSON(url)
    cat("Fetched", nrow(inj), "injury records from API\n")
  }

  # Filter out blank entries and activated players (keep only currently injured)
  inj <- inj %>%
    filter(!is.na(playerName),
           playerName != "",
           status != "Activated" | is.na(status))

  cat("Filtered to", nrow(inj), "active injury records\n")

  # Rename columns to match expected format
  inj <- inj %>%
    rename(
      Player = playerName,
      MLB = team,
      playerid = playerId,
      Injury = injurySurgery,
      `Latest Update` = latestUpdate,
      `Injury / Surgery Date` = date
    ) %>%
    select(Player, MLB, playerid, Injury, `Latest Update`, `Injury / Surgery Date`, position, status)

  # Add playerid using the existing function for any missing
  inj <- addPlayerid(inj)

  write.csv(inj, "../latestInjuries.csv")
  cat("Wrote", nrow(inj), "injuries to ../latestInjuries.csv\n")
  inj
}

# Fetch Pitching+ data — cached JSON from Playwright, fallback to API
getStuffAPI <- function() {
  cat("Loading Pitching+ data...\n")

  currentYear <- as.integer(format(Sys.Date(), "%Y"))
  jsonFile <- "../latestStuff.json"

  stuff <- NULL
  if (file.exists(jsonFile)) {
    stuff <- tryCatch(read_json(jsonFile, simplifyVector = TRUE), error = function(e) NULL)
    if (!is.null(stuff) && nrow(stuff) > 0) {
      cat("Loaded", nrow(stuff), "Pitching+ records from", jsonFile, "\n")
    } else {
      stuff <- NULL
    }
  }

  if (is.null(stuff)) {
    cat("JSON file not found, trying API directly...\n")
    url <- paste0("https://www.fangraphs.com/api/leaders/major-league/data?pos=all&stats=pit&lg=all&season=",
                  currentYear, "&season1=", currentYear,
                  "&ind=0&qual=10&type=36&month=0&pageitems=2000")
    stuff <- tryCatch(fromJSON(url)$data, error = function(e) {
      cat("Error fetching Pitching+ data:", e$message, "\n")
      return(NULL)
    })
  }

  if (is.null(stuff) || nrow(stuff) == 0) {
    cat("No Pitching+ data available.\n")
    return(NULL)
  }

  stuff <- stuff %>%
    select(Player = PlayerName, MLB = TeamNameAbb, `Pitching+` = sp_pitching)

  write.csv(stuff, "../latestStuff.csv")
  cat("Wrote", nrow(stuff), "Pitching+ records to ../latestStuff.csv\n")
  stuff
}

# Fetch hitter leaderboard from FanGraphs API (last 30 days by default)
fetchLeaderboards <- function(startDate, endDate) {
  cat("Fetching leaderboards via Playwright...\n")
  cat("Date range:", startDate, "to", endDate, "\n")
  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd("../scripts")
  result <- system2("node", c("fgFetchLeaders.js", startDate, endDate), stdout = TRUE, stderr = TRUE)
  cat(paste(result, collapse = "\n"), "\n")
}

getHitterLeaders <- function(startDate, endDate) {
  fetchLeaderboards(startDate, endDate)

  jsonFile <- "../latestHitterLeaders.json"
  if (!file.exists(jsonFile)) {
    cat("No hitter leaderboard file found.\n")
    return(NULL)
  }

  hitters <- tryCatch(read_json(jsonFile, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(hitters) || nrow(hitters) == 0) {
    cat("No hitter data returned.\n")
    return(NULL)
  }

  cat("Loaded", nrow(hitters), "hitter records\n")

  hitters <- hitters %>%
    select(Player = PlayerName, MLB = TeamNameAbb, Pos = position,
           G, PA, AB, H, HR, R, RBI, SB, BB, SO,
           AVG, OBP, SLG, OPS, wOBA, `wRC+`, WAR, playerid)

  write.csv(hitters, "../latestHitterLeaders.csv", row.names = FALSE)
  hitters
}

getPitcherLeaders <- function(startDate, endDate) {
  jsonFile <- "../latestPitcherLeaders.json"
  if (!file.exists(jsonFile)) {
    cat("No pitcher leaderboard file found.\n")
    return(NULL)
  }

  pitchers <- tryCatch(read_json(jsonFile, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(pitchers) || nrow(pitchers) == 0) {
    cat("No pitcher data returned.\n")
    return(NULL)
  }

  cat("Loaded", nrow(pitchers), "pitcher records\n")

  pitchers <- pitchers %>%
    select(Player = PlayerName, MLB = TeamNameAbb,
           W, L, ERA, G, GS, SV, HLD, IP, SO, BB,
           `K/9`, `BB/9`, HR, WHIP, BABIP, FIP, xFIP, WAR, playerid)

  write.csv(pitchers, "../latestPitcherLeaders.csv", row.names = FALSE)
  pitchers
}

# Legacy RSelenium version (kept for reference, but getInjuriesAPI is preferred)
getInjuriesRS <- function() {
  remDr$navigate("https://www.fangraphs.com/roster-resource/injury-report")

  # Wait for page to fully load (FanGraphs uses dynamic content)
  Sys.sleep(5)

  html <- remDr$getPageSource()[[1]] %>% read_html() %>% html_nodes("table") %>% html_table()
  cat("Found", length(html), "tables on injury page\n")

  # Skip header/navigation tables - may need adjustment if page structure changes
  r15 <- html[c(-1:-14)]
  cat("Processing", length(r15), "injury tables\n")

  inj <- bind_rows(r15)
  inj <- distinct(inj)
  cat("Total injury records:", nrow(inj), "\n")

  inj <- rename(inj,Player=Name,MLB=Team)
  inj <- addPlayerid(inj)
  colnames(inj)[5] <- 'Injury'
  #colnames(inj)[10] <- 'LatestUpdate'
  write.csv(inj,"../latestInjuries.csv")
  cat("Wrote injuries to ../latestInjuries.csv\n")
  inj
}

getStuffRS <- function() {
  remDr$navigate("https://www.fangraphs.com/leaders/major-league?pos=all&stats=pit&type=36&startdate=&enddate=&month=0&season1=2025&season=2025&pagenum=1&pageitems=2000000000&qual=10")
  
  html <- remDr$getPageSource()[[1]] %>% read_html() %>% html_nodes("table") %>% html_table()
  stuff <- html[[16]]
  #stuff$Name <- stri_enc_toascii(stuff$Name)
  stuff <- stuff %>% select(Player=Name,MLB=Team,`Pitching+`)
  
  write.csv(stuff,"../latestStuff.csv")
  stuff
}

getInjuries <- function() {
  #url <- "http://www.cbssports.com/mlb/injuries/"
  url <- "https://scores.nbcsports.com/mlb/stats.asp?file=inj"
  #url <- "https://sportsdata.usatoday.com/baseball/mlb/injuries"
  page <- read_html(url) %>% html_nodes("table") %>% html_table(,header=TRUE,fill=TRUE)

  page[1] <- NULL
  page <- Filter(function(x) dim(x)[1] > 0, page)
  inj <- bind_rows(page)
  colnames(inj) <- c('Injury','Player','Expected.Return')
  #inj <- inj %>% select(-Team,-Position)
  #name <- str_remove_all(inj$Player,".+\\n") %>% str_trim()
  name <- gsub(",.*", "", inj$Player)
  inj$Player <- name
  #colnames(inj) <- c('Player','Injury','Expected.Return')
  inj
}

getInjuriesNEW <- function() {
  
}



addInjuries <- function(df) {
  left_join(df,inj,by=c('Player'))
}

get2starts <- function() {
  #url <- "https://www.cbssports.com/fantasy/baseball/two-start-pitchers"
  url <- "https://www.fantasypros.com/mlb/two-start-pitchers.php"
  page <- read_html(url) %>% html_nodes("table") %>% html_table(,header=TRUE,fill=TRUE)
  starts <- page[[1]]
  # make first row the column headers
  colnames(starts) <- starts[1, ]
  colnames(starts) <- make.names(colnames(starts))
  starts <- starts[-1, ]
  
  # extract Player and Team
  starts$Player <- str_trim(str_extract(starts$Two.Start.Pitcher,'[^(]+'))
  starts$Team <- str_extract(starts$Two.Start.Pitcher,  "(?<=\\().+?(?=\\))")
  
  starts$twostarts <- 'YES'
  #names(starts) <- make.names(names(starts), unique=TRUE)
  select(starts,Player,twostarts)
}

add2starts <- function(df) {
  left_join(df,twostarts,by=c('Player'))
}

# What on earth is this calculation???
# Where does the weight come from?  This is different by category
pvCat <- function(col,weight,myScore) {
  # average rate of stats collected for this category - times a weight that I don't understand
  # weight is being used as a big week gain/loss - what would that mean in the standings
  # still don't reemember why it changes by category as opposed to a statid value like 0.1
  rate <- (mean(col)/aWeek)*weight
  df <- data_frame(Vals=col)
  # wVals = difference between my score and others divided by rate = how many spots away am I?
  # rVals = a rank order of wVals
  df <- mutate(df,wVals = (Vals-myScore)/rate, rVals=as.numeric(rank(-Vals)))
  # My rVAl
  myr <- as.numeric(filter(df,Vals==myScore)$rVals)
  # pvpVals = for rVals less than me, no credit, if wVals clos, give 1, otherwise 1/wVals
  # pvmVals = same but on the negative side
  df <- mutate(df,pvpVals = ifelse(wVals < 0, 0,
                                   ifelse(rVals<myr,
                                          ifelse(wVals < 1,1,1/wVals),0)))
  df <- mutate(df,pvmVals = ifelse(wVals > 0, 0,
                                   ifelse(rVals>myr,
                                          ifelse(wVals > -1,-1,1/wVals),0)))
  # Add all the column data and you get your overall score
  list(sum(df$pvpVals),sum(df$pvmVals),(sum(df$pvpVals)+sum(df$pvmVals)))
}

# firstRowHeaders <- function(df) {
#   colnames(df) = df[1, ] # the first row will be the header
#   colnames(df) <- make.names(colnames(df))
#   #df[-1, ]          # removing the first row.
#   df
# }
# 
# find correct table automatically
findHTMLTable <- function(ltables,sze) {
  i <- 1
  l <- length(ltables)
  print(l)
  df <- ltables[[i]] %>% html_table(,header=TRUE,fill=TRUE)
  while (nrow(df) != sze & i < l) {
    i <- i + 1
    df <- ltables[[i]] %>% html_table(,header=TRUE,fill=TRUE)
  }
  df
}

getbpReport <- function(bp) {
  page <- read_html(bp) %>% html_nodes("table") %>% findHTMLTable(30)
  names(page)[1]<-"Team"
  #names(page)[6]<-"Extra"
  #crep <- select(page,-Extra)
  crep <- page

  #crep <- readHTMLTable(bp, header=T, which=15,stringsAsFactors=F)
  t <- data.frame(crep$Closer,10,stringsAsFactors=FALSE)
  t2 <- data.frame(crep$'First Up',5,stringsAsFactors=FALSE)
  t3 <- data.frame(crep$'Second Up',2,stringsAsFactors=FALSE)
  colnames(t) <- c('Player','Score')
  colnames(t2) <- c('Player','Score')
  colnames(t3) <- c('Player','Score')
  #crep <- rbind_list(t,t2,t3)
  crep <- bind_rows(t,t2,t3)
  crep$Player <- iconv(crep$Player,'UTF-8','ASCII')
  crep
}

# getBSHEV <- function() {
#   # xOPS for both hitters and pitchers, also alerts for top 100 prospects getting a promotion
#   # get FV for rookies from fangraphs?
#   hev <- read.csv("BSHes.csv",stringsAsFactors=FALSE)
#   hev <- rename(hev,mlb_id=player_id)
#   nr <- left_join(AllH,hev,by="mlb_id") %>% 
#     mutate(est_ops = est_slg + est_woba, ops_delta = est_ops - slg - woba) %>% 
#     filter(Team == 'Free Agent')
#   TopFAH <- group_by(nr,Pos) %>% arrange(Pos,-pDFL,-pSGP) %>% filter(rank(-pSGP) <= 8) %>%
#     select(Player,Pos,Age,pDFL,est_ops,ops_delta, pSGP, Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,AVG,hotscore,Injury,Expected.Return)
#   hunlucky <- filter(nr,est_ops > .8, ops_delta > 0) %>%
#     select(Player,Pos,Age,pDFL,est_ops,ops_delta, pSGP, Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,AVG,hotscore,Injury,Expected.Return)
# 
#   pes <- read.csv("BSPes.csv",stringsAsFactors=FALSE)
#   pes <- rename(pes,mlb_id=player_id)
#   pesAll <- left_join(AllP,pes,by="mlb_id") %>% 
#     mutate(est_ops = est_slg + est_woba, ops_delta = est_ops - slg - woba) %>% 
#     filter(Team == 'Free Agent')
#   allsp <- pesAll %>% arrange(-pDFL,-pSGP) %>% filter(pHLD==0,pSV==0, pGS > 0) %>%
#     select(Player,Pos,Age,pDFL,est_ops,ops_delta,pSGP,Rank,pW,pSO,pERA,pK.9,pFIP,pGS,W,K,S,HD,ERA,hotscore,twostarts,Injury,Expected.Return)
#   punlucky <- filter(pesAll,est_ops < .65, ops_delta < -0.1) %>%
#     select(Player,Pos,Age,pDFL,est_ops,ops_delta,pSGP,Rank,pW,pSO,pERA,pK.9,pFIP,pGS,W,K,S,HD,ERA,hotscore,twostarts,Injury,Expected.Return)
# }
# 
# getProspects <- function() {
#   # # prospects from FG
#   # url <- 'https://www.fangraphs.com/prospects/the-board-scouting-and-stats'
#   # #page <- read_html(url)
#   # #n <- html_nodes(page,"a") %>% html_text() 
#   # #html_nodes(page,"[class='data']") 
#   # 
#   # writeLines(sprintf("var page = require('webpage').create();
#   #                    page.open('%s', function () {
#   #                    console.log(page.content); //page source
#   #                    phantom.exit();
#   #                    });", url), con="scrape.js")
#   # 
#   # system("phantomjs scrape.js > scrape.html")
#   # 
#   # # extract the content you need
#   # pg <- read_html("scrape.html")
#   # n <- html_nodes(pg,"a.data-export") %>% html_attr("href")
#   # csvstring <- substr(n,str_locate(n,',')+1,str_length(n))
#   # #df <- read.csv(text=base64_dec(csvstring))
#   # nstring <- str_replace_all(csvstring,'%2C',',')
#   # df <- read.csv(text=nstring)
#   hplist <- read.csv("fangraphs-the-board-dataH.csv",stringsAsFactors = FALSE) %>%
#     rename(Player = Name,playerid = playerId)
#   pplist <- read.csv("fangraphs-the-board-dataP.csv",stringsAsFactors = FALSE) %>%
#     rename(Player = Name,playerid = playerId)
#   proh <- inner_join(FAH,hplist,by=c('playerid'))
#   prospectH <- select(proh,Player=Player.x,MLB=Org,Current.Level,Pos=Pos.y,Age=Age.y,FV,DFL=pDFL,Top.100,Hit,Game,Raw,Spd) %>%
#     arrange(desc(FV))
#   prop <- inner_join(FAP,pplist,by=c('playerid'))
#   prospectHP <- select(prop,Player=Player.x,MLB=Org,Current.Level,Age=Age.y,FV,DFL=pDFL,Top.100,FB,SL,CB,CH,CMD) %>%
#     arrange(desc(FV))
#   
# }
# 
stripDates <- function(name) {
  s <- str_sub(name,1,str_locate(name,"\\(")[,1]-2)
  ifelse(is.na(s),name,s)
  s
}

firstPos <- function (str) {
  #s <- str_split(str,',')[[1]][1]
  s <- str_sub(str,0,str_locate(str,',')[[1]]-1)
  s
}

firstPosSlash <- function (str) {
  #s <- str_split(str,'/')[[1]][1]
  s <- str_sub(str,0,str_locate(str,'/')[[1]]-1)
  s
}
# 
# calcLVG = (W+L+S+BS+HLD)/IP

getInjuriesFG <- function() {
  df <- read_json("../InjuryReport.json",simplifyVector = TRUE)
  df <- filter(df,status != 'Activated',is.na(returndate)) %>% mutate(Injury = str_c(status," - ",injurySurgery),
                                                    Expected.Return = str_c(eligibledate," - ",latestUpdate))
  df <- df %>% select(playerid=playerId,Injury,Expected.Return)
}

addInjuriesFG <- function(df) {
  left_join(df,inj,by=c('playerid'))
}

getRRClosers <- function() {
  df <- read_json("../Closers.json",simplifyVector = TRUE)
  rr <- df[[1]]
  rrc <- rr %>% select(playerid = playerId,Role,Tags)
}

getFGScouts <- function(fn) {
  df <- read_json(fn,simplifyVector = TRUE)
  rr <- df[[1]]
  rrc <- rr %>% rename(Player = playerName,playerid = PlayerId)
  rrc <- rrc %>% filter(!is.na(playerid))
}

# Load FanGraphs prospect board data (fetched by scripts/fgFetchAll.js)
# Falls back to API if cached file not found
# pos: "bat" for hitters, "pit" for pitchers
getFGProspects <- function(pos = "bat", draft = NULL) {
  # Try cached JSON file first
  cachedFile <- str_c("../prospects_", pos, "_", cyear, ".json")
  if (file.exists(cachedFile)) {
    df <- tryCatch(jsonlite::fromJSON(cachedFile), error = function(e) NULL)
    if (!is.null(df) && nrow(df) > 0) {
      message("Loaded prospects (", pos, ") from ", cachedFile)
    } else {
      df <- NULL
    }
  } else {
    df <- NULL
  }

  # Fall back to API calls if no cached file
  if (is.null(df)) {
    message("No cached prospect file, trying API...")
    fgFetch <- function(url) {
      resp <- tryCatch(httr::GET(url, httr::user_agent(
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
      )), error = function(e) NULL)
      if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)
      tryCatch(jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8")),
               error = function(e) NULL)
    }

    if (is.null(draft)) {
      candidates <- c(paste0(cyear, "updated"), paste0(cyear, "prospect"),
                      paste0(lastyear, "updated"), paste0(lastyear, "prospect"))
      for (cand in candidates) {
        yr <- as.integer(sub("(\\d{4}).*", "\\1", cand))
        url <- paste0("https://www.fangraphs.com/api/prospects/board/data?draft=", cand, "&pos=", pos)
        df <- fgFetch(url)
        if (!is.null(df) && nrow(df) > 0 && df$Season[1] == yr) {
          message("Using FanGraphs prospect list: ", cand)
          break
        }
      }
    } else {
      url <- paste0("https://www.fangraphs.com/api/prospects/board/data?draft=", draft, "&pos=", pos)
      df <- fgFetch(url)
    }
  }

  if (is.null(df) || nrow(df) == 0) {
    warning("getFGProspects: all API calls failed or returned no data for pos=", pos)
    return(NULL)
  }

  if (pos == "bat") {
    df <- df %>% rename(
      Name = playerName, Pos = Position, Org = Team,
      Top.100 = Ovr_Rank, Org.Rk = Org_Rank,
      Pitch.Sel = Pitch_Sel, Bat.Ctrl = Bat_Ctrl,
      Plyr.Type = Player_Type, Game.Pwr = Game,
      Raw.Pwr = Raw, Ath = Athleticism,
      FV = cFV
    )
  } else {
    df <- df %>% rename(
      Name = playerName, Pos = Position, Org = Team,
      Top.100 = Ovr_Rank, Org.Rk = Org_Rank,
      TJ.Date = TJDate, FB.Type = FBType,
      Sits = Range, Tops = Touch, FV = cFV
    )
  }

  df %>% filter(!is.na(PlayerId))
}

# Claude API wrapper for AI-powered team summaries
callClaudeAPI <- function(prompt, api_key = Sys.getenv("ANTHROPIC_API_KEY"), max_tokens = 2048) {
  if (api_key == "" || is.na(api_key)) {
    return("Error: ANTHROPIC_API_KEY environment variable not set. Please set it with Sys.setenv(ANTHROPIC_API_KEY='your-key')")
  }

  tryCatch({
    response <- POST(
      url = "https://api.anthropic.com/v1/messages",
      body = toJSON(list(
        model = "claude-sonnet-4-5-20250929",
        max_tokens = max_tokens,
        messages = list(list(role = "user", content = prompt))
      ), auto_unbox = TRUE),
      add_headers(
        `x-api-key` = api_key,
        `anthropic-version` = "2023-06-01",
        `content-type` = "application/json"
      )
    )

    # Get raw response text
    raw_text <- content(response, as = "text", encoding = "UTF-8")

    # Check HTTP status
    if (http_error(response)) {
      return(paste("API HTTP Error:", status_code(response), "-", raw_text))
    }

    # Parse response
    parsed <- fromJSON(raw_text)

    # Check for API error in response
    if (!is.null(parsed$error)) {
      return(paste("API Error:", parsed$error$message))
    }

    # Extract text from response
    if (is.data.frame(parsed$content)) {
      return(parsed$content$text[1])
    } else if (is.list(parsed$content)) {
      return(parsed$content[[1]]$text)
    } else {
      return(paste("Unexpected response format:", raw_text))
    }
  }, error = function(e) {
    paste("API Error:", e$message)
  })
}

# Clear Trending rows from DAFL.db for all seasons prior to `year`.
# Date is stored as R epoch-days (REAL), so compare against the same.
clearTrendingBefore <- function(year, dbPath = "DAFL.db") {
  cutoff <- as.numeric(as.Date(paste0(year, "-01-01")))
  conn <- dbConnect(RSQLite::SQLite(), dbPath)
  on.exit(dbDisconnect(conn))
  deleted <- dbExecute(conn, "DELETE FROM Trending WHERE Date < ?", params = list(cutoff))
  message(sprintf("Deleted %d Trending rows before %s-01-01", deleted, year))
  invisible(deleted)
}

# ============================================================
# Guardians Tracker — DB schema + helpers (see docs/superpowers/specs/2026-05-20-guardians-tracker-design.md)
# ============================================================

# Create the four Guardians-tracker tables if they don't exist.
# Idempotent: safe to call on every pulse run.
initGuardiansDB <- function(dbPath = "DAFL.db") {
  conn <- dbConnect(RSQLite::SQLite(), dbPath)
  on.exit(dbDisconnect(conn))

  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS GuardiansRoster (
      snapshot_date TEXT NOT NULL,
      mlb_id        INTEGER NOT NULL,
      fg_id         TEXT,
      player        TEXT,
      pos           TEXT,
      level         TEXT,
      team_id       INTEGER,
      age           REAL,
      PRIMARY KEY (snapshot_date, mlb_id)
    )")

  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS GuardiansStats (
      snapshot_date TEXT NOT NULL,
      mlb_id        INTEGER NOT NULL,
      level         TEXT,
      role          TEXT,
      pa INTEGER, ab INTEGER, h INTEGER, hr INTEGER,
      r INTEGER, rbi INTEGER, sb INTEGER, bb INTEGER, k INTEGER,
      avg REAL, obp REAL, slg REAL, woba REAL,
      ip REAL, w INTEGER, l INTEGER, sv INTEGER, hld INTEGER,
      so INTEGER, bb_p INTEGER,
      era REAL, fip REAL, k9 REAL, bb9 REAL, whip REAL,
      PRIMARY KEY (snapshot_date, mlb_id)
    )")

  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS GuardiansTransactions (
      txn_id        TEXT PRIMARY KEY,
      txn_date      TEXT NOT NULL,
      mlb_id        INTEGER,
      player        TEXT,
      type          TEXT,
      from_team_id  INTEGER,
      to_team_id    INTEGER,
      description   TEXT
    )")

  # GuardiansHotscore schema changed (dropped window_days). Drop the old
  # version on first run with the new schema. Safe because hotscores are
  # recomputed daily anyway.
  oldSchema <- tryCatch(
    dbGetQuery(conn, "PRAGMA table_info(GuardiansHotscore)"),
    error = function(e) NULL
  )
  if (!is.null(oldSchema) && "window_days" %in% oldSchema$name) {
    dbExecute(conn, "DROP TABLE GuardiansHotscore")
  }

  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS GuardiansHotscore (
      snapshot_date TEXT NOT NULL,
      mlb_id        INTEGER NOT NULL,
      level         TEXT,
      role          TEXT,
      hotscore      REAL,
      PRIMARY KEY (snapshot_date, mlb_id)
    )")

  invisible(TRUE)
}

# Hardcoded fallback table — the Guardians org affiliates as of 2026.
# Used when mlb_team_affiliates() fails. Values verified against the live API.
# Note: in MLB Stats API, both ACL and DSL use sport_id 16 ("Rookie"); we
# differentiate by team_name pattern.
.guardiansAffiliatesFallback <- function() {
  data.frame(
    level    = c("MLB",       "AAA",      "AA",          "A+",          "A",       "ACL",           "DSL"),
    sport_id = c(1L,          11L,        12L,           13L,           14L,       16L,             16L),
    team_id  = c(114L,        445L,       402L,          437L,          481L,      5374L,           5506L),
    name     = c("Guardians", "Clippers", "RubberDucks", "Captains",    "Howlers", "ACL Guardians", "DSL CLE Goryl"),
    stringsAsFactors = FALSE
  )
}

# Resolve Guardians org → affiliate IDs. Caches result in
# `../data/guardiansAffiliates.csv` for one week. Falls back to the
# hardcoded table if both the cache and the API fail.
resolveGuardiansAffiliates <- function(cachePath = "../data/guardiansAffiliates.csv",
                                       maxAgeDays = 7) {
  cacheAgeDays <- if (file.exists(cachePath)) {
    as.numeric(difftime(Sys.time(), file.info(cachePath)$mtime, units = "days"))
  } else Inf
  if (cacheAgeDays < maxAgeDays) {
    return(read.csv(cachePath, stringsAsFactors = FALSE))
  }
  out <- tryCatch({
    af <- baseballr::mlb_team_affiliates(team_ids = 114, season = as.integer(cyear))
    if (is.null(af) || nrow(af) == 0) stop("empty affiliates from baseballr")
    # baseballr returns columns sport_id, team_id, team_name (+ many more).
    # Filter out sport_id 21 (Indians Alt Site / Org / Prospects — not real
    # affiliates). Keep one row per level; for sport_id 16 (Rookie), the
    # first team starting with "ACL" is ACL, "DSL" is DSL.
    parent <- data.frame(level = "MLB", sport_id = 1L, team_id = 114L,
                         name = "Guardians", stringsAsFactors = FALSE)
    af <- af[af$sport_id %in% c(11L, 12L, 13L, 14L, 16L), , drop = FALSE]
    af$level <- ifelse(af$sport_id == 11L, "AAA",
                ifelse(af$sport_id == 12L, "AA",
                ifelse(af$sport_id == 13L, "A+",
                ifelse(af$sport_id == 14L, "A",
                ifelse(grepl("^ACL", af$team_name), "ACL",
                ifelse(grepl("^DSL", af$team_name), "DSL", NA_character_))))))
    af <- af[!is.na(af$level), , drop = FALSE]
    # Keep first team per level (deterministic by row order from API).
    af <- af[!duplicated(af$level), , drop = FALSE]
    children <- data.frame(
      level    = af$level,
      sport_id = as.integer(af$sport_id),
      team_id  = as.integer(af$team_id),
      name     = af$team_name,
      stringsAsFactors = FALSE
    )
    rbind(parent, children)
  }, error = function(e) {
    warning("resolveGuardiansAffiliates: API failed (", e$message,
            "); using hardcoded fallback")
    .guardiansAffiliatesFallback()
  })
  tryCatch(write.csv(out, cachePath, row.names = FALSE),
           error = function(e) NULL)
  out
}

# Pull today's roster for every Guardians affiliate. Returns a data frame
# with one row per (player, level). team_id is the affiliate id; level is
# MLB / AAA / AA / A+ / A / ACL / DSL.
#
# Uses baseballr::mlb_rosters (not mlb_team_roster — that name doesn't exist
# in baseballr 1.6.0).
pullGuardiansRoster <- function(affiliates = resolveGuardiansAffiliates(),
                                season = cyear) {
  rosters <- lapply(seq_len(nrow(affiliates)), function(i) {
    af <- affiliates[i, ]
    tryCatch({
      r <- suppressMessages(baseballr::mlb_rosters(team_id = af$team_id,
                                                   season = as.integer(season),
                                                   roster_type = "fullSeason"))
      if (is.null(r) || nrow(r) == 0) return(NULL)
      data.frame(
        mlb_id  = as.integer(r$person_id),
        player  = r$person_full_name,
        pos     = r$position_abbreviation,
        level   = af$level,
        team_id = af$team_id,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      warning("pullGuardiansRoster: ", af$level, " (", af$team_id, ") failed: ",
              e$message)
      NULL
    })
  })
  rosters <- Filter(Negate(is.null), rosters)
  if (length(rosters) == 0) {
    return(data.frame(mlb_id = integer(0), player = character(0),
                      pos = character(0), level = character(0),
                      team_id = integer(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, rosters)
}

# Pull season-to-date hitting and pitching stats for every Guardians affiliate.
# Returns a data frame keyed by mlb_id with a `role` column ("H"/"P"). When an
# affiliate has no games yet (spring training, DSL not yet active) the call
# returns NULL for that level and is dropped.
#
# baseballr 1.6.0 signature: mlb_stats(stat_type, stat_group, sport_ids, team_id,
# season, player_pool). `player_pool = "all"` is required to get per-player rows
# (without it, you get a handful of aggregate-split rows).
pullGuardiansStats <- function(affiliates = resolveGuardiansAffiliates(),
                               season = cyear) {
  pullOne <- function(af, group) {
    tryCatch({
      s <- suppressMessages(baseballr::mlb_stats(
        stat_type   = "season",
        stat_group  = group,
        season      = as.integer(season),
        sport_ids   = af$sport_id,
        team_id     = af$team_id,
        player_pool = "all"
      ))
      if (is.null(s) || nrow(s) == 0) return(NULL)
      s$level    <- af$level
      s$mlb_id   <- as.integer(s$player_id)
      s
    }, error = function(e) {
      warning("pullGuardiansStats: ", af$level, "/", group, " failed: ", e$message)
      NULL
    })
  }
  hitRows <- lapply(seq_len(nrow(affiliates)), function(i) pullOne(affiliates[i, ], "hitting"))
  pitRows <- lapply(seq_len(nrow(affiliates)), function(i) pullOne(affiliates[i, ], "pitching"))

  safe <- function(df, col) if (col %in% names(df)) df[[col]] else NA

  normH <- function(df) {
    if (is.null(df)) return(NULL)
    data.frame(
      mlb_id = df$mlb_id, level = df$level, role = "H",
      pa = suppressWarnings(as.integer(safe(df, "plate_appearances"))),
      ab = suppressWarnings(as.integer(safe(df, "at_bats"))),
      h  = suppressWarnings(as.integer(safe(df, "hits"))),
      hr = suppressWarnings(as.integer(safe(df, "home_runs"))),
      r  = suppressWarnings(as.integer(safe(df, "runs"))),
      rbi = suppressWarnings(as.integer(safe(df, "rbi"))),
      sb = suppressWarnings(as.integer(safe(df, "stolen_bases"))),
      bb = suppressWarnings(as.integer(safe(df, "base_on_balls"))),
      k  = suppressWarnings(as.integer(safe(df, "strike_outs"))),
      avg = suppressWarnings(as.numeric(safe(df, "avg"))),
      obp = suppressWarnings(as.numeric(safe(df, "obp"))),
      slg = suppressWarnings(as.numeric(safe(df, "slg"))),
      woba = NA_real_,
      ip = NA_real_, w = NA_integer_, l = NA_integer_,
      sv = NA_integer_, hld = NA_integer_,
      so = NA_integer_, bb_p = NA_integer_,
      era = NA_real_, fip = NA_real_, k9 = NA_real_,
      bb9 = NA_real_, whip = NA_real_,
      stringsAsFactors = FALSE
    )
  }
  normP <- function(df) {
    if (is.null(df)) return(NULL)
    data.frame(
      mlb_id = df$mlb_id, level = df$level, role = "P",
      pa = NA_integer_, ab = NA_integer_, h = NA_integer_, hr = NA_integer_,
      r = NA_integer_, rbi = NA_integer_, sb = NA_integer_, bb = NA_integer_,
      k = NA_integer_, avg = NA_real_, obp = NA_real_, slg = NA_real_, woba = NA_real_,
      ip  = suppressWarnings(as.numeric(safe(df, "innings_pitched"))),
      w   = suppressWarnings(as.integer(safe(df, "wins"))),
      l   = suppressWarnings(as.integer(safe(df, "losses"))),
      sv  = suppressWarnings(as.integer(safe(df, "saves"))),
      hld = suppressWarnings(as.integer(safe(df, "holds"))),
      so  = suppressWarnings(as.integer(safe(df, "strike_outs"))),
      bb_p = suppressWarnings(as.integer(safe(df, "base_on_balls"))),
      era = suppressWarnings(as.numeric(safe(df, "era"))),
      fip = NA_real_,
      k9  = suppressWarnings(as.numeric(safe(df, "strikeouts_per9inn"))),
      bb9 = suppressWarnings(as.numeric(safe(df, "walks_per9inn"))),
      whip = suppressWarnings(as.numeric(safe(df, "whip"))),
      stringsAsFactors = FALSE
    )
  }
  rows <- c(lapply(hitRows, normH), lapply(pitRows, normP))
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame(
      mlb_id = integer(0), level = character(0), role = character(0),
      pa = integer(0), ab = integer(0), h = integer(0), hr = integer(0),
      r = integer(0), rbi = integer(0), sb = integer(0), bb = integer(0), k = integer(0),
      avg = numeric(0), obp = numeric(0), slg = numeric(0), woba = numeric(0),
      ip = numeric(0), w = integer(0), l = integer(0), sv = integer(0),
      hld = integer(0), so = integer(0), bb_p = integer(0),
      era = numeric(0), fip = numeric(0), k9 = numeric(0),
      bb9 = numeric(0), whip = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

# Pull league-wide season hitting+pitching stats per level. Returns a list
# list(H=<hitters df>, P=<pitchers df>) with the same column schema as the
# normalised frames inside pullGuardiansStats. Used to z-score Guardians
# against their level's full league cohort for HotScore.
pullLeagueStats <- function(affiliates = resolveGuardiansAffiliates(),
                            season = cyear) {
  safe <- function(df, col) if (col %in% names(df)) df[[col]] else NA
  pullOne <- function(sport_id, level, group) {
    tryCatch({
      s <- suppressMessages(baseballr::mlb_stats(
        stat_type   = "season",
        stat_group  = group,
        season      = as.integer(season),
        sport_ids   = sport_id,
        player_pool = "all",
        limit       = 5000
      ))
      if (is.null(s) || nrow(s) == 0) return(NULL)
      s$level  <- level
      s$mlb_id <- as.integer(s$player_id)
      s
    }, error = function(e) {
      warning("pullLeagueStats: ", level, "/", group, " failed: ", e$message)
      NULL
    })
  }
  perLevel <- unique(affiliates[, c("sport_id", "level")])
  hitRows <- lapply(seq_len(nrow(perLevel)), function(i)
    pullOne(perLevel$sport_id[i], perLevel$level[i], "hitting"))
  pitRows <- lapply(seq_len(nrow(perLevel)), function(i)
    pullOne(perLevel$sport_id[i], perLevel$level[i], "pitching"))

  normH <- function(df) {
    if (is.null(df)) return(NULL)
    data.frame(
      mlb_id = df$mlb_id, level = df$level, role = "H",
      pa  = suppressWarnings(as.integer(safe(df, "plate_appearances"))),
      ab  = suppressWarnings(as.integer(safe(df, "at_bats"))),
      h   = suppressWarnings(as.integer(safe(df, "hits"))),
      hr  = suppressWarnings(as.integer(safe(df, "home_runs"))),
      bb  = suppressWarnings(as.integer(safe(df, "base_on_balls"))),
      avg = suppressWarnings(as.numeric(safe(df, "avg"))),
      obp = suppressWarnings(as.numeric(safe(df, "obp"))),
      slg = suppressWarnings(as.numeric(safe(df, "slg"))),
      stringsAsFactors = FALSE
    )
  }
  normP <- function(df) {
    if (is.null(df)) return(NULL)
    ipraw <- suppressWarnings(as.numeric(if ("innings_pitched" %in% names(df)) df$innings_pitched else NA))
    df <- df[!is.na(ipraw) & ipraw > 0, , drop = FALSE]
    if (nrow(df) == 0) return(NULL)
    data.frame(
      mlb_id = df$mlb_id, level = df$level, role = "P",
      ip   = suppressWarnings(as.numeric(safe(df, "innings_pitched"))),
      so   = suppressWarnings(as.integer(safe(df, "strike_outs"))),
      bb_p = suppressWarnings(as.integer(safe(df, "base_on_balls"))),
      hr   = suppressWarnings(as.integer(safe(df, "home_runs"))),
      era  = suppressWarnings(as.numeric(safe(df, "era"))),
      k9   = suppressWarnings(as.numeric(safe(df, "strikeouts_per9inn"))),
      whip = suppressWarnings(as.numeric(safe(df, "whip"))),
      stringsAsFactors = FALSE
    )
  }
  hitDf <- do.call(rbind, lapply(hitRows, normH))
  pitDf <- do.call(rbind, lapply(pitRows, normP))
  list(H = hitDf, P = pitDf)
}

# Pull the last `lookbackDays` of transactions for the Guardians org. Returns
# one row per transaction matching our schema.
#
# baseballr 1.6.0 has no transactions wrapper — call the MLB Stats API directly.
# We narrow the query by teamId=114 (parent club); the API returns all txns
# involving the parent OR any affiliate (call-ups, options, etc.). Response
# has nested `person`/`toTeam`/`fromTeam` objects; we flatten them.
pullGuardiansTransactions <- function(affiliates = resolveGuardiansAffiliates(),
                                      lookbackDays = 30) {
  # `affiliates` is accepted for API symmetry with sibling pullGuardians* helpers;
  # the org-level transactions endpoint returns all levels for teamId=114, so
  # we don't currently iterate over affiliate IDs here.
  emptyResult <- function() data.frame(
    txn_id = character(0), txn_date = character(0),
    mlb_id = integer(0), player = character(0), type = character(0),
    from_team_id = integer(0), to_team_id = integer(0),
    description = character(0),
    stringsAsFactors = FALSE
  )
  endDate   <- as.character(Sys.Date())
  startDate <- as.character(Sys.Date() - lookbackDays)
  j <- tryCatch({
    resp <- httr::GET("https://statsapi.mlb.com/api/v1/transactions",
                      query = list(teamId = 114,
                                   startDate = startDate,
                                   endDate = endDate))
    httr::stop_for_status(resp)
    jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                       simplifyDataFrame = TRUE)
  }, error = function(e) {
    warning("pullGuardiansTransactions failed: ", e$message); NULL
  })
  if (is.null(j) || is.null(j$transactions) ||
      !is.data.frame(j$transactions) || nrow(j$transactions) == 0) {
    return(emptyResult())
  }
  txns <- j$transactions
  # The API's nested objects appear as flat columns named with `.` separators
  # via simplifyDataFrame; e.g. `person.id`, `person.fullName`, `toTeam.id`,
  # `fromTeam.id`. Fall back gracefully if those columns are absent.
  pick <- function(col, default = NA) if (col %in% names(txns)) txns[[col]] else default
  data.frame(
    txn_id       = as.character(pick("id")),
    txn_date     = as.character(pick("date")),
    mlb_id       = suppressWarnings(as.integer(pick("person.id"))),
    player       = as.character(pick("person.fullName")),
    type         = as.character(pick("typeDesc")),
    from_team_id = suppressWarnings(as.integer(pick("fromTeam.id"))),
    to_team_id   = suppressWarnings(as.integer(pick("toTeam.id"))),
    description  = as.character(pick("description")),
    stringsAsFactors = FALSE
  )
}

# Pull last-30-day game logs for one player (used by HotScore). Calls the FG
# MiLB game-log endpoint, which covers all MiLB levels. Returns the raw FG
# columns plus a normalised `gl_date` (Date) column for date filtering.
# Returns NULL on missing id, no rows, or any failure so the caller can skip
# the player without crashing.
pullGuardiansGameLogs <- function(fg_id, role, season = cyear) {
  if (is.na(fg_id) || !nzchar(as.character(fg_id))) return(NULL)
  tryCatch({
    if (role == "H") {
      df <- baseballr::fg_milb_batter_game_logs(playerid = fg_id,
                                                year = as.integer(season))
    } else {
      df <- baseballr::fg_milb_pitcher_game_logs(playerid = fg_id,
                                                 year = as.integer(season))
    }
    if (is.null(df) || nrow(df) == 0) return(NULL)
    # Standardise the date column name across baseballr versions.
    dateCol <- intersect(c("Date", "date", "game_date"), names(df))[1]
    if (is.na(dateCol)) return(NULL)
    df$gl_date <- suppressWarnings(as.Date(df[[dateCol]]))
    df <- df[!is.na(df$gl_date) & df$gl_date >= Sys.Date() - 30, , drop = FALSE]
    if (nrow(df) == 0) return(NULL)
    df
  }, error = function(e) {
    warning("pullGuardiansGameLogs(", fg_id, "/", role, ") failed: ", e$message)
    NULL
  })
}

# Compute a daily hot score per (player, level). Cohort = ALL players at the
# same level across the entire league (from pullLeagueStats). An A+ Guardian
# is scored against every other A+ player; an MLB Guardian against the whole
# of MLB. Z-scoring against a 200+ player cohort gives the wide ±3 spread the
# user expects (the prior org-only cohort capped at ±1.4 with n=3).
#
# Args:
#   roster: data.frame with mlb_id, level, role (Guardians org subset)
#   leagueStats: list(H=<league hitters>, P=<league pitchers>) from pullLeagueStats
#
# Returns: data.frame(mlb_id, level, role, hotscore) — one row per Guardian
#          who has enough activity (pa >= 20 hitters; ip >= 5 pitchers).
computeGuardiansHotscore <- function(roster, leagueStats) {
  metricH <- function(df) {
    # OPS-style: ((H + BB) / PA) * 1.2 + (TB / AB).
    # mlb_stats doesn't return total bases directly; reconstruct from slg * ab.
    tb <- ifelse(is.na(df$slg) | is.na(df$ab), NA_real_, df$slg * df$ab)
    metric <- ((df$h + df$bb) / df$pa) * 1.2 + (tb / df$ab)
    df$metric <- ifelse(!is.na(df$pa) & df$pa >= 20 & !is.na(df$ab) & df$ab > 0,
                        metric, NA_real_)
    df
  }
  metricP <- function(df) {
    # (SO - BB - 2*HR) / IP, per inning.
    metric <- (df$so - df$bb_p - 2 * df$hr) / df$ip
    df$metric <- ifelse(!is.na(df$ip) & df$ip >= 5, metric, NA_real_)
    df
  }
  zscoreByLevel <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df <- df[!is.na(df$metric), , drop = FALSE]
    if (nrow(df) == 0) return(NULL)
    # Per-level z-score against the whole league cohort.
    out <- do.call(rbind, lapply(split(df, df$level), function(g) {
      if (nrow(g) < 5) {
        g$z <- 0
      } else {
        m <- mean(g$metric, na.rm = TRUE); s <- sd(g$metric, na.rm = TRUE)
        g$z <- if (is.na(s) || s == 0) 0 else (g$metric - m) / s
      }
      g
    }))
    out
  }
  leagueH <- zscoreByLevel(metricH(leagueStats$H))
  leagueP <- zscoreByLevel(metricP(leagueStats$P))

  # Filter to Guardians and emit the hotscore rows.
  org <- as.integer(roster$mlb_id)
  outH <- if (!is.null(leagueH)) {
    sub <- leagueH[leagueH$mlb_id %in% org, , drop = FALSE]
    if (nrow(sub) == 0) NULL else
      data.frame(mlb_id = sub$mlb_id, level = sub$level, role = "H",
                 hotscore = sub$z, stringsAsFactors = FALSE)
  } else NULL
  outP <- if (!is.null(leagueP)) {
    sub <- leagueP[leagueP$mlb_id %in% org, , drop = FALSE]
    if (nrow(sub) == 0) NULL else
      data.frame(mlb_id = sub$mlb_id, level = sub$level, role = "P",
                 hotscore = sub$z, stringsAsFactors = FALSE)
  } else NULL
  rows <- Filter(Negate(is.null), list(outH, outP))
  if (length(rows) == 0) {
    return(data.frame(mlb_id = integer(0), level = character(0),
                      role = character(0), hotscore = numeric(0),
                      stringsAsFactors = FALSE))
  }
  result <- do.call(rbind, rows)
  # Dedup: a player on a rehab/option can appear at multiple levels in the
  # league-wide pull. Keep the highest-level row per player (PK is mlb_id).
  levelOrder <- c("MLB" = 1L, "AAA" = 2L, "AA" = 3L, "A+" = 4L, "A" = 5L,
                  "ACL" = 6L, "DSL" = 7L)
  result$lvlRank <- levelOrder[result$level]
  result$lvlRank[is.na(result$lvlRank)] <- 99L
  result <- result[order(result$mlb_id, result$lvlRank), ]
  result <- result[!duplicated(result$mlb_id), ]
  result$lvlRank <- NULL
  result
}

