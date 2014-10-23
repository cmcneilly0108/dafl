# TBD - Function that maps input files to Lahman IDs
# CBS, Fangraphs


getd <- function(c) {
  as.numeric(unlist(r3[r3$Category==c,'ad']))
}

loadPast <- function() {
  results <- read.xlsx("DAFLSGP.xlsx",2)
  teams <- read.xlsx("DAFLSGP.xlsx",3)
  r2 <- inner_join(results,teams,by=c('Year'),copy=FALSE)
  # figure out ERA, AVG
  r2$denom <- with(r2,(Top - Bottom)/Teams)
  r3 <- r2 %>% group_by(Category) %>% summarize(ad = mean(denom))
  list(r2,r3)
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
  
  with(p,W/getd('W') + SO/getd('K') + (SV/getd('SV')) + 0.3*(HLD/getd('HLD')) +
         ((avgera - ((eruns+ER) * (9/(innpit+IP))))/getd('ERA'))
  )  
}

pitSGPh <- function(p) {
  innpit <- (6 * 200) + (2 * 75)
  avgera <- mean(eras)
  eruns <-  (avgera/9) * innpit
  innpit <- innpit * 7/8
  eruns <- eruns * 7/8
  
  with(p,pW/getd('W') + pSO/getd('K') + pSV/getd('SV') + 0.3*(pHLD/getd('HLD')) +
         ((avgera - ((eruns+pER) * (9/(innpit+pIP))))/getd('ERA'))
  )
}

pitSGPhALL <- function(p) {
  innpit <- (6 * 200) + (2 * 75)
  avgera <- mean(eras)
  # avgera is not correct - is the avg of the denoms
  eruns <-  (avgera/9) * innpit
  innpit <- innpit * 7/8
  eruns <- eruns * 7/8
  
  p$pSGP <- with(p,pW/getd('W') + pSO/getd('K') + 0.7*(pSV/getd('SV')) + 0.35*(pHLD/getd('HLD')) +
         ((avgera - ((eruns+pER) * (9/(innpit+pIP))))/getd('ERA')))
  p$sW <- with(p,pW/getd('W'))
  p$sK <- with(p,pSO/getd('K'))
  p$sSV <- with(p,pSV/getd('SV'))
  p$sHLD <- with(p,pHLD/getd('HLD'))
  p$sERA <- with(p,((avgera - ((eruns+pER) * (9/(innpit+pIP))))/getd('ERA')))
  print(avgera)
  return(p)
}


swapName <- function(n){
  comma <- str_locate(n,',')
  ln <- str_sub(n,1,comma-1)
  fn <- str_sub(n,comma+2,-1)
  nn <- str_join(fn,ln,sep=" ",collapse=NULL)
  nn[1] 
}

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
  p <- str_match(n,".+, .+ ([^|]+) .+")
  p <- p[,2]
  p <- ifelse((p =='P'),'RP',p)
  ifelse((p %in% c('CF','RF','LF')),'OF',p)
}

pullMLB <- function(n){
  n <- str_trim(n)
  p <- str_match(n,".+, .+ .+ (.+)")
  p <- p[,2]
}

pullTeam <- function(tn){
  tH <- filter(AllH,Team == tn)
  tH <- select(tH,-Team)
  tP <- filter(AllP,Team == tn)
  tP <- select(tP,-Team)
  tP <- tP %>% arrange(-pDFL) %>% 
    select(Player,Pos,pDFL,pSGP,Rank,pW,pSO,pHLD,pSV,pERA,pK.9,pFIP,W,K,HD,S,ERA)
  tH <- tH %>% arrange(-pDFL) %>%
    select(Player,Pos,pDFL,pSGP,Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)
  list(tH,tP)
}

addSheet <- function(l,w){
  sht <- createSheet(wb=w,sheetName=l[[1]])
  addDataFrame(x=l[[2]],sheet=sht)
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

loadPast2 <- function() {
  f1 <- read.csv("fs2013.csv")
  res <- genDenoms(f1)
  eras <- f1$ERA
  avgs <- f1$AVG
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

preDollars2 <- function(ihitters,ipitchers) {
  # GENERATE DFL dollar values for all players
  #Set parameters
  nteams <- 15
  tdollars <- nteams * 260
  # 63/37 split - just guessing
  pdollars <- round(tdollars*0.37)
  hdollars <- tdollars - pdollars
  # 13/12 hitters/pitchers based on rosters on 5/29/14
  nhitters <- 12
  npitchers <- 13
  thitters <- (nhitters * nteams)
  tpitchers <- (npitchers * nteams)
  # Only value a certain number of players
  bhitters <- filter(ihitters,rank(-pSGP) <= thitters)
  hitSGP <- round(sum(bhitters$pSGP))
  bpitchers <- filter(ipitchers,rank(-pSGP) <= tpitchers)
  pitSGP <- round(sum(bpitchers$pSGP))
  hsgpd <- hdollars/hitSGP
  psgpd <- pdollars/pitSGP
  # Create dollar amounts
  bhitters$pDFL <- bhitters$pSGP * hsgpd
  bpitchers$pDFL <- bpitchers$pSGP * psgpd
  bhitters <- select(bhitters,playerid,pDFL)
  bpitchers <- select(bpitchers,playerid,pDFL)
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

preDollars <- function(ihitters,ipitchers,prot) {
  # GENERATE DFL dollar values for all players
  #Set parameters
  nteams <- 15
  tdollars <- nteams * 260
  tdollars <- tdollars - sum(prot$Salary)
  # 63/37 split - just guessing
  pdollars <- round(tdollars*0.37)
  hdollars <- tdollars - pdollars
  # 13/12 hitters/pitchers based on rosters on 5/29/14
  nhitters <- 12
  npitchers <- 13
  thitters <- (nhitters * nteams)
  tpitchers <- (npitchers * nteams)
  
  # Remove protected players and change counts and dollars
  ih2 <- anti_join(ihitters,prot,by=c('Player'),copy=FALSE)
  ip2 <- anti_join(ipitchers,prot,by=c('Player'),copy=FALSE)
  tpitchers <- tpitchers - nrow(prot[prot$Pos == 'P',])
  thitters <- thitters - nrow(prot[prot$Pos != 'P',])
  
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
  bhitters <- select(bhitters,playerid,pDFL)
  bpitchers <- select(bpitchers,playerid,pDFL)
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

read.fg <- function(fn) {
  m2 <- select(master,playerid,Pos,MLB)
  df <- read.csv(fn,stringsAsFactors=FALSE)
  colnames(df) <- str_join('p',colnames(df))
  df <- rename(df,playerid=pplayerid,Player=pName)
  df <- left_join(df,m2,by=c('playerid'),copy=FALSE)
}

read.cbs <- function(fn) {
  m2 <- select(master,-Pos)
  df <- read.csv(fn,skip=1,stringsAsFactors=FALSE)
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
  gname <- inner_join(dfleft, m2,by=c('Player'))
  gname <- select(gname,-MLB.x) %>% rename(MLB=MLB.y) 
#  gname <- select(gname,-Pos.y) %>% rename(Pos=Pos.x) 
  rbind(gfull,gname)
}
