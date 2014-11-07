# TBD
# Improve holds projections
# Fix - Miguel Gonzalez - only obvious dup left "AllP2014.csv"
# Bug seems to be in read.cbs


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
  fn <- str_match(n,".+, (.+) [^|]+ .+")
  fn <- fn[,2]
  #space <- str_locate(rest,' ')
  #fn <- str_sub(rest,1,space-1)
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

preDollars <- function(ihitters,ipitchers,prot=data.frame(),ratio=1,dadj=0,padj=0) {
  # GENERATE DFL dollar values for all players
  #Set parameters
  nteams <- 15
  tdollars <- nteams * (260 +dadj) * ratio
  # 66/34 split - just guessing
  # books say 69/31, but that seems high for DAFL
  pdollars <- round(tdollars*0.39)
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
    pdollars <- pdollars - sum(protected[prot$Pos %in% c('SP','MR','CL'),'Salary'])
    hdollars <- hdollars - sum(protected[!(prot$Pos %in% c('SP','MR','CL')),'Salary'])
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
  nc <- nrow(filter(bhitters,Pos=='C'))
  bh2 <- head(bhitters,-(nteams-nc))
  ac <- filter(hitters,Pos=='C') %>% arrange(-pSGP)
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
postDollars <- function(ihitters,ipitchers) {
  # GENERATE DFL dollar values for all players
  #Set parameters
  nteams <- 15
  tdollars <- nteams * (260 + 75)
  # 66/34 split - just guessing
  # books say 69/31, but that seems high for DAFL
  pdollars <- round(tdollars*0.39)
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

read.fg <- function(fn) {
  m2 <- select(master,playerid,Pos,MLB)
  df <- read.csv(fn,stringsAsFactors=FALSE)
  colnames(df) <- str_join('p',colnames(df))
  df <- rename(df,playerid=pplayerid,Player=pName)
  df <- left_join(df,m2,by=c('playerid'),copy=FALSE)
}

read.cbs <- function(fn) {
  m2 <- select(master,-Pos,-Player) %>% rename(Player=cbs_name)
  df <- read.csv(fn,skip=1,stringsAsFactors=FALSE)
  df <- mutate(df, Pos = pullPos(Player))
  df <- mutate(df, MLB = pullMLB(Player))
  # okay to here - swapname2 stripping out middle name
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

read.inseasonrecap <- function(fn,pos) {
  m2 <- select(master,-Pos,-Player) %>% rename(Player=cbs_name)
  df <- read.xlsx(fn,pos,stringsAsFactors=FALSE)
  colnames(df) <- str_join('p',colnames(df))
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
  lyp <- read.cbs("AllP2014.csv")
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
  return(pitchers)  
}

# Year End Totals
sTots <- list()

l1 <- loadPast2()
eras <- l1[[1]]
avgs <- l1[[2]]
r3 <- l1[[3]]
# Load Master file
master <- read.csv("master141031.csv",stringsAsFactors=FALSE)
master <- rename(master,playerid=fg_id,Pos = mlb_pos,MLB=mlb_team,Player=mlb_name)

