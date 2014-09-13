# TBD - Function that maps input files to Lahman IDs
# CBS, Fangraphs


getd <- function(c) {
  r3[r3$Category==c,'ad']
}

loadPast <- function() {
  results <- read.xlsx("DAFLSGP.xlsx",2)
  teams <- read.xlsx("DAFLSGP.xlsx",3)
  r2 <- inner_join(results,teams,by=c('Year'),copy=FALSE)
  # figure out ERA, AVG
  r2$denom <- with(r2,(Top - Bottom)/Teams)
  r3 <- r2 %.% group_by(Category) %.% summarize(ad = mean(denom))
  list(r2,r3)
}

hitSGP <- function(h) {
  atBats <- 510 * 9
  avgavg <- mean(avgs)
  hits <- round(atBats * avgavg)
  atBats <- atBats *8 / 9
  hits <- round(hits  *8 / 9)
  
  with(h,R/getd('R') + HR/getd('HR') + RBI/getd('RBI') + SB/getd('SB') +
         (((hits + H)/(atBats + AB)) - avgavg)/getd('AVG'))
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
  tP <- tP %.% arrange(-pDFL) %.% 
    select(Player,Pos,pDFL,pSGP,Rank,pW,pSO,pHLD,pSV,pERA,pK.9,pFIP,W,K,HD,S,ERA)
  tH <- tH %.% arrange(-pDFL) %.%
    select(Player,Pos,pDFL,pSGP,Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)
  list(tH,tP)
}

addSheet <- function(l){
  sht <- createSheet(wb=wkly,sheetName=l[[1]])
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

