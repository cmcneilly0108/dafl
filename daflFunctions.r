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
  avgavg <- mean((r2[r2$Category=='AVG','Top']+r2[r2$Category=='AVG','Bottom'])/2)
  hits <- round(atBats * avgavg)
  atBats <- atBats *8 / 9
  hits <- round(hits  *8 / 9)
  
  with(h,R/getd('R') + HR/getd('HR') + RBI/getd('RBI') + SB/getd('SB') +
         (((hits + H)/(atBats + AB)) - avgavg)/getd('AVG'))
}

pitSGP <- function(p) {
  innpit <- (6 * 200) + (2 * 75)
  avgera <- mean((r2[r2$Category=='ERA','Top']+r2[r2$Category=='ERA','Bottom'])/2)
  eruns <-  (avgera/9) * innpit
  innpit <- innpit * 7/8
  eruns <- eruns * 7/8
  
  with(p,W/getd('W') + SO/getd('K') + SV/getd('SV') +
         #         ((((eruns+ER)/(innpit+IP)*9) - avgera)/getd('ERA')))  
         ((avgera - ((eruns+ER) * (9/(innpit+IP))))/-getd('ERA'))
  )  
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
  p <- str_match(n,".+, .+ (.+) .+")
  p <- p[,2]
  p <- ifelse((p =='P'),'RP',p)
  ifelse((p %in% c('CF','RF','LF')),'OF',p)
}

pullTeam <- function(tn){
  tH <- filter(AllH,Team == tn)
  tH <- select(tH,-Team)
  tP <- filter(AllP,Team == tn)
  tP <- select(tP,-Team)
  tP <- tP %.% arrange(-pSGP) %.% 
    select(Player,Pos,pSGP,gVAL,Rank,wVAL,pW,pSO,pHLD,pSV,pERA,pK.9,pFIP,W,K,HD,S,ERA)
  tH <- tH %.% arrange(-pSGP) %.%
    select(Player,Pos,pSGP,gVAL,Rank,wVAL,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,BA)
  list(tH,tP)
}

addSheet <- function(l){
  sht <- createSheet(wb=wkly,sheetName=l[[1]])
  addDataFrame(x=l[[2]],sheet=sht)
}
