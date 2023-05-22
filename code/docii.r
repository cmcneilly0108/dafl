

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
library("tidyr")


source("./daflFunctions.r")

whichFile <- "new"
#whichFile <- "year"

### Pull last 30 days of stats ###
# Update data files
fd <- file.info("dociH.csv")$mtime
cd <- Sys.time()
dt <- difftime(cd, fd, units = "hours")
if (dt > 8) {
  system("bash ../scrips/doci.sh")
}





stand <- getMLBstandings()
inj <- getInjuries()




#pitchers <- read.cbs("preseasonP.csv")
#hitters <- read.cbs("preseasonH.csv")

if (whichFile == "new") {
  pitchers <- read.cbs("dociP.csv")
  hitters <- read.cbs("dociH.csv")
} else {
  pitchers <- read.cbs("AllPYTD.csv")
  hitters <- read.cbs("AllHYTD.csv")
}


# rename columns for SGP calc
hitters <- rename(hitters,pHR = HR,pRBI = RBI,pSB = SB,pH = H,pAB = AB,pAVG = AVG)
hitters <- mutate(hitters,pR=1)
pitchers <- mutate(pitchers,pIP=27,pER=ERA*3,pHLD=0)
pitchers <- rename(pitchers,pW = W, pSV = S, pSO = K,pERA=ERA)

hitters$pSGP <- hitSGP(hitters)
#hitters <- select(hitters,-Player,-MLB,-Pos)

pitchers$pSGP <- pitSGP(pitchers)
#pitchers <- select(pitchers,-Player,-MLB,-Pos)

AllH <- hitters
AllP <- pitchers

#inj <- getInjuries()
AllH <- AllH %>% addInjuries()
AllP <- AllP %>% addInjuries()

AllH <- replace_na(AllH,list(Expected.Return = ""))
AllP <- replace_na(AllP,list(Expected.Return = ""))


#### Add in playoff percentages
podds <- read.csv("playoffodds.csv",stringsAsFactors=FALSE)
AllH <- left_join(AllH,podds,by=c('MLB'))
AllP <- left_join(AllP,podds,by=c('MLB'))

# Prune some players first - not making playoffs, out for season
#AllH <- AllH %>% filter(MakePlayoffs>0,!str_detect(Expected.Return,"Out for the season"))
#AllP <- AllP %>% filter(MakePlayoffs>0,!str_detect(Expected.Return,"Out for the season"))
AllH <- AllH %>% filter(MakePlayoffs>0)
AllP <- AllP %>% filter(MakePlayoffs>0)

#df <- AllP %>% filter(str_length(Expected.Return) == 0)



#Generate dollars
# Adjusted for short season
#nlist <- preLPP(AllH,AllP,data.frame(),1,25,40)
#nlist <- preLPP(AllH,AllP)
nlist <- dociiDollars(AllH,AllP)


bhitters <- nlist[[1]]
bpitchers <- nlist[[2]]

# Incorporate scores back into AllH, AllP
AllH <- left_join(AllH,bhitters,by=c('playerid'))
AllP <- left_join(AllP,bpitchers,by=c('playerid'))
AllH <- rename(AllH,pDFL=zDFL)
AllP <- rename(AllP,pDFL=zDFL)
AllH$pDFL <- replace(AllH$pDFL,is.na(AllH$pDFL),0)
AllP$pDFL <- replace(AllP$pDFL,is.na(AllP$pDFL),0)



# Create doci dollars
AllH <- mutate(AllH,doci = pDFL*WinLDS)
AllP <- mutate(AllP,doci = pDFL*WinLDS)

newH <- select(AllH,Player,Rank,MLB,pDFL,doci,Injury,Expected.Return) %>% filter(doci > 0)
newP <- select(AllP,Player,Rank,MLB,pDFL,doci,Injury,Expected.Return) %>% filter(doci > 0)

###  Create Reports  ###
#write.csv(newH,"preH.csv")
#write.csv(newP,"preP.csv")

if (whichFile == "new") {
  write.csv(newH,"newH.csv")
  write.csv(newP,"newP.csv")
} else {
  write.csv(newH,"yearH.csv")
  write.csv(newP,"yearP.csv")
}




# Questions
# Play around with different multipliers - maybe make it to division series?
# what about returning injured players?  Luke Voit and the Yankee OFs?

newh <- read.csv("newH.csv",stringsAsFactors = FALSE)
newp <- read.csv("newP.csv",stringsAsFactors = FALSE)
yearh <- read.csv("yearH.csv",stringsAsFactors = FALSE)
yearp <- read.csv("yearP.csv",stringsAsFactors = FALSE)
preh <- read.csv("preH.csv",stringsAsFactors = FALSE)
prep <- read.csv("preP.csv",stringsAsFactors = FALSE)

#otherh <- anti_join(fullh,lasth,by=c('Player'))
#otherp <- anti_join(fullp,lastp,by=c('Player'))

combinedH <- rename(newh,newD = doci) %>% select(-X,-pDFL,-Rank)
combinedP <- rename(newp,newD = doci) %>% select(-X,-pDFL,-Rank)
yearh <- select(yearh,Player,MLB,yearD = doci)
yearp <- select(yearp,Player,MLB,yearD = doci)
preh <- select(preh,Player,MLB,preD = doci)
prep <- select(prep,Player,MLB,preD = doci)

combinedH <- full_join(combinedH,yearh,by=c('Player','MLB'))
combinedH <- full_join(combinedH,preh,by=c('Player','MLB'))
combinedP <- full_join(combinedP,yearp,by=c('Player','MLB'))
combinedP <- full_join(combinedP,prep,by=c('Player','MLB'))

# Change NA to 0
combinedH$newD <- replace(combinedH$newD,is.na(combinedH$newD),0)
combinedH$yearD <- replace(combinedH$yearD,is.na(combinedH$yearD),0)
combinedH$preD <- replace(combinedH$preD,is.na(combinedH$preD),0)
combinedP$newD <- replace(combinedP$newD,is.na(combinedP$newD),0)
combinedP$yearD <- replace(combinedP$yearD,is.na(combinedP$yearD),0)
combinedP$preD <- replace(combinedP$preD,is.na(combinedP$preD),0)


combinedH <- mutate(combinedH,allD = newD +yearD + preD) %>% arrange(-allD)
combinedH <- combinedH %>% select(Player,MLB,allD,newD,yearD,preD,Injury,Expected.Return)

combinedP <- mutate(combinedP,allD = newD +yearD + preD) %>% arrange(-allD)
combinedP <- combinedP %>% select(Player,MLB,allD,newD,yearD,preD,Injury,Expected.Return)

write.csv(combinedH,"finaldociH.csv")
write.csv(combinedP,"finaldociP.csv")


# Next round update

# scrape Craig's page
url <- "http://craigandmary.com/DOCI/DOCIStandings.php"
page <- read_html(url) %>% html_nodes("table") %>% html_table(,header=TRUE,fill=TRUE)
p2 <- tail(page,18)
ndf <- bind_rows(p2)
taken <- select(ndf,Player=Name)

fH <- read.csv("finaldociH.csv",stringsAsFactors = FALSE)
fP <- read.csv("finaldociP.csv",stringsAsFactors = FALSE)

fH <- anti_join(fH,taken)
fP <- anti_join(fP,taken)


# Teams left
Teams <- tibble(MLB=c("LAD","ATL","HOU","TB"))

fH <- inner_join(fH,Teams)
fP <- inner_join(fP,Teams)
