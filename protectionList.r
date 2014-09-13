#Generate based on 2014 projections

library("xlsx")
library("stringr")
library("dplyr")
library("XML")
library("ggplot2")
library("reshape2")

source("./daflFunctions.r")

# Year End Totals
sTots <- list()

l1 <- loadPast2()
eras <- l1[[1]]
avgs <- l1[[2]]
r3 <- l1[[3]]

#Load steamer data
hitters <- read.csv("steamerH2014.csv")
hitters$SGP <- hitSGP(hitters)
colnames(hitters) <- str_join('p',colnames(hitters))
hitters$Player <- as.character(hitters$pName)

pitchers <- read.csv("steamerP2014.csv")
colnames(pitchers) <- str_join('p',colnames(pitchers))
pitchers$Player <- as.character(pitchers$pName)

#Load 2013 final rosters
rosters <- read.xlsx("2013+Season+Ending+Rosters.xlsx",1)
rosters$Player <- as.character(rosters$Name..Qual.Pos.)
rosters$Player <- unlist(lapply(rosters$Player,swapName2))
rosters <- filter(rosters,X2014.Contract != 'NA')
#split into P,H tables
rHitters <- filter(rosters,Pos != 'P')
rPitchers <- filter(rosters,Pos == 'P')

#merge with steamer
AllH <- inner_join(rHitters,hitters,by=c('Player'),copy=FALSE)
AllP <- inner_join(rPitchers,pitchers,by=c('Player'),copy=FALSE)


#assign $$$
#take top 12
#compare