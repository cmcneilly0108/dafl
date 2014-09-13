
library("xlsx")
library("stringr")
library("dplyr")
library("XML")
library("ggplot2")
library("reshape2")


source("./daflFunctions.r")

#Load Steamer rest of season projections
hitters <- read.csv("steamerHROS.csv")
colnames(hitters) <- str_join('p',colnames(hitters))
hitters$Player <- as.character(hitters$pName)

pitchers <- read.csv("steamerPROS.csv")
colnames(pitchers) <- str_join('p',colnames(pitchers))
pitchers$Player <- as.character(pitchers$pName)

#Load minor league stats
mhitters <- read.csv("minHitters.csv")
mhitters$Player <- as.character(mhitters$Name)
AminH <- inner_join(mhitters,hitters,by=c('Player'),copy=FALSE)

mpitchers <- read.csv("minPitchers.csv")
mpitchers$Player <- as.character(mpitchers$Name)
AminP <- inner_join(mpitchers,pitchers,by=c('Player'),copy=FALSE)

#Load All ML Players - Extract Free Agents
Allhitters <- read.csv("AllHitters.csv",skip=1)
Allhitters$Player <- as.character(Allhitters$Player)
Allhitters <- mutate(Allhitters, MLB = pullMLB(Player))
Allhitters <- mutate(Allhitters, Pos = pullPos(Player))
Allhitters$Player <- unlist(lapply(Allhitters$Player,swapName2))
Rhitters <- filter(Allhitters,Team != 'Free Agent')

AvMinH <- anti_join(AminH,Rhitters,by=c('Player'),copy=FALSE)

Allpitchers <- read.csv("AllPitchers.csv",skip=1)
Allpitchers$Player <- as.character(Allpitchers$Player)
Allpitchers$Team <- as.character(Allpitchers$Team)
Allpitchers <- mutate(Allpitchers, Pos = pullPos(Player))
Allpitchers <- mutate(Allpitchers, MLB = pullMLB(Player))
Allpitchers$Player <- unlist(lapply(Allpitchers$Player,swapName2))
Allpitchers$Pos <- with(Allpitchers,ifelse(Pos=='SP','SP',ifelse(S>HD,'CL',ifelse(HD>0,'MR','SP'))))
Rpitchers <- filter(Allpitchers,Team != 'Free Agent')

AvMinP <- anti_join(AminP,Rpitchers,by=c('Player'),copy=FALSE)
