library("xlsx")
library("stringr")
library("dplyr")


hitters <- read.csv("steamerHROS.csv")
colnames(hitters) <- str_join('p',colnames(hitters))
hitters$Player <- as.character(hitters$pName)
pitchers <- read.csv("steamerPROS.csv")
colnames(pitchers) <- str_join('p',colnames(pitchers))
pitchers$Player <- as.character(pitchers$pName)


pl <- data.frame(c('Jason Kubel','Marlon Byrd','Matt Joyce','Jay Bruce'))
colnames(pl) <- c('Player')
pl$Player <- as.character(pl$Player)
t <- inner_join(pl,hitters,by=c('Player'),copy=FALSE)
t2 <- select(t,Player,pOPS,pHR,pRBI,pR,pSB,pAVG)

