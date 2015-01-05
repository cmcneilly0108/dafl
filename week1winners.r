# Strip out protected players
# Add in previous DFL numbers - want to adjust prices

library("xlsx")
library("stringr")
library("dplyr")
library("XML")
library("ggplot2")
library("reshape2")

source("./daflFunctions.r")

# Load accrued stats
hitters <- read.cbs("AllH2014wk1.csv")
hitters <- rename(hitters,pHR=HR,pRBI=RBI,pR=R,pSB=SB,pH=H,pAB=AB)
hitters$pSGP <- hitSGP(hitters)

pitchers <- read.cbs("AllP2014wk1.csv")
pitchers <- rename(pitchers,pSV=S,pHLD=HD,pIP=INN,pW=W,pSO=K) %>% mutate(pER=(ERA*pIP)/9)
pitchers$pSGP <- pitSGP(pitchers)

#Generate dollars
nlist <- postDollars(hitters,pitchers)
AllH <- nlist[[1]]
AllP <- nlist[[2]]

AllH <- AllH %>% mutate(pDFL = pDFL/26)
AllP <- AllP %>% mutate(pDFL = pDFL/26)


bh <- AllH %>% filter(pDFL > 3) %>%  arrange(-pDFL,-pSGP) %>%
  select(Player,MLB,Pos,DFL=pDFL,SGP=pSGP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=BA)
bp <- AllP %>% filter(pDFL > 1) %>% arrange(-pDFL,-pSGP) %>%
  select(Player,MLB,DFL=pDFL,SGP=pSGP,W=pW,SO=pSO,ERA,SV=pSV,HLD=pHLD)



# Create spreadsheet
review <- createWorkbook()
tabs <- list()
tabs[[length(tabs)+1]] <- list('Hitters',bh)
tabs[[length(tabs)+1]] <- list('Pitchers',bp)

lapply(tabs,addSheet,review)
saveWorkbook(review,"Week1Winners.xlsx")
