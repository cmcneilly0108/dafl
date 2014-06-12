# Combine scores back in to full list, put $0 for the rest - how sort the $0 players?
# Better prospect analysis
# Improve preseason holds projections based on fangraphs bullpen role chart (saves too?)

library("xlsx")
library("dplyr")

source("./daflFunctions.r")

# Start processing - load Past totals
l1 <- loadPast2()
eras <- l1[[1]]
avgs <- l1[[2]]
r3 <- l1[[3]]


#Read in projections
hitters <- read.csv("steamerH2014.csv")
hitters$SGP <- hitSGP(hitters)

pitchers <- read.csv("steamerP2014.csv")
pitchers$Name <- as.character(pitchers$Name)
# Use past year
lyp <- read.csv("AllP2013.csv",skip=1)
lyp$Player <- as.character(lyp$Player)
lyp$Player <- unlist(lapply(lyp$Player,swapName2))
lyp <- select(lyp,Player,HD)
colnames(lyp) <- c('Name','HLD')
# Bug in the join
pitchers <- left_join(pitchers,lyp,by=c('Name')) %>% arrange(-HLD)
pitchers$HLD <- replace(pitchers$HLD,is.na(pitchers$HLD),0)
pitchers$HLD <- replace(pitchers$HLD,pitchers$SV>9,0)

pitchers$SGP <- pitSGP(pitchers)

# Dollar Calculations

#Set parameters
nteams <- 15
tdollars <- nteams * 260
# 63/37 split - just guessing
pdollars <- round(tdollars*0.37)
hdollars <- tdollars - pdollars
# 13/12 hitters/pitchers based on rosters on 5/29/14
nhitters <- 12
npitchers <- 13
thitters <- nhitters * nteams
tpitchers <- npitchers * nteams

# Remove protected players
protected <- read.xlsx("2014ProtectionLists.xls",1)
protected$Name <- as.character(protected$Name)
protected$Name <- unlist(lapply(protected$Name,swapName))
protected <- filter(protected,Contract > 1)
pp <- filter(protected,POS=='P') %>% summarize(np = length(Name),sp = sum(Salary))
ph <- filter(protected,POS!='P') %>% summarize(np = length(Name),sp = sum(Salary))
thitters <- thitters - ph[1,1]
tpitchers <- tpitchers - pp[1,1]
hdollars <- hdollars - ph[1,2]
pdollars <- pdollars - pp[1,2]
hitters <- anti_join(hitters,protected,by=c('Name'))
pitchers <- anti_join(pitchers,protected,by=c('Name'))


# Back to core processing
# Reduce players to number worth bidding on
bhitters <- filter(hitters,rank(-SGP) <= thitters)
hitSGP <- round(sum(bhitters$SGP))
bpitchers <- filter(pitchers,rank(-SGP) <= tpitchers)
pitSGP <- round(sum(bpitchers$SGP))
hsgpd <- hdollars/hitSGP
psgpd <- pdollars/pitSGP

# Create dollar amounts
bhitters$DFL <- bhitters$SGP * hsgpd
bpitchers$DFL <- bpitchers$SGP * psgpd
bhitters <- arrange(bhitters,-DFL)
bpitchers <- arrange(bpitchers,-DFL)

# find min $, subtract from everyone, then multiply everyone by %diff
# Normalize for auction - three iterations
hmin <- min(bhitters$DFL) - 1
hlost <- hmin * thitters
bhitters$DFL <- (bhitters$DFL - hmin) * (hdollars/(hdollars - hlost))
hmin <- min(bhitters$DFL) - 1
hlost <- hmin * thitters
bhitters$DFL <- (bhitters$DFL - hmin) * (hdollars/(hdollars - hlost))
hmin <- min(bhitters$DFL) - 1
hlost <- hmin * thitters
bhitters$DFL <- (bhitters$DFL - hmin) * (hdollars/(hdollars - hlost))

pmin <- min(bpitchers$DFL) - 1
plost <- pmin * tpitchers
bpitchers$DFL <- (bpitchers$DFL - pmin) * (pdollars/(pdollars - plost))
pmin <- min(bpitchers$DFL) - 1
plost <- pmin * tpitchers
bpitchers$DFL <- (bpitchers$DFL - pmin) * (pdollars/(pdollars - plost))
pmin <- min(bpitchers$DFL) - 1
plost <- pmin * tpitchers
bpitchers$DFL <- (bpitchers$DFL - pmin) * (pdollars/(pdollars - plost))

