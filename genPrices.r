# Make sure Holds are included in SGP calculations/data
# Take protections lists into account - Keeper League Inflation
# Calculate H/P split (dollars AND players) from 2014 draft - do we have others?

# Check for pitching accuracy
# Combine scores back in to full list, put $0 for the rest - how sort the $0 players?

library("xlsx")
library("dplyr")

source("./daflFunctions.r")

# Start processing - load Past totals
#l1 <- loadPast()
l1 <- loadPast2()
r2 <- l1[[1]]
r3 <- l1[[2]]


#Read in projections
hitters <- read.csv("steamerH2014.csv")
hitters$SGP <- hitSGP(hitters)

pitchers <- read.csv("steamerP2014.csv")
pitchers$SGP <- pitSGP(pitchers)

# Dollar Calculations

#Set parameters
nteams <- 15
tdollars <- nteams * 260
# 63/37 split - just guessing
pdollars <- round(tdollars*0.37)
hdollars <- tdollars - pdollars
# 13/12 hitters/pitchers based on rosters on 5/29/14
nhitters <- 13
npitchers <- 12
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

