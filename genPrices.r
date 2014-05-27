
library("xlsx")
library("dplyr")

source("./daflFunctions.r")
# ((3.98-((ER +469)*(9/(IP+1060))))/.08)

# Start processing - load Past totals
l1 <- loadPast()
r2 <- l1[[1]]
r3 <- l1[[2]]


#Read in projections
hitters <- read.csv("steamerH2014.csv")
hitters$SGP <- hitSGP(hitters)


pitchers <- read.csv("steamerP2014.csv")
pitchers$SGP <- pitSGP(pitchers)

# Dollar Calculations
nteams <- 15
tdollars <- nteams * 260
# 67/33 split
pdollars <- round(tdollars/3)
hdollars <- tdollars - pdollars
nhitters <- 12
npitchers <- 13
thitters <- nhitters * nteams
tpitchers <- npitchers * nteams
# find top players total SGPs

bhitters <- filter(hitters,rank(-SGP) <= thitters)
hitSGP <- round(sum(bhitters$SGP))
bpitchers <- filter(pitchers,rank(-SGP) <= tpitchers)
pitSGP <- round(sum(bpitchers$SGP))
hsgpd <- hdollars/hitSGP
psgpd <- pdollars/pitSGP

bhitters$DFL <- bhitters$SGP * hsgpd
bpitchers$DFL <- bpitchers$SGP * psgpd
bhitters <- arrange(bhitters,-DFL)
bpitchers <- arrange(bpitchers,-DFL)

# find min $, subtract from everyone, then multiply everyone by %diff
hmin <- min(bhitters$DFL) - 1
hlost <- hmin * thitters
#bhitters$DFL <- bhitters$DFL - hmin
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

#Check for pitching accuracy
#Combine scores back in to full list, put $0 for the rest

# Take protections lists into account
# Add Holds - to projections, to SGP calculations
#Convert SGP to $$$