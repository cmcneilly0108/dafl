
library("xlsx")
library("dplyr")

# Load standings csv
results <- read.xlsx("DAFLSGP.xlsx",2)
teams <- read.xlsx("DAFLSGP.xlsx",3)

r2 <- inner_join(results,teams,by=c('Year'),copy=FALSE)

# figure out ERA, AVG
r2$denom <- with(r2,(Top - Bottom)/Teams)

r3 <- r2 %.% group_by(Category) %.% summarize(ad = mean(denom))

getd <- function(c) {
  r3[r3$Category==c,'ad']
}

atBats <- 550 * 9
avgavg <- mean((r2[r2$Category=='AVG','Top']+r2[r2$Category=='AVG','Bottom'])/2)
hits <- round(atBats * avgavg)

atBats <- atBats *8 / 9
hits <- round(hits  *8 / 9)

#Read in projections
hitters <- read.csv("steamerH2014.csv")
hitters$SGP <- with(hitters,R/getd('R') + HR/getd('HR') + RBI/getd('RBI') + SB/getd('SB') +
                      (((hits + H)/(atBats + AB)) - avgavg)/getd('AVG'))

innpit <- (6 * 200) + (2 * 75)
avgera <- mean((r2[r2$Category=='ERA','Top']+r2[r2$Category=='ERA','Bottom'])/2)
eruns <-  (avgera/9) * innpit
innpit <- innpit * 7/8
eruns <- eruns * 7/8

pitchers <- read.csv("steamerP2014.csv")
pitchers$SGP <- with(pitchers,W/getd('W') + SO/getd('K') + SV/getd('SV') +
                       ((((eruns+ER)/(innpit+IP)*9) - avgera)/getd('ERA')))
pitchers$SGP2 <- with(pitchers,W/getd('W') + SO/getd('K') + SV/getd('SV'))
pitchers$ES <- with(pitchers,(avgera - ((eruns+ER)/(innpit+IP)*9))/getd('ERA'))

# Dollar Calculations
nteams <- 15
tdollars <- nteams * 260
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