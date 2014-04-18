# Generate category denominators
# Generate SGPs

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

atBats <- 480 * 9
avgavg <- (r2[r2$Category=='AVG','Top']+r2[r2$Category=='AVG','Bottom'])/2
hits <- round(atBats * avgavg)

atBats <- atBats *8 / 9
hits <- round(hits  *8 / 9)

#Read in projections
hitters <- read.csv("steamerH2014.csv")
hitters$SGP <- with(hitters,R/getd('R') + HR/getd('HR') + RBI/getd('RBI') + SB/getd('SB') +
                      (((hits + H)/(atBats + AB)) - avgavg)/getd('AVG'))

pitchers <- read.csv("steamerP2014.csv")
pitchers$SGP <- with(pitchers,W/getd('W') + SO/getd('K') + SV/getd('SV'))

# Add ERA
# Add Holds - to projections, to SGP calculations
#Convert SGP to $$$