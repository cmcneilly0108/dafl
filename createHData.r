

library("xlsx")
library("stringr")
library("dplyr")
library("XML")
library("ggplot2")
library("reshape2")
library("lubridate")
library("pitchRx")

offset <- 0
while (offset < 10000) {
#while (offset < 19321) {
  system(str_c("./bf.sh ",toString(offset)))
  offset <- offset + 300
}

rf <- readHTMLTable('p2015.html',stringsAsFactors=F)
f <- lapply(rf,function(x) {is.data.frame(x) && ncol(x) == 40})
c2 <- rf[unlist(f)]
d <- rbind.fill(c2)
d <- filter(d,!str_detect(Rk,'Rk'))
write.csv(d,'papp2015.csv')

pf <- read.csv('papp2014.csv',stringsAsFactors=F)
pf$Hld <- ifelse(str_detect(pf$App.Dec,'H'),1,0)

urls <- makeUrls(start = "2015-06-01", end = "2015-06-01")
library(XML2R)
files <- paste0(urls, "/inning/inning_all.xml")
obs <- XML2Obs(files, url.map = TRUE, quiet = TRUE)
table(names(obs))

tmp <- re_name(obs, equiv = c("game//inning//top//atbat",
                              "game//inning//bottom//atbat"), diff.name = "inning_side")
tmp <- re_name(tmp, equiv = c("game//inning//top//atbat//runner",
                              "game//inning//bottom//atbat//runner"), diff.name = "inning_side")
tmp <- re_name(tmp, equiv = c("game//inning//top//action",
                              "game//inning//bottom//action"), diff.name = "inning_side")
tmp <- re_name(tmp, equiv = c("game//inning//top//atbat//po",
                              "game//inning//bottom//atbat//po"), diff.name = "inning_side")
obs2 <- re_name(tmp, equiv = c("game//inning//top//atbat//pitch",
                               "game//inning//bottom//atbat//pitch"), diff.name = "inning_side")
table(names(obs2))
obswkey <- add_key(obs2, parent = "game//inning", recycle = "num", key.name = "inning")
obswkey <- add_key(obswkey, parent = "game//inning", recycle = "next")
obswkey <- add_key(obswkey, parent = "game//inning//atbat", recycle = "num")

tables <- collapse_obs(obswkey)
#As mentioned before, we do not need the 'inning' table
tables <- tables[!grepl("^game//inning$", names(tables))]
table.names <- c("game", "action", "atbat", "pitch", "po", "runner")
tables <- setNames(tables, table.names)
head(tables[["runner"]])


library(dplyr)
db <- src_sqlite("GamedayDB.sqlite3", create = TRUE)
# Collect and store all PITCHf/x data from 2008 to now
scrape(start = "2014-04-15", end = "2014-04-21",
       suffix = "inning/inning_all.xml", connect = db$con)
