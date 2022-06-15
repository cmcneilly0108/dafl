library(tidyr)
library(dplyr)
library(stringr)
library("lubridate")
library(rvest)

source('./daflFunctions.r')

year <- '2021'

getAge <- function(playerid) {
  print(playerid)
  page <- read_html(str_c("https://www.fangraphs.com/players/nolan-jones/",playerid,"/stats"))
  byear <- page %>% html_node(".player-info__bio-birthdate td") %>% html_text() %>% 
    str_extract("[0-9/]{8,}") %>% mdy() %>% year()
  byear
}

read.cbs2 <- function(fn) {
  df <- read.csv(fn,skip=1,stringsAsFactors=FALSE, encoding="UTF-8")
  df <- mutate(df, Pos = pullPos(Player))
  df <- mutate(df, MLB = pullMLB(Player))
  df$Player <- unlist(lapply(df$Player,stripName))
  # Team abbreviations are not the same - find all discrepancies WAS->WSH
  df$MLB <- replace(df$MLB,df$MLB=='WAS','WSH')
  df$MLB <- replace(df$MLB,df$MLB=='CHW','CWS')

  # Create Team column
  #df <- mutate(df,Avail=str_replace(Avail,'\\.\\.\\.',''))
  #df <- left_join(df,nicks,by=c('Avail'))
  df$Team <- df$Avail
  #df$Team <- str_replace(df$Team,'W','Free Agent')
  df$Team <- ifelse(str_detect(df$Team,'W '),'Free Agent',df$Team)
  df <- select(df,-Avail)
  #addPlayerid(df)
}

# Start with previous mymaster (playerid,Player,birth_year,cbs_name)
mymaster <- read.csv("../mymaster.csv",stringsAsFactors=FALSE) %>% select(-X)

#cleanup any blank cbs_name
mymaster <- mutate(mymaster,cbs_name = ifelse(cbs_name=='',Player,cbs_name))

