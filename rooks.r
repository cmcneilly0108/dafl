

m2 <- select(master,playerid,Pos,MLB,birth_year)
dfh <- read.csv("steamerHROS.csv",stringsAsFactors=FALSE)
colnames(dfh) <- str_join('p',colnames(dfh))
dfh <- rename(dfh,playerid=pplayerid,Player=pName)
dfh <- anti_join(df,m2,by=c('playerid'),copy=FALSE)

dfp <- read.csv("steamerPROS.csv",stringsAsFactors=FALSE)
colnames(dfp) <- str_join('p',colnames(dfp))
dfp <- rename(dfp,playerid=pplayerid,Player=pName)
dfp <- anti_join(dfp,m2,by=c('playerid'),copy=FALSE)

df <- read.csv("steamerHROS.csv",stringsAsFactors=FALSE)
m2 <- select(master,playerid,Pos,MLB,birth_year)
colnames(df) <- str_c('p',colnames(df))
df <- rename(df,playerid=pplayerid,Player=pName)
df <- left_join(df,m2,by=c('playerid'),copy=FALSE)
df$birth_year <- replace(df$birth_year,is.na(df$birth_year),2010)
df <- mutate(df,Age=year(Sys.time())-birth_year)
dfh <- anti_join(df,m2,by=c('playerid'),copy=FALSE)
df$playerid <- ifelse(df$playerid %in% dfh$playerid,df$Player,df$playerid)



