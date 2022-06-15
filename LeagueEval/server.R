library("plotly")
library("bslib")

setwd("../code/")
source("./inSeasonPulse.r")
#setwd("../LeagueEval")

teams <- sort(unique(RTot$Team))

htrend <- read.csv("./hTrend.csv")
htrend$date <- mdy(htrend$date)


shinyServer(function(input, output,session) {
  updateSelectizeInput(session, 'e1', choices = teams, selected = 'Liquor Crickets')
#  steamH <- reactive({ pullTeam(input$e1)[[1]]  })
  
#  updateSelectInput(session, 'rteam', choices = as.list(teams))
#  output$TeamH <- renderGvis({ gvisTable(pullTeam('Liquor Crickets')[[1]])})
  output$tname <- renderText({ input$e1 })
#  output$TeamH <- renderGvis({ gvisTable(steamH())})
  output$TeamH <- renderDataTable({ pullTeam(input$e1)[[1]] })
  output$TeamP <- renderDataTable({ pullTeam(input$e1)[[2]] })
  output$RTot <- renderDataTable({ RTot })
  output$problems <- renderDataTable({ problems })
  
  tprof <- reactive({
    ifelse(input$e2 %in% c('SP','MR','CL'),df<-AllP,df<-AllH)
    f <- df %>% filter(Pos == input$e2,pDFL > input$pd) %>% group_by(Team) %>% summarize(nGood = length(Team))
    f2 <- df %>% filter(Pos == input$e2) %>% group_by(Team) %>% summarize(nTotal = length(Team))
    ff <- left_join(f2,f,by=c('Team')) %>% arrange(-nGood,-nTotal)
  })
  output$tprofile <- renderDataTable({tprof()})
  uh <- reactive({
    unHit <- arrange(AllH,-diffscore) %>% 
      filter(!(Team %in% c('Free Agent','Liquor Crickets')),diffscore > 0,hotscore > input$hsc,pDFL > input$upd, Salary > input$sal) %>% 
      select(Player, Team, Salary, Contract, pDFL, diffscore, hotscore, ytdscore,pScore)
  })
  output$undH <- renderDataTable({uh()})
  up <- reactive({
    unPit <- arrange(AllP,-diffscore) %>% 
      filter(!(Team %in% c('Free Agent','Liquor Crickets')),diffscore > 0,hotscore > input$hsc,pDFL > input$upd) %>% 
      select(Player, Team, pDFL, diffscore, hotscore, ytdscore,pScore)
  })
  output$undP <- renderDataTable({up()})

  # Top By Position
  topPos <- reactive({
#    f <- df %>% filter(Pos == input$e3,pDFL > input$pd) %>% group_by(Team) %>% summarize(nGood = length(Team))
#    f2 <- df %>% filter(Pos == input$e3) %>% group_by(Team) %>% summarize(nTotal = length(Team))
#    ff <- left_join(f2,f,by=c('Team')) %>% arrange(-nGood,-nTotal)
    ifelse(input$e3 %in% c('SP','MR','CL'),
           ff <- AllP %>% filter(Pos == input$e3) %>% arrange(-pDFL) %>% 
             select(Player,Pos,pDFL,Team,Salary,Contract,pSGP,Rank,pW,pSO,pSV,pHLD,pERA,pK.9,pFIP,W,K,S,HD,ERA,hotscore,LVG,Injury,Expected.Return),
           ff <- AllH %>% filter(str_detect(posEl,input$e3)) %>%
             select(Player,Pos,Age,pDFL,Team,pSGP,Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,AVG,hotscore,Injury,Expected.Return) %>%
             arrange(-pDFL)
    )
    ff
  })
  output$topPlayers <- renderDataTable({topPos()})

  output$lcgraph <- renderPlotly({
    plot_ly(htrend, x = ~date, y = ~hotscore)  %>%
      filter(Player %in% input$choice) %>%
      group_by(Player) %>%
      add_lines(color=~Player)
  })
  output$g1 <- renderPlot(print(g1),res=120)
  output$g2 <- renderPlot(print(g2),res=120)
  output$g3 <- renderPlot(print(g3),res=120)
  
})
