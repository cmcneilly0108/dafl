
setwd("../code/")
source("./inSeasonPulse.r")
#setwd("../LeagueEval")

teams <- sort(unique(RTot$Team))


shinyServer(function(input, output,session) {
  htrend <- read.csv("./hTrend.csv")
  htrend$date <- ymd(htrend$date)
  htrend$hotscore <- as.numeric(htrend$hotscore)
  
  updateSelectizeInput(session, 'e1', choices = teams, selected = 'Liquor Crickets')
  output$tname <- renderText({ input$e1 })

  dtTeamH <- reactive({ mdf <- datatable(pullTeam(input$e1)[[1]],options = list(pageLength = 20)) %>%
    formatCurrency('pDFL') %>% formatRound(c('pSGP','hotscore','pAVG'),3) %>% 
    formatRound(c('pHR','pRBI','pSB','pR','Age'),0)  })
  output$TeamH <- DT::renderDataTable({ dtTeamH() })

  dtStandFull <- datatable(dstandfull,options = list(pageLength = 20))
#    formatCurrency(c('hDFL', 'piDFL','tDFL')) %>% formatRound('zScore',2)
  output$StandFull <- DT::renderDataTable({ dtStandFull })
  
  
  dtTeamP <- reactive({ mdf <- datatable(pullTeam(input$e1)[[2]],options = list(pageLength = 20)) %>%
    formatCurrency('pDFL') %>% formatRound(c('pSGP','hotscore','pERA','pFIP','pK/9'),3) %>%
    formatRound(c('pW','pSO','pSV','pHLD','Age'),0) })
  output$TeamP <- DT::renderDataTable({ dtTeamP() })
#  dtTeamP <- reactive({ rdf <- datatable(pullTeam(input$e1)[[2]],options = list(pageLength = 20)) %>% 
#    formatCurrency('pDFL') %>% formatRound(c('pSGP','hotscore'),2) })
#  output$TeamP <- DT::renderDataTable({ dtTeamP })
  
#  output$TeamH <- DT::renderDataTable({ pullTeam(input$e1)[[1]] },
#                                      options = list(pageLength = 20))
#  output$TeamP <- DT::renderDataTable({ pullTeam(input$e1)[[2]] },
#                                      options = list(pageLength = 20))

  dtRTot <- datatable(RTot,options = list(pageLength = 20)) %>% 
    formatCurrency(c('hDFL', 'piDFL','tDFL')) %>% formatRound('zScore',2)
  output$RTot <- DT::renderDataTable({ dtRTot })

  dtRTotTop <- datatable(RTotTop,options = list(pageLength = 20)) %>% 
    formatCurrency(c('hDFL', 'piDFL','tDFL')) %>% formatRound('zScore',2)
  output$RTotTop <- DT::renderDataTable({ dtRTotTop })
  
  dtproblems <- datatable(problems,options = list(pageLength = 20)) %>% 
    formatRound('hotscore',2) %>% formatRound('Age',0) %>% formatCurrency('pDFL')
  output$problems <- DT::renderDataTable({ dtproblems },
                                         options = list(pageLength = 20))

  dtcatSummary <- datatable(catSummary,options = list(pageLength = 20)) %>% 
    formatRound(c('pvp','pvm','opportunity'),2)
  output$catSummary <- DT::renderDataTable({ dtcatSummary })
  
  tprof <- reactive({
    ifelse(input$e2 %in% c('SP','MR','CL'),df<-AllP,df<-AllH)
    f <- df %>% filter(Pos == input$e2,pDFL > input$pd) %>% group_by(Team) %>% summarize(nGood = length(Team))
    f2 <- df %>% filter(Pos == input$e2) %>% group_by(Team) %>% summarize(nTotal = length(Team))
    ff <- left_join(f2,f,by=c('Team')) %>% arrange(-nGood,-nTotal)
  })
  output$tprofile <- DT::renderDataTable({tprof()},
                                         options = list(pageLength = 20))
  
  uh <- reactive({
    unHit <- arrange(AllH,-diffscore) %>% 
      filter(!(Team %in% c('Free Agent','Liquor Crickets')),diffscore > 0,hotscore > input$hsc,pDFL > input$upd, Salary > input$sal) %>% 
      select(Player, Team, Salary, Contract, pDFL, diffscore, hotscore, ytdscore,pScore)
  })
  output$undH <- DT::renderDataTable({uh()},
                                     options = list(pageLength = 20))
  up <- reactive({
    unPit <- arrange(AllP,-diffscore) %>% 
      filter(!(Team %in% c('Free Agent','Liquor Crickets')),diffscore > 0,hotscore > input$hsc,pDFL > input$upd) %>% 
      select(Player, Team, pDFL, diffscore, hotscore, ytdscore,pScore)
  })
  output$undP <- DT::renderDataTable({up()},
                                     options = list(pageLength = 20))

#  Top By Position
  topPos <- reactive({
    ifelse(input$fa == TRUE,ffh <- filter(AllH,Team=='Free Agent'),ffh <- AllH)
    ifelse(input$fa == TRUE,ffp <- filter(AllP,Team=='Free Agent'),ffp <- AllP)
    ifelse(input$e3 %in% c('SP','MR','CL'),
           ff <- ffp %>% filter(Pos == input$e3) %>% arrange(-pDFL) %>%
             select(Player,Pos,Age,pDFL,Team,Salary,Contract,pSGP,Rank,'Pitching+',pW,pSO,pSV,pHLD,pERA,`pK/9`,pFIP,W,K,S,HD,ERA,hotscore,twostarts,LVG,Injury,Expected.Return),
           ifelse(input$e3 == 'Hitters',
                  ff <- ffh %>%
                    select(Player,Pos,Age,pDFL,Team,Salary,Contract,pSGP,Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,AVG,hotscore,Injury,Expected.Return) %>%
                    arrange(-pDFL),
                  ff <- ffh %>% filter(str_detect(posEl,input$e3)) %>%
                    select(Player,Pos,Age,pDFL,Team,Salary,Contract,pSGP,Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,AVG,hotscore,Injury,Expected.Return) %>%
                    arrange(-pDFL)
           )
    )
    ifelse(input$e3 %in% c('SP','MR','CL'),
           res <- datatable(ff,options = list(pageLength = 20), filter='top') %>% formatCurrency('pDFL') %>%
              formatRound(c('pSGP','hotscore','pERA','pK/9','pFIP','LVG'),2) %>% formatRound(c('Age','pW','pSO','pSV','pHLD'),0),
           res <- datatable(ff,options = list(pageLength = 20), filter='top') %>% formatCurrency('pDFL') %>%
             formatRound(c('pSGP','hotscore'),2) %>% formatRound(c('Age','pHR','pR','pRBI','pSB'),0) %>%
             formatRound(c('pAVG'),3)
    )
    res
  })
  #output$topPlayers <- DT::renderDataTable({ dtRTot })
  output$topPlayers <- DT::renderDataTable({topPos()})
  
  # RRClosers - rrcResults
  dtrrcResults <- datatable(rrcResults,options = list(pageLength = 20), filter='top') %>% 
    formatRound(c('pSGP','hotscore','LVG','pERA','pK/9','pBB/9'),3) %>% formatCurrency('pDFL') %>%
    formatRound(c('pW','pSO','pSV','pHLD'),0)
  output$rrcResults <- DT::renderDataTable({ dtrrcResults })
  
  output$lcgraph <- renderPlotly({
    plot_ly(htrend, x = ~date, y = ~hotscore)  %>%
      filter(Player %in% input$choice) %>%
      group_by(Player) %>%
      add_lines(color=~Player)
  })
  output$g1 <- renderPlot(print(g1),res=120)
  output$g2 <- renderPlot(print(g2),res=120)
  output$g3 <- renderPlot(print(g3),res=120)

  dtcTrades <- datatable(candTrades,options = list(pageLength = 20)) %>% 
    formatCurrency(c('pDFL'))
  output$cTrades <- DT::renderDataTable({ dtcTrades })

  dtProPit <- datatable(prospectP,options = list(pageLength = 20)) %>% formatRound(c('Age'),1)
  output$ProPit <- DT::renderDataTable({ dtProPit })
  dtProHit <- datatable(prospectH,options = list(pageLength = 20))  %>% formatRound(c('Age'),1)
  output$ProHit <- DT::renderDataTable({ dtProHit })

  dtinjOrig <- datatable(injOrig,options = list(pageLength = 20,autoWidth = FALSE, info = FALSE), filter='top') %>%
    formatCurrency('pDFL')
  output$injOrig <- DT::renderDataTable({dtinjOrig})
  
})
