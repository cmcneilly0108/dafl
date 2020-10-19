
setwd("../")
source("./inSeasonPulse.r")
setwd("./LeagueEval")

teams <- sort(unique(RTot$Team))



shinyServer(function(input, output,session) {
  updateSelectizeInput(session, 'e1', choices = teams, selected = 'Liquor Crickets')
#  steamH <- reactive({ pullTeam(input$e1)[[1]]  })
  
#  updateSelectInput(session, 'rteam', choices = as.list(teams))
#  output$TeamH <- renderGvis({ gvisTable(pullTeam('Liquor Crickets')[[1]])})
  output$tname <- renderText({ input$e1 })
#  output$TeamH <- renderGvis({ gvisTable(steamH())})
  output$TeamH <- renderDataTable({ pullTeam(input$e1)[[1]] })
  output$TeamP <- renderDataTable({ pullTeam(input$e1)[[2]] })

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


})
