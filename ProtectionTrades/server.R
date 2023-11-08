
setwd("../code/")
source("./protectionList.r")
setwd("../LeagueEval")

teams <- sort(unique(as.character(totals$Team)))

pullPlayers <- function(tm) {
  res <- filter(rpreds,Team == tm,Value > 1) %>% arrange(-Value) %>% mutate(Rank=rank(-Value)) %>% 
    select(-Team,Rank,Player:Expected.Return)
}

aggHitters <- function(tm,pos) {
  res <- filter(rpreds,Team == tm,Value > 1,!(Pos %in% c('SP','CL','MR'))) %>% 
    arrange(-Value) %>% mutate(Rank=rank(-Value)) %>%
    select(-Team,Rank,Player:Expected.Return) %>% group_by(Pos) %>%
    summarize(Players = length(Pos),TSalary = sum(Salary),TValue = sum(Value))
}

aggPitchers <- function(tm,pos) {
  res <- filter(rpreds,Team == tm,Value > 1,(Pos %in% c('SP','CL','MR'))) %>% 
    arrange(-Value) %>% mutate(Rank=rank(-Value)) %>%
    select(-Team,Rank,Player:Expected.Return) %>% group_by(Pos) %>%
    summarize(Players = length(Pos),TSalary = sum(Salary),TValue = sum(Value))
}



shinyServer(function(input, output,session) {
  updateSelectizeInput(session, 'e1', choices = teams, selected = 'Liquor Crickets')
  output$tname <- renderText({ input$e1 })
  output$totals <- renderDataTable({ totals })
  output$Players <- renderDataTable({ pullPlayers(input$e1) })
  output$THitters <- renderDataTable({ aggHitters(input$e1) })
  output$TPitchers <- renderDataTable({ aggPitchers(input$e1) })
  bh <- reactive({ as.data.frame(rpreds) %>% filter(valueRatio > input$rath, Value>input$pdh,
                                                    pDFL>input$hdfl,Pos!='SP',Pos!='CL') %>% 
                     arrange(-Value) %>% select(Player,Team,Pos:Expected.Return) })
  bp <- reactive({ as.data.frame(rpreds) %>% filter((Pos=='SP' | Pos=='CL'),
                                                    valueRatio > input$ratp,Value>input$pdp,
                                                    pDFL>input$pdfl) %>% 
                     arrange(-Value) %>% select(Player,Team,Pos:Expected.Return) })
  
  
  
  output$bp <- renderDataTable({ bp() })
  output$bh <- renderDataTable({ bh() })

})
