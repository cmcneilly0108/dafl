
setwd("../code/")
source("./protectionList.r")
setwd("../LeagueEval")

teams <- sort(unique(as.character(totals$Team)))

pullPlayers <- function(tm) {
#  res <- filter(rpreds,Team == tm,netValue > 1) %>% arrange(-netValue) %>% mutate(Rank=rank(-Value)) %>% 
  res <- filter(rpreds,Team == tm) %>% arrange(-netValue) %>% mutate(Rank=rank(-netValue)) %>% 
    select(-Team,Rank,Player:Expected.Return)
}

aggHitters <- function(tm,pos) {
  res <- filter(rpreds,Team == tm,netValue > 1,!(Pos %in% c('SP','CL','MR'))) %>% 
    arrange(-netValue) %>% mutate(Rank=rank(-netValue)) %>%
    select(-Team,Rank,Player:Expected.Return) %>% group_by(Pos) %>%
    summarize(Players = length(Pos),TSalary = sum(Salary),TValue = sum(Value))
}

aggPitchers <- function(tm,pos) {
  res <- filter(rpreds,Team == tm,netValue > 1,(Pos %in% c('SP','CL','MR'))) %>% 
    arrange(-netValue) %>% mutate(Rank=rank(-netValue)) %>%
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
  bh <- reactive({ as.data.frame(rpreds) %>% filter(pADP > input$hadp, netValue>input$netVh,
                                                    pDFL>input$hdfl,Pos!='SP',Pos!='CL') %>% 
                     arrange(-netValue) %>% select(Player,Team,Pos:netValue) })
  bp <- reactive({ as.data.frame(rpreds) %>% filter((Pos=='SP' | Pos=='CL'),
                                                    pADP > input$padp,netValue>input$netVp,
                                                    pDFL>input$pdfl) %>% 
                     arrange(-netValue) %>% select(Player,Team,Pos:netValue) })
  
  
  
  output$bp <- renderDataTable({ bp() })
  output$bh <- renderDataTable({ bh() })

})
