
setwd("../code/")
source("./protectionList.r")
setwd("../LeagueEval")

teams <- sort(unique(as.character(totals$Team)))

pullPlayers <- function(tm) {
#  res <- filter(rpreds,Team == tm,netValue > 1) %>% arrange(-netValue) %>% mutate(Rank=rank(-Value)) %>% 
  res <- filter(rpreds,Team == tm) %>% arrange(-netValue) %>% mutate(Rank=rank(-netValue)) %>% 
    select(-Team,-rdOne,-Rank,Player:Expected.Return)
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
  #output$totals <- renderDataTable({ totals })
  dttotals <- datatable(totals,options = list(pageLength = 20,autoWidth = FALSE, paging = FALSE, searching = FALSE, info = FALSE)) %>% 
    formatCurrency(c('TotalValue', 'MoneyEarned','VPPlayer','PostDraftEst')) %>% formatRound(c('ValueRatio','zScore'),2)
  output$totals <- DT::renderDataTable({ dttotals })
  
  
  
  updateSelectizeInput(session, 'e1', choices = teams, selected = 'Liquor Crickets')
  output$tname <- renderText({ input$e1 })

  dtPlayers <- reactive({df <- datatable(pullPlayers(input$e1),options = list(pageLength = 20,autoWidth = FALSE, paging = FALSE, searching = FALSE, info = FALSE)) %>%
    formatRound(c('Age','pADP','s1','s2','s3','s4'),0) %>%
    formatRound(c('valueRatio'),3) %>%
    formatCurrency(c('pDFL','Value','netValue'))})
  output$Players <- DT::renderDataTable({ dtPlayers() })
  
  
  dtTHitters <- reactive({df <- datatable(aggHitters(input$e1),options = list(pageLength = 20,autoWidth = FALSE, paging = FALSE, searching = FALSE, info = FALSE)) %>%
    formatCurrency(c('TValue'))})
  output$THitters <- DT::renderDataTable({ dtTHitters() })
  
  dtTPitchers <- reactive({df <- datatable(aggPitchers(input$e1),options = list(pageLength = 20,autoWidth = FALSE, paging = FALSE, searching = FALSE, info = FALSE)) %>%
    formatCurrency(c('TValue'))})
  output$TPitchers <- DT::renderDataTable({ dtTPitchers() })
  
  
  
  bh <- reactive({ as.data.frame(rpreds) %>% filter(pADP > input$hadp, netValue>input$netVh,
                                                    pDFL>input$hdfl,Pos!='SP',Pos!='CL') %>% 
                     arrange(-netValue) %>% select(Player,Team,Pos:netValue) })
  bp <- reactive({ as.data.frame(rpreds) %>% filter((Pos=='SP' | Pos=='CL'),
                                                    pADP > input$padp,netValue>input$netVp,
                                                    pDFL>input$pdfl) %>% 
                     arrange(-netValue) %>% select(Player,Team,Pos:netValue) })
  
  
  
  dtbp <- reactive({df <- datatable(bp(),options = list(pageLength = 20,autoWidth = FALSE)) %>%
    formatRound(c('Age','pADP','s1','s2','s3','s4'),0) %>%
    formatCurrency(c('pDFL','Value','netValue'))})
  output$bp <- DT::renderDataTable({ dtbp() })
  
  #output$bh <- renderDataTable({ bh() })
  dtbh <- reactive({df <- datatable(bh(),options = list(pageLength = 20,autoWidth = FALSE)) %>%
    formatRound(c('Age','pADP','s1','s2','s3','s4'),0) %>%
    formatCurrency(c('pDFL','Value','netValue'))})
  output$bh <- DT::renderDataTable({ dtbh() })
  
})
