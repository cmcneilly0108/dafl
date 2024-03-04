# Move a player from FA to a Team, update both lists
# GUI to pick a player


setwd("../code/")
source("./draftGuide.r")
setwd("../draftTool")

teams <- sort(unique(pstandings$Team))
hpos <- list('C','1B','2B','SS','3B','OF')
ppos <- list('SP','MR','CL')

tProtect <- function(tm) {
  res <- filter(protClean,Team == tm ) %>% select(-Team,-playerid) %>% arrange(-pDFL)
}

tpSummary <- function(tm) {
  res <- filter(currentSummary,Team == tm )
}

hitPlayersbyPos <- function(pos) {
  res <- AllH %>% filter(Pos == pos | str_detect(posEl,pos),pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% dplyr::rename(DFL=pDFL,SGP=pSGP)
  res <- mutate(res,RPV = (SGP - aRPV(pc,nrow(filter(pc,DFL>0))))/aRPV(pc,nrow(filter(pc,DFL>0))))
  res <- select(res,Player,MLB,posEl,Age,DFL,RPV,SGP,orank,ADP=pADP,HR=pHR,RBI=pRBI,R=pR,SB=pSB,AVG=pAVG,Injury,Expected.Return)
}

pitPlayersbyPos <- function(pos) {
  res <- AllP %>% filter(Pos==pos,pSGP > 0) %>% arrange(-pDFL,-pSGP) %>% dplyr::rename(DFL=pDFL,SGP=pSGP) %>% head(200)
  res <- mutate(res,RPV = (SGP - aRPV(psp,nrow(filter(psp,DFL>0))))/aRPV(psp,nrow(filter(psp,DFL>0))))
  res <- select(res,Player,MLB,Age,DFL,RPV,SGP,orank,ADP=pADP,W=pW,SO=pSO,ERA=pERA,SV=pSV,HLD=pHLD,Injury,Expected.Return)
}

shinyServer(function(input, output,session) {
  dtpstandings <- datatable(pstandings,options = list(pageLength = 20,autoWidth = FALSE, paging = FALSE, searching = FALSE, info = FALSE)) %>% 
    formatCurrency(c('TotalValue', 'Earned','VPPlayer','DPP','FullValue')) %>% formatRound(c('ValueRatio','zScore'),2)
  output$pstandings <- DT::renderDataTable({ dtpstandings })

  dtprotectSummary <- datatable(protectSummary,options = list(pageLength = 20,autoWidth = FALSE, paging = FALSE, searching = FALSE, info = FALSE)) %>%
    formatRound(c('playersProt','ToFill','valueTaken'),2)
  output$protectSummary <- DT::renderDataTable({ dtprotectSummary })
  dtppp <- datatable(ppp,options = list(pageLength = 20,autoWidth = FALSE, paging = FALSE, searching = FALSE, info = FALSE))
  output$ppp <- DT::renderDataTable({ dtppp })
  
  
  updateSelectizeInput(session, 'e1', choices = teams, selected = 'Liquor Crickets')
  output$tname <- renderText({ input$e1 })

  dttProtect <- reactive({df <- datatable(tProtect(input$e1),options = list(pageLength = 20,autoWidth = FALSE, paging = FALSE, searching = FALSE, info = FALSE)) %>%
                           formatRound('pDFL',2)})
  output$tProtect <- DT::renderDataTable({ dttProtect() })
  dttpSummary <- reactive({df <- datatable(tpSummary(input$e1),options = list(pageLength = 20,autoWidth = FALSE, paging = FALSE, searching = FALSE, info = FALSE)) %>%
    formatRound('salleft',2)})
  output$tpSummary <- DT::renderDataTable({ dttpSummary() })
  
  updateSelectizeInput(session, 'e2', choices = hpos, selected = 'OF')
  output$hpos <- renderText({ input$e2 })

  dthpbpos <- reactive({df <- datatable(hitPlayersbyPos(input$e2),options = list(pageLength = 20,autoWidth = FALSE, searching = FALSE, info = FALSE)) %>%
    formatRound(c('Age','ADP','HR','RBI','R','SB'),0) %>% formatCurrency('DFL') %>%
    formatRound(c('RPV','SGP','AVG'),3)
  })
  output$hpbpos <- DT::renderDataTable({ dthpbpos() })

  updateSelectizeInput(session, 'e3', choices = ppos, selected = 'SP')
  output$ppos <- renderText({ input$e3 })

  dtppbpos <- reactive({df <- datatable(pitPlayersbyPos(input$e3),options = list(pageLength = 20,autoWidth = FALSE, info = FALSE)) %>%
    formatRound(c('Age','ADP','W','SV','HLD','SO'),0) %>% formatCurrency('DFL') %>%
    formatRound(c('RPV','SGP','ERA'),3)
  })
  output$ppbpos <- DT::renderDataTable({ dtppbpos() })
  
  dtrrcResults <- datatable(rrcResults,options = list(pageLength = 20,autoWidth = FALSE, info = FALSE)) %>%
    formatRound(c('pADP','pW','pSV','pHLD','pSO'),0) %>% formatCurrency('pDFL') %>%
    formatRound(c('pSGP','pERA','pK/9','pBB/9'),3)
  output$rrcResults <- DT::renderDataTable({dtrrcResults})
  
  dtinjOrig <- datatable(injOrig,options = list(pageLength = 20,autoWidth = FALSE, info = FALSE)) %>%
    formatCurrency('pDFL')
  output$injOrig <- DT::renderDataTable({dtinjOrig})

  dttopHitters <- datatable(topHitters,options = list(pageLength = 20,autoWidth = FALSE, searching = FALSE, info = FALSE)) %>%
    formatRound(c('Age','ADP','HR','RBI','R','SB'),0) %>% formatCurrency('pDFL') %>%
    formatRound(c('AVG'),3)
  output$topHitters <- DT::renderDataTable({ dttopHitters })
  
  dtprospectH <- datatable(prospectH,options = list(pageLength = 20,autoWidth = FALSE, searching = FALSE, info = FALSE)) %>%
    formatRound(c('Age','ADP'),0) %>% formatCurrency('DFL')
  output$prospectH <- DT::renderDataTable({ dtprospectH })

  dtprospectP <- datatable(prospectP,options = list(pageLength = 20,autoWidth = FALSE, searching = FALSE, info = FALSE)) %>%
    formatRound(c('Age','ADP'),0) %>% formatCurrency('DFL')
  output$prospectP <- DT::renderDataTable({ dtprospectP })
  
  #output$prospectH <- renderDataTable({ prospectH })  
  #output$prospectP <- renderDataTable({ prospectP })
})
