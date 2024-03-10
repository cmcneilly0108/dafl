# Move a player from FA to a Team, update both lists
# GUI to pick a player


setwd("../code/")
source("./draftGuide.r")
setwd("../draftTool")

teams <- sort(unique(pstandings$Team))
hpos <- list('C','1B','2B','SS','3B','OF')
ppos <- list('SP','MR','CL')
allpos <- c(hpos,list('SP','RP'))

tProtect <- function(tm) {
  res <- filter(protClean,Team == tm ) %>% select(Player,Pos,Age,pDFL,Salary, Contract) %>% arrange(-pDFL)
}

posProtect <- function(pos) {
  res <- filter(protClean,Pos == pos ) %>% select(Player,Team,Age,pDFL,Salary, Contract) %>% arrange(Team)
}

uniqueProtect <- function(pos) {
  res <- filter(protClean,Pos == pos ) %>% select(Team) %>% unique() %>% nrow()
  r2 <- paste("Unique Teams = ",res)
}

teamsInterested <- function(pos) {
  teams <- sort(unique(pstandings$Team))
  allteams <- data.frame(Team=teams)
  have <- filter(protClean,Pos == pos ) %>% select(Team) %>% unique()
  need <- anti_join(allteams,have)
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
    formatPercentage(c('playersProt','dollarsSpent','valueTaken'),2) %>%
    formatRound(c('ToFill'),0)
  output$protectSummary <- DT::renderDataTable({ dtprotectSummary })
  dtppp <- datatable(ppp,options = list(pageLength = 20,autoWidth = FALSE, paging = FALSE, searching = FALSE, info = FALSE))
  output$ppp <- DT::renderDataTable({ dtppp })
  
  
  updateSelectizeInput(session, 'e1', choices = teams, selected = 'Liquor Crickets')
  output$tname <- renderText({ input$e1 })

  dttProtect <- reactive({df <- datatable(tProtect(input$e1),options = list(pageLength = 20,autoWidth = FALSE, paging = FALSE, searching = FALSE, info = FALSE)) %>%
                           formatCurrency('pDFL') %>%
                          formatRound('Age',0)})
  output$tProtect <- DT::renderDataTable({ dttProtect() })
  
  dtGoals <- reactive({df <- datatable(calcGoals(rpitchers,rhitters,targets,input$e1),options = list(pageLength = 20,autoWidth = FALSE, paging = FALSE, searching = FALSE, info = FALSE)) %>%
    formatPercentage('pc',2) %>%
    formatRound(c('collected','needed'),0)})
  output$Goals <- DT::renderDataTable({ dtGoals() })

  dttpSummary <- reactive({df <- datatable(tpSummary(input$e1),options = list(pageLength = 20,autoWidth = FALSE, paging = FALSE, searching = FALSE, info = FALSE)) %>%
    formatRound('salleft',0)})
  output$tpSummary <- DT::renderDataTable({ dttpSummary() })
  
  updateSelectizeInput(session, 'e2', choices = hpos, selected = 'OF')
  output$hpos <- renderText({ input$e2 })

  dthpbpos <- reactive({df <- datatable(hitPlayersbyPos(input$e2),options = list(pageLength = 20,autoWidth = FALSE, searching = FALSE, info = FALSE), filter='top') %>%
    formatRound(c('Age','ADP','HR','RBI','R','SB'),0) %>% formatCurrency('DFL') %>%
    formatRound(c('RPV','SGP','AVG'),3)
  })
  output$hpbpos <- DT::renderDataTable({ dthpbpos() })

  updateSelectizeInput(session, 'e3', choices = ppos, selected = 'SP')
  output$ppos <- renderText({ input$e3 })

  dtppbpos <- reactive({df <- datatable(pitPlayersbyPos(input$e3),options = list(pageLength = 20,autoWidth = FALSE, info = FALSE), filter='top') %>%
    formatRound(c('Age','ADP','W','SV','HLD','SO'),0) %>% formatCurrency('DFL') %>%
    formatRound(c('RPV','SGP','ERA'),3)
  })
  output$ppbpos <- DT::renderDataTable({ dtppbpos() })

  updateSelectizeInput(session, 'e4', choices = allpos, selected = 'OF')
  output$allpos <- renderText({ input$e4 })
  output$uniquePos <- renderText({ uniqueProtect(input$e4) })
  
  dtposProtect <- reactive({df <- datatable(posProtect(input$e4),options = list(pageLength = 20,autoWidth = FALSE, info = FALSE), filter='top') %>%
    formatCurrency('pDFL') %>%
    formatRound('Age',0)
  })
  output$posProtect <- DT::renderDataTable({ dtposProtect() })

  dttNeed <- reactive({df <- datatable(teamsInterested(input$e4),options = list(pageLength = 20,autoWidth = FALSE, paging = FALSE, searching = FALSE, info = FALSE))
  })
  output$tNeed <- DT::renderDataTable({ dttNeed() })
  
  dtrrcResults <- datatable(rrcResults,options = list(pageLength = 20,autoWidth = FALSE, info = FALSE), filter='top') %>%
    formatRound(c('pADP','pW','pSV','pHLD','pSO'),0) %>% formatCurrency('pDFL') %>%
    formatRound(c('pSGP','pERA','pK/9','pBB/9'),3)
  output$rrcResults <- DT::renderDataTable({dtrrcResults})
  
  dtinjOrig <- datatable(injOrig,options = list(pageLength = 20,autoWidth = FALSE, info = FALSE), filter='top') %>%
    formatCurrency('pDFL')
  output$injOrig <- DT::renderDataTable({dtinjOrig})

  dttopHitters <- datatable(topHitters,options = list(pageLength = 20,autoWidth = FALSE, searching = FALSE, info = FALSE)) %>%
    formatRound(c('Age','ADP','HR','RBI','R','SB'),0) %>% formatCurrency('pDFL') %>%
    formatRound(c('AVG'),3)
  output$topHitters <- DT::renderDataTable({ dttopHitters })
  
  dtprospectH <- datatable(prospectH,options = list(pageLength = 20,autoWidth = FALSE, searching = FALSE, info = FALSE), filter='top') %>%
    formatRound(c('Age','ADP'),0) %>% formatCurrency('DFL')
  output$prospectH <- DT::renderDataTable({ dtprospectH })

  dtprospectP <- datatable(prospectP,options = list(pageLength = 20,autoWidth = FALSE, searching = FALSE, info = FALSE)) %>%
    formatRound(c('Age','ADP'),0) %>% formatCurrency('DFL')
  output$prospectP <- DT::renderDataTable({ dtprospectP })
  
})
