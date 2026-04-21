# LeagueEval server.R — reactive in-season evaluation tool

setwd("../code/")
source("./inSeasonPulse.r")

teams <- sort(unique(RTot$Team))
targetFile <- "../InSeasonTargets.csv"


shinyServer(function(input, output,session) {

  # --- Reactive state ---
  rv <- reactiveValues(
    refreshCount = 0,
    targets = {
      if (file.exists(targetFile)) {
        as.character(read.csv(targetFile, stringsAsFactors = FALSE)$playerid)
      } else {
        character(0)
      }
    }
  )


  # --- Helper: inline target star for player rows ---
  targetStar <- function(pid, isTarget) {
    star <- ifelse(isTarget, "\u2605", "\u2606")
    color <- ifelse(isTarget, "#f1c40f", "#ccc")
    paste0("<span id='tgt-", pid, "' onclick='Shiny.setInputValue(\"toggleTarget\", \"", pid,
           "\", {priority: \"event\"}); return false;' ",
           "style='cursor:pointer; font-size:16px; color:", color, ";'>", star, "</span>")
  }

  # --- Helper: mark target players with star column ---
  markTargets <- function(df, targets) {
    df$isTarget <- as.integer(df$playerid %in% targets)
    df$Target <- targetStar(df$playerid, df$isTarget == 1)
    df
  }

  # --- Universal target toggle handler ---
  observeEvent(input$toggleTarget, {
    pid <- input$toggleTarget
    allPlayers <- bind_rows(
      AllH %>% select(playerid, Player),
      AllP %>% select(playerid, Player)
    ) %>% distinct(playerid, .keep_all = TRUE)
    pName <- allPlayers$Player[allPlayers$playerid == pid]
    if (length(pName) == 0) pName <- "Player"
    if (pid %in% rv$targets) {
      rv$targets <- rv$targets[rv$targets != pid]
      isNowTarget <- FALSE
      showNotification(paste0("Removed target: ", pName), type = "message")
    } else {
      rv$targets <- c(rv$targets, pid)
      isNowTarget <- TRUE
      showNotification(paste0("Added target: ", pName), type = "message")
    }
    write.csv(data.frame(playerid = rv$targets, stringsAsFactors = FALSE), targetFile, row.names = FALSE)
    session$sendCustomMessage("toggleStar", list(pid = pid, isTarget = isNowTarget))
  })


# --- Refresh Data ---
  observeEvent(input$refreshBtn, {
    showNotification("Refreshing data... this may take a minute", type = "message", duration = NULL, id = "refreshMsg")
    tryCatch({
      source("../code/inSeasonPulse.r", local = globalenv())
      teams <<- sort(unique(RTot$Team))
      updateSelectizeInput(session, 'e1', choices = teams, selected = 'Liquor Crickets')
      updateSelectizeInput(session, 'choice', choices = trending$Player, server = TRUE)
      rv$refreshCount <- rv$refreshCount + 1
      removeNotification("refreshMsg")
      showNotification("Data refreshed!", type = "message")
    }, error = function(e) {
      removeNotification("refreshMsg")
      showNotification(paste0("Refresh failed: ", e$message), type = "error", duration = 10)
    })
  })


# Standings
  output$StandFull <- DT::renderDataTable({
    rv$refreshCount
    datatable(dstandfull,options = list(pageLength = 20))
  })

  output$RTot <- DT::renderDataTable({
    rv$refreshCount
    datatable(RTot,options = list(pageLength = 20), escape=FALSE) %>%
      formatCurrency(c('hDFL', 'piDFL','tDFL')) %>% formatRound('zScore',2)
  })

  output$RTotTop <- DT::renderDataTable({
    rv$refreshCount
    datatable(RTotTop,options = list(pageLength = 20), escape=FALSE) %>%
      formatCurrency(c('hDFL', 'piDFL','tDFL')) %>% formatRound('zScore',2)
  })

# By Team
  updateSelectizeInput(session, 'e1', choices = teams, selected = 'Liquor Crickets')
  output$tname <- renderText({ input$e1 })

  dtTeamH <- reactive({
    rv$refreshCount
    datatable(pullTeam(input$e1)[[1]],options = list(pageLength = 20), escape=FALSE) %>%
      formatCurrency('pDFL') %>% formatRound(c('pSGP','hotscore','pAVG'),3) %>%
      formatRound(c('pHR','pRBI','pSB','pR','Age'),0)
  })
  output$TeamH <- DT::renderDataTable({ dtTeamH() })

  dtTeamP <- reactive({
    rv$refreshCount
    datatable(pullTeam(input$e1)[[2]],options = list(pageLength = 20), escape=FALSE) %>%
      formatCurrency('pDFL') %>% formatRound(c('pSGP','hotscore','pERA','pFIP','pK/9'),3) %>%
      formatRound(c('pW','pSO','pSV','pHLD','Age'),0)
  })
  output$TeamP <- DT::renderDataTable({ dtTeamP() })

# By Position
  topPosData <- reactive({
    rv$refreshCount
    ifelse(input$fa == TRUE,ffh <- filter(AllH,Team=='Free Agent'),ffh <- AllH)
    ifelse(input$fa == TRUE,ffp <- filter(AllP,Team=='Free Agent'),ffp <- AllP)
    ifelse(input$e3 %in% c('SP','MR','CL'),
           ff <- ffp %>% filter(Pos == input$e3) %>% arrange(-pDFL) %>%
             select(playerid,Player,Pos,Age,pDFL,Team,Salary,Contract,pSGP,Rank,'Pitching+',pW,pSO,pSV,pHLD,pERA,`pK/9`,pFIP,W,K,S,HD,ERA,hotscore,twostarts,LVG,Injury,Expected.Return),
           ifelse(input$e3 == 'Hitters',
                  ff <- ffh %>%
                    select(playerid,Player,Pos,Age,pDFL,Team,Salary,Contract,pSGP,Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,AVG,hotscore,Injury,Expected.Return) %>%
                    arrange(-pDFL),
                  ff <- ffh %>% filter(str_detect(posEl,input$e3)) %>%
                    select(playerid,Player,Pos,Age,pDFL,Team,Salary,Contract,pSGP,Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,AVG,hotscore,Injury,Expected.Return) %>%
                    arrange(-pDFL)
           )
    )
    markTargets(ff, isolate(rv$targets))
  })

  topPos <- reactive({
    ff <- topPosData() %>% select(Target, everything(), -playerid, -isTarget)
    ifelse(input$e3 %in% c('SP','MR','CL'),
           res <- datatable(ff,options = list(pageLength = 20), filter='top', escape=FALSE) %>% formatCurrency('pDFL') %>%
             formatRound(c('pSGP','hotscore','pERA','pK/9','pFIP','LVG'),2) %>% formatRound(c('Age','pW','pSO','pSV','pHLD','Pitching+'),0),
           res <- datatable(ff,options = list(pageLength = 20), filter='top', escape=FALSE) %>% formatCurrency('pDFL') %>%
             formatRound(c('pSGP','hotscore'),2) %>% formatRound(c('Age','pHR','pR','pRBI','pSB'),0) %>%
             formatRound(c('pAVG'),3)
    )
    res
  })
  output$topPlayers <- DT::renderDataTable({topPos()})

# Reliever Detail
  output$rrcResults <- DT::renderDataTable({
    rv$refreshCount
    ff <- markTargets(rrcResults, isolate(rv$targets)) %>% select(Target, everything(), -playerid, -isTarget)
    datatable(ff,options = list(pageLength = 20), filter='top', escape=FALSE) %>%
      formatRound(c('pSGP','hotscore','LVG','pERA','pK/9','pBB/9'),3) %>% formatCurrency('pDFL') %>%
      formatRound(c('pW','pSO','pSV','pHLD'),0)
  })

# LC Trends
  output$g1 <- renderPlot({ rv$refreshCount; g1 },height=1000,width=2000,res=150)
  output$g2 <- renderPlot({ rv$refreshCount; g2 },height=1000,width=2000,res=150)
  output$g3 <- renderPlot({ rv$refreshCount; g3 },height=1000,width=2000,res=150)

# Player Trends
  updateSelectizeInput(session, 'choice', choices = trending$Player, server=TRUE)
  output$lcgraph <- renderPlotly({
    rv$refreshCount
    plot_ly(trending, x = ~Date, y = ~hotscore)  %>%
      filter(Player %in% input$choice) %>%
      group_by(Player) %>%
      add_lines(color=~Player,line = list(width=5)) %>%
      add_trace(color=~Player,type="scatter",mode = "markers",marker=list(size=15))
  })

# Category Status
  output$catSummary <- DT::renderDataTable({
    rv$refreshCount
    datatable(catSummary,options = list(pageLength = 20)) %>%
      formatRound(c('pvp','pvm','opportunity'),2)
  })

# Positional Surplus
  tprof <- reactive({
    rv$refreshCount
    ifelse(input$e2 %in% c('SP','MR','CL'),df<-AllP,df<-AllH)
    f <- df %>% filter(Pos == input$e2,pDFL > input$pd) %>% group_by(Team) %>% summarize(nGood = length(Team))
    f2 <- df %>% filter(Pos == input$e2) %>% group_by(Team) %>% summarize(nTotal = length(Team))
    ff <- left_join(f2,f,by=c('Team')) %>% arrange(-nGood,-nTotal)
  })
  output$tprofile <- DT::renderDataTable({tprof()},
                                         options = list(pageLength = 20))
# Prospects
  output$ProPit <- DT::renderDataTable({
    rv$refreshCount
    datatable(prospectP,options = list(pageLength = 20), escape=FALSE) %>% formatRound(c('Age'),1)
  })
  output$ProHit <- DT::renderDataTable({
    rv$refreshCount
    datatable(prospectH,options = list(pageLength = 20), escape=FALSE) %>% formatRound(c('Age'),1)
  })

# Dumpers
  output$cTrades <- DT::renderDataTable({
    rv$refreshCount
    datatable(candTrades,options = list(pageLength = 20)) %>%
      formatCurrency(c('pDFL'))
  })

# Desperate
  output$problems <- DT::renderDataTable({
    rv$refreshCount
    datatable(problems,options = list(pageLength = 20)) %>%
      formatRound('hotscore',2) %>% formatRound('Age',0) %>% formatCurrency('pDFL')
  })

# Injured
  output$injOrig <- DT::renderDataTable({
    rv$refreshCount
    ff <- markTargets(injOrig, isolate(rv$targets)) %>% select(Target, everything(), -playerid, -isTarget)
    datatable(ff,options = list(pageLength = 20,autoWidth = FALSE, info = FALSE), filter='top', escape=FALSE) %>%
      formatCurrency('pDFL')
  })

# My Targets
  output$targetTable <- DT::renderDataTable({
    rv$targets  # react to target changes
    rv$refreshCount
    allPlayers <- bind_rows(
      AllH %>% select(playerid, Player, Pos, Age, pDFL, hotscore, Injury, Expected.Return),
      AllP %>% select(playerid, Player, Pos, Age, pDFL, hotscore, Injury, Expected.Return)
    ) %>% distinct(playerid, .keep_all = TRUE)
    info <- allPlayers %>% filter(playerid %in% rv$targets) %>% arrange(-pDFL)
    ff <- markTargets(info, rv$targets) %>% select(Target, Player, Pos, Age, pDFL, hotscore, Injury, Expected.Return, -playerid, -isTarget)
    datatable(ff,
              options = list(paging = FALSE, info = FALSE, autoWidth = FALSE),
              escape = FALSE) %>%
      formatCurrency('pDFL') %>%
      formatRound(c('hotscore'), 2) %>%
      formatRound('Age', 0)
  })

})
