# LeagueEval server.R — reactive in-season evaluation tool

setwd("../code/")
source("./inSeasonPulse.r")
library(shinyjs)

teams <- sort(unique(RTot$Team))
targetFile <- "../InSeasonTargets.csv"

# --- LeagueEval settings (projection source) ---
# Persisted across sessions so the chosen projection survives restarts.
# inSeasonPulse.r reads this file to set the default `activeProj` for the inline
# pipeline; we read it here too so the Shiny UI starts in the same state.
leSettingsFile <- "../leagueEvalSettings.json"
leSettings <- tryCatch({
  if (file.exists(leSettingsFile)) jsonlite::fromJSON(leSettingsFile) else list()
}, error = function(e) list())
initialProjSource <- if (!is.null(leSettings$projSource) &&
                         leSettings$projSource %in% c('atc','steamer','batx')) {
  leSettings$projSource
} else 'atc'


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
    },
    researchH = data.frame(),
    researchP = data.frame(),
    researchUnmatched = character(0),
    researchTitle = ""
  )

  projSource <- reactiveVal(initialProjSource)

  # --- Settings modal: projection source + Refresh Data ---
  observeEvent(input$settingsBtn, {
    showModal(modalDialog(
      title = "Settings",
      size = "s",
      easyClose = TRUE,
      radioButtons('projSource', 'Projection System',
                   choices = c('ATC' = 'atc',
                               'Steamer' = 'steamer',
                               'THE BAT X' = 'batx'),
                   selected = isolate(projSource()),
                   inline = TRUE),
      tags$hr(),
      actionButton('refreshBtn', 'Refresh Data',
                   class = 'btn-success btn-sm',
                   icon = icon('refresh')),
      footer = modalButton("Close")
    ))
  })

  # On projection change: swap globals from prebuilt pool, persist, refresh outputs
  observeEvent(input$projSource, {
    src <- input$projSource
    if (is.null(src) || identical(src, projSource())) return()
    if (!exists("leaguePools") || is.null(leaguePools[[src]])) {
      showNotification(paste0("Projection pool '", src, "' not available."),
                       type = "error", duration = 5)
      return()
    }
    pool <- leaguePools[[src]]
    AllH       <<- pool$AllH
    AllP       <<- pool$AllP
    RTot       <<- pool$RTot
    RTotTop    <<- pool$RTotTop
    rrcResults <<- pool$rrcResults
    candTrades <<- pool$candTrades
    problems   <<- pool$problems
    injOrig    <<- pool$injOrig
    projSource(src)
    rv$refreshCount <- rv$refreshCount + 1
    tryCatch(
      write(jsonlite::toJSON(list(projSource = src), auto_unbox = TRUE), leSettingsFile),
      error = function(e) NULL
    )
    showNotification(paste0("Projection: ", switch(src, atc = "ATC", steamer = "Steamer", batx = "THE BAT X")),
                     type = "message", duration = 3)
  }, ignoreInit = TRUE)


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
    pName <- gsub("<[^>]+>", "", pName)
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
    removeModal()
    showNotification("Refreshing data... this may take a minute", type = "message", duration = NULL, id = "refreshMsg")
    Sys.setenv(DAFL_FORCE_REFRESH = "1")
    on.exit(Sys.unsetenv("DAFL_FORCE_REFRESH"), add = TRUE)
    tryCatch({
      source("../code/inSeasonPulse.r", local = globalenv())
      teams <<- sort(unique(RTot$Team))
      updateSelectizeInput(session, 'e1', choices = teams, selected = 'Liquor Crickets')
      updateSelectizeInput(session, 'teamSelect', choices = teams, selected = 'Liquor Crickets')
      updateSelectizeInput(session, 'tradeTeamA',
                           choices = c('Pick a team' = '', teams),
                           selected = '')
      updateSelectizeInput(session, 'tradeTeamB',
                           choices = c('Pick a team' = '', teams),
                           selected = '')
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
  updateSelectizeInput(session, 'teamSelect', choices = teams, selected = 'Liquor Crickets')
  updateSelectizeInput(session, 'tradeTeamA',
                       choices = c('Pick a team' = '', teams),
                       selected = '')
  updateSelectizeInput(session, 'tradeTeamB',
                       choices = c('Pick a team' = '', teams),
                       selected = '')
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
      formatRound(c('pW','pSO','pSV','pHLD','Age','Pitching+'),0)
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

# Trade Eval — team-name labels
  output$tradeTeamAName <- renderText({ input$tradeTeamA })
  output$tradeTeamBName <- renderText({ input$tradeTeamB })

  # Trade Eval — roster builder. Carries hidden projection columns (pHR/pAVG/pAB
  # for hitters; pW/pERA/pIP/etc. for pitchers) so the summary reactive can
  # compute deltas without re-querying AllH / AllP.
  buildRoster <- function(teamName) {
    if (is.null(teamName) || teamName == '') return(NULL)
    hCols <- c('Player','Pos','pDFL','Salary','Contract','hotscore','Injury',
               'pHR','pRBI','pR','pSB','pAVG','pAB')
    pCols <- c('Player','Pos','pDFL','Salary','Contract','hotscore','Injury',
               'pW','pSO','pSV','pHLD','pERA','pIP')
    hOnly <- AllH %>% filter(Team == teamName)
    hOnly <- hOnly[, intersect(hCols, colnames(hOnly)), drop = FALSE]
    pOnly <- AllP %>% filter(Team == teamName)
    pOnly <- pOnly[, intersect(pCols, colnames(pOnly)), drop = FALSE]
    bind_rows(hOnly, pOnly) %>% arrange(-pDFL)
  }

  rosterA <- reactive({ rv$refreshCount; buildRoster(input$tradeTeamA) })
  rosterB <- reactive({ rv$refreshCount; buildRoster(input$tradeTeamB) })

  rosterTable <- function(roster) {
    req(roster)
    visibleCols <- intersect(
      c('Player','Pos','pDFL','Salary','Contract','hotscore','Injury'),
      colnames(roster)
    )
    display <- roster[, visibleCols, drop = FALSE]
    datatable(display,
              selection = list(mode = 'multiple'),
              options = list(paging = FALSE,
                             searching = FALSE, info = FALSE),
              rownames = FALSE, escape = FALSE) %>%
      formatCurrency(intersect(c('pDFL','Salary'), visibleCols)) %>%
      formatRound(intersect('hotscore', visibleCols), 2)
  }

  output$tradeRosterA <- DT::renderDataTable({ rosterTable(rosterA()) })
  output$tradeRosterB <- DT::renderDataTable({ rosterTable(rosterB()) })

  # Trade Eval — summary helpers
  catCounting <- list(
    list(name='HR',  col='pHR'),  list(name='RBI', col='pRBI'),
    list(name='R',   col='pR'),   list(name='SB',  col='pSB'),
    list(name='W',   col='pW'),   list(name='K',   col='pSO'),
    list(name='SV',  col='pSV'),  list(name='HD',  col='pHLD')
  )

  sum0 <- function(x) sum(x, na.rm = TRUE)

  weightedRate <- function(rows, rateCol, volCol) {
    if (!rateCol %in% colnames(rows) || !volCol %in% colnames(rows)) return(NA_real_)
    v <- rows[[volCol]]; r <- rows[[rateCol]]
    ok <- !is.na(v) & !is.na(r) & v > 0
    if (!any(ok)) return(NA_real_)
    sum(r[ok] * v[ok]) / sum(v[ok])
  }

  rateDelta <- function(fullRoster, outgoing, incoming, rateCol, volCol) {
    before <- weightedRate(fullRoster, rateCol, volCol)
    kept   <- fullRoster %>% filter(!Player %in% outgoing$Player)
    after  <- weightedRate(bind_rows(kept, incoming), rateCol, volCol)
    if (is.na(before) || is.na(after)) return(NA_real_)
    after - before
  }

  fmtCount <- function(x) {
    if (is.na(x) || x == 0) '0'
    else if (x > 0) sprintf('+%d', round(x))
    else sprintf('%d', round(x))
  }
  fmtBA <- function(x) {
    if (is.na(x)) '—'
    else if (x == 0) '0.000'
    else if (x > 0) sprintf('+%.3f', x)
    else sprintf('%.3f', x)
  }
  fmtERA <- function(x) {
    if (is.na(x)) '—'
    else if (x == 0) '0.00'
    else if (x > 0) sprintf('+%.2f', x)
    else sprintf('%.2f', x)
  }
  fmtDFL <- function(x) {
    if (is.na(x) || x == 0) '$0'
    else if (x > 0) sprintf('+$%d', round(x))
    else sprintf('-$%d', round(abs(x)))
  }

  output$tradeSummary <- DT::renderDataTable({
    rv$refreshCount
    req(input$tradeTeamA, input$tradeTeamB)

    if (input$tradeTeamA == input$tradeTeamB) {
      return(datatable(
        data.frame(Note = "Pick two different teams"),
        options = list(paging = FALSE, info = FALSE, searching = FALSE,
                       ordering = FALSE, dom = 't'),
        rownames = FALSE))
    }

    rA <- rosterA(); rB <- rosterB()
    selA <- if (length(input$tradeRosterA_rows_selected) > 0)
              rA[input$tradeRosterA_rows_selected, , drop = FALSE]
            else rA[0, , drop = FALSE]
    selB <- if (length(input$tradeRosterB_rows_selected) > 0)
              rB[input$tradeRosterB_rows_selected, , drop = FALSE]
            else rB[0, , drop = FALSE]

    # Two-row summary: each team and the categories where they come out ahead.
    aheadA <- character(0)
    aheadB <- character(0)
    addStat <- function(name, deltaA, deltaB, fmt, lowerBetter = FALSE, bold = FALSE) {
      fav <- function(v) !is.na(v) && v != 0 && (if (lowerBetter) v < 0 else v > 0)
      label <- function(v) {
        s <- sprintf("%s (%s)", name, fmt(v))
        if (bold) paste0("<strong>", s, "</strong>") else s
      }
      if (fav(deltaA)) aheadA <<- c(aheadA, label(deltaA))
      if (fav(deltaB)) aheadB <<- c(aheadB, label(deltaB))
    }

    # Order: HR, RBI, R, SB, BA, W, K, SV, HD, ERA, pDFL.
    for (c in catCounting[1:4]) {  # HR, RBI, R, SB
      out <- if (c$col %in% colnames(selA)) sum0(selA[[c$col]]) else 0
      inc <- if (c$col %in% colnames(selB)) sum0(selB[[c$col]]) else 0
      deltaA <- inc - out
      addStat(c$name, deltaA, -deltaA, fmtCount)
    }
    addStat('BA', rateDelta(rA, selA, selB, 'pAVG', 'pAB'),
                  rateDelta(rB, selB, selA, 'pAVG', 'pAB'), fmtBA)
    for (c in catCounting[5:8]) {  # W, K, SV, HD
      out <- if (c$col %in% colnames(selA)) sum0(selA[[c$col]]) else 0
      inc <- if (c$col %in% colnames(selB)) sum0(selB[[c$col]]) else 0
      deltaA <- inc - out
      addStat(c$name, deltaA, -deltaA, fmtCount)
    }
    addStat('ERA', rateDelta(rA, selA, selB, 'pERA', 'pIP'),
                   rateDelta(rB, selB, selA, 'pERA', 'pIP'),
                   fmtERA, lowerBetter = TRUE)
    outDFL <- if ('pDFL' %in% colnames(selA)) sum0(selA$pDFL) else 0
    incDFL <- if ('pDFL' %in% colnames(selB)) sum0(selB$pDFL) else 0
    dflDelta <- incDFL - outDFL
    addStat('pDFL', dflDelta, -dflDelta, fmtDFL, bold = TRUE)

    df <- data.frame(
      Team = c(input$tradeTeamA, input$tradeTeamB),
      `Comes out ahead on` = c(
        if (length(aheadA) > 0) paste(aheadA, collapse = ", ") else "—",
        if (length(aheadB) > 0) paste(aheadB, collapse = ", ") else "—"
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    datatable(df,
              options = list(paging = FALSE, info = FALSE, searching = FALSE,
                             ordering = FALSE, dom = 't'),
              rownames = FALSE, escape = FALSE)
  })

# Category Status — per-team detail (new)
  output$teamCatDetail <- DT::renderDataTable({
    rv$refreshCount
    team <- input$teamSelect
    if (is.null(team) || team == '') return(NULL)
    # cstand uses short nicknames (via the nicks join in inSeasonPulse.r);
    # AllH / AllP use full team names. Map for the tier lookup only.
    shortTeam <- if (exists("nicks") && team %in% nicks$Team) {
      nicks$Short[match(team, nicks$Team)]
    } else team

    cats <- list(
      list(name = "HR",  col = "HR",  reverse = FALSE, kind = "H",   sortKey = "pHR"),
      list(name = "RBI", col = "RBI", reverse = FALSE, kind = "H",   sortKey = "pRBI"),
      list(name = "R",   col = "R",   reverse = FALSE, kind = "H",   sortKey = "pR"),
      list(name = "SB",  col = "SB",  reverse = FALSE, kind = "H",   sortKey = "pSB"),
      list(name = "BA",  col = "BA",  reverse = FALSE, kind = "BA"),
      list(name = "W",   col = "W",   reverse = FALSE, kind = "P",   sortKey = "pW"),
      list(name = "K",   col = "K",   reverse = FALSE, kind = "P",   sortKey = "pSO"),
      list(name = "SV",  col = "S",   reverse = FALSE, kind = "P",   sortKey = "pSV"),
      list(name = "HD",  col = "HD",  reverse = FALSE, kind = "P",   sortKey = "pHLD"),
      list(name = "ERA", col = "ERA", reverse = TRUE,  kind = "ERA")
    )

    fmtVal <- function(catName, x) {
      if (is.na(x))                  ""
      else if (catName == "BA")      sprintf("%.3f", x)
      else if (catName == "ERA")     sprintf("%.2f", x)
      else                           as.character(round(x))
    }

    # Tier for the selected team in one category. cstand is a global from
    # inSeasonPulse.r with one row per team and columns matching `col`.
    computeTier <- function(catCol, reverse) {
      v <- suppressWarnings(as.numeric(cstand[[catCol]]))
      ord <- if (reverse) order(v, na.last = TRUE) else order(-v, na.last = TRUE)
      teamsRanked <- cstand$Team[ord]
      r <- which(teamsRanked == shortTeam)
      if (length(r) == 0) return(list(tier = NA_character_, value = NA_real_))
      list(tier  = if (r <= 4) 'High' else if (r <= 9) 'Medium' else 'Low',
           value = v[ord][r])
    }

    # Position display for a hitter row — prefer Position (eligibility-derived)
    # if present, fall back to Pos.
    hitterPos <- function(df) {
      if ("Position" %in% colnames(df)) df$Position else df$Pos
    }

    # Top 4 hitters for a counting category (HR / RBI / R / SB).
    topHitters <- function(sortKey) {
      df <- AllH %>% filter(Team == team)
      if (!sortKey %in% colnames(df) || nrow(df) == 0) return(character(0))
      df <- df %>% arrange(desc(.data[[sortKey]]), Player) %>% head(4)
      mapply(function(p, ps, v) sprintf("%s (%s, %d)", p, ps, round(v)),
             df$Player, hitterPos(df), df[[sortKey]],
             USE.NAMES = FALSE)
    }

    # Top 4 hitters for BA — volume-weighted (pAVG - 0.250) * pAB. Falls back
    # to pAVG desc when pAB is missing from the projection.
    topBA <- function() {
      df <- AllH %>% filter(Team == team)
      if (nrow(df) == 0 || !"pAVG" %in% colnames(df)) return(character(0))
      if ("pAB" %in% colnames(df)) {
        df <- df %>% filter(pAB > 0) %>%
          mutate(.k = (pAVG - 0.250) * pAB) %>%
          arrange(desc(.k), Player) %>% head(4)
      } else {
        df <- df %>% arrange(desc(pAVG), Player) %>% head(4)
      }
      mapply(function(p, ps, avg) sprintf("%s (%s, %.3f)", p, ps, avg),
             df$Player, hitterPos(df), df$pAVG, USE.NAMES = FALSE)
    }

    # Top 4 pitchers for a counting category (W / K / SV / HD).
    topPitchers <- function(sortKey) {
      df <- AllP %>% filter(Team == team)
      if (!sortKey %in% colnames(df) || nrow(df) == 0) return(character(0))
      df <- df %>% arrange(desc(.data[[sortKey]]), Player) %>% head(4)
      mapply(function(p, ps, v) sprintf("%s (%s, %d)", p, ps, round(v)),
             df$Player, df$Pos, df[[sortKey]],
             USE.NAMES = FALSE)
    }

    # Top 4 pitchers for ERA — volume-weighted (4.00 - pERA) * pIP / 9. Falls
    # back to pERA asc when pIP is missing.
    topERA <- function() {
      df <- AllP %>% filter(Team == team)
      if (nrow(df) == 0 || !"pERA" %in% colnames(df)) return(character(0))
      if ("pIP" %in% colnames(df)) {
        df <- df %>% filter(pIP > 0) %>%
          mutate(.k = (4.00 - pERA) * pIP / 9) %>%
          arrange(desc(.k), Player) %>% head(4)
      } else {
        df <- df %>% arrange(pERA, Player) %>% head(4)
      }
      mapply(function(p, ps, era) sprintf("%s (%s, %.2f)", p, ps, era),
             df$Player, df$Pos, df$pERA, USE.NAMES = FALSE)
    }

    rows <- lapply(cats, function(c) {
      t <- computeTier(c$col, c$reverse)
      tierLabel <- if (is.na(t$tier)) ""
                   else sprintf("%s (%s)", t$tier, fmtVal(c$name, t$value))
      contribs <- switch(c$kind,
                         "H"   = topHitters(c$sortKey),
                         "P"   = topPitchers(c$sortKey),
                         "BA"  = topBA(),
                         "ERA" = topERA(),
                         character(0))
      data.frame(
        Category   = c$name,
        Tier       = tierLabel,
        TierBg     = if (is.na(t$tier)) "" else t$tier,
        TierRank   = if (is.na(t$tier)) 4L
                     else switch(t$tier, High = 1L, Medium = 2L, Low = 3L),
        Contributors = paste(contribs, collapse = ", "),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })
    df <- do.call(rbind, rows)
    # Rename to display label only at the end (commas/keys clean above).
    names(df)[names(df) == "Contributors"] <- "Top 4 Contributors"

    datatable(df,
              options = list(paging = FALSE, info = FALSE, searching = FALSE,
                             ordering = TRUE, autoWidth = FALSE,
                             order = list(),  # preserve default category order on initial render
                             columnDefs = list(
                               list(targets = which(names(df) == "TierBg")   - 1, visible = FALSE),
                               list(targets = which(names(df) == "TierRank") - 1, visible = FALSE),
                               list(targets = which(names(df) == "Tier") - 1,
                                    orderData = which(names(df) == "TierRank") - 1),
                               list(targets = which(names(df) == "Top 4 Contributors") - 1,
                                    orderable = FALSE),
                               list(targets = 0, width = "60px"),
                               list(targets = 1, width = "180px")
                             )),
              rownames = FALSE, escape = FALSE) %>%
      formatStyle('Tier',
                  valueColumns = 'TierBg',
                  backgroundColor = styleEqual(
                    c('High', 'Medium', 'Low'),
                    c('#d4edda', '#fff3cd', '#f8d7da')
                  ))
  })

# Category Status — Points by Category for the selected team.
# Mirrors the pvResults / myscores / catSummary computation in inSeasonPulse.r,
# but parameterised on input$teamSelect (the global catSummary is hardcoded to
# 'Cricket' and is left intact for the weekly xlsx pipeline).
  output$catSummary <- DT::renderDataTable({
    rv$refreshCount
    team <- input$teamSelect
    if (is.null(team) || team == '') return(NULL)
    shortTeam <- if (exists("nicks") && team %in% nicks$Team) {
      nicks$Short[match(team, nicks$Team)]
    } else team
    if (!shortTeam %in% cstand$Team) return(NULL)

    # Counting categories only — pvCat doesn't apply to BA/ERA.
    pvCats <- c('HR','RBI','SB','R','W','HD','S','K')
    pvRows <- lapply(pvCats, function(cat) {
      myVal <- as.numeric(cstand[cstand$Team == shortTeam, cat])
      r <- pvCat(cstand[[cat]], 0.3, myVal)
      data.frame(category = cat, pvp = r[[1]], pvm = r[[2]], opportunity = r[[3]],
                 stringsAsFactors = FALSE)
    })
    pvDf <- do.call(rbind, pvRows) %>% arrange(-opportunity)

    # Per-category rank for the selected team. ERA ranked low-to-high, the rest
    # high-to-low (lower score = the team is worse in that category).
    rankCats <- list(
      list(cat='W',  rev=FALSE), list(cat='K',  rev=FALSE),
      list(cat='S',  rev=FALSE), list(cat='HD', rev=FALSE),
      list(cat='ERA',rev=TRUE),  list(cat='HR', rev=FALSE),
      list(cat='RBI',rev=FALSE), list(cat='R',  rev=FALSE),
      list(cat='SB', rev=FALSE), list(cat='BA', rev=FALSE)
    )
    msRows <- lapply(rankCats, function(rc) {
      v <- suppressWarnings(as.numeric(cstand[[rc$cat]]))
      ord <- if (rc$rev) order(-v, na.last = TRUE) else order(v, na.last = TRUE)
      data.frame(category = rc$cat,
                 score = which(cstand$Team[ord] == shortTeam),
                 stringsAsFactors = FALSE)
    })
    msDf <- do.call(rbind, msRows) %>% arrange(score)

    teamCatSummary <- left_join(msDf, pvDf, by = 'category',
                                relationship = "many-to-many") %>%
      arrange(-opportunity)

    datatable(teamCatSummary, options = list(pageLength = 20)) %>%
      formatRound(c('pvp','pvm','opportunity'), 2)
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

# Statistical Surplus — team tiers by category
  output$statSurplus <- DT::renderDataTable({
    rv$refreshCount
    cats <- list(
      list(name = "HR",  col = "HR",  reverse = FALSE),
      list(name = "RBI", col = "RBI", reverse = FALSE),
      list(name = "R",   col = "R",   reverse = FALSE),
      list(name = "SB",  col = "SB",  reverse = FALSE),
      list(name = "BA",  col = "BA",  reverse = FALSE),
      list(name = "W",   col = "W",   reverse = FALSE),
      list(name = "K",   col = "K",   reverse = FALSE),
      list(name = "SV",  col = "S",   reverse = FALSE),
      list(name = "HD",  col = "HD",  reverse = FALSE),
      list(name = "ERA", col = "ERA", reverse = TRUE)
    )
    nT <- nrow(cstand)
    rows <- lapply(cats, function(c) {
      v <- suppressWarnings(as.numeric(cstand[[c$col]]))
      ord <- if (c$reverse) order(v, na.last = TRUE) else order(-v, na.last = TRUE)
      teamsRanked <- cstand$Team[ord]
      valsRanked  <- v[ord]
      fmt <- function(t, x) {
        if (c$col == "BA")       paste0(t, " (", sprintf("%.3f", x), ")")
        else if (c$col == "ERA") paste0(t, " (", sprintf("%.2f", x), ")")
        else                      paste0(t, " (", round(x), ")")
      }
      labels <- mapply(fmt, teamsRanked, valsRanked, USE.NAMES = FALSE)
      high <- paste(labels[1:min(4, nT)], collapse = ", ")
      med  <- if (nT >= 5) paste(labels[5:min(9, nT)], collapse = ", ") else ""
      low  <- if (nT >= 10) paste(labels[10:nT], collapse = ", ") else ""
      data.frame(Category = c$name, High = high, Medium = med, Low = low,
                 stringsAsFactors = FALSE)
    })
    df <- do.call(rbind, rows)
    datatable(df,
              options = list(paging = FALSE, info = FALSE, searching = FALSE,
                             ordering = FALSE, autoWidth = FALSE,
                             columnDefs = list(
                               list(targets = 0, width = "60px"),
                               list(targets = 1:3, width = "32%")
                             )),
              rownames = FALSE, escape = FALSE) %>%
      formatStyle('High',   backgroundColor = '#d4edda') %>%
      formatStyle('Medium', backgroundColor = '#fff3cd') %>%
      formatStyle('Low',    backgroundColor = '#f8d7da')
  })
# Prospects
  output$ProPit <- DT::renderDataTable({
    rv$refreshCount
    df <- if (isTRUE(input$faProspects)) filter(prospectP, Team == 'Free Agent') else prospectP
    datatable(df,options = list(pageLength = 20), escape=FALSE) %>% formatRound(c('Age'),1)
  })
  output$ProHit <- DT::renderDataTable({
    rv$refreshCount
    df <- if (isTRUE(input$faProspects)) filter(prospectH, Team == 'Free Agent') else prospectH
    datatable(df,options = list(pageLength = 20), escape=FALSE) %>% formatRound(c('Age'),1)
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

# Streamers
  output$streamersTable <- DT::renderDataTable({
    rv$refreshCount
    rv$targets  # react to target changes
    stat <- input$streamersStat
    req(stat)
    hitterStats <- c('HR','R','RBI','SB','AVG')

    if (stat %in% hitterStats) {
      df <- AllH %>% select(playerid, Player, Pos, Team,
                            AB, HR, R, RBI, SB, AVG,
                            pHR, pR, pRBI, pSB, pAVG,
                            pDFL, hotscore)
    } else {
      df <- AllP %>% select(playerid, Player, Pos, Team,
                            INN, W, K, S, HD, ERA,
                            pW, pSO, pSV, pHLD, pERA,
                            pDFL, hotscore)
    }
    if (isTRUE(input$faStreamers)) df <- filter(df, Team == 'Free Agent')
    if (stat == 'ERA') {
      df <- df %>% arrange(.data[[stat]])
    } else {
      df <- df %>% arrange(desc(.data[[stat]]))
    }
    df <- head(df, 20)
    ff <- markTargets(df, isolate(rv$targets)) %>%
      select(Target, everything(), -playerid, -isTarget)
    dt <- datatable(ff,
                    options = list(pageLength = 20, autoWidth = FALSE, info = FALSE),
                    escape = FALSE) %>%
      formatCurrency('pDFL')
    dt <- dt %>% formatRound('hotscore', 2)
    if (stat %in% hitterStats) {
      dt %>% formatRound(c('AVG','pAVG'), 3) %>%
        formatRound(c('AB','pHR','pR','pRBI','pSB'), 0)
    } else {
      dt %>% formatRound(c('ERA','pERA'), 2) %>%
        formatRound('INN', 1) %>%
        formatRound(c('W','K','S','HD','pW','pSO','pSV','pHLD'), 0)
    }
  })

# My Targets
  output$targetTable <- DT::renderDataTable({
    rv$targets  # react to target changes
    rv$refreshCount
    allPlayers <- bind_rows(
      AllH %>% select(playerid, Player, Pos, Age, pDFL, hotscore, Injury, Expected.Return, Team),
      AllP %>% select(playerid, Player, Pos, Age, pDFL, hotscore, Injury, Expected.Return, Team)
    ) %>% distinct(playerid, .keep_all = TRUE)
    info <- allPlayers %>% filter(playerid %in% rv$targets) %>% arrange(-pDFL)
    if (isTRUE(input$faTargets)) info <- filter(info, Team == 'Free Agent')
    ff <- markTargets(info, rv$targets) %>% select(Target, Player, Pos, Age, pDFL, hotscore, Injury, Expected.Return, -playerid, -isTarget)
    datatable(ff,
              options = list(paging = FALSE, info = FALSE, autoWidth = FALSE),
              escape = FALSE) %>%
      formatCurrency('pDFL') %>%
      formatRound(c('hotscore'), 2) %>%
      formatRound('Age', 0)
  })

# --- Player Snapshot: unified search + detail view ---
  searchData_r <- reactive({
    rv$refreshCount
    hCols <- c("playerid", "Player", "Pos", "MLB", "Age", "pDFL", "Team", "Salary", "Contract", "hotscore")
    pCols <- c("playerid", "Player", "Pos", "MLB", "Age", "pDFL", "Team", "Salary", "Contract", "hotscore")
    h <- AllH %>% select(any_of(hCols)) %>% mutate(Type = "Hitter")
    p <- AllP %>% select(any_of(pCols)) %>% mutate(Type = "Pitcher")
    bind_rows(h, p) %>%
      distinct(playerid, .keep_all = TRUE) %>%
      arrange(-pDFL)
  })

  output$searchTable <- DT::renderDataTable({
    rv$refreshCount
    data <- searchData_r()
    data <- markTargets(data, isolate(rv$targets))
    data <- data %>% select(Target, Player, Type, Pos, MLB, Age, Owner = Team, Salary, Contract,
                            DFL = pDFL, Hot = hotscore, playerid)
    data <- data %>% select(-playerid)
    datatable(data, selection = 'single',
              options = list(pageLength = 25, autoWidth = FALSE, info = FALSE),
              filter = 'top', escape = FALSE) %>%
      formatCurrency('DFL') %>%
      formatRound(c('Age'), 0) %>%
      formatRound('Hot', 2) %>%
      formatCurrency('Salary', digits = 0)
  })

  output$playerSnapshot <- renderUI({
    sel <- input$searchTable_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      return(tags$div(style = "color:#888; padding:12px; font-style:italic;",
                      "Select a player from the table to see details."))
    }
    data <- searchData_r()
    if (sel > nrow(data)) return(NULL)
    pid <- as.character(data$playerid[sel])

    playerH <- AllH %>% filter(playerid == pid)
    playerP <- AllP %>% filter(playerid == pid)
    isHitter <- nrow(playerH) > 0
    if (!isHitter && nrow(playerP) == 0) {
      return(tags$div(style = "color:gray; padding:12px;", "Player not found."))
    }
    player <- if (isHitter) playerH[1, ] else playerP[1, ]

    playerName <- player$Player
    playerPos  <- player$Pos
    posElStr <- if (isHitter && !is.null(player$posEl) && !is.na(player$posEl) && player$posEl != "") {
      paste0(" (", player$posEl, ")")
    } else ""
    playerAge <- round(player$Age)
    playerMLB <- player$MLB
    ownerStr <- if (!is.na(player$Team) && player$Team != "Free Agent") {
      paste0(player$Team, "  —  $", player$Salary,
             if (!is.null(player$Contract) && !is.na(player$Contract)) paste0(" (", player$Contract, "yr)") else "")
    } else {
      "Free Agent"
    }

    headerUI <- tags$div(
      style = "padding:12px 16px; background:#2c3e50; color:white; border-radius:6px 6px 0 0;",
      tags$div(style = "font-size:20px; font-weight:bold;", HTML(playerName)),
      tags$div(style = "font-size:14px; margin-top:4px; color:#bdc3c7;",
               paste0(playerPos, posElStr, "  |  ", playerMLB, "  |  Age ", playerAge)),
      tags$div(style = "font-size:14px; margin-top:2px; color:#ecf0f1;", ownerStr)
    )

    hs <- if (!is.null(player$hotscore) && !is.na(player$hotscore)) round(player$hotscore, 2) else NA
    heroLine <- tags$div(style = "display:flex; gap:24px; font-size:22px; font-weight:bold; margin-bottom:8px;",
      tags$span(paste0("$", round(player$pDFL))),
      tags$span(style = "color:#888;", "|"),
      tags$span(paste0("Hotscore: ", ifelse(is.na(hs), "—", hs)))
    )
    sgp <- if (!is.null(player$pSGP) && !is.na(player$pSGP)) sprintf("%.2f", player$pSGP) else "—"
    rnk <- if (!is.null(player$Rank) && !is.na(player$Rank)) round(player$Rank) else "—"
    valLine <- tags$div(style = "display:flex; gap:16px; flex-wrap:wrap; font-size:15px; margin-bottom:8px;",
      tags$span(tags$strong("SGP: "), sgp),
      tags$span(tags$strong("Rank: "), rnk)
    )
    if (isHitter) {
      statLine <- tags$div(style = "display:flex; gap:16px; flex-wrap:wrap; font-size:15px;",
        tags$span(tags$strong("HR: "),  round(player$pHR)),
        tags$span(tags$strong("RBI: "), round(player$pRBI)),
        tags$span(tags$strong("R: "),   round(player$pR)),
        tags$span(tags$strong("SB: "),  round(player$pSB)),
        tags$span(tags$strong("AVG: "), sprintf("%.3f", player$pAVG))
      )
      actualLine <- tags$div(style = "display:flex; gap:16px; flex-wrap:wrap; font-size:13px; color:#666; margin-top:4px;",
        tags$span("YTD:"),
        tags$span(paste0("HR ",  ifelse(is.na(player$HR), 0, player$HR))),
        tags$span(paste0("RBI ", ifelse(is.na(player$RBI), 0, player$RBI))),
        tags$span(paste0("R ",   ifelse(is.na(player$R), 0, player$R))),
        tags$span(paste0("SB ",  ifelse(is.na(player$SB), 0, player$SB))),
        tags$span(paste0("AVG ", sprintf("%.3f", ifelse(is.na(player$AVG), 0, player$AVG))))
      )
    } else {
      statLine <- tags$div(style = "display:flex; gap:16px; flex-wrap:wrap; font-size:15px;",
        tags$span(tags$strong("W: "),   round(player$pW)),
        tags$span(tags$strong("SO: "),  round(player$pSO)),
        tags$span(tags$strong("ERA: "), sprintf("%.2f", player$pERA)),
        tags$span(tags$strong("SV: "),  round(player$pSV)),
        tags$span(tags$strong("HLD: "), round(player$pHLD))
      )
      actualLine <- tags$div(style = "display:flex; gap:16px; flex-wrap:wrap; font-size:13px; color:#666; margin-top:4px;",
        tags$span("YTD:"),
        tags$span(paste0("W ",   ifelse(is.na(player$W), 0, player$W))),
        tags$span(paste0("K ",   ifelse(is.na(player$K), 0, player$K))),
        tags$span(paste0("ERA ", sprintf("%.2f", ifelse(is.na(player$ERA), 0, player$ERA)))),
        tags$span(paste0("S ",   ifelse(is.na(player$S), 0, player$S))),
        tags$span(paste0("HD ",  ifelse(is.na(player$HD), 0, player$HD)))
      )
    }
    statsUI <- tags$div(
      style = "padding:12px 16px; background:#f8f9fa; border:1px solid #ddd; border-top:none;",
      heroLine, valLine, statLine, actualLine
    )

    pool <- if (isHitter) AllH else AllP
    comps <- pool %>% filter(Pos == playerPos, playerid != pid,
                             pDFL >= player$pDFL - 10, pDFL <= player$pDFL + 10) %>%
      arrange(abs(pDFL - player$pDFL)) %>% head(5)
    if (nrow(comps) > 0) {
      compRows <- lapply(seq_len(nrow(comps)), function(j) {
        hsVal <- if (!is.na(comps$hotscore[j])) round(comps$hotscore[j], 2) else NA
        hsColor <- if (is.na(hsVal)) "#888" else if (hsVal >= 2) "#2ecc71" else if (hsVal < -1) "#e74c3c" else "#666"
        hsText <- if (is.na(hsVal)) "" else paste0("HS ", hsVal)
        ownerText <- if (!is.na(comps$Team[j]) && comps$Team[j] != "Free Agent") comps$Team[j] else "FA"
        tags$div(style = "font-size:14px; padding:2px 0;",
          tags$span(style = "display:inline-block; width:180px;", HTML(comps$Player[j])),
          tags$span(style = "display:inline-block; width:60px;", paste0("$", round(comps$pDFL[j]))),
          tags$span(style = "display:inline-block; width:140px; color:#666;", ownerText),
          tags$span(style = paste0("color:", hsColor, ";"), hsText)
        )
      })
      comparablesUI <- tags$div(
        tags$strong(style = "font-size:16px;", "Comparable Players"),
        tags$div(style = "font-size:12px; color:#888; margin-bottom:4px;",
                 paste0("Same position, ±$10 DFL")),
        tags$div(compRows)
      )
    } else {
      comparablesUI <- tags$div(style = "color:#888; font-size:14px;", "No comparable players at this position.")
    }

    hasInjury <- !is.null(player$Injury) && !is.na(player$Injury) &&
                 nchar(trimws(as.character(player$Injury))) > 0
    injUI <- if (hasInjury) {
      retStr <- if (!is.null(player$Expected.Return) && !is.na(player$Expected.Return) &&
                    player$Expected.Return != "") {
        tags$span(style = "margin-left:12px; color:#888;",
                  paste0("(return: ", player$Expected.Return, ")"))
      } else NULL
      tags$div(style = "padding:10px 16px; background:#fff3cd; border:1px solid #ddd; border-top:none; font-size:14px;",
        tags$div(tags$strong("Injury: "), player$Injury, retStr)
      )
    } else NULL

    sectionStyle <- "border:1px solid #ddd; border-top:none; padding:12px 16px;"
    tags$div(style = "border-radius:6px; overflow:hidden;",
      headerUI,
      statsUI,
      tags$div(style = sectionStyle, comparablesUI),
      injUI
    )
  })

# --- Research tab: article scraping + LLM extraction ---
  observeEvent(input$analyzeBtn, {
    mode <- input$researchMode

    if (mode == "url") {
      url <- trimws(input$researchUrl)
      if (url == "" || !grepl("^https?://", url)) {
        showNotification("Please enter a valid URL", type = "warning")
        return()
      }
    } else {
      pastedText <- trimws(input$researchText)
      if (pastedText == "" || nchar(pastedText) < 50) {
        showNotification("Please paste article text (at least 50 characters)", type = "warning")
        return()
      }
    }

    shinyjs::disable("analyzeBtn")
    showNotification("Fetching article...", type = "message", duration = NULL, id = "researchMsg")

    tryCatch({
      if (mode == "url") {
        page <- rvest::read_html(url)
        articleNodes <- page %>% rvest::html_nodes("article")
        if (length(articleNodes) == 0) articleNodes <- page %>% rvest::html_nodes("#content")
        if (length(articleNodes) == 0) articleNodes <- page %>% rvest::html_nodes(".post-content, .entry-content, .article-body")
        if (length(articleNodes) > 0) {
          articleText <- articleNodes %>% rvest::html_text2() %>% paste(collapse = "\n\n")
        } else {
          articleText <- page %>%
            rvest::html_nodes("p, h1, h2, h3, h4, li") %>%
            rvest::html_text2() %>%
            paste(collapse = "\n\n")
        }

        pageTitle <- tryCatch(
          page %>% rvest::html_node("title") %>% rvest::html_text2(),
          error = function(e) "Unknown Article"
        )
        sourceDomain <- gsub("^https?://([^/]+).*", "\\1", url)
      } else {
        articleText <- pastedText
        pageTitle <- "Pasted Article"
        sourceDomain <- "manual"
      }

      if (nchar(articleText) > 12000) {
        articleText <- substr(articleText, 1, 12000)
      }

      removeNotification("researchMsg")
      showNotification("Analyzing with Claude...", type = "message", duration = NULL, id = "researchMsg")

      prompt <- paste0(
        'You are a baseball fantasy analyst assistant. Extract all baseball players ',
        'mentioned in the following article. For each player the author is highlighting ',
        'as a target, sleeper, breakout, value pick, or otherwise recommending, return ',
        'a JSON array with these fields:\n\n',
        '- full_name: the player\'s full name (first and last)\n',
        '- summary: one sentence describing why the author thinks this player is interesting\n',
        '- tags: comma-separated list from these options: Sleeper, Breakout, Bounce-back, ',
        'Value, Upside, Buy-low, Sell-high, Injury-risk, Closer, Holds, Steals, Power, ',
        'AVG, Pitching, Strikeouts, Saves, Speed, Ratios\n\n',
        'Only include players the author is specifically recommending or discussing ',
        'positively. Skip players mentioned only in passing or as comparisons.\n\n',
        'Return ONLY the raw JSON array. No markdown, no code fences, no explanation. Example:\n',
        '[{"full_name": "Luis Arraez", "summary": "Hitting .340 in spring with strong ',
        'lineup protection boosting BA and R upside", "tags": "Sleeper, AVG, Value"}]\n\n',
        'Article text:\n', articleText
      )

      response <- callClaudeAPI(prompt, max_tokens = 4096)

      if (is.null(response) || length(response) == 0 || is.na(response) || response == "") {
        removeNotification("researchMsg")
        showNotification("Empty response from Claude API", type = "error", duration = 15)
        shinyjs::enable("analyzeBtn")
        return()
      }

      response <- gsub("^\\s*```json\\s*", "", response)
      response <- gsub("^\\s*```\\s*", "", response)
      response <- gsub("\\s*```\\s*$", "", response)
      response <- trimws(response)

      if (grepl("^\\[", response) && !grepl("\\]\\s*$", response)) {
        lastBrace <- regexpr("\\}[^\\}]*$", response)
        if (lastBrace > 0) {
          response <- paste0(substr(response, 1, lastBrace), "]")
        }
      }

      if (!grepl("^\\s*\\[", response)) {
        retryPrompt <- paste0(
          'Return ONLY a raw JSON array (no markdown, no code fences) of objects with fields: full_name, summary, tags. ',
          'Example: [{"full_name":"Mike Trout","summary":"Still elite","tags":"Power"}]. ',
          'Extract players recommended in this article:\n\n',
          substr(articleText, 1, 4000)
        )
        response <- callClaudeAPI(retryPrompt, max_tokens = 4096)
        response <- gsub("^\\s*```json\\s*", "", response)
        response <- gsub("^\\s*```\\s*", "", response)
        response <- gsub("\\s*```\\s*$", "", response)
        response <- trimws(response)
        if (!grepl("^\\s*\\[", response)) {
          removeNotification("researchMsg")
          cat("Research tab Claude API error:", substr(response, 1, 500), "\n")
          showNotification(paste0("Claude API error: ", substr(response, 1, 200)), type = "error", duration = 30)
          shinyjs::enable("analyzeBtn")
          return()
        }
      }

      extracted <- tryCatch(
        jsonlite::fromJSON(response),
        error = function(e) {
          removeNotification("researchMsg")
          cat("Research tab JSON parse error:", e$message, "\n")
          showNotification(paste0("Failed to parse Claude response: ", e$message), type = "error", duration = 30)
          shinyjs::enable("analyzeBtn")
          return(NULL)
        }
      )

      if (is.null(extracted) || !is.data.frame(extracted) || nrow(extracted) == 0) {
        removeNotification("researchMsg")
        showNotification("No players found in this article", type = "warning")
        rv$researchH <- data.frame()
        rv$researchP <- data.frame()
        rv$researchUnmatched <- character(0)
        rv$researchTitle <- pageTitle
        shinyjs::enable("analyzeBtn")
        return()
      }

      removeNotification("researchMsg")
      showNotification("Matching players...", type = "message", duration = NULL, id = "researchMsg")

      availH <- filter(AllH, Team == 'Free Agent')
      availP <- filter(AllP, Team == 'Free Agent')
      allAvail <- bind_rows(
        availH %>% mutate(poolType = "H"),
        availP %>% mutate(poolType = "P")
      )
      cleanNames <- tolower(allAvail$Player)

      matchedRows <- list()
      unmatched <- character(0)

      for (i in seq_len(nrow(extracted))) {
        fname <- extracted$full_name[i]
        fnameL <- tolower(fname)

        exactIdx <- which(cleanNames == fnameL)
        if (length(exactIdx) > 0) {
          row <- allAvail[exactIdx[1], ]
          row$Tags <- extracted$tags[i]
          row$Summary <- extracted$summary[i]
          row$fuzzy <- FALSE
          matchedRows <- c(matchedRows, list(row))
          next
        }

        fuzzyIdx <- agrep(fnameL, cleanNames, max.distance = 0.15, ignore.case = TRUE)
        if (length(fuzzyIdx) > 0) {
          row <- allAvail[fuzzyIdx[1], ]
          row$Tags <- extracted$tags[i]
          row$Summary <- extracted$summary[i]
          row$fuzzy <- TRUE
          matchedRows <- c(matchedRows, list(row))
        } else {
          unmatched <- c(unmatched, fname)
        }
      }

      if (length(matchedRows) == 0) {
        removeNotification("researchMsg")
        showNotification("No matched free agents found", type = "warning")
        rv$researchH <- data.frame()
        rv$researchP <- data.frame()
        rv$researchUnmatched <- unmatched
        rv$researchTitle <- pageTitle
        shinyjs::enable("analyzeBtn")
        return()
      }

      matched <- bind_rows(matchedRows)

      matched$Player <- ifelse(matched$fuzzy,
                               paste0("~ ", matched$Player),
                               matched$Player)

      mH <- matched %>% filter(poolType == "H") %>%
        arrange(-pDFL) %>%
        select(Player, Pos, Tags, Summary, Age, DFL = pDFL, SGP = pSGP,
               HR = pHR, RBI = pRBI, R = pR, SB = pSB, AVG = pAVG,
               Injury, Expected.Return, playerid)

      mP <- matched %>% filter(poolType == "P") %>%
        arrange(-pDFL) %>%
        select(Player, Pos, Tags, Summary, Age, DFL = pDFL, SGP = pSGP,
               W = pW, SO = pSO, ERA = pERA, SV = pSV, HLD = pHLD, `K/9` = `pK/9`,
               Injury, Expected.Return, playerid)

      rv$researchH <- mH
      rv$researchP <- mP
      rv$researchUnmatched <- unmatched
      rv$researchTitle <- paste0(pageTitle, " (", sourceDomain, ")")

      removeNotification("researchMsg")
      showNotification(paste0("Found ", nrow(matched), " free agent(s) from article"), type = "message")
      shinyjs::enable("analyzeBtn")

    }, error = function(e) {
      removeNotification("researchMsg")
      cat("Research tab error:", e$message, "\n")
      showNotification(paste0("Error: ", e$message), type = "error", duration = 30)
      shinyjs::enable("analyzeBtn")
    })
  })

  output$researchH <- DT::renderDataTable({
    df <- rv$researchH
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Message = "No hitters found. Paste an article URL and click Analyze."),
                       options = list(dom = 't'), selection = 'none'))
    }
    df <- markTargets(df, isolate(rv$targets))
    df <- df %>% select(Target, everything(), -playerid, -isTarget)
    datatable(df, escape = FALSE,
              options = list(pageLength = 20)) %>%
      formatCurrency('DFL') %>%
      formatRound(c('SGP', 'AVG'), 3) %>%
      formatRound(c('Age', 'HR', 'RBI', 'R', 'SB'), 0)
  })

  output$researchP <- DT::renderDataTable({
    df <- rv$researchP
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Message = "No pitchers found. Paste an article URL and click Analyze."),
                       options = list(dom = 't'), selection = 'none'))
    }
    df <- markTargets(df, isolate(rv$targets))
    df <- df %>% select(Target, everything(), -playerid, -isTarget)
    datatable(df, escape = FALSE,
              options = list(pageLength = 20)) %>%
      formatCurrency('DFL') %>%
      formatRound(c('SGP', 'ERA', 'K/9'), 3) %>%
      formatRound(c('Age', 'W', 'SO', 'SV', 'HLD'), 0)
  })

  output$researchStatus <- renderUI({
    title <- rv$researchTitle
    nH <- nrow(rv$researchH)
    nP <- nrow(rv$researchP)
    if (title == "" && nH == 0 && nP == 0) return(NULL)
    tags$div(style = "margin-top:10px; font-size:13px; line-height:1.6;",
      tags$strong(title),
      tags$br(),
      paste0(nH, " hitter(s), ", nP, " pitcher(s) found")
    )
  })

  output$researchUnmatched <- renderUI({
    um <- rv$researchUnmatched
    if (length(um) == 0) return(NULL)
    tags$div(style = "margin-top:10px; font-size:12px; color:#888;",
      tags$em(paste0("Could not match: ", paste(um, collapse = ", ")))
    )
  })

})
