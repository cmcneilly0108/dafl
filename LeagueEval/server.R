# LeagueEval server.R — reactive in-season evaluation tool

# runApp("~/Dropbox/Personal/DAFL/LeagueEval", host = "0.0.0.0", port = 3838)

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

# --- Research-tab recurring-column sources (Get Latest) ---
# Hand-edited list of recurring article columns; see researchSources.json.
researchSourcesFile <- "../researchSources.json"
researchSources <- tryCatch({
  if (file.exists(researchSourcesFile)) {
    jsonlite::fromJSON(researchSourcesFile, simplifyDataFrame = FALSE)
  } else list()
}, error = function(e) list())
researchSourceNames <- vapply(researchSources, function(s) s$name, character(1))


# --- Category-status shared helpers ------------------------------------------
# Used by output$catSummary (per-team category detail) and output$pointsInPlay
# (league-wide roll-up). Both read the same globals from inSeasonPulse.r —
# cstand, standings, aWeek — so these stay plain functions over those rather
# than reactives; the renderers already invalidate on rv$refreshCount.

# Per-category rank direction. ERA ranked low-to-high, the rest high-to-low
# (lower score = the team is worse in that category).
catRankCats <- list(
  list(cat='W',  rev=FALSE), list(cat='K',  rev=FALSE),
  list(cat='S',  rev=FALSE), list(cat='HD', rev=FALSE),
  list(cat='ERA',rev=TRUE),  list(cat='HR', rev=FALSE),
  list(cat='RBI',rev=FALSE), list(cat='R',  rev=FALSE),
  list(cat='SB', rev=FALSE), list(cat='BA', rev=FALSE)
)
# Counting cats accumulate linearly, so the yardstick is weekly production.
# BA/ERA are rate stats — handled via their typical weekly swing instead.
catCountingCats <- c('HR','RBI','SB','R','W','HD','S','K')

# Rate-stat yardstick: the league's typical week-over-week movement in BA/ERA,
# measured from the weekly standings snapshots (the analog of a week's worth
# of production). Ratios get stickier as AB/IP pile up, so we use only the
# most recent ~4 week transitions — the window self-adjusts as the season
# wears on. NA if snapshot history is missing.
catSwing <- function(col, nWeeks = 4) {
  if (!exists("standings") ||
      !all(c(col, "Team", "Week") %in% names(standings))) return(NA_real_)
  s   <- standings
  wks <- sort(unique(s$Week))
  keep <- if (length(wks) > nWeeks + 1) tail(wks, nWeeks + 1) else wks
  s    <- s[s$Week %in% keep, ]
  s    <- s[order(s$Team, s$Week), ]
  vals <- suppressWarnings(as.numeric(s[[col]]))
  byTeam <- tapply(vals, s$Team, function(x) mean(abs(diff(x)), na.rm = TRUE))
  mean(byTeam, na.rm = TRUE)
}

# One week of typical movement in a category, in that category's own units.
# `v` is the league-wide vector of current values for the category. Returns NA
# when the inputs aren't there (no aWeek yet, too little snapshot history for
# the rate stats) — callers treat such a category as having no measurable gap.
catWeekly <- function(cat, v) {
  if (cat %in% catCountingCats) {
    if (!exists("aWeek") || !is.finite(aWeek) || aWeek <= 0) return(NA_real_)
    mean(v, na.rm = TRUE) / aWeek
  } else if (cat == "BA")  catSwing("BA")
  else   if (cat == "ERA") catSwing("ERA")
  else                     NA_real_
}

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
    researchTitle = "",
    researchExtracted = 0,
    researchFound = ""
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
      actionButton('reloadBtn', 'Reload from Disk',
                   class = 'btn-primary btn-sm',
                   icon = icon('rotate-right')),
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


# --- Reload from Disk (re-read files, skip upstream fetch when files are fresh) ---
  observeEvent(input$reloadBtn, {
    removeModal()
    showNotification("Reloading from disk...", type = "message", duration = NULL, id = "reloadMsg")
    tryCatch({
      source("../code/inSeasonPulse.r", local = globalenv())
      message("[reload] dstandfull leader: ",
              paste(dstandfull[1, ], collapse = " / "))
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
      removeNotification("reloadMsg")
      showNotification("Reloaded from disk!", type = "message")
    }, error = function(e) {
      removeNotification("reloadMsg")
      showNotification(paste0("Reload failed: ", e$message), type = "error", duration = 10)
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
             select(playerid,Player,Pos,Age,pDFL,hotscore,Team,Salary,Contract,pSGP,Rank,'Pitching+',pW,pSO,pSV,pHLD,pERA,`pK/9`,pFIP,W,K,S,HD,ERA,twostarts,LVG,Injury,Expected.Return),
           ifelse(input$e3 == 'Hitters',
                  ff <- ffh %>%
                    select(playerid,Player,Pos,Age,pDFL,hotscore,Team,Salary,Contract,pSGP,Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,AVG,Injury,Expected.Return) %>%
                    arrange(-pDFL),
                  ff <- ffh %>% filter(str_detect(posEl,input$e3)) %>%
                    select(playerid,Player,Pos,Age,pDFL,hotscore,Team,Salary,Contract,pSGP,Rank,pHR,pRBI,pR,pSB,pAVG,HR,RBI,R,SB,AVG,Injury,Expected.Return) %>%
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
      c('Player','Pos','pDFL','hotscore','Salary','Contract','Injury'),
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
                    c('#E2EFE4', '#FAF0D7', '#F7DFD9')
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

    # Rank direction, the counting/rate split and the weekly yardstick are
    # shared with output$pointsInPlay — see catRankCats / catWeekly above.
    rankCats <- catRankCats

    # Map a short nickname back to the full team name for display.
    fullName <- function(short) {
      if (exists("nicks") && short %in% nicks$Short)
        nicks$Team[match(short, nicks$Short)]
      else short
    }
    # Teams in the overall top 5 (by standings Rank) — bolded in the name cells.
    top5 <- if ("Rank" %in% names(cstand))
              cstand$Team[which(suppressWarnings(as.numeric(cstand$Rank)) <= 5)]
            else character(0)
    nameCell <- function(short) {
      if (is.na(short)) return("—")
      nm <- fullName(short)
      if (short %in% top5) paste0("<b>", nm, "</b>") else nm
    }
    # toNext / awayFrom: how much of the stat separates this team from the team
    # one rank better / worse. Magnitude only — the neighbour is always strictly
    # better/worse, so direction is implicit and BA/ERA fall out the same way.
    fmtGap <- function(cat, x) {
      if (is.na(x))          "—"
      else if (x == 0)       "tie"   # dead even with the neighbour
      else if (cat == "BA")  sprintf("%.3f", x)
      else if (cat == "ERA") sprintf("%.2f", x)
      else                   as.character(round(x))
    }
    # gainIn / loseIn: that gap expressed in weeks of typical category movement.
    fmtWeeks <- function(x) if (is.na(x)) "—" else sprintf("%.1f", x)

    msRows <- lapply(rankCats, function(rc) {
      v <- suppressWarnings(as.numeric(cstand[[rc$cat]]))
      ord <- if (rc$rev) order(-v, na.last = TRUE) else order(v, na.last = TRUE)
      teamsRanked <- cstand$Team[ord]   # position 1 = worst, last = best
      valsRanked  <- v[ord]
      pos <- which(teamsRanked == shortTeam)
      nT  <- length(ord)
      myVal <- valsRanked[pos]

      aheadShort  <- if (pos < nT) teamsRanked[pos + 1] else NA_character_
      behindShort <- if (pos > 1)  teamsRanked[pos - 1] else NA_character_
      toNextVal   <- if (pos < nT) abs(valsRanked[pos + 1] - myVal) else NA_real_
      awayFromVal <- if (pos > 1)  abs(myVal - valsRanked[pos - 1]) else NA_real_

      # Yardstick: weekly production for counting stats, typical weekly swing for BA/ERA.
      leagueWeekly <- catWeekly(rc$cat, v)
      okRate <- !is.na(leagueWeekly) && leagueWeekly > 0
      weeksGain <- if (okRate && !is.na(toNextVal))   toNextVal   / leagueWeekly else NA_real_
      weeksLose <- if (okRate && !is.na(awayFromVal)) awayFromVal / leagueWeekly else NA_real_

      data.frame(category   = rc$cat,
                 score      = pos,
                 teamAhead  = nameCell(aheadShort),
                 toNext     = fmtGap(rc$cat, toNextVal),
                 gainIn     = fmtWeeks(weeksGain),
                 teamBehind = nameCell(behindShort),
                 awayFrom   = fmtGap(rc$cat, awayFromVal),
                 loseIn     = fmtWeeks(weeksLose),
                 .wG = weeksGain, .wL = weeksLose,   # numeric helpers, dropped below
                 stringsAsFactors = FALSE)
    })
    msDf <- do.call(rbind, msRows)

    # Priority flag: a point within ~1 week of production is in play this week.
    msDf$priority <- mapply(function(g, l) {
      gain   <- !is.na(g) && g <= 1
      defend <- !is.na(l) && l <= 1
      if (gain && defend) "⚔ Both"
      else if (gain)      "🎯 Gain"
      else if (defend)    "🛡 Defend"
      else                ""
    }, msDf$.wG, msDf$.wL)

    # Most in-play categories first (smallest weeks-to-act); N/A categories last.
    actionKey <- mapply(function(g, l) {
      vals <- c(g, l); vals <- vals[!is.na(vals)]
      if (length(vals) == 0) NA_real_ else min(vals)
    }, msDf$.wG, msDf$.wL)
    msDf <- msDf[order(actionKey, na.last = TRUE), ]

    teamCatSummary <- msDf[, c('category','score','teamAhead','toNext','gainIn',
                               'teamBehind','awayFrom','loseIn','priority')]

    datatable(teamCatSummary,
              rownames = FALSE,
              escape = FALSE,   # render <b> bolding on top-5 team names
              caption = htmltools::tags$caption(
                style = 'caption-side: bottom; font-size: 12px; color: #666;',
                'gainIn / loseIn = weeks of typical category movement to close the gap. ',
                '🎯 Gain = a point within ~1 week above you · 🛡 Defend = within ~1 week below · ⚔ Both.'),
              options = list(pageLength = 20,
                             columnDefs = list(list(className = 'dt-center',
                                                    targets = '_all'))))
  })

# Category Status — league-wide roll-up of the points each team can gain or
# lose in a single week. Same yardstick as catSummary above, but instead of
# only the immediate neighbours it counts *every* rival within a week's
# production: a team sitting in a bunched category has several points genuinely
# in play, and the neighbour-only view flattens exactly that signal.
  output$pointsInPlay <- DT::renderDataTable({
    rv$refreshCount
    if (!exists("cstand") || nrow(cstand) == 0) return(NULL)

    teamsAll <- cstand$Team
    nT       <- length(teamsAll)
    gainN <- setNames(integer(nT), teamsAll)
    riskN <- setNames(integer(nT), teamsAll)
    # Per-team named integer vectors: category -> points in play there.
    gainBy <- setNames(vector("list", nT), teamsAll)
    riskBy <- setNames(vector("list", nT), teamsAll)

    for (rc in catRankCats) {
      v  <- suppressWarnings(as.numeric(cstand[[rc$cat]]))
      wk <- catWeekly(rc$cat, v)
      # No usable yardstick (no aWeek, or too little snapshot history for the
      # rate stats) — this category contributes nothing to anyone, matching the
      # "—" fallback in the per-team table.
      if (is.na(wk) || wk <= 0) next
      ord <- if (rc$rev) order(-v, na.last = TRUE) else order(v, na.last = TRUE)
      teamsRanked <- cstand$Team[ord]   # position 1 = worst, last = best
      valsRanked  <- v[ord]
      for (i in seq_len(nT)) {
        me <- valsRanked[i]
        if (is.na(me)) next
        tm     <- teamsRanked[i]
        better <- if (i < nT) valsRanked[(i + 1):nT] else numeric(0)
        worse  <- if (i > 1)  valsRanked[1:(i - 1)]  else numeric(0)
        # Exact ties have a gap of 0 and so count as in play — which is how a
        # tie actually behaves in the standings.
        g <- sum(!is.na(better) & abs(better - me) <= wk)
        r <- sum(!is.na(worse)  & abs(me - worse)  <= wk)
        if (g > 0) {
          gainN[[tm]] <- gainN[[tm]] + g
          gainBy[[tm]] <- c(gainBy[[tm]], setNames(g, rc$cat))
        }
        if (r > 0) {
          riskN[[tm]] <- riskN[[tm]] + r
          riskBy[[tm]] <- c(riskBy[[tm]], setNames(r, rc$cat))
        }
      }
    }

    # "SB(2), HD(1)" — busiest categories first, ties keeping category order.
    fmtBy <- function(x) {
      if (length(x) == 0) return("")
      x <- x[order(-x)]
      paste(sprintf("%s(%d)", names(x), x), collapse = ", ")
    }
    whereCell <- vapply(teamsAll, function(tm) {
      parts <- c(if (nzchar(fmtBy(gainBy[[tm]]))) paste0("Gain: ", fmtBy(gainBy[[tm]])),
                 if (nzchar(fmtBy(riskBy[[tm]]))) paste0("Risk: ", fmtBy(riskBy[[tm]])))
      if (length(parts) == 0) "—" else paste(parts, collapse = " · ")
    }, character(1))

    # The teamSelect sidebar doesn't filter this table, so bold the selected
    # team's name to keep the control visibly related to it.
    sel <- input$teamSelect
    selShort <- if (!is.null(sel) && sel != '' && exists("nicks") && sel %in% nicks$Team) {
      nicks$Short[match(sel, nicks$Team)]
    } else sel
    teamCell <- vapply(teamsAll, function(short) {
      nm <- if (exists("nicks") && short %in% nicks$Short)
              nicks$Team[match(short, nicks$Short)] else short
      if (!is.null(selShort) && identical(short, selShort)) paste0("<b>", nm, "</b>") else nm
    }, character(1))

    gain <- as.integer(gainN[teamsAll])
    risk <- as.integer(riskN[teamsAll])
    df <- data.frame(
      Rank  = suppressWarnings(as.numeric(cstand$Rank)),
      Team  = teamCell,
      Total = suppressWarnings(as.numeric(cstand$Total)),
      Gain  = gain,
      Risk  = risk,
      Net   = gain - risk,
      Swing = gain + risk,
      Where = whereCell,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    df <- df[order(df$Rank, na.last = TRUE), ]

    datatable(df,
              rownames = FALSE,
              escape = FALSE,   # render <b> on the selected team
              caption = htmltools::tags$caption(
                style = 'caption-side: bottom; font-size: 12px; color: #666;',
                'Gain / Risk = standings points within ~1 week of typical category movement ',
                'above / below you, counting every rival in range — not just the nearest. ',
                'Net = Gain − Risk · Swing = total points in play.'),
              options = list(paging = FALSE, info = FALSE, searching = FALSE,
                             ordering = TRUE, order = list(),
                             columnDefs = list(
                               list(className = 'dt-center',
                                    targets = which(!names(df) %in%
                                                    c("Team", "Where")) - 1),
                               list(targets = which(names(df) == "Where") - 1,
                                    orderable = FALSE)
                             ))) %>%
      formatStyle('Net',
                  backgroundColor = styleInterval(
                    c(-0.5, 0.5),
                    c('#F7DFD9', '#FAF0D7', '#E2EFE4')
                  ))
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
      formatStyle('High',   backgroundColor = '#E2EFE4') %>%
      formatStyle('Medium', backgroundColor = '#FAF0D7') %>%
      formatStyle('Low',    backgroundColor = '#F7DFD9')
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

    # Players can be starred from lists sourced independently of the active
    # projection pool (e.g. the Injured list uses the FanGraphs injury feed).
    # AllH/AllP are built with an inner_join against the active projection source
    # (inSeasonPulse.r), so a starred player the active source doesn't project
    # (season-ending injuries under ATC, etc.) is absent here and would silently
    # vanish from My Targets. Backfill those from the injury feed so anything you
    # can star, you can see. Ownership comes from the CBS frames (Allhitters/
    # Allpitchers), which carry live Team/Free-Agent status for every CBS player
    # including the ones the projection join drops; not in CBS => not owned => FA.
    missing <- setdiff(as.character(rv$targets), allPlayers$playerid)
    if (length(missing) > 0 && exists("injOrigBase")) {
      cbsOwn <- bind_rows(
        if (exists("Allhitters"))  Allhitters  %>% select(playerid, Team) else NULL,
        if (exists("Allpitchers")) Allpitchers %>% select(playerid, Team) else NULL
      ) %>% mutate(playerid = as.character(playerid)) %>%
        distinct(playerid, .keep_all = TRUE)
      fb <- injOrigBase %>%
        filter(as.character(playerid) %in% missing) %>%
        transmute(
          playerid        = as.character(playerid),
          Player          = Player,
          Pos             = position,
          Age             = suppressWarnings(as.integer(cyear) - as.integer(birth_year)),
          pDFL            = NA_real_,
          hotscore        = NA_real_,
          Injury          = Injury,
          Expected.Return = `Latest Update`
        ) %>%
        distinct(playerid, .keep_all = TRUE) %>%
        left_join(cbsOwn, by = "playerid") %>%
        mutate(Team = ifelse(is.na(Team), "Free Agent", Team))
      if (nrow(fb) > 0) allPlayers <- bind_rows(allPlayers, fb)
    }

    info <- allPlayers %>% filter(playerid %in% rv$targets) %>% arrange(-pDFL)
    if (isTRUE(input$faTargets)) info <- filter(info, Team == 'Free Agent')
    ff <- markTargets(info, rv$targets) %>% select(Target, Player, Team, Pos, Age, pDFL, hotscore, Injury, Expected.Return, -playerid, -isTarget)
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

  # playerid driving the snapshot detail + trend chart. Set by selecting a row in
  # the search table OR by the "Player Snapshot" item in the player-name popup menu
  # (input$gotoSnapshot, fired from daflPlayerMenu in ui.R).
  snapshotPid <- reactiveVal(NULL)
  # Only react to an actual row selection (ignoreNULL default TRUE) — otherwise a
  # NULL selection emitted when the table redraws on tab-switch would clobber a
  # pid that the player-name menu just set.
  observeEvent(input$searchTable_rows_selected, {
    sel <- input$searchTable_rows_selected
    data <- searchData_r()
    if (sel <= nrow(data)) snapshotPid(as.character(data$playerid[sel]))
  })
  observeEvent(input$gotoSnapshot, {
    snapshotPid(as.character(input$gotoSnapshot))
    updateNavbarPage(session, "mainNav", selected = "Player Snapshot")
    session$sendCustomMessage("removePlayerMenu", list())
  })

  output$playerSnapshot <- renderUI({
    pid <- snapshotPid()
    if (is.null(pid)) {
      return(tags$div(style = "color:#888; padding:12px; font-style:italic;",
                      "Select a player from the table to see details."))
    }

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
      tags$div(style = "padding:10px 16px; background:#FAF0D7; border:1px solid #ddd; border-top:none; font-size:14px;",
        tags$div(tags$strong("Injury: "), player$Injury, retStr)
      )
    } else NULL

    sectionStyle <- "border:1px solid #ddd; border-top:none; padding:12px 16px;"
    trendUI <- tags$div(style = sectionStyle,
      tags$strong(style = "font-size:16px;", "Hotscore Trend"),
      plotlyOutput("snapshotTrend", height = "230px")
    )
    tags$div(style = "border-radius:6px; overflow:hidden;",
      headerUI,
      statsUI,
      trendUI,
      tags$div(style = sectionStyle, comparablesUI),
      injUI
    )
  })

  # Hotscore-over-time chart for the selected snapshot player (compact, single line).
  output$snapshotTrend <- renderPlotly({
    rv$refreshCount
    pid <- snapshotPid()
    # Collapse to one point per date — two-way players (e.g. Ohtani) carry
    # separate hitter and pitcher hotscore rows that would otherwise zigzag.
    df <- if (is.null(pid)) NULL else
      trending %>% filter(as.character(playerid) == pid) %>%
        group_by(Date) %>%
        summarise(hotscore = mean(hotscore, na.rm = TRUE), .groups = "drop") %>%
        arrange(Date)
    if (is.null(df) || nrow(df) == 0) {
      return(
        plot_ly() %>%
          plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
                 annotations = list(text = "No trend history", showarrow = FALSE,
                                    xref = "paper", yref = "paper", x = 0.5, y = 0.5,
                                    font = list(color = "#888", size = 14))) %>%
          plotly::config(displayModeBar = FALSE)
      )
    }
    plot_ly(df, x = ~Date, y = ~hotscore, type = "scatter", mode = "lines+markers",
            line = list(width = 3, color = "#2c3e50"),
            marker = list(size = 7, color = "#2c3e50"),
            hovertemplate = "%{x|%b %d}: %{y:.2f}<extra></extra>") %>%
      plotly::layout(margin = list(l = 40, r = 10, t = 10, b = 30),
             xaxis = list(title = ""),
             yaxis = list(title = "Hotscore", zeroline = TRUE, zerolinecolor = "#ccc"),
             showlegend = FALSE) %>%
      plotly::config(displayModeBar = FALSE)
  })

# --- Research tab: article scraping + LLM extraction ---
  # Core analysis used by both "Analyze Article" and "Get Latest".
  doAnalyze <- function(mode, url, pastedText) {
    if (mode == "url") {
      url <- trimws(url)
      if (url == "" || !grepl("^https?://", url)) {
        showNotification("Please enter a valid URL", type = "warning")
        return()
      }
    } else {
      pastedText <- trimws(pastedText)
      if (pastedText == "" || nchar(pastedText) < 50) {
        showNotification("Please paste article text (at least 50 characters)", type = "warning")
        return()
      }
    }

    shinyjs::disable("analyzeBtn")
    shinyjs::disable("getLatestBtn")
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

      articleCharLimit <- 100000
      if (nchar(articleText) > articleCharLimit) {
        articleText <- substr(articleText, 1, articleCharLimit)
        showNotification(
          paste0("Article is long; analyzed the first ", format(articleCharLimit, big.mark = ","),
                 " characters. Some players near the end may be missed."),
          type = "warning", duration = 10)
      }

      removeNotification("researchMsg")
      showNotification("Analyzing with Claude...", type = "message", duration = NULL, id = "researchMsg")

      prompt <- paste0(
        'You are a baseball fantasy analyst assistant. Extract EVERY player the ',
        'article presents as a potential pickup, add, waiver target, or recommendation. ',
        'Be exhaustive: include all players discussed as add candidates, even briefly. ',
        'List players in the order they appear in the article.\n\n',
        'For each player, return a JSON array with these fields:\n\n',
        '- full_name: the player\'s full name (first and last)\n',
        '- mlb_team: the player\'s current MLB team as a standard abbreviation ',
        '(e.g. NYY, LAD, WSN, CHW). Use the parent club for minor leaguers. ',
        'Use an empty string "" only if the team is genuinely unclear.\n',
        '- summary: one sentence describing why the author thinks this player is interesting\n',
        '- tags: comma-separated list from these options: Sleeper, Breakout, Bounce-back, ',
        'Value, Upside, Buy-low, Sell-high, Injury-risk, Closer, Holds, Steals, Power, ',
        'AVG, Pitching, Strikeouts, Saves, Speed, Ratios\n\n',
        'Exclude players mentioned only as comparisons, or only as drop/avoid candidates.\n\n',
        'Return ONLY the raw JSON array. No markdown, no code fences, no explanation. Example:\n',
        '[{"full_name": "Luis Arraez", "mlb_team": "SDP", "summary": "Hitting .340 with strong ',
        'lineup protection boosting BA and R upside", "tags": "AVG, Value"}]\n\n',
        'Article text:\n', articleText
      )

      response <- callClaudeAPI(prompt, max_tokens = 8192, temperature = 0)

      if (is.null(response) || length(response) == 0 || is.na(response) || response == "") {
        removeNotification("researchMsg")
        showNotification("Empty response from Claude API", type = "error", duration = 15)
        shinyjs::enable("analyzeBtn"); shinyjs::enable("getLatestBtn")
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
          'Return ONLY a raw JSON array (no markdown, no code fences) of objects with fields: full_name, mlb_team, summary, tags. ',
          'mlb_team is the standard MLB abbreviation (e.g. NYY, LAD), or "" if unclear. ',
          'Example: [{"full_name":"Mike Trout","mlb_team":"LAA","summary":"Still elite","tags":"Power"}]. ',
          'Extract players recommended in this article:\n\n',
          articleText
        )
        response <- callClaudeAPI(retryPrompt, max_tokens = 8192, temperature = 0)
        response <- gsub("^\\s*```json\\s*", "", response)
        response <- gsub("^\\s*```\\s*", "", response)
        response <- gsub("\\s*```\\s*$", "", response)
        response <- trimws(response)
        if (!grepl("^\\s*\\[", response)) {
          removeNotification("researchMsg")
          cat("Research tab Claude API error:", substr(response, 1, 500), "\n")
          showNotification(paste0("Claude API error: ", substr(response, 1, 200)), type = "error", duration = 30)
          shinyjs::enable("analyzeBtn"); shinyjs::enable("getLatestBtn")
          return()
        }
      }

      extracted <- tryCatch(
        jsonlite::fromJSON(response),
        error = function(e) {
          removeNotification("researchMsg")
          cat("Research tab JSON parse error:", e$message, "\n")
          showNotification(paste0("Failed to parse Claude response: ", e$message), type = "error", duration = 30)
          shinyjs::enable("analyzeBtn"); shinyjs::enable("getLatestBtn")
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
        rv$researchExtracted <- 0
        shinyjs::enable("analyzeBtn"); shinyjs::enable("getLatestBtn")
        return()
      }

      rv$researchExtracted <- nrow(extracted)

      removeNotification("researchMsg")
      showNotification("Matching players...", type = "message", duration = NULL, id = "researchMsg")

      availH <- filter(AllH, Team == 'Free Agent')
      availP <- filter(AllP, Team == 'Free Agent')
      allAvail <- bind_rows(
        availH %>% mutate(poolType = "H"),
        availP %>% mutate(poolType = "P")
      )
      # Match on normalized name + MLB team (avoids fuzzy false positives like
      # "Luis Lara" -> "Luis Garcia"). See helpers in daflFunctions.r.
      poolNorm <- normPlayerName(allAvail$Player)
      poolTeam <- normMlbTeam(allAvail$MLB)
      hasTeamCol <- "mlb_team" %in% names(extracted)

      matchedRows <- list()
      unmatched <- character(0)

      for (i in seq_len(nrow(extracted))) {
        fname <- extracted$full_name[i]
        qName <- normPlayerName(fname)
        qTeam <- if (hasTeamCol) normMlbTeam(extracted$mlb_team[i]) else ""

        idx <- matchExtractedPlayer(qName, qTeam, poolNorm, poolTeam)
        if (!is.na(idx)) {
          row <- allAvail[idx, ]
          row$Tags <- extracted$tags[i]
          row$Summary <- extracted$summary[i]
          # Flag as approximate when matched by name only (no confirming team).
          row$fuzzy <- !(nzchar(qTeam) && poolTeam[idx] == qTeam)
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
        shinyjs::enable("analyzeBtn"); shinyjs::enable("getLatestBtn")
        return()
      }

      matched <- bind_rows(matchedRows)

      matched$Player <- ifelse(matched$fuzzy,
                               paste0("~ ", matched$Player),
                               matched$Player)

      mH <- matched %>% filter(poolType == "H") %>%
        arrange(-pDFL) %>%
        select(Player, Pos, Tags, Summary, Age, DFL = pDFL, Hot = hotscore, SGP = pSGP,
               HR = pHR, RBI = pRBI, R = pR, SB = pSB, AVG = pAVG,
               Injury, Expected.Return, playerid)

      mP <- matched %>% filter(poolType == "P") %>%
        arrange(-pDFL) %>%
        select(Player, Pos, Tags, Summary, Age, DFL = pDFL, Hot = hotscore, SGP = pSGP,
               W = pW, SO = pSO, ERA = pERA, SV = pSV, HLD = pHLD, `K/9` = `pK/9`,
               Injury, Expected.Return, playerid)

      rv$researchH <- mH
      rv$researchP <- mP
      rv$researchUnmatched <- unmatched
      rv$researchTitle <- paste0(pageTitle, " (", sourceDomain, ")")

      removeNotification("researchMsg")
      showNotification(paste0("Found ", nrow(matched), " free agent(s) from article"), type = "message")
      shinyjs::enable("analyzeBtn"); shinyjs::enable("getLatestBtn")

    }, error = function(e) {
      removeNotification("researchMsg")
      cat("Research tab error:", e$message, "\n")
      showNotification(paste0("Error: ", e$message), type = "error", duration = 30)
      shinyjs::enable("analyzeBtn"); shinyjs::enable("getLatestBtn")
    })
  }

  # Analyze button: use whatever is currently in the input controls.
  observeEvent(input$analyzeBtn, {
    rv$researchFound <- ""
    doAnalyze(input$researchMode, input$researchUrl, input$researchText)
  })

  # Populate the recurring-column dropdown from researchSources.json.
  if (length(researchSourceNames) > 0) {
    updateSelectInput(session, "researchSource", choices = researchSourceNames)
  }

  # Get Latest: resolve the newest URL for the selected source, then analyze it.
  observeEvent(input$getLatestBtn, {
    if (length(researchSources) == 0) {
      showNotification("No sources configured (researchSources.json)", type = "warning")
      return()
    }
    idx <- match(input$researchSource, researchSourceNames)
    if (is.na(idx)) {
      showNotification("Please pick a source", type = "warning")
      return()
    }
    src <- researchSources[[idx]]
    shinyjs::disable("getLatestBtn"); shinyjs::disable("analyzeBtn")
    showNotification(paste0("Finding latest: ", src$name, "..."),
                     type = "message", duration = NULL, id = "researchMsg")
    latest <- researchLatestUrl(src)
    removeNotification("researchMsg")
    if (is.null(latest) || is.null(latest$url) || latest$url == "") {
      showNotification(paste0("Couldn't find the latest article for ", src$name),
                       type = "error", duration = 15)
      shinyjs::enable("getLatestBtn"); shinyjs::enable("analyzeBtn")
      return()
    }
    updateRadioButtons(session, "researchMode", selected = "url")
    updateTextInput(session, "researchUrl", value = latest$url)
    rv$researchFound <- paste0("Found: ", latest$title,
                               " (", substr(latest$date, 1, 10), ")")
    doAnalyze("url", latest$url, "")
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
      formatRound('Hot', 2) %>%
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
      formatRound('Hot', 2) %>%
      formatRound(c('SGP', 'ERA', 'K/9'), 3) %>%
      formatRound(c('Age', 'W', 'SO', 'SV', 'HLD'), 0)
  })

  output$researchStatus <- renderUI({
    title <- rv$researchTitle
    nH <- nrow(rv$researchH)
    nP <- nrow(rv$researchP)
    nExtracted <- rv$researchExtracted
    found <- rv$researchFound
    if (title == "" && nH == 0 && nP == 0 && found == "") return(NULL)
    tags$div(style = "margin-top:10px; font-size:13px; line-height:1.6;",
      if (found != "") tagList(tags$span(style = "color:#3c763d;", found), tags$br()),
      tags$strong(title),
      tags$br(),
      paste0("Extracted: ", nExtracted, " player(s) from article"),
      tags$br(),
      paste0("Free agents: ", nH, " hitter(s), ", nP, " pitcher(s)")
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
