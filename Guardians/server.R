# Guardians Tracker — Shiny server. Sources the pulse script which hydrates
# globals (gRoster, gStats, gHot, gTxn, gIL, gProspects, gDepth, gTrend).

setwd("../code/")
source("./guardiansPulse.r")

library("dplyr")
library("DT")
library("plotly")

shinyServer(function(input, output, session) {

  rv <- reactiveValues(refreshCount = 0)

  # --- Settings modal: Refresh button ---
  observeEvent(input$gSettingsBtn, {
    showModal(modalDialog(
      title = "Settings",
      size = "s", easyClose = TRUE,
      tags$p(paste0("Latest snapshot: ", today)),
      actionButton('gRefreshBtn', 'Refresh Data',
                   class = 'btn-success btn-sm', icon = icon('refresh')),
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$gRefreshBtn, {
    removeModal()
    showNotification("Refreshing Guardians data…", type = "message",
                     duration = NULL, id = "gRefreshMsg")
    Sys.setenv(DAFL_FORCE_REFRESH = "1")
    on.exit(Sys.unsetenv("DAFL_FORCE_REFRESH"), add = TRUE)
    tryCatch({
      source("../code/guardiansPulse.r", local = globalenv())
      rv$refreshCount <- rv$refreshCount + 1
      removeNotification("gRefreshMsg")
      showNotification("Refreshed!", type = "message")
    }, error = function(e) {
      removeNotification("gRefreshMsg")
      showNotification(paste0("Refresh failed: ", e$message),
                       type = "error", duration = 15)
    })
  })

  # Placeholder outputs — populated by later tasks.
  # One card per level. Inside each card, players sorted by position and one-
  # line stat. ⬆/⬇ badge if the player's level changed in the last 7 days.
  output$gOrgTree <- renderUI({
    rv$refreshCount
    if (nrow(gRoster) == 0) {
      return(tags$div(style = "color:#888; font-style:italic;",
                      "No roster snapshot available."))
    }
    levels <- c("MLB","AAA","AA","A+","A","ACL","DSL")

    # Recent level changes (last 7 days) — query DB for snapshot history.
    recentMoves <- tryCatch({
      conn2 <- dbConnect(RSQLite::SQLite(), "../code/DAFL.db")
      on.exit(dbDisconnect(conn2))
      dbGetQuery(conn2, "
        SELECT mlb_id, level, snapshot_date FROM GuardiansRoster
        WHERE snapshot_date >= date(?, '-7 days')
        ORDER BY mlb_id, snapshot_date",
        params = list(as.character(Sys.Date())))
    }, error = function(e) data.frame())
    moveBadge <- function(pid) {
      if (nrow(recentMoves) == 0) return("")
      hist <- recentMoves[recentMoves$mlb_id == pid, ]
      if (nrow(hist) < 2) return("")
      levOrder <- c("DSL"=1,"ACL"=2,"A"=3,"A+"=4,"AA"=5,"AAA"=6,"MLB"=7)
      first <- levOrder[hist$level[1]]; last <- levOrder[hist$level[nrow(hist)]]
      if (is.na(first) || is.na(last) || first == last) return("")
      if (last > first) " ⬆" else " ⬇"
    }

    levelCard <- function(lvl) {
      sub <- gRoster[gRoster$level == lvl, ]
      if (nrow(sub) == 0) {
        return(tags$div(class = "card", style = "margin-bottom:10px; padding:8px; border:1px solid #ddd; border-radius:4px;",
                        tags$h4(lvl), tags$div(style="color:#888;","No roster.")))
      }
      sub <- sub %>% arrange(pos, player)
      rows <- lapply(seq_len(nrow(sub)), function(i) {
        pid <- sub$mlb_id[i]
        st <- gStats[gStats$mlb_id == pid, ]
        line <- if (nrow(st) > 0) {
          if (st$role[1] == "H" && !is.na(st$avg[1])) {
            sprintf(" — .%s / %d HR / %.3f OBP",
                    sub("^0\\.", "", sprintf("%.3f", st$avg[1])),
                    ifelse(is.na(st$hr[1]), 0, as.integer(st$hr[1])),
                    ifelse(is.na(st$obp[1]), 0, st$obp[1]))
          } else if (st$role[1] == "P" && !is.na(st$era[1])) {
            sprintf(" — %.2f ERA / %.1f K/9 / %.2f WHIP",
                    st$era[1],
                    ifelse(is.na(st$k9[1]), 0, st$k9[1]),
                    ifelse(is.na(st$whip[1]), 0, st$whip[1]))
          } else ""
        } else ""
        tags$div(style = "font-size:13px; padding:2px 0;",
                 tags$strong(sub$player[i]),
                 tags$span(style="color:#888;", paste0(" (", sub$pos[i], ")")),
                 tags$span(line),
                 tags$span(style="color:#27ae60;", moveBadge(pid)))
      })
      tags$div(class = "card", style = "margin-bottom:10px; padding:8px; border:1px solid #ddd; border-radius:4px;",
               tags$h4(paste0(lvl, " (", nrow(sub), ")")),
               do.call(tagList, rows))
    }

    do.call(tagList, lapply(levels, levelCard))
  })

  output$gDepthChart <- DT::renderDataTable({
    rv$refreshCount
    if (!is.data.frame(gDepth) || nrow(gDepth) == 0) {
      return(datatable(data.frame(Note = "FG depth chart not available"),
                       options = list(dom = 't'), selection = 'none', rownames = FALSE))
    }
    datatable(gDepth,
              options = list(pageLength = 30, dom = 't', autoWidth = FALSE),
              rownames = FALSE)
  })
  output$gHotTable <- DT::renderDataTable({
    rv$refreshCount
    req(input$gHotWindow, input$gHotRole, input$gHotLevel)
    win <- as.integer(input$gHotWindow)
    df <- gHot %>% filter(window_days == win)
    if (input$gHotRole != "A") df <- df %>% filter(role == input$gHotRole)
    if (input$gHotLevel != "All") df <- df %>% filter(level == input$gHotLevel)
    if (nrow(df) == 0) {
      return(datatable(data.frame(Note = "No hot/cold data for this filter"),
                       options = list(dom = 't'), selection = 'none', rownames = FALSE))
    }
    # Join in player name + a short window-line from gStats for context.
    df <- df %>%
      left_join(gRoster %>% select(mlb_id, player, pos, age), by = "mlb_id") %>%
      left_join(gStats  %>% select(mlb_id, avg, hr, obp, era, k9, whip),
                by = "mlb_id") %>%
      mutate(WindowLine = ifelse(role == "H",
                  sprintf(".%s / %d HR / %.3f OBP",
                          sub("^0\\.", "", sprintf("%.3f", ifelse(is.na(avg), 0, avg))),
                          ifelse(is.na(hr), 0, as.integer(hr)),
                          ifelse(is.na(obp), 0, obp)),
                  sprintf("%.2f ERA / %.1f K/9 / %.2f WHIP",
                          ifelse(is.na(era), 0, era),
                          ifelse(is.na(k9), 0, k9),
                          ifelse(is.na(whip), 0, whip)))) %>%
      arrange(desc(hotscore)) %>%
      select(Player = player, Lvl = level, Pos = pos, Age = age,
             `Window line` = WindowLine, HotScore = hotscore)

    datatable(df,
              options = list(pageLength = 25, autoWidth = FALSE),
              rownames = FALSE) %>%
      formatRound("HotScore", 2) %>%
      formatRound("Age", 0) %>%
      formatStyle("HotScore",
                  backgroundColor = styleInterval(c(-0.5, 0.5),
                                                  c("#f8d7da", "#ffffff", "#d4edda")))
  })
  output$gPlayerCard <- renderUI({
    rv$refreshCount
    nm <- input$gPlayerPick
    if (is.null(nm) || nm == "") {
      return(tags$div(style = "color:#888; padding:12px; font-style:italic;",
                      "Type a name in the search box to see player details."))
    }
    row <- gRoster %>% filter(player == nm)
    if (nrow(row) == 0) return(tags$div("Player not found in current roster."))
    row <- row[1, ]
    st <- gStats %>% filter(mlb_id == row$mlb_id)
    pros <- gProspects %>% filter(Name == nm)

    # Header
    headerUI <- tags$div(
      style = "padding:12px 16px; background:#0F223E; color:white; border-radius:6px 6px 0 0;",
      tags$div(style = "font-size:20px; font-weight:bold;", row$player),
      tags$div(style = "font-size:14px; margin-top:4px; color:#bdc3c7;",
               paste0(row$pos, "  |  ", row$level, "  |  Age ",
                      ifelse(is.na(row$age), "?", round(row$age, 1)),
                      if (nrow(pros) > 0 && "FV" %in% names(pros) && !is.na(pros$FV[1]))
                        paste0("  |  FV ", pros$FV[1]) else "",
                      if (nrow(pros) > 0 && "Top.100" %in% names(pros) && !is.na(pros$Top.100[1]))
                        paste0("  |  #", pros$Top.100[1], " overall") else ""))
    )

    # Hero line — current season slash or pitching summary
    heroUI <- if (nrow(st) > 0 && st$role[1] == "H") {
      tags$div(style = "padding:12px 16px; background:#f8f9fa; border:1px solid #ddd; border-top:none; font-size:18px;",
        tags$strong(sprintf(".%s / .%s / .%s",
              sub("^0\\.", "", sprintf("%.3f", ifelse(is.na(st$avg[1]), 0, st$avg[1]))),
              sub("^0\\.", "", sprintf("%.3f", ifelse(is.na(st$obp[1]), 0, st$obp[1]))),
              sub("^0\\.", "", sprintf("%.3f", ifelse(is.na(st$slg[1]), 0, st$slg[1]))))),
        tags$span(style = "margin-left:16px; color:#666; font-size:14px;",
                  sprintf("%d HR · %d RBI · %d R · %d SB",
                          ifelse(is.na(st$hr[1]), 0, as.integer(st$hr[1])),
                          ifelse(is.na(st$rbi[1]), 0, as.integer(st$rbi[1])),
                          ifelse(is.na(st$r[1]), 0, as.integer(st$r[1])),
                          ifelse(is.na(st$sb[1]), 0, as.integer(st$sb[1])))))
    } else if (nrow(st) > 0 && st$role[1] == "P") {
      tags$div(style = "padding:12px 16px; background:#f8f9fa; border:1px solid #ddd; border-top:none; font-size:18px;",
        tags$strong(sprintf("%.2f ERA · %.2f WHIP · %.1f K/9",
              ifelse(is.na(st$era[1]),  0, st$era[1]),
              ifelse(is.na(st$whip[1]), 0, st$whip[1]),
              ifelse(is.na(st$k9[1]),   0, st$k9[1]))),
        tags$span(style = "margin-left:16px; color:#666; font-size:14px;",
                  sprintf("%d W · %d SV · %d HLD · %d K",
                          ifelse(is.na(st$w[1]), 0, as.integer(st$w[1])),
                          ifelse(is.na(st$sv[1]), 0, as.integer(st$sv[1])),
                          ifelse(is.na(st$hld[1]), 0, as.integer(st$hld[1])),
                          ifelse(is.na(st$so[1]), 0, as.integer(st$so[1])))))
    } else {
      tags$div(style = "padding:12px 16px; background:#f8f9fa; border:1px solid #ddd; border-top:none; color:#888;",
              "No season stats yet for this player.")
    }

    # Trend plot — HotScore over snapshot_date (14d window) from gTrend
    trendDf <- gTrend %>%
      filter(mlb_id == row$mlb_id, window_days == 14) %>%
      mutate(snapshot_date = as.Date(snapshot_date)) %>%
      arrange(snapshot_date)
    trendUI <- if (nrow(trendDf) >= 5) {
      tags$div(style = "padding:12px 16px; border:1px solid #ddd; border-top:none;",
        tags$strong("HotScore trend (14-day window)"),
        plotly::plotlyOutput("gPlayerTrend", height = 220))
    } else {
      tags$div(style = "padding:12px 16px; border:1px solid #ddd; border-top:none; color:#888; font-size:13px;",
        "Not enough history yet for a trend chart (need 5+ daily snapshots).")
    }

    tags$div(style = "border-radius:6px; overflow:hidden;",
             headerUI, heroUI, trendUI)
  })

  # Trend plot output — paired with the renderUI above. plotly works when the
  # output is referenced inside a UI that's already on the page; renderUI
  # registers the placeholder div, and renderPlotly fills it.
  output$gPlayerTrend <- plotly::renderPlotly({
    nm <- input$gPlayerPick
    if (is.null(nm) || nm == "") return(NULL)
    row <- gRoster %>% filter(player == nm)
    if (nrow(row) == 0) return(NULL)
    trendDf <- gTrend %>%
      filter(mlb_id == row$mlb_id[1], window_days == 14) %>%
      mutate(snapshot_date = as.Date(snapshot_date)) %>%
      arrange(snapshot_date)
    if (nrow(trendDf) < 5) return(NULL)
    plotly::plot_ly(trendDf, x = ~snapshot_date, y = ~hotscore,
                    type = "scatter", mode = "lines+markers",
                    line = list(width = 3), marker = list(size = 8))
  })

  # Risers: players whose 14d HotScore has been > 0 for 3+ consecutive
  # snapshots, OR whose level moved up in the last 7 days.
  output$gRisers <- DT::renderDataTable({
    rv$refreshCount
    if (nrow(gTrend) == 0) {
      return(datatable(data.frame(Note = "Not enough history yet for risers."),
                       options = list(dom = 't'), selection = 'none', rownames = FALSE))
    }
    # Streaks on 14d HotScore
    t14 <- gTrend %>%
      filter(window_days == 14) %>%
      mutate(snapshot_date = as.Date(snapshot_date)) %>%
      arrange(mlb_id, snapshot_date)
    streaks <- t14 %>%
      group_by(mlb_id) %>%
      summarise(latest = tail(snapshot_date, 1),
                streak = {
                  s <- rev(hotscore > 0)
                  rl <- rle(s)
                  if (length(rl$values) == 0 || !isTRUE(rl$values[1])) 0L
                  else as.integer(rl$lengths[1])
                },
                .groups = "drop") %>%
      filter(streak >= 3, latest >= Sys.Date() - 1) %>%
      left_join(gRoster %>% select(mlb_id, player, pos, level), by = "mlb_id") %>%
      mutate(reason = paste0(streak, " consecutive positive HotScores")) %>%
      select(Player = player, Pos = pos, Lvl = level, Reason = reason)

    # Promotions in the last 7 days (level went up)
    promos <- tryCatch({
      conn3 <- dbConnect(RSQLite::SQLite(), "../code/DAFL.db")
      on.exit(dbDisconnect(conn3))
      hist <- dbGetQuery(conn3, "
        SELECT mlb_id, player, level, snapshot_date FROM GuardiansRoster
        WHERE snapshot_date >= date(?, '-7 days')
        ORDER BY mlb_id, snapshot_date",
        params = list(as.character(Sys.Date())))
      levOrder <- c("DSL"=1,"ACL"=2,"A"=3,"A+"=4,"AA"=5,"AAA"=6,"MLB"=7)
      hist %>%
        group_by(mlb_id) %>%
        summarise(player = tail(player, 1),
                  from = head(level, 1), to = tail(level, 1),
                  .groups = "drop") %>%
        filter(!is.na(levOrder[to]), !is.na(levOrder[from]),
               levOrder[to] > levOrder[from]) %>%
        mutate(Player = player, Pos = NA_character_, Lvl = to,
               Reason = paste0("Promoted ", from, " → ", to)) %>%
        select(Player, Pos, Lvl, Reason)
    }, error = function(e) data.frame(Player = character(), Pos = character(),
                                       Lvl = character(), Reason = character()))

    out <- bind_rows(streaks, promos) %>%
      distinct(Player, .keep_all = TRUE)
    if (nrow(out) == 0) {
      return(datatable(data.frame(Note = "No risers right now."),
                       options = list(dom = 't'), selection = 'none', rownames = FALSE))
    }
    datatable(out, options = list(pageLength = 15, dom = 'tip', autoWidth = FALSE),
              rownames = FALSE)
  })
  output$gTxnTable <- DT::renderDataTable({
    rv$refreshCount
    if (nrow(gTxn) == 0) {
      return(datatable(data.frame(Note = "No transactions in the last 14 days."),
                       options = list(dom = 't'), selection = 'none', rownames = FALSE))
    }
    df <- gTxn %>%
      select(Date = txn_date, Player = player, Type = type,
             From = from_team_id, To = to_team_id, Description = description)
    datatable(df,
              options = list(pageLength = 25, filter = 'top', autoWidth = FALSE),
              filter = 'top', rownames = FALSE)
  })
  output$gILTable <- DT::renderDataTable({
    rv$refreshCount
    if (nrow(gIL) == 0) {
      return(datatable(data.frame(Note = "No active IL placements."),
                       options = list(dom = 't'), selection = 'none', rownames = FALSE))
    }
    df <- gIL %>%
      select(Date = txn_date, Player = player, Type = type, Notes = description)
    datatable(df,
              options = list(pageLength = 15, autoWidth = FALSE),
              rownames = FALSE)
  })

  # Populate the player picker; re-runs whenever rv$refreshCount changes so
  # newly-added players appear after the Refresh button is pressed.
  observe({
    rv$refreshCount
    updateSelectizeInput(session, 'gPlayerPick',
                         choices = sort(unique(gRoster$player)),
                         server = TRUE)
  })
})
