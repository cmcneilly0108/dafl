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
  output$gPlayerCard  <- renderUI({ tags$div("Pick a player.") })
  output$gRisers      <- DT::renderDataTable({ datatable(data.frame()) })
  output$gTxnTable    <- DT::renderDataTable({ datatable(data.frame()) })
  output$gILTable     <- DT::renderDataTable({ datatable(data.frame()) })

  # Populate the player picker once the pulse globals are available.
  updateSelectizeInput(session, 'gPlayerPick',
                       choices = sort(unique(gRoster$player)),
                       server = TRUE)
})
