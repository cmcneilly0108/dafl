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
  output$gOrgTree     <- renderUI({ tags$div("Depth chart coming soon.") })
  output$gDepthChart  <- DT::renderDataTable({ datatable(data.frame()) })
  output$gHotTable    <- DT::renderDataTable({ datatable(data.frame()) })
  output$gPlayerCard  <- renderUI({ tags$div("Pick a player.") })
  output$gRisers      <- DT::renderDataTable({ datatable(data.frame()) })
  output$gTxnTable    <- DT::renderDataTable({ datatable(data.frame()) })
  output$gILTable     <- DT::renderDataTable({ datatable(data.frame()) })

  # Populate the player picker once the pulse globals are available.
  updateSelectizeInput(session, 'gPlayerPick',
                       choices = sort(unique(gRoster$player)),
                       server = TRUE)
})
