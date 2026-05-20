library("shiny")
library("plotly")
library("bslib")
library("DT")

shinyUI(
  navbarPage(
    theme = bs_theme(bootswatch = "flatly"),
    "Cleveland Guardians Tracker",
    header = tagList(
      tags$div(style = "position:absolute; right:15px; top:8px; z-index:1000;",
        actionButton('gSettingsBtn', 'Settings', class = 'btn-default btn-sm')
      )
    ),
    tabPanel("Depth Chart",
      fluidRow(
        column(width = 8, h3("Org by Level"), uiOutput("gOrgTree")),
        column(width = 4, h3("MLB Depth Chart"), DT::dataTableOutput("gDepthChart"))
      )
    ),
    tabPanel("Hot / Cold",
      sidebarLayout(
        sidebarPanel(
          selectInput("gHotWindow", "Window (days)", choices = c(7, 14, 30), selected = 14),
          radioButtons("gHotRole", "Role", choices = c("Hitters" = "H", "Pitchers" = "P", "All" = "A"),
                       selected = "A", inline = TRUE),
          selectInput("gHotLevel", "Level", choices = c("All","MLB","AAA","AA","A+","A","ACL","DSL"),
                      selected = "All"),
          width = 3
        ),
        mainPanel(DT::dataTableOutput("gHotTable"), width = 9)
      )
    ),
    tabPanel("Player Detail",
      sidebarLayout(
        sidebarPanel(selectizeInput("gPlayerPick", "Search player",
                                    choices = NULL, options = list(placeholder = "type a name…")),
                     width = 3),
        mainPanel(uiOutput("gPlayerCard"), width = 9)
      )
    ),
    tabPanel("Risers & Transactions",
      h3("Risers"), DT::dataTableOutput("gRisers"),
      tags$hr(),
      h3("Recent Transactions (last 14 days)"), DT::dataTableOutput("gTxnTable"),
      tags$hr(),
      h3("Current IL"), DT::dataTableOutput("gILTable")
    )
  )
)
