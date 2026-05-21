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
      sidebarLayout(
        sidebarPanel(
          selectInput("gDepthLevel", "Level",
                      choices = c("MLB","AAA","AA","A+","A","ACL","DSL"),
                      selected = "MLB"),
          width = 2
        ),
        mainPanel(uiOutput("gDepthDiamond"), width = 10)
      )
    ),
    tabPanel("Hot / Cold",
      sidebarLayout(
        sidebarPanel(
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
    tabPanel("Prospects",
      tabsetPanel(
        type = "tabs",
        tabPanel("Hitters",  DT::dataTableOutput("gProspectsH")),
        tabPanel("Pitchers", DT::dataTableOutput("gProspectsP"))
      )
    ),
    tabPanel("Risers & Transactions",
      tabsetPanel(
        type = "tabs",
        tabPanel("Risers", DT::dataTableOutput("gRisers")),
        tabPanel("Transactions", DT::dataTableOutput("gTxnTable")),
        tabPanel("IL", DT::dataTableOutput("gILTable"))
      )
    )
  )
)
