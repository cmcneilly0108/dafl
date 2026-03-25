
# 1st Tool - pullTeam
library("plotly")
library("bslib")
library("DT")


shinyUI(
  navbarPage(
#    theme = bs_theme(version = 4, bootswatch = "slate"),
    # "cerulean", "cosmo", "cyborg", "darkly", "flatly", "journal", "litera", "lumen", "lux", "materia", "minty",
    # "pulse", "sandstone", "simplex", "sketchy", "slate", "solar", "spacelab", "superhero", "united", "yeti"
    theme = bs_theme(bootswatch = "flatly"),
    "DAFL Evaluator, v3.0",
    header = tags$div(style = "position:absolute; right:15px; top:8px; z-index:1000;",
      actionButton('refreshBtn', 'Refresh Data',
                    class = 'btn-success btn-sm',
                    icon = icon('refresh'))
    ),
    tabPanel("Standings",
             tabsetPanel(
               type = 'tabs',
               tabPanel("Overall Standings",DT::dataTableOutput("StandFull")),
               tabPanel("Rest of Season Prediction",DT::dataTableOutput("RTot")),
               tabPanel("Starters Only",DT::dataTableOutput("RTotTop"))
             )
    ),
    tabPanel(
      "By Team",
      sidebarLayout(
        fluid = TRUE,
        sidebarPanel(selectizeInput('e1', 'Select Team', choices =
                                      NULL), width = 2),
        mainPanel(
          h2(textOutput("tname")),
          tabsetPanel(
            type = 'tabs',
            #          tabPanel('Hitters',htmlOutput("TeamH")),
            tabPanel('Hitters', DT::dataTableOutput("TeamH")),
            tabPanel('Pitchers', DT::dataTableOutput("TeamP"))
          )
        )
      )
    ),
    tabPanel("By Position",
             sidebarLayout(
               sidebarPanel(selectizeInput(
                 'e3',
                 'Select Position',
                 choices = c('Hitters','C', '1B', '2B', 'SS', '3B', 'OF', 'SP', 'MR', 'CL')
               ),
               checkboxInput('fa','Free Agents Only'),
               actionButton('targetPosBtn', 'Toggle Target',
                            class = 'btn-info btn-sm',
                            style = 'width:100%; margin-top:10px;'),
               value=TRUE,width = 1),
               mainPanel(DT::dataTableOutput("topPlayers"))
             )),
    tabPanel("Reliever Detail",
             verticalLayout(
               actionButton('targetRRBtn', 'Toggle Target',
                            class = 'btn-info btn-sm',
                            style = 'margin-bottom:10px;'),
               h2("Roster Resource"),
               DT::dataTableOutput("rrcResults")
             )
    ),
    tabPanel(
      "Player Trends",
      selectizeInput(
        "choice",
        "Pick Player",
#        choices = trending$Player,
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      plotlyOutput("lcgraph",height="800px")

    ),
    tabPanel("LC Trends",
             mainPanel(
               tabsetPanel(
                 type = 'tabs',
                 tabPanel('Standings', plotOutput("g1")),
                 tabPanel('Hitting', plotOutput("g2")),
                 tabPanel('Pitching', plotOutput("g3"))
               )
             )),
    tabPanel("Category Status",
             mainPanel(
               h2("Points by Category"),
               DT::dataTableOutput("catSummary")
             )
    ),
    tabPanel(
      "Positional Surplus",
      sidebarLayout(
        sidebarPanel(
          selectizeInput(
            'e2',
            'Select Position',
            choices = c('C', '1B', '2B', 'SS', '3B', 'OF', 'SP', 'MR', 'CL')
          ),
          sliderInput(
            "pd",
            "pDFL",
            min = 0,
            max = 30,
            value = 10
          ),
          width = 2
        ),
        mainPanel(DT::dataTableOutput("tprofile"))
      )
    ),
    tabPanel(
      "Prospects",
        mainPanel(
          tabsetPanel(
            type = 'tabs',
            tabPanel('Hitters', DT::dataTableOutput("ProHit")),
            tabPanel('Pitchers', DT::dataTableOutput("ProPit"))
          )
        )
    ),
    tabPanel("Dumpers",
             mainPanel(
               h2("Who Could Be Dumping"),
               DT::dataTableOutput("cTrades")
             )),
    tabPanel("Desperate",
             mainPanel(
               h2("Who Could Be Desperate"),
               DT::dataTableOutput("problems")
             )),
    tabPanel("Injured",
             verticalLayout(
               actionButton('targetInjBtn', 'Toggle Target',
                            class = 'btn-info btn-sm',
                            style = 'margin-bottom:10px;'),
               DT::dataTableOutput("injOrig")
             )),
    tabPanel("My Targets",
             verticalLayout(
               actionButton('removeTargetBtn', 'Remove Selected',
                            class = 'btn-warning btn-sm',
                            style = 'margin-bottom:10px;'),
               h2("Targeted Players"),
               DT::dataTableOutput("targetTable")
             ))
  )
)
