
# 1st Tool - pullTeam
library("plotly")
library("bslib")
library("DT")


shinyUI(
  navbarPage(
#    theme = bs_theme(version = 4, bootswatch = "slate"),
    # “cerulean”, “cosmo”, “cyborg”, “darkly”, “flatly”, “journal”, “litera”, “lumen”, “lux”, “materia”, “minty”, 
    # “pulse”, “sandstone”, “simplex”, “sketchy”, “slate”, “solar”, “spacelab”, “superhero”, “united”, “yeti”
    theme = bs_theme(bootswatch = "flatly"),
    "DAFL Evaluator, v2.0",
    tabPanel("Talent View",
             tabsetPanel(
               type = 'tabs',
               tabPanel("Overall Standings",DT::dataTableOutput("StandFull")),
               tabPanel("Rest of Season Prediction",DT::dataTableOutput("RTot")),
               tabPanel("Starters Only",DT::dataTableOutput("RTotTop"))
             )
    ),
    tabPanel(
      "Hotness by Team",
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
    tabPanel("Ranked Players",
             sidebarLayout(
               sidebarPanel(selectizeInput(
                 'e3',
                 'Select Position',
                 choices = c('Hitters','C', '1B', '2B', 'SS', '3B', 'OF', 'SP', 'MR', 'CL')
               ),
               checkboxInput('fa','Free Agents Only'),value=TRUE,width = 1),
               mainPanel(DT::dataTableOutput("topPlayers"))
             )),
    tabPanel("Closer Detail",
             mainPanel(
               h2("Roster Resource"),
               DT::dataTableOutput("rrcResults")
             )
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
    tabPanel(
      "Player Trends",
      selectizeInput(
        "choice",
        "Pick Player",
        choices = trending$Player,
        selected = NULL,
        multiple = TRUE
      ),
      plotlyOutput("lcgraph",height="800px")
      
    ),
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
               DT::dataTableOutput("injOrig")
             ))
  )
)
