# 1st Tool - pullTeam

shinyUI(
  navbarPage(
#    theme = bs_theme(version = 4, bootswatch = "slate"),
    # “cerulean”, “cosmo”, “cyborg”, “darkly”, “flatly”, “journal”, “litera”, “lumen”, “lux”, “materia”, “minty”, 
    # “pulse”, “sandstone”, “simplex”, “sketchy”, “slate”, “solar”, “spacelab”, “superhero”, “united”, “yeti”
    theme = bs_theme(bootswatch = "flatly"),
    "DAFL Evaluator, v1.1",
    tabPanel("Talent View",
             tabsetPanel(
               type = 'tabs',
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
                 choices = c('C', '1B', '2B', 'SS', '3B', 'OF', 'SP', 'MR', 'CL')
               ), width = 1),
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
      "LC Hitter Trends",
      selectizeInput(
        "choice",
        "Pick Player",
        choices = htrend$Player,
        selected = NULL,
        multiple = TRUE
      ),
      plotlyOutput("lcgraph")
      
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
            max = 20,
            value = 5
          ),
          width = 2
        ),
        mainPanel(DT::dataTableOutput("tprofile"))
      )
    ),
    tabPanel("Opportunities",
             mainPanel(
               h2("Injured or Sucky"),
               DT::dataTableOutput("problems")
             ))
    #   tabPanel("by Statistic",
    #            sidebarLayout(
    #              sidebarPanel(
    #                selectizeInput(
    #                  'e3', 'Select Statistic', choices=c('HR','RBI','R','SB','BA','W','K','ERA','HLD','SV'))
    #              ),
    #              mainPanel(
    #                h1('foo')
    #                #        tableOutput("Thitting")
    #              )
    #            )
    #   ),
    # tabPanel("Under Performers",
    #          sidebarLayout(fluid=FALSE,
    #                        sidebarPanel(
    #                          sliderInput("hsc", "hotscore",
    #                                      min=0, max=20, value=8),
    #                          sliderInput("upd", "pDFL",
    #                                      min=0, max=20, value=5),
    #                          sliderInput("sal", "Salary",
    #                                      min=0, max=100, value=10)
    #                        ),
    #                        mainPanel(
    #                          tabsetPanel(type='tabs',
    #                                      tabPanel('Hitters',dataTableOutput("undH")),
    #                                      tabPanel('Pitchers',dataTableOutput("undP"))
    #                          )
    #                        )
    #          )
    # )
  )
)
