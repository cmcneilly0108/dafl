library("bslib")
library("DT")

shinyUI(navbarPage("DAFL Live Draft Tool",
                   theme = bs_theme(bootswatch = "flatly"),
                   tabPanel("Draft",
                            fluidRow(
                              column(4,
                                     wellPanel(
                                       h3("Draft a Player"),
                                       selectizeInput('playerSearch', 'Search Player',
                                                      choices = NULL,
                                                      options = list(placeholder = 'Type player name...')),
                                       selectInput('draftTeam', 'Team', choices = NULL),
                                       numericInput('draftSalary', 'Salary', value = 1, min = 1, max = 260),
                                       actionButton('draftBtn', 'Draft Player',
                                                    class = 'btn-primary btn-block',
                                                    style = 'width:100%; margin-bottom:10px;'),
                                       actionButton('undoBtn', 'Undo Last Pick',
                                                    class = 'btn-warning btn-block',
                                                    style = 'width:100%;')
                                     )
                              ),
                              column(8,
                                     h3("Recent Picks"),
                                     DT::dataTableOutput("recentPicks"),
                                     h3("Live Standings"),
                                     DT::dataTableOutput("draftStandings")
                              )
                            )
                   ),
                   tabPanel("Overview",
                            verticalLayout(
                              DT::dataTableOutput("pstandings")
                            )
                   ),
                   tabPanel("Auction Stats",
                            splitLayout(
                              verticalLayout(
                                h2("Inflation Summary"),
                                DT::dataTableOutput("protectSummary")),
                              verticalLayout(
                                h2("Protection by Position"),
                                DT::dataTableOutput("ppp"))
                            )
                   ),
                   tabPanel("Protection Lists",
                            sidebarLayout(fluid=FALSE,
                                          sidebarPanel(
                                            selectizeInput(
                                              'e1', 'Select Team', choices=NULL)
                                            ,width=2),
                                          mainPanel(
                                            h2(textOutput("tname")),
                                            DT::dataTableOutput("tpSummary"),
                                            DT::dataTableOutput("Goals"),
                                            DT::dataTableOutput("tProtect"))
                                          )
                   ),
                   tabPanel("Protect by Pos",
                            sidebarLayout(fluid=FALSE,
                                          sidebarPanel(
                                            selectizeInput(
                                              'e4', 'Select Position', choices=NULL)
                                            ,width=2),
                                          mainPanel(
                                            h2(textOutput("allpos")),
                                            h2(textOutput("uniquePos")),
                                            DT::dataTableOutput("tNeed"),
                                            DT::dataTableOutput("posProtect")
                                          )
                            )
                   ),
                   tabPanel("Hitters",
                            sidebarLayout(fluid=FALSE,
                              sidebarPanel(
                                selectizeInput(
                                  'e2', 'Select Position', choices=NULL)
                                  ,width=2),
                                mainPanel(
                                  h2(textOutput("hpos")),
                                  DT::dataTableOutput("hpbpos"))
                                )
                   ),
                   tabPanel("Pitchers",
                            sidebarLayout(fluid=FALSE,
                                          sidebarPanel(
                                            selectizeInput(
                                              'e3', 'Select Role', choices=NULL)
                                            ,width=2),
                                          mainPanel(
                                            h2(textOutput("ppos")),
                                            DT::dataTableOutput("ppbpos"))
                            )
                   ),
                   tabPanel("Bullpen Depth Charts",
                            verticalLayout(fluid=FALSE,
                                           DT::dataTableOutput("rrcResults")
                            )
                   ),
                   tabPanel("Prospects",
                            mainPanel(
                              tabsetPanel(type='tabs',
                                          tabPanel('Hitters',
                                                   DT::dataTableOutput("prospectH")),
                                          tabPanel('Pitchers',
                                                   DT::dataTableOutput("prospectP"))
                              )
                            )
                   ),
                   tabPanel("Top Hitters",
                            verticalLayout(
                              DT::dataTableOutput("topHitters")
                            )
                   ),
                   tabPanel("Injured",
                            verticalLayout(
                              DT::dataTableOutput("injOrig")
                            )
                   )
)
)
