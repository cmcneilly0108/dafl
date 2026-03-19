library("bslib")
library("DT")

shinyUI(navbarPage("DAFL Live Draft Tool v 2.0",
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
                                                    style = 'width:100%;'),
                                       tags$hr(),
                                       actionButton('targetBtn', 'Toggle Target',
                                                    class = 'btn-info btn-block',
                                                    style = 'width:100%;')
                                     ),
                                     h4("Draft Market", style = "margin-top:15px;"),
                                     selectInput('myTeam', 'My Team', choices = NULL),
                                     uiOutput("spendingPower"),
                                     selectInput('marketPos', 'Filter by Position',
                                                 choices = c('All', 'C','1B','2B','SS','3B','OF','SP','MR','CL'),
                                                 selected = 'All'),
                                     uiOutput("inflationDisplay"),
                                     uiOutput("buyNowSignal"),
                                     plotOutput("buyNowGauge", height = "200px")
                              ),
                              column(8,
                                     h3("Recent Picks"),
                                     DT::dataTableOutput("recentPicks"),
                                     h3("Live Standings"),
                                     DT::dataTableOutput("draftStandings")
                              )
                            )
                   ),
                   tabPanel("Rosters",
                            sidebarLayout(fluid = FALSE,
                              sidebarPanel(
                                selectizeInput('rosterTeam', 'Select Team', choices = NULL),
                                uiOutput("rosterBudget"),
                                tabsetPanel(type = "pills",
                                  tabPanel("Balance", plotOutput("spiderChart", height = "280px")),
                                  tabPanel("Goals", DT::dataTableOutput("rosterGoals"))
                                ),
                                width = 3
                              ),
                              mainPanel(
                                fluidRow(
                                  column(6, h3(textOutput("rosterTeamTitle"))),
                                  column(6, uiOutput("budgetAllocation"),
                                         div(style = "text-align:right;",
                                             actionButton('clearBudget', 'Clear Budget',
                                                          class = 'btn-danger btn-sm')))
                                ),
                                h4("Hitters"),
                                DT::dataTableOutput("rosterH"),
                                h4("Pitchers"),
                                DT::dataTableOutput("rosterP")
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
                                  'e2', 'Select Position', choices=NULL),
                                actionButton('targetHBtn', 'Toggle Target',
                                             class = 'btn-info btn-sm',
                                             style = 'width:100%; margin-top:10px;')
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
                                              'e3', 'Select Role', choices=NULL),
                                            actionButton('targetPBtn', 'Toggle Target',
                                                         class = 'btn-info btn-sm',
                                                         style = 'width:100%; margin-top:10px;')
                                            ,width=2),
                                          mainPanel(
                                            h2(textOutput("ppos")),
                                            DT::dataTableOutput("ppbpos"))
                            )
                   ),
                   tabPanel("Bullpen Depth Charts",
                            verticalLayout(fluid=FALSE,
                                           actionButton('targetBPBtn', 'Toggle Target',
                                                        class = 'btn-info btn-sm',
                                                        style = 'margin-bottom:10px;'),
                                           DT::dataTableOutput("rrcResults")
                            )
                   ),
                   tabPanel("Prospects",
                            mainPanel(
                              actionButton('targetProspBtn', 'Toggle Target',
                                           class = 'btn-info btn-sm',
                                           style = 'margin-bottom:10px;'),
                              tabsetPanel(id = 'prospectTab', type='tabs',
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
                              actionButton('targetInjBtn', 'Toggle Target',
                                           class = 'btn-info btn-sm',
                                           style = 'margin-bottom:10px;'),
                              DT::dataTableOutput("injOrig")
                            )
                   ),
                   tabPanel("My Targets",
                            verticalLayout(
                              DT::dataTableOutput("targetTable")
                            )
                   ),
                   tabPanel("Leaderboards",
                            sidebarLayout(fluid = FALSE,
                              sidebarPanel(
                                dateInput('lbStartDate', 'Start Date',
                                          value = Sys.Date() - 30, format = "yyyy-mm-dd"),
                                dateInput('lbEndDate', 'End Date',
                                          value = Sys.Date(), format = "yyyy-mm-dd"),
                                actionButton('fetchLeaders', 'Fetch Leaderboards',
                                             class = 'btn-primary', style = 'width:100%;'),
                                tags$hr(),
                                radioButtons('valMode', 'Valuation Mode',
                                             choices = c('Projections Only' = 'proj',
                                                         'Blended' = 'blend'),
                                             selected = 'proj'),
                                uiOutput("blendStatus"),
                                width = 2
                              ),
                              mainPanel(
                                tabsetPanel(type = 'tabs',
                                  tabPanel('Hitters',
                                           DT::dataTableOutput("leaderH")),
                                  tabPanel('Pitchers',
                                           DT::dataTableOutput("leaderP"))
                                )
                              )
                            )
                   )
)
)
