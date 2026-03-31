library("bslib")
library("DT")
library("shinyjs")

shinyUI(navbarPage("DAFL Live Draft Tool v 2.0",
                   id = "mainNav",
                   theme = bs_theme(bootswatch = "flatly"),
    useShinyjs(),
                   header = tags$div(style = "position:absolute; right:15px; top:8px; z-index:1000; display:flex; align-items:center; gap:12px;",
                     uiOutput("draftProgress", inline = TRUE),
                     uiOutput("myTeamBadge", inline = TRUE),
                     actionButton('settingsBtn', 'Settings',
                                  class = 'btn-default btn-sm')
                   ),
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
                                     ),
                                     h4("Draft Market", style = "margin-top:15px;"),
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
                                     DT::dataTableOutput("draftStandings"),
                                     h3("Pre-Draft Summary"),
                                     uiOutput("preDraftSummary"),
                                     uiOutput("protListComparison")
                              )
                            )
                   ),
                   tabPanel("Player Snapshot",
                            fluidRow(
                              column(7, DT::dataTableOutput("searchTable")),
                              column(5,
                                     uiOutput("auctionReturnLink"),
                                     uiOutput("playerSnapshot"))
                            )
                   ),
                   tabPanel("Nominations",
                            sidebarLayout(fluid = FALSE,
                              sidebarPanel(
                                selectInput('nomTeam', 'My Team', choices = NULL),
                                uiOutput("nomSpendingPower"),
                                uiOutput("nomStrategyCard"),
                                tags$hr(),
                                uiOutput("nomAuctionReturnLink"),
                                h4("Competition Check"),
                                selectizeInput('compPlayer', 'Select Player',
                                               choices = NULL,
                                               options = list(placeholder = 'Type player name...')),
                                uiOutput("competitionReport"),
                                width = 3
                              ),
                              mainPanel(
                                h3("Positional Inflation"),
                                DT::dataTableOutput("posInflation"),
                                tags$div(style = "margin-top:8px; margin-bottom:16px; font-size:12px; color:#666; line-height:1.6;",
                                  tags$strong("Inflation"), " = avg $ above/below projected value per drafted player at position.",
                                  tags$br(),
                                  tags$span(style="color:#2ecc71;", "Negative = bargains"),
                                  " | ",
                                  tags$span(style="color:#f39c12;", "$0\u2013$5 moderate"),
                                  " | ",
                                  tags$span(style="color:#e74c3c;", "> $5 overpay"),
                                  tags$br(),
                                  tags$strong("Trend"), " = same calc for last 5 drafted at position."
                                ),
                                h3("Nomination Targets"),
                                DT::dataTableOutput("nomTargets")
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
                                tags$hr(),
                                tags$div(style = "font-size:12px; line-height:1.8;",
                                  tags$strong("Slot Colors (vs league avg)"),
                                  tags$div(style = "background:#b7e4c7; padding:2px 6px; margin-top:4px; border-radius:3px;", "Elite (120%+)"),
                                  tags$div(style = "background:#d4edda; padding:2px 6px; border-radius:3px;", "Above Avg (90%+)"),
                                  tags$div(style = "background:#fff3cd; padding:2px 6px; border-radius:3px;", "Below Avg (70%+)"),
                                  tags$div(style = "background:#f8d7da; padding:2px 6px; border-radius:3px;", "Weak (< 70%)"),
                                  tags$div(style = "margin-top:6px;",
                                    tags$strong("Value: "),
                                    tags$span(style = "color:#2ecc71;", "Positive"),
                                    " / ",
                                    tags$span(style = "color:#e74c3c;", "Negative")
                                  )
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
                   tabPanel("Positional Pressure",
                            sidebarLayout(fluid=FALSE,
                                          sidebarPanel(
                                            selectizeInput(
                                              'e4', 'Select Position', choices=NULL),
                                            uiOutput("posSummaryCard"),
                                            h4("Pressure", style = "margin-top:15px;"),
                                            DT::dataTableOutput("pressureTable"),
                                            width=3),
                                          mainPanel(
                                            h3(textOutput("posNeedHeader")),
                                            DT::dataTableOutput("posNeedTable")
                                          )
                            )
                   ),
                   tabPanel("Hitters",
                            sidebarLayout(fluid=FALSE,
                              sidebarPanel(
                                selectizeInput(
                                  'e2', 'Select Position', choices=NULL),
                                tags$div(style = 'margin-top:15px; font-size:12px; line-height:1.8;',
                                  tags$strong('Price Tiers'),
                                  tags$div(style = 'background:#e8f4fd; padding:2px 6px; margin-top:4px;', 'Elite $30+'),
                                  tags$div(style = 'background:#edf7ee; padding:2px 6px;', 'Solid $15+'),
                                  tags$div(style = 'background:#fef9e7; padding:2px 6px;', 'Value $5+'),
                                  tags$div(style = 'background:#f5f5f5; padding:2px 6px;', 'Dollar $1+')
                                )
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
                                            tags$div(style = 'margin-top:15px; font-size:12px; line-height:1.8;',
                                              tags$strong('Price Tiers'),
                                              tags$div(style = 'background:#e8f4fd; padding:2px 6px; margin-top:4px;', 'Elite $30+'),
                                              tags$div(style = 'background:#edf7ee; padding:2px 6px;', 'Solid $15+'),
                                              tags$div(style = 'background:#fef9e7; padding:2px 6px;', 'Value $5+'),
                                              tags$div(style = 'background:#f5f5f5; padding:2px 6px;', 'Dollar $1+')
                                            )
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
                   tabPanel("Leaderboards",
                            sidebarLayout(fluid = FALSE,
                              sidebarPanel(
                                dateInput('lbStartDate', 'Start Date',
                                          value = as.Date("2026-03-25"), format = "yyyy-mm-dd"),
                                dateInput('lbEndDate', 'End Date',
                                          value = Sys.Date(), format = "yyyy-mm-dd"),
                                actionButton('fetchLeaders', 'Fetch Leaderboards',
                                             class = 'btn-primary', style = 'width:100%;'),
                                uiOutput("blendStatus"),
                                width = 2
                              ),
                              mainPanel(
                                tabsetPanel(id = 'leaderTab', type = 'tabs',
                                  tabPanel('Hitters',
                                           DT::dataTableOutput("leaderH")),
                                  tabPanel('Pitchers',
                                           DT::dataTableOutput("leaderP"))
                                )
                              )
                            )
                   ),
                   tabPanel("Top Hitters",
                            verticalLayout(
                              DT::dataTableOutput("topHitters")
                            )
                   ),
                   tabPanel("Prospects",
                            mainPanel(
                              tabsetPanel(id = 'prospectTab', type='tabs',
                                          tabPanel('Hitters',
                                                   DT::dataTableOutput("prospectH")),
                                          tabPanel('Pitchers',
                                                   DT::dataTableOutput("prospectP"))
                              )
                            )
                   ),
                   tabPanel("Injured",
                            verticalLayout(
                              DT::dataTableOutput("injOrig")
                            )
                   ),
                   tabPanel("Research",
                            sidebarLayout(fluid = FALSE,
                              sidebarPanel(
                                radioButtons('researchMode', 'Input Mode',
                                             choices = c('Paste Article Text' = 'paste', 'Scrape URL' = 'url'),
                                             selected = 'paste', inline = TRUE),
                                conditionalPanel(
                                  condition = "input.researchMode == 'url'",
                                  textInput('researchUrl', 'Article URL',
                                            placeholder = 'https://www.fangraphs.com/...')
                                ),
                                conditionalPanel(
                                  condition = "input.researchMode == 'paste'",
                                  textAreaInput('researchText', 'Article Text',
                                                placeholder = 'Copy and paste article text here...',
                                                rows = 8)
                                ),
                                actionButton('analyzeBtn', 'Analyze Article',
                                             class = 'btn-primary',
                                             style = 'width:100%; margin-bottom:10px;'),
                                uiOutput('researchStatus'),
                                width = 3
                              ),
                              mainPanel(
                                tabsetPanel(id = 'researchTab', type = 'tabs',
                                  tabPanel('Hitters',
                                           DT::dataTableOutput('researchH')),
                                  tabPanel('Pitchers',
                                           DT::dataTableOutput('researchP'))
                                ),
                                uiOutput('researchUnmatched')
                              )
                            )
                   ),
                   tabPanel("My Targets",
                            verticalLayout(
                              DT::dataTableOutput("targetTable")
                            )
                   )
)
)
