# Just getting this started

#library("plotly")
library("bslib")
library("DT")


shinyUI(navbarPage("Live Auction Tool, v0.5",
                   theme = bs_theme(bootswatch = "flatly"),
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
                                            DT::dataTableOutput("tProtect"))
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