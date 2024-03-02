# Just getting this started

shinyUI(navbarPage("Live Auction Tool, v0.1",
                   tabPanel("Overview",
                            verticalLayout(
                              dataTableOutput("pstandings")
                            )
                   ),
                   tabPanel("Auction Stats",
                            verticalLayout(
                              h2("Inflation Summary"),
                              dataTableOutput("protectSummary"),
                              h2("Protection by Position"),
                              dataTableOutput("ppp")
                            )
                   ),
                   tabPanel("Prospects",
                            mainPanel(
                              h2("Prospects"),
                              tabsetPanel(type='tabs',
                                          tabPanel('Hitters',
                                                   dataTableOutput("prospectH")),
                                          tabPanel('Pitchers',
                                                   dataTableOutput("prospectP"))
                              )
                            )
                   ),
                   tabPanel("Top Hitters",
                            verticalLayout(
                              dataTableOutput("topHitters")
                            )
                   ),
                   tabPanel("Injured",
                            verticalLayout(
                              dataTableOutput("injOrig")
                            )
                   )
)
)