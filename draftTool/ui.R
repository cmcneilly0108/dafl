# page height
# submit button - make it work, move player, recalc DFL, standings, lists
# do same for pitchers - place side by side

shinyUI(navbarPage("DAFL Draft Day Tool, v0.2",
                   tabPanel("Teams",  
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("teamSelector")
                              ),
                            mainPanel(
                              tableOutput("Standings")
                            ))
                   ),
                   tabPanel("Garbage",  
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("position", "Choose Position:", c('C','1B','2B','SS','3B','OF','DH','SP','MR','CL'))
                              ),
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("League",tableOutput("Standings")),
                                            tabPanel("Team View",h3(textOutput("showTeam", container = span)),
                                                     h4("Hitters"),
                                                     tableOutput("TeamHitters"),
                                                     h4("Pitchers"),
                                                     tableOutput("TeamPitchers")),
                                            tabPanel("Available Players",h3(textOutput("position", container = span)),
                                                     tableOutput("Avail")),
                                            tabPanel("Move Player",selectizeInput('fah', 'Choose Hitter',choices = NULL,
                                                                                  options = list(maxOptions = 10)),
                                                     textInput("nsal", "Salary"),
                                                     selectInput('rteam', 'Choose Team',choi = NULL),
                                                     actionButton("apl","Assign Player"),
                                                     class="row-fluid",
                                                     h3('Ugly '),h3('Ugly ')
                                            )
                                )
                              )
                            )
                   )
)
)
