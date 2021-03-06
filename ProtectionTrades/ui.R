# 1st Tool - pullTeam

shinyUI(navbarPage("Offseason Trade Evaluator, v1.0",
  tabPanel("Overview",
    verticalLayout(
      dataTableOutput("totals")
    )
  ),
  tabPanel("by Team",
    sidebarLayout(fluid=FALSE,
      sidebarPanel(
        selectizeInput(
          'e1', 'Select Team', choices=NULL)
      ),
      mainPanel(
        h2(textOutput("tname")),
        tabsetPanel(type='tabs',          
          tabPanel('Hitting',
                   dataTableOutput("THitters")),
          tabPanel('Pitching',dataTableOutput("TPitchers")),
          
          tabPanel('Players',dataTableOutput("Players"))          
        )
      )
    )
  ),
  tabPanel("Best Pitchers",
           verticalLayout(
             sliderInput("pdp", "Min Value", min=0, max=20, value=7),
             sliderInput("pdfl", "Min DFL", min=0, max=100, value=10),
             dataTableOutput("bp")
           )
  ),
  tabPanel("Best Hitters",
           verticalLayout(
             sliderInput("pdh", "Min Value", min=0, max=40, value=20),
             sliderInput("hdfl", "Min DFL", min=0, max=100, value=10),
             dataTableOutput("bh")
           )
  )
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
#   tabPanel("Best Hitters",
#            sidebarLayout(fluid=FALSE,
#                          sidebarPanel(
#                            sliderInput("hsc", "hotscore", 
#                                        min=0, max=20, value=8),
#                            sliderInput("upd", "pDFL", 
#                                        min=0, max=20, value=5),                         
#                            sliderInput("sal", "Salary", 
#                                        min=0, max=100, value=10)                           
#                          ),
#                          mainPanel(
#                            tabsetPanel(type='tabs',
#                                        tabPanel('Hitters',dataTableOutput("undH")),          
#                                        tabPanel('Pitchers',dataTableOutput("undP"))
#                            )
#                          )
#            )
#   )
)
)
