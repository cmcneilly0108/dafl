# 1st Tool - pullTeam

shinyUI(navbarPage("League Evaluator, v0.1",
  tabPanel("by Team",
    sidebarLayout(fluid=FALSE,
      sidebarPanel(
        selectizeInput(
          'e1', 'Select Team', choices=NULL)
      ),
      mainPanel(
        h2(textOutput("tname")),
        tabsetPanel(type='tabs',
#          tabPanel('Hitters',htmlOutput("TeamH")),
          tabPanel('Hitters',dataTableOutput("TeamH")),          
          tabPanel('Pitchers',dataTableOutput("TeamP"))
        )
      )
    )
  ),
  tabPanel("by Position",
           sidebarLayout(
             sidebarPanel(
               selectizeInput(
                 'e2', 'Select Position', choices=c('C','1B','2B','SS','3B','OF','SP','MR','CL')),
               sliderInput("pd", "pDFL", 
                           min=0, max=20, value=5)
             ),
             mainPanel(
               dataTableOutput("tprofile")
             )
           )
  ),
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
  tabPanel("Under Performers",
           sidebarLayout(fluid=FALSE,
                         sidebarPanel(
                           sliderInput("hsc", "hotscore", 
                                       min=0, max=20, value=8),
                           sliderInput("upd", "pDFL", 
                                       min=0, max=20, value=5),                         
                           sliderInput("sal", "Salary", 
                                       min=0, max=100, value=10)                           
                         ),
                         mainPanel(
                           tabsetPanel(type='tabs',
                                       tabPanel('Hitters',dataTableOutput("undH")),          
                                       tabPanel('Pitchers',dataTableOutput("undP"))
                           )
                         )
           )
  )
)
)
