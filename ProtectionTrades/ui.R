# 1st Tool - pullTeam
library("bslib")
library("DT")


shinyUI(navbarPage("Offseason Trade Evaluator, v1.13",
        theme = bs_theme(bootswatch = "flatly"),
        tabPanel("Overview",
                 verticalLayout(DT::dataTableOutput("totals"))
        ),
        tabPanel("by Team",
            sidebarLayout(fluid=FALSE,
              sidebarPanel(
                selectizeInput(
                  'e1', 'Select Team', choices=NULL)
                ,width=2),
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
                     sliderInput("netVp", "Min Net Value", min=0, max=20, value=10),
                     sliderInput("padp", "Min ADP", min=0, max=200, value=0, step=10),
                     sliderInput("pdfl", "Min DFL", min=0, max=30, value=10),
                     dataTableOutput("bp")
                   )
          ),
          tabPanel("Best Hitters",
                   verticalLayout(
                     sliderInput("netVh", "Min Net Value", min=0, max=40, value=10),
                     sliderInput("hadp", "Min ADP", min=0, max=200, value=0, step=10),
                     sliderInput("hdfl", "Min DFL", min=0, max=30, value=10),
                     dataTableOutput("bh")
                   )
          )
)
)