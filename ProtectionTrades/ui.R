# 1st Tool - pullTeam
library("bslib")
library("DT")
library("shinyjs")


shinyUI(navbarPage("Offseason Trade Evaluator, v2.0",
        theme = bs_theme(bootswatch = "flatly"),
        shinyjs::useShinyjs(),
        tabPanel("Overview",
                 verticalLayout(
                   fluidRow(
                     column(12,
                       actionButton("refreshData", "Refresh Data",
                                    icon = icon("refresh"),
                                    class = "btn-primary",
                                    style = "margin-bottom: 15px;")
                     )
                   ),
                   DT::dataTableOutput("totals")
                 )
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
                  tabPanel('AI Summary',
                    fluidRow(
                      column(12,
                        actionButton("generateSummary", "Generate AI Summary",
                                     icon = icon("robot"),
                                     class = "btn-info",
                                     style = "margin-bottom: 15px;"),
                        actionButton("refreshSummary", "Refresh Summary",
                                     icon = icon("refresh"),
                                     class = "btn-warning",
                                     style = "margin-bottom: 15px; margin-left: 10px;")
                      )
                    ),
                    wellPanel(
                      h4("Team Analysis"),
                      uiOutput("teamSummary")
                    )
                  ),
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
          ),
        tabPanel("Create Protection Lists",
                 sidebarLayout(
                   sidebarPanel(
                     selectizeInput('protTeam', 'Select Team', choices = NULL),
                     h4(textOutput("protCounter")),
                     actionButton("protSubmit", "Submit Protection List",
                                  icon = icon("save"),
                                  class = "btn-success",
                                  style = "margin-top: 15px;"),
                     hr(),
                     h5("Protection Summary"),
                     DT::dataTableOutput("protSummary"),
                     width = 3
                   ),
                   mainPanel(
                     DT::dataTableOutput("protTable"),
                     width = 9
                   )
                 )
        )
)
)