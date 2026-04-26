
# 1st Tool - pullTeam
library("plotly")
library("bslib")
library("DT")
library("shinyjs")


shinyUI(
  navbarPage(
#    theme = bs_theme(version = 4, bootswatch = "slate"),
    # "cerulean", "cosmo", "cyborg", "darkly", "flatly", "journal", "litera", "lumen", "lux", "materia", "minty",
    # "pulse", "sandstone", "simplex", "sketchy", "slate", "solar", "spacelab", "superhero", "united", "yeti"
    theme = bs_theme(bootswatch = "flatly"),
    "DAFL Evaluator, v3.0",
    header = tagList(
      shinyjs::useShinyjs(),
      tags$div(style = "position:absolute; right:15px; top:8px; z-index:1000; display:flex; gap:8px;",
        actionButton('settingsBtn', 'Settings',
                     class = 'btn-default btn-sm'),
        actionButton('refreshBtn', 'Refresh Data',
                      class = 'btn-success btn-sm',
                      icon = icon('refresh'))
      )
    ),
    tags$head(tags$script(HTML("
      Shiny.addCustomMessageHandler('toggleStar', function(msg) {
        var els = document.querySelectorAll('[id=\"tgt-' + msg.pid + '\"]');
        els.forEach(function(el) {
          if (msg.isTarget) {
            el.innerHTML = '\\u2605';
            el.style.color = '#f1c40f';
          } else {
            el.innerHTML = '\\u2606';
            el.style.color = '#ccc';
          }
        });
      });
    "))),
    tabPanel("Standings",
             tabsetPanel(
               type = 'tabs',
               tabPanel("Overall Standings",DT::dataTableOutput("StandFull")),
               tabPanel("Rest of Season Prediction",DT::dataTableOutput("RTot")),
               tabPanel("Starters Only",DT::dataTableOutput("RTotTop"))
             )
    ),
    tabPanel(
      "By Team",
      sidebarLayout(
        fluid = TRUE,
        sidebarPanel(selectizeInput('e1', 'Select Team', choices =
                                      NULL), width = 2),
        mainPanel(
          h2(textOutput("tname")),
          tabsetPanel(
            type = 'tabs',
            #          tabPanel('Hitters',htmlOutput("TeamH")),
            tabPanel('Hitters', DT::dataTableOutput("TeamH")),
            tabPanel('Pitchers', DT::dataTableOutput("TeamP"))
          )
        )
      )
    ),
    tabPanel("By Position",
             sidebarLayout(
               sidebarPanel(selectizeInput(
                 'e3',
                 'Select Position',
                 choices = c('Hitters','C', '1B', '2B', 'SS', '3B', 'OF', 'SP', 'MR', 'CL')
               ),
               checkboxInput('fa','Free Agents Only'),
               value=TRUE,width = 1),
               mainPanel(DT::dataTableOutput("topPlayers"))
             )),
    tabPanel("Reliever Detail",
             verticalLayout(
               h2("Roster Resource"),
               DT::dataTableOutput("rrcResults")
             )
    ),
    tabPanel(
      "Player Trends",
      selectizeInput(
        "choice",
        "Pick Player",
#        choices = trending$Player,
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      plotlyOutput("lcgraph",height="800px")

    ),
    tabPanel("LC Trends",
             mainPanel(
               tabsetPanel(
                 type = 'tabs',
                 tabPanel('Standings', plotOutput("g1")),
                 tabPanel('Hitting', plotOutput("g2")),
                 tabPanel('Pitching', plotOutput("g3"))
               )
             )),
    tabPanel("Category Status",
             mainPanel(
               h2("Points by Category"),
               DT::dataTableOutput("catSummary")
             )
    ),
    tabPanel(
      "Surplus",
      tabsetPanel(
        type = 'tabs',
        tabPanel(
          "Positional",
          sidebarLayout(
            sidebarPanel(
              selectizeInput(
                'e2',
                'Select Position',
                choices = c('C', '1B', '2B', 'SS', '3B', 'OF', 'SP', 'MR', 'CL')
              ),
              sliderInput(
                "pd",
                "pDFL",
                min = 0,
                max = 30,
                value = 10
              ),
              width = 2
            ),
            mainPanel(DT::dataTableOutput("tprofile"))
          )
        ),
        tabPanel(
          "Statistical",
          mainPanel(
            h2("Team Tiers by Category"),
            tags$div(style = "font-size:13px; color:#666; margin-bottom:8px;",
                     "High = ranks 1–4, Medium = ranks 5–9, Low = ranks 10–13. ERA ranked low-to-high."),
            DT::dataTableOutput("statSurplus")
          )
        )
      )
    ),
    tabPanel(
      "Prospects",
        sidebarLayout(
          sidebarPanel(
            checkboxInput('faProspects','Free Agents Only'),
            width = 2
          ),
          mainPanel(
            tabsetPanel(
              type = 'tabs',
              tabPanel('Hitters', DT::dataTableOutput("ProHit")),
              tabPanel('Pitchers', DT::dataTableOutput("ProPit"))
            )
          )
        )
    ),
    tabPanel("Dumpers",
             mainPanel(
               h2("Who Could Be Dumping"),
               DT::dataTableOutput("cTrades")
             )),
    tabPanel("Desperate",
             mainPanel(
               h2("Who Could Be Desperate"),
               DT::dataTableOutput("problems")
             )),
    tabPanel("Injured",
             verticalLayout(
               DT::dataTableOutput("injOrig")
             )),
    tabPanel("My Targets",
             verticalLayout(
               h2("Targeted Players"),
               DT::dataTableOutput("targetTable")
             )),
    tabPanel("Player Snapshot",
             fluidRow(
               column(7, DT::dataTableOutput("searchTable")),
               column(5, uiOutput("playerSnapshot"))
             )),
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
             ))
  )
)
