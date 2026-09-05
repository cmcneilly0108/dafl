
# 1st Tool - pullTeam
library("plotly")
library("bslib")
library("DT")
library("shinyjs")


shinyUI(
  navbarPage(
    id = "mainNav",
#    theme = bs_theme(version = 4, bootswatch = "slate"),
    # "cerulean", "cosmo", "cyborg", "darkly", "flatly", "journal", "litera", "lumen", "lux", "materia", "minty",
    # "pulse", "sandstone", "simplex", "sketchy", "slate", "solar", "spacelab", "superhero", "united", "yeti"
    theme = dafl_theme(),
    title = dafl_brand("Evaluator"),
    windowTitle = "DAFL Evaluator",
    header = tagList(
      shinyjs::useShinyjs(),
      tags$div(style = "position:absolute; right:15px; top:8px; z-index:1000;",
        actionButton('settingsBtn', 'Settings',
                     class = 'btn-default btn-sm')
      )
    ),
    tags$head(
      tags$script(HTML("
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

      // Server can force the player-name menu closed (e.g. after navigating to Snapshot).
      Shiny.addCustomMessageHandler('removePlayerMenu', function(msg) {
        var m = document.getElementById('dafl-player-menu');
        if (m) m.remove();
        // Selecting a navbarMenu child via updateNavbarPage leaves the parent
        // ('Players') dropdown open — close any open navbar dropdown.
        document.querySelectorAll('.navbar li.dropdown.open, .navbar li.dropdown.show')
          .forEach(function(li) { li.classList.remove('open'); li.classList.remove('show'); });
      });

      // Popup menu for a clicked player name: Savant / FanGraphs / internal Snapshot.
      // Destinations arrive as data attributes from savantAnchor() (daflFunctions.r).
      function daflPlayerMenu(el) {
        var old = document.getElementById('dafl-player-menu');
        if (old) old.remove();
        var savant = el.getAttribute('data-savant');
        var fg = el.getAttribute('data-fg');
        var pid = el.getAttribute('data-pid');
        var menu = document.createElement('div');
        menu.id = 'dafl-player-menu';
        menu.className = 'dafl-menu';

        var title = document.createElement('div');
        title.className = 'dafl-menu-title';
        title.textContent = el.textContent;
        menu.appendChild(title);

        function addLink(text, href) {
          var a = document.createElement('a');
          a.className = 'dafl-menu-item';
          a.textContent = text;
          a.href = href;
          a.target = '_blank';
          menu.appendChild(a);
        }
        if (savant) addLink('Baseball Savant \\u2197', savant);
        if (fg) addLink('FanGraphs \\u2197', fg);

        var snap = document.createElement('a');
        snap.className = 'dafl-menu-item';
        snap.textContent = 'Player Snapshot';
        snap.href = '#';
        snap.onclick = function(e) {
          e.preventDefault();
          Shiny.setInputValue('gotoSnapshot', pid, {priority: 'event'});
          menu.remove();
          return false;
        };
        menu.appendChild(snap);

        document.body.appendChild(menu);
        var r = el.getBoundingClientRect();
        menu.style.top = (window.scrollY + r.bottom + 2) + 'px';
        menu.style.left = (window.scrollX + r.left) + 'px';

        // close after picking an external link (new tab opens first)
        menu.addEventListener('click', function() { setTimeout(function(){ menu.remove(); }, 0); });
        // close on outside click (deferred so this opening click doesn't close it)
        setTimeout(function() {
          document.addEventListener('click', function handler(ev) {
            if (!menu.contains(ev.target)) { menu.remove(); document.removeEventListener('click', handler); }
          });
        }, 0);
      }
    "))
    ),
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
    navbarMenu("Players",
      tabPanel("Player Snapshot",
               fluidRow(
                 column(7, DT::dataTableOutput("searchTable")),
                 column(5, uiOutput("playerSnapshot"))
               )),
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
      tabPanel("My Targets",
               verticalLayout(
                 h2("Targeted Players"),
                 checkboxInput('faTargets', 'Free Agents Only', value = TRUE),
                 DT::dataTableOutput("targetTable")
               ))
    ),
    navbarMenu("Free Agents",
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
      tabPanel("Streamers",
               sidebarLayout(
                 sidebarPanel(
                   checkboxInput('faStreamers', 'Free Agents Only', value = TRUE),
                   selectInput('streamersStat', 'Statistic',
                               choices = list(
                                 Hitters = c('HR','R','RBI','SB','AVG'),
                                 Pitchers = c('W','K','S','HD','ERA')
                               ),
                               selected = 'SB'),
                   width = 2
                 ),
                 mainPanel(
                   h2("Streamers — last 14 days"),
                   DT::dataTableOutput("streamersTable")
                 )
               )),
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
      tabPanel("Injured",
               verticalLayout(
                 DT::dataTableOutput("injOrig")
               ))
    ),
    navbarMenu("Analysis",
      tabPanel("Category Status by Team",
               sidebarLayout(
                 sidebarPanel(
                   selectizeInput('teamSelect', 'Team', choices = NULL),
                   width = 2
                 ),
                 mainPanel(
                   h2("Points by Category"),
                   DT::dataTableOutput("catSummary"),
                   br(),
                   h2("League Points In Play"),
                   DT::dataTableOutput("pointsInPlay"),
                   br(),
                   h2("Team Category Detail"),
                   DT::dataTableOutput("teamCatDetail")
                 )
               )
      ),
      tabPanel(
        "Surplus by Team",
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
      tabPanel("Trade Eval",
               sidebarLayout(
                 sidebarPanel(
                   selectizeInput('tradeTeamA', 'Team A', choices = NULL),
                   selectizeInput('tradeTeamB', 'Team B', choices = NULL),
                   width = 2
                 ),
                 mainPanel(
                   fluidRow(
                     column(6, h4(textOutput('tradeTeamAName')),
                            DT::dataTableOutput('tradeRosterA')),
                     column(6, h4(textOutput('tradeTeamBName')),
                            DT::dataTableOutput('tradeRosterB'))
                   ),
                   br(),
                   h3('Trade Summary'),
                   DT::dataTableOutput('tradeSummary')
                 )
               )
      )
    ),
    navbarMenu("Signals",
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
      tabPanel("LC Trends",
               mainPanel(
                 tabsetPanel(
                   type = 'tabs',
                   tabPanel('Standings', plotOutput("g1")),
                   tabPanel('Hitting', plotOutput("g2")),
                   tabPanel('Pitching', plotOutput("g3"))
                 )
               )),
      tabPanel("Research",
               sidebarLayout(fluid = FALSE,
                 sidebarPanel(
                   selectInput('researchSource', 'Recurring Column', choices = NULL),
                   actionButton('getLatestBtn', 'Get Latest',
                                class = 'btn-success',
                                style = 'width:100%; margin-bottom:12px;'),
                   tags$hr(style = 'margin:8px 0;'),
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
)
