# Move a player from FA to a Team, update both lists
# GUI to pick a player

setwd("../")
source("./draftSetup.r")
setwd("./draftTool")

teams <- unique(protected$Team) 
pstandings2 <- protected %>% group_by(Team) %>%
  summarize(NumProtected = length(Team),
            Spent = sum(Salary),
            TotalValue = sum(pDFL),
            MoneyEarned = TotalValue - Spent,
            VPPlayer = TotalValue/NumProtected,
            FullValue = TotalValue + (260-sum(Salary)),
            DollarValue = TotalValue/Spent) %>%
  arrange(-FullValue)

pstandings <- protected %>% group_by(Team) %>%
  summarize(PlayRem = 25 - length(Team),
            DolRem = 260 - sum(Salary),
            NumProtected = length(Team),
            Spent = sum(Salary),
            FullValue = sum(pDFL) + (260-sum(Salary))) %>%
  arrange(-FullValue)




shinyServer(function(input, output,session) {
  
  steam <- reactive({ filter(protected,Team==input$showTeam) %>% select(-X,-Team)
  })
  shitters <- reactive({ filter(PH,Team==input$showTeam) %>% select(-Team)
  })
  
  spitchers <- reactive({ filter(PP,Team==input$showTeam) %>% select(-Team)
  })
  
  avail <- reactive({ avbyPos(input$position)
  })
  
  output$teamSelector <- renderUI({
    selectInput("showTeam", "Choose Team:", as.list(teams)) })
  
  output$Standings <- renderGvis({ gvisTable(pstandings) })
  
  output$showTeam <- renderText({input$showTeam})
  output$TeamDetail <- renderGvis({ gvisTable(steam())})
  output$TeamHitters <- renderGvis({ gvisTable(shitters())})
  
  output$TeamPitchers <- renderGvis({ gvisTable(spitchers())})
  
  output$Avail <- renderGvis({ gvisTable(avail())})
  
  updateSelectizeInput(session, 'fah', choices = AvH$Player, server = TRUE)
  updateSelectInput(session, 'rteam', choices = as.list(teams))
  
  })
