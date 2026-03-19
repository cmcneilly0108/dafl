# Source daflFunctions.r for Claude API
source("../code/daflFunctions.r")
library(markdown)

# Load data from protectionList.r
loadData <- function() {
  oldwd <- getwd()
  setwd("../code/")
  source("./protectionList.r", local = TRUE)
  setwd(oldwd)

  # Return all the data frames we need
  list(
    totals = totals,
    rpreds = rpreds
  )
}

# Cache directory for team summaries
SUMMARY_CACHE_DIR <- "../data/team_summaries"
if (!dir.exists(SUMMARY_CACHE_DIR)) dir.create(SUMMARY_CACHE_DIR, recursive = TRUE)

# Helper function to get cache file path
getCacheFile <- function(team_name) {
  file.path(SUMMARY_CACHE_DIR, paste0(gsub(" ", "_", team_name), "_summary.txt"))
}

# Helper function to check if cache is valid (based on rpreds.csv modification time)
isCacheValid <- function(team_name) {
  cache_file <- getCacheFile(team_name)
  rpreds_file <- "../rpreds.csv"

  if (!file.exists(cache_file)) return(FALSE)
  if (!file.exists(rpreds_file)) return(TRUE)

  file.mtime(cache_file) >= file.mtime(rpreds_file)
}

# Load cached summary if valid
loadCachedSummary <- function(team_name) {
  cache_file <- getCacheFile(team_name)
  if (isCacheValid(team_name)) {
    return(paste(readLines(cache_file, warn = FALSE), collapse = "\n"))
  }
  NULL
}

# Save summary to cache
saveSummaryCache <- function(team_name, summary) {
  cache_file <- getCacheFile(team_name)
  writeLines(summary, cache_file)
}

# Generate team summary using Claude API
generateTeamSummary <- function(team_name, player_data, totals_data) {
  my_team <- "Liquor Crickets"

  # Get players with positive netValue for the selected team (strip HTML from Player names)
  team_players <- player_data %>%
    filter(Team == team_name, netValue > 0) %>%
    arrange(-netValue) %>%
    mutate(PlayerName = gsub("<[^>]+>", "", Player))  # Strip HTML tags

  # Get Liquor Crickets players with positive netValue
  crickets_players <- player_data %>%
    filter(Team == my_team, netValue > 0) %>%
    arrange(-netValue) %>%
    mutate(PlayerName = gsub("<[^>]+>", "", Player))

  # Count players with positive netValue for both teams (already filtered)
  positive_value_count <- nrow(team_players)
  crickets_positive_count <- nrow(crickets_players)

  # Calculate minimum netValue threshold for Crickets (12th best player)
  # Players below this threshold wouldn't make the protection list
  crickets_min_threshold <- if (crickets_positive_count >= 12) {
    round(crickets_players$netValue[12], 1)
  } else {
    0  # If fewer than 12 players, any positive value player is protectable
  }

  # Get team totals
  team_totals <- totals_data %>% filter(Team == team_name)
  team_rank <- which(totals_data$Team == team_name)
  crickets_totals <- totals_data %>% filter(Team == my_team)
  crickets_rank <- which(totals_data$Team == my_team)

  # Format player data as text
  player_text <- team_players %>%
    select(PlayerName, Pos, Age, Salary, pDFL, netValue, s1, s2, s3, s4) %>%
    capture.output(print(.)) %>%
    paste(collapse = "\n")

  crickets_text <- crickets_players %>%
    select(PlayerName, Pos, Age, Salary, pDFL, netValue, s1, s2, s3, s4) %>%
    capture.output(print(.)) %>%
    paste(collapse = "\n")

  # Determine trade strategy based on positive value player count
  if (positive_value_count > 12) {
    trade_strategy <- paste0(
      "This team has ", positive_value_count, " players with positive netValue but can only protect 12. ",
      "They should consider CONSOLIDATION TRADES - trading 2-for-1 or 3-for-2 to convert excess value into fewer, higher-value players."
    )
  } else if (positive_value_count < 12) {
    trade_strategy <- paste0(
      "This team only has ", positive_value_count, " players with positive netValue (fewer than the 12 they can protect). ",
      "They should consider EXPANSION TRADES - trading 1-for-2 to acquire more protectable assets, even if individually less valuable."
    )
  } else {
    trade_strategy <- "This team has exactly 12 players with positive netValue - a perfect fit for protection. Focus on improving quality or positional balance."
  }

  # Crickets trade strategy
  if (crickets_positive_count > 12) {
    crickets_strategy <- paste0(
      "Liquor Crickets has ", crickets_positive_count, " players with positive netValue - looking to CONSOLIDATE."
    )
  } else if (crickets_positive_count < 12) {
    crickets_strategy <- paste0(
      "Liquor Crickets has ", crickets_positive_count, " players with positive netValue - looking to EXPAND."
    )
  } else {
    crickets_strategy <- "Liquor Crickets has exactly 12 positive value players - looking to improve quality or balance."
  }

  # Build prompt
  prompt <- paste0(
    "Analyze this fantasy baseball team's roster for trade planning with Liquor Crickets.\n\n",
    "LEAGUE CONTEXT:\n",
    "- 13 team league with 25 player rosters (13 hitters, 12 pitchers)\n",
    "- Protection lists allow a maximum of 12 players\n",
    "- Ideal protection is balanced between hitters and pitchers (roughly 6-7 of each)\n",
    "- Positional scarcity matters: avoid protecting multiple players at the same position (except OF where you can start 3)\n",
    "- Hitting categories: HR, RBI, R, SB, AVG\n",
    "- Pitching categories: W, K, HLD, SV, ERA\n\n",
    "=== TEAM BEING ANALYZED ===\n",
    "Team: ", team_name, " (Ranked #", team_rank, ")\n",
    "Total Value: $", round(team_totals$TotalValue), " | Profit: $", round(team_totals$MoneyEarned), "\n",
    "TRADE STRATEGY: ", trade_strategy, "\n\n",
    "Protectable Players (positive netValue only; s1-s4 are HR/RBI/R/SB for hitters, W/SO/HLD/SV for pitchers):\n",
    player_text,
    "\n\n=== LIQUOR CRICKETS (MY TEAM) ===\n",
    "Ranked #", crickets_rank, " | Total Value: $", round(crickets_totals$TotalValue),
    " | Profit: $", round(crickets_totals$MoneyEarned), "\n",
    crickets_strategy, "\n\n",
    "Protectable Players (positive netValue only):\n",
    crickets_text,
    "\n\n=== ANALYSIS REQUEST ===\n",
    "Provide analysis using BULLETED LISTS (not paragraphs) covering:\n\n",
    "## ", team_name, " Strengths\n- (list positions/categories where they excel)\n\n",
    "## ", team_name, " Weaknesses\n- (list positions/categories they need help)\n\n",
    "## Their Trade Chips\n- (list players they might trade - positional redundancy or surplus value)\n\n",
    "## What They're Looking For\n- (list what they need, keeping their trade strategy in mind)\n\n",
    "## Top 5 Targets for Crickets\n",
    "List the 5 best players from ", team_name, " that Crickets should try to acquire:\n",
    "- Must have netValue >= $", crickets_min_threshold, " (Crickets' 12th player threshold)\n",
    "- Prioritize players at positions where ", team_name, " has SURPLUS (more willing to trade)\n",
    "- Consider what categories/positions would help Crickets\n",
    "Format: Player (Pos, $netValue) - why they might be available\n\n",
    "## Top 5 Crickets Trade Chips\n",
    "List the 5 best Crickets players to offer ", team_name, ":\n",
    "- Players at positions where Crickets have SURPLUS or redundancy\n",
    "- Players who fill GAPS in ", team_name, "'s roster\n",
    "- Consider what categories/positions ", team_name, " needs\n",
    "Format: Player (Pos, $netValue) - why they'd want this player\n\n",
    "Note: All dollar values (Salary, pDFL, netValue) are in actual dollars, not millions."
  )

  callClaudeAPI(prompt)
}

# Initial data load
data <- loadData()
totals <- data$totals
rpreds <- data$rpreds
teams <- sort(unique(as.character(totals$Team)))

pullPlayers <- function(tm, data) {
#  res <- filter(data,Team == tm,netValue > 1) %>% arrange(-netValue) %>% mutate(Rank=rank(-Value)) %>%
  res <- filter(data, Team == tm) %>% arrange(-netValue) %>% mutate(Rank=rank(-netValue)) %>%
    select(-Team,-Rank,Player:Expected.Return) %>%
    rename(Skew=pSkew)
}

aggHitters <- function(tm, data) {
  res <- filter(data, Team == tm, netValue > 1, !(Pos %in% c('SP','CL','MR'))) %>%
    arrange(-netValue) %>% mutate(Rank=rank(-netValue)) %>%
    select(-Team,Rank,Player:Expected.Return) %>% group_by(Pos) %>%
    summarize(Players = length(Pos),TSalary = sum(Salary),TValue = sum(Value))
}

aggPitchers <- function(tm, data) {
  res <- filter(data, Team == tm, netValue > 1, (Pos %in% c('SP','CL','MR'))) %>%
    arrange(-netValue) %>% mutate(Rank=rank(-netValue)) %>%
    select(-Team,Rank,Player:Expected.Return) %>% group_by(Pos) %>%
    summarize(Players = length(Pos),TSalary = sum(Salary),TValue = sum(Value))
}



shinyServer(function(input, output,session) {

  # Reactive values to hold data
  rv <- reactiveValues(
    totals = totals,
    rpreds = rpreds,
    teamSummary = NULL
  )

  # Refresh data when button clicked
  observeEvent(input$refreshData, {
    showNotification("Refreshing data...", type = "message", duration = 2)
    newData <- loadData()
    rv$totals <- newData$totals
    rv$rpreds <- newData$rpreds
    rv$teamSummary <- NULL  # Clear summary when data refreshes
    showNotification("Data refreshed!", type = "message", duration = 3)
  })

  # Load cached summary when team changes
  observeEvent(input$e1, {
    cached <- loadCachedSummary(input$e1)
    rv$teamSummary <- cached
  })

  # Generate summary when button clicked (checks cache first)
  observeEvent(input$generateSummary, {
    # Check cache first
    cached <- loadCachedSummary(input$e1)
    if (!is.null(cached)) {
      rv$teamSummary <- cached
      showNotification("Loaded from cache", type = "message", duration = 2)
      return()
    }

    showNotification("Generating AI summary... This may take a few seconds.", type = "message", duration = 5)
    summary <- generateTeamSummary(input$e1, rv$rpreds, rv$totals)
    rv$teamSummary <- summary
    saveSummaryCache(input$e1, summary)
    showNotification("Summary complete!", type = "message", duration = 2)
  })

  # Force refresh (ignores cache)
  observeEvent(input$refreshSummary, {
    showNotification("Regenerating AI summary... This may take a few seconds.", type = "message", duration = 5)
    summary <- generateTeamSummary(input$e1, rv$rpreds, rv$totals)
    rv$teamSummary <- summary
    saveSummaryCache(input$e1, summary)
    showNotification("Summary refreshed!", type = "message", duration = 2)
  })

  # Render team summary output
  output$teamSummary <- renderUI({
    if (is.null(rv$teamSummary)) {
      return(HTML("<p><em>Click 'Generate AI Summary' to analyze this team's roster.</em></p>"))
    }
    HTML(markdownToHTML(text = rv$teamSummary, fragment.only = TRUE))
  })

  #output$totals <- renderDataTable({ totals })
  output$totals <- DT::renderDataTable({
    datatable(rv$totals, options = list(pageLength = 20, autoWidth = FALSE, paging = FALSE, searching = FALSE, info = FALSE)) %>%
      formatCurrency(c('TotalValue', 'MoneyEarned','VPPlayer','PostDraftEst')) %>%
      formatRound(c('ValueRatio','zScore'), 2)
  })
  
  
  
  updateSelectizeInput(session, 'e1', choices = teams, selected = 'Liquor Crickets')
  updateSelectizeInput(session, 'protTeam', choices = teams, selected = 'Liquor Crickets')
  output$tname <- renderText({ input$e1 })

  # --- Create Protection Lists tab ---

  # Build the protection list file path
  protFilePath <- paste0("../", cyear, "ProtectionLists.csv")

  # Reactive: players for selected team (for DT display)
  protPlayers <- reactive({
    req(input$protTeam)
    rv$rpreds %>%
      filter(Team == input$protTeam) %>%
      mutate(PlayerName = gsub("<[^>]+>", "", Player)) %>%
      select(playerid, PlayerName, Pos, Age, Salary, Contract, pDFL, netValue) %>%
      arrange(-netValue)
  })

  # Reactive: pre-selected row indices from existing CSV
  protPreSelected <- reactive({
    req(input$protTeam)
    if (!file.exists(protFilePath)) return(integer(0))
    existing <- read.csv(protFilePath, stringsAsFactors = FALSE)
    teamExisting <- existing %>% filter(Team == input$protTeam)
    if (nrow(teamExisting) == 0) return(integer(0))
    players <- protPlayers()
    which(players$playerid %in% teamExisting$playerid)
  })

  output$protTable <- DT::renderDataTable({
    players <- protPlayers()
    displayDf <- players %>% select(-playerid)
    datatable(displayDf,
              selection = list(mode = 'multiple', selected = protPreSelected()),
              options = list(pageLength = 30, paging = FALSE, searching = FALSE, info = FALSE)) %>%
      formatCurrency(c('pDFL', 'netValue')) %>%
      formatRound('Age', 0)
  })

  output$protCounter <- renderText({
    sel <- input$protTable_rows_selected
    n <- length(sel)
    paste0(n, " / 12 selected")
  })

  observe({
    sel <- input$protTable_rows_selected
    if (length(sel) > 12 || length(sel) == 0) {
      shinyjs::disable("protSubmit")
    } else {
      shinyjs::enable("protSubmit")
    }
  })

  observeEvent(input$protSubmit, {
    sel <- input$protTable_rows_selected
    if (length(sel) > 12) {
      showNotification("Cannot protect more than 12 players.", type = "error")
      return()
    }
    if (length(sel) == 0) {
      showNotification("No players selected.", type = "warning")
      return()
    }

    # Get selected playerids
    players <- protPlayers()
    selectedIds <- players$playerid[sel]

    # Read full roster to get output columns matching Rosters.csv schema
    rosterFile <- paste0("../", cyear, "Rosters.csv")
    rosters <- read.csv(rosterFile, stringsAsFactors = FALSE)
    selectedRoster <- rosters %>% filter(playerid %in% selectedIds)

    # Safety check: ensure join actually matched
    if (nrow(selectedRoster) == 0) {
      showNotification("Error: could not match selected players to roster file.", type = "error")
      return()
    }

    # Read existing protection list (or create empty)
    if (file.exists(protFilePath)) {
      existingProt <- read.csv(protFilePath, stringsAsFactors = FALSE)
      # Remove this team's old entries
      existingProt <- existingProt %>% filter(Team != input$protTeam)
    } else {
      existingProt <- rosters[0, ]  # Empty df with same columns
    }

    # Append new selections and write
    updatedProt <- rbind(existingProt, selectedRoster)
    write.csv(updatedProt, protFilePath)

    showNotification(
      paste0("Saved ", length(sel), " players for ", input$protTeam, "!"),
      type = "message", duration = 4
    )
  })

  dtPlayers <- reactive({df <- datatable(pullPlayers(input$e1, rv$rpreds),options = list(pageLength = 20,autoWidth = FALSE, paging = FALSE, searching = FALSE, info = FALSE), escape = FALSE) %>%
    formatRound(c('Age','pADP','rankDiff','s1','s2','s3','s4'),0) %>%
    formatRound(c('valueRatio'),3) %>%
    formatRound(c('Skew'),2) %>%
    formatCurrency(c('pDFL','Value','netValue'))})
  output$Players <- DT::renderDataTable({ dtPlayers() })


  dtTHitters <- reactive({df <- datatable(aggHitters(input$e1, rv$rpreds),options = list(pageLength = 20,autoWidth = FALSE, paging = FALSE, searching = FALSE, info = FALSE)) %>%
    formatCurrency(c('TValue'))})
  output$THitters <- DT::renderDataTable({ dtTHitters() })

  dtTPitchers <- reactive({df <- datatable(aggPitchers(input$e1, rv$rpreds),options = list(pageLength = 20,autoWidth = FALSE, paging = FALSE, searching = FALSE, info = FALSE)) %>%
    formatCurrency(c('TValue'))})
  output$TPitchers <- DT::renderDataTable({ dtTPitchers() })
  
  
  
  bh <- reactive({ as.data.frame(rv$rpreds) %>% filter(pADP > input$hadp, netValue>input$netVh,
                                                    pDFL>input$hdfl,Pos!='SP',Pos!='CL') %>%
                     arrange(-netValue) %>% select(Player,Team,Pos:netValue) })
  bp <- reactive({ as.data.frame(rv$rpreds) %>% filter((Pos=='SP' | Pos=='CL'),
                                                    pADP > input$padp,netValue>input$netVp,
                                                    pDFL>input$pdfl) %>%
                     arrange(-netValue) %>% select(Player,Team,Pos:netValue) })
  
  
  
  dtbp <- reactive({df <- datatable(bp(),options = list(pageLength = 20,autoWidth = FALSE), escape = FALSE) %>%
    formatRound(c('Age','pADP','rankDiff','s1','s2','s3','s4'),0) %>%
    formatCurrency(c('pDFL','Value','netValue'))})
  output$bp <- DT::renderDataTable({ dtbp() })

  #output$bh <- renderDataTable({ bh() })
  dtbh <- reactive({df <- datatable(bh(),options = list(pageLength = 20,autoWidth = FALSE), escape = FALSE) %>%
    formatRound(c('Age','pADP','rankDiff','s1','s2','s3','s4'),0) %>%
    formatCurrency(c('pDFL','Value','netValue'))})
  output$bh <- DT::renderDataTable({ dtbh() })
  
})
