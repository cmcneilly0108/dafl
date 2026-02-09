# LiveDraftTool server.R — reactive draft tool

setwd("../code/")
source("./draftGuide.r")
setwd("../LiveDraftTool")

# --- Constants ---
teams <- sort(unique(pstandings$Team))
hpos <- list('C','1B','2B','SS','3B','OF')
ppos <- list('SP','MR','CL')
allpos <- c(hpos,list('SP','RP'))
cap <- 260
nteams <- 13
nhitters <- 13
npitchers <- 12

# --- Roster file initialization ---
rosterFile <- str_c("../", cyear, "DraftRosters.csv")

if (!file.exists(rosterFile)) {
  # Seed from protection list
  initRoster <- read.csv("../2026fakeprotected.csv", stringsAsFactors = FALSE)
  initRoster$playerid <- as.character(initRoster$playerid)
  # Remove row-number column if present
  if ("X" %in% names(initRoster)) initRoster$X <- NULL

  # Look up MLB team from full player pools
  hLookup <- AllH_full %>% select(playerid, MLB) %>% distinct()
  pLookup <- AllP_full %>% select(playerid, MLB) %>% distinct()
  mlbLookup <- bind_rows(hLookup, pLookup) %>% distinct(playerid, .keep_all = TRUE)
  initRoster <- left_join(initRoster, mlbLookup, by = "playerid")
  initRoster$MLB <- replace_na(initRoster$MLB, "")

  initRoster <- initRoster %>% select(Player, Pos, Team, Salary, Contract, MLB, playerid, orank)
  write.csv(initRoster, rosterFile, row.names = FALSE)
}


shinyServer(function(input, output, session) {

  # --- Reactive state ---
  rv <- reactiveValues(
    roster = {
      df <- read.csv(rosterFile, stringsAsFactors = FALSE)
      df$playerid <- as.character(df$playerid)
      df
    },
    draftLog = list(),
    pendingPick = NULL
  )

  # --- Helper: split roster into H/P ---
  isPitcherPos <- function(pos) pos %in% c('P','SP','MR','CL','RP')

  # --- Derived reactive: roster joined with projections ---
  rhitters_r <- reactive({
    roster <- rv$roster
    rH <- filter(roster, !isPitcherPos(Pos) | is.na(Pos))
    rH$playerid <- as.character(rH$playerid)
    res <- left_join(rH, AllH_full, by = c('playerid'), copy = FALSE)
    # Resolve .x/.y columns from join
    res <- res %>%
      mutate(Pos = coalesce(Pos.y, Pos.x),
             Player = coalesce(Player.y, Player.x)) %>%
      select(-any_of(c('Pos.x','Pos.y','Player.x','Player.y')))
    res$Value <- replace_na(res$pDFL, 0) - res$Salary
    res$pDFL <- replace_na(res$pDFL, 0)
    res
  })

  rpitchers_r <- reactive({
    roster <- rv$roster
    rP <- filter(roster, isPitcherPos(Pos))
    rP$playerid <- as.character(rP$playerid)
    res <- left_join(rP, AllP_full, by = c('playerid'), copy = FALSE)
    res <- res %>%
      mutate(Pos = coalesce(Pos.y, Pos.x),
             Player = coalesce(Player.y, Player.x)) %>%
      select(-any_of(c('Pos.x','Pos.y','Player.x','Player.y')))
    res$Value <- replace_na(res$pDFL, 0) - res$Salary
    res$pDFL <- replace_na(res$pDFL, 0)
    res
  })

  # --- protClean: combined roster with projections ---
  protClean_r <- reactive({
    rh <- rhitters_r()
    rp <- rpitchers_r()
    # Use common columns
    cols <- c('Team','Player','Contract','Salary','pDFL','pADP','pSkew','rankDiff','Age','Pos','playerid')
    hcols <- intersect(cols, names(rh))
    pcols <- intersect(cols, names(rp))
    pc <- bind_rows(
      rh %>% select(any_of(cols)),
      rp %>% select(any_of(cols))
    )
    pc$pDFL <- replace_na(pc$pDFL, 0)
    pc
  })

  # --- pstandings ---
  pstandings_r <- reactive({
    pc <- protClean_r()
    ps <- pc %>% group_by(Team) %>%
      summarize(nPlayers = n(),
                totalSpent = sum(Salary),
                TotalValue = sum(pDFL),
                .groups = 'drop') %>%
      mutate(Needed = 25 - nPlayers,
             CashLeft = cap - totalSpent,
             Earned = TotalValue - totalSpent,
             VPPlayer = TotalValue / nPlayers,
             DPP = CashLeft / Needed,
             FullValue = TotalValue + auctionROI * CashLeft,
             ValueRatio = TotalValue / totalSpent) %>%
      select(Team, Needed, CashLeft, TotalValue, Earned, VPPlayer, DPP, FullValue, ValueRatio) %>%
      arrange(-FullValue)
    ps$zScore <- as.numeric(scale(ps$FullValue))
    ps
  })

  # --- currentSummary ---
  currentSummary_r <- reactive({
    rh <- rhitters_r()
    rp <- rpitchers_r()
    htots <- rh %>% group_by(Team) %>%
      summarise(needed = nhitters - n(), salleft = (cap * (1 - hpratio)) - sum(Salary), .groups = 'drop')
    htots$group <- 'hitting'
    ptots <- rp %>% group_by(Team) %>%
      summarise(needed = npitchers - n(), salleft = (cap * hpratio) - sum(Salary), .groups = 'drop')
    ptots$group <- 'pitching'
    bind_rows(htots, ptots) %>% arrange(group, -salleft) %>% select(Team, group, needed, salleft)
  })

  # --- protectSummary ---
  protectSummary_r <- reactive({
    rh <- rhitters_r()
    rp <- rpitchers_r()
    hitterTotal <- nteams * nhitters
    pitcherTotal <- nteams * npitchers
    hitterTaken <- nrow(rh)
    pitcherTaken <- nrow(rp)
    hitterSpent <- sum(rh$Salary)
    pitcherSpent <- sum(rp$Salary)
    hitterMoneyTotal <- nteams * (cap * (1 - hpratio))
    pitcherMoneyTotal <- nteams * (cap * hpratio)
    rh$pDFL <- replace_na(rh$pDFL, 0)
    hpdfl <- sum(rh$pDFL)
    ppdfl <- sum(rp$pDFL)
    hpr <- hitterTaken / hitterTotal
    hsr <- hitterSpent / hitterMoneyTotal
    ppr <- pitcherTaken / pitcherTotal
    psr <- pitcherSpent / pitcherMoneyTotal
    hvr <- hpdfl / hitterMoneyTotal
    pvr <- ppdfl / pitcherMoneyTotal
    hnleft <- hitterTotal - hitterTaken
    pnleft <- pitcherTotal - pitcherTaken
    data.frame(type = c("hitter","pitcher"),
               playersProt = c(hpr, ppr),
               dollarsSpent = c(hsr, psr),
               ToFill = c(hnleft, pnleft),
               valueTaken = c(hvr, pvr))
  })

  # --- ppp: protected by position ---
  ppp_r <- reactive({
    pc <- protClean_r()
    # Assign Position like draftGuide does
    pc <- pc %>% mutate(Position = Pos)
    group_by(pc, Position) %>% summarize(Count = n(), .groups = 'drop')
  })

  # --- Available player pools (remove rostered) ---
  AllH_avail <- reactive({
    roster <- rv$roster
    anti_join(AllH_full, roster, by = c('playerid')) %>% arrange(-pDFL)
  })

  AllP_avail <- reactive({
    roster <- rv$roster
    anti_join(AllP_full, roster, by = c('playerid')) %>% arrange(-pDFL)
  })

  # --- topHitters ---
  topHitters_r <- reactive({
    AllH_avail() %>% filter(pDFL > 15) %>%
      select(Player, MLB, posEl, Age, pDFL, ADP = pADP, rankDiff, Skew = pSkew,
             HR = pHR, RBI = pRBI, R = pR, SB = pSB, AVG = pAVG)
  })

  # --- injOrig (injuries minus rostered) ---
  injOrig_r <- reactive({
    roster <- rv$roster
    anti_join(injOrig_full, roster, by = c('Player'))
  })

  # --- Helper functions (use reactive data) ---
  tProtect <- function(tm) {
    pc <- protClean_r()
    filter(pc, Team == tm) %>%
      select(Player, Pos, Age, pDFL, Salary, Contract, rankDiff, Skew = pSkew) %>% arrange(-pDFL)
  }

  posProtect <- function(pos) {
    pc <- protClean_r()
    filter(pc, Pos == pos) %>%
      select(Player, Team, Age, pDFL, Salary, Contract, rankDiff, Skew = pSkew) %>% arrange(Team)
  }

  uniqueProtect <- function(pos) {
    pc <- protClean_r()
    res <- filter(pc, Pos == pos) %>% select(Team) %>% unique() %>% nrow()
    paste("Unique Teams = ", res)
  }

  teamsInterested <- function(pos) {
    pc <- protClean_r()
    cs <- currentSummary_r()
    allteams <- data.frame(Team = teams)
    have <- filter(pc, Pos == pos) %>% select(Team) %>% unique()
    need <- anti_join(allteams, have, by = 'Team')
    need <- inner_join(need, cs, by = 'Team')
    if (pos %in% c('SP','RP')) {
      need <- filter(need, group == 'pitching')
    } else {
      need <- filter(need, group == 'hitting')
    }
    arrange(need, -salleft)
  }

  tpSummary <- function(tm) {
    cs <- currentSummary_r()
    filter(cs, Team == tm)
  }

  hitPlayersbyPos <- function(pos) {
    ah <- AllH_avail()
    res <- ah %>% filter(Pos == pos | str_detect(posEl, pos), pSGP > 0) %>%
      arrange(-pDFL, -pSGP) %>% dplyr::rename(DFL = pDFL, SGP = pSGP)
    res <- mutate(res, RPV = (SGP - aRPV(res, nrow(filter(res, DFL > 0)))) /
                    aRPV(res, nrow(filter(res, DFL > 0))))
    select(res, Player, MLB, posEl, Age, DFL, RPV, SGP, orank, ADP = pADP, rankDiff, Skew = pSkew,
           HR = pHR, RBI = pRBI, R = pR, SB = pSB, AVG = pAVG, Injury, Expected.Return)
  }

  pitPlayersbyPos <- function(pos) {
    ap <- AllP_avail()
    res <- ap %>% filter(Pos == pos, pSGP > 0) %>%
      arrange(-pDFL, -pSGP) %>% dplyr::rename(DFL = pDFL, SGP = pSGP) %>% head(200)
    res <- mutate(res, RPV = (SGP - aRPV(res, nrow(filter(res, DFL > 0)))) /
                    aRPV(res, nrow(filter(res, DFL > 0))))
    select(res, Player, MLB, Age, DFL, RPV, SGP, orank, ADP = pADP, rankDiff, Skew = pSkew,
           W = pW, SO = pSO, ERA = pERA, SV = pSV, HLD = pHLD, Injury, Expected.Return)
  }

  # ============================
  # Draft action handler (with validation)
  # ============================
  observeEvent(input$draftBtn, {
    req(input$playerSearch, input$draftTeam, input$draftSalary)

    pid <- input$playerSearch
    team <- input$draftTeam
    salary <- input$draftSalary

    # Look up player in full pools
    hMatch <- AllH_full %>% filter(playerid == pid)
    pMatch <- AllP_full %>% filter(playerid == pid)

    if (nrow(hMatch) > 0) {
      pInfo <- hMatch[1,]
      pos <- pInfo$Pos
      mlb <- pInfo$MLB
    } else if (nrow(pMatch) > 0) {
      pInfo <- pMatch[1,]
      pos <- pInfo$Pos
      mlb <- pInfo$MLB
    } else {
      showNotification("Player not found!", type = "error")
      return()
    }

    playerName <- pInfo$Player

    newRow <- data.frame(
      Player = playerName,
      Pos = pos,
      Team = team,
      Salary = salary,
      Contract = 1,
      MLB = mlb,
      playerid = as.character(pid),
      orank = NA,
      stringsAsFactors = FALSE
    )

    # --- Validation checks ---
    teamRoster <- rv$roster %>% filter(Team == team)
    teamSpent <- sum(teamRoster$Salary)
    teamCount <- nrow(teamRoster)

    warnings <- c()
    # Check 1: Would exceed $260 cap
    if (teamSpent + salary > cap) {
      warnings <- c(warnings, paste0("Team would exceed $260 cap ($", teamSpent + salary, " total)"))
    }
    # Check 2: Less than $1/player for remaining spots
    slotsLeft <- 25 - teamCount - 1  # after this pick
    dollarsLeft <- cap - teamSpent - salary
    if (slotsLeft > 0 && dollarsLeft < slotsLeft) {
      warnings <- c(warnings, paste0("Only $", dollarsLeft, " left for ", slotsLeft, " remaining spots"))
    }
    # Check 3: Would exceed 25 players
    if (teamCount >= 25) {
      warnings <- c(warnings, paste0("Team already has ", teamCount, " players (max 25)"))
    }

    if (length(warnings) > 0) {
      # Stash pending pick and show warning modal
      rv$pendingPick <- newRow
      showModal(modalDialog(
        title = "Draft Warning",
        tags$ul(lapply(warnings, tags$li)),
        paste0("Draft ", playerName, " to ", team, " for $", salary, "?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirmDraftBtn", "Draft Anyway", class = "btn-danger")
        )
      ))
    } else {
      # No warnings — draft immediately
      rv$draftLog <- c(rv$draftLog, list(newRow))
      rv$roster <- bind_rows(rv$roster, newRow)
      write.csv(rv$roster, rosterFile, row.names = FALSE)
      updateSelectizeInput(session, 'playerSearch', selected = "")
      showNotification(paste0("Drafted ", playerName, " to ", team, " for $", salary),
                       type = "message")
    }
  })

  # ============================
  # Confirmed draft handler (after warning modal)
  # ============================
  observeEvent(input$confirmDraftBtn, {
    req(rv$pendingPick)
    newRow <- rv$pendingPick
    rv$pendingPick <- NULL

    rv$draftLog <- c(rv$draftLog, list(newRow))
    rv$roster <- bind_rows(rv$roster, newRow)
    write.csv(rv$roster, rosterFile, row.names = FALSE)
    updateSelectizeInput(session, 'playerSearch', selected = "")
    removeModal()
    showNotification(paste0("Drafted ", newRow$Player, " to ", newRow$Team, " for $", newRow$Salary),
                     type = "message")
  })

  # ============================
  # Undo handler
  # ============================
  observeEvent(input$undoBtn, {
    if (length(rv$draftLog) == 0) {
      showNotification("Nothing to undo!", type = "warning")
      return()
    }

    lastPick <- rv$draftLog[[length(rv$draftLog)]]
    rv$draftLog <- rv$draftLog[-length(rv$draftLog)]

    # Remove that playerid from roster
    rv$roster <- rv$roster %>% filter(playerid != lastPick$playerid)

    # Persist
    write.csv(rv$roster, rosterFile, row.names = FALSE)

    showNotification(paste0("Undid pick: ", lastPick$Player), type = "message")
  })

  # ============================
  # Player search updater
  # ============================
  observe({
    ah <- AllH_avail()
    ap <- AllP_avail()

    hChoices <- setNames(
      ah$playerid,
      paste0(ah$Player, " (", ah$Pos, " - $", round(ah$pDFL), ")")
    )
    pChoices <- setNames(
      ap$playerid,
      paste0(ap$Player, " (", ap$Pos, " - $", round(ap$pDFL), ")")
    )
    allChoices <- c(hChoices, pChoices)
    # Sort by display label
    allChoices <- allChoices[order(names(allChoices))]

    updateSelectizeInput(session, 'playerSearch',
                         choices = allChoices,
                         server = TRUE)
  })

  # Team dropdown for draft
  updateSelectInput(session, 'draftTeam', choices = teams)

  # ============================
  # New outputs: Recent Picks, Draft Standings
  # ============================
  output$recentPicks <- DT::renderDataTable({
    log <- rv$draftLog
    if (length(log) == 0) {
      df <- data.frame(Player = character(), Team = character(),
                       Pos = character(), Salary = numeric())
    } else {
      df <- bind_rows(rev(log)) %>% select(Player, Team, Pos, Salary, MLB)
    }
    datatable(df, options = list(pageLength = 15, autoWidth = FALSE,
                                 searching = FALSE, info = FALSE))
  })

  output$draftStandings <- DT::renderDataTable({
    ps <- pstandings_r()
    datatable(ps, options = list(pageLength = 20, autoWidth = FALSE,
                                 paging = FALSE, searching = FALSE, info = FALSE)) %>%
      formatCurrency(c('TotalValue','Earned','VPPlayer','DPP','FullValue')) %>%
      formatRound('CashLeft', 0) %>%
      formatRound(c('ValueRatio','zScore'), 2)
  })

  # ============================
  # Existing outputs — now reactive
  # ============================
  output$pstandings <- DT::renderDataTable({
    ps <- pstandings_r()
    datatable(ps, options = list(pageLength = 20, autoWidth = FALSE,
                                 paging = FALSE, searching = FALSE, info = FALSE)) %>%
      formatCurrency(c('TotalValue','Earned','VPPlayer','DPP','FullValue')) %>%
      formatRound('CashLeft', 0) %>%
      formatRound(c('ValueRatio','zScore'), 2)
  })

  output$protectSummary <- DT::renderDataTable({
    datatable(protectSummary_r(),
              options = list(pageLength = 20, autoWidth = FALSE,
                             paging = FALSE, searching = FALSE, info = FALSE)) %>%
      formatPercentage(c('playersProt','dollarsSpent','valueTaken'), 2) %>%
      formatRound(c('ToFill'), 0)
  })

  output$ppp <- DT::renderDataTable({
    datatable(ppp_r(),
              options = list(pageLength = 20, autoWidth = FALSE,
                             paging = FALSE, searching = FALSE, info = FALSE))
  })

  # Team selector
  updateSelectizeInput(session, 'e1', choices = teams, selected = 'Liquor Crickets')
  output$tname <- renderText({ input$e1 })

  output$tProtect <- DT::renderDataTable({
    req(input$e1)
    datatable(tProtect(input$e1),
              options = list(pageLength = 20, autoWidth = FALSE,
                             paging = FALSE, searching = FALSE, info = FALSE)) %>%
      formatCurrency('pDFL') %>%
      formatRound(c('Age','rankDiff'), 0) %>%
      formatRound('Skew', 3)
  })

  output$Goals <- DT::renderDataTable({
    req(input$e1)
    rp <- rpitchers_r()
    rh <- rhitters_r()
    datatable(calcGoals(rp, rh, targets, input$e1),
              options = list(pageLength = 20, autoWidth = FALSE,
                             paging = FALSE, searching = FALSE, info = FALSE)) %>%
      formatPercentage('pc', 2) %>%
      formatRound(c('collected','needed'), 0)
  })

  output$tpSummary <- DT::renderDataTable({
    req(input$e1)
    datatable(tpSummary(input$e1),
              options = list(pageLength = 20, autoWidth = FALSE,
                             paging = FALSE, searching = FALSE, info = FALSE)) %>%
      formatRound('salleft', 0)
  })

  # Position selector
  updateSelectizeInput(session, 'e2', choices = hpos, selected = 'OF')
  output$hpos <- renderText({ input$e2 })

  output$hpbpos <- DT::renderDataTable({
    req(input$e2)
    datatable(hitPlayersbyPos(input$e2),
              options = list(pageLength = 20, autoWidth = FALSE,
                             searching = FALSE, info = FALSE), filter = 'top') %>%
      formatRound(c('Age','ADP','rankDiff','HR','RBI','R','SB'), 0) %>%
      formatCurrency('DFL') %>%
      formatRound(c('RPV','SGP','Skew','AVG'), 3)
  })

  # Pitcher role selector
  updateSelectizeInput(session, 'e3', choices = ppos, selected = 'SP')
  output$ppos <- renderText({ input$e3 })

  output$ppbpos <- DT::renderDataTable({
    req(input$e3)
    datatable(pitPlayersbyPos(input$e3),
              options = list(pageLength = 20, autoWidth = FALSE,
                             info = FALSE), filter = 'top') %>%
      formatRound(c('Age','ADP','rankDiff','W','SV','HLD','SO'), 0) %>%
      formatCurrency('DFL') %>%
      formatRound(c('RPV','SGP','Skew','ERA'), 3)
  })

  # Protect by position
  updateSelectizeInput(session, 'e4', choices = allpos, selected = 'OF')
  output$allpos <- renderText({ input$e4 })
  output$uniquePos <- renderText({
    req(input$e4)
    uniqueProtect(input$e4)
  })

  output$posProtect <- DT::renderDataTable({
    req(input$e4)
    datatable(posProtect(input$e4),
              options = list(pageLength = 20, autoWidth = FALSE,
                             info = FALSE), filter = 'top') %>%
      formatCurrency('pDFL') %>%
      formatRound(c('Age','rankDiff'), 0) %>%
      formatRound('Skew', 3)
  })

  output$tNeed <- DT::renderDataTable({
    req(input$e4)
    datatable(teamsInterested(input$e4),
              options = list(pageLength = 20, autoWidth = FALSE,
                             paging = FALSE, searching = FALSE, info = FALSE)) %>%
      formatRound('salleft', 0)
  })

  # Static outputs that don't change with drafting
  output$rrcResults <- DT::renderDataTable({
    datatable(rrcResults,
              options = list(pageLength = 20, autoWidth = FALSE,
                             info = FALSE), filter = 'top') %>%
      formatRound(c('pADP','pW','pSV','pHLD','pSO'), 0) %>%
      formatCurrency('pDFL') %>%
      formatRound(c('pSGP','pERA','pK/9','pBB/9'), 3)
  })

  output$injOrig <- DT::renderDataTable({
    datatable(injOrig_r(),
              options = list(pageLength = 20, autoWidth = FALSE,
                             info = FALSE), filter = 'top') %>%
      formatCurrency('pDFL')
  })

  output$topHitters <- DT::renderDataTable({
    datatable(topHitters_r(),
              options = list(pageLength = 20, autoWidth = FALSE,
                             searching = FALSE, info = FALSE)) %>%
      formatRound(c('Age','ADP','HR','RBI','R','SB'), 0) %>%
      formatCurrency('pDFL') %>%
      formatRound(c('AVG'), 3)
  })

  output$prospectH <- DT::renderDataTable({
    datatable(prospectH,
              options = list(pageLength = 20, autoWidth = FALSE,
                             searching = FALSE, info = FALSE), filter = 'top') %>%
      formatRound(c('Age','ADP'), 0) %>% formatCurrency('DFL')
  })

  output$prospectP <- DT::renderDataTable({
    datatable(prospectP,
              options = list(pageLength = 20, autoWidth = FALSE,
                             searching = FALSE, info = FALSE)) %>%
      formatRound(c('Age','ADP'), 0) %>% formatCurrency('DFL')
  })

})
