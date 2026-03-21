# LiveDraftTool server.R — reactive draft tool

library(ggplot2)

setwd("../code/")
source("./draftGuide.r")
setwd("../LiveDraftTool")

# --- Constants ---
teams <- sort(unique(pstandings$Team))
hpos <- list('C','1B','2B','SS','3B','OF')
ppos <- list('SP','MR','CL')
allpos <- c(hpos,list('SP','MR','CL'))
posThresholds <- c('OF' = 3, 'SP' = 5)  # positions needing >1 protected
cap <- 260
nteams <- 13
nhitters <- 13
npitchers <- 12

# Save original pools for blend toggle
AllH_orig <- AllH_full
AllP_orig <- AllP_full

# --- Roster file initialization ---
rosterFile <- str_c("../", cyear, "DraftRosters.csv")
budgetFile <- str_c("../", cyear, "DraftBudgets.csv")
targetFile <- str_c("../", cyear, "DraftTargets.csv")
protFile   <- str_c("../", cyear, "ProtectionLists.csv")

needSeed <- !file.exists(rosterFile)

# Re-seed if protection list is newer and no picks have been made yet
if (!needSeed && file.exists(protFile)) {
  protMtime <- file.info(protFile)$mtime
  rostMtime <- file.info(rosterFile)$mtime
  if (!is.na(protMtime) && !is.na(rostMtime) && protMtime > rostMtime) {
    existingRoster <- read.csv(rosterFile, stringsAsFactors = FALSE)
    if (all(is.na(existingRoster$DraftOrder))) {
      needSeed <- TRUE
    }
  }
}

if (needSeed) {
  # Seed from official protection list
  initRoster <- read.csv(protFile, stringsAsFactors = FALSE)
  initRoster$playerid <- as.character(initRoster$playerid)
  # Remove row-number column if present
  if ("X" %in% names(initRoster)) initRoster$X <- NULL

  # Look up MLB team from full player pools (fills in any missing MLB values)
  hLookup <- AllH_full %>% select(playerid, MLB) %>% distinct()
  pLookup <- AllP_full %>% select(playerid, MLB) %>% distinct()
  mlbLookup <- bind_rows(hLookup, pLookup) %>% distinct(playerid, .keep_all = TRUE)
  if (!"MLB" %in% names(initRoster)) {
    initRoster <- left_join(initRoster, mlbLookup, by = "playerid")
  } else {
    # Fill missing MLB values from lookup
    initRoster <- left_join(initRoster, mlbLookup, by = "playerid", suffix = c("", ".lookup"))
    initRoster$MLB <- ifelse(is.na(initRoster$MLB) | initRoster$MLB == "",
                             initRoster$MLB.lookup, initRoster$MLB)
    initRoster$MLB.lookup <- NULL
  }
  initRoster$MLB <- replace_na(initRoster$MLB, "")

  if (!"orank" %in% names(initRoster)) initRoster$orank <- NA
  initRoster$DraftOrder <- NA
  initRoster <- initRoster %>% select(Player, Pos, Team, Salary, Contract, MLB, playerid, orank, DraftOrder)
  write.csv(initRoster, rosterFile, row.names = FALSE)
}


shinyServer(function(input, output, session) {

  # --- Reactive state ---
  rv <- reactiveValues(
    roster = {
      df <- read.csv(rosterFile, stringsAsFactors = FALSE)
      df$playerid <- as.character(df$playerid)
      if (!"DraftOrder" %in% names(df)) df$DraftOrder <- NA
      df
    },
    draftLog = {
      df <- read.csv(rosterFile, stringsAsFactors = FALSE)
      df$playerid <- as.character(df$playerid)
      if (!"DraftOrder" %in% names(df)) df$DraftOrder <- NA
      drafted <- df %>% filter(!is.na(DraftOrder)) %>% arrange(DraftOrder)
      if (nrow(drafted) > 0) {
        split(drafted, seq_len(nrow(drafted)))
      } else {
        list()
      }
    },
    pendingPick = NULL,
    pendingBudgetEdit = NULL,
    targets = {
      if (file.exists(targetFile)) {
        as.character(read.csv(targetFile, stringsAsFactors = FALSE)$playerid)
      } else {
        character()
      }
    },
    budgets = {
      if (file.exists(budgetFile)) {
        read.csv(budgetFile, stringsAsFactors = FALSE)
      } else {
        data.frame(Team = character(), Slot = character(), Budget = numeric(), stringsAsFactors = FALSE)
      }
    }
  )

  # --- Blended player pools (actual stats + projections) ---
  blendedPools_r <- reactive({
    mode <- input$valMode
    if (is.null(mode) || mode == "proj" ||
        is.null(leaderboards$hitters) || is.null(leaderboards$pitchers)) {
      return(list(hitters = AllH_orig, pitchers = AllP_orig))
    }

    lbH <- leaderboards$hitters
    lbP <- leaderboards$pitchers
    lbH$playerid <- as.character(lbH$playerid)
    lbP$playerid <- as.character(lbP$playerid)

    # Start with copies of original pools
    newH <- AllH_orig
    newP <- AllP_orig

    # Blend hitter stats: actual counting stats + projected
    actH <- lbH %>% select(playerid, aHR = HR, aR = R, aRBI = RBI, aSB = SB, aH = H, aAB = AB)
    newH <- left_join(newH, actH, by = "playerid")
    newH <- newH %>% mutate(
      pHR = pHR + replace_na(aHR, 0),
      pR = pR + replace_na(aR, 0),
      pRBI = pRBI + replace_na(aRBI, 0),
      pSB = pSB + replace_na(aSB, 0),
      pH = pH + replace_na(aH, 0),
      pAB = pAB + replace_na(aAB, 0),
      pAVG = ifelse(pAB > 0, pH / pAB, 0)
    ) %>% select(-aHR, -aR, -aRBI, -aSB, -aH, -aAB)

    # Blend pitcher stats: actual counting stats + projected
    actP <- lbP %>% select(playerid, aW = W, aSO = SO, aHLD = HLD, aSV = SV, aIP = IP, aERA = ERA)
    newP <- left_join(newP, actP, by = "playerid")
    newP <- newP %>% mutate(
      aER = replace_na(aERA, 0) * replace_na(aIP, 0) / 9,
      pW = pW + replace_na(aW, 0),
      pSO = pSO + replace_na(aSO, 0),
      pHLD = pHLD + replace_na(aHLD, 0),
      pSV = pSV + replace_na(aSV, 0),
      pIP = pIP + replace_na(aIP, 0),
      pER = pER + aER,
      pERA = ifelse(pIP > 0, pER / pIP * 9, 0)
    ) %>% select(-aW, -aSO, -aHLD, -aSV, -aIP, -aERA, -aER)

    # Recompute SGP with blended stats
    newP$pSGP <- pitSGP(newP)
    newH$pSGP <- hitSGP(newH)

    # Re-run valuation to get new pDFL
    nlist <- preLPP(newH, newP)
    th <- nlist[[1]]
    tp <- nlist[[2]]

    # Update pDFL
    newH <- newH %>% select(-pDFL) %>%
      left_join(th %>% dplyr::rename(pDFL = zDFL), by = "playerid")
    newH$pDFL <- replace_na(newH$pDFL, 0)

    newP <- newP %>% select(-pDFL) %>%
      left_join(tp %>% dplyr::rename(pDFL = zDFL), by = "playerid")
    newP$pDFL <- replace_na(newP$pDFL, 0)

    # Recompute orank within position
    newH <- newH %>% select(-any_of('orank')) %>%
      group_by(Pos) %>% mutate(orank = rank(-pDFL)) %>% ungroup()
    newP <- newP %>% select(-any_of('orank')) %>%
      group_by(Pos) %>% mutate(orank = rank(-pDFL)) %>% ungroup()

    # Recompute rankDiff
    allRanks <- bind_rows(
      newH %>% select(playerid, pDFL),
      newP %>% select(playerid, pDFL)
    ) %>% group_by(playerid) %>% summarise(pDFL = max(pDFL), .groups = 'drop') %>%
      mutate(myRank = rank(-pDFL)) %>% select(playerid, myRank)

    newH <- newH %>% select(-any_of(c('myRank', 'rankDiff'))) %>%
      left_join(allRanks, by = 'playerid') %>%
      mutate(rankDiff = pADP - myRank)
    newP <- newP %>% select(-any_of(c('myRank', 'rankDiff'))) %>%
      left_join(allRanks, by = 'playerid') %>%
      mutate(rankDiff = pADP - myRank)

    list(hitters = newH, pitchers = newP)
  })

  AllH_active <- reactive({ blendedPools_r()$hitters })
  AllP_active <- reactive({ blendedPools_r()$pitchers })

  # Blend mode status indicator
  output$blendStatus <- renderUI({
    mode <- input$valMode
    if (is.null(mode) || mode == "proj") {
      tags$div(style = "text-align:center; padding:6px; margin-top:10px; border-radius:4px; background:#d4edda;",
               tags$small(style = "color:#155724;", "Using projections only"))
    } else if (is.null(leaderboards$hitters) || is.null(leaderboards$pitchers)) {
      tags$div(style = "text-align:center; padding:6px; margin-top:10px; border-radius:4px; background:#fff3cd;",
               tags$small(style = "color:#856404;", "Fetch leaderboards first to enable blend"))
    } else {
      tags$div(style = "text-align:center; padding:6px; margin-top:10px; border-radius:4px; background:#cce5ff;",
               tags$small(style = "color:#004085;", "Using blended (actual + projected) stats"))
    }
  })

  # --- Helper: split roster into H/P ---
  isPitcherPos <- function(pos) pos %in% c('P','SP','MR','CL','RP')

  # --- Helper: mark target players with star + hidden flag ---
  markTargets <- function(df, targets) {
    df$isTarget <- as.integer(df$playerid %in% targets)
    tIdx <- which(df$isTarget == 1)
    if (length(tIdx) > 0) {
      df$Player[tIdx] <- paste0("\u2605 ", df$Player[tIdx])
    }
    df
  }

  # --- Target toggle (Draft tab player search) ---
  observeEvent(input$targetBtn, {
    req(input$playerSearch)
    pid <- input$playerSearch
    allPlayers <- bind_rows(
      AllH_active() %>% select(playerid, Player),
      AllP_active() %>% select(playerid, Player)
    ) %>% distinct(playerid, .keep_all = TRUE)
    pName <- allPlayers$Player[allPlayers$playerid == pid]
    if (length(pName) == 0) pName <- "Player"

    if (pid %in% rv$targets) {
      rv$targets <- rv$targets[rv$targets != pid]
      showNotification(paste0("Removed target: ", pName), type = "message")
    } else {
      rv$targets <- c(rv$targets, pid)
      showNotification(paste0("Added target: ", pName), type = "message")
    }
    write.csv(data.frame(playerid = rv$targets, stringsAsFactors = FALSE), targetFile, row.names = FALSE)
  })

  # --- Target toggle (Hitters tab row selection) ---
  observeEvent(input$targetHBtn, {
    sel <- input$hpbpos_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      showNotification("Select a player row first", type = "warning")
      return()
    }
    data <- hitPlayersbyPos(input$e2)
    pid <- as.character(data$playerid[sel])
    if (pid %in% rv$targets) {
      rv$targets <- rv$targets[rv$targets != pid]
      showNotification(paste0("Removed target: ", data$Player[sel]), type = "message")
    } else {
      rv$targets <- c(rv$targets, pid)
      showNotification(paste0("Added target: ", data$Player[sel]), type = "message")
    }
    write.csv(data.frame(playerid = rv$targets, stringsAsFactors = FALSE), targetFile, row.names = FALSE)
  })

  # --- Target toggle (Pitchers tab row selection) ---
  observeEvent(input$targetPBtn, {
    sel <- input$ppbpos_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      showNotification("Select a player row first", type = "warning")
      return()
    }
    data <- pitPlayersbyPos(input$e3)
    pid <- as.character(data$playerid[sel])
    if (pid %in% rv$targets) {
      rv$targets <- rv$targets[rv$targets != pid]
      showNotification(paste0("Removed target: ", data$Player[sel]), type = "message")
    } else {
      rv$targets <- c(rv$targets, pid)
      showNotification(paste0("Added target: ", data$Player[sel]), type = "message")
    }
    write.csv(data.frame(playerid = rv$targets, stringsAsFactors = FALSE), targetFile, row.names = FALSE)
  })

  # --- Target toggle (Prospects tab — one button, checks active subtab) ---
  observeEvent(input$targetProspBtn, {
    tab <- input$prospectTab
    if (!is.null(tab) && tab == "Pitchers") {
      sel <- input$prospectP_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        showNotification("Select a player row first", type = "warning")
        return()
      }
      data <- prospectP_r()
      pid <- as.character(data$playerid[sel])
      pName <- data$Player[sel]
    } else {
      sel <- input$prospectH_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        showNotification("Select a player row first", type = "warning")
        return()
      }
      data <- prospectH_r()
      pid <- as.character(data$playerid[sel])
      pName <- data$Player[sel]
    }
    if (pid %in% rv$targets) {
      rv$targets <- rv$targets[rv$targets != pid]
      showNotification(paste0("Removed target: ", pName), type = "message")
    } else {
      rv$targets <- c(rv$targets, pid)
      showNotification(paste0("Added target: ", pName), type = "message")
    }
    write.csv(data.frame(playerid = rv$targets, stringsAsFactors = FALSE), targetFile, row.names = FALSE)
  })

  # --- Target toggle (Injuries tab) ---
  observeEvent(input$targetInjBtn, {
    sel <- input$injOrig_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      showNotification("Select a player row first", type = "warning")
      return()
    }
    data <- injOrig_r()
    pid <- as.character(data$playerid[sel])
    if (pid %in% rv$targets) {
      rv$targets <- rv$targets[rv$targets != pid]
      showNotification(paste0("Removed target: ", data$Player[sel]), type = "message")
    } else {
      rv$targets <- c(rv$targets, pid)
      showNotification(paste0("Added target: ", data$Player[sel]), type = "message")
    }
    write.csv(data.frame(playerid = rv$targets, stringsAsFactors = FALSE), targetFile, row.names = FALSE)
  })

  # --- Target toggle (Bullpen Depth Charts tab) ---
  observeEvent(input$targetBPBtn, {
    sel <- input$rrcResults_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      showNotification("Select a player row first", type = "warning")
      return()
    }
    pid <- as.character(rrcResults$playerid[sel])
    pName <- rrcResults$Player[sel]
    if (pid %in% rv$targets) {
      rv$targets <- rv$targets[rv$targets != pid]
      showNotification(paste0("Removed target: ", pName), type = "message")
    } else {
      rv$targets <- c(rv$targets, pid)
      showNotification(paste0("Added target: ", pName), type = "message")
    }
    write.csv(data.frame(playerid = rv$targets, stringsAsFactors = FALSE), targetFile, row.names = FALSE)
  })

  # --- My Targets tab: table + remove ---
  output$targetTable <- DT::renderDataTable({
    allPlayers <- bind_rows(
      AllH_active() %>% select(playerid, Player, Pos, MLB, Age, pDFL),
      AllP_active() %>% select(playerid, Player, Pos, MLB, Age, pDFL)
    ) %>% distinct(playerid, .keep_all = TRUE)
    info <- allPlayers %>% filter(playerid %in% rv$targets) %>%
      mutate(Player = fgLink(Player, playerid)) %>%
      arrange(-pDFL) %>%
      select(Player, Pos, MLB, Age, pDFL)
    datatable(info, selection = 'single',
              options = list(paging = FALSE, searching = FALSE, info = FALSE,
                             ordering = FALSE, autoWidth = FALSE),
              escape = FALSE) %>%
      formatCurrency('pDFL') %>%
      formatRound('Age', 0)
  })

  observeEvent(input$targetTable_rows_selected, {
    sel <- input$targetTable_rows_selected
    if (is.null(sel) || length(sel) == 0) return()
    allPlayers <- bind_rows(
      AllH_active() %>% select(playerid, Player, pDFL),
      AllP_active() %>% select(playerid, Player, pDFL)
    ) %>% distinct(playerid, .keep_all = TRUE)
    info <- allPlayers %>% filter(playerid %in% rv$targets) %>% arrange(-pDFL)
    if (sel <= nrow(info)) {
      pid <- info$playerid[sel]
      pName <- info$Player[sel]
      rv$targets <- rv$targets[rv$targets != pid]
      write.csv(data.frame(playerid = rv$targets, stringsAsFactors = FALSE), targetFile, row.names = FALSE)
      showNotification(paste0("Removed target: ", pName), type = "message")
    }
  })

  # --- Build roster card for a team (slot assignment) ---
  buildTeamRoster <- function(hitters, pitchers) {
    makeRow <- function(slot, p) {
      data.frame(Slot = slot, Player = p$Player[1], Pos = p$Pos[1],
                 MLB = p$MLB[1], Age = p$Age[1], Yr = p$Contract[1],
                 Salary = p$Salary[1], pDFL = p$pDFL[1], Value = p$Value[1],
                 playerid = p$playerid[1],
                 stringsAsFactors = FALSE)
    }
    emptyRow <- function(slot) {
      data.frame(Slot = slot, Player = "", Pos = "", MLB = "",
                 Age = NA_real_, Yr = NA_real_, Salary = NA_real_,
                 pDFL = NA_real_, Value = NA_real_,
                 playerid = NA_character_, stringsAsFactors = FALSE)
    }

    # --- Hitter slots: C, 1B, 2B, SS, 3B, OF x3, DH, BN x4 = 13 ---
    hSlots <- list(
      list(slot = "C",  pos = "C",  n = 1),
      list(slot = "1B", pos = "1B", n = 1),
      list(slot = "2B", pos = "2B", n = 1),
      list(slot = "SS", pos = "SS", n = 1),
      list(slot = "3B", pos = "3B", n = 1),
      list(slot = "OF", pos = "OF", n = 3)
    )

    usedIds <- c()
    hRows <- list()
    for (sd in hSlots) {
      cands <- hitters %>% filter((!is.na(posEl) & str_detect(posEl, fixed(sd$pos))) | (is.na(posEl) & Pos == sd$pos), !playerid %in% usedIds) %>% arrange(-pDFL)
      for (k in seq_len(sd$n)) {
        label <- if (sd$n > 1) paste0(sd$slot, k) else sd$slot
        if (k <= nrow(cands)) {
          hRows[[length(hRows) + 1]] <- makeRow(label, cands[k, ])
          usedIds <- c(usedIds, cands$playerid[k])
        } else {
          hRows[[length(hRows) + 1]] <- emptyRow(label)
        }
      }
    }

    # DH + 4 bench
    remaining <- hitters %>% filter(!playerid %in% usedIds) %>% arrange(-pDFL)
    benchH <- c("DH", "BN1", "BN2", "BN3", "BN4")
    for (k in seq_along(benchH)) {
      if (k <= nrow(remaining)) {
        hRows[[length(hRows) + 1]] <- makeRow(benchH[k], remaining[k, ])
      } else {
        hRows[[length(hRows) + 1]] <- emptyRow(benchH[k])
      }
    }
    # Overflow hitters beyond defined slots
    if (nrow(remaining) > length(benchH)) {
      for (k in (length(benchH) + 1):nrow(remaining)) {
        hRows[[length(hRows) + 1]] <- makeRow(paste0("EX", k - length(benchH)), remaining[k, ])
      }
    }

    # --- Pitcher slots: SP x5, MR x2, CL x2, BN x3 = 12 ---
    pSlots <- list(
      list(slot = "SP", pos = "SP", n = 7),
      list(slot = "MR", pos = "MR", n = 2),
      list(slot = "CL", pos = "CL", n = 2)
    )

    usedIds <- c()
    pRows <- list()
    for (sd in pSlots) {
      cands <- pitchers %>% filter(Pos == sd$pos, !playerid %in% usedIds) %>% arrange(-pDFL)
      for (k in seq_len(sd$n)) {
        label <- if (sd$n > 1) paste0(sd$slot, k) else sd$slot
        if (k <= nrow(cands)) {
          pRows[[length(pRows) + 1]] <- makeRow(label, cands[k, ])
          usedIds <- c(usedIds, cands$playerid[k])
        } else {
          pRows[[length(pRows) + 1]] <- emptyRow(label)
        }
      }
    }

    remaining <- pitchers %>% filter(!playerid %in% usedIds) %>% arrange(-pDFL)
    for (k in 1:1) {
      if (k <= nrow(remaining)) {
        pRows[[length(pRows) + 1]] <- makeRow("BNP", remaining[k, ])
      } else {
        pRows[[length(pRows) + 1]] <- emptyRow("BNP")
      }
    }
    # Overflow pitchers beyond defined slots
    if (nrow(remaining) > 1) {
      for (k in 2:nrow(remaining)) {
        pRows[[length(pRows) + 1]] <- makeRow(paste0("EXP", k - 1), remaining[k, ])
      }
    }

    list(hitters = bind_rows(hRows), pitchers = bind_rows(pRows))
  }

  # Merge budget values into empty roster slots
  applyBudgets <- function(rosterDf, team, budgets) {
    tb <- budgets %>% filter(Team == team)
    if (nrow(tb) == 0) return(rosterDf)
    for (i in seq_len(nrow(rosterDf))) {
      if (rosterDf$Player[i] == "") {
        brow <- tb %>% filter(Slot == rosterDf$Slot[i])
        if (nrow(brow) > 0) {
          rosterDf$Salary[i] <- brow$Budget[1]
        }
      }
    }
    rosterDf
  }

  # --- Derived reactive: roster joined with projections ---
  rhitters_r <- reactive({
    roster <- rv$roster
    rH <- filter(roster, !isPitcherPos(Pos) | is.na(Pos))
    rH$playerid <- as.character(rH$playerid)
    res <- left_join(rH, AllH_active(), by = c('playerid'), copy = FALSE)
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
    res <- left_join(rP, AllP_active(), by = c('playerid'), copy = FALSE)
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

  # --- posNeed: position intelligence table ---
  posNeed_r <- reactive({
    req(input$e4)
    pos <- input$e4
    pc <- protClean_r()
    ps <- pstandings_r()
    cs <- currentSummary_r()
    rh <- rhitters_r()
    rp <- rpitchers_r()

    # Position thresholds (from global constant)
    threshold <- ifelse(pos %in% names(posThresholds), posThresholds[pos], 1)

    # Count protected per team at this position (using Pos column only)
    posCounts <- pc %>% filter(Pos == pos) %>%
      group_by(Team) %>% summarize(have = n(), .groups = 'drop')

    # All teams, join counts, compute Still Need
    allteams <- data.frame(Team = teams, stringsAsFactors = FALSE)
    need <- left_join(allteams, posCounts, by = 'Team') %>%
      mutate(have = replace_na(have, 0),
             StillNeed = pmax(0, threshold - have)) %>%
      filter(StillNeed > 0)

    if (nrow(need) == 0) return(data.frame(
      Team = character(), `Still Need` = integer(), Market = character(),
      `Cash Left` = numeric(), `$/Player` = numeric(), `Weakest Stats` = character(),
      stringsAsFactors = FALSE, check.names = FALSE
    ))

    # Market label from pstandings DPP ratio
    need <- left_join(need, ps %>% select(Team, Needed, DPP), by = 'Team')

    need$Market <- sapply(seq_len(nrow(need)), function(i) {
      tm <- need$Team[i]
      tmRow <- need[i, ]
      others <- ps %>% filter(Team != tm, Needed > 0)
      leagueAvgDPP <- if (sum(others$Needed) > 0) sum(others$CashLeft) / sum(others$Needed) else 0
      ratio <- if (leagueAvgDPP > 0 && tmRow$Needed > 0) tmRow$DPP / leagueAvgDPP else NA
      case_when(
        is.na(ratio) || tmRow$Needed <= 0 ~ "Full",
        ratio >= 1.3 ~ "Strong Buy",
        ratio >= 1.0 ~ "Lean Buy",
        ratio >= 0.8 ~ "Neutral",
        TRUE ~ "Wait"
      )
    })

    # Position-adjusted cash left from currentSummary
    isHitterPos <- pos %in% c('C','1B','2B','SS','3B','OF')
    csGroup <- if (isHitterPos) 'hitting' else 'pitching'
    csSub <- cs %>% filter(group == csGroup) %>% select(Team, salleft)
    need <- left_join(need, csSub, by = 'Team')
    need$CashLeft <- round(need$salleft, 0)

    # Weakest stats per team
    hitterStats <- c('HR','RBI','R','SB')
    pitcherStats <- c('W','K','SV','HLD')
    relevantStats <- if (isHitterPos) hitterStats else pitcherStats

    need$WeakestStats <- sapply(need$Team, function(tm) {
      goals <- calcGoals(rp, rh, targets, tm)
      goals <- goals %>% filter(statistic %in% relevantStats, pc < 0.65) %>%
        arrange(pc) %>% head(3)
      if (nrow(goals) == 0) return('<span style="color:#2ecc71;">On track</span>')
      paste(sapply(seq_len(nrow(goals)), function(j) {
        pct <- round(goals$pc[j] * 100)
        color <- if (goals$pc[j] < 0.50) '#e74c3c' else '#f39c12'
        paste0('<span style="color:', color, ';">', goals$statistic[j], ' ', pct, '%</span>')
      }), collapse = ', ')
    })

    # Sort by market label priority, then DPP descending
    statusOrd <- c("Strong Buy" = 1, "Lean Buy" = 2, "Neutral" = 3, "Wait" = 4, "Full" = 5)
    need <- need %>%
      mutate(ord = statusOrd[Market]) %>%
      arrange(ord, -DPP) %>%
      select(Team, StillNeed, Market, CashLeft, DPP, WeakestStats) %>%
      dplyr::rename(`Still Need` = StillNeed, `$/Player` = DPP, `Cash Left` = CashLeft,
                     `Weakest Stats` = WeakestStats)
    need
  })

  # --- FanGraphs link helper ---
  fgLink <- function(name, pid) {
    ifelse(is.na(pid) | name == "",
           name,
           paste0("<a target='_blank' href='//www.fangraphs.com/players/x/", pid, "/stats'>", name, "</a>"))
  }

  # --- Available player pools (remove rostered) ---
  AllH_avail <- reactive({
    roster <- rv$roster
    anti_join(AllH_active(), roster, by = c('playerid')) %>% arrange(-pDFL)
  })

  AllP_avail <- reactive({
    roster <- rv$roster
    anti_join(AllP_active(), roster, by = c('playerid')) %>% arrange(-pDFL)
  })

  # --- Buy Now gauge data ---
  buyNowData_r <- reactive({
    pos <- input$marketPos
    log <- rv$draftLog
    if (length(log) == 0) return(NULL)
    df <- bind_rows(log)

    # Filter draft log by position if selected
    if (!is.null(pos) && pos != "All") {
      df <- df %>% filter(Pos == pos)
      if (nrow(df) == 0) return(NULL)
    }

    valueLookup <- bind_rows(
      AllH_active() %>% select(playerid, pDFL),
      AllP_active() %>% select(playerid, pDFL)
    ) %>% distinct(playerid, .keep_all = TRUE)
    df <- left_join(df, valueLookup, by = "playerid")
    df$pDFL <- replace_na(df$pDFL, 0)
    df$surplus <- df$pDFL - df$Salary
    df$pickNum <- seq_len(nrow(df))
    window <- 5
    df$rollingAvg <- sapply(seq_len(nrow(df)), function(i) {
      start <- max(1, i - window + 1)
      mean(df$surplus[start:i])
    })
    df
  })

  output$buyNowGauge <- renderPlot({
    data <- buyNowData_r()
    if (is.null(data) || nrow(data) < 2) {
      plot.new()
      text(0.5, 0.5, "Picks will appear here", cex = 1.2, col = "gray")
      return()
    }
    ggplot(data, aes(x = pickNum, y = surplus)) +
      geom_col(aes(fill = surplus > 0), width = 0.7) +
      geom_line(aes(y = rollingAvg), color = "#3498db", linewidth = 1.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
      scale_fill_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"), guide = "none") +
      labs(x = "Pick #", y = "Surplus ($)") +
      theme_minimal(base_size = 12) +
      theme(plot.margin = margin(5, 10, 5, 10))
  }, height = 200)

  output$buyNowSignal <- renderUI({
    data <- buyNowData_r()
    if (is.null(data) || nrow(data) < 3) return(tags$span("Not enough picks yet", style="color:gray;"))
    n <- nrow(data)
    currentAvg <- data$rollingAvg[n]
    prevAvg <- if (n >= 6) data$rollingAvg[max(1, n - 5)] else 0
    trending_up <- currentAvg > prevAvg
    if (currentAvg >= 0 && trending_up) {
      label <- "Bargain Zone"; color <- "#2ecc71"
    } else if (currentAvg >= 0 && !trending_up) {
      label <- "Cooling Off"; color <- "#f39c12"
    } else if (currentAvg < 0 && trending_up) {
      label <- "Warming Up"; color <- "#f39c12"
    } else {
      label <- "Overpay Zone"; color <- "#e74c3c"
    }
    tags$div(
      style = paste0("text-align:center; padding:6px; border-radius:4px; background:", color, "20;"),
      tags$strong(style = paste0("color:", color, "; font-size:16px;"), label),
      tags$br(),
      tags$small(style = "color:gray;",
                 paste0("Last 5 avg: $", round(currentAvg, 1)))
    )
  })

  # --- Inflation rate ---
  inflationData_r <- reactive({
    pos <- input$marketPos
    pc <- protClean_r()

    if (!is.null(pos) && pos != "All") {
      pc <- pc %>% filter(Pos == pos)
    }

    totalSalary <- sum(pc$Salary)
    totalValue  <- sum(pc$pDFL)
    inflation   <- if (totalValue > 0) totalSalary / totalValue else NA
    nRostered   <- nrow(pc)

    list(totalSalary = totalSalary, totalValue = totalValue,
         nRostered = nRostered, inflation = inflation)
  })

  output$inflationDisplay <- renderUI({
    d <- inflationData_r()
    if (is.na(d$inflation)) return(tags$span("No data yet", style = "color:gray;"))

    pct <- (d$inflation - 1) * 100
    color <- if (pct > 30) "#e74c3c" else if (pct > 0) "#f39c12" else "#2ecc71"

    tags$div(
      style = "text-align:center; padding:8px; border-radius:4px; background:#f8f9fa; margin-bottom:10px;",
      tags$strong(style = paste0("font-size:22px; color:", color, ";"),
                  paste0(sprintf("%.2f", pct), "%")),
      tags$br(),
      tags$small(style = "color:gray;",
                 paste0("$", round(d$totalSalary), " spent / $", round(d$totalValue),
                        " value / ", d$nRostered, " rostered"))
    )
  })

  # --- Spending Power ---
  output$spendingPower <- renderUI({
    req(input$myTeam)
    ps <- pstandings_r()
    me <- ps %>% filter(Team == input$myTeam)
    if (nrow(me) == 0) return(tags$span("Team not found", style = "color:gray;"))

    myDPP <- me$DPP
    others <- ps %>% filter(Team != input$myTeam, Needed > 0)
    if (nrow(others) == 0) return(tags$span("No other teams with needs", style = "color:gray;"))

    leagueAvgDPP <- sum(others$CashLeft) / sum(others$Needed)
    ratio <- if (leagueAvgDPP > 0) myDPP / leagueAvgDPP else NA
    myRank <- sum(ps$DPP <= myDPP, na.rm = TRUE)  # rank among all teams (1 = worst)
    myRank <- nteams - myRank + 1  # flip so 1 = best

    if (is.na(ratio) || me$Needed <= 0) {
      label <- "Roster Full"; color <- "gray"
    } else if (ratio >= 1.3) {
      label <- "Strong Buy"; color <- "#2ecc71"
    } else if (ratio >= 1.0) {
      label <- "Lean Buy"; color <- "#27ae60"
    } else if (ratio >= 0.8) {
      label <- "Neutral"; color <- "#f39c12"
    } else {
      label <- "Wait"; color <- "#e74c3c"
    }

    tags$div(
      style = "text-align:center; padding:8px; border-radius:4px; background:#f8f9fa; margin-bottom:10px;",
      tags$strong(style = paste0("font-size:18px; color:", color, ";"), label),
      tags$br(),
      tags$span(style = "font-size:14px;",
                paste0("$", round(myDPP, 1), "/player vs $", round(leagueAvgDPP, 1), " avg")),
      tags$br(),
      tags$small(style = "color:gray;",
                 paste0("Rank: ", myRank, "/", nteams,
                        " | $", round(me$CashLeft), " left, ", me$Needed, " spots"))
    )
  })

  # ============================
  # Nominations tab
  # ============================
  updateSelectInput(session, 'nomTeam', choices = teams, selected = 'Liquor Crickets')

  output$nomSpendingPower <- renderUI({
    req(input$nomTeam)
    ps <- pstandings_r()
    me <- ps %>% filter(Team == input$nomTeam)
    if (nrow(me) == 0) return(tags$span("Team not found", style = "color:gray;"))

    myDPP <- me$DPP
    others <- ps %>% filter(Team != input$nomTeam, Needed > 0)
    if (nrow(others) == 0) return(tags$span("No other teams with needs", style = "color:gray;"))

    leagueAvgDPP <- sum(others$CashLeft) / sum(others$Needed)
    ratio <- if (leagueAvgDPP > 0) myDPP / leagueAvgDPP else NA
    myRank <- sum(ps$DPP <= myDPP, na.rm = TRUE)
    myRank <- nteams - myRank + 1

    if (is.na(ratio) || me$Needed <= 0) {
      label <- "Roster Full"; color <- "gray"
    } else if (ratio >= 1.3) {
      label <- "Strong Buy"; color <- "#2ecc71"
    } else if (ratio >= 1.0) {
      label <- "Lean Buy"; color <- "#27ae60"
    } else if (ratio >= 0.8) {
      label <- "Neutral"; color <- "#f39c12"
    } else {
      label <- "Wait"; color <- "#e74c3c"
    }

    tags$div(
      style = "text-align:center; padding:8px; border-radius:4px; background:#f8f9fa; margin-bottom:10px;",
      tags$strong(style = paste0("font-size:18px; color:", color, ";"), label),
      tags$br(),
      tags$span(style = "font-size:14px;",
                paste0("$", round(myDPP, 1), "/player vs $", round(leagueAvgDPP, 1), " avg")),
      tags$br(),
      tags$small(style = "color:gray;",
                 paste0("Rank: ", myRank, "/", nteams,
                        " | $", round(me$CashLeft), " left, ", me$Needed, " spots"))
    )
  })

  # --- Positional Inflation table ---
  posInflation_r <- reactive({
    positions <- c('C','1B','2B','SS','3B','OF','SP','MR','CL')
    pc <- protClean_r()

    # Value lookup for surplus from draft log
    valueLookup <- bind_rows(
      AllH_active() %>% select(playerid, pDFL),
      AllP_active() %>% select(playerid, pDFL)
    ) %>% distinct(playerid, .keep_all = TRUE)
    log <- rv$draftLog
    logDf <- if (length(log) > 0) bind_rows(log) else data.frame()

    rows <- lapply(positions, function(pos) {
      posPc <- pc %>% filter(Pos == pos)
      totalSalary <- sum(posPc$Salary)
      totalValue  <- sum(posPc$pDFL)
      inflation   <- if (totalValue > 0) (totalSalary / totalValue - 1) * 100 else NA

      # Trend: inflation % for last 5 drafted at this position
      trend <- NA
      if (nrow(logDf) > 0) {
        posPicks <- logDf %>% filter(Pos == pos)
        if (nrow(posPicks) > 0) {
          posPicks <- left_join(posPicks, valueLookup, by = "playerid")
          posPicks$pDFL <- replace_na(posPicks$pDFL, 0)
          last5 <- tail(posPicks, 5)
          trendValue <- sum(last5$pDFL)
          trend <- if (trendValue > 0) (sum(last5$Salary) / trendValue - 1) * 100 else NA
        }
      }

      data.frame(Position = pos, Rostered = nrow(posPc),
                 InflationNum = inflation, TrendNum = trend,
                 Inflation = if (!is.na(inflation)) paste0(sprintf("%.2f", inflation), "%") else NA,
                 Trend = if (!is.na(trend)) paste0(sprintf("%.2f", trend), "%") else NA,
                 stringsAsFactors = FALSE)
    })
    bind_rows(rows)
  })

  output$posInflation <- DT::renderDataTable({
    df <- posInflation_r()
    datatable(df %>% select(Position, Rostered, Inflation, Trend, InflationNum, TrendNum),
              options = list(paging = FALSE, searching = FALSE, info = FALSE,
                             ordering = FALSE, autoWidth = FALSE,
                             columnDefs = list(list(visible = FALSE, targets = c(5, 6))))) %>%
      formatStyle('Inflation', valueColumns = 'InflationNum',
                  color = styleInterval(c(0, 30), c('#2ecc71', '#f39c12', '#e74c3c'))) %>%
      formatStyle('Trend', valueColumns = 'TrendNum',
                  color = styleInterval(c(0, 30), c('#2ecc71', '#f39c12', '#e74c3c')))
  })

  # --- Nomination Targets table ---
  nomTargets_r <- reactive({
    req(input$nomTeam)
    myTeam <- input$nomTeam
    ps <- pstandings_r()
    pc <- protClean_r()
    ah <- AllH_avail()
    ap <- AllP_avail()

    # My filled positions
    myPositions <- pc %>% filter(Team == myTeam) %>% pull(Pos) %>% unique()

    otherTeams <- ps %>% filter(Team != myTeam)

    rows <- lapply(seq_len(nrow(otherTeams)), function(i) {
      tm <- otherTeams$Team[i]
      tmRow <- otherTeams[i, ]

      # Compute their spending status
      others <- ps %>% filter(Team != tm, Needed > 0)
      leagueAvgDPP <- if (sum(others$Needed) > 0) sum(others$CashLeft) / sum(others$Needed) else 0
      ratio <- if (leagueAvgDPP > 0 && tmRow$Needed > 0) tmRow$DPP / leagueAvgDPP else NA
      status <- case_when(
        is.na(ratio) || tmRow$Needed <= 0 ~ "Full",
        ratio >= 1.3 ~ "Strong Buy",
        ratio >= 1.0 ~ "Lean Buy",
        ratio >= 0.8 ~ "Neutral",
        TRUE ~ "Wait"
      )

      # Positions I have but they don't
      theirPositions <- pc %>% filter(Team == tm) %>% pull(Pos) %>% unique()
      targetPositions <- setdiff(myPositions, theirPositions)

      # Find best nominee across target positions (highest pDFL)
      bestPos <- ""
      bestPlayer <- ""
      bestPid <- NA_character_
      bestDFL <- NA_real_

      for (tp in targetPositions) {
        if (tp %in% c('SP','MR','CL')) {
          pool <- ap %>% filter(Pos == tp) %>% arrange(-pDFL)
        } else {
          pool <- ah %>% filter(Pos == tp | (!is.na(posEl) & str_detect(posEl, tp))) %>% arrange(-pDFL)
        }
        if (nrow(pool) > 0 && (is.na(bestDFL) || pool$pDFL[1] > bestDFL)) {
          bestDFL <- pool$pDFL[1]
          bestPos <- tp
          bestPlayer <- pool$Player[1]
          bestPid <- pool$playerid[1]
        }
      }

      data.frame(
        Team = tm, Status = status,
        CashLeft = tmRow$CashLeft, Needed = tmRow$Needed, DPP = tmRow$DPP,
        TargetPos = bestPos,
        Nominee = if (bestPlayer != "") fgLink(bestPlayer, bestPid) else "",
        pDFL = bestDFL,
        stringsAsFactors = FALSE
      )
    })

    result <- bind_rows(rows)
    statusOrd <- c("Strong Buy" = 1, "Lean Buy" = 2, "Neutral" = 3, "Wait" = 4, "Full" = 5)
    result %>% mutate(ord = statusOrd[Status]) %>% arrange(ord, -DPP) %>% select(-ord)
  })

  output$nomTargets <- DT::renderDataTable({
    datatable(nomTargets_r(),
              options = list(paging = FALSE, searching = FALSE, info = FALSE,
                             ordering = FALSE, autoWidth = FALSE),
              escape = FALSE) %>%
      formatRound(c('CashLeft', 'DPP'), 0) %>%
      formatCurrency('pDFL') %>%
      formatStyle('Status',
                  backgroundColor = styleEqual(
                    c('Strong Buy', 'Lean Buy', 'Neutral', 'Wait', 'Full'),
                    c('#d4edda', '#d4edda', '#fff3cd', '#f8d7da', '#e9ecef')))
  })

  # --- topHitters ---
  topHitters_r <- reactive({
    AllH_avail() %>% arrange(-pDFL) %>% head(40) %>%
      mutate(Player = fgLink(Player, playerid)) %>%
      select(Player, MLB, posEl, Age, pDFL, ADP = pADP, rankDiff, Skew = pSkew,
             HR = pHR, RBI = pRBI, R = pR, SB = pSB, AVG = pAVG, playerid)
  })

  # --- prospects (reactive to remove drafted players) ---
  prospectH_r <- reactive({
    AllH_avail() %>% inner_join(hplist, by = 'playerid') %>%
      mutate(Player = fgLink(Name, playerid)) %>%
      select(Player, MLB = Org, Pos = Pos.y, Age = Age.x, DFL = pDFL, ADP = pADP, FV, Top.100, Game = Game.Pwr, Raw = Raw.Pwr, Spd, playerid) %>%
      arrange(desc(FV))
  })

  prospectP_r <- reactive({
    AllP_avail() %>% inner_join(pplist, by = 'playerid') %>%
      mutate(Player = fgLink(Name, playerid)) %>%
      select(Player, MLB = Org, Age = Age.x, DFL = pDFL, ADP = pADP, FV, Top.100, FB, SL, CB, CH, CMD, Sits, Tops, playerid) %>%
      arrange(desc(FV))
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
    res <- mutate(res, Player = fgLink(Player, playerid))
    select(res, Player, MLB, posEl, Age, DFL, RPV, SGP, orank, ADP = pADP, rankDiff, Skew = pSkew,
           HR = pHR, RBI = pRBI, R = pR, SB = pSB, AVG = pAVG, Injury, Expected.Return, playerid)
  }

  pitPlayersbyPos <- function(pos) {
    ap <- AllP_avail()
    res <- ap %>% filter(Pos == pos, pSGP > 0) %>%
      arrange(-pDFL, -pSGP) %>% dplyr::rename(DFL = pDFL, SGP = pSGP) %>% head(200)
    res <- mutate(res, RPV = (SGP - aRPV(res, nrow(filter(res, DFL > 0)))) /
                    aRPV(res, nrow(filter(res, DFL > 0))))
    res <- mutate(res, Player = fgLink(Player, playerid))
    select(res, Player, MLB, Age, DFL, RPV, SGP, orank, ADP = pADP, rankDiff, Skew = pSkew,
           W = pW, SO = pSO, ERA = pERA, SV = pSV, HLD = pHLD, Injury, Expected.Return, playerid)
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
    hMatch <- AllH_active() %>% filter(playerid == pid)
    pMatch <- AllP_active() %>% filter(playerid == pid)

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

    maxOrder <- max(c(0, rv$roster$DraftOrder), na.rm = TRUE)
    newRow <- data.frame(
      Player = playerName,
      Pos = pos,
      Team = team,
      Salary = salary,
      Contract = 1,
      MLB = mlb,
      playerid = as.character(pid),
      orank = NA,
      DraftOrder = maxOrder + 1,
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

    # Recompute DraftOrder in case roster changed since modal was shown
    maxOrder <- max(c(0, rv$roster$DraftOrder), na.rm = TRUE)
    newRow$DraftOrder <- maxOrder + 1

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
    allChoices <- allChoices[order(names(allChoices))]

    updateSelectizeInput(session, 'playerSearch',
                         choices = allChoices,
                         selected = character(0),
                         server = TRUE)
  })

  # Team dropdowns
  updateSelectInput(session, 'draftTeam', choices = teams)
  updateSelectInput(session, 'myTeam', choices = teams, selected = 'Liquor Crickets')

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


  # Position selector
  updateSelectizeInput(session, 'e2', choices = hpos, selected = 'OF')
  output$hpos <- renderText({ input$e2 })

  output$hpbpos <- DT::renderDataTable({
    req(input$e2)
    data <- markTargets(hitPlayersbyPos(input$e2), rv$targets)
    tierColors <- c('#e8f4fd', '#edf7ee', '#fef9e7', '#f5f5f5')
    data$rowBg <- case_when(
      data$DFL >= 30 ~ '#e8f4fd',
      data$DFL >= 15 ~ '#edf7ee',
      data$DFL >= 5  ~ '#fef9e7',
      data$DFL >= 1  ~ '#f5f5f5',
      TRUE ~ ''
    )
    data <- data %>% select(-playerid, -isTarget)
    visibleCols <- setdiff(names(data), 'rowBg')

    datatable(data, selection = 'single', rownames = FALSE,
              options = list(pageLength = 20, autoWidth = FALSE,
                             searching = FALSE, info = FALSE,
                             columnDefs = list(list(visible = FALSE, targets = ncol(data) - 1))),
              filter = 'top', escape = FALSE) %>%
      formatRound(c('Age','ADP','rankDiff','HR','RBI','R','SB'), 0) %>%
      formatCurrency('DFL') %>%
      formatRound(c('RPV','SGP','Skew','AVG'), 3) %>%
      formatStyle(visibleCols, valueColumns = 'rowBg',
                  backgroundColor = styleEqual(tierColors, tierColors))
  })

  # Pitcher role selector
  updateSelectizeInput(session, 'e3', choices = ppos, selected = 'SP')
  output$ppos <- renderText({ input$e3 })

  output$ppbpos <- DT::renderDataTable({
    req(input$e3)
    data <- markTargets(pitPlayersbyPos(input$e3), rv$targets)
    tierColors <- c('#e8f4fd', '#edf7ee', '#fef9e7', '#f5f5f5')
    data$rowBg <- case_when(
      data$DFL >= 30 ~ '#e8f4fd',
      data$DFL >= 15 ~ '#edf7ee',
      data$DFL >= 5  ~ '#fef9e7',
      data$DFL >= 1  ~ '#f5f5f5',
      TRUE ~ ''
    )
    data <- data %>% select(-playerid, -isTarget)
    visibleCols <- setdiff(names(data), 'rowBg')

    datatable(data, selection = 'single', rownames = FALSE,
              options = list(pageLength = 20, autoWidth = FALSE,
                             info = FALSE,
                             columnDefs = list(list(visible = FALSE, targets = ncol(data) - 1))),
              filter = 'top', escape = FALSE) %>%
      formatRound(c('Age','ADP','rankDiff','W','SV','HLD','SO'), 0) %>%
      formatCurrency('DFL') %>%
      formatRound(c('RPV','SGP','Skew','ERA'), 3) %>%
      formatStyle(visibleCols, valueColumns = 'rowBg',
                  backgroundColor = styleEqual(tierColors, tierColors))
  })


  # Protect by position — new intelligence view
  updateSelectizeInput(session, 'e4', choices = allpos, selected = 'OF')

  output$posNeedHeader <- renderText({
    req(input$e4)
    paste("Teams That Need", input$e4)
  })

  output$posSummaryCard <- renderUI({
    req(input$e4)
    pos <- input$e4
    pc <- protClean_r()
    atPos <- pc %>% filter(Pos == pos)
    nProtected <- nrow(atPos)
    nTeamsNeed <- nrow(posNeed_r())
    avgSal <- if (nProtected > 0) round(mean(atPos$Salary, na.rm = TRUE)) else 0
    avgVal <- if (nProtected > 0) round(mean(atPos$pDFL, na.rm = TRUE)) else 0

    tags$div(style = "margin-top:15px; background:#f8f9fa; border-radius:6px; padding:12px;",
      tags$div(style = "font-weight:bold; margin-bottom:8px; border-bottom:1px solid #dee2e6; padding-bottom:6px;",
               "Position Summary"),
      tags$table(class = "table table-condensed", style = "margin-bottom:0; font-size:12px;",
        tags$tr(tags$td("Protected"), tags$td(style = "text-align:right; font-weight:bold;", nProtected)),
        tags$tr(tags$td("Teams Need"), tags$td(style = "text-align:right; font-weight:bold; color:#e74c3c;", nTeamsNeed)),
        tags$tr(tags$td("Avg Salary"), tags$td(style = "text-align:right; font-weight:bold;", paste0("$", avgSal))),
        tags$tr(tags$td("Avg Value"), tags$td(style = "text-align:right; font-weight:bold;", paste0("$", avgVal)))
      )
    )
  })

  output$posNeedTable <- DT::renderDataTable({
    req(input$e4)
    data <- posNeed_r()
    datatable(data, escape = FALSE, rownames = FALSE,
              options = list(pageLength = 20, autoWidth = FALSE,
                             paging = FALSE, searching = FALSE, info = FALSE,
                             ordering = FALSE)) %>%
      formatRound('$/Player', 0) %>%
      formatStyle('Market',
                  backgroundColor = styleEqual(
                    c('Strong Buy', 'Lean Buy', 'Neutral', 'Wait', 'Full'),
                    c('#d4edda', '#d4edda', '#fff3cd', '#f8d7da', '#e9ecef')))
  })

  # Static outputs that don't change with drafting
  output$rrcResults <- DT::renderDataTable({
    data <- rrcResults %>% mutate(Player = fgLink(Player, playerid))
    data <- markTargets(data, rv$targets)
    tRows <- which(data$isTarget == 1)
    data <- data %>% select(-playerid, -isTarget)
    dt <- datatable(data, selection = 'single',
              options = list(pageLength = 20, autoWidth = FALSE,
                             info = FALSE), filter = 'top', escape = FALSE) %>%
      formatRound(c('pADP','pW','pSV','pHLD','pSO'), 0) %>%
      formatCurrency('pDFL') %>%
      formatRound(c('pSGP','pERA','pK/9','pBB/9'), 3)
    if (length(tRows) > 0) dt <- dt %>% formatStyle(1, target = 'row', backgroundColor = styleRow(tRows, '#fff9c4'))
    dt
  })

  output$injOrig <- DT::renderDataTable({
    data <- injOrig_r()
    data$playerid <- as.character(data$playerid)
    data <- markTargets(data, rv$targets)
    tRows <- which(data$isTarget == 1)
    data <- data %>% select(-playerid, -isTarget)
    dt <- datatable(data, selection = 'single',
              options = list(pageLength = 20, autoWidth = FALSE,
                             info = FALSE), filter = 'top') %>%
      formatCurrency('pDFL')
    if (length(tRows) > 0) dt <- dt %>% formatStyle(1, target = 'row', backgroundColor = styleRow(tRows, '#fff9c4'))
    dt
  })

  output$topHitters <- DT::renderDataTable({
    data <- markTargets(topHitters_r(), rv$targets)
    tRows <- which(data$isTarget == 1)
    data <- data %>% select(-playerid, -isTarget)
    dt <- datatable(data,
              options = list(pageLength = 20, autoWidth = FALSE,
                             searching = FALSE, info = FALSE),
              escape = FALSE) %>%
      formatRound(c('Age','ADP','rankDiff','HR','RBI','R','SB'), 0) %>%
      formatCurrency('pDFL') %>%
      formatRound(c('Skew','AVG'), 3)
    if (length(tRows) > 0) dt <- dt %>% formatStyle(1, target = 'row', backgroundColor = styleRow(tRows, '#fff9c4'))
    dt
  })

  output$prospectH <- DT::renderDataTable({
    data <- markTargets(prospectH_r(), rv$targets)
    tRows <- which(data$isTarget == 1)
    data <- data %>% select(-playerid, -isTarget)
    dt <- datatable(data, selection = 'single',
              options = list(pageLength = 20, autoWidth = FALSE,
                             searching = FALSE, info = FALSE),
              filter = 'top', escape = FALSE) %>%
      formatRound(c('Age','ADP'), 0) %>% formatCurrency('DFL')
    if (length(tRows) > 0) dt <- dt %>% formatStyle(1, target = 'row', backgroundColor = styleRow(tRows, '#fff9c4'))
    dt
  })

  # ============================
  # Leaderboards tab
  # ============================
  leaderboards <- reactiveValues(hitters = NULL, pitchers = NULL)

  observeEvent(input$fetchLeaders, {
    startDate <- format(input$lbStartDate, "%Y-%m-%d")
    endDate <- format(input$lbEndDate, "%Y-%m-%d")

    showNotification("Fetching hitter leaderboard...", type = "message", duration = 3)
    leaderboards$hitters <- tryCatch(
      getHitterLeaders(startDate, endDate),
      error = function(e) { showNotification(paste("Hitter error:", e$message), type = "error"); NULL }
    )

    showNotification("Fetching pitcher leaderboard...", type = "message", duration = 3)
    leaderboards$pitchers <- tryCatch(
      getPitcherLeaders(startDate, endDate),
      error = function(e) { showNotification(paste("Pitcher error:", e$message), type = "error"); NULL }
    )

    if (!is.null(leaderboards$hitters) && !is.null(leaderboards$pitchers)) {
      showNotification("Leaderboards loaded!", type = "message")
    }
  })

  leaderH_avail <- reactive({
    req(leaderboards$hitters)
    roster <- rv$roster
    h <- leaderboards$hitters
    h$playerid <- as.character(h$playerid)
    # Remove rostered players
    h <- anti_join(h, roster, by = "playerid")
    # Add pDFL from projection pool
    pDFL_lookup <- AllH_active() %>% select(playerid, pDFL) %>% distinct()
    h <- left_join(h, pDFL_lookup, by = "playerid")
    h$pDFL <- replace_na(h$pDFL, 0)
    h %>% arrange(-pDFL)
  })

  leaderP_avail <- reactive({
    req(leaderboards$pitchers)
    roster <- rv$roster
    p <- leaderboards$pitchers
    p$playerid <- as.character(p$playerid)
    # Remove rostered players
    p <- anti_join(p, roster, by = "playerid")
    # Add pDFL from projection pool
    pDFL_lookup <- AllP_active() %>% select(playerid, pDFL) %>% distinct()
    p <- left_join(p, pDFL_lookup, by = "playerid")
    p$pDFL <- replace_na(p$pDFL, 0)
    p %>% arrange(-pDFL)
  })

  output$leaderH <- DT::renderDataTable({
    h <- leaderH_avail() %>% mutate(Player = fgLink(Player, playerid))
    h <- markTargets(h, rv$targets)
    tRows <- which(h$isTarget == 1)
    h <- h %>% select(-playerid, -isTarget)
    dt <- datatable(h,
              options = list(pageLength = 25, autoWidth = FALSE, info = FALSE),
              filter = 'top', escape = FALSE) %>%
      formatCurrency('pDFL') %>%
      formatRound(c('AVG','OBP','SLG','OPS','wOBA'), 3) %>%
      formatRound(c('wRC+','WAR'), 1)
    if (length(tRows) > 0) dt <- dt %>% formatStyle(1, target = 'row', backgroundColor = styleRow(tRows, '#fff9c4'))
    dt
  })

  output$leaderP <- DT::renderDataTable({
    p <- leaderP_avail() %>% mutate(Player = fgLink(Player, playerid))
    p <- markTargets(p, rv$targets)
    tRows <- which(p$isTarget == 1)
    p <- p %>% select(-playerid, -isTarget)
    dt <- datatable(p,
              options = list(pageLength = 25, autoWidth = FALSE, info = FALSE),
              filter = 'top', escape = FALSE) %>%
      formatCurrency('pDFL') %>%
      formatRound(c('ERA','WHIP','BABIP','FIP','xFIP','K/9','BB/9'), 2) %>%
      formatRound('WAR', 1)
    if (length(tRows) > 0) dt <- dt %>% formatStyle(1, target = 'row', backgroundColor = styleRow(tRows, '#fff9c4'))
    dt
  })

  output$prospectP <- DT::renderDataTable({
    data <- markTargets(prospectP_r(), rv$targets)
    tRows <- which(data$isTarget == 1)
    data <- data %>% select(-playerid, -isTarget)
    dt <- datatable(data, selection = 'single',
              options = list(pageLength = 20, autoWidth = FALSE,
                             searching = FALSE, info = FALSE),
              filter = 'top', escape = FALSE) %>%
      formatRound(c('Age','ADP'), 0) %>% formatCurrency('DFL')
    if (length(tRows) > 0) dt <- dt %>% formatStyle(1, target = 'row', backgroundColor = styleRow(tRows, '#fff9c4'))
    dt
  })

  # ============================
  # Rosters tab
  # ============================
  updateSelectizeInput(session, 'rosterTeam', choices = teams, selected = 'Liquor Crickets')

  teamRoster_r <- reactive({
    req(input$rosterTeam)
    roster <- rv$roster %>% filter(Team == input$rosterTeam)
    roster$playerid <- as.character(roster$playerid)

    # Lookup projection data (Age, pDFL)
    allLookup <- bind_rows(
      AllH_active() %>% select(playerid, Age, pDFL, posEl) %>% mutate(playerid = as.character(playerid)),
      AllP_active() %>% select(playerid, Age, pDFL) %>% mutate(playerid = as.character(playerid), posEl = NA_character_)
    ) %>% distinct(playerid, .keep_all = TRUE)

    roster <- left_join(roster, allLookup, by = "playerid")
    roster$pDFL <- replace_na(roster$pDFL, 0)
    roster$Value <- roster$pDFL - roster$Salary

    hitters <- roster %>% filter(!isPitcherPos(Pos)) %>% arrange(-pDFL)
    pitchers <- roster %>% filter(isPitcherPos(Pos)) %>% arrange(-pDFL)

    buildTeamRoster(hitters, pitchers)
  })

  output$rosterTeamTitle <- renderText({ input$rosterTeam })

  # Positional strength: league-wide average pDFL per roster slot
  posStrColors <- c("#b7e4c7", "#d4edda", "#fff3cd", "#f8d7da")

  slotAvgs_r <- reactive({
    roster <- rv$roster
    roster$playerid <- as.character(roster$playerid)
    allLookup <- bind_rows(
      AllH_active() %>% select(playerid, Age, pDFL, posEl) %>% mutate(playerid = as.character(playerid)),
      AllP_active() %>% select(playerid, Age, pDFL) %>% mutate(playerid = as.character(playerid), posEl = NA_character_)
    ) %>% distinct(playerid, .keep_all = TRUE)

    allSlots <- list()
    for (tm in teams) {
      tmRoster <- roster %>% filter(Team == tm)
      tmRoster <- left_join(tmRoster, allLookup, by = "playerid")
      tmRoster$pDFL <- replace_na(tmRoster$pDFL, 0)
      tmRoster$Value <- tmRoster$pDFL - tmRoster$Salary
      hitters <- tmRoster %>% filter(!isPitcherPos(Pos)) %>% arrange(-pDFL)
      pitchers <- tmRoster %>% filter(isPitcherPos(Pos)) %>% arrange(-pDFL)
      built <- buildTeamRoster(hitters, pitchers)
      allSlots[[length(allSlots) + 1]] <- bind_rows(built$hitters, built$pitchers)
    }
    bind_rows(allSlots) %>%
      filter(Player != "") %>%
      group_by(Slot) %>%
      summarise(slotAvg = mean(pDFL, na.rm = TRUE), .groups = 'drop')
  })

  output$rosterH <- DT::renderDataTable({
    tr <- teamRoster_r()
    hDisplay <- applyBudgets(tr$hitters, input$rosterTeam, rv$budgets)
    # Track which rows are empty (for styling budget cells)
    emptyRows <- which(hDisplay$Player == "")

    # Positional strength heatmap: compare player pDFL to league avg at same slot
    hDisplay <- left_join(hDisplay, slotAvgs_r(), by = "Slot")
    hDisplay$slotBg <- case_when(
      hDisplay$Player == "" | is.na(hDisplay$slotAvg) | hDisplay$slotAvg <= 0 ~ "",
      hDisplay$pDFL / hDisplay$slotAvg >= 1.2 ~ posStrColors[1],
      hDisplay$pDFL / hDisplay$slotAvg >= 0.9 ~ posStrColors[2],
      hDisplay$pDFL / hDisplay$slotAvg >= 0.7 ~ posStrColors[3],
      TRUE ~ posStrColors[4]
    )

    dt <- datatable(
      hDisplay %>% mutate(Player = fgLink(Player, playerid)) %>% select(-playerid, -slotAvg),
      rownames = FALSE,
      editable = list(target = 'cell', disable = list(columns = c(0,1,2,3,4,5,7,8,9))),
      options = list(paging = FALSE, searching = FALSE, info = FALSE,
                     ordering = FALSE, autoWidth = FALSE,
                     columnDefs = list(list(visible = FALSE, targets = 9))),
      escape = FALSE) %>%
      formatCurrency(c('Salary', 'pDFL', 'Value')) %>%
      formatRound('Age', 0) %>%
      formatRound('Yr', 0) %>%
      formatStyle('Value', color = styleInterval(0, c('#e74c3c', '#2ecc71'))) %>%
      formatStyle('Player', target = 'row',
                  backgroundColor = styleEqual("", "#f5f5f5")) %>%
      formatStyle('Slot', valueColumns = 'slotBg',
                  backgroundColor = styleEqual(posStrColors, posStrColors))
    # Style budget cells in empty rows with italic + muted color
    if (length(emptyRows) > 0) {
      dt <- dt %>% formatStyle('Salary', target = 'cell',
                               color = styleRow(emptyRows, '#999999'),
                               fontStyle = styleRow(emptyRows, 'italic'))
    }
    dt
  })

  # Helper: save a budget entry (called directly or after confirmation)
  saveBudgetEntry <- function(team, slot, newVal) {
    budgets <- rv$budgets
    budgets <- budgets %>% filter(!(Team == team & Slot == slot))
    if (newVal > 0) {
      budgets <- bind_rows(budgets, data.frame(Team = team, Slot = slot, Budget = newVal, stringsAsFactors = FALSE))
    }
    rv$budgets <- budgets
    write.csv(budgets, budgetFile, row.names = FALSE)
  }

  # Helper: check budget and either save or prompt for confirmation
  checkAndSaveBudget <- function(team, slot, newVal, side) {
    tr <- teamRoster_r()
    if (side == "H") {
      actualSpend <- sum(tr$hitters$Salary[tr$hitters$Player != ""], na.rm = TRUE)
      sideBudget <- round(cap * (1 - hpratio))
    } else {
      actualSpend <- sum(tr$pitchers$Salary[tr$pitchers$Player != ""], na.rm = TRUE)
      sideBudget <- round(cap * hpratio)
    }
    # Sum existing budget entries for this team/side, excluding the slot being edited
    existingBudgets <- rv$budgets %>% filter(Team == team, Slot != slot)
    if (side == "H") {
      hSlotNames <- tr$hitters$Slot
      existingBudgets <- existingBudgets %>% filter(Slot %in% hSlotNames)
    } else {
      pSlotNames <- tr$pitchers$Slot
      existingBudgets <- existingBudgets %>% filter(Slot %in% pSlotNames)
    }
    budgetAllocated <- sum(existingBudgets$Budget, na.rm = TRUE)
    totalPlanned <- actualSpend + budgetAllocated + newVal
    remaining <- sideBudget - actualSpend - budgetAllocated

    if (newVal > 0 && totalPlanned > sideBudget) {
      sideLabel <- if (side == "H") "Hitting" else "Pitching"
      rv$pendingBudgetEdit <- list(team = team, slot = slot, newVal = newVal)
      showModal(modalDialog(
        title = "Over Budget",
        tags$p(paste0("This would put your ", sideLabel, " budget allocations at $",
                       totalPlanned, " / $", sideBudget, ".")),
        tags$p(paste0("You have $", remaining, " remaining but are entering $", newVal, ".")),
        tags$p("Accept anyway?"),
        footer = tagList(
          actionButton("budgetOverrideYes", "Yes, accept", class = "btn-warning"),
          modalButton("Cancel")
        )
      ))
    } else {
      saveBudgetEntry(team, slot, newVal)
    }
  }

  # Confirm over-budget entry
  observeEvent(input$budgetOverrideYes, {
    pending <- rv$pendingBudgetEdit
    if (!is.null(pending)) {
      saveBudgetEntry(pending$team, pending$slot, pending$newVal)
      rv$pendingBudgetEdit <- NULL
    }
    removeModal()
  })

  observeEvent(input$clearBudget, {
    team <- input$rosterTeam
    tr <- teamRoster_r()
    allSlots <- c(tr$hitters$Slot, tr$pitchers$Slot)
    # Remove all budget entries for this team
    budgets <- rv$budgets %>% filter(!(Team == team & Slot %in% allSlots))
    # Set every empty slot to $1
    emptyH <- tr$hitters %>% filter(Player == "")
    emptyP <- tr$pitchers %>% filter(Player == "")
    for (sl in c(emptyH$Slot, emptyP$Slot)) {
      budgets <- bind_rows(budgets, data.frame(Team = team, Slot = sl, Budget = 1, stringsAsFactors = FALSE))
    }
    rv$budgets <- budgets
    write.csv(budgets, budgetFile, row.names = FALSE)
  })

  observeEvent(input$rosterH_cell_edit, {
    info <- input$rosterH_cell_edit
    tr <- teamRoster_r()
    # DT uses 0-based column indices; Salary is column 6 (0-based)
    if (info$col == 6 && tr$hitters$Player[info$row] == "") {
      newVal <- suppressWarnings(as.numeric(info$value))
      if (!is.na(newVal)) {
        checkAndSaveBudget(input$rosterTeam, tr$hitters$Slot[info$row], newVal, "H")
      }
    }
  })

  output$rosterP <- DT::renderDataTable({
    tr <- teamRoster_r()
    pDisplay <- applyBudgets(tr$pitchers, input$rosterTeam, rv$budgets)
    emptyRows <- which(pDisplay$Player == "")

    # Positional strength heatmap: compare player pDFL to league avg at same slot
    pDisplay <- left_join(pDisplay, slotAvgs_r(), by = "Slot")
    pDisplay$slotBg <- case_when(
      pDisplay$Player == "" | is.na(pDisplay$slotAvg) | pDisplay$slotAvg <= 0 ~ "",
      pDisplay$pDFL / pDisplay$slotAvg >= 1.2 ~ posStrColors[1],
      pDisplay$pDFL / pDisplay$slotAvg >= 0.9 ~ posStrColors[2],
      pDisplay$pDFL / pDisplay$slotAvg >= 0.7 ~ posStrColors[3],
      TRUE ~ posStrColors[4]
    )

    dt <- datatable(
      pDisplay %>% mutate(Player = fgLink(Player, playerid)) %>% select(-playerid, -slotAvg),
      rownames = FALSE,
      editable = list(target = 'cell', disable = list(columns = c(0,1,2,3,4,5,7,8,9))),
      options = list(paging = FALSE, searching = FALSE, info = FALSE,
                     ordering = FALSE, autoWidth = FALSE,
                     columnDefs = list(list(visible = FALSE, targets = 9))),
      escape = FALSE) %>%
      formatCurrency(c('Salary', 'pDFL', 'Value')) %>%
      formatRound('Age', 0) %>%
      formatRound('Yr', 0) %>%
      formatStyle('Value', color = styleInterval(0, c('#e74c3c', '#2ecc71'))) %>%
      formatStyle('Player', target = 'row',
                  backgroundColor = styleEqual("", "#f5f5f5")) %>%
      formatStyle('Slot', valueColumns = 'slotBg',
                  backgroundColor = styleEqual(posStrColors, posStrColors))
    if (length(emptyRows) > 0) {
      dt <- dt %>% formatStyle('Salary', target = 'cell',
                               color = styleRow(emptyRows, '#999999'),
                               fontStyle = styleRow(emptyRows, 'italic'))
    }
    dt
  })

  observeEvent(input$rosterP_cell_edit, {
    info <- input$rosterP_cell_edit
    tr <- teamRoster_r()
    if (info$col == 6 && tr$pitchers$Player[info$row] == "") {
      newVal <- suppressWarnings(as.numeric(info$value))
      if (!is.na(newVal)) {
        checkAndSaveBudget(input$rosterTeam, tr$pitchers$Slot[info$row], newVal, "P")
      }
    }
  })

  output$spiderChart <- renderPlot({
    req(input$rosterTeam)
    tr <- teamRoster_r()
    hStarterIds <- tr$hitters %>% filter(!grepl("^BN", Slot)) %>% pull(playerid)
    pStarterIds <- tr$pitchers %>% filter(Slot %in% c("SP1","SP2","SP3","SP4","SP5","MR1","CL1","CL2")) %>% pull(playerid)
    rh <- rhitters_r() %>% filter(playerid %in% hStarterIds)
    rp <- rpitchers_r() %>% filter(playerid %in% pStarterIds)
    goals <- calcGoals(rp, rh, targets, input$rosterTeam)

    # Cap display at 150% so one outlier doesn't flatten everything
    goals$pct <- pmin(goals$pc, 1.5)
    maxVal <- max(1.2, max(goals$pct, na.rm = TRUE))

    n <- nrow(goals)
    angles <- seq(0, 2 * pi, length.out = n + 1)[1:n] - pi / 2  # first point at top

    # Cartesian coordinates for data polygon
    goals$x <- goals$pct * cos(angles)
    goals$y <- goals$pct * sin(angles)
    poly_df <- rbind(goals, goals[1, ])

    # Reference circle helper
    circle_df <- function(r, npts = 100) {
      theta <- seq(0, 2 * pi, length.out = npts)
      data.frame(x = r * cos(theta), y = r * sin(theta))
    }

    # Spoke endpoints
    spokes <- data.frame(xend = maxVal * cos(angles), yend = maxVal * sin(angles))

    # Category labels positioned outside
    labelR <- maxVal + 0.15
    labels_df <- data.frame(
      x = labelR * cos(angles), y = labelR * sin(angles),
      label = as.character(goals$statistic)
    )

    # Pct labels near each point
    pctR <- goals$pct + maxVal * 0.1
    pct_df <- data.frame(
      x = pctR * cos(angles), y = pctR * sin(angles),
      label = paste0(round(goals$pc * 100), "%")
    )

    ggplot() +
      # Reference circles
      geom_path(data = circle_df(0.5), aes(x, y), color = "gray88", linetype = "dotted", linewidth = 0.3) +
      geom_path(data = circle_df(0.75), aes(x, y), color = "gray83", linetype = "dotted", linewidth = 0.3) +
      geom_path(data = circle_df(1.0), aes(x, y), color = "gray75", linetype = "dashed", linewidth = 0.4) +
      # Spokes
      geom_segment(data = spokes, aes(x = 0, y = 0, xend = xend, yend = yend),
                   color = "gray90", linewidth = 0.3) +
      # Filled data polygon
      geom_polygon(data = poly_df, aes(x, y),
                   fill = "#3498db", alpha = 0.25, color = "#2980b9", linewidth = 1.2) +
      # Points
      geom_point(data = goals, aes(x, y, color = pct >= 1.0), size = 3) +
      scale_color_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"), guide = "none") +
      # Pct labels
      geom_text(data = pct_df, aes(x, y, label = label), size = 2.8, fontface = "bold") +
      # Category labels
      geom_text(data = labels_df, aes(x, y, label = label), size = 3.5) +
      coord_equal(clip = "off") +
      theme_void() +
      theme(plot.margin = margin(10, 10, 10, 10))
  }, height = 280, bg = "transparent")

  output$rosterGoals <- DT::renderDataTable({
    req(input$rosterTeam)
    tr <- teamRoster_r()
    # Only count starters toward goals
    hStarterIds <- tr$hitters %>% filter(!grepl("^BN", Slot)) %>% pull(playerid)
    pStarterIds <- tr$pitchers %>% filter(Slot %in% c("SP1","SP2","SP3","SP4","SP5","MR1","CL1","CL2")) %>% pull(playerid)
    rh <- rhitters_r() %>% filter(playerid %in% hStarterIds)
    rp <- rpitchers_r() %>% filter(playerid %in% pStarterIds)
    datatable(calcGoals(rp, rh, targets, input$rosterTeam),
              options = list(paging = FALSE, searching = FALSE, info = FALSE,
                             ordering = FALSE, autoWidth = FALSE),
              rownames = FALSE) %>%
      formatPercentage('pc', 2) %>%
      formatRound(c('collected', 'needed'), 0)
  })

  output$rosterBudget <- renderUI({
    tr <- teamRoster_r()
    hSpend <- sum(tr$hitters$Salary, na.rm = TRUE)
    pSpend <- sum(tr$pitchers$Salary, na.rm = TRUE)
    total <- hSpend + pSpend
    hBudget <- round(cap * (1 - hpratio))
    pBudget <- round(cap * hpratio)
    hFilled <- sum(tr$hitters$Player != "", na.rm = TRUE)
    pFilled <- sum(tr$pitchers$Player != "", na.rm = TRUE)

    tags$div(style = "margin-top:15px;",
      tags$h4("Budget"),
      tags$table(class = "table table-condensed table-bordered",
        tags$tr(tags$td("Hitting"), tags$td(style = "text-align:right;", paste0("$", hSpend, " / $", hBudget))),
        tags$tr(tags$td("Pitching"), tags$td(style = "text-align:right;", paste0("$", pSpend, " / $", pBudget))),
        tags$tr(tags$td(tags$strong("Total")), tags$td(style = "text-align:right;", tags$strong(paste0("$", total, " / $", cap)))),
        tags$tr(tags$td("Remaining"), tags$td(style = "text-align:right;", paste0("$", cap - total, " ($", hBudget - hSpend, "/$", pBudget - pSpend, ")"))),
        tags$tr(tags$td(tags$strong("Max Bid")), tags$td(style = "text-align:right;", tags$strong(paste0("$", cap - total - (25 - hFilled - pFilled - 1)))))
      ),
      tags$h4("Roster"),
      tags$table(class = "table table-condensed table-bordered",
        tags$tr(tags$td("Hitters"), tags$td(style = "text-align:right;", paste0(hFilled, " / ", nhitters))),
        tags$tr(tags$td("Pitchers"), tags$td(style = "text-align:right;", paste0(pFilled, " / ", npitchers))),
        tags$tr(tags$td(tags$strong("Total")), tags$td(style = "text-align:right;", tags$strong(paste0(hFilled + pFilled, " / 25"))))
      )
    )
  })

  output$budgetAllocation <- renderUI({
    tr <- teamRoster_r()
    team <- input$rosterTeam
    budgets <- rv$budgets %>% filter(Team == team)

    hBudgetCap <- round(cap * (1 - hpratio))
    pBudgetCap <- round(cap * hpratio)
    hSpend <- sum(tr$hitters$Salary[tr$hitters$Player != ""], na.rm = TRUE)
    pSpend <- sum(tr$pitchers$Salary[tr$pitchers$Player != ""], na.rm = TRUE)
    hRemaining <- hBudgetCap - hSpend
    pRemaining <- pBudgetCap - pSpend

    hAllocated <- sum(budgets$Budget[budgets$Slot %in% tr$hitters$Slot], na.rm = TRUE)
    pAllocated <- sum(budgets$Budget[budgets$Slot %in% tr$pitchers$Slot], na.rm = TRUE)
    hUnallocated <- hRemaining - hAllocated
    pUnallocated <- pRemaining - pAllocated

    colorVal <- function(v) if (v < 0) "#e74c3c" else "#2ecc71"

    tags$div(style = "text-align:right; margin-top:10px;",
      tags$table(class = "table table-condensed table-bordered", style = "width:auto; margin-left:auto; font-size:13px;",
        tags$thead(tags$tr(
          tags$th(""), tags$th("Remaining"), tags$th("Allocated"), tags$th("Unallocated")
        )),
        tags$tbody(
          tags$tr(
            tags$td(tags$strong("Hitters")),
            tags$td(style = "text-align:right;", paste0("$", hRemaining)),
            tags$td(style = "text-align:right;", paste0("$", hAllocated)),
            tags$td(style = paste0("text-align:right; font-weight:bold; color:", colorVal(hUnallocated)), paste0("$", hUnallocated))
          ),
          tags$tr(
            tags$td(tags$strong("Pitchers")),
            tags$td(style = "text-align:right;", paste0("$", pRemaining)),
            tags$td(style = "text-align:right;", paste0("$", pAllocated)),
            tags$td(style = paste0("text-align:right; font-weight:bold; color:", colorVal(pUnallocated)), paste0("$", pUnallocated))
          )
        )
      )
    )
  })

})
