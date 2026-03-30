# LiveDraftTool server.R — reactive draft tool

library(ggplot2)
library(shinyjs)

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

# Save pools per projection system
AllH_atc <- AllH_full
AllP_atc <- AllP_full
AllH_steamer <- if (!is.null(steamerPool)) steamerPool$hitters else AllH_full
AllP_steamer <- if (!is.null(steamerPool)) steamerPool$pitchers else AllP_full
AllH_batx <- if (!is.null(batxPool)) batxPool$hitters else AllH_full
AllP_batx <- if (!is.null(batxPool)) batxPool$pitchers else AllP_full

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
    },
    researchH = data.frame(),
    researchP = data.frame(),
    researchUnmatched = character(0),
    researchTitle = ""
  )

  # --- Projection source toggle ---
  projPools_r <- reactive({
    src <- input$projSource
    if (is.null(src) || src == "atc") {
      list(hitters = AllH_atc, pitchers = AllP_atc)
    } else if (src == "steamer") {
      list(hitters = AllH_steamer, pitchers = AllP_steamer)
    } else {
      list(hitters = AllH_batx, pitchers = AllP_batx)
    }
  })

  # --- Blended player pools (actual stats + projections) ---
  blendedPools_r <- reactive({
    mode <- input$valMode
    pools <- projPools_r()
    if (is.null(mode) || mode == "proj" ||
        is.null(leaderboards$hitters) || is.null(leaderboards$pitchers)) {
      return(pools)
    }

    lbH <- leaderboards$hitters
    lbP <- leaderboards$pitchers
    lbH$playerid <- as.character(lbH$playerid)
    lbP$playerid <- as.character(lbP$playerid)

    # Start with copies of selected projection pools
    newH <- pools$hitters
    newP <- pools$pitchers

    if (mode == "leaders") {
      # Leaderboard Only: replace projected stats with actual stats (0 for players not on leaderboard)
      actH <- lbH %>% select(playerid, aHR = HR, aR = R, aRBI = RBI, aSB = SB, aH = H, aAB = AB)
      newH <- left_join(newH, actH, by = "playerid")
      newH <- newH %>% mutate(
        pHR = replace_na(aHR, 0),
        pR = replace_na(aR, 0),
        pRBI = replace_na(aRBI, 0),
        pSB = replace_na(aSB, 0),
        pH = replace_na(aH, 0),
        pAB = replace_na(aAB, 0),
        pAVG = ifelse(pAB > 0, pH / pAB, 0)
      ) %>% select(-aHR, -aR, -aRBI, -aSB, -aH, -aAB)

      actP <- lbP %>% select(playerid, aW = W, aSO = SO, aHLD = HLD, aSV = SV, aIP = IP, aERA = ERA)
      newP <- left_join(newP, actP, by = "playerid")
      newP <- newP %>% mutate(
        pW = replace_na(aW, 0),
        pSO = replace_na(aSO, 0),
        pHLD = replace_na(aHLD, 0),
        pSV = replace_na(aSV, 0),
        pIP = replace_na(aIP, 0),
        pER = replace_na(aERA, 0) * replace_na(aIP, 0) / 9,
        pERA = ifelse(pIP > 0, pER / pIP * 9, 0)
      ) %>% select(-aW, -aSO, -aHLD, -aSV, -aIP, -aERA)
    } else {
      # Blended: add actual counting stats to projected
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
    }

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
    src <- input$projSource
    srcLabel <- switch(src %||% "atc", atc = "ATC", steamer = "Steamer", batx = "THE BAT X", "ATC")
    modeLabel <- if (is.null(mode) || mode == "proj") {
      "Projections"
    } else if (mode == "blend") {
      "Blended"
    } else {
      "Leaderboard Only"
    }
    needsLB <- !is.null(mode) && mode != "proj" &&
      (is.null(leaderboards$hitters) || is.null(leaderboards$pitchers))
    tagList(
      tags$div(style = "text-align:center; padding:6px; margin-top:10px; border-radius:4px; background:#f0f0f0;",
               tags$small(tags$strong(srcLabel), " | ", modeLabel)),
      if (needsLB) tags$div(style = "text-align:center; padding:4px; margin-top:4px; border-radius:4px; background:#fff3cd;",
               tags$small(style = "color:#856404;", "Fetch leaderboards first"))
    )
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

  # --- Target toggle (Top Hitters tab) ---
  observeEvent(input$targetTopHBtn, {
    sel <- input$topHitters_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      showNotification("Select a player row first", type = "warning")
      return()
    }
    data <- topHitters_r()
    pid <- as.character(data$playerid[sel])
    pName <- data$Player[sel]
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

  # --- Target toggle (Leaderboards tab) ---
  observeEvent(input$targetLBBtn, {
    tab <- input$leaderTab
    if (is.null(tab)) return()
    if (tab == 'Hitters') {
      sel <- input$leaderH_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        showNotification("Select a player row first", type = "warning")
        return()
      }
      data <- leaderH_avail()
      pid <- as.character(data$playerid[sel])
      pName <- data$Player[sel]
    } else {
      sel <- input$leaderP_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        showNotification("Select a player row first", type = "warning")
        return()
      }
      data <- leaderP_avail()
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

  # --- Search tab: find any player across all pools + roster ---
  searchData_r <- reactive({
    roster <- rv$roster
    rosterLookup <- roster %>% select(playerid, Owner = Team, Salary) %>% distinct()

    pools <- projPools_r()
    allPlayers <- bind_rows(
      pools$hitters %>% select(playerid, Player, Pos, MLB, Age, pDFL, pADP) %>% mutate(Type = "Hitter"),
      pools$pitchers %>% select(playerid, Player, Pos, MLB, Age, pDFL, pADP) %>% mutate(Type = "Pitcher")
    ) %>% distinct(playerid, .keep_all = TRUE)

    allPlayers <- left_join(allPlayers, rosterLookup, by = "playerid")
    allPlayers$Owner <- replace_na(allPlayers$Owner, "Free Agent")
    allPlayers$Salary <- replace_na(allPlayers$Salary, NA_real_)
    allPlayers %>% arrange(-pDFL)
  })

  output$searchTable <- DT::renderDataTable({
    data <- searchData_r() %>%
      mutate(Player = fgLink(Player, playerid),
             Owner = teamLink(Owner))
    data <- markTargets(data, rv$targets)
    tRows <- which(data$isTarget == 1)
    data <- data %>% select(-playerid, -isTarget)
    data <- data %>% select(Player, Type, Pos, MLB, Age, Owner, Salary, DFL = pDFL, ADP = pADP)

    dt <- datatable(data, selection = 'single',
              options = list(pageLength = 25, autoWidth = FALSE, info = FALSE),
              filter = 'top', escape = FALSE) %>%
      formatCurrency('DFL') %>%
      formatRound(c('Age', 'ADP'), 0) %>%
      formatCurrency('Salary', digits = 0)
    if (length(tRows) > 0) dt <- dt %>% formatStyle(1, target = 'row', backgroundColor = styleRow(tRows, '#fff9c4'))
    dt
  })

  observeEvent(input$targetSearchBtn, {
    sel <- input$searchTable_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      showNotification("Select a player row first", type = "warning")
      return()
    }
    data <- searchData_r()
    pid <- as.character(data$playerid[sel])
    pName <- data$Player[sel]
    if (pid %in% rv$targets) {
      rv$targets <- rv$targets[rv$targets != pid]
      showNotification(paste0("Removed target: ", pName), type = "message")
    } else {
      rv$targets <- c(rv$targets, pid)
      showNotification(paste0("Added target: ", pName), type = "message")
    }
    write.csv(data.frame(playerid = rv$targets, stringsAsFactors = FALSE), targetFile, row.names = FALSE)
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
    cols <- c('Team','Player','Contract','Salary','pDFL','pADP','pSkew','rankDiff','Age','Pos','playerid','DraftOrder')
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
    csSub <- cs %>% filter(group == csGroup) %>% select(Team, salleft, needed)
    fullBudget <- if (isHitterPos) cap * (1 - hpratio) else cap * hpratio
    fullNeeded <- if (isHitterPos) nhitters else npitchers
    need <- left_join(need, csSub, by = 'Team')
    need$CashLeft <- round(replace_na(need$salleft, fullBudget), 0)
    groupNeeded <- replace_na(need$needed, fullNeeded)
    need$MaxBid <- pmax(1, need$CashLeft - pmax(0, groupNeeded - 1))

    # Weakest stats per team
    hitterStats <- c('HR','RBI','R','SB')
    pitcherStats <- c('W','K','SV','HLD')
    relevantStats <- if (isHitterPos) hitterStats else pitcherStats

    need$WeakestStats <- sapply(need$Team, function(tm) {
      goals <- calcGoals(rp, rh, targets, tm)
      goals <- goals %>% filter(statistic %in% relevantStats, pc < 0.75) %>%
        arrange(pc) %>% head(3)
      if (nrow(goals) == 0) return('<span style="color:#2ecc71;">On track</span>')
      paste(sapply(seq_len(nrow(goals)), function(j) {
        pct <- round(goals$pc[j] * 100)
        color <- if (goals$pc[j] < 0.50) '#e74c3c' else if (goals$pc[j] < 0.65) '#f39c12' else '#c5a000'
        paste0('<span style="color:', color, ';">', goals$statistic[j], ' ', pct, '%</span>')
      }), collapse = ', ')
    })

    # Sort by market label priority, then DPP descending
    statusOrd <- c("Strong Buy" = 1, "Lean Buy" = 2, "Neutral" = 3, "Wait" = 4, "Full" = 5)
    need <- need %>%
      mutate(ord = statusOrd[Market], Team = teamLink(Team)) %>%
      arrange(ord, -DPP) %>%
      select(Team, StillNeed, Market, CashLeft, MaxBid, DPP, WeakestStats) %>%
      dplyr::rename(`Still Need` = StillNeed, `$/Player` = DPP, `Cash Left` = CashLeft,
                     `Max Bid` = MaxBid, `Weakest Stats` = WeakestStats)
    need
  })

  # --- pressure: positional scarcity indicator ---
  pressure_r <- reactive({
    pc <- protClean_r()
    ah <- AllH_avail()
    ap <- AllP_avail()
    positions <- c('C','1B','2B','SS','3B','OF','SP','MR','CL')
    hitterPositions <- c('C','1B','2B','SS','3B','OF')

    # Tier assignment
    priceTier <- function(dfl) {
      case_when(dfl >= 30 ~ "Elite", dfl >= 15 ~ "Solid", dfl >= 5 ~ "Value", TRUE ~ "Dollar")
    }

    rows <- lapply(positions, function(pos) {
      # How many does the league need?
      threshold <- ifelse(pos %in% names(posThresholds), posThresholds[pos], 1)
      posCounts <- pc %>% filter(Pos == pos) %>%
        group_by(Team) %>% summarize(have = n(), .groups = 'drop')
      allteams <- data.frame(Team = teams, stringsAsFactors = FALSE)
      leagueNeed <- left_join(allteams, posCounts, by = 'Team') %>%
        mutate(have = replace_na(have, 0)) %>%
        mutate(still = pmax(0, threshold - have)) %>%
        pull(still) %>% sum()

      if (leagueNeed <= 1) {
        return(data.frame(Pos = pos, Need = leagueNeed, Pressure = "No", stringsAsFactors = FALSE))
      }

      # Get top N available players at this position
      if (pos %in% hitterPositions) {
        pool <- ah %>% filter(Pos == pos | (!is.na(posEl) & str_detect(posEl, pos))) %>%
          arrange(-pDFL) %>% head(leagueNeed)
      } else {
        pool <- ap %>% filter(Pos == pos) %>% arrange(-pDFL) %>% head(leagueNeed)
      }

      if (nrow(pool) == 0) {
        return(data.frame(Pos = pos, Need = leagueNeed, Pressure = "High", stringsAsFactors = FALSE))
      }

      pool$tier <- priceTier(pool$pDFL)
      topTier <- pool$tier[1]
      nTopTier <- sum(pool$tier == topTier)
      nTiers <- length(unique(pool$tier))
      topVal <- pool$pDFL[1]
      botVal <- pool$pDFL[nrow(pool)]

      pressure <- if (nTopTier == 1) {
        "High"
      } else if (nTiers >= 3) {
        "Medium"
      } else if (topVal - botVal > 5) {
        "Low"
      } else {
        "No"
      }

      data.frame(Pos = pos, Need = leagueNeed, Pressure = pressure, stringsAsFactors = FALSE)
    })

    result <- bind_rows(rows)
    pressureOrd <- c("High" = 1, "Medium" = 2, "Low" = 3, "No" = 4)
    result %>% mutate(ord = pressureOrd[Pressure]) %>% arrange(ord, -Need) %>% select(-ord)
  })

  # --- FanGraphs link helper ---
  # Link team name to Rosters tab
  teamLink <- function(team) {
    ifelse(is.na(team) | team == "" | team == "Free Agent",
           team,
           paste0("<a href='#' onclick='Shiny.setInputValue(\"goToRoster\", \"",
                  htmltools::htmlEscape(team, attribute = TRUE),
                  "\", {priority: \"event\"}); return false;' style='cursor:pointer;'>",
                  htmltools::htmlEscape(team), "</a>"))
  }

  observeEvent(input$goToRoster, {
    updateNavbarPage(session, "mainNav", selected = "Rosters")
    updateSelectizeInput(session, "rosterTeam", selected = input$goToRoster)
  })

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
    df$surplus <- df$Salary - df$pDFL
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
      scale_fill_manual(values = c("TRUE" = "#e74c3c", "FALSE" = "#2ecc71"), guide = "none") +
      labs(x = "Pick #", y = "Overpay ($)") +
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
    if (currentAvg <= 0 && !trending_up) {
      label <- "Bargain Zone"; color <- "#2ecc71"
    } else if (currentAvg <= 0 && trending_up) {
      label <- "Warming Up"; color <- "#f39c12"
    } else if (currentAvg > 0 && !trending_up) {
      label <- "Cooling Off"; color <- "#f39c12"
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
    pc <- protClean_r() %>% filter(!is.na(DraftOrder))

    if (!is.null(pos) && pos != "All") {
      pc <- pc %>% filter(Pos == pos)
    }

    nDrafted <- nrow(pc)
    avgOverpay <- if (nDrafted > 0) mean(pc$Salary - pc$pDFL) else NA

    list(nDrafted = nDrafted, avgOverpay = avgOverpay)
  })

  output$inflationDisplay <- renderUI({
    d <- inflationData_r()
    if (is.na(d$avgOverpay)) return(tags$span("No data yet", style = "color:gray;"))

    avg <- d$avgOverpay
    color <- if (avg > 5) "#e74c3c" else if (avg > 0) "#f39c12" else "#2ecc71"
    sign <- if (avg > 0) "+" else ""

    tags$div(
      style = "text-align:center; padding:8px; border-radius:4px; background:#f8f9fa; margin-bottom:10px;",
      tags$strong(style = paste0("font-size:22px; color:", color, ";"),
                  paste0(sign, "$", sprintf("%.1f", avg))),
      tags$br(),
      tags$small(style = "color:gray;",
                 paste0("avg $/player vs value (", d$nDrafted, " drafted)"))
    )
  })

  # --- Pre-Draft Summary (remaining pool inflation) ---
  output$preDraftSummary <- renderUI({
    pc <- protClean_r()
    protected <- pc %>% filter(is.na(DraftOrder))
    nProtected <- nrow(protected)
    if (nProtected == 0) return(tags$span("No protected players", style = "color:gray;"))

    protectedSalary <- sum(protected$Salary, na.rm = TRUE)
    protectedValue <- sum(protected$pDFL, na.rm = TRUE)

    # Total league dollars and roster spots
    totalDollars <- cap * nteams
    totalSpots <- (nhitters + npitchers) * nteams

    # Remaining pool after protections
    remainingDollars <- totalDollars - protectedSalary
    remainingSpots <- totalSpots - nProtected

    # Value of unprotected players: total pool value minus protected value
    protectedIds <- protected$playerid
    allPlayerValue <- sum(
      AllH_active()$pDFL[AllH_active()$pDFL > 0],
      AllP_active()$pDFL[AllP_active()$pDFL > 0],
      na.rm = TRUE
    )
    remainingValue <- allPlayerValue - protectedValue

    # Inflation: how much more dollars than value in the remaining pool
    inflationRate <- if (remainingValue > 0) ((remainingDollars - remainingValue) / remainingValue) * 100 else 0

    color <- if (inflationRate > 10) "#e74c3c" else if (inflationRate > 0) "#f39c12" else "#2ecc71"
    sign <- if (inflationRate > 0) "+" else ""

    tags$div(
      style = "background:#f8f9fa; border-radius:6px; padding:12px; margin-top:10px; max-width:350px;",
      tags$table(style = "width:100%; font-size:14px;",
        tags$tr(
          tags$td("Protected Players"),
          tags$td(style = "text-align:right; font-weight:bold;", nProtected)
        ),
        tags$tr(
          tags$td("Remaining $"),
          tags$td(style = "text-align:right; font-weight:bold;", paste0("$", remainingDollars))
        ),
        tags$tr(
          tags$td("Remaining Spots"),
          tags$td(style = "text-align:right; font-weight:bold;", remainingSpots)
        ),
        tags$tr(
          tags$td("Remaining Value"),
          tags$td(style = "text-align:right; font-weight:bold;", paste0("$", round(remainingValue)))
        ),
        tags$tr(
          tags$td("Draft Inflation"),
          tags$td(style = paste0("text-align:right; font-weight:bold; font-size:18px; color:", color, ";"),
                  paste0(sign, sprintf("%.1f", inflationRate), "%"))
        )
      )
    )
  })

  # --- Actual vs Optimal Protection List Comparison ---
  output$protListComparison <- renderUI({
    if (is.null(protComparison) || is.null(protPoolComparison)) return(NULL)

    fmtVal <- function(val, isDollar) {
      if (isDollar) paste0("$", val) else as.character(val)
    }
    makeRow <- function(label, a1, o1, a2, o2, bold = FALSE, dollar = FALSE) {
      st <- if (bold) "font-weight:bold;" else ""
      tags$tr(
        tags$td(style = paste0("padding:3px 8px;", st), label),
        tags$td(style = paste0("text-align:right; padding:3px 8px;", st), fmtVal(a1, dollar)),
        tags$td(style = paste0("text-align:right; padding:3px 8px;", st), fmtVal(o1, dollar)),
        tags$td(style = paste0("text-align:right; padding:3px 8px;", st), fmtVal(a2, dollar)),
        tags$td(style = paste0("text-align:right; padding:3px 8px;", st), fmtVal(o2, dollar))
      )
    }

    # Protection summary rows
    pc <- protComparison
    dollarMetrics <- c("Salary", "Value", "Surplus", "Avg $/Player")
    protRows <- lapply(1:nrow(pc), function(i) {
      isBold <- pc$Metric[i] == "Surplus"
      isDollar <- pc$Metric[i] %in% dollarMetrics
      makeRow(pc$Metric[i], pc$Actual_Hitters[i], pc$Optimal_Hitters[i],
              pc$Actual_Pitchers[i], pc$Optimal_Pitchers[i], bold = isBold, dollar = isDollar)
    })

    # Draft pool rows
    pp <- protPoolComparison
    poolDollarMetrics <- c("Remaining $", "$/Spot")
    poolRows <- lapply(1:nrow(pp), function(i) {
      isDollar <- pp$Metric[i] %in% poolDollarMetrics
      makeRow(pp$Metric[i], pp$Actual_Hitters[i], pp$Optimal_Hitters[i],
              pp$Actual_Pitchers[i], pp$Optimal_Pitchers[i], dollar = isDollar)
    })

    tags$div(
      style = "background:#f8f9fa; border-radius:6px; padding:12px; margin-top:14px; max-width:600px;",
      tags$strong(style = "font-size:15px;", "Actual vs Optimal Protections"),
      tags$table(style = "width:100%; font-size:13px; margin-top:8px; border-collapse:collapse;",
        # Header
        tags$tr(style = "border-bottom:2px solid #ddd;",
          tags$th(style = "padding:3px 8px;", ""),
          tags$th(style = "text-align:right; padding:3px 8px;", "Actual H"),
          tags$th(style = "text-align:right; padding:3px 8px;", "Optimal H"),
          tags$th(style = "text-align:right; padding:3px 8px;", "Actual P"),
          tags$th(style = "text-align:right; padding:3px 8px;", "Optimal P")
        ),
        # Protected players section
        tags$tr(tags$td(colspan = "5", style = "padding:6px 0 2px; font-weight:bold; font-size:12px; color:#666;",
                        "Protected")),
        protRows,
        # Draft pool section
        tags$tr(tags$td(colspan = "5", style = "padding:8px 0 2px; font-weight:bold; font-size:12px; color:#666; border-top:1px solid #ddd;",
                        "Draft Pool")),
        poolRows
      )
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

  # --- Nomination Strategy Card ---
  output$nomStrategyCard <- renderUI({
    req(input$nomTeam)
    myTeam <- input$nomTeam
    ps <- pstandings_r()
    me <- ps %>% filter(Team == myTeam)
    if (nrow(me) == 0 || me$Needed <= 0) return(NULL)

    # My market status
    others <- ps %>% filter(Team != myTeam, Needed > 0)
    leagueAvgDPP <- if (sum(others$Needed) > 0) sum(others$CashLeft) / sum(others$Needed) else 0
    ratio <- if (leagueAvgDPP > 0 && me$Needed > 0) me$DPP / leagueAvgDPP else NA

    # Positions I still need
    pc <- protClean_r()
    allPositions <- c('C','1B','2B','SS','3B','OF','SP','MR','CL')
    posCounts <- pc %>% filter(Team == myTeam) %>%
      group_by(Pos) %>% summarize(have = n(), .groups = 'drop')
    neededPositions <- sapply(allPositions, function(p) {
      threshold <- ifelse(p %in% names(posThresholds), posThresholds[p], 1)
      have <- sum(posCounts$Pos == p)
      threshold - have > 0
    })
    neededPositions <- allPositions[neededPositions]

    # Pressure on my needed positions
    press <- pressure_r()
    myPressure <- press %>% filter(Pos %in% neededPositions)

    # Budget per position: aggregate slot budgets (MR1+MR2 → MR total)
    myBudgets <- rv$budgets %>% filter(Team == myTeam)
    posBudget <- sapply(neededPositions, function(pos) {
      matchSlots <- myBudgets %>% filter(grepl(paste0("^", pos), Slot))
      sum(matchSlots$Budget, na.rm = TRUE)
    })
    names(posBudget) <- neededPositions

    # Competition per position: how many teams still need it and can outbid $1
    ps_others <- others
    posCompetitors <- sapply(neededPositions, function(pos) {
      threshold <- ifelse(pos %in% names(posThresholds), posThresholds[pos], 1)
      nComp <- 0
      for (i in seq_len(nrow(ps_others))) {
        tm <- ps_others$Team[i]
        theirMax <- ps_others$CashLeft[i] - (ps_others$Needed[i] - 1)
        if (theirMax <= 1) next
        tmHave <- pc %>% filter(Team == tm, Pos == pos) %>% nrow()
        if (tmHave < threshold) nComp <- nComp + 1
      }
      nComp
    })
    names(posCompetitors) <- neededPositions

    # Decision
    isStrongBuy <- !is.na(ratio) && ratio >= 1.3
    isLeanBuy <- !is.na(ratio) && ratio >= 1.0 && !isStrongBuy
    isBuyer <- isStrongBuy || isLeanBuy

    # Three paths to WANT:
    # Path 1: Strong Buy + budget > $3 + Low/No pressure
    # Path 2: Lean Buy + budget > $3 + No pressure
    # Path 3: Uncontested — budget <= $3 (or $1) AND 0 competitors needing the position
    highValuePos <- neededPositions[as.numeric(posBudget) > 3]
    uncontestedPos <- neededPositions[as.numeric(posCompetitors) == 0]

    pressureOf <- function(pos) {
      p <- myPressure$Pressure[myPressure$Pos == pos]
      if (length(p) == 0) return("Unknown")
      p[1]
    }

    path1Pos <- character(0)
    path2Pos <- character(0)
    path3Pos <- character(0)

    if (isStrongBuy && length(highValuePos) > 0) {
      path1Pos <- highValuePos[vapply(highValuePos, function(p) pressureOf(p) %in% c("Low", "No"), logical(1))]
    }
    if (isLeanBuy && length(highValuePos) > 0) {
      path2Pos <- highValuePos[vapply(highValuePos, function(p) pressureOf(p) == "No", logical(1))]
    }
    # Path 3: uncontested positions I need, regardless of buyer status
    path3Pos <- uncontestedPos[!uncontestedPos %in% c(path1Pos, path2Pos)]

    qualifyingPos <- unique(c(path1Pos, path2Pos, path3Pos))

    if (length(qualifyingPos) > 0) {
      label <- "Nominate someone you WANT"
      color <- "#2ecc71"
      bgColor <- "#d4edda"
      reasons <- c()
      if (length(c(path1Pos, path2Pos)) > 0) {
        reasons <- c(reasons, paste0(if (isStrongBuy) "Strong" else "Lean",
                                     " Buy ($", round(me$DPP), "/player vs $", round(leagueAvgDPP), " avg)"))
      }
      if (length(path3Pos) > 0) {
        reasons <- c(reasons, paste0("Uncontested: ", paste(path3Pos, collapse = ", ")))
      }
      rationale <- paste(reasons, collapse = " | ")
    } else if (!isBuyer) {
      label <- "Nominate someone you DON'T want"
      color <- "#e74c3c"
      bgColor <- "#f8d7da"
      rationale <- paste0("You're in ", if (is.na(ratio) || ratio >= 0.8) "Neutral" else "Wait",
                          " mode \u2014 drain budget from stronger teams")
    } else {
      label <- "Nominate someone you DON'T want"
      color <- "#f39c12"
      bgColor <- "#fff3cd"
      rationale <- if (isStrongBuy) "Strong Buy but no qualifying positions \u2014 let others overpay first"
                   else "Lean Buy but no qualifying positions \u2014 wait for a safer spot"
    }

    # Build position display with budget amounts
    posLine <- if (length(qualifyingPos) > 0) {
      posLabels <- sapply(qualifyingPos, function(p) {
        b <- posBudget[p]
        comp <- posCompetitors[p]
        extra <- c()
        if (!is.na(b) && b > 0) extra <- c(extra, paste0("$", b))
        if (comp == 0) extra <- c(extra, "uncontested")
        if (length(extra) > 0) paste0(p, " (", paste(extra, collapse = ", "), ")") else p
      })
      tags$div(style = "margin-top:4px;",
        tags$small(style = "color:#555; font-weight:bold;",
                   paste0("Target positions: ", paste(posLabels, collapse = ", "))))
    } else NULL

    tags$div(style = paste0("margin-top:15px; padding:10px; border-radius:6px; background:", bgColor, ";"),
      tags$strong(style = paste0("color:", color, ";"), label),
      tags$br(),
      tags$small(style = "color:#555;", rationale),
      posLine
    )
  })

  # --- Competition Check dropdown ---
  observe({
    ah <- AllH_avail()
    ap <- AllP_avail()
    hChoices <- setNames(ah$playerid, paste0(ah$Player, " (", ah$Pos, " $", round(ah$pDFL), ")"))
    pChoices <- setNames(ap$playerid, paste0(ap$Player, " (", ap$Pos, " $", round(ap$pDFL), ")"))
    allChoices <- c(hChoices, pChoices)
    updateSelectizeInput(session, 'compPlayer', choices = allChoices, server = TRUE)
  })

  output$competitionReport <- renderUI({
    req(input$compPlayer, input$nomTeam)
    pid <- input$compPlayer
    if (pid == "") return(NULL)
    myTeam <- input$nomTeam
    ps <- pstandings_r()
    pc <- protClean_r()

    # Find the player
    ah <- AllH_avail()
    ap <- AllP_avail()
    playerH <- ah %>% filter(playerid == pid)
    playerP <- ap %>% filter(playerid == pid)
    isHitter <- nrow(playerH) > 0
    if (!isHitter && nrow(playerP) == 0) return(tags$span("Player not found", style = "color:gray;"))
    player <- if (isHitter) playerH[1,] else playerP[1,]
    playerPos <- player$Pos

    # My max bid
    me <- ps %>% filter(Team == myTeam)
    if (nrow(me) == 0 || me$Needed <= 0) return(NULL)
    myMaxBid <- me$CashLeft - (me$Needed - 1)

    # Check each other team
    otherTeams <- ps %>% filter(Team != myTeam, Needed > 0)
    if (nrow(otherTeams) == 0) return(tags$div(style = "color:#2ecc71;", "No competitors left!"))

    competitors <- lapply(seq_len(nrow(otherTeams)), function(i) {
      tm <- otherTeams$Team[i]
      tmRow <- otherTeams[i,]
      theirMaxBid <- tmRow$CashLeft - (tmRow$Needed - 1)

      if (theirMaxBid <= myMaxBid) return(NULL)

      # Do they need this position as a starter?
      theirPosCounts <- pc %>% filter(Team == tm, Pos == playerPos) %>% nrow()
      threshold <- ifelse(playerPos %in% names(posThresholds), posThresholds[playerPos], 1)
      needsStarter <- theirPosCounts < threshold

      # Do they have bench room? (Needed > 0 means open slots somewhere)
      hasBenchRoom <- tmRow$Needed > 0

      if (!needsStarter && !hasBenchRoom) return(NULL)

      reason <- if (needsStarter) paste0("needs ", playerPos) else "bench"
      data.frame(Team = tm, MaxBid = theirMaxBid, Reason = reason, stringsAsFactors = FALSE)
    })

    competitors <- bind_rows(competitors)
    nComp <- nrow(competitors)

    color <- if (nComp <= 1) "#2ecc71" else if (nComp <= 3) "#f39c12" else "#e74c3c"
    headline <- paste0(nComp, " team", if (nComp != 1) "s", " can outbid you")

    if (nComp > 0) {
      competitors <- competitors %>% arrange(-MaxBid)
      teamLines <- lapply(seq_len(nComp), function(j) {
        tags$div(style = "font-size:12px; padding:2px 0;",
          paste0(competitors$Team[j], " ($", competitors$MaxBid[j], " max, ", competitors$Reason[j], ")"))
      })
    } else {
      teamLines <- list(tags$div(style = "font-size:12px; color:#2ecc71;", "No one can outbid you!"))
    }

    myBidLine <- tags$div(style = "font-size:11px; color:#888; margin-bottom:6px;",
                          paste0("Your max bid: $", myMaxBid))

    tagList(myBidLine,
      tags$div(style = paste0("padding:8px; border-radius:6px; background:", color, "15;"),
        tags$strong(style = paste0("color:", color, "; font-size:14px;"), headline),
        tags$div(style = "margin-top:6px;", teamLines)
      )
    )
  })

  # --- Positional Inflation table ---
  posInflation_r <- reactive({
    positions <- c('C','1B','2B','SS','3B','OF','SP','MR','CL')
    pc <- protClean_r() %>% filter(!is.na(DraftOrder))

    # Value lookup for surplus from draft log
    valueLookup <- bind_rows(
      AllH_active() %>% select(playerid, pDFL),
      AllP_active() %>% select(playerid, pDFL)
    ) %>% distinct(playerid, .keep_all = TRUE)
    log <- rv$draftLog
    logDf <- if (length(log) > 0) bind_rows(log) else data.frame()

    rows <- lapply(positions, function(pos) {
      posPc <- pc %>% filter(Pos == pos)
      nDrafted <- nrow(posPc)
      avgOverpay <- if (nDrafted > 0) mean(posPc$Salary - posPc$pDFL) else NA

      # Trend: avg overpay for last 5 drafted at this position
      trend <- NA
      if (nrow(logDf) > 0) {
        posPicks <- logDf %>% filter(Pos == pos)
        if (nrow(posPicks) > 0) {
          posPicks <- left_join(posPicks, valueLookup, by = "playerid")
          posPicks$pDFL <- replace_na(posPicks$pDFL, 0)
          last5 <- tail(posPicks, 5)
          trend <- mean(last5$Salary - last5$pDFL)
        }
      }

      sign <- if (!is.na(avgOverpay) && avgOverpay > 0) "+" else ""
      trendSign <- if (!is.na(trend) && trend > 0) "+" else ""

      data.frame(Position = pos, Drafted = nDrafted,
                 InflationNum = avgOverpay, TrendNum = trend,
                 Inflation = if (!is.na(avgOverpay)) paste0(sign, "$", sprintf("%.1f", avgOverpay)) else NA,
                 Trend = if (!is.na(trend)) paste0(trendSign, "$", sprintf("%.1f", trend)) else NA,
                 stringsAsFactors = FALSE)
    })
    bind_rows(rows)
  })

  output$posInflation <- DT::renderDataTable({
    df <- posInflation_r()
    datatable(df %>% select(Position, Drafted, Inflation, Trend, InflationNum, TrendNum),
              options = list(paging = FALSE, searching = FALSE, info = FALSE,
                             ordering = FALSE, autoWidth = FALSE,
                             columnDefs = list(list(visible = FALSE, targets = c(5, 6))))) %>%
      formatStyle('Inflation', valueColumns = 'InflationNum',
                  color = styleInterval(c(0, 5), c('#2ecc71', '#f39c12', '#e74c3c'))) %>%
      formatStyle('Trend', valueColumns = 'TrendNum',
                  color = styleInterval(c(0, 5), c('#2ecc71', '#f39c12', '#e74c3c')))
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
        Team = teamLink(tm), Status = status,
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
      select(Player, MLB = Org, Pos = Pos.y, Age = Age.x, Level = mlevel, DFL = pDFL, ADP = pADP, FV, Top.100, Game = Game.Pwr, Raw = Raw.Pwr, Spd, playerid) %>%
      arrange(desc(FV))
  })

  prospectP_r <- reactive({
    AllP_avail() %>% inner_join(pplist, by = 'playerid') %>%
      mutate(Player = fgLink(Name, playerid)) %>%
      select(Player, MLB = Org, Age = Age.x, Level = mlevel, DFL = pDFL, ADP = pADP, FV, Top.100, FB, SL, CB, CH, CMD, Sits, Tops, playerid) %>%
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
      # Remove from targets if present
      pid_char <- as.character(newRow$playerid)
      if (pid_char %in% rv$targets) {
        rv$targets <- rv$targets[rv$targets != pid_char]
        write.csv(data.frame(playerid = rv$targets, stringsAsFactors = FALSE), targetFile, row.names = FALSE)
      }
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
    # Remove from targets if present
    pid_char <- as.character(newRow$playerid)
    if (pid_char %in% rv$targets) {
      rv$targets <- rv$targets[rv$targets != pid_char]
      write.csv(data.frame(playerid = rv$targets, stringsAsFactors = FALSE), targetFile, row.names = FALSE)
    }
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
  # Reset Draft handler
  # ============================
  # --- Settings modal ---
  observeEvent(input$settingsBtn, {
    showModal(modalDialog(
      title = "Settings",
      size = "m",
      easyClose = TRUE,
      selectInput('myTeam', 'My Team', choices = teams,
                  selected = isolate(input$myTeam) %||% 'Liquor Crickets'),
      tags$hr(),
      radioButtons('projSource', 'Projection System',
                   choices = c('ATC' = 'atc',
                               'Steamer' = 'steamer',
                               'THE BAT X' = 'batx'),
                   selected = isolate(input$projSource) %||% 'atc',
                   inline = TRUE),
      tags$hr(),
      radioButtons('valMode', 'Valuation Mode',
                   choices = c('Projections Only' = 'proj',
                               'Blended' = 'blend',
                               'Leaderboard Only' = 'leaders'),
                   selected = isolate(input$valMode) %||% 'proj'),
      uiOutput("blendStatusModal"),
      tags$hr(),
      actionButton('resetDraftBtn', 'Reset Draft',
                   class = 'btn-danger', style = 'width:100%;'),
      footer = modalButton("Close")
    ))
  })

  # Blend status for settings modal
  output$blendStatusModal <- renderUI({
    mode <- input$valMode
    if (is.null(mode) || mode == "proj") {
      NULL
    } else if (is.null(leaderboards$hitters) || is.null(leaderboards$pitchers)) {
      tags$div(style = "text-align:center; padding:6px; margin-top:10px; border-radius:4px; background:#fff3cd;",
               tags$small(style = "color:#856404;", "Fetch leaderboards first to enable blend"))
    } else {
      NULL
    }
  })

  observeEvent(input$resetDraftBtn, {
    showModal(modalDialog(
      title = "Reset Draft",
      tags$p(tags$strong("Are you sure?"), " This will delete all draft data:"),
      tags$ul(
        tags$li("All drafted rosters"),
        tags$li("All budget allocations"),
        tags$li("All saved targets")
      ),
      tags$p("The app will reload with a fresh state."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton('confirmResetBtn', 'Yes, Reset Draft',
                     class = 'btn-danger')
      )
    ))
  })

  observeEvent(input$confirmResetBtn, {
    removeModal()
    # Re-seed roster from protection list (don't delete — app needs it on reload)
    if (file.exists(protFile)) {
      initRoster <- read.csv(protFile, stringsAsFactors = FALSE)
      initRoster$playerid <- as.character(initRoster$playerid)
      if ("X" %in% names(initRoster)) initRoster$X <- NULL
      hLookup <- AllH_full %>% select(playerid, MLB) %>% distinct()
      pLookup <- AllP_full %>% select(playerid, MLB) %>% distinct()
      mlbLookup <- bind_rows(hLookup, pLookup) %>% distinct(playerid, .keep_all = TRUE)
      if (!"MLB" %in% names(initRoster)) {
        initRoster <- left_join(initRoster, mlbLookup, by = "playerid")
      } else {
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
    if (file.exists(budgetFile)) file.remove(budgetFile)
    if (file.exists(targetFile)) file.remove(targetFile)
    session$reload()
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

  # Auto-open Settings on first load so modal inputs (myTeam, projSource, valMode) get initialized
  session$onFlushed(function() { click("settingsBtn") }, once = TRUE)

  # ============================
  # New outputs: Recent Picks, Draft Standings
  # ============================
  output$recentPicks <- DT::renderDataTable({
    log <- rv$draftLog
    if (length(log) == 0) {
      df <- data.frame(Player = character(), Team = character(),
                       Pos = character(), Salary = numeric())
    } else {
      df <- bind_rows(rev(log)) %>%
        mutate(Team = teamLink(Team)) %>%
        select(Player, Team, Pos, Salary, MLB)
    }
    datatable(df, escape = FALSE, options = list(pageLength = 15, autoWidth = FALSE,
                                 searching = FALSE, info = FALSE))
  })

  output$draftStandings <- DT::renderDataTable({
    ps <- pstandings_r() %>% mutate(Team = teamLink(Team))
    datatable(ps, escape = FALSE, options = list(pageLength = 20, autoWidth = FALSE,
                                 paging = FALSE, searching = FALSE, info = FALSE)) %>%
      formatCurrency(c('TotalValue','Earned','VPPlayer','DPP','FullValue')) %>%
      formatRound('CashLeft', 0) %>%
      formatRound(c('ValueRatio','zScore'), 2)
  })

  # ============================
  # Existing outputs — now reactive
  # ============================
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
                             info = FALSE,
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
      formatCurrency('Max Bid', digits = 0) %>%
      formatStyle('Market',
                  backgroundColor = styleEqual(
                    c('Strong Buy', 'Lean Buy', 'Neutral', 'Wait', 'Full'),
                    c('#d4edda', '#d4edda', '#fff3cd', '#f8d7da', '#e9ecef')))
  })

  output$pressureTable <- DT::renderDataTable({
    data <- pressure_r()
    datatable(data, rownames = FALSE,
              options = list(paging = FALSE, searching = FALSE, info = FALSE,
                             ordering = FALSE, autoWidth = FALSE),
              selection = 'none') %>%
      formatStyle('Pressure',
                  backgroundColor = styleEqual(
                    c('High', 'Medium', 'Low', 'No'),
                    c('#f8d7da', '#fff3cd', '#d4edda', '#e9ecef')),
                  fontWeight = styleEqual('High', 'bold'))
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
    dt <- datatable(data, selection = 'single',
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
                             info = FALSE),
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

  # Compute hotScores once (shared by both hitter/pitcher leaderboard reactives)
  leaderHotScores <- reactive({
    h <- leaderboards$hitters
    p <- leaderboards$pitchers
    if (is.null(h) || is.null(p)) return(list(NULL, NULL))
    p_hs <- p %>% rename(INN = IP, K = SO, HD = HLD, S = SV)
    hotScores(h, p_hs)
  })

  leaderH_avail <- reactive({
    req(leaderboards$hitters)
    roster <- rv$roster
    h <- leaderboards$hitters
    h$playerid <- as.character(h$playerid)
    # Add hotscore
    hs <- leaderHotScores()
    if (!is.null(hs[[1]])) {
      hsH <- hs[[1]] %>% rename(hotscore = zScore) %>% mutate(playerid = as.character(playerid))
      h <- left_join(h, hsH, by = "playerid")
    }
    # Remove rostered players
    h <- anti_join(h, roster, by = "playerid")
    # Add pDFL from projection pool
    pDFL_lookup <- AllH_active() %>% select(playerid, pDFL) %>% distinct()
    h <- left_join(h, pDFL_lookup, by = "playerid")
    h$pDFL <- replace_na(h$pDFL, 0)
    if ("hotscore" %in% names(h)) h %>% arrange(-hotscore) else h %>% arrange(-pDFL)
  })

  leaderP_avail <- reactive({
    req(leaderboards$pitchers)
    roster <- rv$roster
    p <- leaderboards$pitchers
    p$playerid <- as.character(p$playerid)
    # Add hotscore
    hs <- leaderHotScores()
    if (!is.null(hs[[2]])) {
      hsP <- hs[[2]] %>% rename(hotscore = zScore) %>% mutate(playerid = as.character(playerid))
      p <- left_join(p, hsP, by = "playerid")
    }
    # Remove rostered players
    p <- anti_join(p, roster, by = "playerid")
    # Add pDFL from projection pool
    pDFL_lookup <- AllP_active() %>% select(playerid, pDFL) %>% distinct()
    p <- left_join(p, pDFL_lookup, by = "playerid")
    p$pDFL <- replace_na(p$pDFL, 0)
    if ("hotscore" %in% names(p)) p %>% arrange(-hotscore) else p %>% arrange(-pDFL)
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
    if ('hotscore' %in% names(h)) dt <- dt %>% formatRound('hotscore', 2)
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
    if ('hotscore' %in% names(p)) dt <- dt %>% formatRound('hotscore', 2)
    if (length(tRows) > 0) dt <- dt %>% formatStyle(1, target = 'row', backgroundColor = styleRow(tRows, '#fff9c4'))
    dt
  })

  output$prospectP <- DT::renderDataTable({
    data <- markTargets(prospectP_r(), rv$targets)
    tRows <- which(data$isTarget == 1)
    data <- data %>% select(-playerid, -isTarget)
    dt <- datatable(data, selection = 'single',
              options = list(pageLength = 20, autoWidth = FALSE,
                             info = FALSE),
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
      geom_text(data = pct_df, aes(x, y, label = label), size = 4.5, fontface = "bold") +
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

  # --- Research tab: article scraping + LLM extraction ---
  observeEvent(input$analyzeBtn, {
    mode <- input$researchMode

    if (mode == "url") {
      url <- trimws(input$researchUrl)
      if (url == "" || !grepl("^https?://", url)) {
        showNotification("Please enter a valid URL", type = "warning")
        return()
      }
    } else {
      pastedText <- trimws(input$researchText)
      if (pastedText == "" || nchar(pastedText) < 50) {
        showNotification("Please paste article text (at least 50 characters)", type = "warning")
        return()
      }
    }

    # Disable button during processing
    shinyjs::disable("analyzeBtn")
    showNotification("Fetching article...", type = "message", duration = NULL, id = "researchMsg")

    tryCatch({
      # Step 1: Get article text
      if (mode == "url") {
        page <- rvest::read_html(url)
        # Try article-specific selectors first, fall back to generic
        articleNodes <- page %>% rvest::html_nodes("article")
        if (length(articleNodes) == 0) articleNodes <- page %>% rvest::html_nodes("#content")
        if (length(articleNodes) == 0) articleNodes <- page %>% rvest::html_nodes(".post-content, .entry-content, .article-body")
        if (length(articleNodes) > 0) {
          articleText <- articleNodes %>% rvest::html_text2() %>% paste(collapse = "\n\n")
        } else {
          articleText <- page %>%
            rvest::html_nodes("p, h1, h2, h3, h4, li") %>%
            rvest::html_text2() %>%
            paste(collapse = "\n\n")
        }

        pageTitle <- tryCatch(
          page %>% rvest::html_node("title") %>% rvest::html_text2(),
          error = function(e) "Unknown Article"
        )
        sourceDomain <- gsub("^https?://([^/]+).*", "\\1", url)
      } else {
        articleText <- pastedText
        pageTitle <- "Pasted Article"
        sourceDomain <- "manual"
      }

      # Smart truncation
      if (nchar(articleText) > 12000) {
        articleText <- substr(articleText, 1, 12000)
      }

      # Step 2: Call Claude API
      removeNotification("researchMsg")
      showNotification("Analyzing with Claude...", type = "message", duration = NULL, id = "researchMsg")

      prompt <- paste0(
        'You are a baseball fantasy analyst assistant. Extract all baseball players ',
        'mentioned in the following article. For each player the author is highlighting ',
        'as a target, sleeper, breakout, value pick, or otherwise recommending, return ',
        'a JSON array with these fields:\n\n',
        '- full_name: the player\'s full name (first and last)\n',
        '- summary: one sentence describing why the author thinks this player is interesting\n',
        '- tags: comma-separated list from these options: Sleeper, Breakout, Bounce-back, ',
        'Value, Upside, Buy-low, Sell-high, Injury-risk, Closer, Holds, Steals, Power, ',
        'AVG, Pitching, Strikeouts, Saves, Speed, Ratios\n\n',
        'Only include players the author is specifically recommending or discussing ',
        'positively. Skip players mentioned only in passing or as comparisons.\n\n',
        'Return ONLY the raw JSON array. No markdown, no code fences, no explanation. Example:\n',
        '[{"full_name": "Luis Arraez", "summary": "Hitting .340 in spring with strong ',
        'lineup protection boosting BA and R upside", "tags": "Sleeper, AVG, Value"}]\n\n',
        'Article text:\n', articleText
      )

      response <- callClaudeAPI(prompt, max_tokens = 4096)

      # Step 3: Parse response — strip markdown code fences if present
      response <- gsub("^\\s*```json\\s*", "", response)
      response <- gsub("^\\s*```\\s*", "", response)
      response <- gsub("\\s*```\\s*$", "", response)
      response <- trimws(response)

      # If response got truncated (no closing ]), try to fix it
      if (grepl("^\\[", response) && !grepl("\\]\\s*$", response)) {
        # Find the last complete object (ends with })
        lastBrace <- regexpr("\\}[^\\}]*$", response)
        if (lastBrace > 0) {
          response <- paste0(substr(response, 1, lastBrace), "]")
        }
      }

      if (!grepl("^\\s*\\[", response)) {
        # Might be an error string — retry with simplified prompt
        retryPrompt <- paste0(
          'Return ONLY a raw JSON array (no markdown, no code fences) of objects with fields: full_name, summary, tags. ',
          'Example: [{"full_name":"Mike Trout","summary":"Still elite","tags":"Power"}]. ',
          'Extract players recommended in this article:\n\n',
          substr(articleText, 1, 4000)
        )
        response <- callClaudeAPI(retryPrompt, max_tokens = 4096)
        response <- gsub("^\\s*```json\\s*", "", response)
        response <- gsub("^\\s*```\\s*", "", response)
        response <- gsub("\\s*```\\s*$", "", response)
        response <- trimws(response)
        if (!grepl("^\\s*\\[", response)) {
          removeNotification("researchMsg")
          cat("Research tab Claude API error:", substr(response, 1, 500), "\n")
          showNotification(paste0("Claude API error: ", substr(response, 1, 200)), type = "error", duration = 30)
          shinyjs::enable("analyzeBtn")
          return()
        }
      }

      extracted <- tryCatch(
        jsonlite::fromJSON(response),
        error = function(e) {
          removeNotification("researchMsg")
          cat("Research tab JSON parse error:", e$message, "\n")
          showNotification(paste0("Failed to parse Claude response: ", e$message), type = "error", duration = 30)
          shinyjs::enable("analyzeBtn")
          return(NULL)
        }
      )

      if (is.null(extracted) || nrow(extracted) == 0) {
        removeNotification("researchMsg")
        showNotification("No players found in this article", type = "warning")
        rv$researchH <- data.frame()
        rv$researchP <- data.frame()
        rv$researchUnmatched <- character(0)
        rv$researchTitle <- pageTitle
        shinyjs::enable("analyzeBtn")
        return()
      }

      # Step 4: Match to player pools
      removeNotification("researchMsg")
      showNotification("Matching players...", type = "message", duration = NULL, id = "researchMsg")

      availH <- AllH_avail()
      availP <- AllP_avail()
      allAvail <- bind_rows(
        availH %>% mutate(poolType = "H"),
        availP %>% mutate(poolType = "P")
      )
      cleanNames <- tolower(allAvail$Player)

      matchedRows <- list()
      unmatched <- character(0)

      for (i in seq_len(nrow(extracted))) {
        fname <- extracted$full_name[i]
        fnameL <- tolower(fname)

        # Exact match
        exactIdx <- which(cleanNames == fnameL)
        if (length(exactIdx) > 0) {
          row <- allAvail[exactIdx[1], ]
          row$Tags <- extracted$tags[i]
          row$Summary <- extracted$summary[i]
          row$fuzzy <- FALSE
          matchedRows <- c(matchedRows, list(row))
          next
        }

        # Fuzzy match
        fuzzyIdx <- agrep(fnameL, cleanNames, max.distance = 0.15, ignore.case = TRUE)
        if (length(fuzzyIdx) > 0) {
          row <- allAvail[fuzzyIdx[1], ]
          row$Tags <- extracted$tags[i]
          row$Summary <- extracted$summary[i]
          row$fuzzy <- TRUE
          matchedRows <- c(matchedRows, list(row))
        } else {
          unmatched <- c(unmatched, fname)
        }
      }

      if (length(matchedRows) == 0) {
        removeNotification("researchMsg")
        showNotification("No matched free agents found", type = "warning")
        rv$researchH <- data.frame()
        rv$researchP <- data.frame()
        rv$researchUnmatched <- unmatched
        rv$researchTitle <- pageTitle
        shinyjs::enable("analyzeBtn")
        return()
      }

      matched <- bind_rows(matchedRows)

      # Add fuzzy prefix
      matched$Player <- ifelse(matched$fuzzy,
                               paste0("~ ", matched$Player),
                               matched$Player)

      # Split into H and P
      mH <- matched %>% filter(poolType == "H") %>%
        mutate(Player = fgLink(Player, playerid)) %>%
        arrange(-pDFL) %>%
        select(Player, Pos, Tags, Summary, Age, DFL = pDFL, SGP = pSGP, ADP = pADP,
               HR = pHR, RBI = pRBI, R = pR, SB = pSB, AVG = pAVG,
               Injury, Expected.Return, playerid)

      mP <- matched %>% filter(poolType == "P") %>%
        mutate(Player = fgLink(Player, playerid)) %>%
        arrange(-pDFL) %>%
        select(Player, Pos, Tags, Summary, Age, DFL = pDFL, SGP = pSGP, ADP = pADP,
               W = pW, SO = pSO, ERA = pERA, SV = pSV, HLD = pHLD, `K/9` = `pK/9`,
               Injury, Expected.Return, playerid)

      rv$researchH <- mH
      rv$researchP <- mP
      rv$researchUnmatched <- unmatched
      rv$researchTitle <- paste0(pageTitle, " (", sourceDomain, ")")

      removeNotification("researchMsg")
      showNotification(paste0("Found ", nrow(matched), " free agent(s) from article"), type = "message")
      shinyjs::enable("analyzeBtn")

    }, error = function(e) {
      removeNotification("researchMsg")
      cat("Research tab error:", e$message, "\n")
      showNotification(paste0("Error: ", e$message), type = "error", duration = 30)
      shinyjs::enable("analyzeBtn")
    })
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

  # --- Research tab: render tables ---
  output$researchH <- DT::renderDataTable({
    df <- rv$researchH
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Message = "No hitters found. Paste an article URL and click Analyze."),
                       options = list(dom = 't'), selection = 'none'))
    }
    datatable(df, selection = 'single', escape = FALSE,
              options = list(pageLength = 20, columnDefs = list(
                list(visible = FALSE, targets = which(names(df) == "playerid") - 1)
              ))) %>%
      formatCurrency('DFL') %>%
      formatRound(c('SGP', 'AVG'), 3) %>%
      formatRound(c('Age', 'HR', 'RBI', 'R', 'SB'), 0)
  })

  output$researchP <- DT::renderDataTable({
    df <- rv$researchP
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Message = "No pitchers found. Paste an article URL and click Analyze."),
                       options = list(dom = 't'), selection = 'none'))
    }
    datatable(df, selection = 'single', escape = FALSE,
              options = list(pageLength = 20, columnDefs = list(
                list(visible = FALSE, targets = which(names(df) == "playerid") - 1)
              ))) %>%
      formatCurrency('DFL') %>%
      formatRound(c('SGP', 'ERA', 'K/9'), 3) %>%
      formatRound(c('Age', 'W', 'SO', 'SV', 'HLD'), 0)
  })

  # --- Research tab: status display ---
  output$researchStatus <- renderUI({
    title <- rv$researchTitle
    nH <- nrow(rv$researchH)
    nP <- nrow(rv$researchP)
    if (title == "" && nH == 0 && nP == 0) return(NULL)
    tags$div(style = "margin-top:10px; font-size:13px; line-height:1.6;",
      tags$strong(title),
      tags$br(),
      paste0(nH, " hitter(s), ", nP, " pitcher(s) found")
    )
  })

  # --- Research tab: unmatched players ---
  output$researchUnmatched <- renderUI({
    um <- rv$researchUnmatched
    if (length(um) == 0) return(NULL)
    tags$div(style = "margin-top:10px; font-size:12px; color:#888;",
      tags$em(paste0("Could not match: ", paste(um, collapse = ", ")))
    )
  })

  # --- Target toggle (Research tab — one button, checks active subtab) ---
  observeEvent(input$targetResBtn, {
    tab <- input$researchTab
    if (!is.null(tab) && tab == "Pitchers") {
      sel <- input$researchP_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        showNotification("Select a player row first", type = "warning")
        return()
      }
      data <- rv$researchP
      pid <- as.character(data$playerid[sel])
      pName <- data$Player[sel]
    } else {
      sel <- input$researchH_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        showNotification("Select a player row first", type = "warning")
        return()
      }
      data <- rv$researchH
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

})
