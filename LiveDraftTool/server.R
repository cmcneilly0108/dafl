# LiveDraftTool server.R — reactive draft tool

library(ggplot2)

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

# Save original pools for blend toggle
AllH_orig <- AllH_full
AllP_orig <- AllP_full

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
    pendingPick = NULL
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
    benchH <- c("DH", "BN", "BN", "BN", "BN")
    for (k in seq_along(benchH)) {
      if (k <= nrow(remaining)) {
        hRows[[length(hRows) + 1]] <- makeRow(benchH[k], remaining[k, ])
      } else {
        hRows[[length(hRows) + 1]] <- emptyRow(benchH[k])
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
        pRows[[length(pRows) + 1]] <- makeRow("BN", remaining[k, ])
      } else {
        pRows[[length(pRows) + 1]] <- emptyRow("BN")
      }
    }

    list(hitters = bind_rows(hRows), pitchers = bind_rows(pRows))
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
    ps <- pstandings_r()
    dollarsLeft <- sum(ps$CashLeft)
    spotsLeft <- sum(ps$Needed)

    ah <- AllH_avail()
    ap <- AllP_avail()

    if (!is.null(pos) && pos != "All") {
      if (pos %in% c('SP','MR','CL')) {
        ah <- ah[0, ]
        ap <- ap %>% filter(Pos == pos)
      } else {
        ah <- ah %>% filter(Pos == pos | (!is.na(posEl) & str_detect(posEl, pos)))
        ap <- ap[0, ]
      }
    }

    allAvail <- bind_rows(
      ah %>% select(playerid, pDFL),
      ap %>% select(playerid, pDFL)
    ) %>% filter(pDFL > 0) %>% arrange(-pDFL)

    topN <- head(allAvail, spotsLeft)
    valueLeft <- sum(topN$pDFL)

    inflationMult <- if (valueLeft > 0) dollarsLeft / valueLeft else NA
    list(dollarsLeft = dollarsLeft, valueLeft = valueLeft,
         spotsLeft = spotsLeft, inflation = inflationMult)
  })

  output$inflationDisplay <- renderUI({
    d <- inflationData_r()
    if (is.na(d$inflation)) return(tags$span("No data yet", style = "color:gray;"))

    pct <- (d$inflation - 1) * 100
    color <- if (pct > 10) "#e74c3c" else if (pct > 0) "#f39c12" else "#2ecc71"
    sign <- if (pct >= 0) "+" else ""

    tags$div(
      style = "text-align:center; padding:8px; border-radius:4px; background:#f8f9fa; margin-bottom:10px;",
      tags$strong(style = paste0("font-size:22px; color:", color, ";"),
                  paste0(sprintf("%.1f", d$inflation), "x")),
      tags$span(style = paste0("font-size:14px; color:", color, ";"),
                paste0(" (", sign, sprintf("%.0f", pct), "%)")),
      tags$br(),
      tags$small(style = "color:gray;",
                 paste0("$", round(d$dollarsLeft), " left / $", round(d$valueLeft),
                        " value / ", d$spotsLeft, " spots"))
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

  # --- topHitters ---
  topHitters_r <- reactive({
    AllH_avail() %>% arrange(-pDFL) %>% head(40) %>%
      mutate(Player = fgLink(Player, playerid)) %>%
      select(Player, MLB, posEl, Age, pDFL, ADP = pADP, rankDiff, Skew = pSkew,
             HR = pHR, RBI = pRBI, R = pR, SB = pSB, AVG = pAVG)
  })

  # --- prospects (reactive to remove drafted players) ---
  prospectH_r <- reactive({
    AllH_avail() %>% inner_join(hplist, by = 'playerid') %>%
      mutate(Player = fgLink(Name, playerid)) %>%
      select(Player, MLB = Org, Pos = Pos.y, Age = Age.x, DFL = pDFL, ADP = pADP, FV, Top.100, Game = Game.Pwr, Raw = Raw.Pwr, Spd) %>%
      arrange(desc(FV))
  })

  prospectP_r <- reactive({
    AllP_avail() %>% inner_join(pplist, by = 'playerid') %>%
      mutate(Player = fgLink(Name, playerid)) %>%
      select(Player, MLB = Org, Age = Age.x, DFL = pDFL, ADP = pADP, FV, Top.100, FB, SL, CB, CH, CMD, Sits, Tops) %>%
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
    res <- mutate(res, Player = fgLink(Player, playerid))
    select(res, Player, MLB, posEl, Age, DFL, RPV, SGP, orank, ADP = pADP, rankDiff, Skew = pSkew,
           HR = pHR, RBI = pRBI, R = pR, SB = pSB, AVG = pAVG, Injury, Expected.Return)
  }

  pitPlayersbyPos <- function(pos) {
    ap <- AllP_avail()
    res <- ap %>% filter(Pos == pos, pSGP > 0) %>%
      arrange(-pDFL, -pSGP) %>% dplyr::rename(DFL = pDFL, SGP = pSGP) %>% head(200)
    res <- mutate(res, RPV = (SGP - aRPV(res, nrow(filter(res, DFL > 0)))) /
                    aRPV(res, nrow(filter(res, DFL > 0))))
    res <- mutate(res, Player = fgLink(Player, playerid))
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
    datatable(hitPlayersbyPos(input$e2),
              options = list(pageLength = 20, autoWidth = FALSE,
                             searching = FALSE, info = FALSE), filter = 'top', escape = FALSE) %>%
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
                             info = FALSE), filter = 'top', escape = FALSE) %>%
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
    datatable(rrcResults %>% mutate(Player = fgLink(Player, playerid)) %>% select(-playerid),
              options = list(pageLength = 20, autoWidth = FALSE,
                             info = FALSE), filter = 'top', escape = FALSE) %>%
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
                             searching = FALSE, info = FALSE), escape = FALSE) %>%
      formatRound(c('Age','ADP','rankDiff','HR','RBI','R','SB'), 0) %>%
      formatCurrency('pDFL') %>%
      formatRound(c('Skew','AVG'), 3)
  })

  output$prospectH <- DT::renderDataTable({
    datatable(prospectH_r(),
              options = list(pageLength = 20, autoWidth = FALSE,
                             searching = FALSE, info = FALSE), filter = 'top', escape = FALSE) %>%
      formatRound(c('Age','ADP'), 0) %>% formatCurrency('DFL')
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
    datatable(h %>% select(-playerid),
              options = list(pageLength = 25, autoWidth = FALSE, info = FALSE),
              filter = 'top', escape = FALSE) %>%
      formatCurrency('pDFL') %>%
      formatRound(c('AVG','OBP','SLG','OPS','wOBA'), 3) %>%
      formatRound(c('wRC+','WAR'), 1)
  })

  output$leaderP <- DT::renderDataTable({
    p <- leaderP_avail() %>% mutate(Player = fgLink(Player, playerid))
    datatable(p %>% select(-playerid),
              options = list(pageLength = 25, autoWidth = FALSE, info = FALSE),
              filter = 'top', escape = FALSE) %>%
      formatCurrency('pDFL') %>%
      formatRound(c('ERA','WHIP','BABIP','FIP','xFIP','K/9','BB/9'), 2) %>%
      formatRound('WAR', 1)
  })

  output$prospectP <- DT::renderDataTable({
    datatable(prospectP_r(),
              options = list(pageLength = 20, autoWidth = FALSE,
                             searching = FALSE, info = FALSE), filter = 'top', escape = FALSE) %>%
      formatRound(c('Age','ADP'), 0) %>% formatCurrency('DFL')
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

  output$rosterH <- DT::renderDataTable({
    tr <- teamRoster_r()
    datatable(tr$hitters %>% mutate(Player = fgLink(Player, playerid)) %>% select(-playerid), rownames = FALSE,
              options = list(paging = FALSE, searching = FALSE, info = FALSE,
                             ordering = FALSE, autoWidth = FALSE), escape = FALSE) %>%
      formatCurrency(c('Salary', 'pDFL', 'Value')) %>%
      formatRound('Age', 0) %>%
      formatRound('Yr', 0) %>%
      formatStyle('Value', color = styleInterval(0, c('#e74c3c', '#2ecc71'))) %>%
      formatStyle('Player', target = 'row',
                  backgroundColor = styleEqual("", "#f5f5f5"))
  })

  output$rosterP <- DT::renderDataTable({
    tr <- teamRoster_r()
    datatable(tr$pitchers %>% mutate(Player = fgLink(Player, playerid)) %>% select(-playerid), rownames = FALSE,
              options = list(paging = FALSE, searching = FALSE, info = FALSE,
                             ordering = FALSE, autoWidth = FALSE), escape = FALSE) %>%
      formatCurrency(c('Salary', 'pDFL', 'Value')) %>%
      formatRound('Age', 0) %>%
      formatRound('Yr', 0) %>%
      formatStyle('Value', color = styleInterval(0, c('#e74c3c', '#2ecc71'))) %>%
      formatStyle('Player', target = 'row',
                  backgroundColor = styleEqual("", "#f5f5f5"))
  })

  output$rosterGoals <- DT::renderDataTable({
    req(input$rosterTeam)
    tr <- teamRoster_r()
    # Only count starters toward goals
    hStarterIds <- tr$hitters %>% filter(!Slot %in% c("BN")) %>% pull(playerid)
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
        tags$tr(tags$td("Remaining"), tags$td(style = "text-align:right;", paste0("$", cap - total))),
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

})
