# Guardians Tracker — Shiny server. Sources the pulse script which hydrates
# globals (gRoster, gStats, gHot, gTxn, gIL, gProspects, gDepth, gTrend).

setwd("../code/")
source("./guardiansPulse.r")

library("dplyr")
library("DT")
library("plotly")

shinyServer(function(input, output, session) {

  rv <- reactiveValues(refreshCount = 0)

  # Escape a string for safe embedding inside a single-quoted JS string in an
  # inline onclick attribute (e.g. names like O'Brien). Backslash first.
  jsStr <- function(s) {
    s <- gsub("\\\\", "\\\\\\\\", s)
    s <- gsub("'", "\\\\'", s)
    s <- gsub('"', "&quot;", s)
    gsub("[\r\n]+", " ", s)
  }

  # Render a player name as a link that opens the Player Detail tab with that
  # player loaded. Returns an <a> only when the name exists in the current
  # roster; otherwise the HTML-escaped `display` text (no dead links). Vectorized
  # so it can map a whole DT column. `display` lets callers show a suffix while
  # linking on the bare name.
  playerLink <- function(name, display = name) {
    vapply(seq_along(name), function(i) {
      nm <- name[i]; dp <- display[i]
      esc <- htmltools::htmlEscape(dp, attribute = FALSE)
      if (is.na(nm) || !(nm %in% gRoster$player)) return(esc)
      sprintf('<a href="#" style="cursor:pointer;" onclick="Shiny.setInputValue(\'gPlayerClick\', \'%s\', {priority:\'event\'}); return false;">%s</a>',
              jsStr(nm), esc)
    }, character(1))
  }

  # Any clicked player name lands here: set the Player Detail picker and switch
  # to that tab. gPlayerPick is a server-side selectize; selected = sets it.
  observeEvent(input$gPlayerClick, {
    updateSelectizeInput(session, 'gPlayerPick', selected = input$gPlayerClick)
    updateNavbarPage(session, 'gNav', selected = "Player Detail")
  })

  # Shared helper: format a player's season stats as a two-line HTML cell.
  # Counting stats (bold labels) on top, slash / rate stats (gray) below.
  # Returns empty string if no stats row exists.
  seasonLineHtml <- function(pid) {
    st <- gStats[gStats$mlb_id == pid, ]
    if (nrow(st) == 0) return("")
    zi <- function(x) ifelse(is.na(x), 0L, as.integer(x))
    zr <- function(x) ifelse(is.na(x), 0, x)
    d3 <- function(x) sub("^0\\.", ".", sprintf("%.3f", zr(x)))
    if (st$role[1] == "H" && !is.na(st$avg[1])) {
      ops <- ifelse(is.na(st$obp[1]) | is.na(st$slg[1]), NA, st$obp[1] + st$slg[1])
      top <- sprintf("%d <b>AB</b> · %d <b>R</b> · %d <b>HR</b> · %d <b>RBI</b> · %d <b>SB</b>",
                     zi(st$ab[1]), zi(st$r[1]), zi(st$hr[1]),
                     zi(st$rbi[1]), zi(st$sb[1]))
      bot <- sprintf("%s / %s / %s", d3(st$avg[1]), d3(st$obp[1]), d3(ops))
      paste0(top, '<br><span style="color:#555;">', bot, '</span>')
    } else if (st$role[1] == "P" && !is.na(st$era[1])) {
      top <- sprintf("%.1f <b>IP</b> · %d-%d · %d <b>K</b> · %d <b>SV</b> · %d <b>HD</b>",
                     zr(st$ip[1]),
                     zi(st$w[1]), zi(st$l[1]),
                     zi(st$so[1]), zi(st$sv[1]), zi(st$hld[1]))
      bot <- sprintf("%.2f <b>ERA</b> · %.2f <b>WHIP</b> · %.1f <b>K/9</b>",
                     zr(st$era[1]), zr(st$whip[1]), zr(st$k9[1]))
      paste0(top, '<br><span style="color:#555;">', bot, '</span>')
    } else ""
  }

  # --- Settings modal: Refresh button ---
  observeEvent(input$gSettingsBtn, {
    showModal(modalDialog(
      title = "Settings",
      size = "s", easyClose = TRUE,
      tags$p(paste0("Latest snapshot: ", today)),
      actionButton('gRefreshBtn', 'Refresh Data',
                   class = 'btn-success btn-sm', icon = icon('refresh')),
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$gRefreshBtn, {
    removeModal()
    showNotification("Refreshing Guardians data…", type = "message",
                     duration = NULL, id = "gRefreshMsg")
    Sys.setenv(DAFL_FORCE_REFRESH = "1")
    on.exit(Sys.unsetenv("DAFL_FORCE_REFRESH"), add = TRUE)
    tryCatch({
      source("../code/guardiansPulse.r", local = globalenv())
      rv$refreshCount <- rv$refreshCount + 1
      removeNotification("gRefreshMsg")
      showNotification("Refreshed!", type = "message")
    }, error = function(e) {
      removeNotification("gRefreshMsg")
      showNotification(paste0("Refresh failed: ", e$message),
                       type = "error", duration = 15)
    })
  })

  # Depth Chart: one level at a time, shown as a baseball diamond with players
  # placed at their roster positions. Non-fielding slots (DH/UT/IF/two-way)
  # render in a "Bench" line under the diamond. A compact roster table with
  # season stat lines sits below the diamond.
  output$gDepthDiamond <- renderUI({
    rv$refreshCount
    req(input$gDepthLevel)
    if (nrow(gRoster) == 0) {
      return(tags$div(style = "color:#888; font-style:italic;",
                      "No roster snapshot available."))
    }
    lvl <- input$gDepthLevel
    sub <- gRoster[gRoster$level == lvl &
                   (is.na(gRoster$status) | gRoster$status == "Active"), ]
    header <- tags$h3(paste0(lvl, " (", nrow(sub), " active)"),
                      style = "margin-bottom:10px;")
    if (nrow(sub) == 0) {
      return(tagList(header,
        tags$div(style = "color:#888; font-style:italic;", "No roster.")))
    }

    # Categorize each player into a field slot, pitcher (split SP/RP via gs),
    # or bench. Generic "OF" players fold into the RF slot per user request.
    # Each player also carries a sort key so within-bucket ordering reflects
    # role / playing time, not alphabet:
    #   - Field positions (hitters):  PA desc — regular starter first
    #   - SP:                         IP desc — workhorse first
    #   - RP:                         SV+HD desc — closer first, then setup
    pitchPos  <- c("P","SP","RP","CL","MR")
    fieldPos  <- c("C","1B","2B","3B","SS","LF","CF","RF")
    grouped     <- list()   # field slot -> char vec of player names
    groupedKey  <- list()   # parallel, numeric sort key
    pitchers    <- list(SP = character(0), RP = character(0))
    pitchersKey <- list(SP = numeric(0),   RP = numeric(0))
    bench <- character(0)
    for (i in seq_len(nrow(sub))) {
      p <- sub$pos[i]; nm <- sub$player[i]; pid <- sub$mlb_id[i]
      st <- gStats[gStats$mlb_id == pid, ]
      if (p %in% pitchPos) {
        gs <- if (nrow(st) > 0 && !is.na(st$gs[1])) as.integer(st$gs[1]) else 0L
        ip <- if (nrow(st) > 0 && !is.na(st$ip[1])) as.numeric(st$ip[1]) else 0
        sv <- if (nrow(st) > 0 && !is.na(st$sv[1])) as.integer(st$sv[1]) else 0L
        hd <- if (nrow(st) > 0 && !is.na(st$hld[1])) as.integer(st$hld[1]) else 0L
        era <- if (nrow(st) > 0 && !is.na(st$era[1])) sprintf(" (%.2f ERA)", st$era[1]) else ""
        bucket <- if (gs >= 3) "SP" else "RP"
        key <- if (bucket == "SP") ip else (sv + hd)
        pitchers[[bucket]]    <- c(pitchers[[bucket]],    paste0(nm, era))
        pitchersKey[[bucket]] <- c(pitchersKey[[bucket]], key)
      } else if (p %in% fieldPos || identical(p, "OF")) {
        slot <- if (identical(p, "OF")) "RF" else p
        pa <- if (nrow(st) > 0 && !is.na(st$pa[1])) as.integer(st$pa[1]) else 0L
        grouped[[slot]]    <- c(grouped[[slot]], nm)
        groupedKey[[slot]] <- c(groupedKey[[slot]], pa)
      } else {
        bench <- c(bench, paste0(nm, " (", p, ")"))
      }
    }
    # Sort each bucket by its sort key, descending. NAs already → 0 above.
    for (slot in names(grouped)) {
      ord <- order(groupedKey[[slot]], decreasing = TRUE)
      grouped[[slot]] <- grouped[[slot]][ord]
    }
    for (bucket in names(pitchers)) {
      ord <- order(pitchersKey[[bucket]], decreasing = TRUE)
      pitchers[[bucket]] <- pitchers[[bucket]][ord]
    }

    # Render the SVG diamond as a raw string. The 600x600 viewBox scales
    # responsively via max-width on the wrapper.
    esc <- function(s) htmltools::htmlEscape(s, attribute = FALSE)
    playersAt <- function(slot, x, y, upward = FALSE) {
      pl <- grouped[[slot]]
      if (is.null(pl) || length(pl) == 0) return("")
      step <- if (upward) -14 else 14
      # Label sits opposite the stack direction (above for down, below for up)
      # so it never collides with the names.
      labelY <- if (upward) y + 14 else y - 14
      label <- sprintf('<text x="%d" y="%d" text-anchor="middle" fill="#ffffff" font-size="11" font-weight="bold" opacity="0.6">%s</text>',
                       x, labelY, slot)
      names <- paste(vapply(seq_along(pl), function(j) {
        nm <- pl[j]
        clickable <- nm %in% gRoster$player
        onclick <- if (clickable) sprintf(' style="cursor:pointer;" onclick="Shiny.setInputValue(\'gPlayerClick\', \'%s\', {priority:\'event\'}); return false;"', jsStr(nm)) else ""
        sprintf('<text x="%d" y="%d" text-anchor="middle" fill="#ffffff" font-size="12" stroke="#0F223E" stroke-width="0.3" paint-order="stroke"%s>%s</text>',
                x, y + (j - 1) * step, onclick, esc(nm))
      }, character(1)), collapse = "")
      paste0(label, names)
    }

    svg <- paste0(
      '<svg viewBox="0 0 600 640" style="width:100%; max-width:640px; display:block; border-radius:8px;">',
      # Outfield grass background (extra height for catcher stack below home).
      '<rect x="0" y="0" width="600" height="640" fill="#5a8d4e"/>',
      # Warning track arc (just a stylistic curve)
      '<path d="M 60 330 A 240 240 0 0 1 540 330" fill="none" stroke="#4a7741" stroke-width="14" opacity="0.6"/>',
      # Infield dirt diamond
      '<polygon points="300,540 510,330 300,120 90,330" fill="#c2a472" stroke="white" stroke-width="3"/>',
      # Pitcher mound
      '<circle cx="300" cy="380" r="32" fill="#a98b5e" stroke="white" stroke-width="2"/>',
      # Bases (rotated squares)
      '<rect x="500" y="322" width="16" height="16" fill="white" transform="rotate(45 508 330)"/>',  # 1B
      '<rect x="292" y="112" width="16" height="16" fill="white" transform="rotate(45 300 120)"/>',  # 2B
      '<rect x="84"  y="322" width="16" height="16" fill="white" transform="rotate(45 92  330)"/>',  # 3B
      # Home plate
      '<polygon points="290,540 310,540 315,548 300,560 285,548" fill="white"/>',
      # Players at each slot
      playersAt("CF", 300,  70),
      playersAt("LF", 130, 130),
      playersAt("RF", 470, 130),
      playersAt("3B", 150, 340),
      playersAt("SS", 235, 285),
      playersAt("2B", 365, 285),
      playersAt("1B", 450, 340),
      # Mound circle stays as a visual element; pitcher names live in the
      # SP/RP columns to the right of the diamond.
      '<text x="300" y="385" text-anchor="middle" fill="#ffffff" font-size="11" font-weight="bold" opacity="0.6">P</text>',
      playersAt("C",  300, 580),
      '</svg>'
    )

    # SP / RP side columns. ERA in parens for quick scanning.
    pitcherCol <- function(label, names) {
      if (length(names) == 0) {
        return(tagList(
          tags$h5(label, style = "margin-bottom:4px;"),
          tags$div(style = "color:#888; font-style:italic; font-size:13px;",
                   "—")
        ))
      }
      tagList(
        tags$h5(paste0(label, " (", length(names), ")"),
                style = "margin-bottom:4px;"),
        tags$div(style = "font-size:13px;",
                 do.call(tagList,
                         lapply(names, function(x) tags$div(style="padding:2px 0;", x))))
      )
    }
    sideUI <- fluidRow(
      column(width = 6, pitcherCol("SP", pitchers$SP)),
      column(width = 6, pitcherCol("RP", pitchers$RP))
    )

    benchUI <- if (length(bench) > 0) {
      tags$div(style = "margin-top:14px;",
               tags$strong("Bench: "),
               tags$span(paste(bench, collapse = "  ·  ")))
    } else NULL

    # Roster stat table for the selected level. Each player's Season cell
    # is HTML: counting stats on top line, rate stats / slash line below.
    # Renders via escape=FALSE on the DT.
    lineFor <- function(pid) {
      st <- gStats[gStats$mlb_id == pid, ]
      if (nrow(st) == 0) return("")
      zi <- function(x) ifelse(is.na(x), 0L, as.integer(x))
      zr <- function(x) ifelse(is.na(x), 0, x)
      # Baseball slash-line format: ".299" for x<1, "1.250" for x>=1.
      d3 <- function(x) sub("^0\\.", ".", sprintf("%.3f", zr(x)))
      if (st$role[1] == "H" && !is.na(st$avg[1])) {
        ops <- ifelse(is.na(st$obp[1]) | is.na(st$slg[1]), NA, st$obp[1] + st$slg[1])
        top <- sprintf("%d <b>AB</b> · %d <b>R</b> · %d <b>HR</b> · %d <b>RBI</b> · %d <b>SB</b>",
                       zi(st$ab[1]), zi(st$r[1]), zi(st$hr[1]),
                       zi(st$rbi[1]), zi(st$sb[1]))
        bot <- sprintf("%s / %s / %s",
                       d3(st$avg[1]), d3(st$obp[1]), d3(ops))
        paste0(top, '<br><span style="color:#555;">', bot, '</span>')
      } else if (st$role[1] == "P" && !is.na(st$era[1])) {
        top <- sprintf("%.1f <b>IP</b> · %d-%d · %d <b>K</b> · %d <b>SV</b> · %d <b>HD</b>",
                       zr(st$ip[1]),
                       zi(st$w[1]), zi(st$l[1]),
                       zi(st$so[1]), zi(st$sv[1]), zi(st$hld[1]))
        bot <- sprintf("%.2f <b>ERA</b> · %.2f <b>WHIP</b> · %.1f <b>K/9</b>",
                       zr(st$era[1]), zr(st$whip[1]), zr(st$k9[1]))
        paste0(top, '<br><span style="color:#555;">', bot, '</span>')
      } else ""
    }
    df <- data.frame(
      Player = sub$player,
      Pos = sub$pos,
      Age = ifelse(is.na(sub$age), NA_real_, round(sub$age, 1)),
      Season = sapply(sub$mlb_id, lineFor),
      stringsAsFactors = FALSE
    )
    df <- df[order(df$Pos, df$Player), ]
    df$Player <- playerLink(df$Player)
    statTable <- DT::datatable(df,
                               options = list(paging = FALSE, dom = "t",
                                              autoWidth = FALSE),
                               rownames = FALSE, escape = FALSE)

    tagList(
      header,
      fluidRow(
        column(width = 8, HTML(svg)),
        column(width = 4, sideUI)
      ),
      benchUI,
      tags$div(style = "margin-top:20px;", statTable)
    )
  })

  output$gHotTable <- DT::renderDataTable({
    rv$refreshCount
    req(input$gHotRole, input$gHotLevel)
    # Start from the roster so EVERY currently-active player shows up, even
    # those under the PA/IP threshold for a hot score (recent call-ups, etc.).
    # Left join gHot for the score (NA when not qualified) and gStats for the
    # season line.
    pitchPos <- c("P","SP","RP","CL","MR","TWP")
    df <- gRoster %>%
      filter(is.na(status) | status == "Active") %>%
      mutate(role = ifelse(pos %in% pitchPos, "P", "H")) %>%
      left_join(gHot %>% select(mlb_id, hotscore), by = "mlb_id") %>%
      left_join(gStats %>% select(mlb_id, ab, r, hr, rbi, sb,
                                  avg, obp, slg,
                                  ip, w, l, sv, hld, so, era, k9, whip),
                by = "mlb_id")
    if (input$gHotRole != "A") df <- df %>% filter(role == input$gHotRole)
    if (input$gHotLevel != "All") df <- df %>% filter(level == input$gHotLevel)
    if (nrow(df) == 0) {
      return(datatable(data.frame(Note = "No players match this filter"),
                       options = list(dom = 't'), selection = 'none', rownames = FALSE))
    }
    # OPS = OBP + SLG (computed on the fly; mlb_stats exposes OPS but not OPS+).
    # Season cell is HTML — counting stats top line, rate stats below.
    # Baseball slash-line format: ".299" for x<1, "1.250" for x>=1 (OPS often
    # crosses 1.000 for top hitters).
    d3 <- function(x) sub("^0\\.", ".", sprintf("%.3f", ifelse(is.na(x), 0, x)))
    df <- df %>%
      mutate(ops = ifelse(is.na(obp) | is.na(slg), NA_real_, obp + slg),
             Season = ifelse(role == "H",
                  paste0(
                    sprintf("%d <b>AB</b> · %d <b>R</b> · %d <b>HR</b> · %d <b>RBI</b> · %d <b>SB</b>",
                            ifelse(is.na(ab), 0L, as.integer(ab)),
                            ifelse(is.na(r), 0L, as.integer(r)),
                            ifelse(is.na(hr), 0L, as.integer(hr)),
                            ifelse(is.na(rbi), 0L, as.integer(rbi)),
                            ifelse(is.na(sb), 0L, as.integer(sb))),
                    '<br><span style="color:#555;">',
                    sprintf("%s / %s / %s", d3(avg), d3(obp), d3(ops)),
                    '</span>'),
                  paste0(
                    sprintf("%.1f <b>IP</b> · %d-%d · %d <b>K</b> · %d <b>SV</b> · %d <b>HD</b>",
                            ifelse(is.na(ip), 0, ip),
                            ifelse(is.na(w), 0L, as.integer(w)),
                            ifelse(is.na(l), 0L, as.integer(l)),
                            ifelse(is.na(so), 0L, as.integer(so)),
                            ifelse(is.na(sv), 0L, as.integer(sv)),
                            ifelse(is.na(hld), 0L, as.integer(hld))),
                    '<br><span style="color:#555;">',
                    sprintf("%.2f <b>ERA</b> · %.2f <b>WHIP</b> · %.1f <b>K/9</b>",
                            ifelse(is.na(era), 0, era),
                            ifelse(is.na(whip), 0, whip),
                            ifelse(is.na(k9), 0, k9)),
                    '</span>'))) %>%
      arrange(desc(!is.na(hotscore)), desc(hotscore)) %>%
      select(Player = player, Lvl = level, Pos = pos, Age = age,
             Season, HotScore = hotscore)

    df$Player <- playerLink(df$Player)
    datatable(df,
              options = list(pageLength = 25, autoWidth = FALSE),
              rownames = FALSE, escape = FALSE) %>%
      formatRound("HotScore", 2) %>%
      formatRound("Age", 0) %>%
      formatStyle("HotScore",
                  backgroundColor = styleInterval(c(-0.5, 0.5),
                                                  c("#f8d7da", "#ffffff", "#d4edda")))
  })
  output$gPlayerCard <- renderUI({
    rv$refreshCount
    nm <- input$gPlayerPick
    if (is.null(nm) || nm == "") {
      return(tags$div(style = "color:#888; padding:12px; font-style:italic;",
                      "Type a name in the search box to see player details."))
    }
    row <- gRoster %>% filter(player == nm)
    if (nrow(row) == 0) return(tags$div("Player not found in current roster."))
    row <- row[1, ]
    st <- gStats %>% filter(mlb_id == row$mlb_id)
    pros <- gProspects %>% filter(Name == nm)

    # Header
    headerUI <- tags$div(
      style = "padding:12px 16px; background:#0F223E; color:white; border-radius:6px 6px 0 0;",
      tags$div(style = "font-size:20px; font-weight:bold;", row$player),
      tags$div(style = "font-size:14px; margin-top:4px; color:#bdc3c7;",
               paste0(row$pos, "  |  ", row$level, "  |  Age ",
                      ifelse(is.na(row$age), "?", round(row$age, 1)),
                      if (nrow(pros) > 0 && "FV" %in% names(pros) && !is.na(pros$FV[1]))
                        paste0("  |  FV ", pros$FV[1]) else "",
                      if (nrow(pros) > 0 && "Top.100" %in% names(pros) && !is.na(pros$Top.100[1]))
                        paste0("  |  #", pros$Top.100[1], " overall") else ""))
    )

    # Hero line — current season slash or pitching summary
    heroUI <- if (nrow(st) > 0 && st$role[1] == "H") {
      tags$div(style = "padding:12px 16px; background:#f8f9fa; border:1px solid #ddd; border-top:none; font-size:18px;",
        tags$strong(sprintf("%s / %s / %s",
              sub("^0\\.", ".", sprintf("%.3f", ifelse(is.na(st$avg[1]), 0, st$avg[1]))),
              sub("^0\\.", ".", sprintf("%.3f", ifelse(is.na(st$obp[1]), 0, st$obp[1]))),
              sub("^0\\.", ".", sprintf("%.3f", ifelse(is.na(st$slg[1]), 0, st$slg[1]))))),
        tags$span(style = "margin-left:16px; color:#666; font-size:14px;",
                  sprintf("%d HR · %d RBI · %d R · %d SB",
                          ifelse(is.na(st$hr[1]), 0, as.integer(st$hr[1])),
                          ifelse(is.na(st$rbi[1]), 0, as.integer(st$rbi[1])),
                          ifelse(is.na(st$r[1]), 0, as.integer(st$r[1])),
                          ifelse(is.na(st$sb[1]), 0, as.integer(st$sb[1])))))
    } else if (nrow(st) > 0 && st$role[1] == "P") {
      tags$div(style = "padding:12px 16px; background:#f8f9fa; border:1px solid #ddd; border-top:none; font-size:18px;",
        tags$strong(sprintf("%.2f ERA · %.2f WHIP · %.1f K/9",
              ifelse(is.na(st$era[1]),  0, st$era[1]),
              ifelse(is.na(st$whip[1]), 0, st$whip[1]),
              ifelse(is.na(st$k9[1]),   0, st$k9[1]))),
        tags$span(style = "margin-left:16px; color:#666; font-size:14px;",
                  sprintf("%d W · %d SV · %d HLD · %d K",
                          ifelse(is.na(st$w[1]), 0, as.integer(st$w[1])),
                          ifelse(is.na(st$sv[1]), 0, as.integer(st$sv[1])),
                          ifelse(is.na(st$hld[1]), 0, as.integer(st$hld[1])),
                          ifelse(is.na(st$so[1]), 0, as.integer(st$so[1])))))
    } else {
      tags$div(style = "padding:12px 16px; background:#f8f9fa; border:1px solid #ddd; border-top:none; color:#888;",
              "No season stats yet for this player.")
    }

    # Trend plot — HotScore over snapshot_date (14d window) from gTrend
    trendDf <- gTrend %>%
      filter(mlb_id == row$mlb_id) %>%
      mutate(snapshot_date = as.Date(snapshot_date)) %>%
      arrange(snapshot_date)
    trendUI <- if (nrow(trendDf) >= 5) {
      tags$div(style = "padding:12px 16px; border:1px solid #ddd; border-top:none;",
        tags$strong("HotScore trend (14-day window)"),
        plotly::plotlyOutput("gPlayerTrend", height = 220))
    } else {
      tags$div(style = "padding:12px 16px; border:1px solid #ddd; border-top:none; color:#888; font-size:13px;",
        "Not enough history yet for a trend chart (need 5+ daily snapshots).")
    }

    tags$div(style = "border-radius:6px; overflow:hidden;",
             headerUI, heroUI, trendUI)
  })

  # Trend plot output — paired with the renderUI above. plotly works when the
  # output is referenced inside a UI that's already on the page; renderUI
  # registers the placeholder div, and renderPlotly fills it.
  output$gPlayerTrend <- plotly::renderPlotly({
    nm <- input$gPlayerPick
    if (is.null(nm) || nm == "") return(NULL)
    row <- gRoster %>% filter(player == nm)
    if (nrow(row) == 0) return(NULL)
    trendDf <- gTrend %>%
      filter(mlb_id == row$mlb_id[1]) %>%
      mutate(snapshot_date = as.Date(snapshot_date)) %>%
      arrange(snapshot_date)
    if (nrow(trendDf) < 5) return(NULL)
    plotly::plot_ly(trendDf, x = ~snapshot_date, y = ~hotscore,
                    type = "scatter", mode = "lines+markers",
                    line = list(width = 3), marker = list(size = 8))
  })

  # Risers: players whose HotScore is trending up over the last 7 days
  # (latest − earliest ≥ 0.3 AND currently > 0), OR whose level moved up
  # in the last 7 days.
  output$gRisers <- DT::renderDataTable({
    rv$refreshCount
    if (nrow(gTrend) == 0) {
      return(datatable(data.frame(Note = "Not enough history yet for risers."),
                       options = list(dom = 't'), selection = 'none', rownames = FALSE))
    }
    # Trend on HotScore over the last 7 days. For each player: take the
    # earliest and latest snapshot in window, require an improvement of
    # at least 0.3 std-devs AND that the current score is still positive.
    trend <- gTrend %>%
      mutate(snapshot_date = as.Date(snapshot_date)) %>%
      filter(snapshot_date >= Sys.Date() - 7) %>%
      arrange(mlb_id, snapshot_date)
    streaks <- trend %>%
      group_by(mlb_id) %>%
      summarise(latest      = tail(snapshot_date, 1),
                first_score = head(hotscore, 1),
                last_score  = tail(hotscore, 1),
                delta       = tail(hotscore, 1) - head(hotscore, 1),
                .groups = "drop") %>%
      filter(delta >= 0.3, last_score > 0, latest >= Sys.Date() - 1) %>%
      arrange(desc(delta)) %>%
      left_join(gRoster %>% select(mlb_id, player, pos, level), by = "mlb_id") %>%
      mutate(reason = sprintf("HotScore %+.2f → %+.2f (Δ %+.2f)",
                              first_score, last_score, delta)) %>%
      select(Player = player, Pos = pos, Lvl = level, Reason = reason)

    # Promotions in the last 7 days (level went up)
    promos <- tryCatch({
      conn3 <- dbConnect(RSQLite::SQLite(), "../code/DAFL.db")
      on.exit(dbDisconnect(conn3))
      hist <- dbGetQuery(conn3, "
        SELECT mlb_id, player, level, snapshot_date FROM GuardiansRoster
        WHERE snapshot_date >= date(?, '-7 days')
          AND (status IS NULL OR status = 'Active')
        ORDER BY mlb_id, snapshot_date",
        params = list(as.character(Sys.Date())))
      levOrder <- c("DSL"=1,"ACL"=2,"A"=3,"A+"=4,"AA"=5,"AAA"=6,"MLB"=7)
      hist %>%
        group_by(mlb_id) %>%
        summarise(player = tail(player, 1),
                  from = head(level, 1), to = tail(level, 1),
                  .groups = "drop") %>%
        filter(!is.na(levOrder[to]), !is.na(levOrder[from]),
               levOrder[to] > levOrder[from]) %>%
        mutate(Player = player, Pos = NA_character_, Lvl = to,
               Reason = paste0("Promoted ", from, " → ", to)) %>%
        select(Player, Pos, Lvl, Reason)
    }, error = function(e) data.frame(Player = character(), Pos = character(),
                                       Lvl = character(), Reason = character()))

    out <- bind_rows(streaks, promos) %>%
      distinct(Player, .keep_all = TRUE)
    if (nrow(out) == 0) {
      return(datatable(data.frame(Note = "No risers right now."),
                       options = list(dom = 't'), selection = 'none', rownames = FALSE))
    }
    out$Reason <- htmltools::htmlEscape(out$Reason)
    out$Player <- playerLink(out$Player)
    datatable(out, options = list(pageLength = 15, dom = 'tip', autoWidth = FALSE),
              rownames = FALSE, escape = FALSE)
  })
  output$gTxnTable <- DT::renderDataTable({
    rv$refreshCount
    if (nrow(gTxn) == 0) {
      return(datatable(data.frame(Note = "No transactions in the last 14 days."),
                       options = list(dom = 't'), selection = 'none', rownames = FALSE))
    }
    df <- gTxn %>%
      select(Date = txn_date, Player = player, Type = type,
             From = from_team_id, To = to_team_id, Description = description)
    df$Description <- htmltools::htmlEscape(df$Description)
    df$Player <- playerLink(df$Player)
    datatable(df,
              options = list(pageLength = 25, filter = 'top', autoWidth = FALSE),
              filter = 'top', rownames = FALSE, escape = FALSE)
  })
  output$gILTable <- DT::renderDataTable({
    rv$refreshCount
    if (nrow(gIL) == 0) {
      return(datatable(data.frame(Note = "No active IL placements."),
                       options = list(dom = 't'), selection = 'none', rownames = FALSE))
    }
    df <- gIL %>%
      mutate(Age = ifelse(is.na(age), NA_real_, round(age, 0))) %>%
      select(Player = player, Pos = pos, Age, Status = status,
             Injury = injury, `Latest Update` = update) %>%
      arrange(Player)
    df$Injury <- htmltools::htmlEscape(df$Injury)
    df$`Latest Update` <- htmltools::htmlEscape(df$`Latest Update`)
    df$Player <- playerLink(df$Player)
    datatable(df,
              options = list(pageLength = 25, autoWidth = FALSE,
                             filter = 'top'),
              filter = 'top', rownames = FALSE, escape = FALSE)
  })

  # --- Prospects tab: Hitters / Pitchers sub-tabs ---
  # gProspects has FG identifiers; we map to mlb_id via gRoster to pick up the
  # player's current level + season line. Top prospects (Bazzana, Genao, etc.)
  # often have FG "sa..." PlayerIds that aren't in the Chadwick fg_id crosswalk,
  # so most gRoster rows have NA fg_id. We join on fg_id when both sides have
  # one, then fall back to a normalized player name for everyone else.
  prospectsBase <- function(role) {
    if (nrow(gProspects) == 0) return(data.frame())
    pr <- gProspects[gProspects$role == role, , drop = FALSE]
    if (nrow(pr) == 0) return(data.frame())
    normName <- function(x) tolower(trimws(gsub("\\s+", " ", x)))
    rosterById <- gRoster %>%
      filter(!is.na(fg_id) & nzchar(fg_id)) %>%
      transmute(fg_id    = as.character(fg_id),
                mlb_id_i = mlb_id,
                Level_i  = level,
                AgeR_i   = ifelse(is.na(age), NA_real_, round(age, 1)))
    rosterByName <- gRoster %>%
      transmute(name_key = normName(player),
                mlb_id_n = mlb_id,
                Level_n  = level,
                AgeR_n   = ifelse(is.na(age), NA_real_, round(age, 1))) %>%
      distinct(name_key, .keep_all = TRUE)
    pr$fg_id    <- as.character(pr$PlayerId)
    pr$name_key <- normName(pr$Name)
    out <- pr %>%
      left_join(rosterById,   by = "fg_id") %>%
      left_join(rosterByName, by = "name_key") %>%
      mutate(mlb_id = ifelse(is.na(mlb_id_i), mlb_id_n, mlb_id_i),
             Level  = ifelse(is.na(Level_i),  Level_n,  Level_i),
             AgeR   = ifelse(is.na(AgeR_i),   AgeR_n,   AgeR_i)) %>%
      mutate(`Org Rk`  = suppressWarnings(as.integer(Org.Rk)),
             FV        = suppressWarnings(as.integer(FV)),
             `Top 100` = ifelse(is.na(suppressWarnings(as.integer(Top.100))),
                                NA_integer_,
                                as.integer(Top.100)),
             ETA       = suppressWarnings(as.integer(ETA)),
             Age       = AgeR,
             Season    = ifelse(is.na(mlb_id), "",
                                vapply(mlb_id, seasonLineHtml, character(1))))
    out %>% arrange(desc(FV), `Org Rk`)
  }

  output$gProspectsH <- DT::renderDataTable({
    rv$refreshCount
    df <- prospectsBase("H")
    if (nrow(df) == 0) {
      return(datatable(data.frame(Note = "No hitter prospects available."),
                       options = list(dom = 't'), selection = 'none', rownames = FALSE))
    }
    df <- df %>%
      select(`Org Rk`, Player = Name, Pos, Age, Level, FV, `Top 100`,
             ETA, Risk,
             Game = Game.Pwr, Raw = Raw.Pwr, Spd,
             Season)
    df$Player <- playerLink(df$Player)
    # Player (col 1) and Season (col 12) each get 2x the share of the other
    # 11 columns: 13.3% vs 6.7%, totalling 100%.
    datatable(df,
              options = list(pageLength = 25, autoWidth = TRUE, filter = 'top',
                             columnDefs = list(
                               list(width = '13.3%', targets = c(1, 12)),
                               list(width = '6.7%',  targets = c(0,2,3,4,5,6,7,8,9,10,11)))),
              filter = 'top', rownames = FALSE, escape = FALSE)
  })

  output$gProspectsP <- DT::renderDataTable({
    rv$refreshCount
    df <- prospectsBase("P")
    if (nrow(df) == 0) {
      return(datatable(data.frame(Note = "No pitcher prospects available."),
                       options = list(dom = 't'), selection = 'none', rownames = FALSE))
    }
    df <- df %>%
      select(`Org Rk`, Player = Name, Pos, Age, Level, FV, `Top 100`,
             ETA, Risk,
             FB, SL, CB, CH, CMD, Sits, Tops,
             Season)
    df$Player <- playerLink(df$Player)
    # Player (col 1) and Season (col 16) each get 2x the share of the other
    # 15 columns: 10.5% vs 5.3%, totalling ~100%.
    datatable(df,
              options = list(pageLength = 25, autoWidth = TRUE, filter = 'top',
                             columnDefs = list(
                               list(width = '10.5%', targets = c(1, 16)),
                               list(width = '5.3%',  targets = c(0,2,3,4,5,6,7,8,9,10,11,12,13,14,15)))),
              filter = 'top', rownames = FALSE, escape = FALSE)
  })

  # Populate the player picker; re-runs whenever rv$refreshCount changes so
  # newly-added players appear after the Refresh button is pressed.
  observe({
    rv$refreshCount
    updateSelectizeInput(session, 'gPlayerPick',
                         choices = sort(unique(gRoster$player)),
                         server = TRUE)
  })
})
