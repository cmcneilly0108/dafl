# Year-over-year charts of the Summary Stats tab from each {year}seasonReview.xlsx
# (written by faabAnalysis.r). Output: ../seasonTrends.pdf
#   Rscript seasonTrends.r

library("openxlsx")
library("dplyr")
library("ggplot2")
library("stringr")

files <- list.files("..", pattern = "^[0-9]{4}seasonReview\\.xlsx$", full.names = TRUE)
stats <- bind_rows(lapply(files, function(f) {
  s <- read.xlsx(f, "Summary Stats")
  data.frame(Year = as.integer(substr(basename(f), 1, 4)),
             Statistic = s$Statistic, Value = as.numeric(s$Value))
})) %>% filter(!is.na(Value))

# Palette: categorical slots 1-4 (blue, orange, aqua, yellow); text/grid in neutral ink
series <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100")
ink <- "#0b0b0b"; inkMuted <- "#52514e"; grid <- "#e4e3de"; surface <- "#fcfcfb"
years <- sort(unique(stats$Year))

theme_trend <- theme_minimal(base_size = 12) +
  theme(plot.background = element_rect(fill = surface, colour = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = grid, linewidth = 0.4),
        axis.text = element_text(colour = inkMuted),
        axis.title = element_text(colour = inkMuted),
        plot.title = element_text(colour = ink, face = "bold"),
        plot.subtitle = element_text(colour = inkMuted),
        plot.caption = element_text(colour = inkMuted, hjust = 0),
        legend.position = "top", legend.justification = "left",
        legend.title = element_blank(), legend.text = element_text(colour = ink),
        plot.margin = margin(12, 12, 12, 12))

# 2020 was a 60-game season - shade it so its values aren't read like the rest
shortSeason <- annotate("rect", xmin = 2019.6, xmax = 2020.4, ymin = -Inf, ymax = Inf,
                        fill = grid, alpha = 0.6)
shortLabel <- function(y) annotate("text", x = 2020, y = y, label = "60-game\nseason",
                                   colour = inkMuted, size = 3, vjust = 1, lineheight = 0.9)

trendChart <- function(df, levels, title, subtitle, ylab, yfmt, refLine = NULL) {
  df <- df %>% mutate(Statistic = factor(Statistic, levels = levels))
  last <- df %>% group_by(Statistic) %>% filter(Year == max(Year))
  top <- max(df$Value) * 1.08
  p <- ggplot(df, aes(Year, Value, colour = Statistic, shape = Statistic)) +
    shortSeason + shortLabel(top)
  if (!is.null(refLine)) {
    p <- p + geom_hline(yintercept = refLine, colour = inkMuted, linewidth = 0.4, linetype = "dashed") +
      annotate("text", x = min(years) - 0.35, y = refLine, label = "break-even",
               colour = inkMuted, size = 3, hjust = 0, vjust = -0.5)
  }
  p + geom_line(linewidth = 0.8) +
    geom_point(size = 2.8, fill = surface, stroke = 1.2) +
    geom_text(data = last, aes(label = yfmt(Value)), hjust = -0.35, size = 3.4,
              colour = ink, show.legend = FALSE) +
    scale_colour_manual(values = setNames(series[seq_along(levels)], levels)) +
    scale_shape_manual(values = setNames(c(21, 22, 24, 23)[seq_along(levels)], levels)) +
    scale_x_continuous(breaks = years, expand = expansion(add = c(0.4, 0.7))) +
    scale_y_continuous(labels = yfmt, limits = c(0, top)) +
    labs(title = title, subtitle = subtitle, x = NULL, y = ylab) +
    theme_trend
}

ratios <- c("Protection ROI", "Draft ROI", "Protection vs Projection", "Draft vs Projection")
pRatio <- trendChart(filter(stats, Statistic %in% ratios), ratios,
  "Return on keepers and draft picks",
  "League average of season value per dollar of salary (ROI) or per dollar of preseason projected value",
  "Value ratio", function(x) sprintf("%.2f", x), refLine = 1) +
  labs(caption = paste0("The vs Projection lines need that year's preseason draft guide - none saved for 2020-2022.\n",
                        "Draft vs Projection is a model diagnostic: the guide's player values aren't on a consistent scale across years."))

dollars <- c("FAAB Value", "Trade Value")
pDollar <- trendChart(filter(stats, Statistic %in% dollars), dollars,
  "Value added in-season",
  "League average per team, in DFL dollars, produced by FAAB pickups and by players acquired in trades",
  "DFL dollars per team", function(x) sprintf("$%.0f", x)) +
  labs(caption = "Trade Value counts production after the trade only; it does not subtract players traded away.")

# ---- My ranks by season: one small panel per valueByAcq rank column, each with
# the final finish as a dashed reference. 1 = best in every rank column.
myTeam <- "Liquor Crickets"
rankCols <- c(value = "Total value", prank = "Protection ROI", drank = "Draft ROI",
              frank = "FAAB value", trank = "Trade value (net)", benchRank = "Bench value (least left)",
              irank = "Injuries (fewest IR moves)")
ranks <- bind_rows(lapply(files, function(f) {
  v <- read.xlsx(f, 1)
  v$value <- rank(-v$overall)
  r <- v[v$Team == myTeam, ]
  if (nrow(r) == 0) return(NULL)
  data.frame(Year = as.integer(substr(basename(f), 1, 4)), teams = nrow(v), Actual = r$Actual,
             metric = names(rankCols), rank = unlist(r[1, intersect(names(rankCols), names(r))])[names(rankCols)])
})) %>% mutate(metric = factor(rankCols[metric], levels = rankCols))
yearLabels <- ranks %>% distinct(Year, teams) %>% arrange(Year) %>% mutate(lab = paste0("'", substr(Year, 3, 4)))
leagueSizes <- yearLabels %>% group_by(teams) %>%
  summarize(yrs = paste0(min(Year), if (n() > 1) paste0("-", substr(max(Year), 3, 4)) else ""), .groups = "drop") %>%
  arrange(yrs) %>% mutate(txt = paste0(yrs, ": ", teams)) %>% pull(txt) %>% paste(collapse = ", ")
pRanks <- ggplot(ranks, aes(Year)) +
  geom_line(aes(y = Actual, linetype = "Final finish"), colour = inkMuted, linewidth = 0.6) +
  geom_line(aes(y = rank, linetype = "Rank"), colour = series[1], linewidth = 0.8, na.rm = TRUE) +
  geom_point(aes(y = rank), colour = series[1], fill = surface, shape = 21, size = 2.4, stroke = 1.1, na.rm = TRUE) +
  geom_text(aes(y = rank, label = rank), colour = ink, size = 2.7, vjust = -1.1, na.rm = TRUE) +
  scale_linetype_manual(values = c("Rank" = "solid", "Final finish" = "dashed"),
                        guide = guide_legend(override.aes = list(colour = c(inkMuted, series[1])))) +
  scale_y_reverse(breaks = c(1, 4, 8, 12, 16), limits = c(17.5, -1)) +
  scale_x_continuous(breaks = yearLabels$Year, labels = yearLabels$lab, expand = expansion(add = 0.4)) +
  facet_wrap(~ metric, ncol = 4) +
  labs(title = str_c(myTeam, ": ranks by season"),
       subtitle = "Rank among all teams in each season review category (1 = best); dashed line = final standings finish",
       x = NULL, y = "Rank (1 = best)",
       caption = str_c("Trade value is net of players traded away. Bench value ranks the least production left on the bench (whole-season players).\n",
                       "Teams per season - ", leagueSizes, ".")) +
  theme_trend + theme(strip.text = element_text(colour = ink, face = "bold", hjust = 0),
                      axis.text.x = element_text(size = 8), panel.spacing = unit(1, "lines"))

pdf("../seasonTrends.pdf", width = 10, height = 6.5)
print(pRatio)
print(pDollar)
print(pRanks)
invisible(dev.off())
message("Wrote ../seasonTrends.pdf (", min(years), "-", max(years), ")")
