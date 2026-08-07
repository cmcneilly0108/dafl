# code/daflTheme.R — shared visual theme for the DAFL Shiny apps.
#
# Sourced from each app's global.R, which Shiny always runs first with the
# working directory set to the app directory. Everything the theme needs is
# read into memory here, so later setwd() calls in server.R cannot break it.

library(bslib)

# --- locate files relative to whichever directory we were sourced from -------
.dafl_find <- function(name) {
  for (p in c(file.path("../code", name), file.path("code", name), name)) {
    if (file.exists(p)) return(normalizePath(p))
  }
  stop("daflTheme.R: cannot locate ", name, " from ", getwd())
}

DAFL_SCSS <- paste(readLines(.dafl_find("dafl.scss"), warn = FALSE), collapse = "\n")

# --- season for the navbar badge --------------------------------------------
# Read cyear straight out of daflFunctions.r; fall back to the calendar year.
dafl_season <- function() {
  out <- tryCatch({
    src <- readLines(.dafl_find("daflFunctions.r"), warn = FALSE)
    hit <- grep('^\\s*cyear\\s*(<-|=)\\s*["\']([0-9]{4})["\']', src, value = TRUE)[1]
    if (is.na(hit)) NA_character_ else sub('.*["\']([0-9]{4})["\'].*', "\\1", hit)
  }, error = function(e) NA_character_)
  if (is.na(out)) format(Sys.Date(), "%Y") else out
}

# --- navbar brand ------------------------------------------------------------
# Rendered on the left so it cannot collide with the absolutely-positioned
# Settings buttons that sit at right:15px in several apps.
dafl_brand <- function(subtitle) {
  shiny::tagList(
    shiny::tags$span(class = "dafl-brand",
      shiny::tags$span(class = "dafl-brand__mark", "DAFL"),
      shiny::tags$span(class = "dafl-brand__sub", subtitle),
      shiny::tags$span(class = "dafl-brand__season", paste0("⚾ ", dafl_season()))
    )
  )
}

# --- theme -------------------------------------------------------------------
dafl_theme <- function() {
  bs_add_rules(
    bs_theme(
      version   = 5,
      bg        = "#FBF8F3",
      fg        = "#1B2A41",
      primary   = "#1B2A41",
      secondary = "#6B7789",
      success   = "#1E7A4B",
      danger    = "#B3402F",
      base_font    = font_google("Inter", wght = c(400, 500, 600), local = TRUE),
      heading_font = font_google("Zilla Slab", wght = c(400, 600), local = TRUE),
      "link-color"    = "#C8663A",
      "border-color"  = "#E4DDD2",
      "border-radius" = "6px"
    ),
    DAFL_SCSS
  )
}
