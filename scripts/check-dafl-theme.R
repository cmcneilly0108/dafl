#!/usr/bin/env Rscript
# Smoke check for the DAFL shared theme.
# Run from the repo root:  Rscript scripts/check-dafl-theme.R
# Compiles the theme and asserts required selectors reached the output CSS.

source("code/daflTheme.R")

theme <- dafl_theme()

# bs_theme_dependencies() compiles the Sass; collect every stylesheet it emits.
deps <- bslib::bs_theme_dependencies(theme)
css  <- paste(unlist(lapply(deps, function(d) {
  if (is.null(d$stylesheet)) return(NULL)
  paths <- file.path(d$src$file, d$stylesheet)
  paths <- paths[file.exists(paths)]
  unlist(lapply(paths, readLines, warn = FALSE))
})), collapse = "\n")

if (!nzchar(css)) stop("check-dafl-theme: theme compiled to zero bytes of CSS")

expect <- c("--dafl-clay", "--dafl-paper", "--dafl-ink",
            ".dafl-brand__mark", ".dafl-brand__season",
            "#settingsBtn", ".nav-tabs")

missing <- expect[!vapply(expect, grepl, logical(1), x = css, fixed = TRUE)]
if (length(missing)) {
  stop("check-dafl-theme: missing from compiled CSS: ", paste(missing, collapse = ", "))
}

cat("check-dafl-theme: OK -", nchar(css), "bytes of CSS,",
    length(expect), "selectors present\n")
cat("season badge resolves to:", dafl_season(), "\n")
