# Resolve each source in researchSources.json to its current article URL.
# Run from the repo's code/ directory so daflFunctions.r loads cleanly:
#   cd code && Rscript ../.claude/skills/add-research-source/scripts/check_sources.R [name-filter]
# Optional arg: a substring to filter source names (case-insensitive).
suppressMessages({library(jsonlite); library(xml2); library(httr)})
options(warn = -1)
sink(file("/dev/null", "wt"), type = "message"); source("daflFunctions.r"); sink(type = "message")

filter <- tolower(paste(commandArgs(trailingOnly = TRUE), collapse = " "))
srcs <- jsonlite::fromJSON("../researchSources.json", simplifyDataFrame = FALSE)

for (s in srcs) {
  if (nzchar(filter) && !grepl(filter, tolower(s$name), fixed = TRUE)) next
  res <- researchLatestUrl(s)
  cat("---", s$name, "(", s$method, ") ---\n")
  if (is.null(res)) {
    cat("  NULL  (no title match or fetch error -- check method/pattern)\n")
  } else {
    cat("  Title:", res$title, "\n")
    cat("  URL:  ", res$url, "\n")
    cat("  Date: ", res$date, "\n")
  }
}
