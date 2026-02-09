#!/usr/bin/env Rscript

library("RSelenium")

cat("Testing navigation issue...\n")

tryCatch({
  rD <- rsDriver(browser="firefox",port=4567L,phantomver = NULL,
                 geckover="latest", verbose=T)
  remDr <- rD[["client"]]

  cat("Browser connected successfully\n")

  # Test different ways to navigate
  cat("Testing basic navigation...\n")
  url <- "https://www.cnn.com"
  cat("URL to navigate to:", url, "\n")

  # Try different navigation methods
  cat("Trying navigate method...\n")
  tryCatch({
    remDr$navigate(url)
    cat("Navigate method successful!\n")
  }, error = function(e) {
    cat("Navigate failed:", e$message, "\n")

    # Try with list format
    cat("Trying navigate with list format...\n")
    remDr$navigate(list(url = url))
    cat("List format successful!\n")
  })

  # Test getting page source
  cat("Getting page source...\n")
  html_source <- remDr$getPageSource()[[1]]
  cat("Page source length:", nchar(html_source), "\n")

  # Clean up
  remDr$close()
  rD$server$stop()

}, error = function(e) {
  cat("ERROR in navigation test:", e$message, "\n")
  print(e)
})

cat("Navigation test completed.\n")