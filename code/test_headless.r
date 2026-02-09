#!/usr/bin/env Rscript

# Test headless Firefox for RSelenium
library("RSelenium")
library("wdman")

cat("Testing headless Firefox...\n")

# Kill any existing processes
system("pkill -f selenium", ignore.stderr = TRUE)
system("pkill -f geckodriver", ignore.stderr = TRUE)
Sys.sleep(2)

tryCatch({
  # Ensure latest GeckoDriver
  gDrv <- wdman::gecko(version = "latest")

  cat("Starting Firefox in headless mode...\n")
  rD <- rsDriver(
    browser = "firefox",
    port = 4567L,
    phantomver = NULL,
    geckover = "latest",
    extraCapabilities = list(
      "moz:firefoxOptions" = list(
        args = list('--headless', '--width=1920', '--height=1080')
      )
    ),
    verbose = TRUE
  )

  remDr <- rD[["client"]]
  cat("SUCCESS: Headless Firefox started!\n")

  # Test navigation
  cat("Testing navigation to Google...\n")
  remDr$navigate("https://www.google.com")
  Sys.sleep(2)

  title <- remDr$getTitle()[[1]]
  cat("Page title:", title, "\n")

  # Cleanup
  remDr$close()
  rD$server$stop()

  cat("Test completed successfully!\n")

}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  # Cleanup
  system("pkill -f selenium", ignore.stderr = TRUE)
  system("pkill -f geckodriver", ignore.stderr = TRUE)
})