#!/usr/bin/env Rscript

# Test script using wdman for proper driver management
library("RSelenium")
library("wdman")
library("dplyr")
library("xml2")
library("rvest")
library("stringr")

# Load the functions from daflFunctions.r
source("./daflFunctions.r")

cat("Testing RSelenium with wdman driver management...\n")

# Clean up any existing selenium processes
tryCatch({
  system("pkill -f selenium", ignore.stderr = TRUE)
  system("pkill -f geckodriver", ignore.stderr = TRUE)
  system("pkill -f chromedriver", ignore.stderr = TRUE)
}, error = function(e) {})

tryCatch({
  cat("Using wdman to ensure correct drivers are available...\n")

  # Ensure latest GeckoDriver is downloaded
  gDrv <- wdman::gecko(version = "latest")
  cat("GeckoDriver available at:", gDrv$path, "\n")

  # Use Firefox since Chrome drivers are outdated in this wdman version
  cat("Starting rsDriver with Firefox and latest GeckoDriver...\n")
  rD <- rsDriver(
    browser = "firefox",
    port = 4567L,
    geckover = "latest",
    phantomver = NULL,
    verbose = TRUE
  )

  remDr <- rD[["client"]]
  cat("SUCCESS: Firefox browser opened via rsDriver!\n")

  # Make remDr global for the functions
  remDr <<- remDr

  # Test navigation
  cat("Testing navigation to CNN...\n")
  remDr$navigate("https://www.cnn.com")
  cat("SUCCESS: Navigation completed!\n")

  # Test the actual functions from the script
  cat("Testing getInjuriesRS()...\n")
  injOrig <- getInjuriesRS()
  cat("SUCCESS: getInjuriesRS() completed!\n")
  cat("Injuries data rows:", nrow(injOrig), "\n")

  cat("Testing getStuffRS()...\n")
  stuff <- getStuffRS()
  cat("SUCCESS: getStuffRS() completed!\n")
  cat("Stuff data rows:", nrow(stuff), "\n")

  # Clean up
  cat("Closing browser and server...\n")
  remDr$close()
  rD$server$stop()

  cat("SUCCESS: All tests completed successfully!\n")

}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  cat("Full error details:\n")
  print(e)

  # Clean up on error
  tryCatch({
    if(exists("remDr")) remDr$close()
    if(exists("rD")) rD$server$stop()
  }, error = function(e2) {})
})

cat("wdman test completed.\n")