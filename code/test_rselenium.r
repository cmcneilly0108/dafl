#!/usr/bin/env Rscript

# Test script for RSelenium browser connection on macOS
library("RSelenium")
library("netstat")
library("dplyr")
library("xml2")
library("rvest")
library("stringr")

# Load the functions from daflFunctions.r
source("./daflFunctions.r")

# Test the exact code block that's failing
cat("Testing RSelenium browser connection...\n")

tryCatch({
  cat("Starting Firefox with latest geckodriver...\n")
  rD <- rsDriver(browser="firefox",port=4567L,phantomver = NULL,
                 geckover="latest", verbose=T)

  # Make remDr global so the functions can access it
  remDr <<- rD[["client"]]

  cat("SUCCESS: Firefox connection established!\n")

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
  cat("Closing browser...\n")
  remDr$close()
  rD$server$stop()

  cat("SUCCESS: All tests completed successfully!\n")

}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  cat("Full error details:\n")
  print(e)
})

cat("Test completed.\n")