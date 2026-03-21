#!/bin/bash

# Script to run protectionList.r via cron
# Sets up proper environment and logging

# Set the working directory
cd "/Users/cmcneilly/Dropbox/Personal/DAFL/code"

# Set up log file with timestamp
LOG_FILE="../logs/protection_list_$(date +%Y%m%d_%H%M%S).log"

# Create logs directory if it doesn't exist
mkdir -p "../logs"

# Add timestamp to log
echo "=== Starting protectionList.r at $(date) ===" >> "$LOG_FILE"

# Run the R script and capture both stdout and stderr
Rscript protectionList.r >> "$LOG_FILE" 2>&1

# Check exit status
if [ $? -eq 0 ]; then
    echo "=== protectionList.r completed successfully at $(date) ===" >> "$LOG_FILE"
    exit 0
else
    echo "=== protectionList.r failed at $(date) ===" >> "$LOG_FILE"
    exit 1
fi
