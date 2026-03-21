#!/bin/bash

# Script to run inSeasonPulse.r via cron
# Sets up proper environment and logging

# Set the working directory
cd "/Users/cmcneilly/Dropbox/Personal/DAFL/code"

# Set up log file with timestamp
LOG_FILE="../logs/inseason_pulse_$(date +%Y%m%d_%H%M%S).log"

# Create logs directory if it doesn't exist
mkdir -p "../logs"

# Add timestamp to log
echo "=== Starting inSeasonPulse.r at $(date) ===" >> "$LOG_FILE"

# Clean up any leftover processes first
pkill -f 'selenium.*4567' 2>/dev/null || true
pkill -f 'geckodriver.*4567' 2>/dev/null || true

# Run the R script and capture both stdout and stderr
Rscript inSeasonPulse.r >> "$LOG_FILE" 2>&1

# Check exit status
if [ $? -eq 0 ]; then
    echo "=== inSeasonPulse.r completed successfully at $(date) ===" >> "$LOG_FILE"
    exit 0
else
    echo "=== inSeasonPulse.r failed at $(date) ===" >> "$LOG_FILE"
    # Clean up any leftover processes on failure
    pkill -f 'selenium.*4567' 2>/dev/null || true
    pkill -f 'geckodriver.*4567' 2>/dev/null || true
    exit 1
fi