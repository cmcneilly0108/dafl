#!/bin/bash

# Script to run guardiansPulse.r via cron/launchd.
# Mirrors code/run_inseason_pulse.sh.

cd "/Users/cmcneilly/Dropbox/Personal/DAFL/code"

LOG_FILE="../logs/guardians_pulse_$(date +%Y%m%d_%H%M%S).log"
mkdir -p "../logs"

echo "=== Starting guardiansPulse.r at $(date) ===" >> "$LOG_FILE"

Rscript guardiansPulse.r >> "$LOG_FILE" 2>&1

if [ $? -eq 0 ]; then
    echo "=== guardiansPulse.r completed successfully at $(date) ===" >> "$LOG_FILE"
    exit 0
else
    echo "=== guardiansPulse.r failed at $(date) ===" >> "$LOG_FILE"
    exit 1
fi
