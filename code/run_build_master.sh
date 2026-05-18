#!/bin/bash

# Script to render buildMaster.Rmd via launchd
# Sets up proper environment and logging

cd "/Users/cmcneilly/Dropbox/Personal/DAFL/code"

LOG_FILE="../logs/build_master_$(date +%Y%m%d_%H%M%S).log"

mkdir -p "../logs"

echo "=== Starting buildMaster.Rmd at $(date) ===" >> "$LOG_FILE"

Rscript -e "rmarkdown::render('buildMaster.Rmd', output_dir = '.')" >> "$LOG_FILE" 2>&1

if [ $? -eq 0 ]; then
    echo "=== buildMaster.Rmd completed successfully at $(date) ===" >> "$LOG_FILE"
    exit 0
else
    echo "=== buildMaster.Rmd failed at $(date) ===" >> "$LOG_FILE"
    exit 1
fi
