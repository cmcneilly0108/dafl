#!/bin/bash

# Daily FanGraphs + CBS data loads (mirrors the system() calls in inSeasonPulse.r).
# Year matches cyear in daflFunctions.r — bump each season.

YEAR="2026"

cd "/Users/cmcneilly/Dropbox/Personal/DAFL/code"

LOG_FILE="../logs/data_loads_$(date +%Y%m%d_%H%M%S).log"
mkdir -p "../logs"

run_step() {
    local name="$1"; shift
    echo "--- $name START $(date) ---" >> "$LOG_FILE"
    "$@" >> "$LOG_FILE" 2>&1
    local rc=$?
    echo "--- $name END   $(date) (exit $rc) ---" >> "$LOG_FILE"
    return $rc
}

echo "=== Starting data loads at $(date) ===" >> "$LOG_FILE"

overall=0
run_step "fgFetchInSeason" node ../scripts/fgFetchInSeason.js "$YEAR" || overall=1
run_step "salaryinfo"      bash ../scripts/salaryinfo.sh             || overall=1
run_step "cbsFetch"        node ../scripts/cbsFetch.js               || overall=1

if [ $overall -eq 0 ]; then
    echo "=== All data loads completed successfully at $(date) ===" >> "$LOG_FILE"
else
    echo "=== Data loads finished with errors at $(date) ===" >> "$LOG_FILE"
fi
exit $overall
