#!/bin/bash
# Preseason FanGraphs projections (Steamer, ATC, THE BAT X) for the given year.
# Superseded the old cookie-based curl calls - see fgFetchAll.js.
# Usage: bash pullSteamer.sh 2027
cd "$(dirname "$0")" && node fgFetchAll.js "${1:?usage: $0 <year>}" pre
