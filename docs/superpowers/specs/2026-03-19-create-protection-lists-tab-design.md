# Create Protection Lists Tab — Design Spec

## Summary

Add a new "Create Protection Lists" tab to the ProtectionTrades Shiny app that lets the user select up to 12 players per team and save them as that team's protection list to `{cyear}ProtectionLists.csv`.

## Tab Layout

**Sidebar:**
- Team dropdown (same team list used in "by Team" tab)
- "X/12 selected" reactive counter
- Submit button — disabled via `shinyjs` when >12 rows selected

**Main panel:**
- DataTable of all players on the selected team
- Columns displayed: Player, Pos, Salary, Contract, pDFL, netValue (and other useful context from `rpreds`)
- DT row selection enabled (click to select/deselect, multi-select)

## Behavior

1. **Team selection** loads that team's players from `rpreds` into the DataTable.
2. **Pre-selection:** On load, if `{cyear}ProtectionLists.csv` exists, rows matching players already protected for the selected team are pre-selected (matched by `playerid`). Players in the CSV but no longer on the team's roster in `rpreds` are silently skipped.
3. **Row selection** highlights rows. Counter updates reactively.
4. **Submit** validates ≤12 selected, then:
   - Reads existing `{cyear}ProtectionLists.csv` (or creates empty data frame if file doesn't exist)
   - Removes all rows for the selected team
   - Appends newly selected players
   - Writes the full file back to disk
5. **Feedback:** Success/error notification via `showNotification()`.

## Output File

- **Path:** `../{cyear}ProtectionLists.csv` (project root, alongside other data files)
- **Column schema** matches `{cyear}Rosters.csv`:
  - `,Player,Pos,Team,Salary,Contract,MLB,playerid` (with row-number index column)
- **File creation:** If file doesn't exist on submit, create it with header + selected players.
- **Idempotent:** Submitting for a team replaces only that team's rows; other teams' rows are preserved.

## Data Flow

```
rpreds (loaded in server) → filter by selected team → display in DT
                                                          ↓
                                                    user selects rows
                                                          ↓
                                                    Submit clicked
                                                          ↓
                                              validate ≤12 selections
                                                          ↓
                                    re-read {cyear}Rosters.csv in server.R,
                                    join selected playerids to get output columns
                                                          ↓
                                    read existing CSV → remove team rows → append → write
```

## Key Implementation Details

- `cyear` is already loaded via `daflFunctions.r` — use it to construct the filename dynamically.
- **Roster data for output:** `rpreds` lacks the `MLB` column needed for the output schema. On submit, server.R reads `{cyear}Rosters.csv` directly (via `read.csv(paste0("../", cyear, "Rosters.csv"))`) and joins selected `playerid`s to extract the output columns. This avoids modifying `loadData()` or `protectionList.r`.
- **File I/O working directory:** All reads/writes to `{cyear}ProtectionLists.csv` happen in the `observeEvent` handler (working directory = `ProtectionTrades/`), not inside `loadData()`.
- DT's `selection = list(mode = 'multiple', selected = <pre-selected indices>)` handles pre-selection. Index computation (`which(team_df$playerid %in% protected_ids)`) must be reactive when the team changes.
- Use `shinyjs` for disabling the Submit button when >12 selected.

## Files Modified

- `ProtectionTrades/ui.R` — Add new tabPanel, add `shinyjs::useShinyjs()` if not present
- `ProtectionTrades/server.R` — Add server logic for the new tab

## No Changes To

- `code/daflFunctions.r`
- `code/protectionList.r`
- Existing tabs in the app
