# Create Protection Lists Tab — Design Spec

## Summary

Add a new "Create Protection Lists" tab to the ProtectionTrades Shiny app that lets the user select up to 12 players per team and save them as that team's protection list to `{cyear}ProtectionLists.csv`.

## Tab Layout

**Sidebar:**
- Team dropdown (same team list used in "by Team" tab)
- "X/12 selected" reactive counter
- Submit button — disabled or shows validation error if >12 rows selected

**Main panel:**
- DataTable of all players on the selected team
- Columns displayed: Player, Pos, Salary, Contract, pDFL, netValue (and other useful context from `rpreds`)
- DT row selection enabled (click to select/deselect, multi-select)

## Behavior

1. **Team selection** loads that team's players from `rpreds` into the DataTable.
2. **Pre-selection:** On load, if `{cyear}ProtectionLists.csv` exists, rows matching players already protected for the selected team are pre-selected.
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
                                    join selected players back to roster data
                                    to get Rosters.csv column schema
                                                          ↓
                                    read existing CSV → remove team rows → append → write
```

## Key Implementation Details

- `cyear` is already loaded via `daflFunctions.r` — use it to construct the filename dynamically.
- The roster data (`{cyear}Rosters.csv`) is already loaded in `protectionList.r` — join selected `rpreds` rows back to roster data by `playerid` to get the correct output columns.
- DT's `selection = list(mode = 'multiple', selected = <pre-selected indices>)` handles pre-selection.
- Use `reactiveVal` for the counter and selection state.

## Files Modified

- `ProtectionTrades/ui.R` — Add new tabPanel
- `ProtectionTrades/server.R` — Add server logic for the new tab

## No Changes To

- `code/daflFunctions.r`
- `code/protectionList.r`
- Existing tabs in the app
