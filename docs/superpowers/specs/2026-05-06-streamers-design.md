# Streamers tab

## Context

The LeagueEval Shiny app helps the user manage a season-long DAFL roster. A common in-season move is **streaming** — dropping a fading player and picking up a free agent who is hot in a specific category for the next 2-3 weeks. Today the app has no view that surfaces "who is hot *in stat X* right now," so the user falls back to eyeballing the Free Agents and Hot tables.

The data needed is already on disk. `AllHitters.csv` is a CBS "All Players Last 14 Days" export, and `hotScores()` in `daflFunctions.r` already computes per-stat z-scores against that 14-day window. The function then sums them into a single `zScore` (renamed `hotscore` downstream) and *discards* the per-stat columns. Exposing those columns and rendering them in a sortable table is enough to give the user the streaming view.

## User-facing change

A new top-level navbar tab `"Streamers"`, placed between `"Injured"` and `"My Targets"` in `LeagueEval/ui.R`. Placement reflects the FA-acquisition flow: Injured (who needs replacing) → Streamers (who to pick up) → My Targets (who I'm watching).

Layout (`verticalLayout`):

```
h2("Streamers — last 14 days")
checkboxInput("faStreamers", "Free Agents Only", value = TRUE)
h3("Hitters")
DT::dataTableOutput("streamersHitters")
h3("Pitchers")
DT::dataTableOutput("streamersPitchers")
```

Stacking hitters and pitchers vertically (rather than side-by-side columns) preserves full table width so all z-score columns are visible without horizontal scroll.

## Data plumbing

`hotScores()` (`code/daflFunctions.r:911`) already mutates the per-stat z-scores `zHR, zR, zRBI, zSB, zxH` (hitters) and `zW, zSO, zHLD, zSV, zxER` (pitchers). The final `select` at line 970-971 drops them. The change is to widen that select.

To preserve backward compatibility with the four existing callers, add a parameter `withZ = FALSE`. When `TRUE`, the returned `bhitters` / `bpitchers` data frames include the per-stat z-scores in addition to `playerid` and `zScore`.

```r
hotScores <- function(toph, topp, tm = FALSE, withZ = FALSE) { ... }
```

Then in `inSeasonPulse.r` (both call sites at line 247 and 945), pass `withZ = TRUE` so the per-stat z-scores ride along on the existing `left_join` into `AllH` / `AllP`.

### Defensive check before implementation

Verify that none of the new column names (`zHR, zR, zRBI, zSB, zxH, zW, zSO, zHLD, zSV, zxER`) already exist on `AllH` or `AllP` from prior joins. If any collide, the join produces `.x` / `.y` suffixes and the downstream `select` will silently miss them. The plan should include a `colnames(AllH)` check at the relevant point in `inSeasonPulse.r`.

### Other callers

- `code/faabAnalysis.r:88` — calls without `withZ`, default `FALSE` preserves current behavior.
- `code/draftGuideLive.r:210` — same.
- `LiveDraftTool/server.R:2477` — same.

No changes needed at any of these sites.

## Hitters table (`streamersHitters`)

### Shape

- **Rows:** every row in `AllH` (filtered to `Team == "Free Agent"` when `input$faStreamers == TRUE`). Sorted by `-pDFL` on initial render.
- **Columns** (visible):
  ```
  Target | Player | Pos | Team | AB | HR | R | RBI | SB | AVG | zHR | zR | zRBI | zSB | zxH | pDFL
  ```
- **Hidden columns:** `playerid`, `isTarget` (carried for `markTargets()` and the toggle handler).

### Formatting

- `formatRound(c('zHR','zR','zRBI','zSB','zxH'), 2)`
- `formatRound('AVG', 3)`
- `formatCurrency('pDFL')`
- `formatRound('AB', 0)`

## Pitchers table (`streamersPitchers`)

### Shape

- **Rows:** every row in `AllP`, same FA filter.
- **Columns** (visible):
  ```
  Target | Player | Pos | Team | INN | W | K | S | HD | ERA | zW | zSO | zSV | zHLD | zxER | pDFL
  ```
- **Hidden columns:** `playerid`, `isTarget`.

### Formatting

- `formatRound(c('zW','zSO','zSV','zHLD','zxER'), 2)`
- `formatRound('ERA', 2)`
- `formatCurrency('pDFL')`
- `formatRound(c('INN','W','K','S','HD'), 0)`

## Reactives

Two `DT::renderDataTable` blocks in `LeagueEval/server.R`, modeled on the existing My Targets renderer (`server.R:709-724`):

```r
output$streamersHitters <- DT::renderDataTable({
  rv$refreshCount
  rv$targets
  df <- AllH
  if (isTRUE(input$faStreamers)) df <- filter(df, Team == 'Free Agent')
  df <- df %>% arrange(desc(pDFL))
  df <- markTargets(df, isolate(rv$targets))
  df <- df %>% select(Target, Player, Pos, Team, AB, HR, R, RBI, SB, AVG,
                      zHR, zR, zRBI, zSB, zxH, pDFL,
                      -playerid, -isTarget)
  datatable(df,
            options = list(pageLength = 25, autoWidth = FALSE, info = FALSE),
            filter = 'top', escape = FALSE) %>%
    formatCurrency('pDFL') %>%
    formatRound(c('zHR','zR','zRBI','zSB','zxH'), 2) %>%
    formatRound('AVG', 3) %>%
    formatRound('AB', 0)
})
```

Pitcher block is symmetric.

`filter = 'top'` enables DT's per-column filter row, which lets the user narrow further (e.g., position regex, AB threshold) without dedicated UI.

## Out of scope (v1)

- **Trend direction.** Would require a per-stat history. Today only the overall `hotscore` is logged in the SQLite `Trending` table. Adding per-stat history is a separate spec.
- **Position-eligibility filter.** Use the DT column filter on `Pos` for now.
- **Min-AB / min-INN floor.** Deferred per user direction — see how the unfiltered table looks first; small-sample noise can be addressed if it proves to be a problem.
- **AI summary / recommendations.** Stay declarative — the user is the decision-maker.

## Risks

1. **Column name collision** in the `hotScores()` widening (see Defensive check above). Most likely path: implementation grep finds no collision, and the plumbing change is one-line per call site.
2. **`AVG` and `ERA` computed from small samples** can be misleading (e.g., `1-for-2 = .500`). Acknowledged in v1; a min-AB / min-INN floor can be added later if it bites.
3. **`AllP$Pos`** is overwritten by `inSeasonPulse.r:180` to `CL`/`SP`/`MR` based on usage. The `Pos` column shown in the pitcher table will reflect that, not the eligibility roster slot. Consistent with other LeagueEval tabs, so not a regression.
