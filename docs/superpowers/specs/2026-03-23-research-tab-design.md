# Research Tab — LiveDraftTool

## Summary

A new tab in the LiveDraftTool Shiny app that lets the user paste a URL to a baseball article, scrapes the page, uses Claude to extract mentioned players with summaries and category tags, matches them to the player pool, filters to free agents, and displays them with full stat detail and targeting support.

## Motivation

During draft prep, you read articles from FanGraphs, ESPN, CBS, Yahoo, etc. highlighting sleepers, breakouts, and value picks. Currently there's no way to quickly cross-reference those recommendations against the DAFL player pool. This tab turns any article into an actionable, filterable player list with one click.

## Design

### UI Layout

New "Research" tab in the LiveDraftTool navbar.

**Sidebar (width 3):**
- `textInput('researchUrl', ...)` for URL
- `actionButton('analyzeBtn', 'Analyze Article', class = 'btn-primary')`
- `uiOutput('researchStatus')` for status (article title, source domain, player count)
- `actionButton('targetResBtn', 'Toggle Target', class = 'btn-info btn-sm')`

**Main panel:**
- Tabset with two sub-tabs: "Hitters" and "Pitchers" (consistent with Leaderboards and Prospects tabs)
- `DT::dataTableOutput('researchH')` — hitters table
- `DT::dataTableOutput('researchP')` — pitchers table
- Each table has Tags and Summary columns prepended to the standard stat columns
- Row selection enabled (`selection = 'single'`) for targeting
- `uiOutput('researchUnmatched')` — small text block listing players that could not be matched

### Server Flow

1. User pastes URL, clicks "Analyze Article"
2. Disable the button and show "Analyzing..." notification
3. `rvest::read_html(url)` fetches the page
4. Extract article text: `html_nodes("p, h1, h2, h3, li")` joined as plain text. For long articles, keep the first 4,000 characters, all heading text, and the first paragraph after each heading, up to ~12,000 characters total.
5. Build prompt for `callClaudeAPI()` (see Prompt Design below)
6. Check if returned string is valid JSON (starts with `[`). If it starts with "Error" or "API", treat as an error and show notification. Otherwise parse with `jsonlite::fromJSON()` into a data frame of `full_name`, `summary`, `tags`.
7. Match player names to `AllH_active()` / `AllP_active()` (respects blend mode toggle):
   - The `Player` column on these pools is plain text — no HTML stripping needed
   - First pass: exact match via `tolower(full_name) == tolower(Player)`
   - Second pass: fuzzy match via `agrep(max.distance = 0.15, ignore.case = TRUE)` for unmatched names
   - Fuzzy-matched players get a "~ " prefix on their displayed name
   - Unmatched players collected into a "could not match" list
8. Filter to free agents using `AllH_avail()` / `AllP_avail()` reactive pools (consistent with Hitters/Pitchers tabs — these already exclude drafted players)
9. Join Claude's summary + tags onto the matched player stat data
10. Store results in `rv$researchH` and `rv$researchP` reactive values
11. Display in DT tables with `escape = FALSE` for FanGraphs links, re-enable button

### Prompt Design

```
You are a baseball fantasy analyst assistant. Extract all baseball players
mentioned in the following article. For each player the author is highlighting
as a target, sleeper, breakout, value pick, or otherwise recommending, return
a JSON array with these fields:

- full_name: the player's full name (first and last)
- summary: one sentence describing why the author thinks this player is interesting
- tags: comma-separated list from these options: Sleeper, Breakout, Bounce-back,
  Value, Upside, Buy-low, Sell-high, Injury-risk, Closer, Holds, Steals, Power,
  AVG, Pitching, Strikeouts, Saves, Speed, Ratios

Only include players the author is specifically recommending or discussing
positively. Skip players mentioned only in passing or as comparisons.

Return ONLY the JSON array, no other text. Example:
[{"full_name": "Luis Arraez", "summary": "Hitting .340 in spring with strong
lineup protection boosting BA and R upside", "tags": "Sleeper, AVG, Value"}]

Article text:
{article_text}
```

### Name Matching

- Match against plain-text `Player` column in the active player pools
- Exact match: `tolower(full_name) == tolower(Player)`
- Fuzzy match fallback: `agrep(full_name, Player_names, max.distance = 0.15, ignore.case = TRUE)`
- Fuzzy-matched players displayed with "~ " prefix so the user can verify
- Apply `fgLink()` at display time only, consistent with other tabs
- Unmatched players listed below the tables

### Free Agent Filter

Use `AllH_avail()` and `AllP_avail()` reactive pools, consistent with all other tabs. These pools already exclude players on DAFL team rosters and respect the blend mode toggle.

### Targeting

- `targetResBtn` button uses row selection from the active DT sub-tab (checks `input$researchTab` to determine Hitters vs Pitchers, same pattern as Prospects/Leaderboards tabs)
- Reads `playerid` from the underlying data frame at the selected row
- Calls the existing toggle pattern: check if `pid %in% rv$targets`, add/remove, write to `targetFile`
- Players show up in the "My Targets" tab

### Display Columns

**Hitters table (`researchH`):**
- `Player` (with FanGraphs link via `fgLink()`, "~ " prefix if fuzzy-matched)
- `Pos`, `Tags`, `Summary`
- `Age`, `pDFL`, `pADP`, `pSGP`
- `pHR`, `pRBI`, `pR`, `pSB`, `pAVG`
- `Injury`, `Expected.Return`
- `playerid` (hidden, used for targeting)

**Pitchers table (`researchP`):**
- `Player` (with FanGraphs link via `fgLink()`, "~ " prefix if fuzzy-matched)
- `Pos`, `Tags`, `Summary`
- `Age`, `pDFL`, `pADP`, `pSGP`
- `pW`, `pSO`, `pSV`, `pHLD`, `pERA`, `pK/9`
- `Injury`, `Expected.Return`
- `playerid` (hidden, used for targeting)

### Error Handling

- Invalid URL / fetch failure: show notification, re-enable button, don't crash
- Claude API failure: show notification with error message
- Empty extraction (no players found): show "No players found in this article" message
- JSON parse failure from Claude: retry once with simplified prompt ("Return a JSON array of objects with fields: full_name, summary, tags. Example: [{\"full_name\":\"Mike Trout\",\"summary\":\"Still elite\",\"tags\":\"Power\"}]. Article: {first_4000_chars}"), then show error if retry also fails

## Files Modified

- `LiveDraftTool/ui.R` — add Research tab
- `LiveDraftTool/server.R` — add Research tab server logic (scrape, LLM call, matching, display, targeting)

## Dependencies

- `rvest` (loaded transitively via `draftGuide.r`)
- `jsonlite` (loaded transitively via `daflFunctions.r`)
- `httr` (loaded transitively for `callClaudeAPI`)
- `callClaudeAPI()` in `daflFunctions.r` (already exists)
- `ANTHROPIC_API_KEY` environment variable (already used)
- `fgLink()` helper in `server.R` (already exists)
- `AllH_avail()`, `AllP_avail()`, `AllH_active()`, `AllP_active()` reactives (already exist)
