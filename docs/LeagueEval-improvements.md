# LeagueEval Shiny App — Improvement Ideas

## 1. FAAB Bid Recommender Tab
Add a tab that shows recommended FAAB bid amounts based on pDFL, hotscore, positional scarcity, and category needs (using `pvResults`/`catSummary` already computed in inSeasonPulse.r). Instead of bouncing between tabs to cross-reference, one view says "bid $X on this player because you need SB and he's hot."

## 2. My Targets List (ported from LiveDraftTool) [IMPLEMENTED]
Port the Toggle Target / My Targets pattern from LiveDraftTool. Click a button to bookmark free agents from any tab, then see the full shortlist in a dedicated "My Targets" tab. Persisted to a CSV so targets survive app restarts.

## 3. Category-Aware Free Agent Search
Add a mode to the "By Position" tab that sorts free agents by impact on your weakest categories instead of raw pDFL. Wire `myscores` and `pvResults` into the position filter to rank by marginal category value. If you're 11th in SB, surface the high-SB free agents first.

## 4. Reactive Data Refresh [IMPLEMENTED]
Add a "Refresh Data" button that re-sources inSeasonPulse.r without restarting the app. Allows checking for hot pickups, injury updates, and stat changes throughout the week.

## 5. Head-to-Head Comparison View
Add a side-by-side comparison panel for deciding between two free agents. Select two players and see projected stats, hotscores, category contributions, and injury status next to each other. Could use the spider chart pattern from LiveDraftTool to visualize category strengths.
