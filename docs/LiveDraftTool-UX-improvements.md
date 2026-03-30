# Live Draft Tool — UX Improvements

## 1. Remove Settings auto-open; show My Team in nav bar
The Settings modal interrupting on startup is jarring. Instead, put a compact "My Team: Liquor Crickets" badge in the nav bar (always visible), defaulting to your team. Settings is there when you need it but doesn't force itself on you. This also gives persistent context — you always know which team you're managing regardless of which tab you're on.

## 2. Reorder tabs to match draft-day workflow
Current order puts Rosters 3rd and My Targets 13th. Better flow:

1. Draft
2. My Targets
3. Nominations
4. Hitters
5. Pitchers
6. Positional Pressure
7. Rosters
8. Leaderboards
9. Prospects
10. Bullpen Depth Charts
11. Injured
12. Research
13. Search / Player Snapshot

## 3. Add draft progress indicator to the nav bar
Something like "Pick 47/325 | 13 teams | $2,418 remaining" so you always know where you are in the draft regardless of which tab you're viewing.

## 4. Inline target toggle on player rows
Every table with players currently requires: select row → find button → click. An inline star or checkbox icon in each row would be one click. Consistent across all tabs.

## 5. Show active projection system and valuation mode in nav bar
A compact badge like "ATC | Projections" so you always know what's driving the numbers without opening Settings.

## 6. Evolve Search into Player Snapshot
Transform the Search tab into a one-stop "Player Snapshot" view for when a player comes up for auction. Still has the search/filter table, but when you select a player, it expands a detail panel showing:

- **Projected stats** — full stat line from the active projection system
- **Valuation** — pDFL, SGP, ADP, rankDiff across all projection systems (ATC, Steamer, BAT X)
- **Positional context** — how many teams still need this position, market pressure (from Positional Pressure tab)
- **Competition** — which teams can outbid you, their max bids (from Competition Check logic)
- **Comparable players** — other available players at the same position with similar value
- **Injury status** — if injured, show status and expected return
- **Prospect info** — if a prospect, show FV, level, ETA
- **Owner info** — if already rostered, show team, salary, contract year

The goal: when someone is nominated, you go to Player Snapshot, type the name, and immediately have everything you need to decide how much to bid — without tab-hopping.
