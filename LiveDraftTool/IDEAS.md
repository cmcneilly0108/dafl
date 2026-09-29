# LiveDraftTool Improvement Ideas

Draft date: ~April 8, 2026

## 2027 Priorities (from the 2026 offseason analysis)

*Added 2026-09-29. Pick these up once 2027 projections come in. Background: [2026OffseasonSummary.md](../2026OffseasonSummary.md) and [2027DFLPricingPlan.md](../2027DFLPricingPlan.md). Suggested order: 1 → 2 → 3, then 4.*

- [ ] **1. Rank players by standings points they'd add to my team** — pDFL is the same for all 13 teams, but a player's worth depends on where my roster sits in each category. In 2026, +20 SB would have been worth +3 points (SB was bunched just above us) versus +2 for +33 HR, and we lost by 4. For each remaining player, add his projected stats to my roster's projected totals, place them in last season's final standings, and show the roto points gained and points per dollar. Add it as a column in Hitters/Pitchers plus a "best for my team" list on the Draft tab. This covers the "Category projections" idea below. *Effort: medium* (reuses `calcGoals()`, projected totals, `data/fs{year}.csv`).
  - [ ] **Fix the Goals targets:** `draftGuide.r` builds them from `pgoals('../data/fs2023.csv')`, the third-place team's totals from 2023, and `fs2024.csv` was never saved. Take targets from last season's final standings automatically, and save each year's final standings file.
- [ ] **2. Market price next to value** — add `mktPrice` = pDFL + SB premium (about +$0.30–0.40 per projected steal, much more for 31+ SB) − saves (about −$0.20/SV) − holds (about −$0.18/HLD), calibrated from 2023–26 auctions. Show `value − market` as the bargain signal. Point price alerts and nomination suggestions at market price (nominate speedsters to drain budgets; wait on closers and holds relievers, who go cheap). Optional: re-estimate the premiums live from this year's picks (extend the existing surplus-by-pick chart). *Effort: small–medium.*
- [ ] **3. Bench-value warning / "startable value"** — we rostered more everyday hitters than lineup slots in 2024 and 2026, and their production sat on the bench (last in SB in 2026 with 40 SB benched). Show startable value (top 13 hitters + 12 pitchers) next to total value, and flag a pick that would push a comparable regular to the bench. *Effort: small.*
- [ ] **4. Risk-adjusted value toggle** — provisional haircuts from the keeper studies: hitters 32–34 about −35%, 29–31 about −10–15%; pitchers about −15%; pull the top projections toward the mean; flag recent injuries. Keep it a toggle (plain vs risk-adjusted) until 2027 data confirms the age effect. *Effort: small.*

## Hitters/Pitchers Tabs - Tiers & Draft Strategy

- [x] **Price tiers with color bands** — cluster players by pDFL value into tiers (e.g., elite $30+, solid $15-29, value $5-14, dollar days $1-4) with alternating background colors so you can quickly scan which tier you're shopping in
- [x] **"My targets" flagging** — let you star/bookmark players pre-draft so they're highlighted when they come up in any tab
- [ ] **Scarcity indicator** — show how many players remain at each position tier, so you know when a run is happening (e.g., "3 OF left in Tier 2")

## Rosters Page

- [ ] **Roster grade/score** — a quick letter grade or numeric score for the team based on total projected value vs salary spent
- [x] **Positional strength heatmap** — color the slot labels green/yellow/red based on how that player compares to the league average at the position
- [ ] **Budget planner auto-fill** — a button that auto-distributes your remaining budget across empty slots based on average pDFL at each position

## Draft Page

- [ ] **Draft ticker/log** — a scrolling banner showing the last 5-10 picks across all teams
- [x] **Nomination suggestions** — highlight players that other teams need but you don't, good candidates to nominate to drain their budgets
- [ ] **Price alerts** — flash when a player goes for significantly above or below their pDFL

## Overview/Analytics

- [ ] **Draft pace tracker** — how far through the draft are we, average time per pick
- [ ] **Value leaderboard** — which teams are getting the most surplus value (pDFL - Salary) so far
- [x] **Category balance view** — a radar/spider chart per team showing how balanced they are across stat categories (added to Rosters sidebar, tabbed with Goals table)

## Draft Day UX

- [ ] **Nomination queue** — a sortable list of players you want to nominate next, separate from Targets
- [ ] **Draft log** — a scrollable feed showing each pick as it happens (player, team, price, timestamp)
- [ ] **"Who needs what" summary** — a quick grid showing which teams still need which positions, visible from the Draft tab

## Analytics (New)

- [ ] **Value tracker** — running chart of avg draft price vs projected value as the draft progresses (are bargains drying up?)
- [ ] **Positional scarcity alerts** — flag when a position is about to dry up (e.g., "Only 2 closers left with pDFL > $10")
- [ ] **Category projections** — show projected standings based on current rosters (who wins each category?)

## In-Season Prep

- [ ] **Trade targets** — cross-reference your team's stat gaps with other teams' bench players to suggest trade partners

## Quality of Life

- [ ] **Keyboard shortcuts** on the Draft tab (quick-draft without clicking)
- [ ] **Dark mode** toggle
- [ ] **Export roster** to CSV or clipboard for pasting into CBS
