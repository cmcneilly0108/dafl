# DAFL 2026 Offseason Session Summary

*Session: 2026-09-27 to 2026-09-29*

## What we learned

### Why the Crickets finish 2nd or 3rd

- **Total season value decides the standings.** Once scored correctly, value matches the final standings at 0.96. How value is spread across the roster (top-heavy or deep) doesn't matter.
- **The keeper lists are the best on paper and deliver the least.** They projected #1 in 2023–25 and #3 in 2026, but delivered **49%** of projected value over those four years. Other teams delivered 63–88%. The gap:
  - **Projections too high (−$166):** Acuña's 2024 ACL tear, pitchers (Webb, Glasnow, Skubal), and hitters aged 32–34.
  - **Keeper production that didn't count for us (−$301):**
    - produced for other teams after we traded them, mostly 2023 (Snell, Altuve, Phillips),
    - and bench time.
- **Age:** the 2026 keeper list was the league's oldest. Keepers aged 32–34 delivered 64% of projection, with half busting, while keepers 25 and under delivered 141%. Pitchers under-deliver at every age, and expensive single keepers under-deliver most.
- **Our hitters are benched more than anyone's.** We rank in the bottom 1–5 for lineup efficiency (at-bats and value) every year since 2020. It's a roster-shape problem: too many interchangeable regulars sitting while we finished last in SB.
- **Lineup changes:** discretionary swaps are close to break-even for every team. Setting lineups by hot score benches cold hitters right before they rebound, and "playing the rebound" is worse still. The best predictor blends season-to-date value, rest-of-season projection and recent playing time, but even that only gains about $4–5 a season.
- **Model issues:**
  - The draft guide's position-sheet values aren't on a consistent scale across years or between hitters and pitchers.
  - Projections carry no injury, age or pitcher-risk discount.

### Data bugs found and fixed

- **Strikeouts:** CBS's accrued "SO" column is shutouts, not strikeouts. K is now taken from the standard view, or estimated for past seasons.
- **Season-review scoring:**
  - holds were weighted at 0.6,
  - the ERA and AVG baselines were unweighted averages,
  - players were scored from the worst player, which rewarded churning the roster.
- **Duplicate and missing player ids** double-counted players (Tatis 4×). 2022's final standings were mis-mapped.
- **Other broken pieces:**
  - the `waitForFunction` timeouts were really 30 seconds,
  - `buildMaster.Rmd` leaked connections,
  - the FanGraphs fetch was blocked by Cloudflare.

## New tools and reports

| Tool / report | What it does |
|---|---|
| `faabAnalysis.r [year]` | Accurate season review for any year from 2020 on. New: lineup efficiency (at-bats and value), `benchDFL` and `benchRank`, a **Protection** tab (projected → full season → counted for the team), and a Description column on Summary Stats |
| `seasonTrends.pdf` (`seasonTrends.r`) | Year-over-year charts: ROI and projection ratios, FAAB and trade value, and the Crickets' ranks for every category against the final finish |
| `lineupAudit.r [year]` | Daily lineup rebuild: bench and IR splits for hitters and pitchers, value efficiency, weekly lineup regret, and lineup-change analysis |
| `startScore` | In My Hitters (`weeklyUpdate.xlsx`) and LeagueEval's team tab: season value + projection + recent playing time, for weekly lineup choices |
| Draft guide: Protected Players sheet | Saves every team's keeper projections each year for future checks |
| Browserless FanGraphs fetch | Plus season totals and 14-day files |
| Rebuilt 2026 preseason guide; backups | The preseason guide is recovered, and the original 2020–25 season reviews are backed up in `data/seasonReviewBackup/` |

## For next spring

- **Re-enable the launchd jobs:**
  - run `chmod +x run_protection_list.sh`,
  - and point a job at `run_inseason_pulse.sh`. The job currently named "inseasonpulse" runs `protectionList.r` instead.
- **After the draft:** save `draftGuide.xlsx` as `{year}draftGuide.xlsx`.
- **Before building a formal age and pitcher discount into protection valuation:** re-run the keeper age study with 2027 data.
- **Uncommitted:** `benchRank`, the ranks chart, the Protection tab and the Protected Players sheet.
