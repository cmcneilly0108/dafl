# DFL Pricing Plan for 2027

*Written 2026-09-29 as a working note. Evidence comes from the 2023–26 auctions, the season reviews and the keeper studies in [2026OffseasonSummary.md](2026OffseasonSummary.md).*

## Two prices, not one

Right now pDFL tries to be both *what a player is worth to the standings* and *what to bid*. The data shows the room pays differently from value in predictable ways (SB up; saves and holds down). So the plan is to keep two numbers:

- **Value (pDFL):** projected contribution to the standings. Used for keeper decisions and lineup/start score.
- **Market price (new, e.g. `mktPrice`):** value adjusted for what this league actually pays. Used for draft bids and nomination strategy.

Where they differ is the opportunity: target players whose value is above their market price, and don't get priced out of a category you need (SB in 2026).

## Current settings (where pricing lives)

| Setting | Where | Value | Note |
|---|---|---|---|
| Holds weight, dollar model | `preLPP()` / `preLPP2()` in `daflFunctions.r` | `0.5 * zHLD` | This is what pDFL uses |
| Holds weight, SGP | `pitSGP()` | `hdiscount <- 0.2` | Code comment: "I forget why I'm multiplying Holds by 0.2" |
| Holds weight, hot score | `hotScores()` | `0.6 * zHLD` | In-season only, not pricing |
| Holds weight, season review | `seasonScores()` in `faabAnalysis.r` | 1.0 | Evaluation uses full weight, which is correct for measuring value |
| Hitter/pitcher dollar split | `hpratio` | 0.38 (global, `preLPP`) vs **0.35** (`preLPP2`) | Inconsistent, so pick one |
| Auction ROI | `auctionROI` | 0.80 | |
| Stale code | `dociiDollars()` | hitter z-score leaves out runs (R) | Looks unused; delete or fix |

## Evidence: what the league pays vs the model (2023–26 auctions, about 700 drafted players)

Price was fitted against the guide's projected DFL within each year, separately for hitters and pitchers. The coefficients are what the room pays per projected unit **beyond** the model.

**Hitters:** SB **+$0.32 per projected steal** (t = 3.8); HR +$0.11 (not significant).

| Projected SB | Players | Avg price | Avg model | Median over model |
|---|---|---|---|---|
| 0–5 | 144 | $10 | $6 | +$1.5 |
| 6–12 | 93 | $11 | $8 | −$2.9 |
| 13–20 | 49 | $20 | $9 | +$1.6 |
| 21–30 | 36 | $23 | $11 | +$1.4 (mean +$3.8) |
| **31+** | 6 | **$47** | **$15** | **+$20** |

The SB premium by year is +$0.69 (2023), +$0.28 (2024), +$0.12 (2025) and +$0.41 (2026). It's always positive and largest for elite base-stealers.

**Pitchers:** SV **−$0.20 per projected save**; HLD **−$0.18 per projected hold**, on top of the existing 0.5 discount.

| Role | Players | Avg price | Avg model | Median over model |
|---|---|---|---|---|
| CL | 65 | $8.5 | $14.6 | −$2.9 |
| MR | 86 | $4.4 | $5.5 | −$2.5 |
| SP | 219 | $10.3 | $10.2 | +$0.1 |

Middle relievers projected for 16–25 holds sell for about $4.9 against a model price of $8.2.

## Planned adjustments

### Market price (draft bidding)
1. **SB boost (market only):** add about **$0.30–0.40 per projected steal**, with extra for elite speed (31+ SB goes for about $20 over the model). Recalibrate each spring from the prior auctions. The standings don't support raising SB in value (see "Settled" below).
2. **Holds:** price at about 60% of the current model, which means an effective hold weight of roughly 0.25–0.3 instead of 0.5.
3. **Saves:** about −$0.20 per projected save, so closers price about $3 under the model.
4. **Use it to find value:** closers and holds relievers are where you get more value than you pay for. Elite steals are where you pay more than value. Plan the auction budget around that instead of sticking to model prices.

### Value (keepers and projections)
5. **Age discount for hitters:**
   - 29–31: about −10–15%
   - 32–34: about −35%

   2026 keepers aged 32–34 delivered 64% of projection, and half busted. Across 2022–26, hitters 29+ kept about 70% of the prior season's value versus about 90% at 25 and under. Keepers 25 and under *beat* projection (141%), so consider a small boost for them.
6. **Pitcher risk haircut:** about −$5, or about 15%, compared with hitters with the same projection. Pitchers keep about 74% of prior value at every age, and 30–40% bust.
7. **Shrink the top projections:** the most highly projected keepers miss by the most (−$0.78 per projected $ in the 2026 regression). Pull the top of the projections toward the mean, for example by 10–20% of the amount above the median.
8. **Injury risk:** there's no injury discount today (Acuña 2024: projected $72, delivered $0). Consider a discount based on recent IL history.
9. **Fix scale consistency:** the position-sheet DFL flipped between years. Pitchers were worth about twice hitters in 2023–25, and the reverse in the 2026 rebuild. Also reconcile the `hpratio` values (0.38 vs 0.35) and the three holds weights (0.2, 0.5, 0.6).

### Category balance (draft plan)
10. **Category targets from the standings, not prices:** 2026 finished last in SB (1 point) with 40 SB left on the bench, when +20 SB would have been worth +3 points. Plan the draft and FAAB with the team-specific "units needed for the next points" view, rather than only chasing total value.

## Settled: is SB under-valued, or does the room overpay?

The value of one SB was compared in two places:
- **In the standings:** units needed per roto point (SGP), from the final standings.
- **In the pricing model:** the z-score weight, 1 ÷ standard deviation across the top-170 hitter pool.

| One SB is worth… | HR | R | RBI |
|---|---|---|---|
| In the standings (2021–23, 2025–26 average) | 1.15 | 2.42 | 2.76 |
| In the pricing model (2023–26 average) | 1.29 | 3.88 | 3.80 |

**Answer: the room overpays.** The model already weights SB at or above its standings value. So the SB boost goes in **market price only**, not in value. Watch one thing: in 2025–26 the standings put SB slightly above the model relative to HR (1.30/1.20 vs 1.11/1.07). Re-check with 2027.

**What actually hurt in 2026 was category position, not price.** SB was bunched just above us, which made marginal steals cheap for our team specifically:

| 2026 Crickets | Points gained |
|---|---|
| +6 SB | +1 |
| +20 SB | +3 |
| +40 SB (what sat on our bench) | +7 |
| +11 HR / +33 HR | +1 / +2 |

We lost by 4 points. **Action:** add a team-specific category plan. Extend the `CategoryPoints` sheet in `weeklyUpdate.xlsx` (and use it at the draft) to show, per category, the units needed for the next 1, 2 and 3 points. Use it for targeting and FAAB, not for global prices.

## Open questions
- Should holds and saves be discounted in **value** too, or only in price? The season review scores them at full weight and value tracks the standings at 0.96, which suggests full weight is right for value.

## How to validate in 2027
- After the 2027 draft, re-run the price fit (the price-vs-projection check above) with 2027 added.
- End of 2027: re-run the keeper age study (the Protected Players sheet in `2027draftGuide.xlsx`), and check the Protection tab and the Draft/Protection vs Projection trends in `seasonTrends.pdf`.
- Save `draftGuide.xlsx` as `2027draftGuide.xlsx` right after the draft.
