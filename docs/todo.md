# DAFL Draft Tool TODOs

## 1. Protection List Inflation Modeling

The current protection list assumes all teams act rationally with perfect information, producing ~15% inflation. In practice, actual protections come in around ~7% inflation because teams don't optimize perfectly.

**Problem:** This gap means the projected dollar values used to decide who to protect are inflated. Players who look like good protection values at 15% inflation might not be at 7%. This could cause us to over-protect expensive players and miss cheaper value picks.

**To discuss:**
- How to model "real world" inflation instead of theoretical optimal
- Should we use historical actual inflation rates as a baseline?
- Could we build a hybrid: start with theoretical, then adjust toward actual as protection lists come in?
- Does this affect the `auctionROI` parameter (currently 0.80) or is it a separate adjustment?

## 2. Nomination Strategy — MR / Compressed Value Positions

The nomination WANT/DON'T WANT logic uses positional pressure, but positions with compressed values (like MR) always show "No Pressure" because the spread between top and bottom available players is small. This makes the system recommend nominating MR targets early, which isn't ideal — you don't want to tip your hand on relievers when the price difference is minimal.

**Problem:** "No Pressure" at MR doesn't mean "safe to nominate" — it means "the position doesn't matter much." Nominating an MR you want early just invites unnecessary bidding competition for minimal strategic gain.

**To discuss:**
- Should we exclude compressed-value positions (MR, maybe CL) from the "qualifying positions" logic?
- Or weight the recommendation by the dollar value at stake — nominating a $25 OF target is worth the risk, but a $5 MR isn't
- Could use a minimum pDFL threshold for a position to count as a "want to nominate" position
- Maybe the rule should be: only nominate WANT at positions where the top available player has significant value (e.g., pDFL > $10?)
- **Key factor: team budget allocation.** The Rosters tab already has budget planning per slot. If I've budgeted $1 for MR, nominating an MR I want is low-stakes — there's no real bidding war to win or lose. The nomination strategy should factor in how much I'm planning to spend at each position. A position where I'm budgeting big dollars is worth protecting in the nomination order; a $1 position isn't. The budget data is already in `rv$budgets` / the DraftBudgets CSV.
