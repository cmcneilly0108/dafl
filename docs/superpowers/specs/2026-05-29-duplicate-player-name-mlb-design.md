# Player overrides for ambiguous (name, MLB team) pairs

## Background

CBS and FanGraphs identify players differently. CBS rosters use a
free-form `Player` string like `Jared Jones 1B | PIT`; FG uses a
stable numeric `playerid`. The bridge is `code/daflFunctions.r ::
addPlayerid()`, which inner-joins CBS rows to `mymaster.csv` on
`(Player, MLB)` with `relationship = "many-to-many"`, then falls back
to name-only for unmatched rows.

This breaks whenever two real MLB players share the same name and team
in the same season. Currently:

- **CBS** `AllHitters.csv` has `Jared Jones 1B | PIT` owned by Hogan's
  Heroes, and `AllPitchers01.csv` has `Jared Jones P | PIT` owned by
  Liquor Crickets.
- **mymaster.csv** has only the SP (`playerid 27863`, PIT).
- The 1B is **not** in FG at all (no row in `atcHROS.json`).

`addPlayerid()` stamps both CBS rows with `playerid 27863`. Downstream
`inner_join`s and many-to-many `left_join`s on `playerid` then either
duplicate the SP onto Liquor Crickets or leak the 1B's salary/Avail
into Liquor Crickets' roster view via `buildRoster()` in
`LeagueEval/server.R:302`.

Historical examples of the same shape: Luis Garcia (HOU vs WSN), José
Ramírez (CLE vs SEA), Josh Bell (PIT — comments in
`code/inSeasonPulse.r:176` still say "AllH - too many josh bell").

## Goals

- Each real human gets a distinct `playerid` (FG ID when available,
  `sa_`-prefixed synthetic ID otherwise) in every CBS-derived table.
- Jared Jones (1B) no longer appears on Liquor Crickets in
  LeagueEval's Trade Eval roster.
- Adding a new collision is a one-line edit to a curated CSV — no code
  change needed.
- A diagnostic surfaces every currently-known collision so the
  override file can be seeded.

## Non-goals

- **Not** restructuring `mymaster.csv` to carry a `role` column or
  splitting it into hitter/pitcher tables (that's Approach A, deferred).
- **Not** changing FG-side matching in `read.fg()`; FG already uses
  stable IDs.
- **Not** rewriting `read.cbs()` callers in other Shiny apps
  (`draftTool`, `ProtectionTrades`, `Guardians`); they will pick up
  the new behavior transparently because they share `daflFunctions.r`.

## Solution

A curated CSV of (name, MLB team, CBS position) → canonical playerid
overrides, applied inside `addPlayerid()` before the existing
name+MLB master join. Rows that hit an override skip the ambiguous
join entirely; everything else flows through unchanged.

### Components

1. **`data/playerOverrides.csv`** — the curated override file.
2. **`code/findAmbiguousPlayers.r`** — one-shot diagnostic that lists
   every current `(Player, MLB)` collision across CBS files and
   mymaster.
3. **`code/daflFunctions.r :: addPlayerid()`** — modified to consult
   overrides first.
4. **`mymaster.csv`** — backfilled with one row per synthetic
   `sa_`-prefixed playerid introduced by overrides, so downstream
   code that joins back to master doesn't NA-out.

### Component 1: `data/playerOverrides.csv`

Columns:

| col | type | example | meaning |
|---|---|---|---|
| `cbs_name` | string | `Jared Jones` | output of `stripName()` on the CBS `Player` field |
| `mlb_team` | string | `PIT` | output of `pullMLB()` after the WAS→WSN etc. remapping in `read.cbs()` |
| `cbs_pos` | string | `1B` | output of `pullPos()` on the raw CBS `Player` field (so `P` becomes `RP`, `CF`/`LF`/`RF` become `OF`) |
| `playerid` | string | `sa_jaredjones_1b` | canonical ID for this human; real FG ID if known, otherwise `sa_<lowercase-name-no-space>_<lowercase-cbs_pos>`. The leading underscore distinguishes our synthetic IDs from FG's existing `sa#####` minor-league IDs already present in master. |
| `note` | string | `2026: Pirates 1B prospect, distinct from SP #27863` | free-text rationale |

Matching key is `(cbs_name, mlb_team, cbs_pos)`. Exact match, no
regex — keeps the file readable and the join simple.

Seed contents (from running the diagnostic against current data —
final list TBD by running the script in the implementation phase, but
expected to include at minimum):

```csv
cbs_name,mlb_team,cbs_pos,playerid,note
Jared Jones,PIT,1B,sa_jaredjones_1b,2026: Pirates 1B prospect distinct from SP playerid 27863
```

### Component 2: `code/findAmbiguousPlayers.r`

A standalone R script that sources `daflFunctions.r` (for `read.cbs`'s
parsing helpers but not its join behavior — see implementation note
below) and reports every collision shape that could mismatch.

Inputs:
- `../AllHitters.csv`
- `../AllPitchers01.csv`
- `../AllHYTD.csv`
- `../AllPYTD02.csv`
- `../poselig.csv`
- `../mymaster.csv` (via the already-loaded `master` object)

For each CBS file, parses raw `Player` strings into
`(cbs_name, mlb_team, cbs_pos)` using `stripName`, `pullMLB`, `pullPos`
plus the team-abbreviation remapping already in `read.cbs()`.

Output: `data/ambiguousPlayers.csv` with one row per detected issue.
Columns:

| col | meaning |
|---|---|
| `cbs_name` | parsed name |
| `mlb_team` | parsed MLB team |
| `kind` | one of `cbs_multi_pos`, `master_multi_row`, `unmatched` |
| `details` | comma-joined positions / playerids / file names that triggered the flag |

Three detection rules:

- **`cbs_multi_pos`** — same `(cbs_name, mlb_team)` appears with 2+
  distinct `cbs_pos` values across CBS files. (Jared Jones: `1B`, `RP`.)
- **`master_multi_row`** — `mymaster.csv` has 2+ rows with the same
  `(cbs_name, MLB)`.
- **`unmatched`** — CBS row whose `(cbs_name, mlb_team)` has no row at
  all in master. Surfaces missing-master cases that today silently
  fall through to the name-only fallback.

The script also prints a summary to stdout: counts per kind, plus the
full table of `cbs_multi_pos` rows (the ones most likely to need
overrides).

Implementation note: the script must not rely on the modified
`addPlayerid()`, because that would mask `unmatched` cases. It should
duplicate the small parsing block from `read.cbs()` (5 lines) or
extract that into a helper `parseCbsPlayer()` and have both
`read.cbs()` and the diagnostic call it.

### Component 3: `addPlayerid()` modification

Current shape (`daflFunctions.r:780`):

```r
addPlayerid <- function(df) {
  m2 <- select(master,-Pos,-Player) %>% dplyr::rename(Player=cbs_name)
  gfull  <- inner_join(df, m2, by=c('Player','MLB'), relationship="many-to-many")
  dfleft <- anti_join (df, m2, by=c('Player','MLB'))
  m2     <- anti_join (m2, df, by=c('Player','MLB'))
  gname  <- left_join (dfleft, m2, by=c('Player'), relationship="many-to-many")
  gname  <- select(gname, -MLB.x) %>% dplyr::rename(MLB=MLB.y)
  final  <- rbind(gfull, gname)
  final
}
```

New shape:

```r
addPlayerid <- function(df) {
  # 0. Apply curated overrides first.
  ov <- loadPlayerOverrides()   # cached at file scope
  if (nrow(ov) > 0 && all(c('Player','MLB','Pos') %in% colnames(df))) {
    keyed <- df %>%
      left_join(ov, by = c('Player' = 'cbs_name',
                           'MLB'    = 'mlb_team',
                           'Pos'    = 'cbs_pos'))
    overridden <- keyed %>% filter(!is.na(playerid)) %>% select(-note)
    df         <- keyed %>% filter(is.na(playerid)) %>% select(-playerid, -note)

    # Pick up birth_year (and any other master-only columns) for
    # overridden rows. Requires Component 4 backfill to populate
    # master rows for synthetic sa_-prefixed playerids; otherwise
    # birth_year is NA, same as today's behavior for unmatched rows.
    if (nrow(overridden) > 0) {
      mById <- select(master, playerid, birth_year)
      overridden <- left_join(overridden, mById, by = 'playerid')
    }
  } else {
    overridden <- tibble()
  }

  # 1. Existing master joins (unchanged).
  m2 <- select(master,-Pos,-Player) %>% dplyr::rename(Player=cbs_name)
  gfull  <- inner_join(df, m2, by=c('Player','MLB'), relationship="many-to-many")
  dfleft <- anti_join (df, m2, by=c('Player','MLB'))
  m2     <- anti_join (m2, df, by=c('Player','MLB'))
  gname  <- left_join (dfleft, m2, by=c('Player'), relationship="many-to-many")
  gname  <- select(gname, -MLB.x) %>% dplyr::rename(MLB=MLB.y)

  # 2. Reattach overridden rows. bind_rows aligns by column name and
  #    fills missing columns with NA, which is fine because overridden
  #    rows carry the same df_cols + playerid + birth_year shape.
  final <- bind_rows(overridden, gfull, gname)
  final
}

loadPlayerOverrides <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    fn <- "../data/playerOverrides.csv"
    if (!file.exists(fn)) { cached <<- tibble(); return(cached) }
    cached <<- read.csv(fn, stringsAsFactors=FALSE)
    cached
  }
})
```

Key invariants the new shape preserves:

- Every row in the input `df` appears exactly once in the output
  (overridden rows are removed from `df` before the master joins).
- Column set of the output is unchanged: `Pos`, `MLB`, `playerid`,
  `birth_year` come from either the override file (carries
  `playerid`; the rest are filled in from master via the
  backfill in Component 4) or the master join.
- Order is not preserved (already not preserved today — `rbind(gfull,
  gname)`).

Edge cases:

- **Override file missing**: `loadPlayerOverrides()` returns an empty
  tibble; the function behaves identically to today.
- **Override file present but `df` lacks `Pos`**: skip overrides
  entirely. `Pos` is always set by `read.cbs()` before `addPlayerid()`
  is called, so this is a defensive guard for hand-built callers.
- **Override matches but FG has no row for that playerid**: row stays
  in the CBS-derived table but drops out of `AllH`/`AllP` at the
  `inner_join(., hitters/pitchers, by='playerid')` step. This is the
  correct outcome for Jared Jones 1B — he has no projection, so he
  shouldn't show up in League Eval rosters anywhere.
- **Two CBS rows with same override key**: both get the same playerid
  (rare in practice — would mean the same player listed twice in one
  file by CBS).

### Component 4: `mymaster.csv` backfill

For each synthetic `sa_`-prefixed playerid introduced by overrides,
add a corresponding row to `mymaster.csv`:

```csv
"NNNN","sa_jaredjones_1b","Jared Jones",2003,"Jared Jones","1B","PIT"
```

(`birth_year` may be approximate; use the player's best-known DOB or
leave at a placeholder year if unknown.)

Why this matters: a handful of downstream code paths re-join CBS-side
tables back to `master` on `playerid` (e.g., `predictHolds` in
`daflFunctions.r:841`, age lookups in `read.fg`). Without the master
row, those joins NA-out for synthetic IDs. With it, they behave the
same as for any other player without a projection.

## Data flow

```
AllHitters.csv ──┐
AllPitchers*.csv ├─→ read.cbs() ──→ addPlayerid()
poselig.csv ─────┘                       │
                                         ├─→ playerOverrides.csv (priority)
                                         └─→ mymaster.csv (fallback)
                                                  │
                                                  ▼
                                          inSeasonPulse.r builds
                                          AllH = Allhitters ⋈ hitters
                                          AllP = Allpitchers ⋈ pitchers
                                                  │
                                                  ▼
                                          LeagueEval buildRoster()
```

Before: 1B Jared Jones CBS row gets playerid `27863`. Survives downstream
joins via many-to-many fan-outs against the SP's projection rows,
appearing on Liquor Crickets' roster.

After: 1B Jared Jones CBS row gets playerid `sa_jaredjones_1b`. The
`inner_join(Allhitters, hitters, by='playerid')` drops him because
`hitters` has no `sa_jaredjones_1b`. He never reaches `AllH` and is
not displayed.

## Error handling

- Missing `data/playerOverrides.csv` → empty tibble, log nothing,
  behave as today. (No `stop()`. Callers should be able to run
  inSeasonPulse without the override file existing.)
- Malformed override row (missing one of the 5 columns) → fail loudly
  at load time with `stop("playerOverrides.csv missing column: ...")`.
  These files are hand-edited; a typo should surface immediately, not
  silently corrupt joins.
- Override matches a row whose `playerid` collides with a real FG ID
  → not validated automatically. The diagnostic in Component 2 will
  catch the inverse case (`master_multi_row`) on the next run, which
  is when the user would add an override.

## Testing

Manual verification — code only paths exist in R/Shiny; there is no
existing test harness. Verification steps:

1. **Diagnostic run**: `Rscript code/findAmbiguousPlayers.r`. Inspect
   `data/ambiguousPlayers.csv` — confirm Jared Jones / PIT appears as
   `cbs_multi_pos`. Confirm no surprise rows (the file should list a
   small handful, not hundreds).
2. **Override applied**: edit `data/playerOverrides.csv`, add the
   Jared Jones 1B row, re-run `Rscript code/inSeasonPulse.r` (or click
   "Refresh Data" in LeagueEval). Confirm in R:
   - `Allhitters %>% filter(Player == 'Jared Jones')` → exactly one row
     with `playerid == 'sa_jaredjones_1b'`, Team `Hogan's Heroes`.
   - `AllH %>% filter(Player == 'Jared Jones')` → 0 rows.
   - `AllP %>% filter(Player == 'Jared Jones')` → exactly one row,
     Team `Liquor Crickets`, `playerid == '27863'`.
3. **LeagueEval UI smoke test**: open the app, Trade Eval tab, select
   Liquor Crickets. Jared Jones appears exactly once (as the SP).
   Select Hogan's Heroes. The 1B does not appear (correct — no
   projection).
4. **Regression check**: pick three teams with no known collisions
   (e.g., Crickets, top of standings). Confirm their roster row
   counts and pDFL totals are unchanged from a pre-change run. A
   quick way: dump `buildRoster(team) %>% select(Player, pDFL)` to
   CSV before and after; `diff` should be empty.
5. **Sanity check on other apps**: launch `draftTool` and
   `ProtectionTrades`. Confirm they still start cleanly and produce
   sensible tables — they share `daflFunctions.r` and inherit the
   override behavior transparently.

## Files changed

- `data/playerOverrides.csv` (new)
- `code/findAmbiguousPlayers.r` (new)
- `code/daflFunctions.r` (modify `addPlayerid()`, add
  `loadPlayerOverrides()`; optionally extract `parseCbsPlayer()`
  helper to share with the diagnostic)
- `mymaster.csv` (append one row per synthetic playerid introduced)
- `data/ambiguousPlayers.csv` (new, output of diagnostic — gitignored
  or committed as a snapshot, user preference)
