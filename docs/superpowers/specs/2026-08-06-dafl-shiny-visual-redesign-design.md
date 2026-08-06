# DAFL Shiny Apps — Visual Redesign

**Date:** 2026-08-06
**Status:** Approved

## Goal

Give the five DAFL Shiny apps a polished, professional look with baseball
character. League Eval is used daily and gets verified first; the other four
inherit the same theme.

## Design direction

**Ballpark editorial (light).** Warm off-white paper, deep navy, infield-clay
accent. A slab serif for headings and a clean sans for body and numbers. Reads
like a well-designed baseball annual — professional, with personality in the
details rather than in motifs.

Personality is deliberately restrained: a serif wordmark, a stitched-seam
accent line, scorecard-style section rules, clay for money and links. Nothing
that competes with the numbers for attention.

## Scope

Chrome and table styling only. No `server.R` logic changes, with one named
carve-out (see "Tier color carve-out" below). Layout structure, reactive logic,
and data pipelines are untouched.

Out of scope: semantic table styling (in-cell bars, tier badges, conditional
hot/cold coloring), League Eval layout restructuring (stat-card headers,
redesigned Player Snapshot). These were considered and deferred.

## Current state

- All five apps use stock `bs_theme(bootswatch = "flatly")`
  (`LeagueEval/ui.R:15`, `draftTool/ui.R:9`, `ProtectionTrades/ui.R:8`,
  `Guardians/ui.R:9`, `LiveDraftTool/ui.R:8`).
- No `.css` files and no `www/` directories exist anywhere in the repo.
- The only custom CSS is 10 lines inline in `LeagueEval/ui.R:104-113`, styling
  the player-name link and its popup menu.
- League Eval renders roughly 20 DT tables, all with near-default options.

## Environment facts (verified)

| Package | Version | Consequence |
|---|---|---|
| bslib | 0.11.0 | `bs_add_rules()` and `font_google(local = TRUE)` available; Bootstrap 5 default |
| shiny | 1.13.0 | — |
| DT | 0.34.0 | Ships **DataTables 1.13.6** → legacy `.dataTables_wrapper` / `.dataTables_filter` / `.dataTables_paginate` class names, **not** the DataTables 2.x `dt-container` / `dt-search` names |
| sass | 0.4.10 | SCSS compilation available |

DT tags numeric columns with class `dt-right` automatically, which lets CSS
apply tabular figures to every numeric column across all tables without
touching any `renderDataTable` call.

## Architecture

```
code/daflTheme.R              # dafl_theme() → bs_theme object
                              # dafl_brand(subtitle) → navbar title HTML
code/dafl.scss                # all styling: chrome + DT tables
LeagueEval/global.R           # source("../code/daflTheme.R")   ← new, 1 line
draftTool/global.R            # (same, 1 line each)
ProtectionTrades/global.R
Guardians/global.R
LiveDraftTool/global.R
```

Each `ui.R` changes two lines:

- `theme = bs_theme(bootswatch = "flatly")` → `theme = dafl_theme()`
- the title string → `dafl_brand(<subtitle>)`

`dafl_brand()` renders the shared `D A F L` wordmark plus a per-app subtitle.
Existing titles map as follows; version suffixes are dropped from the visible
subtitle and retained nowhere:

| App | Current title | `dafl_brand()` subtitle |
|---|---|---|
| LeagueEval | `DAFL Evaluator, v3.0` | `Evaluator` |
| draftTool | `Live Auction Tool, v0.5` | `Live Auction` |
| LiveDraftTool | `DAFL Live Draft Tool v 2.0` | `Live Draft` |
| ProtectionTrades | `Offseason Trade Evaluator, v2.0` | `Offseason Trades` |
| Guardians | `Cleveland Guardians Tracker` | `Guardians Tracker` |

`dafl_theme()` composes `bs_theme()` with the Bootstrap variable overrides and
appends the compiled SCSS via `bs_add_rules()`. Everything ships inside the
theme object; no `www/` directory and no `includeCSS()` calls.

### Why `global.R` rather than sourcing inside `ui.R`

`LeagueEval/server.R:5` runs `setwd("../code/")` and never restores the
working directory. Relative paths evaluated in `ui.R` are therefore not
reliable, and the ui/server source order should not be depended on.

Shiny always sources `global.R` first, with the working directory set to the
app directory. `daflTheme.R` reads `dafl.scss` into a string at load time and
stores the compiled theme, so nothing touches the filesystem after `global.R`
has run — the working directory can change freely afterward.

### Season badge

`dafl_brand()` renders a season pill. The season value is read from `cyear` in
`code/daflFunctions.r` with a single-line regex at load time, falling back to
the calendar year if the file or the assignment cannot be found. This keeps the
badge correct in the Oct–Dec window when `cyear` has already rolled forward.

`daflTheme.R` must not depend on `cyear` being in the global environment —
`global.R` runs before `server.R` has sourced anything.

## Design tokens

Defined as CSS custom properties on `:root` and mirrored into the relevant
Bootstrap Sass variables.

| Token | Value | Used for |
|---|---|---|
| `--dafl-paper` | `#FBF8F3` | page background |
| `--dafl-card` | `#FFFFFF` | tables, sidebar panels |
| `--dafl-ink` | `#1B2A41` | navbar, headings, body text |
| `--dafl-muted` | `#6B7789` | secondary text, table meta |
| `--dafl-clay` | `#C8663A` | links, active tab, focus rings, money |
| `--dafl-rule` | `#E4DDD2` | hairlines, borders |
| `--dafl-good` | `#1E7A4B` | hot / positive |
| `--dafl-bad` | `#B3402F` | cold / negative |
| `--dafl-hover` | `#F3EDE4` | table row hover |

### Typography

Loaded with `font_google(local = TRUE)` — fetched and cached on first run, then
served locally and available offline.

- **Zilla Slab** 600 — wordmark, `h1`–`h4`, table headers.
- **Inter** 400/500 — all body and UI text.
- Numbers use Inter with `font-variant-numeric: tabular-nums`. No separate
  numeric typeface.

Every font stack ends in a system fallback
(`-apple-system, BlinkMacSystemFont, "Segoe UI", Georgia, serif|sans-serif`) so
the apps render correctly if the font fetch fails or the machine is offline
before the cache is warm.

## Chrome

- **Navbar** — navy `--dafl-ink`, 52px tall. Brand renders `D A F L` in Zilla
  Slab with wide letter-spacing, the app subtitle in lighter Inter beside it,
  and a season pill (`⚾ 2026`) right-aligned.
- **Stitch line** — a 3px `repeating-linear-gradient` in clay directly under
  the navbar, evoking baseball seam stitching. The primary flavor element.
- **Nav items** — active item takes a 2px clay underline rather than a filled
  background. `navbarMenu` dropdowns (Players, Free Agents, Analysis, Signals)
  get the paper background and a soft shadow.
- **`tabsetPanel` tabs** — underline style; uppercase 11px labels with `.06em`
  tracking; clay when active.
- **`sidebarPanel`** — white card, 1px `--dafl-rule` border, 8px radius,
  `0 1px 3px rgba(27,42,65,.06)` shadow. Replaces the current flat gray slab.
- **Buttons** — flat, 6px radius, no gradients. `btn-primary` navy;
  `btn-success` (the Research tab's *Get Latest* button) in `--dafl-good`;
  `btn-default` (the Settings button) as an outlined ghost button.
- **Headings** — `h2`/`h3`/`h4` in Zilla Slab with a short clay rule beneath,
  so "Targeted Players", "Who Could Be Dumping", "Roster Resource" read as
  section markers instead of loose bold text.
- **Player links and popup menu** — the inline `<style>` block at
  `LeagueEval/ui.R:104-113` is deleted; `.dafl-player`, `.dafl-menu`,
  `.dafl-menu-title`, and `.dafl-menu-item` are rewritten in `dafl.scss`
  against the new palette. The JavaScript in `ui.R:25-103` is unchanged.

## Tables

All rules target the DataTables 1.13.6 class names. Applies to every DT table
in every app with no per-table changes.

- **Sticky headers** — `position: sticky` on `thead th`, keeping column names
  visible while scrolling the long tables (Standings, Free Agents by Position,
  Injured, Prospects).
- **Header row** — navy ink, uppercase, 11px, `.06em` tracking, 2px clay
  bottom border.
- **Rows** — padding tightened from DT's `8px` default to `5px 10px`; 13.5px
  text; hairline `--dafl-rule` separators instead of zebra striping. Yields
  roughly three additional visible rows per screen.
- **Numerics** — `td.dt-right { font-variant-numeric: tabular-nums; }` aligns
  digits across all tables, using DT's automatic numeric-column class.
- **Hover and selection** — `--dafl-hover` on row hover; clay-tinted selected
  row (used by the Player Snapshot search table, which has
  `selection = 'single'`).
- **Sort indicators** — restyled carets; the active sort column header takes a
  faint clay wash so the current sort is visible at a glance.
- **Controls** — the search box, page-length dropdown, and `filter = 'top'`
  column filter inputs are restyled as consistent form controls with clay focus
  rings. Pagination becomes pill buttons with a clay active state.
- **Summary tables** — tables rendered with `dom = 't'` (Trade Summary,
  Team Category Detail, Statistical Surplus) get a tighter borderless
  "summary card" treatment: no wrapper chrome, lighter separators.

### Tier color carve-out

Five lines in `LeagueEval/server.R` hardcode Bootstrap-3 pastels for tier
backgrounds:

- `server.R:613-618` — `formatStyle('Tier', ...)` with
  `styleEqual(c('High','Medium','Low'), c('#d4edda','#fff3cd','#f8d7da'))`
- `server.R:818-820` — `formatStyle` on the `High` / `Medium` / `Low` columns
  with the same three hexes

`formatStyle` emits **inline** styles, which CSS cannot override. Against the
warm paper background these read as cold and off-system.

**Decision:** swap the three hex values for paper-harmonized equivalents.

| Tier | Old | New |
|---|---|---|
| High | `#d4edda` | `#E2EFE4` |
| Medium | `#fff3cd` | `#FAF0D7` |
| Low | `#f8d7da` | `#F7DFD9` |

This is the only `server.R` edit in scope. It changes three literal values in
five lines and touches no logic.

## Rollout

1. Build `code/daflTheme.R` and `code/dafl.scss`.
2. Wire up **LeagueEval** only. Verify each tab area against the current look:
   Standings, By Team, Player Snapshot, Player Trends, My Targets, Free Agents
   (By Position / Reliever Detail / Streamers / Prospects / Injured), Analysis
   (Category Status / Surplus / Trade Eval), Signals (Dumpers / Desperate /
   LC Trends / Research).
3. Apply the tier-color carve-out.
4. Roll out to draftTool, ProtectionTrades, Guardians, LiveDraftTool — one
   `global.R` plus two `ui.R` lines each. Check each for layout regressions;
   their UIs are simpler and should inherit cleanly.

## Verification

- Each app launches via `shiny::runApp()` without error.
- League Eval: every tab renders, tables sort and filter, the player-name popup
  menu opens and its three links work, the target star toggles.
- Fonts resolve; confirm the fallback path by testing with the font cache
  cleared and no network.
- Sticky headers hold on a long table; numeric columns align.

## Rollback

Revert `theme = dafl_theme()` to `bs_theme(bootswatch = "flatly")` and restore
the title string in each `ui.R`. The new files are additive and can be left in
place. The tier hex values revert independently.

## Risks

- **Font fetch on first run** requires network access. Mitigated by the system
  fallback stack; the apps degrade in appearance but never fail.
- **Sticky headers** can interact badly with DT's `scrollX`/`scrollY` options
  if any table uses them. None currently do; verify during rollout.
- **`autoWidth` tables** (Injured, Streamers, My Targets set
  `autoWidth = FALSE`) compute column widths from rendered text. Changing the
  font and padding shifts those widths; check these three tables specifically.
