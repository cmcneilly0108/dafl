# DAFL Shiny Visual Redesign Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the stock `bootswatch = "flatly"` look in all five DAFL Shiny apps with a shared "ballpark editorial" theme — warm paper, deep navy, infield clay — covering navbar chrome and DT table styling.

**Architecture:** One SCSS file and one R helper live in `code/`. Each app gains a one-line `global.R` that sources the helper (guaranteed to run in the app directory before `server.R` changes the working directory), and its `ui.R` swaps two arguments. All styling ships inside the `bs_theme()` object via `bs_add_rules()` — no `www/` directory, no `includeCSS()`.

**Tech Stack:** R 4.5, shiny 1.13.0, bslib 0.11.0, sass 0.4.10, DT 0.34.0 (bundles DataTables 1.13.6).

**Spec:** `docs/superpowers/specs/2026-08-06-dafl-shiny-visual-redesign-design.md`

## Global Constraints

- **No `server.R` logic changes.** The only permitted `server.R` edit is Task 5 (three hex literals in `LeagueEval/server.R`).
- **Each `ui.R` changes exactly two arguments:** `theme = bs_theme(bootswatch = "flatly")` → `theme = dafl_theme()`, and the title string → `title = dafl_brand(<subtitle>)` plus a new `windowTitle =`. No other `ui.R` edits except Task 4's deletion of the inline `<style>` block in `LeagueEval/ui.R`.
- **DataTables class names are the 1.13.6 legacy set:** `.dataTables_wrapper`, `.dataTables_filter`, `.dataTables_length`, `.dataTables_info`, `.dataTables_paginate`. Do **not** use the DataTables 2.x names (`dt-container`, `dt-search`, `dt-paging`).
- **Every font stack ends in a system fallback** so the apps render if the Google Fonts fetch fails.
- **Exact token values** (used verbatim throughout):

| Token | Value |
|---|---|
| `--dafl-paper` | `#FBF8F3` |
| `--dafl-card` | `#FFFFFF` |
| `--dafl-ink` | `#1B2A41` |
| `--dafl-muted` | `#6B7789` |
| `--dafl-clay` | `#C8663A` |
| `--dafl-rule` | `#E4DDD2` |
| `--dafl-good` | `#1E7A4B` |
| `--dafl-bad` | `#B3402F` |
| `--dafl-hover` | `#F3EDE4` |
| `--dafl-sel` | `#FBEDE4` |

- **Verification is visual.** This repo has no test framework and the change is cosmetic. Each task ends with a launch-and-look step plus, where possible, a scripted smoke check. Screenshots are not required; the reviewer looks at the running app.
- **Launch command** (used in every verification step):
  ```bash
  cd /Users/cmcneilly/Dropbox/Personal/DAFL && \
    Rscript -e 'shiny::runApp("LeagueEval", port = 3838, launch.browser = FALSE)'
  ```
  Then open `http://127.0.0.1:3838`. `Ctrl-C` to stop.

## Deviations from the spec

Two, both flagged for the reviewer:

1. **Season pill moves from the navbar's right edge to the left, inside the brand.** `LeagueEval/ui.R:19`, `Guardians/ui.R:12`, and `LiveDraftTool/ui.R:24` each absolutely-position a Settings button at `right:15px; top:8px`. A right-aligned season pill would collide with it. Placing the pill immediately after the subtitle avoids the collision and needs no extra `ui.R` edits.
2. **Navbar height is 46px, not the 52px in the spec.** Those same overlay buttons are hard-coded at `top:8px` in inline styles that CSS cannot override. A 46px navbar vertically centres a 30px `btn-sm` at exactly `top:8px`, so the existing inline styles stay correct and no third `ui.R` edit is needed.

## File Structure

| File | Responsibility |
|---|---|
| `code/dafl.scss` (create) | All styling. Four sections: tokens/base, chrome, tables, player-menu. |
| `code/daflTheme.R` (create) | `dafl_theme()` builds the `bs_theme` object; `dafl_brand()` builds the navbar title HTML; `dafl_season()` reads `cyear`. Reads `dafl.scss` into a string at load time. |
| `scripts/check-dafl-theme.R` (create) | Smoke check: compiles the theme and asserts the CSS contains expected selectors. Runnable without launching a Shiny app. |
| `LeagueEval/global.R` (create) | One line: `source("../code/daflTheme.R")`. |
| `draftTool/global.R`, `ProtectionTrades/global.R`, `Guardians/global.R`, `LiveDraftTool/global.R` (create) | Same one line. |
| `LeagueEval/ui.R` (modify) | Two-argument swap (Task 1); delete inline `<style>` block at lines 104-113 (Task 4). |
| The other four `ui.R` files (modify) | Two-argument swap only (Task 6). |
| `LeagueEval/server.R` (modify) | Three hex literals at lines 613-618 and 818-820 (Task 5). |

`code/dafl.scss` is one file rather than four partials because `bs_add_rules()` takes a single Sass string and the total is ~250 lines — small enough to hold in context, and splitting would add a concatenation step for no benefit.

---

### Task 1: Theme scaffolding, wired into League Eval

Builds `dafl.scss` with tokens and base typography only, `daflTheme.R`, the smoke-check script, `LeagueEval/global.R`, and the `ui.R` swap. At the end of this task the app runs on the new theme with paper background and the new fonts, but no chrome or table styling yet.

**Files:**
- Create: `code/dafl.scss`
- Create: `code/daflTheme.R`
- Create: `scripts/check-dafl-theme.R`
- Create: `LeagueEval/global.R`
- Modify: `LeagueEval/ui.R:15-16`

**Interfaces:**
- Consumes: nothing.
- Produces:
  - `dafl_theme()` → a `bslib::bs_theme` object. No arguments.
  - `dafl_brand(subtitle)` → a `shiny::tagList`. `subtitle` is a length-1 character, e.g. `"Evaluator"`.
  - `dafl_season()` → a length-1 character, e.g. `"2026"`.
  - `DAFL_SCSS` → a length-1 character holding the contents of `dafl.scss`. Tasks 2, 3, and 4 append to `dafl.scss`; they do not touch `daflTheme.R`.

- [ ] **Step 1: Create `code/dafl.scss` with tokens and base typography**

```scss
/* ============================================================
   DAFL shared theme — "ballpark editorial"
   Loaded by code/daflTheme.R via bslib::bs_add_rules().
   Section 1: tokens + base typography
   ============================================================ */

:root {
  --dafl-paper: #FBF8F3;
  --dafl-card:  #FFFFFF;
  --dafl-ink:   #1B2A41;
  --dafl-muted: #6B7789;
  --dafl-clay:  #C8663A;
  --dafl-rule:  #E4DDD2;
  --dafl-good:  #1E7A4B;
  --dafl-bad:   #B3402F;
  --dafl-hover: #F3EDE4;
  --dafl-sel:   #FBEDE4;

  --dafl-serif: "Zilla Slab", Georgia, "Times New Roman", serif;
  --dafl-sans:  "Inter", -apple-system, BlinkMacSystemFont, "Segoe UI", Helvetica, Arial, sans-serif;
}

body {
  background-color: var(--dafl-paper);
  color: var(--dafl-ink);
  font-family: var(--dafl-sans);
  font-size: 14px;
  -webkit-font-smoothing: antialiased;
}

.container-fluid > .row,
.tab-content { padding-top: 4px; }

a { color: var(--dafl-clay); text-decoration: none; }
a:hover { color: #A9502C; text-decoration: underline; }
```

- [ ] **Step 2: Create `code/daflTheme.R`**

`DAFL_SCSS` is read once at source time so nothing touches the filesystem after `global.R` returns — `LeagueEval/server.R:5` changes the working directory and never restores it. `dafl_season()` reads `cyear` by regex rather than from the global environment, because `global.R` runs before `server.R` has sourced `daflFunctions.r`.

```r
# code/daflTheme.R — shared visual theme for the DAFL Shiny apps.
#
# Sourced from each app's global.R, which Shiny always runs first with the
# working directory set to the app directory. Everything the theme needs is
# read into memory here, so later setwd() calls in server.R cannot break it.

library(bslib)

# --- locate files relative to whichever directory we were sourced from -------
.dafl_find <- function(name) {
  for (p in c(file.path("../code", name), file.path("code", name), name)) {
    if (file.exists(p)) return(normalizePath(p))
  }
  stop("daflTheme.R: cannot locate ", name, " from ", getwd())
}

DAFL_SCSS <- paste(readLines(.dafl_find("dafl.scss"), warn = FALSE), collapse = "\n")

# --- season for the navbar badge --------------------------------------------
# Read cyear straight out of daflFunctions.r; fall back to the calendar year.
dafl_season <- function() {
  out <- tryCatch({
    src <- readLines(.dafl_find("daflFunctions.r"), warn = FALSE)
    hit <- grep('^\\s*cyear\\s*(<-|=)\\s*["\']([0-9]{4})["\']', src, value = TRUE)[1]
    if (is.na(hit)) NA_character_ else sub('.*["\']([0-9]{4})["\'].*', "\\1", hit)
  }, error = function(e) NA_character_)
  if (is.na(out)) format(Sys.Date(), "%Y") else out
}

# --- navbar brand ------------------------------------------------------------
# Rendered on the left so it cannot collide with the absolutely-positioned
# Settings buttons that sit at right:15px in several apps.
dafl_brand <- function(subtitle) {
  shiny::tagList(
    shiny::tags$span(class = "dafl-brand",
      shiny::tags$span(class = "dafl-brand__mark", "DAFL"),
      shiny::tags$span(class = "dafl-brand__sub", subtitle),
      shiny::tags$span(class = "dafl-brand__season", paste0("⚾ ", dafl_season()))
    )
  )
}

# --- theme -------------------------------------------------------------------
dafl_theme <- function() {
  bs_add_rules(
    bs_theme(
      version   = 5,
      bg        = "#FBF8F3",
      fg        = "#1B2A41",
      primary   = "#1B2A41",
      secondary = "#6B7789",
      success   = "#1E7A4B",
      danger    = "#B3402F",
      base_font    = font_google("Inter", wght = c(400, 500, 600), local = TRUE),
      heading_font = font_google("Zilla Slab", wght = c(400, 600), local = TRUE),
      "link-color"    = "#C8663A",
      "border-color"  = "#E4DDD2",
      "border-radius" = "6px"
    ),
    DAFL_SCSS
  )
}
```

- [ ] **Step 3: Create `scripts/check-dafl-theme.R`**

This is the automated part of verification: it proves the SCSS compiles and that the selectors the later tasks depend on actually made it into the output. Tasks 2-4 extend the `expect` vector.

Must be run from the repo root — it takes no steps to locate itself.

```r
#!/usr/bin/env Rscript
# Smoke check for the DAFL shared theme.
# Run from the repo root:  Rscript scripts/check-dafl-theme.R
# Compiles the theme and asserts required selectors reached the output CSS.

source("code/daflTheme.R")

theme <- dafl_theme()

# bs_theme_dependencies() compiles the Sass; collect every stylesheet it emits.
deps <- bslib::bs_theme_dependencies(theme)
css  <- paste(unlist(lapply(deps, function(d) {
  if (is.null(d$stylesheet)) return(NULL)
  paths <- file.path(d$src$file, d$stylesheet)
  paths <- paths[file.exists(paths)]
  unlist(lapply(paths, readLines, warn = FALSE))
})), collapse = "\n")

if (!nzchar(css)) stop("check-dafl-theme: theme compiled to zero bytes of CSS")

expect <- c("--dafl-clay", "--dafl-paper", "--dafl-ink")

missing <- expect[!vapply(expect, grepl, logical(1), x = css, fixed = TRUE)]
if (length(missing)) {
  stop("check-dafl-theme: missing from compiled CSS: ", paste(missing, collapse = ", "))
}

cat("check-dafl-theme: OK -", nchar(css), "bytes of CSS,",
    length(expect), "selectors present\n")
cat("season badge resolves to:", dafl_season(), "\n")
```

- [ ] **Step 4: Run the smoke check — expect it to fail on a missing file**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL && Rscript scripts/check-dafl-theme.R
```

Expected on first run: the fonts download (takes 5-20 seconds, needs network). If it errors with `cannot locate dafl.scss`, the `.dafl_find` candidates are wrong — fix before continuing. Expected final output: `check-dafl-theme: OK - <N> bytes of CSS, 3 selectors present` and `season badge resolves to: 2026`.

If it prints a season other than `2026`, the regex in `dafl_season()` does not match `code/daflFunctions.r:14`. Read that line and fix the pattern.

- [ ] **Step 5: Create `LeagueEval/global.R`**

```r
# Loads the shared DAFL visual theme. Shiny sources global.R first, with the
# working directory set to this app directory — before server.R calls setwd().
source("../code/daflTheme.R")
```

- [ ] **Step 6: Swap the two arguments in `LeagueEval/ui.R`**

Replace lines 15-16, which currently read:

```r
    theme = bs_theme(bootswatch = "flatly"),
    "DAFL Evaluator, v3.0",
```

with:

```r
    theme = dafl_theme(),
    title = dafl_brand("Evaluator"),
    windowTitle = "DAFL Evaluator",
```

`windowTitle` is required: `navbarPage()` derives the browser tab title from `title` only when `title` is a character string, and `dafl_brand()` returns a tag.

Leave the commented-out `theme =` line at 12 and the bootswatch list at 13-14 alone.

- [ ] **Step 7: Launch and verify**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL && \
  Rscript -e 'shiny::runApp("LeagueEval", port = 3838, launch.browser = FALSE)'
```

Open `http://127.0.0.1:3838`. Confirm:
- No error in the console.
- Page background is warm off-white (`#FBF8F3`), not white or gray.
- Body text is Inter; headings ("Targeted Players" etc.) are Zilla Slab.
- Browser tab reads "DAFL Evaluator".
- Navbar shows `DAFL Evaluator ⚾ 2026` on the left (unstyled at this point — plain text is expected).
- Every tab still loads and its tables still render.

- [ ] **Step 8: Commit**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL
git add code/dafl.scss code/daflTheme.R scripts/check-dafl-theme.R LeagueEval/global.R LeagueEval/ui.R
git commit -m "feat(theme): shared DAFL theme scaffolding, wired into LeagueEval"
```

---

### Task 2: Chrome styling

**Files:**
- Modify: `code/dafl.scss` (append section 2)
- Modify: `scripts/check-dafl-theme.R` (extend the `expect` vector)

**Interfaces:**
- Consumes: the CSS custom properties and `--dafl-serif` / `--dafl-sans` from Task 1; the `.dafl-brand`, `.dafl-brand__mark`, `.dafl-brand__sub`, `.dafl-brand__season` class names emitted by `dafl_brand()`.
- Produces: no new R interfaces.

- [ ] **Step 1: Append the chrome section to `code/dafl.scss`**

Rules are written against both the Bootstrap 3 (`.navbar-nav > li > a`, `.nav-tabs > li.active > a`) and Bootstrap 5 (`.nav-link`, `.nav-link.active`) class sets, because shiny's `navbarPage()` markup varies with the theme version.

```scss
/* ============================================================
   Section 2: chrome — navbar, tabs, sidebar, controls, buttons
   ============================================================ */

/* --- navbar ---------------------------------------------------------------
   46px, not 52px: several apps absolutely-position a 30px btn-sm at top:8px
   in an inline style CSS cannot override. 46px centres it exactly.          */
.navbar {
  background-color: var(--dafl-ink) !important;
  background-image: none;
  border: 0;
  border-radius: 0;
  min-height: 46px;
  padding: 0 16px;
  margin-bottom: 0;
  position: relative;
  box-shadow: none;
}

/* seam stitching under the navbar — the one overt baseball motif */
.navbar::after {
  content: "";
  position: absolute;
  left: 0; right: 0; bottom: -3px;
  height: 3px;
  background: repeating-linear-gradient(
    115deg,
    var(--dafl-clay) 0 2px,
    transparent 2px 9px
  );
}

.navbar .navbar-brand { padding: 0 14px 0 0; height: auto; display: flex; align-items: center; }

.dafl-brand { display: flex; align-items: baseline; gap: 9px; line-height: 46px; }
.dafl-brand__mark {
  font-family: var(--dafl-serif);
  font-weight: 600;
  font-size: 19px;
  letter-spacing: .22em;
  text-transform: uppercase;
  color: #FFFFFF;
}
.dafl-brand__sub {
  font-family: var(--dafl-sans);
  font-size: 13px;
  font-weight: 400;
  letter-spacing: .02em;
  color: rgba(255, 255, 255, .62);
}
.dafl-brand__season {
  font-size: 10.5px;
  font-weight: 600;
  letter-spacing: .06em;
  color: #FFFFFF;
  background: rgba(200, 102, 58, .92);
  border-radius: 999px;
  padding: 2px 9px;
  position: relative;
  top: -1px;
}

.navbar .navbar-nav > li > a,
.navbar .nav-link {
  color: rgba(255, 255, 255, .72) !important;
  font-size: 13px;
  font-weight: 500;
  letter-spacing: .03em;
  padding: 13px 13px;
  border-bottom: 2px solid transparent;
  background: transparent !important;
}
.navbar .navbar-nav > li > a:hover,
.navbar .nav-link:hover { color: #FFFFFF !important; }
.navbar .navbar-nav > .active > a,
.navbar .nav-link.active,
.navbar .nav-item.show > .nav-link,
.navbar .navbar-nav > .open > a {
  color: #FFFFFF !important;
  border-bottom-color: var(--dafl-clay);
}

.navbar .dropdown-menu {
  background: var(--dafl-paper);
  border: 1px solid var(--dafl-rule);
  border-radius: 8px;
  box-shadow: 0 6px 18px rgba(27, 42, 65, .14);
  padding: 6px;
  margin-top: 2px;
}
.navbar .dropdown-menu > li > a,
.navbar .dropdown-item {
  color: var(--dafl-ink);
  border-radius: 5px;
  padding: 7px 12px;
  font-size: 13.5px;
}
.navbar .dropdown-menu > li > a:hover,
.navbar .dropdown-item:hover,
.navbar .dropdown-item.active {
  background: var(--dafl-hover);
  color: var(--dafl-ink);
}

/* Settings buttons overlaid on the navy navbar (#settingsBtn in LeagueEval and
   LiveDraftTool, #gSettingsBtn in Guardians) need light-on-dark treatment. */
#settingsBtn, #gSettingsBtn {
  background: rgba(255, 255, 255, .10) !important;
  border: 1px solid rgba(255, 255, 255, .28) !important;
  color: rgba(255, 255, 255, .90) !important;
  font-size: 12px;
  font-weight: 500;
  padding: 5px 12px;
}
#settingsBtn:hover, #gSettingsBtn:hover {
  background: rgba(255, 255, 255, .20) !important;
  border-color: rgba(255, 255, 255, .50) !important;
  color: #FFFFFF !important;
}

/* --- tabsetPanel tabs ----------------------------------------------------- */
.nav-tabs {
  border-bottom: 1px solid var(--dafl-rule);
  margin-bottom: 14px;
}
.nav-tabs > li > a,
.nav-tabs .nav-link {
  border: 0;
  border-bottom: 2px solid transparent;
  border-radius: 0;
  background: transparent;
  color: var(--dafl-muted);
  font-size: 11.5px;
  font-weight: 600;
  letter-spacing: .06em;
  text-transform: uppercase;
  padding: 9px 14px;
  margin-right: 2px;
}
.nav-tabs > li > a:hover,
.nav-tabs .nav-link:hover {
  background: transparent;
  border-color: transparent;
  border-bottom-color: var(--dafl-rule);
  color: var(--dafl-ink);
}
.nav-tabs > li.active > a,
.nav-tabs > li.active > a:hover,
.nav-tabs .nav-link.active {
  background: transparent !important;
  border: 0 !important;
  border-bottom: 2px solid var(--dafl-clay) !important;
  color: var(--dafl-clay) !important;
}

/* --- sidebarPanel --------------------------------------------------------- */
.well {
  background: var(--dafl-card);
  border: 1px solid var(--dafl-rule);
  border-radius: 8px;
  box-shadow: 0 1px 3px rgba(27, 42, 65, .06);
  padding: 14px;
}

/* --- form controls -------------------------------------------------------- */
.form-control,
.form-select,
.selectize-input,
textarea.form-control {
  border: 1px solid var(--dafl-rule);
  border-radius: 6px;
  background: var(--dafl-card);
  color: var(--dafl-ink);
  font-size: 13.5px;
  box-shadow: none;
}
.form-control:focus,
.form-select:focus,
.selectize-input.focus,
.selectize-input.input-active {
  border-color: var(--dafl-clay);
  box-shadow: 0 0 0 3px rgba(200, 102, 58, .15);
  outline: none;
}
.selectize-dropdown .active { background: var(--dafl-hover); color: var(--dafl-ink); }

/* Field labels read as small caps; checkbox and radio labels must not. */
.control-label,
.shiny-input-container > label {
  font-size: 11.5px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: .05em;
  color: var(--dafl-muted);
  margin-bottom: 5px;
}
.checkbox label,
.radio label,
.form-check-label,
.shiny-input-container .checkbox > label,
.shiny-input-container .radio > label {
  text-transform: none;
  letter-spacing: 0;
  font-size: 13.5px;
  font-weight: 400;
  color: var(--dafl-ink);
}

/* --- buttons -------------------------------------------------------------- */
.btn {
  border-radius: 6px;
  font-size: 13px;
  font-weight: 600;
  letter-spacing: .02em;
  padding: 7px 14px;
  border: 1px solid transparent;
  background-image: none;
  box-shadow: none;
}
.btn-primary { background: var(--dafl-ink); border-color: var(--dafl-ink); color: #FFFFFF; }
.btn-primary:hover, .btn-primary:focus { background: #243651; border-color: #243651; color: #FFFFFF; }
.btn-success { background: var(--dafl-good); border-color: var(--dafl-good); color: #FFFFFF; }
.btn-success:hover, .btn-success:focus { background: #186139; border-color: #186139; color: #FFFFFF; }
.btn-default, .btn-secondary {
  background: transparent;
  color: var(--dafl-ink);
  border-color: var(--dafl-rule);
}
.btn-default:hover, .btn-secondary:hover {
  background: var(--dafl-hover);
  border-color: var(--dafl-clay);
  color: var(--dafl-ink);
}

/* --- headings ------------------------------------------------------------- */
h1, h2, h3, h4, h5 {
  font-family: var(--dafl-serif);
  font-weight: 600;
  color: var(--dafl-ink);
  letter-spacing: -.005em;
}
h2 {
  font-size: 21px;
  margin: 18px 0 13px;
  padding-bottom: 7px;
  position: relative;
}
h2::after {
  content: "";
  position: absolute;
  left: 0; bottom: 0;
  width: 38px; height: 2px;
  background: var(--dafl-clay);
}
h3 { font-size: 17px; margin: 16px 0 10px; }
h4 { font-size: 15px; margin: 12px 0 8px; }
```

- [ ] **Step 2: Extend the `expect` vector in `scripts/check-dafl-theme.R`**

Replace the `expect <- c(...)` line with:

```r
expect <- c("--dafl-clay", "--dafl-paper", "--dafl-ink",
            ".dafl-brand__mark", ".dafl-brand__season",
            "#settingsBtn", ".nav-tabs")
```

- [ ] **Step 3: Run the smoke check**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL && Rscript scripts/check-dafl-theme.R
```

Expected: `check-dafl-theme: OK - <N> bytes of CSS, 7 selectors present`. A Sass compile error here means a typo in the appended block — the error message names the line.

- [ ] **Step 4: Launch and verify chrome**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL && \
  Rscript -e 'shiny::runApp("LeagueEval", port = 3838, launch.browser = FALSE)'
```

Confirm at `http://127.0.0.1:3838`:
- Navbar is deep navy with the `DAFL Evaluator ⚾ 2026` brand on the left.
- A thin clay stitched line runs under the navbar.
- The active top-level nav item has a clay underline; hovering others brightens them.
- The Settings button at top-right is a light outlined ghost button, legible on navy, and **vertically centred** — if it sits high or low, the 46px navbar height is wrong.
- Open the `Players`, `Free Agents`, `Analysis`, and `Signals` dropdowns: paper background, rounded, soft shadow, clay-free hover in `--dafl-hover`.
- On **By Team**: the sidebar is a white rounded card; the `Hitters`/`Pitchers` tabs are uppercase with a clay underline on the active one; `Select Team` label is small-caps muted.
- On **Free Agents → Streamers**: the `Free Agents Only` checkbox label is **normal case**, not uppercase.
- On **Signals → Research**: `Get Latest` is green, `Analyze Article` is navy.
- Headings like "Targeted Players" and "Who Could Be Dumping" have a short clay rule beneath.

- [ ] **Step 5: Commit**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL
git add code/dafl.scss scripts/check-dafl-theme.R
git commit -m "feat(theme): navbar, tabs, sidebar, controls and button styling"
```

---

### Task 3: DT table styling

**Files:**
- Modify: `code/dafl.scss` (append section 3)
- Modify: `scripts/check-dafl-theme.R` (extend the `expect` vector)

**Interfaces:**
- Consumes: tokens from Task 1.
- Produces: no new R interfaces.

Three implementation notes the engineer needs:

1. **No `overflow: hidden` on `table.dataTable`.** An ancestor with `overflow: hidden` silently disables `position: sticky` on the header. The table keeps its `border-radius`; cell borders simply do not clip at the corners, which is not visible in practice.
2. **Only the first `thead` row is sticky.** Tables built with `filter = 'top'` get a second `thead` row of filter inputs. Making both sticky stacks them on top of each other, so the selector is `thead tr:first-child th`.
3. **`border-collapse: separate` is required** for borders to render on sticky header cells.

- [ ] **Step 1: Append the tables section to `code/dafl.scss`**

```scss
/* ============================================================
   Section 3: DT tables (DataTables 1.13.6 — legacy class names)
   ============================================================ */

.dataTables_wrapper {
  font-size: 13.5px;
  color: var(--dafl-ink);
  margin-bottom: 22px;
}

table.dataTable {
  width: 100% !important;
  background: var(--dafl-card);
  border: 1px solid var(--dafl-rule);
  border-radius: 8px;
  /* No overflow:hidden here — it would disable sticky headers. */
  border-collapse: separate;
  border-spacing: 0;
  margin: 8px 0 !important;
}

/* --- header --------------------------------------------------------------- */
table.dataTable thead th {
  background: var(--dafl-card);
  font-family: var(--dafl-serif);
  font-size: 11px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: .06em;
  color: var(--dafl-ink);
  border-bottom: 2px solid var(--dafl-clay) !important;
  border-top: 0 !important;
  padding: 9px 10px;
  white-space: nowrap;
}
/* Sticky only on the label row; filter='top' adds a second row below it. */
table.dataTable thead tr:first-child th {
  position: sticky;
  top: 0;
  z-index: 2;
}

/* --- body ----------------------------------------------------------------- */
table.dataTable tbody tr { background: var(--dafl-card); }
table.dataTable tbody tr.odd,
table.dataTable.stripe tbody tr.odd,
table.dataTable.display tbody tr.odd { background: var(--dafl-card); }
table.dataTable tbody td {
  padding: 5px 10px;
  border-top: 1px solid var(--dafl-rule);
  vertical-align: middle;
}
table.dataTable tbody tr:hover,
table.dataTable.hover tbody tr:hover,
table.dataTable.display tbody tr:hover { background: var(--dafl-hover) !important; }

table.dataTable tbody tr.selected,
table.dataTable tbody tr.selected td {
  background: var(--dafl-sel) !important;
  color: var(--dafl-ink) !important;
  box-shadow: inset 3px 0 0 var(--dafl-clay);
}

/* Tabular figures on numeric columns. DT tags these `dt-right` automatically,
   so this reaches every numeric column in every table with no server changes. */
td.dt-right, th.dt-right,
table.dataTable td.dt-body-right {
  font-variant-numeric: tabular-nums;
  font-feature-settings: "tnum" 1;
}

/* --- sort indicators ------------------------------------------------------ */
table.dataTable thead th.sorting_asc,
table.dataTable thead th.sorting_desc { background: var(--dafl-hover); }
table.dataTable thead .sorting::before,
table.dataTable thead .sorting::after,
table.dataTable thead .sorting_asc::before,
table.dataTable thead .sorting_asc::after,
table.dataTable thead .sorting_desc::before,
table.dataTable thead .sorting_desc::after { opacity: .28; }
table.dataTable thead .sorting_asc::before,
table.dataTable thead .sorting_desc::after { opacity: 1; color: var(--dafl-clay); }

/* --- controls ------------------------------------------------------------- */
.dataTables_wrapper .dataTables_filter input,
.dataTables_wrapper .dataTables_length select {
  border: 1px solid var(--dafl-rule);
  border-radius: 6px;
  background: var(--dafl-card);
  color: var(--dafl-ink);
  padding: 4px 9px;
  font-size: 13px;
}
.dataTables_wrapper .dataTables_filter input:focus {
  outline: none;
  border-color: var(--dafl-clay);
  box-shadow: 0 0 0 3px rgba(200, 102, 58, .15);
}
.dataTables_wrapper .dataTables_filter label,
.dataTables_wrapper .dataTables_length label,
.dataTables_wrapper .dataTables_info {
  font-size: 11.5px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: .05em;
  color: var(--dafl-muted);
}

.dataTables_wrapper .dataTables_paginate .paginate_button {
  border: 0 !important;
  background: transparent !important;
  border-radius: 6px !important;
  padding: 4px 10px !important;
  margin: 0 1px !important;
  font-size: 12.5px;
  font-weight: 600;
  color: var(--dafl-ink) !important;
}
.dataTables_wrapper .dataTables_paginate .paginate_button:hover {
  background: var(--dafl-hover) !important;
  color: var(--dafl-ink) !important;
}
.dataTables_wrapper .dataTables_paginate .paginate_button.current,
.dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
  background: var(--dafl-clay) !important;
  color: #FFFFFF !important;
}
.dataTables_wrapper .dataTables_paginate .paginate_button.disabled,
.dataTables_wrapper .dataTables_paginate .paginate_button.disabled:hover {
  background: transparent !important;
  color: var(--dafl-rule) !important;
}

/* --- filter='top' column filters ------------------------------------------ */
table.dataTable thead tr:nth-child(2) td { padding: 4px 6px; border-bottom: 1px solid var(--dafl-rule); }
table.dataTable thead input[type="text"],
table.dataTable thead input[type="search"],
table.dataTable thead .form-control {
  font-size: 12px;
  border-radius: 5px;
  border: 1px solid var(--dafl-rule);
  padding: 3px 7px;
  width: 100%;
}
table.dataTable thead .selectize-input { font-size: 12px; padding: 3px 7px; min-height: 0; }

/* --- summary tables (dom = 't': no pagination block) -----------------------
   Trade Summary, Team Category Detail, Statistical Surplus. Uses :has(), so
   browsers without it simply keep the standard table look.                   */
.dataTables_wrapper:not(:has(.dataTables_paginate)) table.dataTable {
  border: 0;
  border-radius: 0;
}
.dataTables_wrapper:not(:has(.dataTables_paginate)) table.dataTable tbody td {
  border-top: 1px solid var(--dafl-rule);
  padding: 6px 10px;
}
```

- [ ] **Step 2: Extend the `expect` vector in `scripts/check-dafl-theme.R`**

```r
expect <- c("--dafl-clay", "--dafl-paper", "--dafl-ink",
            ".dafl-brand__mark", ".dafl-brand__season",
            "#settingsBtn", ".nav-tabs",
            ".dataTables_wrapper", ".dataTables_paginate", "tabular-nums")
```

- [ ] **Step 3: Run the smoke check**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL && Rscript scripts/check-dafl-theme.R
```

Expected: `check-dafl-theme: OK - <N> bytes of CSS, 10 selectors present`.

- [ ] **Step 4: Launch and verify tables**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL && \
  Rscript -e 'shiny::runApp("LeagueEval", port = 3838, launch.browser = FALSE)'
```

Confirm at `http://127.0.0.1:3838`:
- **Standings → Overall Standings**: uppercase slab header with a clay underline; rows are tighter than before; hovering a row tints it; scrolling the page keeps the header row visible (sticky).
- **Standings → Rest of Season Prediction**: `hDFL`/`piDFL`/`tDFL`/`zScore` digits line up vertically in their columns.
- Click a column header: the sorted column gets a faint tint and a clay arrow.
- Pagination at the bottom: pill buttons, current page filled clay.
- **Free Agents → By Position**: `filter='top'` inputs sit in a second header row, styled to match, **not** sticky and **not** overlapping the label row.
- **Free Agents → Injured** and **Streamers**, and **Players → My Targets**: these three set `autoWidth = FALSE`, so column widths are computed from rendered text and the font change shifts them. Check no column is clipped or absurdly wide.
- **Players → Player Snapshot**: click a row in the left table — it highlights in `--dafl-sel` with a clay left edge, and the right-hand snapshot still populates.
- **Analysis → Trade Eval** and **Analysis → Category Status**: the `dom = 't'` summary tables render borderless and tight.

- [ ] **Step 5: Commit**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL
git add code/dafl.scss scripts/check-dafl-theme.R
git commit -m "feat(theme): DT table styling — sticky headers, tabular figures, restyled controls"
```

---

### Task 4: Migrate the player link and popup menu out of `ui.R`

`LeagueEval/ui.R:104-113` holds a `tags$style(HTML(...))` block styling `.dafl-player` and `.dafl-menu` with the old palette. It moves into `dafl.scss` and is restyled. The JavaScript above it (`ui.R:25-103`) is **not** touched.

**Files:**
- Modify: `code/dafl.scss` (append section 4)
- Modify: `LeagueEval/ui.R:104-113` (delete the style block)
- Modify: `scripts/check-dafl-theme.R` (extend the `expect` vector)

**Interfaces:**
- Consumes: the class names emitted by the `daflPlayerMenu()` JavaScript at `LeagueEval/ui.R:51-102` — `.dafl-menu`, `.dafl-menu-title`, `.dafl-menu-item` — and by `savantAnchor()` in `code/daflFunctions.r` — `.dafl-player`.
- Produces: nothing.

- [ ] **Step 1: Append the player-menu section to `code/dafl.scss`**

```scss
/* ============================================================
   Section 4: player-name links and the click-through popup menu
   Class names come from daflPlayerMenu() in LeagueEval/ui.R and
   savantAnchor() in code/daflFunctions.r — do not rename.
   ============================================================ */

.dafl-player {
  color: inherit;
  cursor: pointer;
  text-decoration: underline;
  text-decoration-style: dotted;
  text-decoration-color: var(--dafl-rule);
  text-underline-offset: 2px;
}
.dafl-player:hover {
  color: var(--dafl-clay);
  text-decoration-color: var(--dafl-clay);
}

.dafl-menu {
  position: absolute;
  z-index: 3000;
  background: var(--dafl-card);
  border: 1px solid var(--dafl-rule);
  border-radius: 8px;
  box-shadow: 0 8px 22px rgba(27, 42, 65, .16);
  min-width: 176px;
  font-size: 13.5px;
  overflow: hidden;
}
.dafl-menu-title {
  padding: 7px 12px;
  font-family: var(--dafl-serif);
  font-weight: 600;
  font-size: 13px;
  background: var(--dafl-ink);
  color: #FFFFFF;
  white-space: nowrap;
}
.dafl-menu-item {
  display: block;
  padding: 8px 12px;
  color: var(--dafl-ink);
  text-decoration: none;
  cursor: pointer;
}
.dafl-menu-item:hover {
  background: var(--dafl-hover);
  color: var(--dafl-clay);
  text-decoration: none;
}
```

- [ ] **Step 2: Delete the inline style block from `LeagueEval/ui.R`**

Remove lines 104-113 in full — the `tags$style(HTML("...")),` call and everything between the quotes:

```r
      tags$style(HTML("
      .dafl-player { color:inherit; cursor:pointer; text-decoration:underline; text-decoration-style:dotted; }
      .dafl-player:hover { color:#2980b9; }
      .dafl-menu { position:absolute; z-index:3000; background:#fff; border:1px solid #bbb;
                   border-radius:6px; box-shadow:0 2px 10px rgba(0,0,0,0.2); min-width:160px;
                   font-size:14px; overflow:hidden; }
      .dafl-menu-title { padding:6px 12px; font-weight:bold; background:#2c3e50; color:#fff; white-space:nowrap; }
      .dafl-menu-item { display:block; padding:8px 12px; color:#2c3e50; text-decoration:none; cursor:pointer; }
      .dafl-menu-item:hover { background:#ecf0f1; }
      "))
```

The preceding `tags$script(HTML("..."))` block ends with `")),` at line 103. After deleting, `tags$head(` must contain only the script call, so line 103's trailing comma becomes a syntax error — change `")),` to `"))` before the closing `)` of `tags$head`.

- [ ] **Step 3: Extend the `expect` vector in `scripts/check-dafl-theme.R`**

```r
expect <- c("--dafl-clay", "--dafl-paper", "--dafl-ink",
            ".dafl-brand__mark", ".dafl-brand__season",
            "#settingsBtn", ".nav-tabs",
            ".dataTables_wrapper", ".dataTables_paginate", "tabular-nums",
            ".dafl-menu-item", ".dafl-player")
```

- [ ] **Step 4: Run the smoke check**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL && Rscript scripts/check-dafl-theme.R
```

Expected: `check-dafl-theme: OK - <N> bytes of CSS, 12 selectors present`.

- [ ] **Step 5: Verify `ui.R` still parses**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL && Rscript -e 'invisible(parse("LeagueEval/ui.R")); cat("ui.R parses OK\n")'
```

Expected: `ui.R parses OK`. A parse error means the comma fix in Step 2 was missed.

- [ ] **Step 6: Launch and verify the menu**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL && \
  Rscript -e 'shiny::runApp("LeagueEval", port = 3838, launch.browser = FALSE)'
```

On **By Team → Hitters**, confirm:
- Player names show a faint dotted underline and turn clay on hover.
- Clicking a name opens the popup: navy slab title bar, rounded, soft shadow, items highlight in `--dafl-hover` with clay text.
- All three items work — Baseball Savant and FanGraphs open in new tabs; **Player Snapshot** navigates to the Snapshot tab and the menu closes.

- [ ] **Step 7: Commit**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL
git add code/dafl.scss scripts/check-dafl-theme.R LeagueEval/ui.R
git commit -m "refactor(theme): move player-menu CSS from ui.R into dafl.scss"
```

---

### Task 5: Tier color carve-out

The only `server.R` edit in the plan. `formatStyle()` emits **inline** styles, which CSS cannot override, so the Bootstrap-3 pastels have to change at the source. Three literal values, five lines, no logic.

**Files:**
- Modify: `LeagueEval/server.R:613-618`
- Modify: `LeagueEval/server.R:818-820`

**Interfaces:** none.

| Tier | Old | New |
|---|---|---|
| High | `#d4edda` | `#E2EFE4` |
| Medium | `#fff3cd` | `#FAF0D7` |
| Low | `#f8d7da` | `#F7DFD9` |

- [ ] **Step 1: Update the `Tier` column styling at `server.R:613-618`**

Replace:

```r
      formatStyle('Tier',
                  valueColumns = 'TierBg',
                  backgroundColor = styleEqual(
                    c('High', 'Medium', 'Low'),
                    c('#d4edda', '#fff3cd', '#f8d7da')
                  ))
```

with:

```r
      formatStyle('Tier',
                  valueColumns = 'TierBg',
                  backgroundColor = styleEqual(
                    c('High', 'Medium', 'Low'),
                    c('#E2EFE4', '#FAF0D7', '#F7DFD9')
                  ))
```

- [ ] **Step 2: Update the Statistical Surplus columns at `server.R:818-820`**

Replace:

```r
      formatStyle('High',   backgroundColor = '#d4edda') %>%
      formatStyle('Medium', backgroundColor = '#fff3cd') %>%
      formatStyle('Low',    backgroundColor = '#f8d7da')
```

with:

```r
      formatStyle('High',   backgroundColor = '#E2EFE4') %>%
      formatStyle('Medium', backgroundColor = '#FAF0D7') %>%
      formatStyle('Low',    backgroundColor = '#F7DFD9')
```

- [ ] **Step 3: Confirm no old hexes remain**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL && \
  grep -rn "d4edda\|fff3cd\|f8d7da" LeagueEval/ || echo "clean — no old tier hexes remain"
```

Expected: `clean — no old tier hexes remain`.

- [ ] **Step 4: Launch and verify**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL && \
  Rscript -e 'shiny::runApp("LeagueEval", port = 3838, launch.browser = FALSE)'
```

- **Analysis → Category Status by Team**: the `Tier` column cells are tinted warm green / warm gold / warm red, and still track High/Medium/Low correctly.
- **Analysis → Surplus by Team → Statistical**: the `High`/`Medium`/`Low` columns carry the same three tints, now reading as part of the paper palette rather than against it.

- [ ] **Step 5: Commit**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL
git add LeagueEval/server.R
git commit -m "style(leagueeval): warm tier background colors to match theme palette"
```

---

### Task 6: Roll out to the remaining four apps

Each app gets a `global.R` and the same two-argument `ui.R` swap. No other changes.

**Files:**
- Create: `draftTool/global.R`, `ProtectionTrades/global.R`, `Guardians/global.R`, `LiveDraftTool/global.R`
- Modify: `draftTool/ui.R:8-9`, `ProtectionTrades/ui.R:7-8`, `Guardians/ui.R:9-10`, `LiveDraftTool/ui.R:6-8`

**Interfaces:**
- Consumes: `dafl_theme()` and `dafl_brand(subtitle)` from Task 1.
- Produces: nothing.

- [ ] **Step 1: Create the four `global.R` files**

Identical contents in each of `draftTool/global.R`, `ProtectionTrades/global.R`, `Guardians/global.R`, `LiveDraftTool/global.R`:

```r
# Loads the shared DAFL visual theme. Shiny sources global.R first, with the
# working directory set to this app directory — before server.R calls setwd().
source("../code/daflTheme.R")
```

- [ ] **Step 2: Update `draftTool/ui.R`**

Replace lines 8-9:

```r
shinyUI(navbarPage("Live Auction Tool, v0.5",
                   theme = bs_theme(bootswatch = "flatly"),
```

with:

```r
shinyUI(navbarPage(title = dafl_brand("Live Auction"),
                   windowTitle = "DAFL Live Auction",
                   theme = dafl_theme(),
```

- [ ] **Step 3: Update `ProtectionTrades/ui.R`**

Replace lines 7-8:

```r
shinyUI(navbarPage("Offseason Trade Evaluator, v2.0",
        theme = bs_theme(bootswatch = "flatly"),
```

with:

```r
shinyUI(navbarPage(title = dafl_brand("Offseason Trades"),
        windowTitle = "DAFL Offseason Trades",
        theme = dafl_theme(),
```

- [ ] **Step 4: Update `Guardians/ui.R`**

Replace lines 9-10:

```r
    theme = bs_theme(bootswatch = "flatly"),
    "Cleveland Guardians Tracker",
```

with:

```r
    theme = dafl_theme(),
    title = dafl_brand("Guardians Tracker"),
    windowTitle = "Guardians Tracker",
```

- [ ] **Step 5: Update `LiveDraftTool/ui.R`**

Replace lines 6-8:

```r
shinyUI(navbarPage("DAFL Live Draft Tool v 2.0",
                   id = "mainNav",
                   theme = bs_theme(bootswatch = "flatly"),
```

with:

```r
shinyUI(navbarPage(title = dafl_brand("Live Draft"),
                   windowTitle = "DAFL Live Draft",
                   id = "mainNav",
                   theme = dafl_theme(),
```

- [ ] **Step 6: Confirm all four parse and no bootswatch references remain**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL
for a in draftTool ProtectionTrades Guardians LiveDraftTool; do
  Rscript -e "invisible(parse('$a/ui.R')); cat('$a/ui.R parses OK\n')"
done
grep -rn 'bootswatch = "flatly"' */ui.R || echo "clean — no live bootswatch themes remain"
```

Expected: four `parses OK` lines, then `clean — no live bootswatch themes remain`. (`LeagueEval/ui.R:12` still mentions bootswatch inside a comment; the grep pattern requires the live `theme =` form, so a hit there means Task 1 was not applied.)

- [ ] **Step 7: Launch each app and check for layout regressions**

Run each in turn, stopping with `Ctrl-C` between:

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL
Rscript -e 'shiny::runApp("draftTool",        port = 3839, launch.browser = FALSE)'
Rscript -e 'shiny::runApp("ProtectionTrades", port = 3840, launch.browser = FALSE)'
Rscript -e 'shiny::runApp("Guardians",        port = 3841, launch.browser = FALSE)'
Rscript -e 'shiny::runApp("LiveDraftTool",    port = 3842, launch.browser = FALSE)'
```

For each, confirm: navy navbar with the correct brand and subtitle, stitch line present, tables styled, every tab renders, no console errors. Additionally:
- **Guardians**: the `#gSettingsBtn` Settings button is legible on navy and vertically centred.
- **LiveDraftTool**: the header at `ui.R:24` is a flex row holding the Settings button plus others — confirm nothing overflows the 46px navbar. Its `Draft Player` / `Undo Last Pick` / `Clear Budget` / `Fetch Leaderboards` buttons should pick up the new button styling; check none became illegible.
- **draftTool** and **ProtectionTrades**: both use `verticalLayout` with no sidebar, so mainly check table styling and tab underlines.

- [ ] **Step 8: Commit**

```bash
cd /Users/cmcneilly/Dropbox/Personal/DAFL
git add draftTool/global.R ProtectionTrades/global.R Guardians/global.R LiveDraftTool/global.R \
        draftTool/ui.R ProtectionTrades/ui.R Guardians/ui.R LiveDraftTool/ui.R
git commit -m "feat(theme): apply shared DAFL theme to draftTool, ProtectionTrades, Guardians, LiveDraftTool"
```

---

## Rollback

Per app: restore `theme = bs_theme(bootswatch = "flatly")` and the original title string in `ui.R`, and delete that app's `global.R`. `code/dafl.scss`, `code/daflTheme.R`, and `scripts/check-dafl-theme.R` are additive and can be left in place. The Task 5 hex values revert independently of everything else.

## Known risks

- **First run needs network** for `font_google(..., local = TRUE)` to fetch and cache Inter and Zilla Slab. Every stack falls back to system fonts, so the apps degrade in appearance but never fail. To confirm the fallback, run with the sass cache cleared: `Rscript -e 'sass::sass_cache_get()$prune(0)'` while offline.
- **`autoWidth = FALSE` tables** (Injured, Streamers, My Targets in League Eval) size columns from rendered text; the font and padding change shifts those widths. Task 3 Step 4 checks them explicitly.
- **`:has()` support** — the summary-table treatment in Task 3 degrades to the standard table look on browsers without it. No functional impact.
- **Sticky headers** conflict with DT's `scrollX`/`scrollY`. Verified: no table in any of the five apps sets either option.
