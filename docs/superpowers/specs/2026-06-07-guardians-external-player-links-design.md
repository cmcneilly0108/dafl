# Guardians Tracker — External Player Links (FanGraphs + Baseball Savant)

Date: 2026-06-07
Status: Approved

## Goal

On the **Player Detail** card in the Cleveland Guardians Tracker Shiny app
(`Guardians/server.R`, `output$gPlayerCard`), add a row of buttons — below the
HotScore trend graph — that open the selected player's **FanGraphs** and
**Baseball Savant** pages in a new browser tab.

## Background

An iframe embed was considered and tested first. It works technically
(no `X-Frame-Options`/CSP blocking; Cloudflare allows a real browser), but it
pulls in the entire FanGraphs site (nav, ads, sidebar), depends on Cloudflare
staying permissive, and only ~16% of roster players carry a FanGraphs id in
`gRoster`. New-tab links avoid all three problems: native full pages, no embed
fragility, and graceful per-player handling of a missing id.

The Player Detail card (`output$gPlayerCard`) renders, in order: `headerUI`
(dark name header), `heroUI` (season slash / pitching line), and `trendUI`
(HotScore plot or a "not enough history" note), wrapped in a single
`tags$div`. Early returns handle the empty picker and the
not-in-roster cases before any of these are built.

## Decisions

1. **Links, not an iframe** — open in a new tab.
2. **Both sites.** Baseball Savant (keyed on `mlb_id`) and FanGraphs (keyed on a
   FanGraphs id).
3. **Placement: under the graph** — a button row appended after `trendUI`.
4. **Missing FanGraphs id → hide the FanGraphs button.** Baseball Savant is
   always shown (every roster player has an `mlb_id`). No guessed/search links.

## Design

### URL construction

Both sites treat the numeric id as the key and the name slug as cosmetic — a
Savant URL with a wrong slug 301-redirects to the canonical page, verified. A
best-effort slug is built for readability but correctness does not depend on it.

- **Slug helper** (`slugify(name)`): transliterate accents to ASCII
  (`iconv(..., to = "ASCII//TRANSLIT")`), lowercase, replace each run of
  non-`[a-z0-9]` with a single hyphen, strip leading/trailing hyphens. Defined
  once at the top of the server function alongside the other helpers.

- **Baseball Savant URL:**
  `https://baseballsavant.mlb.com/savant-player/{slug}-{mlb_id}`

- **FanGraphs URL:**
  `https://www.fangraphs.com/players/{slug}/{fg_id}/stats/{batting|pitching}`
  where the trailing segment is `pitching` for pitchers and `batting`
  otherwise.

### Role detection (FanGraphs batting vs pitching)

Use the season stats role when present, else fall back to the roster position:

```r
isPitcher <- if (nrow(st) > 0 && !is.na(st$role[1])) {
               st$role[1] == "P"
             } else {
               row$pos %in% c("P","SP","RP","CL","MR","TWP")
             }
```

`st` and `row` are already computed earlier in `gPlayerCard`.

### FanGraphs id resolution

Try `gRoster.fg_id` first; fall back to the prospects data (`gProspects`, joined
by name — `pros` is already computed in `gPlayerCard` as
`gProspects %>% filter(Name == nm)`):

```r
fgid <- if (!is.na(row$fg_id) && nzchar(row$fg_id)) {
          as.character(row$fg_id)
        } else if (nrow(pros) > 0 && "PlayerId" %in% names(pros) &&
                   !is.na(pros$PlayerId[1]) && nzchar(as.character(pros$PlayerId[1]))) {
          as.character(pros$PlayerId[1])
        } else {
          NA_character_
        }
```

`NA` ⇒ no FanGraphs button for that player.

### Rendering (`linksUI`)

A bordered panel matching the card's existing section styling
(`padding:12px 16px; border:1px solid #ddd; border-top:none;`), holding up to two
anchors styled as Bootstrap buttons (the flatly theme provides `.btn`), each
`target="_blank" rel="noopener"`:

- FanGraphs button (only when `fgid` is non-NA), with a right margin.
- Baseball Savant button (always).

`linksUI` is appended after `trendUI` in the final `tags$div` of `gPlayerCard`.

## Out of scope

- The other tabs (links live only on the Player Detail card).
- Any data-pull / schema change. `mlb_id`, `fg_id`, and `gProspects.PlayerId`
  already exist.
- Players not in the current roster (handled by the existing early return).

## Testing

Manual verification against the running app (Playwright / webapp-testing):

1. Pick an MLB veteran with a FanGraphs id (e.g. José Ramírez): confirm both
   buttons appear under the graph and each opens the correct page in a new tab,
   FanGraphs on the correct batting/pitching view.
2. Pick a pitcher with an id: confirm the FanGraphs link uses `/stats/pitching`.
3. Pick a lower-level player with no resolvable FanGraphs id: confirm only the
   Baseball Savant button shows, and it opens the right player.
4. Spot-check a name with an accent (e.g. José Ramírez): confirm the slug is
   clean and the links resolve.
