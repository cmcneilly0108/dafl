# Guardians Tracker — Clickable Player Names → Player Detail

Date: 2026-06-07
Status: Approved

## Goal

In the Cleveland Guardians Tracker Shiny app (`Guardians/`), make every player
name across all tabs a clickable link that navigates to the **Player Detail**
tab with that player pre-loaded in the search picker.

## Background

The app is a `navbarPage` (`ui.R`) with five tabs served by `server.R`:

- **Depth Chart** — an SVG baseball diamond with player-name `<text>` nodes,
  SP/RP side columns and a Bench line (plain text), plus a DT roster table.
- **Hot / Cold** — DT table, `Player` column.
- **Player Detail** — `selectizeInput("gPlayerPick", ...)` search box driving a
  rendered player card. **This is the navigation destination.**
- **Prospects** — Hitters / Pitchers DT sub-tables, `Name` column.
- **Risers & Transactions** — Risers / Transactions / IL DT sub-tables,
  `Player` column.

The Player Detail card looks players up by `gRoster$player`
(`server.R` `output$gPlayerCard`), so a clickable name only resolves to a card
when that name exists in `gRoster`.

## Decisions

1. **Link scope: everywhere.** Names are linked in every DT table *and* inside
   the Depth Chart SVG diamond, SP/RP columns, and Bench line.
2. **Only link matched players.** A name renders as a link only when it exists
   in `gRoster$player`; otherwise it renders as plain (escaped) text. This
   avoids dead links for Prospects rows matched by name only.

## Design

### Navigation mechanism

- Add `id = "gNav"` to the `navbarPage` in `ui.R`.
- Each linked name fires, on click,
  `Shiny.setInputValue('gPlayerClick', '<name>', {priority:'event'})` and
  returns false so no real navigation occurs. Inline `Shiny.setInputValue` is
  used directly — no custom JS message handler is registered.
- A single server observer handles every click:

  ```r
  observeEvent(input$gPlayerClick, {
    updateSelectizeInput(session, 'gPlayerPick', selected = input$gPlayerClick)
    updateNavbarPage(session, 'gNav', selected = "Player Detail")
  })
  ```

  `gPlayerPick` is a server-side selectize; `selected =` sets the value and the
  existing `output$gPlayerCard` reactive renders the card.

### Shared helpers (defined once in `server.R`)

- `jsStr(s)` — escape a string for safe embedding inside a single-quoted JS
  string literal in an HTML attribute (backslash, single quote; collapse any
  newlines). Handles names like `O'Brien`.
- `playerLink(name, display = name)` — returns an HTML `<a href="#" ...>` string
  when `name %in% gRoster$player`, else the HTML-escaped `display` as plain
  text. The anchor carries `cursor:pointer` styling and the inline `onclick`
  above. `display` lets callers show a suffix (e.g. an ERA) while linking on the
  bare name.

### Application points

**DT tables** — map the name column through `playerLink()` before building the
datatable:

- Hot / Cold (`gHotTable`), Depth Chart roster table, Prospects Hitters
  (`gProspectsH`), Prospects Pitchers (`gProspectsP`) — these already render
  with `escape = FALSE`; just wrap the name values.
- Risers (`gRisers`), Transactions (`gTxnTable`), IL (`gILTable`) — currently
  render with the default `escape = TRUE`. Switch each to `escape = FALSE` and
  wrap their free-text columns in `htmltools::htmlEscape()` so only intended
  HTML renders:
  - Risers: `Reason`
  - Transactions: `Description` (the only free-text column; Type/From/To are
    controlled values)
  - IL: `Injury`, `Latest Update`

**Depth Chart SVG diamond** — in `playersAt()`, render each name `<text>` with
`style="cursor:pointer"` and the inline `onclick` (using `jsStr()` on the raw
name), preserving the existing white fill / dark stroke styling. Only emit the
click handler when the name is in `gRoster$player`.

**Depth Chart SP/RP columns & Bench** — today names are pre-concatenated with
suffixes (`paste0(nm, era)`, `paste0(nm, " (", p, ")")`). Restructure the
grouping so the bare name and its suffix are kept separately, then render the
name via `playerLink(nm, display = nm)` followed by the plain-text suffix. The
SP/RP `pitcherCol()` and the Bench `<span>` switch to HTML rendering.

## Out of scope

- The Player Detail tab itself (it is the destination).
- Names that do not resolve to a `gRoster` row — rendered as plain text by
  design.
- Any change to data pulls, the HotScore model, or the snapshot schema.

## Testing

Manual verification against the running app (Playwright / webapp-testing skill):

1. Launch the app, click a player name in each tab (Depth Chart diamond, Depth
   roster table, Hot/Cold, Prospects H, Prospects P, Risers, Transactions, IL).
2. Confirm each click lands on **Player Detail** with the clicked player loaded
   and their card rendered.
3. Confirm a Prospects name not on the current roster renders as plain text
   (no link, no click).
4. Spot-check a name containing an apostrophe links and navigates correctly.
