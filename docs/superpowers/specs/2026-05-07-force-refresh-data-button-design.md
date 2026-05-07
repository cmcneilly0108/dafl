# Force Refresh Data button + relocation to Settings modal

## Background

The LeagueEval Shiny app has a "Refresh Data" button in the top-right
nav header. Clicking it re-sources `code/inSeasonPulse.r`, which itself
guards the expensive network fetches behind file-age staleness checks.
That means clicking Refresh does not necessarily refresh anything from
the network — if the cached files are recent, the button just rebuilds
the in-memory derived tables from existing CSV/JSON.

Two changes:

1. The Refresh button should bypass all four staleness checks so it
   forces a fresh fetch from every upstream source.
2. The button should live inside the Settings modal, not in the top
   nav.

## Goals

- A single "Refresh Data" click forces a refetch of: FanGraphs
  projections, salary info, CBS endpoints, injuries, Stuff+.
- The top nav contains only the Settings button.
- Clicking Refresh inside the Settings modal closes the modal and
  surfaces the existing in-progress / completion notifications.
- Default (non-button) sourcing of `inSeasonPulse.r` continues to
  honor the existing staleness checks. The flag is opt-in.

## Non-goals

- No change to the projection-system swap logic (already uses
  prebuilt pools and does not refetch).
- No change to the staleness windows (10h for projections + CBS, 20h
  for injuries / Stuff+).
- No change to fallback behavior on fetch failure (cached file
  fallback inside the injuries / Stuff+ `tryCatch` blocks remains).

## Design

### Force-refresh mechanism: env-var flag

`inSeasonPulse.r` is sourced (not called as a function) and is also
run from the command line outside Shiny. An environment variable is
the cleanest signal that crosses the source boundary without
requiring a refactor of the file's top-level structure.

Near the top of `code/inSeasonPulse.r` (after the `library()` calls
and the `activeProj` block, before the projection-fetch guard):

```r
forceRefresh <- nchar(Sys.getenv("DAFL_FORCE_REFRESH")) > 0
```

Then modify the four staleness guards:

- **Projections + salary** (currently line 58):
  `if (forceRefresh || projMissing || any(projAges > 10)) { ... }`
- **CBS endpoints** (currently line 66):
  `if (forceRefresh || is.na(cbsAge) || cbsAge > 10) { ... }`
- **Injuries** (currently line 296):
  `if (!forceRefresh && injAge < 20) { use cache } else { fetch }`
- **Stuff+** (currently line 312):
  `if (!forceRefresh && stuffAge < 20) { use cache } else { fetch }`

The fallback paths inside the injuries / Stuff+ fetch branches
(falling back to the cached CSV if the API call throws) are
unchanged. Forcing means "don't skip the fetch", not "fail loudly
if the fetch breaks".

### Server: Refresh handler

In `LeagueEval/server.R`, the existing `observeEvent(input$refreshBtn, …)`
gains:

- `removeModal()` as the first call so the Settings modal dismisses
  immediately.
- Set the env var before sourcing, clear it after — using `on.exit()`
  so the flag clears even if `source()` errors:

```r
observeEvent(input$refreshBtn, {
  removeModal()
  showNotification("Refreshing data... this may take a minute",
                   type = "message", duration = NULL, id = "refreshMsg")
  Sys.setenv(DAFL_FORCE_REFRESH = "1")
  on.exit(Sys.unsetenv("DAFL_FORCE_REFRESH"), add = TRUE)
  tryCatch({
    source("../code/inSeasonPulse.r", local = globalenv())
    # ... existing select-input updates and refreshCount bump ...
  }, error = function(e) {
    removeNotification("refreshMsg")
    showNotification(paste0("Refresh failed: ", e$message),
                     type = "error", duration = 10)
  })
})
```

### Settings modal

In the `observeEvent(input$settingsBtn, …)` block, add an action
button below the projection radio group, separated by a small
spacer:

```r
showModal(modalDialog(
  title = "Settings",
  size = "s",
  easyClose = TRUE,
  radioButtons('projSource', 'Projection System', ...),
  tags$hr(),
  actionButton('refreshBtn', 'Refresh Data',
               class = 'btn-success btn-sm',
               icon = icon('refresh')),
  footer = modalButton("Close")
))
```

The button id stays `refreshBtn` so the existing observer fires
unchanged.

### UI: top nav

In `LeagueEval/ui.R`, remove `refreshBtn` and the surrounding
flex `tags$div` wrapper. The remaining `settingsBtn` is positioned
directly via the existing inline style (or kept inside a simpler
wrapper).

## Risks and mitigations

- **Env-var leak across processes.** `Sys.setenv` / `Sys.unsetenv`
  affect only the current R process, which is the Shiny worker.
  Safe.
- **Source error leaving the flag set.** Mitigated by `on.exit()`
  inside the handler.
- **Concurrent users.** The DAFL eval app is a single-user local
  tool, so there's no concern about one user's force-refresh
  affecting another's source.

## Test plan (manual — Shiny app, no automated tests)

After implementing, run the app and confirm:

1. Top-right nav shows only the Settings button.
2. Clicking Settings opens the modal with the projection radios and
   a green Refresh Data button.
3. Clicking Refresh Data closes the modal immediately and shows the
   "Refreshing data... this may take a minute" notification.
4. The R console shows fresh fetches running, with no
   `Using cached injuries file` or `Using cached Stuff+ file`
   messages even if those files are <20 hours old.
5. On completion the "Data refreshed!" notification appears and
   tables update.
6. Running `source("code/inSeasonPulse.r")` from the command line
   (without setting the env var) still respects the staleness
   checks — i.e. unchanged from today's behavior.
