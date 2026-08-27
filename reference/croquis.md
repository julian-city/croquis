# Croquis: transit sketch planning Shiny app

Launches the Croquis Shiny app

## Usage

``` r
croquis(ssfs = NULL, lang = "en")
```

## Arguments

- ssfs:

  an optional SSFS to load into the app on launch. Defaults to NULL.

- lang:

  UI language code: `"en"` (English, default), `"fr"` (French), or
  `"es"` (Spanish). Sets the initial language for all interface
  elements. The language can also be changed mid-session via the
  selector in the top-right corner. Additional languages can be
  registered in `SUPPORTED_LANGS` (see `R/i18n.R`).

## Value

Does not inherently return anything

## Examples

``` r
if (FALSE) { # \dontrun{
#Launch the app to start a project from scratch or load a GTFS from within the app
croquis()

#Launch the app with a SSFS project pre-loaded
croquis(stm_metro)

#Launch the app in French
croquis(lang = "fr")
} # }
```
