# Internal UI helper: Bootstrap 3 info popover icon
# Used throughout Croquis to show contextual help tooltips
#
# `key` tags the popover for live language switching via the JS scanner
# (updateI18n); pass it for popovers in static UI. Popovers rendered
# inside renderUI re-translate on their own and only need `lang`, which
# localises the "Read more" link. `tokens` is a named list of raw HTML
# fragments substituted for {name} markers in `text`, both at build time
# (here) and at language switch (via jsTr replacements read from
# data-i18n-popover-token-* attributes).
info_popover <- function(
  text,
  link = NULL,
  key = NULL,
  lang = "en",
  tokens = NULL
) {
  if (!is.null(tokens)) {
    for (nm in names(tokens)) {
      text <- gsub(
        paste0("{", nm, "}"),
        as.character(tokens[[nm]]),
        text,
        fixed = TRUE
      )
    }
  }

  content <- if (is.null(link)) {
    text
  } else {
    paste0(
      text,
      "<br><a href='",
      link,
      "' target='_blank'>",
      tr("lbl_read_more", lang),
      "</a>"
    )
  }

  token_attrs <- list()
  if (!is.null(key) && !is.null(tokens)) {
    token_attrs <- lapply(tokens, as.character)
    names(token_attrs) <- paste0(
      "data-i18n-popover-token-",
      names(tokens)
    )
  }

  do.call(
    tags$span,
    c(
      list(
        class = "info-icon",
        `data-toggle` = "popover",
        `data-trigger` = "click",
        `data-html` = "true",
        `data-placement` = "right",
        `data-content` = content,
        `data-i18n-popover` = key,
        `data-i18n-popover-link` = if (!is.null(key)) link else NULL,
        tabindex = "0"
      ),
      token_attrs,
      list(icon("info-circle", class = "text-muted"))
    )
  )
}

# Internal UI helper: tag a Shiny input's inner text field for live
# placeholder translation via the data-i18n-placeholder scanner branch
# of updateI18n(). Works for textInput and fileInput, whose placeholder
# arguments accept character only: the placeholder is set at build time
# via tr(), and this wrapper adds the attribute the JS scanner needs to
# retranslate it on language switch. The key must be mirrored in
# inst/www/js/i18n.js.
i18n_placeholder <- function(tag, key) {
  htmltools::tagQuery(tag)$find(".form-control")$addAttrs(
    `data-i18n-placeholder` = key
  )$allTags()
}
