# Internal UI helper: Bootstrap 3 info popover icon
# Used throughout Croquis to show contextual help tooltips
info_popover <- function(text, link = NULL) {
  tags$span(
    class = "info-icon",
    `data-toggle` = "popover",
    `data-trigger` = "click",
    `data-html` = "true",
    `data-placement` = "right",
    `data-content` = if (is.null(link)) {
      text
    } else {
      paste0(
        text,
        "<br><a href='",
        link,
        "' target='_blank'>Read more</a>"
      )
    },
    tabindex = "0",
    icon("info-circle", class = "text-muted")
  )
}