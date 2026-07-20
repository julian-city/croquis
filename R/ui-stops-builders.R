# Build the stop edit/add form
#
# @param stop_id_val Current stop ID value (or "" for new)
# @param stop_name_val Current stop name value (or "" for new)
# @param is_new If TRUE, shows placeholder text in inputs
# @param lang "en" by default
build_stop_form <- function(
  stop_id_val = "",
  stop_name_val = "",
  is_new = TRUE,
  lang = "en"
) {
  div(
    class = "route-edit-form",
    tags$label(
      tr("lbl_stop_id", lang),
      info_popover(
        tr("pop_stop_id", lang),
        "https://gtfs.org/schedule/reference/#stopstxt",
        lang = lang
      )
    ),
    if (is_new) {
      tags$input(
        type = "text",
        id = "inline_stop_id",
        placeholder = tr("stop_ph_id", lang),
        value = stop_id_val
      )
    } else {
      tags$input(
        type = "text",
        id = "inline_stop_id",
        value = stop_id_val
      )
    },
    tags$label(
      tr("lbl_stop_name", lang),
      info_popover(
        tr("pop_stop_name", lang),
        "https://gtfs.org/schedule/reference/#stopstxt",
        lang = lang
      )
    ),
    if (is_new) {
      tags$input(
        type = "text",
        id = "inline_stop_name",
        placeholder = tr("stop_ph_name", lang),
        value = stop_name_val
      )
    } else {
      tags$input(
        type = "text",
        id = "inline_stop_name",
        value = stop_name_val
      )
    },
    div(
      class = "btn-row",
      tags$button(
        class = "btn-save",
        onclick = "saveEditingStop()",
        htmltools::HTML(paste0("&#10003; ", tr("btn_save", lang)))
      ),
      tags$button(
        class = "btn-cancel",
        onclick = "cancelEditingStop()",
        tr("btn_cancel", lang)
      )
    )
  )
}
