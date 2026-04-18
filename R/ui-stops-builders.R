# Build the stop edit/add form
#
# @param stop_id_val Current stop ID value (or "" for new)
# @param stop_name_val Current stop name value (or "" for new)
# @param is_new If TRUE, shows placeholder text in inputs
build_stop_form <- function(
  stop_id_val = "",
  stop_name_val = "",
  is_new = TRUE
) {
  div(
    class = "route-edit-form",
    tags$label(
      "Stop ID",
      info_popover(
        "Unique identifier for a stop, station or platform.",
        "https://gtfs.org/schedule/reference/#stopstxt"
      )
    ),
    if (is_new) {
      tags$input(
        type = "text",
        id = "inline_stop_id",
        placeholder = "e.g., S001",
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
      "Stop name",
      info_popover(
        "Name of the stop, station or platform. It should match the agency's rider-facing name for the location as printed on a timetable, published online, or represented on signage.",
        "https://gtfs.org/schedule/reference/#stopstxt"
      )
    ),
    if (is_new) {
      tags$input(
        type = "text",
        id = "inline_stop_name",
        placeholder = "e.g., Main St Station",
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
        htmltools::HTML("&#10003; Save")
      ),
      tags$button(
        class = "btn-cancel",
        onclick = "cancelEditingStop()",
        "Cancel"
      )
    )
  )
}
