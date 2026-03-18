#' @keywords internal
#' @aliases croquis-package
"_PACKAGE"

# dplyr ----
#' @importFrom dplyr filter select mutate summarise group_by ungroup arrange
#'   left_join rename pull distinct across if_else case_when bind_rows
#'   row_number n lag lead all_of everything where count between first last
NULL

# tidyr ----
#' @importFrom tidyr unnest fill replace_na complete expand
NULL

# sf ----
#' @importFrom sf st_as_sf st_cast st_length st_nearest_feature st_coordinates
#'   st_buffer st_join st_distance st_sfc st_sf st_point st_linestring
#'   st_centroid st_transform st_bbox
NULL

# stringr ----
#' @importFrom stringr str_c str_detect
NULL

# purrr ----
#' @importFrom purrr map map_df keep reduce
NULL

# tibble ----
#' @importFrom tibble tibble as_tibble enframe
NULL

# lubridate ----
#' @importFrom lubridate as.duration duration days hours minutes seconds hms
#'   hour minute second now interval period
NULL

# data.table ----
#' @importFrom data.table as.data.table data.table
NULL

# rlang (used via !!sym() in gtfs_to_ssfs) ----
#' @importFrom rlang sym !!
NULL

# shiny ----
# Core shiny functions used heavily throughout the croquis() Shiny app.
#' @importFrom shiny shinyApp fluidPage fluidRow column navbarPage tabPanel
#'   titlePanel sidebarLayout sidebarPanel mainPanel wellPanel conditionalPanel
#'   actionButton downloadButton fileInput textInput numericInput selectInput
#'   checkboxInput radioButtons dateInput
#'   updateTextInput updateNumericInput updateSelectInput updateCheckboxInput
#'   updateRadioButtons updateDateInput
#'   reactive reactiveVal observe observeEvent isolate req validate need
#'   renderText renderUI textOutput uiOutput
#'   downloadHandler showNotification
#'   tags tagList HTML icon h3 h4 h5 p a br hr div span pre code strong helpText
NULL

# Packages used sparingly — call with pkg::function() in code instead.
# No bulk imports needed; just list them here for documentation.
#
# leaflet:     leaflet(), leafletOutput(), renderLeaflet(), leafletProxy(),
#              addProviderTiles(), addPolylines(), addCircleMarkers(),
#              addMarkers(), addPolygons(), addLayersControl(), fitBounds(),
#              setView(), clearGroup(), clearMarkers(), clearShapes(),
#              showGroup(), makeIcon(), markerOptions(), labelOptions(),
#              layersControlOptions()
#
# plotly:      plot_ly(), plotlyOutput(), renderPlotly(), layout(), config(),
#              style()
#
# DT:          DTOutput(), renderDT(), datatable()
#
# shinyjs:     useShinyjs(), runjs()
#
# htmltools:   HTML(), htmlEscape()
#
# htmlwidgets: onRender()
#
# gtfstools:   read_gtfs(), get_trip_speed(), write_gtfs()
#
# osrm:        osrmRoute()
#
# tools:       file_ext()  (base R, no import needed)
