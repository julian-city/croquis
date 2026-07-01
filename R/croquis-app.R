#' Croquis: transit sketch planning Shiny app
#'
#' Launches the Croquis Shiny app
#'
#' @param ssfs an optional SSFS to load into the app on launch. Defaults to NULL.
#'
#' @returns Does not inherently return anything
#'
#' @export
#' @examples
#' \dontrun{
#' #Launch the app to start a project from scratch or load a GTFS from within the app
#' croquis()
#'
#' #Launch the app with a SSFS project pre-loaded
#' croquis(stm_metro)
#' }
croquis <- function(ssfs = NULL) {
  # Validate input ssfs (and change name to avoid name collision in the server)

  input_ssfs <- NULL

  if (!is.null(ssfs)) {
    validate_ssfs(ssfs, verbose = FALSE) # throws informative error if invalid
    input_ssfs <- ssfs
  }

  #convert typical format ssfs into format ready to immediately assign to the ReactiveVal

  if (!is.null(input_ssfs)) {
    # Join stop_name into stop_seq (the app expects this column)
    stop_id_to_stopname <-
      input_ssfs$stops |> as.data.frame() |> select(stop_id, stop_name)

    input_ssfs$stop_seq <-
      input_ssfs$stop_seq |>
      left_join(stop_id_to_stopname, by = "stop_id")

    # Ensure CRS 4326 for spatial tables
    input_ssfs$itin <-
      input_ssfs$itin |>
      st_transform(4326)

    input_ssfs$stops <-
      input_ssfs$stops |>
      st_transform(4326)
  }

  # Detect number of cores for parallel processing (used in conversion functions between gtfs and ssfs)
  detected_cores <- parallel::detectCores(logical = FALSE)
  default_gtfs_workers <- if (is.na(detected_cores) || detected_cores < 1) {
    1L
  } else {
    as.integer(min(4L, detected_cores))
  }

  #UI-----------------------------

  # UI Definition
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    # CSS and JavaScript in the head
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "www/css/croquis.css"
      ),

      tags$script(src = "www/js/theme.js"),
      tags$script(src = "www/js/loading.js"),
      tags$script(src = "www/js/agency.js"),
      tags$script(src = "www/js/stops.js"),
      tags$script(src = "www/js/routes.js"),
      tags$script(src = "www/js/itineraries.js"),
      tags$script(src = "www/js/popovers.js"),
      tags$script(src = "www/js/schedule.js"),
      tags$script(src = "www/js/undo.js")
    ),

    #loading indicator div
    div(id = "loading-content", div(class = "loading-spinner")),

    #Module architecture
    navbarPage(
      title = "Croquis",
      # div for the undo/redo buttons and theme toggle
      header = tagList(
        div(
          style = "position: absolute; right: 10px; top: 10px; z-index: 1000; display: flex; gap: 4px; align-items: center;",
          tags$button(
            id = "undo_btn",
            onclick = "Shiny.setInputValue('undo_click', Math.random(), {priority:'event'})",
            class = "btn btn-default btn-sm",
            title = "Undo (Ctrl+Z)",
            disabled = "disabled",
            icon("rotate-left", class = "fa-solid")
          ),
          tags$button(
            id = "redo_btn",
            onclick = "Shiny.setInputValue('redo_click', Math.random(), {priority:'event'})",
            class = "btn btn-default btn-sm",
            title = "Redo (Ctrl+Shift+Z)",
            disabled = "disabled",
            icon("rotate-right", class = "fa-solid")
          ),
          tags$button(
            id = "theme-toggle",
            onclick = "toggleTheme()",
            class = "btn btn-default btn-sm",
            htmltools::HTML("&#9680;")
          )
        )
      ),

      # Home / Agency module
      tabPanel(
        tags$span(icon("house")),
        #unicode house emoji
        fluidPage(
          titlePanel("Home"),

          wellPanel(
            style = "font-size: 14px; margin-bottom: 12px; line-height: 1.5; color: var(--text-color);",
            p(
              tags$strong(
                "Croquis (crow-KEY) is a transit sketch planning tool and GTFS creator."
              ),
              "The stops, routes and schedule tabs above allow you to manage all these aspects of your transit network model.",
              "Get started on this page by loading an existing network, or by creating the agency details and projet location if starting from scratch.",
              "This open-source software was developed in R Shiny. It is in active development.  Save your work often by clicking the Save",
              icon("floppy-disk", class = "fa-solid"),
              "icon above and exporting your project file.",
              "Please report any bugs and provide your ideas for improvement by submitting an",
              tags$a(
                "issue on GitHub",
                href = "https://github.com/julian-city/croquis/issues/new",
                target = "_blank"
              ),
              "."
            )
          ),

          # -- Load Network (collapsible, collapsed by default) --
          div(
            id = "load-network-panel",
            class = "collapsible-section collapsed",
            div(
              class = "collapsible-section-header",
              onclick = "togglePanel('load-network-panel')",
              h4("Load Network"),
              tags$button(
                class = "floating-panel-toggle",
                htmltools::HTML("+")
              )
            ),
            div(
              class = "floating-panel-content",
              # Top row: GTFS + Croquis side by side
              fluidRow(
                column(
                  6,
                  wellPanel(
                    h4("Load a GTFS"),
                    p(
                      "You can load an existing GTFS here.",
                      tags$br(),
                      tags$small(
                        "Larger files may take several minutes",
                        "(maximum size: 100MB)."
                      )
                    ),
                    fileInput(
                      "load_gtfs",
                      "",
                      multiple = FALSE,
                      accept = ".zip",
                      placeholder = "Drag and drop or click to select file"
                    ),
                    tags$small(
                      "Uploading a GTFS here will convert it to an",
                      "editable format in Croquis"
                    )
                  )
                ),
                column(
                  6,
                  wellPanel(
                    h4("Load your croquis"),
                    p(
                      "To continue working on a previous croquis,",
                      "upload your .rds file:"
                    ),
                    fileInput(
                      "load_ssfs",
                      "",
                      multiple = FALSE,
                      accept = ".rds",
                      placeholder = "Drag and drop or click to select file"
                    ),
                    tags$small(
                      "Upload a transit model .rds file previously",
                      "created with Croquis"
                    )
                  )
                )
              ),
              # Bottom row: Sample networks
              wellPanel(
                h4("Load a sample transit network"),
                p(
                  "To explore this tool, you can get started by",
                  "loading a sample network. The Ligne Jaune model",
                  "is the simplest and will help you familiarize",
                  "yourself with how Croquis works."
                ),
                actionButton(
                  "load_yellowline_ssfs",
                  "STM Ligne Jaune",
                  class = "btn-success"
                ),
                actionButton(
                  "load_metro_ssfs",
                  "STM Metro",
                  class = "btn-success"
                ),
                actionButton(
                  "load_mileend_ssfs",
                  "STM Mile-End bus network",
                  class = "btn-success"
                ),
                actionButton(
                  "load_ttcsubway_ssfs",
                  "TTC Subway",
                  class = "btn-success"
                ),
                actionButton(
                  "load_translink_ssfs",
                  "TransLink Vancouver",
                  class = "btn-success"
                )
              )
            )
          ),

          # -- Project Location + Map --
          fluidRow(
            # Left column: Project Location
            column(
              4,
              wellPanel(
                #style = paste0(
                #  "height: 30vh; min-height: 200px; ",
                #  "overflow-y: auto; margin-bottom: 15px;"
                #),
                h4("Project Location"),
                div(
                  style = "display: flex; align-items: flex-end; gap: 8px;",
                  div(
                    style = "position: relative; flex: 1;",
                    textInput(
                      "city_search",
                      tags$label(
                        "Search for a city",
                        info_popover(
                          "Start typing a city name and select city, if starting project from scratch. If you are not able to find your city, you made need to set coordinates manually below.",
                        )
                      ),
                      placeholder = "Type city name...",
                      width = "100%"
                    ),
                    div(
                      id = "city_suggestions",
                      class = "suggestions-panel"
                    )
                  ),
                  div(
                    style = "margin-bottom: 15px;",
                    actionButton(
                      "select_city",
                      "Select City",
                      class = "btn-info"
                    )
                  )
                ),
                tags$small("Updates the map center and fetches timezone"),
                h5("...Or set project coordinates manually"),
                fluidRow(
                  column(
                    6,
                    numericInput(
                      "manual_lat",
                      "Latitude",
                      value = NA,
                      min = -90,
                      max = 90,
                      step = 0.00001
                    )
                  ),
                  column(
                    6,
                    numericInput(
                      "manual_lng",
                      "Longitude",
                      value = NA,
                      min = -180,
                      max = 180,
                      step = 0.00001
                    )
                  )
                )
              )
            ),
            # Right column: Map
            column(
              8,
              div(
                style = paste0(
                  "height: 30vh; min-height: 200px; ",
                  "margin-bottom: 15px; ",
                  "border: 1px solid var(--border-color); ",
                  "border-radius: 4px; overflow: hidden;"
                ),
                leaflet::leafletOutput(
                  "agency_map",
                  height = "100%",
                  width = "100%"
                )
              )
            )
          ),

          # -- Agency list --
          wellPanel(
            style = "overflow-y: auto; max-height: 60vh;",
            h4("Agencies"),
            div(
              class = "agency-list-container",
              uiOutput("agency_list_ui")
            )
          ),

          # -- Instructions --
          wellPanel(
            h3("Instructions"),
            p(
              "Build your transit system model by following these steps:"
            ),

            h4(
              "1. Get started here by loading an existing network or specifying agency details for a new one"
            ),
            p(
              tags$ul(
                tags$li(
                  "Load a GTFS or a network that you've previously worked on in Croquis"
                ),
                tags$li(
                  "Set the location of your network, if you're starting a network from scratch"
                ),
                tags$li("View and edit agency details.")
              )
            ),

            h4("2. Create and edit stops in the stops module"),
            p(
              tags$ul(
                tags$li("Manage and create stops using the left-hand panel"),
                tags$li(
                  "When creating or editing a stop, click on the map or drag the stop to set its location."
                ),
                tags$li(
                  "Provide unique stop IDs and stop names for each stop"
                ),
              )
            ),

            h4(
              "3. Create your routes and route itineraries in the routes module"
            ),
            p(
              tags$ul(
                tags$li(
                  "Create routes with their details (mode, colours) and define route itineraries within each route."
                ),
                tags$li(
                  "A route itinerary corresponds to a unique stop pattern for trips. Each itinerary is associated with a stop sequence and a shape."
                ),
                tags$li(
                  "Create and edit route geometries by selecting stops in the desired order and by creating waypoints by clicking on the map and along the route. You may delete waypoints or remove stops from a route itinerary by right-clicking."
                ),
                tags$li(
                  "Move a waypoint by clicking on it and activating editing mode. Click on the desired location on the map or on a stop to move the waypoint there. If clicked on a stop, it will be added to the sequence."
                ),
                tags$li(
                  "Toggle between network and simple drawing modes. Network drawing mode calculates the path along the Open Street Maps road network between stops and waypoints."
                ),
                tags$li(
                  "Toggle between prepending and appending stops when drawing a route itinerary. Prepend mode adds stops clicked to the beginning of the stop sequence (the default is that stops clicks are added to the end)."
                )
              )
            ),

            h4(
              "4. Define and edit service levels and speeds for routes in the schedule module"
            ),
            p(
              tags$ul(
                tags$li(
                  "Bulk apply preset service levels (e.g. all-day frequent or peak frequent), speeds and operating hours to routes by service."
                ),
                tags$li(
                  "View cumulative service-level by route segment by hour by clicking on the map."
                ),
                tags$li(
                  "Apply preset service levels, speeds and operating hours for individual route itineraries."
                ),
                tags$li(
                  "Define and edit headways and speeds by hour in detail for individual route itineraries, if desired."
                ),
                tags$li(
                  "View and toggle interstop speeds at the route itinerary level, if desired."
                ),
                tags$li(
                  "Manage service level presets, create them from scratch, or create them based on the service level of an existing route itinerary."
                ),
                tags$li(
                  "Manage service calendar, including start and end dates for services defined by day of the week active (e.g. weekday vs. weekend service)."
                )
              )
            ),

            h4(
              "5. Click the save",
              icon("floppy-disk", class = "fa-solid"),
              "icon to export a GTFS or save your croquis in .rds format to work on it later"
            )
          )
        )
      ),

      #stops tab
      stopsUI("stops"),

      #routes tab
      routesUI("routes"),

      # Schedule tab
      scheduleUI("schedule"),

      #export tab
      tabPanel(
        tags$span(icon("floppy-disk", class = "fa-solid")),
        fluidPage(
          titlePanel("export or save your project"),

          # Export gtfs
          wellPanel(
            h3("Export GTFS"),
            textInput("exportgtfs_filename", "Filename:", value = "gtfs.zip"),
            checkboxInput(
              "include_dist_traveled",
              "Include shape_dist_traveled",
              value = FALSE
            ),
            tags$small(
              "When checked, adds shape_dist_traveled to shapes and stop_times tables. This increases export time."
            ),
            tags$br(),
            tags$br(),
            downloadButton(
              "download_gtfs",
              "Download GTFS",
              class = "btn-primary"
            )
          ),

          # Export raw ssfs
          wellPanel(
            h3("Save your project to work on it later"),
            p(
              "This saves the raw Croquis (SSFS) file as a .rds:"
            ),
            textInput(
              "exportssfs_filename",
              "Filename:",
              value = "croquis.rds"
            ),
            downloadButton(
              "download_ssfs",
              "Download Croquis file",
              class = "btn-primary"
            ),
            tags$br(),
            tags$br(),
            tags$small(
              "Your transit system will be saved as an .rds file that you can reload later."
            )
          )
        )
      ),

      #settings tab
      tabPanel(
        tags$span(icon("gear")),
        fluidPage(
          titlePanel("settings"),

          wellPanel(
            h3("Feed info"),
            textInput(
              "fi_feed_publisher_name",
              label = tagList(
                "Publisher name",
                info_popover(
                  "Full name of the organization that publishes the feed.",
                  "https://gtfs.org/schedule/reference/#feed_infotxt"
                )
              ),
              value = "Comotive"
            ),
            textInput(
              "fi_feed_publisher_url",
              label = tagList(
                "Publisher URL",
                info_popover(
                  "URL of the feed publishing organization's website.",
                  "https://gtfs.org/schedule/reference/#feed_infotxt"
                )
              ),
              value = "https://www.comotive.net"
            ),
            selectInput(
              "fi_feed_lang",
              label = tagList(
                "Feed language",
                info_popover(
                  "Default language used for text in this dataset (IETF BCP 47 language code).",
                  "https://gtfs.org/schedule/reference/#feed_infotxt"
                )
              ),
              choices = local({
                lc <- ISOcodes::ISO_639_2[!is.na(ISOcodes::ISO_639_2$Alpha_2), ]
                ch <- stats::setNames(
                  lc$Alpha_2,
                  paste0(lc$Name, " (", lc$Alpha_2, ")")
                )
                ch[order(names(ch))]
              }),
              selected = "en"
            ),
            textInput(
              "fi_feed_contact_email",
              label = tagList(
                "Contact email",
                info_popover(
                  "Email address for communication regarding the GTFS dataset and data publishing practices.",
                  "https://gtfs.org/schedule/reference/#feed_infotxt"
                )
              ),
              value = "julian@comotive.net"
            ),
            textInput(
              "fi_feed_version",
              label = tagList(
                "Version",
                info_popover(
                  "String that indicates the current version of their GTFS dataset.",
                  "https://gtfs.org/schedule/reference/#feed_infotxt"
                )
              ),
              value = paste0("v", Sys.Date())
            )
          ),

          # Advanced settings panel
          wellPanel(
            h3("Advanced settings"),
            selectInput(
              "settings_routing_server",
              label = tagList(
                "Default routing server",
                info_popover(
                  "Routing server used to draw segments along the road network between stops and waypoints in the routes module."
                )
              ),
              choices = c("OSRM", "Valhalla"),
              selected = "OSRM"
            ),
            numericInput(
              "settings_gtfs_workers",
              label = tagList(
                "GTFS import workers",
                info_popover(
                  "Number of worker processes to use during GTFS to SSFS conversion. Values above 1 speed up imports on Linux servers; Windows falls back to a single worker."
                )
              ),
              value = default_gtfs_workers,
              min = 1,
              max = if (is.na(detected_cores) || detected_cores < 1) {
                32
              } else {
                detected_cores
              },
              step = 1,
              width = "240px"
            ),
            numericInput(
              "settings_min_stop_dist",
              label = tagList(
                "Minimum stop spacing (m)",
                info_popover(
                  "Minimum distance in metres between auto-generated stops. Also used as the buffer distance around existing stops when determining eligible locations for new stops."
                )
              ),
              value = 200,
              min = 50,
              max = 1000,
              step = 50
            ),
            selectInput(
              "settings_osm_provider",
              label = tagList(
                "OSM extract provider",
                info_popover(
                  "OpenStreetMap data provider used when generating stops from road network data. Different providers have different regional coverage."
                )
              ),
              choices = suppressMessages(
                osmextract::oe_providers()$available_providers
              ),
              selected = "openstreetmap_fr"
            )
          )
        )
      )
    )
  )

  #SERVER-------------------------

  shiny::addResourcePath("www", system.file("www", package = "croquis"))

  server <- function(input, output, session) {
    options(shiny.maxRequestSize = 100 * 1024^2)

    #   #   #
    #
    #   REACTIVE VALUES AND FUNCTIONS
    #
    #   #   #

    # Initialize ssfs : data structure for the whole app
    # ssfs_raw holds the current SSFS state.  The ssfs() wrapper defined
    # below adds undo/redo history tracking on every write.

    ssfs_raw <- reactiveVal(
      if (!is.null(input_ssfs)) {
        input_ssfs
      } else {
        list(
          agency = data.frame(
            agency_id = character(),
            agency_name = character(),
            agency_url = character(),
            agency_timezone = character()
          ),
          routes = data.frame(
            route_id = character(),
            agency_id = character(),
            route_short_name = character(),
            route_long_name = character(),
            route_type = integer(),
            route_color = character(),
            route_text_color = character()
          ),
          stops = st_sf(
            stop_id = character(),
            stop_name = character(),
            geometry = st_sfc(crs = 4326)
          ),
          itin = st_sf(
            itin_id = character(),
            route_id = character(),
            direction_id = integer(),
            trip_headsign = character(),
            geometry = st_sfc(crs = 4326)
          ),
          stop_seq = data.frame(
            itin_id = character(),
            stop_id = character(),
            stop_sequence = integer(),
            speed_factor = double(),
            stop_name = character()
          ),
          #default mon-sun service loaded
          calendar = data.frame(
            service_id = "mon-sun",
            monday = 1L,
            tuesday = 1L,
            wednesday = 1L,
            thursday = 1L,
            friday = 1L,
            saturday = 1L,
            sunday = 1L,
            start_date = format(Sys.Date(), "%Y-%m-%d"),
            end_date = format(Sys.Date() + 365, "%Y-%m-%d"),
            stringsAsFactors = FALSE
          ),
          span = data.frame(
            itin_id = character(),
            service_id = character(),
            service_window = integer(),
            first_dep = character(),
            last_dep = character()
          ),
          hsh = data.frame(
            itin_id = character(),
            service_id = character(),
            hour_dep = character(),
            headway = integer(),
            speed = double()
          )
        )
      }
    )
    #stringsAsFactors = FALSE used to be in each table, removed as it is not relevant
    #for versions of R > 4.0

    # --- Undo / redo history ---
    ssfs_history <- reactiveVal(list()) # undo stack
    ssfs_future <- reactiveVal(list()) # redo stack
    SSFS_MAX_HISTORY <- 40L

    # History-aware wrapper
    # Read:  ssfs()           -- returns current value, registers reactive dependency
    # Write: ssfs(new_value)  -- pushes previous state to undo stack, clears redo stack
    ssfs <- function(new_value) {
      if (missing(new_value)) {
        return(ssfs_raw())
      }
      # Push current state onto undo stack (isolate to avoid reactive dependency)
      history <- isolate(ssfs_history())
      history <- c(history, list(isolate(ssfs_raw())))
      if (length(history) > SSFS_MAX_HISTORY) {
        history <- history[
          (length(history) - SSFS_MAX_HISTORY + 1):length(history)
        ]
      }
      ssfs_history(history)
      # Any new edit clears the redo stack
      ssfs_future(list())
      ssfs_raw(new_value)
    }

    # --- Undo / redo event handlers ---

    observeEvent(input$undo_click, {
      if (isTRUE(input$routes_editing_active)) {
        return()
      }
      history <- ssfs_history()
      if (length(history) == 0) {
        return()
      }

      # Push current state onto redo stack
      future <- ssfs_future()
      future <- c(future, list(isolate(ssfs_raw())))
      ssfs_future(future)

      # Pop from undo stack and apply (write directly to ssfs_raw so we
      # don't push another history entry)
      prev_state <- history[[length(history)]]
      ssfs_history(history[-length(history)])
      ssfs_raw(prev_state)
    })

    observeEvent(input$redo_click, {
      if (isTRUE(input$routes_editing_active)) {
        return()
      }
      future <- ssfs_future()
      if (length(future) == 0) {
        return()
      }

      # Push current state onto undo stack
      history <- ssfs_history()
      history <- c(history, list(isolate(ssfs_raw())))
      ssfs_history(history)

      # Pop from redo stack and apply
      next_state <- future[[length(future)]]
      ssfs_future(future[-length(future)])
      ssfs_raw(next_state)
    })

    # Enable/disable the toolbar buttons based on stack state and whether editing an itinerary
    observe({
      editing <- isTRUE(input$routes_editing_active)
      shinyjs::toggleState(
        "undo_btn",
        condition = !editing && length(ssfs_history()) > 0
      )
      shinyjs::toggleState(
        "redo_btn",
        condition = !editing && length(ssfs_future()) > 0
      )
    })

    #reactive values for cities db and agency info on home page / in gtfs

    # Reactive values for map center and agency info
    map_center <- reactiveVal(
      if (!is.null(input_ssfs) && nrow(input_ssfs$stops) > 0) {
        bbox <- st_bbox(input_ssfs$stops)
        list(
          lng = (bbox[["xmin"]] + bbox[["xmax"]]) / 2,
          lat = (bbox[["ymin"]] + bbox[["ymax"]]) / 2
        )
      } else {
        list(lng = -73.567, lat = 45.5017) # Montreal default
      }
    )

    # TRUE when the loaded network has at least one stop with geometry
    network_has_stops <- reactive({
      current_data <- ssfs()
      nrow(current_data$stops) > 0 &&
        !all(sf::st_is_empty(current_data$stops$geometry))
    })

    # Filtered cities for autocomplete
    filtered_cities <- reactiveVal(data.frame())

    #current zoom reactive value
    current_zoom <- reactiveVal(10)

    # an update map draw function used to be here

    #an update agency form helper function used to be here

    # An observer to sync agency form inputs to ssfs$agency table used to be here

    #   #   #
    #
    ##   HOME MODULE-------
    #
    #   #   #

    # Handle ssfs file upload
    observeEvent(input$load_ssfs, {
      req(input$load_ssfs)
      tryCatch(
        {
          loaded_ssfs <- readRDS(input$load_ssfs$datapath)

          validate_ssfs(loaded_ssfs, verbose = FALSE)

          stop_id_to_stopname <-
            loaded_ssfs$stops |> as.data.frame() |> select(stop_id, stop_name)

          loaded_ssfs$stop_seq <-
            loaded_ssfs$stop_seq |>
            left_join(stop_id_to_stopname, by = "stop_id")

          loaded_ssfs$itin <-
            loaded_ssfs$itin |>
            st_transform(4326)

          loaded_ssfs$stops <-
            loaded_ssfs$stops |>
            st_transform(4326)

          ssfs(loaded_ssfs)

          #update center

          bbox <- st_bbox(loaded_ssfs$stops)

          # Calculate center point
          center <- list(
            lng = (bbox[["xmin"]] + bbox[["xmax"]]) / 2,
            lat = (bbox[["ymin"]] + bbox[["ymax"]]) / 2
          )

          map_center(center)

          showNotification(
            "Transit system loaded successfully",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Error loading file:", e$message),
            type = "error"
          )
        }
      )
    })

    # Handle gtfs file upload
    observeEvent(input$load_gtfs, {
      req(input$load_gtfs)
      tryCatch(
        {
          loaded_gtfs <- gtfstools::read_gtfs(input$load_gtfs$datapath)

          loaded_ssfs <- croquis::gtfs_to_ssfs(
            loaded_gtfs,
            routing_server = input$settings_routing_server,
            workers = input$settings_gtfs_workers
          )

          stop_id_to_stopname <-
            loaded_ssfs$stops |> as.data.frame() |> select(stop_id, stop_name)

          loaded_ssfs$stop_seq <-
            loaded_ssfs$stop_seq |>
            left_join(stop_id_to_stopname, by = "stop_id")

          loaded_ssfs$itin <-
            loaded_ssfs$itin |>
            st_transform(4326)

          loaded_ssfs$stops <-
            loaded_ssfs$stops |>
            st_transform(4326)

          ssfs(loaded_ssfs)

          #update center

          bbox <- st_bbox(loaded_ssfs$stops)

          # Calculate center point
          center <- list(
            lng = (bbox[["xmin"]] + bbox[["xmax"]]) / 2,
            lat = (bbox[["ymin"]] + bbox[["ymax"]]) / 2
          )

          map_center(center)

          showNotification("GTFS loaded successfully", type = "message")
        },
        error = function(e) {
          showNotification(
            paste("Error loading file:", e$message),
            type = "error"
          )
        }
      )
    })

    #handle load_ligne_jaune_ssfs
    #NOTE all sample network observe handlers
    # are a bit superflous now that the networks are internal objects
    #not necessary to use tryCatch
    observeEvent(input$load_yellowline_ssfs, {
      tryCatch(
        {
          loaded_ssfs <- croquis::ligne_jaune

          stop_id_to_stopname <-
            loaded_ssfs$stops |> as.data.frame() |> select(stop_id, stop_name)

          loaded_ssfs$stop_seq <-
            loaded_ssfs$stop_seq |>
            left_join(stop_id_to_stopname, by = "stop_id")

          loaded_ssfs$itin <-
            loaded_ssfs$itin |>
            st_transform(4326)

          loaded_ssfs$stops <-
            loaded_ssfs$stops |>
            st_transform(4326)

          ssfs(loaded_ssfs)

          map_center(list(lng = -73.567, lat = 45.5017))

          showNotification(
            "STM Ligne Jaune loaded successfully",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Error loading STM Ligne Jaune:", e$message),
            type = "error"
          )
        }
      )
    })

    #handle load_metro_ssfs
    observeEvent(input$load_metro_ssfs, {
      tryCatch(
        {
          loaded_ssfs <- croquis::stm_metro

          stop_id_to_stopname <-
            loaded_ssfs$stops |> as.data.frame() |> select(stop_id, stop_name)

          loaded_ssfs$stop_seq <-
            loaded_ssfs$stop_seq |>
            left_join(stop_id_to_stopname, by = "stop_id")

          loaded_ssfs$itin <-
            loaded_ssfs$itin |>
            st_transform(4326)

          loaded_ssfs$stops <-
            loaded_ssfs$stops |>
            st_transform(4326)

          ssfs(loaded_ssfs)

          map_center(list(lng = -73.567, lat = 45.5017))

          showNotification(
            "STM metro network loaded successfully",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Error loading STM metro network:", e$message),
            type = "error"
          )
        }
      )
    })

    #handle load_mileend_ssfs
    observeEvent(input$load_mileend_ssfs, {
      tryCatch(
        {
          loaded_ssfs <- croquis::mileend

          stop_id_to_stopname <-
            loaded_ssfs$stops |> as.data.frame() |> select(stop_id, stop_name)

          loaded_ssfs$stop_seq <-
            loaded_ssfs$stop_seq |>
            left_join(stop_id_to_stopname, by = "stop_id")

          loaded_ssfs$itin <-
            loaded_ssfs$itin |>
            st_transform(4326)

          loaded_ssfs$stops <-
            loaded_ssfs$stops |>
            st_transform(4326)

          ssfs(loaded_ssfs)

          map_center(list(lng = -73.567, lat = 45.5017))

          showNotification(
            "STM Mile-End bus network loaded successfully",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Error loading STM Mile-End bus network:", e$message),
            type = "error"
          )
        }
      )
    })

    #handle load_ttcsubway_ssfs
    observeEvent(input$load_ttcsubway_ssfs, {
      tryCatch(
        {
          loaded_ssfs <- croquis::ttc_subway

          stop_id_to_stopname <-
            loaded_ssfs$stops |> as.data.frame() |> select(stop_id, stop_name)

          loaded_ssfs$stop_seq <-
            loaded_ssfs$stop_seq |>
            left_join(stop_id_to_stopname, by = "stop_id")

          loaded_ssfs$itin <-
            loaded_ssfs$itin |>
            st_transform(4326)

          loaded_ssfs$stops <-
            loaded_ssfs$stops |>
            st_transform(4326)

          ssfs(loaded_ssfs)

          bbox <- st_bbox(loaded_ssfs$stops)
          center <- list(
            lng = (bbox[["xmin"]] + bbox[["xmax"]]) / 2,
            lat = (bbox[["ymin"]] + bbox[["ymax"]]) / 2
          )
          map_center(center)

          showNotification(
            "TTC subway network loaded successfully",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Error loading TTC subway network:", e$message),
            type = "error"
          )
        }
      )
    })

    #handle load_translink_ssfs
    observeEvent(input$load_translink_ssfs, {
      tryCatch(
        {
          loaded_ssfs <- croquis::translink

          stop_id_to_stopname <-
            loaded_ssfs$stops |> as.data.frame() |> select(stop_id, stop_name)

          loaded_ssfs$stop_seq <-
            loaded_ssfs$stop_seq |>
            left_join(stop_id_to_stopname, by = "stop_id")

          loaded_ssfs$itin <-
            loaded_ssfs$itin |>
            st_transform(4326)

          loaded_ssfs$stops <-
            loaded_ssfs$stops |>
            st_transform(4326)

          ssfs(loaded_ssfs)

          bbox <- st_bbox(loaded_ssfs$stops)
          center <- list(
            lng = (bbox[["xmin"]] + bbox[["xmax"]]) / 2,
            lat = (bbox[["ymin"]] + bbox[["ymax"]]) / 2
          )
          map_center(center)

          showNotification(
            "TransLink network loaded successfully",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Error loading TransLink network:", e$message),
            type = "error"
          )
        }
      )
    })

    # City search autocomplete
    observeEvent(input$city_search, {
      if (network_has_stops()) {
        return()
      }

      search_term <- input$city_search

      if (nchar(search_term) >= 2) {
        matches <- cities_db |>
          filter(stringr::str_detect(
            tolower(name),
            tolower(stringr::str_escape(search_term))
          ))

        if (nrow(matches) > 0 && nrow(matches) <= 10) {
          # Show suggestions if we have 1-10 matches
          filtered_cities(matches)

          # Create suggestion HTML
          suggestions_html <- paste0(
            "<div style='padding: 5px; cursor: pointer; border-bottom: 1px solid #eee;' ",
            "onclick='selectCity(\"",
            matches$name,
            "\")'>",
            matches$name,
            "</div>",
            collapse = ""
          )

          # Show suggestions dropdown
          session$sendCustomMessage("showSuggestions", suggestions_html)
        } else if (nrow(matches) == 1) {
          # Exactly one match - hide suggestions
          session$sendCustomMessage("hideSuggestions", "")
          filtered_cities(matches)
        } else {
          # No matches or too many matches
          session$sendCustomMessage("hideSuggestions", "")
          filtered_cities(data.frame())
        }
      } else {
        # Search term too short
        session$sendCustomMessage("hideSuggestions", "")
        filtered_cities(data.frame())
      }
    })

    # Handle city selection from dropdown
    observeEvent(input$selected_city_name, {
      updateTextInput(session, "city_search", value = input$selected_city_name)
      session$sendCustomMessage("hideSuggestions", "")
      #to force hiding suggestions
      filtered_cities(data.frame())
    })

    # Handle select city button
    observeEvent(input$select_city, {
      if (network_has_stops()) {
        showNotification(
          paste(
            "The map center is set from the loaded network's stops.",
            "Remove all stops to set a city manually."
          ),
          type = "warning"
        )
        return()
      }

      search_term <- input$city_search

      if (is.null(search_term) || search_term == "") {
        showNotification("Please enter a city name", type = "warning")
        return()
      }

      # Find exact matches (case insensitive)
      exact_matches <- cities_db[
        tolower(cities_db$name) == tolower(search_term),
      ]

      if (nrow(exact_matches) == 0) {
        showNotification(
          "City not found. Please select from the suggestions.",
          type = "warning"
        )
        return()
      } else if (nrow(exact_matches) > 1) {
        showNotification(
          "Multiple cities found with that name. Please be more specific.",
          type = "warning"
        )
        return()
      } else {
        # Exactly one match : update city center and form with agency timezone
        selected_city <- exact_matches[1, ]

        # Update map center
        map_center(list(lng = selected_city$long, lat = selected_city$lat))

        # Fill timezone field for new agencies added in the table
        session$sendCustomMessage("agFillTimezone", selected_city$tz)

        #store the last selected city's timezone
        ag_last_tz(selected_city$tz)

        # Hide suggestions
        session$sendCustomMessage("hideSuggestions", "")

        showNotification(
          paste("City set to:", selected_city$name),
          type = "message"
        )
      }
    })

    # Sync manual coordinate inputs with the current map center
    observe({
      center <- map_center()
      updateNumericInput(session, "manual_lat", value = round(center$lat, 5))
      updateNumericInput(session, "manual_lng", value = round(center$lng, 5))
    })

    # Lock manual coordinates once stops exist; reflect the network center
    observe({
      current_data <- ssfs()

      has_stops <- network_has_stops()

      if (has_stops) {
        shinyjs::disable("manual_lat")
        shinyjs::disable("manual_lng")

        bbox <- st_bbox(current_data$stops)
        updateNumericInput(
          session,
          "manual_lat",
          value = round((bbox[["ymin"]] + bbox[["ymax"]]) / 2, 5)
        )
        updateNumericInput(
          session,
          "manual_lng",
          value = round((bbox[["xmin"]] + bbox[["xmax"]]) / 2, 5)
        )

        shinyjs::disable("city_search")
        shinyjs::disable("select_city")
        session$sendCustomMessage("hideSuggestions", "")
      } else {
        shinyjs::enable("manual_lat")
        shinyjs::enable("manual_lng")
        shinyjs::enable("city_search")
        shinyjs::enable("select_city")
      }
    })

    # Apply manually entered coordinates to the map center (scratch projects only)
    manual_coords <- reactive({
      list(lat = input$manual_lat, lng = input$manual_lng)
    }) |>
      shiny::debounce(800)

    observeEvent(
      manual_coords(),
      {
        coords <- manual_coords()
        req(coords$lat, coords$lng)

        current_data <- isolate(ssfs())
        if (nrow(current_data$stops) > 0) {
          return()
        }

        if (
          !is.finite(coords$lat) ||
            !is.finite(coords$lng) ||
            coords$lat < -90 ||
            coords$lat > 90 ||
            coords$lng < -180 ||
            coords$lng > 180
        ) {
          showNotification(
            "Latitude must be between -90 and 90, longitude between -180 and 180",
            type = "warning"
          )
          return()
        }

        center <- isolate(map_center())
        if (
          isTRUE(all.equal(center$lat, coords$lat, tolerance = 1e-6)) &&
            isTRUE(all.equal(center$lng, coords$lng, tolerance = 1e-6))
        ) {
          return()
        }

        map_center(list(lng = coords$lng, lat = coords$lat))
      },
      ignoreInit = TRUE
    )

    #   #   #
    #
    ##   AGENCY MODULE-------
    #
    #   #   #

    # -- Agency reactive state --
    ag_editing_id <- reactiveVal(NULL) # agency_id being edited, or NULL
    ag_adding <- reactiveVal(FALSE) # TRUE when adding a new agency
    ag_last_tz <- reactiveVal(NULL) # timezone associated with selected city

    # -- Helper: build an agency display row --
    build_agency_row <- function(agency) {
      div(
        class = "agency-list-row",
        div(
          class = "agency-info",
          span(class = "agency-id-label", agency$agency_id),
          span(class = "agency-name-label", agency$agency_name),
          span(
            class = "agency-detail-line",
            paste0(
              agency$agency_timezone,
              if (nchar(agency$agency_url) > 0) {
                paste0(" \u2014 ", agency$agency_url)
              } else {
                ""
              }
            )
          )
        ),
        div(
          class = "route-actions",
          tags$button(
            class = "route-action-btn edit-btn",
            onclick = sprintf(
              "event.stopPropagation(); editAgencyFromList('%s')",
              agency$agency_id
            ),
            title = "Edit agency",
            htmltools::HTML("&#9998;")
          ),
          tags$button(
            class = "route-action-btn delete-btn",
            onclick = sprintf(
              "event.stopPropagation(); deleteAgencyFromList('%s')",
              agency$agency_id
            ),
            title = "Delete agency",
            htmltools::HTML('<i class="fa-solid fa-trash"></i>')
          )
        )
      )
    }

    # -- Helper: build the inline agency edit/add form --
    build_agency_form <- function(agency = NULL, default_tz = NULL) {
      is_new <- is.null(agency)
      div(
        class = "agency-edit-form",
        tags$label(
          "Agency ID",
          info_popover(
            "Identifies a unique transit agency or transit brand.",
            "https://gtfs.org/schedule/reference/#agencytxt"
          )
        ),
        tags$input(
          type = "text",
          id = "inline_ag_agency_id",
          value = if (!is_new) agency$agency_id else NULL,
          placeholder = if (is_new) "e.g., STM" else NULL
        ),
        tags$label(
          "Agency name",
          info_popover(
            "Full name of the transit agency.",
            "https://gtfs.org/schedule/reference/#agencytxt"
          )
        ),
        tags$input(
          type = "text",
          id = "inline_ag_agency_name",
          value = if (!is_new) agency$agency_name else NULL,
          placeholder = if (is_new) {
            "e.g., Soci\u00e9t\u00e9 de transport de Montr\u00e9al"
          } else {
            NULL
          }
        ),
        tags$label(
          "Agency URL",
          info_popover(
            "URL of the transit agency.",
            "https://gtfs.org/schedule/reference/#agencytxt"
          )
        ),
        tags$input(
          type = "text",
          id = "inline_ag_agency_url",
          value = if (!is_new) agency$agency_url else NULL,
          placeholder = if (is_new) "e.g., http://www.stm.info" else NULL
        ),
        tags$label(
          "Agency timezone",
          info_popover(
            "Timezone in IANA tz database format.",
            "https://gtfs.org/schedule/reference/#agencytxt"
          )
        ),
        tags$input(
          type = "text",
          id = "inline_ag_agency_timezone",
          value = if (!is_new) agency$agency_timezone else default_tz,
          placeholder = if (is_new) "e.g., America/Montreal" else NULL
        ),
        div(
          class = "btn-row",
          tags$button(
            class = "btn-save",
            onclick = "saveAgencyFromForm()",
            if (is_new) "Create" else htmltools::HTML("&#10003; Save")
          ),
          tags$button(
            class = "btn-cancel",
            onclick = "cancelAgencyEdit()",
            "Cancel"
          )
        )
      )
    }

    # -- Render the agency list UI --
    output$agency_list_ui <- renderUI({
      current_data <- ssfs()
      editing_id <- ag_editing_id()
      is_adding <- ag_adding()

      rows <- list()

      # Agency rows
      if (nrow(current_data$agency) > 0) {
        for (i in seq_len(nrow(current_data$agency))) {
          ag <- current_data$agency[i, ]
          is_editing_this <- !is.null(editing_id) &&
            editing_id == ag$agency_id

          # Always show the display row
          rows[[length(rows) + 1]] <- build_agency_row(ag)

          # If editing this row, show form directly below
          if (is_editing_this) {
            rows[[length(rows) + 1]] <- build_agency_form(ag)
          }
        }
      }

      # "Add new agency" row or add form
      if (is_adding) {
        rows[[length(rows) + 1]] <- build_agency_form(
          default_tz = isolate(ag_last_tz())
        )
      } else {
        rows[[length(rows) + 1]] <- div(
          class = "stop-list-row add-row",
          onclick = "startAddingAgency()",
          tags$button(
            class = "stop-action-btn add-btn",
            onclick = "event.stopPropagation(); startAddingAgency()",
            title = "Add new agency",
            htmltools::HTML("+")
          ),
          span(style = "margin-left: 8px;", "Add new agency")
        )
      }

      do.call(tagList, rows)
    })

    # -- Edit agency (pencil click) --
    observeEvent(input$ag_list_edit_click, {
      clicked_id <- input$ag_list_edit_click$id
      # Toggle: if already editing this one, cancel
      if (!is.null(ag_editing_id()) && ag_editing_id() == clicked_id) {
        ag_editing_id(NULL)
      } else {
        ag_editing_id(clicked_id)
        ag_adding(FALSE)
      }
    })

    # -- Start adding new agency --
    observeEvent(input$ag_list_add_click, {
      ag_adding(TRUE)
      ag_editing_id(NULL)
    })

    # -- Cancel edit / add --
    observeEvent(input$ag_list_cancel_click, {
      ag_editing_id(NULL)
      ag_adding(FALSE)
    })

    # -- Save agency (handles both add and edit) --
    observeEvent(input$ag_list_save_data, {
      data <- input$ag_list_save_data
      new_agency_id <- trimws(data$agency_id)

      if (nchar(new_agency_id) == 0) {
        showNotification("Agency ID cannot be empty.", type = "warning")
        return()
      }

      current_data <- ssfs()

      if (ag_adding()) {
        # -- Adding a new agency --
        if (new_agency_id %in% current_data$agency$agency_id) {
          showNotification(
            "This agency ID already exists. Please use a different ID.",
            type = "warning"
          )
          return()
        }

        new_agency <- data.frame(
          agency_id = new_agency_id,
          agency_name = trimws(data$agency_name),
          agency_url = trimws(data$agency_url),
          agency_timezone = trimws(data$agency_timezone),
          stringsAsFactors = FALSE
        )

        current_data$agency <- rbind(current_data$agency, new_agency)
        ssfs(current_data)
        ag_adding(FALSE)

        showNotification("Agency added successfully", type = "message")
      } else if (!is.null(ag_editing_id())) {
        # ── Editing an existing agency ──
        old_agency_id <- ag_editing_id()
        idx <- which(current_data$agency$agency_id == old_agency_id)

        if (length(idx) == 0) {
          showNotification("Agency not found.", type = "error")
          return()
        }

        # If agency_id changed, check for conflicts
        if (new_agency_id != old_agency_id) {
          other_ids <- current_data$agency$agency_id[-idx]
          if (new_agency_id %in% other_ids) {
            showNotification(
              "This agency ID already exists. Please use a different ID.",
              type = "warning"
            )
            return()
          }
        }

        # Update the row
        current_data$agency$agency_id[idx] <- new_agency_id
        current_data$agency$agency_name[idx] <- trimws(data$agency_name)
        current_data$agency$agency_url[idx] <- trimws(data$agency_url)
        current_data$agency$agency_timezone[idx] <- trimws(data$agency_timezone)

        # Cascade agency_id change to routes
        if (new_agency_id != old_agency_id && nrow(current_data$routes) > 0) {
          current_data$routes$agency_id[
            current_data$routes$agency_id == old_agency_id
          ] <- new_agency_id
        }

        ssfs(current_data)
        ag_editing_id(NULL)

        showNotification("Agency updated successfully", type = "message")
      }
    })

    # -- Delete agency (with route protection) --
    observeEvent(input$ag_list_delete_click, {
      agency_to_delete <- input$ag_list_delete_click$id
      current_data <- ssfs()

      # Block deletion if any route references this agency
      if (
        nrow(current_data$routes) > 0 &&
          agency_to_delete %in% current_data$routes$agency_id
      ) {
        showNotification(
          paste0(
            "Cannot delete agency '",
            agency_to_delete,
            "'. It is referenced by one or more routes. ",
            "Delete or reassign the routes first."
          ),
          type = "error",
          duration = 5
        )
        return()
      }

      current_data$agency <- current_data$agency[
        current_data$agency$agency_id != agency_to_delete,
      ]
      ssfs(current_data)

      # If we were editing the deleted agency, clear state
      if (!is.null(ag_editing_id()) && ag_editing_id() == agency_to_delete) {
        ag_editing_id(NULL)
      }

      showNotification("Agency deleted successfully", type = "message")
    })

    # Agency map initialization
    output$agency_map <- leaflet::renderLeaflet({
      center <- map_center()
      leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE)) |>
        leaflet::addProviderTiles("CartoDB.Positron", group = "Positron") |>
        leaflet::setView(lng = center$lng, lat = center$lat, zoom = 10)
    })

    # Agency map bounding box observer
    observe({
      req(input$agency_map_bounds) # this ensures the quick display of the bounding box
      current_data <- ssfs()
      center <- map_center()

      # Determine bounding box
      if (
        nrow(current_data$stops) > 0 &&
          !all(sf::st_is_empty(current_data$stops$geometry))
      ) {
        # Use stops bounding box
        bbox <- st_bbox(current_data$stops)
        bbox_coords <- list(
          lng1 = bbox[["xmin"]],
          lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]],
          lat2 = bbox[["ymax"]]
        )
      } else {
        # Default 30km wide x 15km high box around center
        # At ~45° latitude: 1° lng is approx 78.7km, 1° lat is approx 111km
        lng_offset <- 15 / 78.7 # half of 30km in degrees longitude
        lat_offset <- 7.5 / 111 # half of 15km in degrees latitude
        bbox_coords <- list(
          lng1 = center$lng - lng_offset,
          lat1 = center$lat - lat_offset,
          lng2 = center$lng + lng_offset,
          lat2 = center$lat + lat_offset
        )
      }

      # Build bounding box polygon coordinates (closed ring)
      bb_lngs <- c(
        bbox_coords$lng1,
        bbox_coords$lng2,
        bbox_coords$lng2,
        bbox_coords$lng1,
        bbox_coords$lng1
      )
      bb_lats <- c(
        bbox_coords$lat1,
        bbox_coords$lat1,
        bbox_coords$lat2,
        bbox_coords$lat2,
        bbox_coords$lat1
      )

      # Update map
      proxy <- leaflet::leafletProxy("agency_map") |>
        leaflet::clearShapes() |>
        leaflet::clearMarkers() |>
        leaflet::addPolygons(
          lng = bb_lngs,
          lat = bb_lats,
          color = "#000000",
          weight = 2,
          fillOpacity = 0.05,
          group = "bbox"
        ) |>
        leaflet::fitBounds(
          lng1 = bbox_coords$lng1,
          lat1 = bbox_coords$lat1,
          lng2 = bbox_coords$lng2,
          lat2 = bbox_coords$lat2
        )
    })

    #   #   #
    #
    ##   STOPS MODULE---------
    #
    #   #   #

    stopsServer(
      "stops",
      ssfs,
      map_center,
      current_zoom,
      reactive(input$settings_min_stop_dist),
      reactive(input$settings_osm_provider)
    )

    #   #   #
    #
    ##   ROUTES MODULE--------
    #
    #   #   #

    routesServer(
      "routes",
      ssfs,
      map_center,
      current_zoom,
      reactive(input$settings_routing_server)
    )

    #   #   #
    #
    ## SCHEDULE MODULE---------
    #
    #   #   #

    scheduleServer("schedule", ssfs, map_center)

    ###
    #
    ## EXPORT MODULE--------
    #
    ###

    # Handle ssfs download
    output$download_ssfs <- downloadHandler(
      filename = function() {
        if (!grepl("\\.rds$", input$exportssfs_filename)) {
          paste0(input$exportssfs_filename, ".rds")
        } else {
          input$exportssfs_filename
        }
      },
      content = function(file) {
        current_ssfs <- ssfs()

        current_ssfs$itin <-
          current_ssfs$itin |>
          mutate(direction_id = as.integer(direction_id))

        current_ssfs$routes <-
          current_ssfs$routes |>
          mutate(route_type = as.integer(route_type))

        current_ssfs$stop_seq <-
          current_ssfs$stop_seq |>
          select(-stop_name)

        unique_stop_ids <-
          current_ssfs$stop_seq$stop_id |> unique()

        current_ssfs$stops <-
          current_ssfs$stops |>
          filter(stop_id %in% unique_stop_ids)

        current_ssfs$calendar <-
          current_ssfs$calendar |>
          mutate(start_date = as.Date(start_date), end_date = as.Date(end_date))

        saveRDS(current_ssfs, file)
      }
    )

    # Handle gtfs download
    output$download_gtfs <- downloadHandler(
      filename = function() {
        if (!grepl("\\.zip$", input$exportgtfs_filename)) {
          paste0(input$exportgtfs_filename, ".zip")
        } else {
          input$exportgtfs_filename
        }
      },
      content = function(file) {
        current_ssfs <- ssfs()

        current_ssfs$itin <-
          current_ssfs$itin |>
          mutate(direction_id = as.integer(direction_id))

        current_ssfs$routes <-
          current_ssfs$routes |>
          mutate(route_type = as.integer(route_type))

        current_ssfs$stop_seq <-
          current_ssfs$stop_seq |>
          select(-stop_name)

        unique_stop_ids <-
          current_ssfs$stop_seq$stop_id |> unique()

        current_ssfs$stops <-
          current_ssfs$stops |>
          filter(stop_id %in% unique_stop_ids)

        current_ssfs$calendar <-
          current_ssfs$calendar |>
          mutate(start_date = as.Date(start_date), end_date = as.Date(end_date))

        current_gtfs <- croquis::ssfs_to_gtfs(
          current_ssfs,
          dist_traveled = input$include_dist_traveled
        )

        # Add feed info details that are specified in the Settings tab.
        current_gtfs$feed_info <- data.table(
          feed_publisher_name = input$fi_feed_publisher_name,
          feed_publisher_url = input$fi_feed_publisher_url,
          feed_lang = input$fi_feed_lang,
          feed_start_date = min(current_ssfs$calendar$start_date),
          feed_end_date = max(current_ssfs$calendar$end_date),
          feed_version = input$fi_feed_version,
          feed_contact_email = input$fi_feed_contact_email
        )

        gtfstools::write_gtfs(current_gtfs, file)
      }
    )
  }

  #APP----------------------------

  #run the app
  shinyApp(ui = ui, server = server)
}
