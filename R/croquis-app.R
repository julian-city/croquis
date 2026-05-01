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
      tags$script(src = "www/js/stops.js"),
      tags$script(src = "www/js/routes.js"),
      tags$script(src = "www/js/itineraries.js"),
      tags$script(src = "www/js/popovers.js"),
      tags$script(src = "www/js/schedule.js")
    ),

    #loading indicator div
    div(id = "loading-content", div(class = "loading-spinner")),

    #Module architecture
    navbarPage(
      title = "Croquis",
      # div for the theme toggle
      header = tagList(
        div(
          style = "position: absolute; right: 10px; top: 10px; z-index: 1000;",
          tags$button(
            id = "theme-toggle",
            onclick = "toggleTheme()",
            class = "btn btn-default btn-sm",
            htmltools::HTML("&#9680;")
          )
        )
      ),

      #home tab
      tabPanel(
        tags$span(icon("house")),
        #unicode house emoji
        fluidPage(
          titlePanel("Home"),

          #Info
          wellPanel(
            h3("About Croquis 0.1"),
            p(
              "Croquis is a transit sketch planning tool. Use it to create and edit transit networks and schedules in GTFS (General Transit Feed Specification) file format"
            ),
            p(htmltools::HTML(
              "This is a shiny app prototype by <a href='https://julian.city' target='_blank'>Julian Villafuerte Diaz</a>. This version was deployed in March 2026."
            )),
            p(
              "You can get started on your transit planning project by uploading an existing GTFS or croquis.rds file. You may also follow the instructions below to create a new network and schedule."
            ),
            p(
              "This project is in active development. Please get in touch with your feedback and ideas for improvement !"
            ),
            p(htmltools::HTML(
              "<a href='https://julian.city' target='_blank'>Get in touch</a>"
            ))
          ),

          # Upload sample transit systems
          wellPanel(
            h3("Load a sample transit network"),
            p(
              "To explore this tool, you can get started by loading a sample network. The Ligne Jaune model is the simplest and will help you familiarize yourself with how Croquis works."
            ),
            actionButton(
              "load_yellowline_ssfs",
              "STM Ligne Jaune",
              class = "btn-success"
            ),
            actionButton("load_metro_ssfs", "STM Metro", class = "btn-success"),
            actionButton(
              "load_mileend_ssfs",
              "STM Mile-End bus network",
              class = "btn-success"
            ),
            actionButton(
              "load_ttcsubway_ssfs",
              "TTC Subway",
              class = "btn-success"
            )
          ),

          # ssfs upload section
          wellPanel(
            h3("Load your croquis"),
            p(
              "To continue working on a previous croquis, upload your .rds file:"
            ),
            fileInput(
              "load_ssfs",
              "",
              multiple = FALSE,
              accept = ".rds",
              placeholder = "Drag and drop or click to select file"
            ),
            tags$small(
              "Upload a transit model .rds file previously created with Croquis"
            )
          ),

          # ssfs upload section
          wellPanel(
            h3("Load a GTFS"),
            p(
              "You can load an existing GTFS here. Larger files may take several minutes (maximum size: 100MB)."
            ),
            fileInput(
              "load_gtfs",
              "",
              multiple = FALSE,
              accept = ".zip",
              placeholder = "Drag and drop or click to select file"
            ),
            tags$small(
              "Uploading a gtfs here will convert it to an editable format in Croquis"
            )
          ),

          # Instructions
          wellPanel(
            h3("Instructions"),
            p("Build your transit system model by following these steps:"),

            h4("1. Specify the agencies and region of your network"),
            p(
              "In the 'agencies' module:",
              tags$ul(
                tags$li("Add agencies to the agency table"),
                tags$li("Edit existing agencies in the table"),
                tags$li(
                  "Set the location of your transit network if starting from scratch"
                )
              )
            ),

            h4("2. Create the stops of your transit system"),
            p(
              "In the 'stops' module:",
              tags$ul(
                tags$li("Click on the map to add stops"),
                tags$li("Provide unique stop IDs and stop names for each stop"),
                tags$li("Edit the location and details for existing stops"),
                tags$li(
                  "Limitation : for now, it is not possible to delete stops once they have been created"
                )
              )
            ),

            h4("3. Create your routes and route itineraries"),
            p(
              "In the 'routes' module:",
              tags$ul(
                tags$li(
                  "Create routes with their details (mode, colours) and define route itineraries within each route."
                ),
                tags$li(
                  "A route itinerary corresponds to a unique stop pattern for trips (e.g. a transit line will have an itinerary for each direction). Each itinerary is associated with a stop sequence and a shape."
                ),
                tags$li(
                  "Create and edit route geometries by selecting stops in the desired order and by creating waypoints by clicking on the map and along the route. You may delete waypoints or remove stops from a route itinerary by right-clicking."
                ),
                tags$li(
                  "Move a waypoint by clicking on it and activating editing mode. Click on the desired location on the map or on a stop to move the waypoint there. If clicked on a stop, it will be added to the sequence."
                ),
                tags$li(
                  "Toggle between network and simple drawing modes. Network drawing mode calculates the path along the Open Street Maps road network between stops and waypoints."
                )
              )
            ),

            h4("4. Define service calendar"),
            p(
              "In the 'calendar' module:",
              tags$ul(
                tags$li(
                  "Specify which days of the week each service operates, as well as the date ranges for each services."
                ),
                tags$li(
                  "The table in this module is identical to the calendar table in gtfs and is passed on directly."
                )
              )
            ),

            h4("5. Configure service spans"),
            p(
              "In the 'spans' module:",
              tags$ul(
                tags$li(
                  "Define operating hours for each route / service combination."
                )
              )
            ),

            h4("6. Define headway presets"),
            p(
              "In the 'headway presets' module:",
              tags$ul(
                tags$li(
                  "Create and edit predefined schedule schemes, including all-day frequent service or peak-frequent service."
                ),
                tags$li(
                  "You can use these presets to set frequencies on routes in the next module quicker."
                )
              )
            ),

            h4("7. Specify headways and speeds by hour"),
            p(
              "In 'headways' module:",
              tags$ul(
                tags$li(
                  "After configuring service spans, initialize the headways and speeds by hour table in this module, then edit details by route itinerary, calendar service, and hour."
                )
              )
            ),

            h4("8. Modify interstop speeds"),
            p(
              "In 'speed profiles' module:",
              tags$ul(
                tags$li(
                  "Set the speed factors by stop that will be used to adjust interstop speeds."
                ),
                tags$li(
                  "View interstop speeds by hour and by service"
                )
              )
            ),

            h4(
              "9. When finished, use the 'export' module to create a GTFS or save your croquis in .rds format"
            )
          )
        )
      ),

      tabPanel(
        "agency",
        fluidPage(
          titlePanel("agency"),

          # Agency map - top 30% of page
          div(
            style = "width: 100%; height: 30vh; min-height: 200px; margin-bottom: 15px; border: 1px solid var(--border-color); border-radius: 4px; overflow: hidden;",
            leaflet::leafletOutput(
              "agency_map",
              height = "100%",
              width = "100%"
            )
          ),

          # City selector and agency form side by side below the map
          fluidRow(
            # Left column: City selector
            column(
              4,
              wellPanel(
                h4("Project Location"),
                p("Set the map center and timezone:"),
                div(
                  style = "position: relative;",
                  textInput(
                    "city_search",
                    "Search for a city",
                    placeholder = "Type city name..."
                  ),
                  div(
                    id = "city_suggestions",
                    style = "position: absolute; z-index: 1000; background: var(--panel-bg); 
               border: 1px solid var(--border-color); color: var(--text-color);
               max-height: 200px; overflow-y: auto; width: 100%; display: none;"
                  )
                ),
                actionButton("select_city", "Select City", class = "btn-info"),
                tags$br(),
                tags$small("Updates the map center and fetches timezone")
              )
            ),
            # Middle column: Agency form
            column(
              4,
              wellPanel(
                h4("Agency Details"),
                textInput(
                  "ag_agency_id",
                  label = tagList(
                    "Agency ID",
                    info_popover(
                      "Identifies a unique transit agency or transit brand.",
                      "https://gtfs.org/schedule/reference/#agencytxt"
                    )
                  ),
                  placeholder = "e.g., STM"
                ),
                textInput(
                  "ag_agency_name",
                  label = tagList(
                    "Agency name",
                    info_popover(
                      "Full name of the transit agency.",
                      "https://gtfs.org/schedule/reference/#agencytxt"
                    )
                  ),
                  placeholder = "e.g., Soci\u00e9t\u00e9 de transport de Montr\u00e9al"
                ),
                textInput(
                  "ag_agency_url",
                  label = tagList(
                    "Agency URL",
                    info_popover(
                      "URL of the transit agency.",
                      "https://gtfs.org/schedule/reference/#agencytxt"
                    )
                  ),
                  placeholder = "e.g., http://www.stm.info"
                ),
                textInput(
                  "ag_agency_timezone",
                  label = tagList(
                    "Agency timezone",
                    info_popover(
                      "Timezone where the transit agency is located in IANA timezone database (tz database) format.",
                      "https://gtfs.org/schedule/reference/#agencytxt"
                    )
                  ),
                  placeholder = "e.g., America/Montreal"
                ),
                hr(),
                conditionalPanel(
                  condition = "output.editing_agency == true",
                  actionButton(
                    "save_agency_edit",
                    "Save changes",
                    class = "btn-success"
                  ),
                  actionButton(
                    "cancel_agency_edit",
                    "Cancel",
                    class = "btn-warning"
                  )
                ),
                conditionalPanel(
                  condition = "output.editing_agency == false",
                  actionButton(
                    "edit_agency_row",
                    "Edit selected row",
                    class = "btn-info"
                  ),
                  actionButton(
                    "add_agency",
                    "Add new agency",
                    class = "btn-success"
                  ),
                  actionButton(
                    "clear_agency_form",
                    "Clear form",
                    class = "btn-warning"
                  )
                ),
                hr(),
                actionButton(
                  "delete_selected_agency",
                  "Delete selected agency",
                  class = "btn-danger"
                )
              )
            ),
            # Right column: Agency table
            column(
              4,
              wellPanel(
                h4("Agencies"),
                DT::DTOutput("agency_table")
              )
            )
          )
        )
      ),

      #stops tab
      stopsUI("stops"),

      #routes tab (consolidated routes + itineraries)
      routesUI("routes"),

      #speed profiles tab
      tabPanel(
        "speed profiles",
        fluidPage(
          titlePanel("speed profiles"),
          sidebarLayout(
            sidebarPanel(
              width = 3,

              # Edit section
              h4("Edit speed factors for:"),
              selectInput("sp_itin_id", "Route itinerary", choices = NULL),
              hr(),

              #View section
              tags$div(
                style = "background-color: var(--hover-color); padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                tags$strong("View speeds for:"),
                tags$p(
                  style = "font-size: 0.85em; color: gray; margin-top: 4px; margin-bottom: 8px;",
                  "Changing service or hour only changes the displayed speeds (km/h). ",
                  "Speed factors are defined once per itinerary and apply to all services and hours."
                ),
                selectInput("sp_service_id", "Service ID", choices = NULL),
                selectInput("sp_hour", "Hour", choices = NULL)
              ),

              actionButton("load_sp", "Load", class = "btn-info"),
              hr(),
              textOutput("sp_average_display"),
              hr(),
              actionButton(
                "save_sp",
                "Save speed factors",
                class = "btn-success"
              ),
              actionButton(
                "reset_sp",
                "Reset all to 1.0",
                style = "background-color: #F4A582; color: white;"
              )
            ),
            mainPanel(
              plotly::plotlyOutput("sp_speed_plot", height = "350px"),
              hr(),
              h4("Adjust Speed Factors"),
              uiOutput("sp_table_ui")
            )
          )
        )
      ),

      # Schedule tab
      scheduleUI("schedule"),

      #export tab
      tabPanel(
        "export",
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
            h3("Export raw project file (ssfs)"),
            p(
              "If you want to save your project and continue working later, export .rds file:"
            ),
            textInput(
              "exportssfs_filename",
              "Filename:",
              value = "croquis.rds"
            ),
            downloadButton(
              "download_ssfs",
              "Download Transit System",
              class = "btn-primary"
            ),
            tags$br(),
            tags$br(),
            tags$small(
              "Your transit system will be saved as an .rds file that you can reload later"
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
              value = "contact@julian.city"
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
              selected = "Valhalla"
            ),
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

    ssfs <- reactiveVal(
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
          calendar = data.frame(
            service_id = character(),
            monday = integer(),
            tuesday = integer(),
            wednesday = integer(),
            thursday = integer(),
            friday = integer(),
            saturday = integer(),
            sunday = integer(),
            start_date = character(),
            end_date = character()
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

    # Filtered cities for autocomplete
    filtered_cities <- reactiveVal(data.frame())

    #current zoom reactive value
    current_zoom <- reactiveVal(10)

    # Function to update any map with current ssfs data
    # TODO: Remove this?
    updateMapWithSsfsData <- function(
      map_id,
      current_data,
      highlight_ids = NULL,
      show_stops = TRUE,
      show_shapes = TRUE
    ) {
      proxy <- leaflet::leafletProxy(map_id)

      # Clear all existing content
      proxy |>
        leaflet::clearGroup("shapes") |>
        leaflet::clearGroup("stops") |>
        leaflet::clearMarkers() # For backward compatibility

      # Add shapes first (as bottom layer)
      if (
        show_shapes &&
          !is.null(current_data$itin) &&
          nrow(current_data$itin) > 0
      ) {
        for (i in seq_len(nrow(current_data$itin))) {
          line_coords <- st_coordinates(current_data$itin$geometry[i])
          proxy <- proxy |>
            leaflet::addPolylines(
              lng = line_coords[, 1],
              lat = line_coords[, 2],
              group = "shapes",
              color = "#05AEEF",
              weight = 2,
              opacity = 0.6
            )
        }
      }

      # Add stops (on top of shapes)
      if (
        show_stops &&
          !is.null(current_data$stops) &&
          nrow(current_data$stops) > 0
      ) {
        # Calculate marker size based on current zoom
        marker_size <- calculateMarkerSize(current_zoom())

        # Determine colors based on highlight IDs if provided
        fill_colors <- if (!is.null(highlight_ids)) {
          ifelse(
            current_data$stops$stop_id %in% highlight_ids,
            "#B2182B",
            "#7f7f7f"
          )
        } else {
          "#7f7f7f"
        }

        proxy <- proxy |>
          leaflet::addCircleMarkers(
            data = current_data$stops,
            radius = marker_size,
            color = "white",
            weight = 1,
            stroke = TRUE,
            fillColor = fill_colors,
            fillOpacity = 0.7,
            layerId = ~stop_id,
            popup = ~ paste("ID:", stop_id, "<br>Name:", stop_name),
            group = "stops"
          )
      }

      proxy
    }

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
            routing_server = input$settings_routing_server
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

    # City search autocomplete
    observeEvent(input$city_search, {
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

        # Fill the agency timezone text input with the selected city's timezone
        updateTextInput(session, "ag_agency_timezone", value = selected_city$tz)

        # Hide suggestions
        session$sendCustomMessage("hideSuggestions", "")

        showNotification(
          paste("City set to:", selected_city$name),
          type = "message"
        )
      }
    })

    #   #   #
    #
    ##   AGENCY MODULE-------
    #
    #   #   #

    # Editing state for agency form
    editing_agency <- reactiveVal(FALSE)

    # Expose editing state to conditionalPanel
    output$editing_agency <- reactive({
      editing_agency()
    })
    shiny::outputOptions(output, "editing_agency", suspendWhenHidden = FALSE)

    # Render agency table
    output$agency_table <- DT::renderDT({
      current_data <- ssfs()

      if (nrow(current_data$agency) == 0) {
        return(DT::datatable(
          data.frame(
            agency_id = character(),
            agency_name = character(),
            agency_url = character(),
            agency_timezone = character()
          ),
          selection = "single",
          rownames = FALSE,
          options = list(
            pageLength = 10,
            ordering = FALSE,
            dom = "t"
          )
        ))
      }

      DT::datatable(
        current_data$agency,
        selection = "single",
        rownames = FALSE,
        options = list(
          pageLength = 10,
          ordering = FALSE,
          dom = "t"
        ),
        colnames = c(
          "Agency ID",
          "Agency Name",
          "Agency URL",
          "Agency Timezone"
        )
      )
    })

    # Clear agency form
    observeEvent(input$clear_agency_form, {
      updateTextInput(session, "ag_agency_id", value = "")
      updateTextInput(session, "ag_agency_name", value = "")
      updateTextInput(session, "ag_agency_url", value = "")
      updateTextInput(session, "ag_agency_timezone", value = "")
      editing_agency(FALSE)
    })

    # Edit selected agency row - populate form
    observeEvent(input$edit_agency_row, {
      req(input$agency_table_rows_selected)
      current_data <- ssfs()

      selected_row <- current_data$agency[input$agency_table_rows_selected, ]

      updateTextInput(session, "ag_agency_id", value = selected_row$agency_id)
      updateTextInput(
        session,
        "ag_agency_name",
        value = selected_row$agency_name
      )
      updateTextInput(session, "ag_agency_url", value = selected_row$agency_url)
      updateTextInput(
        session,
        "ag_agency_timezone",
        value = selected_row$agency_timezone
      )

      editing_agency(TRUE)
    })

    # Cancel agency edit
    observeEvent(input$cancel_agency_edit, {
      updateTextInput(session, "ag_agency_id", value = "")
      updateTextInput(session, "ag_agency_name", value = "")
      updateTextInput(session, "ag_agency_url", value = "")
      updateTextInput(session, "ag_agency_timezone", value = "")
      editing_agency(FALSE)
    })

    # Save agency edit (update existing row)
    observeEvent(input$save_agency_edit, {
      req(input$agency_table_rows_selected)
      req(input$ag_agency_id)

      current_data <- ssfs()
      selected_idx <- input$agency_table_rows_selected
      old_agency_id <- current_data$agency$agency_id[selected_idx]
      new_agency_id <- trimws(input$ag_agency_id)

      # Validate non-empty
      if (nchar(new_agency_id) == 0) {
        showNotification("Agency ID cannot be empty.", type = "warning")
        return()
      }

      # If agency_id is being changed, check for conflict
      if (new_agency_id != old_agency_id) {
        other_agency_ids <- current_data$agency$agency_id[-selected_idx]
        if (new_agency_id %in% other_agency_ids) {
          showNotification(
            "This agency ID already exists. Please use a different ID.",
            type = "warning"
          )
          return()
        }
      }

      # Update the row
      current_data$agency$agency_id[selected_idx] <- new_agency_id
      current_data$agency$agency_name[selected_idx] <- trimws(
        input$ag_agency_name
      )
      current_data$agency$agency_url[selected_idx] <- trimws(
        input$ag_agency_url
      )
      current_data$agency$agency_timezone[selected_idx] <- trimws(
        input$ag_agency_timezone
      )

      # If agency_id was changed, update references in routes table
      if (new_agency_id != old_agency_id && nrow(current_data$routes) > 0) {
        current_data$routes$agency_id[
          current_data$routes$agency_id == old_agency_id
        ] <- new_agency_id
      }

      ssfs(current_data)

      # Clear form and exit edit mode
      updateTextInput(session, "ag_agency_id", value = "")
      updateTextInput(session, "ag_agency_name", value = "")
      updateTextInput(session, "ag_agency_url", value = "")
      updateTextInput(session, "ag_agency_timezone", value = "")
      editing_agency(FALSE)

      showNotification("Agency updated successfully", type = "message")
    })

    # Add new agency
    observeEvent(input$add_agency, {
      req(input$ag_agency_id)

      current_data <- ssfs()
      new_agency_id <- trimws(input$ag_agency_id)

      # Validate non-empty
      if (nchar(new_agency_id) == 0) {
        showNotification("Agency ID cannot be empty.", type = "warning")
        return()
      }

      # Check if agency_id already exists
      if (new_agency_id %in% current_data$agency$agency_id) {
        showNotification(
          "This agency ID already exists. Please use a different ID.",
          type = "warning"
        )
        return()
      }

      new_agency <- data.frame(
        agency_id = new_agency_id,
        agency_name = trimws(input$ag_agency_name),
        agency_url = trimws(input$ag_agency_url),
        agency_timezone = trimws(input$ag_agency_timezone),
        stringsAsFactors = FALSE
      )

      current_data$agency <- rbind(current_data$agency, new_agency)
      ssfs(current_data)

      # Clear form
      updateTextInput(session, "ag_agency_id", value = "")
      updateTextInput(session, "ag_agency_name", value = "")
      updateTextInput(session, "ag_agency_url", value = "")
      updateTextInput(session, "ag_agency_timezone", value = "")

      showNotification("Agency added successfully", type = "message")
    })

    # Delete selected agency (with protection)
    observeEvent(input$delete_selected_agency, {
      req(input$agency_table_rows_selected)
      current_data <- ssfs()

      agency_to_delete <- current_data$agency$agency_id[
        input$agency_table_rows_selected
      ]

      # Check if any route references this agency
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
        -input$agency_table_rows_selected,
      ]
      ssfs(current_data)

      # Exit edit mode if active
      editing_agency(FALSE)

      showNotification("Agency deleted successfully", type = "message")
    })

    # Agency map initialization
    output$agency_map <- leaflet::renderLeaflet({
      center <- map_center()
      leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE)) |>
        addBaseMaps() |>
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

    stopsServer("stops", ssfs, map_center, current_zoom)

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

    ###
    #
    ## SPEED PROFILES MODULE----------
    #
    ###

    # Reactive value to hold the working speed factors (excludes last stop)
    sp_speed_factors <- reactiveVal(NULL)
    # Reactive value to hold the loaded stop_seq subset (excludes last stop)
    sp_stop_data <- reactiveVal(NULL)
    # Reactive value to track the base speed from hsh for current selection
    sp_base_speed <- reactiveVal(20)

    # Update itin_id choices (preserve current selection)
    observe({
      current_data <- ssfs()
      itin_choices <- if (nrow(current_data$itin) > 0) {
        ids <- current_data$itin$itin_id
        headsigns <- current_data$itin$trip_headsign
        labels <- ifelse(
          !is.na(headsigns) & nchar(headsigns) > 0,
          paste0(ids, " (", headsigns, ")"),
          ids
        )
        setNames(ids, labels)
      } else {
        character(0)
      }
      current_sel <- isolate(input$sp_itin_id)
      selected <- if (!is.null(current_sel) && current_sel %in% itin_choices) {
        current_sel
      } else {
        ""
      }
      updateSelectInput(
        session,
        "sp_itin_id",
        choices = c("", itin_choices),
        selected = selected
      )
    })

    # Update service_id and hour choices based on selected itin_id (from hsh entries)
    # Performs full cascade: service_id -> hour -> auto-load speed profile
    observeEvent(
      input$sp_itin_id,
      {
        req(input$sp_itin_id != "")
        current_data <- ssfs()

        service_choices <- current_data$hsh |>
          filter(itin_id == input$sp_itin_id) |>
          pull(service_id) |>
          unique()

        if (length(service_choices) == 0) {
          updateSelectInput(session, "sp_service_id", choices = character(0))
          updateSelectInput(session, "sp_hour", choices = NULL)
          sp_speed_factors(NULL)
          sp_stop_data(NULL)
          return()
        }

        first_service <- service_choices[1]
        updateSelectInput(
          session,
          "sp_service_id",
          choices = service_choices,
          selected = first_service
        )

        # Directly compute hour choices for first service
        hour_choices <- current_data$hsh |>
          filter(itin_id == input$sp_itin_id, service_id == first_service) |>
          arrange(hour_dep) |>
          pull(hour_dep) |>
          unique()

        if (length(hour_choices) == 0) {
          updateSelectInput(session, "sp_hour", choices = NULL)
          sp_speed_factors(NULL)
          sp_stop_data(NULL)
          return()
        }

        first_hour <- hour_choices[1]
        updateSelectInput(
          session,
          "sp_hour",
          choices = hour_choices,
          selected = first_hour
        )

        # Directly load speed profile
        stop_data <- current_data$stop_seq |>
          filter(itin_id == input$sp_itin_id) |>
          arrange(stop_sequence)

        if (nrow(stop_data) >= 2) {
          stop_data <- stop_data[-nrow(stop_data), ]

          base_speed <- current_data$hsh |>
            filter(
              itin_id == input$sp_itin_id,
              service_id == first_service,
              hour_dep == first_hour
            ) |>
            pull(speed)

          if (length(base_speed) > 0) {
            sp_base_speed(base_speed[1])
            sp_stop_data(stop_data)
            sp_speed_factors(stop_data$speed_factor)
          }
        }
      },
      ignoreInit = TRUE
    )

    # Update hour choices based on selected itin_id + service_id (from hsh entries)
    # Auto-selects first hour and loads speed profile
    observeEvent(
      input$sp_service_id,
      {
        req(input$sp_itin_id != "", input$sp_service_id != "")
        current_data <- ssfs()

        hour_choices <- current_data$hsh |>
          filter(
            itin_id == input$sp_itin_id,
            service_id == input$sp_service_id
          ) |>
          arrange(hour_dep) |>
          pull(hour_dep) |>
          unique()

        if (length(hour_choices) > 0) {
          updateSelectInput(
            session,
            "sp_hour",
            choices = hour_choices,
            selected = hour_choices[1]
          )

          # Auto-load speed profile with first hour
          stop_data <- current_data$stop_seq |>
            filter(itin_id == input$sp_itin_id) |>
            arrange(stop_sequence)

          if (nrow(stop_data) >= 2) {
            # Exclude last stop
            stop_data <- stop_data[-nrow(stop_data), ]

            base_speed <- current_data$hsh |>
              filter(
                itin_id == input$sp_itin_id,
                service_id == input$sp_service_id,
                hour_dep == hour_choices[1]
              ) |>
              pull(speed)

            if (length(base_speed) > 0) {
              sp_base_speed(base_speed[1])
              sp_stop_data(stop_data)
              sp_speed_factors(stop_data$speed_factor)
            }
          }
        } else {
          updateSelectInput(session, "sp_hour", choices = NULL)
        }
      },
      ignoreInit = TRUE
    )

    # Load speed profile data
    observeEvent(input$load_sp, {
      req(input$sp_itin_id != "", input$sp_service_id != "", input$sp_hour)
      current_data <- ssfs()

      # Get stop_seq for this itin_id
      stop_data <- current_data$stop_seq |>
        filter(itin_id == input$sp_itin_id) |>
        arrange(stop_sequence)

      if (nrow(stop_data) < 2) {
        showNotification("Not enough stops in this itinerary", type = "warning")
        return()
      }

      # Exclude last stop (speed_factor is always NA for the last stop)
      stop_data <- stop_data[-nrow(stop_data), ]

      # Get the base speed from hsh for the selected hour
      base_speed <- current_data$hsh |>
        filter(
          itin_id == input$sp_itin_id,
          service_id == input$sp_service_id,
          hour_dep == input$sp_hour
        ) |>
        pull(speed)

      if (length(base_speed) == 0) {
        showNotification(
          "No speed data found for this combination",
          type = "error"
        )
        return()
      }

      sp_base_speed(base_speed[1])
      sp_stop_data(stop_data)
      sp_speed_factors(stop_data$speed_factor)

      showNotification("Speed profile loaded", type = "message")
    })

    # Render the plotly chart
    output$sp_speed_plot <- plotly::renderPlotly({
      req(sp_speed_factors(), sp_stop_data())

      stop_data <- sp_stop_data()
      sf_values <- sp_speed_factors()
      base_speed <- sp_base_speed()

      # Calculate actual speed = speed_factor * base_speed, rounded to 1 decimal
      actual_speeds <- round(sf_values * base_speed, 1)

      # Use stop_name for hover label, fall back to stop_id
      stop_labels <- if (
        "stop_name" %in% names(stop_data) && !all(is.na(stop_data$stop_name))
      ) {
        stop_data$stop_name
      } else {
        stop_data$stop_id
      }

      plot_data <- data.frame(
        stop_seq = stop_data$stop_sequence,
        speed = actual_speeds,
        speed_factor = sf_values,
        stop_name = stop_labels
      )

      plotly::plot_ly(
        plot_data,
        x = ~stop_seq,
        y = ~speed,
        text = ~ paste0(
          "Stop: ",
          stop_name,
          " (seq ",
          stop_seq,
          ")",
          "\nSpeed: ",
          speed,
          " km/h\nFactor: ",
          speed_factor
        ),
        hoverinfo = "text",
        type = "scatter",
        mode = "lines+markers",
        marker = list(size = 12, color = "#124559"),
        line = list(color = "#124559", width = 2)
      ) |>
        plotly::layout(
          title = paste0(
            "Speed profile (base: ",
            base_speed,
            " km/h, ",
            input$sp_service_id,
            " @ ",
            input$sp_hour,
            ")"
          ),
          xaxis = list(
            title = "Stop sequence",
            fixedrange = TRUE,
            dtick = 1,
            range = c(
              min(plot_data$stop_seq) - 0.5,
              max(plot_data$stop_seq) + 0.5
            )
          ),
          yaxis = list(
            title = "Speed (km/h)",
            range = c(0, max(actual_speeds) * 1.3),
            fixedrange = TRUE
          )
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # Display average speed factor
    output$sp_average_display <- renderText({
      req(sp_speed_factors())
      avg <- mean(sp_speed_factors())
      paste("Average speed factor:", sprintf("%.2f", avg))
    })

    # Render the editable table with up/down buttons
    output$sp_table_ui <- renderUI({
      req(sp_speed_factors(), sp_stop_data())

      stop_data <- sp_stop_data()
      sf_values <- sp_speed_factors()
      n <- length(sf_values)

      tags$table(
        class = "table",
        style = "width: 100%;",
        tags$thead(
          tags$tr(
            tags$th("From stop", style = "width: 30%;"),
            tags$th("Sequence", style = "width: 10%; text-align: center;"),
            tags$th("Speed Factor", style = "width: 20%; text-align: center;"),
            tags$th("Speed (km/h)", style = "width: 20%; text-align: center;"),
            tags$th("Adjust", style = "width: 20%;")
          )
        ),
        tags$tbody(
          lapply(1:n, function(i) {
            label <- if (
              "stop_name" %in%
                names(stop_data) &&
                !is.na(stop_data$stop_name[i])
            ) {
              stop_data$stop_name[i]
            } else {
              stop_data$stop_id[i]
            }
            tags$tr(
              tags$td(label),
              tags$td(
                stop_data$stop_sequence[i],
                style = "text-align: center;"
              ),
              tags$td(
                textOutput(paste0("sp_sf_", i), inline = TRUE),
                style = "text-align: center;"
              ),
              tags$td(
                textOutput(paste0("sp_spd_", i), inline = TRUE),
                style = "text-align: center;"
              ),
              tags$td(
                actionButton(
                  paste0("sp_down_", i),
                  "\u2193",
                  style = "padding: 2px 12px; margin-right: 5px;"
                ),
                actionButton(
                  paste0("sp_up_", i),
                  "\u2191",
                  style = "padding: 2px 12px;"
                )
              )
            )
          })
        )
      )
    })

    # Track how many text outputs have been created
    sp_text_outputs_created <- reactiveVal(0L)

    # Create text outputs for speed factor and speed values (only for new indices)
    observe({
      req(sp_speed_factors(), sp_stop_data())

      n <- length(sp_speed_factors())
      already_created <- isolate(sp_text_outputs_created())

      if (n > already_created) {
        lapply((already_created + 1):n, function(i) {
          output[[paste0("sp_sf_", i)]] <- renderText({
            sf <- sp_speed_factors()
            if (length(sf) >= i) sprintf("%.1f", sf[i]) else ""
          })
          output[[paste0("sp_spd_", i)]] <- renderText({
            sf <- sp_speed_factors()
            if (length(sf) >= i) {
              sprintf("%.1f", round(sf[i] * sp_base_speed(), 1))
            } else {
              ""
            }
          })
        })
        sp_text_outputs_created(n)
      }
    })

    # Helper function for normalization (from prototype)
    sp_normalize_if_needed <- function(values, threshold = 0.1) {
      avg <- mean(values)
      if (abs(avg - 1.0) > threshold) {
        normalized <- values / avg
        return(round(normalized, 1))
      }
      round(values, 1)
    }

    # Track how many button observers have been created to avoid duplicates
    sp_observers_created <- reactiveVal(0L)

    # Create up/down button observers only for NEW indices (never re-create)
    observe({
      req(sp_speed_factors(), sp_stop_data())

      n <- length(sp_speed_factors())
      already_created <- isolate(sp_observers_created())

      if (n > already_created) {
        lapply((already_created + 1):n, function(i) {
          observeEvent(
            input[[paste0("sp_up_", i)]],
            {
              current <- sp_speed_factors()
              if (length(current) >= i) {
                current[i] <- min(2.5, current[i] + 0.1)
                current[i] <- round(current[i], 1)
                sp_speed_factors(sp_normalize_if_needed(current))
              }
            },
            ignoreInit = TRUE
          )

          observeEvent(
            input[[paste0("sp_down_", i)]],
            {
              current <- sp_speed_factors()
              if (length(current) >= i) {
                current[i] <- max(0.1, current[i] - 0.1)
                current[i] <- round(current[i], 1)
                sp_speed_factors(sp_normalize_if_needed(current))
              }
            },
            ignoreInit = TRUE
          )
        })
        sp_observers_created(n)
      }
    })

    # Save speed factors back to ssfs
    observeEvent(input$save_sp, {
      req(sp_speed_factors(), sp_stop_data(), input$sp_itin_id != "")

      current_data <- ssfs()
      stop_data <- sp_stop_data()
      sf_values <- sp_speed_factors()

      # Update speed_factor in stop_seq for the matching itin_id rows (excluding last stop)
      for (i in seq_along(sf_values)) {
        match_idx <- which(
          current_data$stop_seq$itin_id == input$sp_itin_id &
            current_data$stop_seq$stop_sequence == stop_data$stop_sequence[i]
        )
        if (length(match_idx) == 1) {
          current_data$stop_seq$speed_factor[match_idx] <- sf_values[i]
        }
      }

      ssfs(current_data)
      showNotification("Speed factors saved", type = "message")
    })

    # Reset all speed factors to 1.0
    observeEvent(input$reset_sp, {
      req(sp_speed_factors())
      n <- length(sp_speed_factors())
      sp_speed_factors(rep(1.0, n))
    })

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
