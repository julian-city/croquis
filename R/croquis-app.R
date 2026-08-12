#' Croquis: transit sketch planning Shiny app
#'
#' Launches the Croquis Shiny app
#'
#' @param ssfs an optional SSFS to load into the app on launch. Defaults to NULL.
#' @param lang UI language code: `"en"` (English, default), `"fr"` (French),
#'   or `"es"` (Spanish).  Sets the initial language for all interface elements.
#'   The language can also be changed mid-session via the selector in the
#'   top-right corner.  Additional languages can be registered in
#'   `SUPPORTED_LANGS` (see `R/i18n.R`).
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
#'
#' #Launch the app in French
#' croquis(lang = "fr")
#' }
croquis <- function(ssfs = NULL, lang = "en") {
  # Validate input ssfs (and change name to avoid name collision in the server)

  input_ssfs <- NULL

  # Validate language parameter
  lang_init <- match.arg(lang, names(SUPPORTED_LANGS))

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
      tags$script(src = "www/js/i18n.js"),
      tags$script(htmltools::HTML(sprintf("croquisLang = '%s';", lang_init))),
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
          do.call(
            tags$select,
            c(
              list(
                id = "lang_select",
                class = "btn btn-default btn-sm",
                style = "padding: 2px 6px; font-size: 12px; cursor: pointer;",
                onchange = "Shiny.setInputValue('app_lang', this.value, {priority: 'event'})"
              ),
              build_lang_options(lang_init)
            )
          ),
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
        value = "home",
        fluidPage(
          tags$h2(span(
            tr("home_title", lang_init),
            `data-i18n` = "home_title"
          )),

          wellPanel(
            style = "font-size: 14px; margin-bottom: 12px; line-height: 1.5; color: var(--text-color);",
            p(
              tags$strong(
                span(
                  tr("intro_tagline", lang_init),
                  `data-i18n` = "intro_tagline"
                )
              ),
              span(
                tr("intro_tabs", lang_init),
                `data-i18n` = "intro_tabs"
              ),
              span(
                tr("intro_get_started", lang_init),
                `data-i18n` = "intro_get_started"
              ),
              span(
                tr("intro_save_pre", lang_init),
                `data-i18n` = "intro_save_pre"
              ),
              icon("floppy-disk", class = "fa-solid"),
              span(
                tr("intro_save_post", lang_init),
                `data-i18n` = "intro_save_post"
              ),
              span(
                tr("intro_report_pre", lang_init),
                `data-i18n` = "intro_report_pre"
              ),
              tags$a(
                span(
                  tr("intro_report_link", lang_init),
                  `data-i18n` = "intro_report_link"
                ),
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
              h4(span(
                tr("load_network", lang_init),
                `data-i18n` = "load_network"
              )),
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
                    h4(span(
                      tr("load_gtfs", lang_init),
                      `data-i18n` = "load_gtfs"
                    )),
                    p(
                      span(
                        tr("load_gtfs_desc", lang_init),
                        `data-i18n` = "load_gtfs_desc"
                      ),
                      tags$br(),
                      tags$small(
                        span(
                          tr("load_gtfs_size", lang_init),
                          `data-i18n` = "load_gtfs_size"
                        )
                      )
                    ),
                    i18n_placeholder(
                      fileInput(
                        "load_gtfs",
                        "",
                        multiple = FALSE,
                        accept = ".zip",
                        buttonLabel = span(
                          tr("btn_browse", lang_init),
                          `data-i18n` = "btn_browse"
                        ),
                        placeholder = tr("file_placeholder", lang_init)
                      ),
                      "file_placeholder"
                    ),
                    tags$small(
                      span(
                        tr("load_gtfs_note", lang_init),
                        `data-i18n` = "load_gtfs_note"
                      )
                    )
                  )
                ),
                column(
                  6,
                  wellPanel(
                    h4(span(
                      tr("load_croquis", lang_init),
                      `data-i18n` = "load_croquis"
                    )),
                    p(
                      span(
                        tr("load_croquis_desc", lang_init),
                        `data-i18n` = "load_croquis_desc"
                      )
                    ),
                    i18n_placeholder(
                      fileInput(
                        "load_ssfs",
                        "",
                        multiple = FALSE,
                        accept = ".rds",
                        buttonLabel = span(
                          tr("btn_browse", lang_init),
                          `data-i18n` = "btn_browse"
                        ),
                        placeholder = tr("file_placeholder", lang_init)
                      ),
                      "file_placeholder"
                    ),
                    tags$small(
                      span(
                        tr("load_croquis_note", lang_init),
                        `data-i18n` = "load_croquis_note"
                      )
                    )
                  )
                )
              ),
              # Bottom row: Sample networks
              wellPanel(
                h4(span(
                  tr("load_sample", lang_init),
                  `data-i18n` = "load_sample"
                )),
                p(
                  span(
                    tr("load_sample_desc", lang_init),
                    `data-i18n` = "load_sample_desc"
                  )
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
                  "STM Mile-End bus",
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
                h4(span(
                  tr("loc_title", lang_init),
                  `data-i18n` = "loc_title"
                )),
                div(
                  style = "display: flex; align-items: flex-end; gap: 8px;",
                  div(
                    style = "position: relative; flex: 1;",
                    i18n_placeholder(
                      textInput(
                        "city_search",
                        tags$label(
                          span(
                            tr("loc_search_label", lang_init),
                            `data-i18n` = "loc_search_label"
                          ),
                          info_popover(
                            tr("pop_city_search", lang_init),
                            key = "pop_city_search",
                            lang = lang_init
                          )
                        ),
                        placeholder = tr("loc_search_ph", lang_init),
                        width = "100%"
                      ),
                      "loc_search_ph"
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
                      span(
                        tr("btn_select_city", lang_init),
                        `data-i18n` = "btn_select_city"
                      ),
                      class = "btn-info"
                    )
                  )
                ),
                tags$small(span(
                  tr("loc_updates_note", lang_init),
                  `data-i18n` = "loc_updates_note"
                )),
                h5(span(
                  tr("loc_manual_title", lang_init),
                  `data-i18n` = "loc_manual_title"
                )),
                fluidRow(
                  column(
                    6,
                    numericInput(
                      "manual_lat",
                      span(
                        tr("lbl_latitude", lang_init),
                        `data-i18n` = "lbl_latitude"
                      ),
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
                      span(
                        tr("lbl_longitude", lang_init),
                        `data-i18n` = "lbl_longitude"
                      ),
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
            h4(span(
              tr("agencies_title", lang_init),
              `data-i18n` = "agencies_title"
            )),
            div(
              class = "agency-list-container",
              uiOutput("agency_list_ui")
            )
          ),

          # -- Instructions --
          wellPanel(
            uiOutput("home_instructions_ui")
          )
        )
      ),

      #stops tab
      stopsUI("stops", lang = lang_init),

      #routes tab
      routesUI("routes", lang = lang_init),

      # Schedule tab
      scheduleUI("schedule", lang = lang_init),

      #export tab
      tabPanel(
        tags$span(icon("floppy-disk", class = "fa-solid")),
        value = "export",
        fluidPage(
          tags$h2(span(
            tr("export_title", lang_init),
            `data-i18n` = "export_title"
          )),

          # Export gtfs
          wellPanel(
            h3(span(
              tr("export_gtfs_title", lang_init),
              `data-i18n` = "export_gtfs_title"
            )),
            textInput(
              "exportgtfs_filename",
              label = span(
                tr("lbl_filename", lang_init),
                `data-i18n` = "lbl_filename"
              ),
              value = "gtfs.zip"
            ),
            checkboxInput(
              "include_dist_traveled",
              label = span(
                tr("export_dist_traveled", lang_init),
                `data-i18n` = "export_dist_traveled"
              ),
              value = FALSE
            ),
            tags$small(span(
              tr("export_dist_desc", lang_init),
              `data-i18n` = "export_dist_desc"
            )),
            tags$br(),
            tags$br(),
            downloadButton(
              "download_gtfs",
              span(
                tr("export_download_gtfs", lang_init),
                `data-i18n` = "export_download_gtfs"
              ),
              class = "btn-primary"
            )
          ),

          # Export raw ssfs
          wellPanel(
            h3(span(
              tr("export_save_title", lang_init),
              `data-i18n` = "export_save_title"
            )),
            p(span(
              tr("export_save_desc", lang_init),
              `data-i18n` = "export_save_desc"
            )),
            textInput(
              "exportssfs_filename",
              label = span(
                tr("lbl_filename", lang_init),
                `data-i18n` = "lbl_filename"
              ),
              value = "croquis.rds"
            ),
            downloadButton(
              "download_ssfs",
              span(
                tr("export_download_croquis", lang_init),
                `data-i18n` = "export_download_croquis"
              ),
              class = "btn-primary"
            ),
            tags$br(),
            tags$br(),
            tags$small(span(
              tr("export_save_note", lang_init),
              `data-i18n` = "export_save_note"
            ))
          )
        )
      ),

      #settings tab
      tabPanel(
        tags$span(icon("gear")),
        value = "settings",
        fluidPage(
          tags$h2(span(
            tr("settings_title", lang_init),
            `data-i18n` = "settings_title"
          )),
          wellPanel(
            h3(span(
              tr("settings_feed_info", lang_init),
              `data-i18n` = "settings_feed_info"
            )),
            textInput(
              "fi_feed_publisher_name",
              label = tagList(
                span(
                  tr("lbl_publisher_name", lang_init),
                  `data-i18n` = "lbl_publisher_name"
                ),
                info_popover(
                  tr("pop_publisher_name", lang_init),
                  "https://gtfs.org/schedule/reference/#feed_infotxt",
                  key = "pop_publisher_name",
                  lang = lang_init
                )
              ),
              value = "Comotive"
            ),
            textInput(
              "fi_feed_publisher_url",
              label = tagList(
                span(
                  tr("lbl_publisher_url", lang_init),
                  `data-i18n` = "lbl_publisher_url"
                ),
                info_popover(
                  tr("pop_publisher_url", lang_init),
                  "https://gtfs.org/schedule/reference/#feed_infotxt",
                  key = "pop_publisher_url",
                  lang = lang_init
                )
              ),
              value = "https://www.comotive.net"
            ),
            selectInput(
              "fi_feed_lang",
              label = tagList(
                span(
                  tr("lbl_feed_lang", lang_init),
                  `data-i18n` = "lbl_feed_lang"
                ),
                info_popover(
                  tr("pop_feed_lang", lang_init),
                  "https://gtfs.org/schedule/reference/#feed_infotxt",
                  key = "pop_feed_lang",
                  lang = lang_init
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
                span(
                  tr("lbl_contact_email", lang_init),
                  `data-i18n` = "lbl_contact_email"
                ),
                info_popover(
                  tr("pop_contact_email", lang_init),
                  "https://gtfs.org/schedule/reference/#feed_infotxt",
                  key = "pop_contact_email",
                  lang = lang_init
                )
              ),
              value = "julian@comotive.net"
            ),
            textInput(
              "fi_feed_version",
              label = tagList(
                span(
                  tr("lbl_feed_version", lang_init),
                  `data-i18n` = "lbl_feed_version"
                ),
                info_popover(
                  tr("pop_feed_version", lang_init),
                  "https://gtfs.org/schedule/reference/#feed_infotxt",
                  key = "pop_feed_version",
                  lang = lang_init
                )
              ),
              value = paste0("v", Sys.Date())
            )
          ),

          # Advanced settings panel
          wellPanel(
            h3(span(
              tr("settings_advanced", lang_init),
              `data-i18n` = "settings_advanced"
            )),
            selectInput(
              "settings_routing_server",
              label = tagList(
                span(
                  tr("lbl_routing_server", lang_init),
                  `data-i18n` = "lbl_routing_server"
                ),
                info_popover(
                  tr("pop_routing_server", lang_init),
                  key = "pop_routing_server",
                  lang = lang_init
                )
              ),
              choices = c("OSRM", "Valhalla"),
              selected = "OSRM"
            ),
            numericInput(
              "settings_gtfs_workers",
              label = tagList(
                span(
                  tr("lbl_gtfs_workers", lang_init),
                  `data-i18n` = "lbl_gtfs_workers"
                ),
                info_popover(
                  tr("pop_gtfs_workers", lang_init),
                  key = "pop_gtfs_workers",
                  lang = lang_init
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
            checkboxInput(
              "settings_gtfs_max_date_enable",
              label = tagList(
                span(
                  tr("lbl_gtfs_max_date", lang_init),
                  `data-i18n` = "lbl_gtfs_max_date"
                ),
                info_popover(
                  tr("pop_gtfs_max_date", lang_init),
                  key = "pop_gtfs_max_date",
                  lang = lang_init
                )
              ),
              value = FALSE
            ),
            dateInput(
              "settings_gtfs_max_date",
              label = NULL,
              value = Sys.Date(),
              width = "240px"
            ),
            numericInput(
              "settings_min_stop_dist",
              label = tagList(
                span(
                  tr("lbl_min_stop_dist", lang_init),
                  `data-i18n` = "lbl_min_stop_dist"
                ),
                info_popover(
                  tr("pop_min_stop_dist", lang_init),
                  key = "pop_min_stop_dist",
                  lang = lang_init
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
                span(
                  tr("lbl_osm_provider", lang_init),
                  `data-i18n` = "lbl_osm_provider"
                ),
                info_popover(
                  tr("pop_osm_provider", lang_init),
                  key = "pop_osm_provider",
                  lang = lang_init
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

    # ── i18n language state ──
    lang <- reactiveVal(lang_init)

    observeEvent(input$app_lang, {
      lang(input$app_lang)
    })

    # Sync language to JS and re-translate static DOM elements
    observeEvent(lang(), {
      shinyjs::runjs(sprintf("croquisLang = '%s'; updateI18n();", lang()))
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
            tr("notif_project_loaded", lang()),
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            sprintf(tr("notif_load_file_error", lang()), e$message),
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

          gtfs_max_date <- if (isTRUE(input$settings_gtfs_max_date_enable)) {
            input$settings_gtfs_max_date
          } else {
            NULL
          }

          loaded_ssfs <- croquis::gtfs_to_ssfs(
            loaded_gtfs,
            max_date = gtfs_max_date,
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

          showNotification(tr("notif_gtfs_loaded", lang()), type = "message")
        },
        error = function(e) {
          showNotification(
            sprintf(tr("notif_load_file_error", lang()), e$message),
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
            sprintf(tr("notif_sample_loaded", lang()), "STM Ligne Jaune"),
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            sprintf(
              tr("notif_load_sample_error", lang()),
              "STM Ligne Jaune",
              e$message
            ),
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
            sprintf(tr("notif_sample_loaded", lang()), "STM Metro"),
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            sprintf(
              tr("notif_load_sample_error", lang()),
              "STM Metro",
              e$message
            ),
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
            sprintf(tr("notif_sample_loaded", lang()), "STM Mile-End"),
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            sprintf(
              tr("notif_load_sample_error", lang()),
              "STM Mile-End",
              e$message
            ),
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
            sprintf(tr("notif_sample_loaded", lang()), "TTC Subway"),
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            sprintf(
              tr("notif_load_sample_error", lang()),
              "TTC Subway",
              e$message
            ),
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
            sprintf(tr("notif_sample_loaded", lang()), "TransLink Vancouver"),
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            sprintf(
              tr("notif_load_sample_error", lang()),
              "TransLink Vancouver",
              e$message
            ),
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
          tr("notif_center_from_stops", lang()),
          type = "warning"
        )
        return()
      }

      search_term <- input$city_search

      if (is.null(search_term) || search_term == "") {
        showNotification(tr("notif_city_empty", lang()), type = "warning")
        return()
      }

      # Find exact matches (case insensitive)
      exact_matches <- cities_db[
        tolower(cities_db$name) == tolower(search_term),
      ]

      if (nrow(exact_matches) == 0) {
        showNotification(
          tr("notif_city_not_found", lang()),
          type = "warning"
        )
        return()
      } else if (nrow(exact_matches) > 1) {
        #flag : text for this notification could be changed. Verify that the case is real
        showNotification(
          tr("notif_city_multiple", lang()),
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
          sprintf(tr("notif_city_set", lang()), selected_city$name),
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
            tr("notif_coords_range", lang()),
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
    build_agency_row <- function(agency, lang) {
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
            title = tr("agency_edit_title", lang),
            htmltools::HTML("&#9998;")
          ),
          tags$button(
            class = "route-action-btn delete-btn",
            onclick = sprintf(
              "event.stopPropagation(); deleteAgencyFromList('%s')",
              agency$agency_id
            ),
            title = tr("agency_delete_title", lang),
            htmltools::HTML('<i class="fa-solid fa-trash"></i>')
          )
        )
      )
    }

    # -- Helper: build the inline agency edit/add form --
    build_agency_form <- function(agency = NULL, default_tz = NULL, lang) {
      is_new <- is.null(agency)
      div(
        class = "agency-edit-form",
        tags$label(
          tr("lbl_agency_id", lang),
          info_popover(
            tr("pop_agency_id", lang),
            "https://gtfs.org/schedule/reference/#agencytxt",
            lang = lang
          )
        ),
        tags$input(
          type = "text",
          id = "inline_ag_agency_id",
          value = if (!is_new) agency$agency_id else NULL,
          placeholder = if (is_new) tr("agency_ph_id", lang) else NULL
        ),
        tags$label(
          tr("lbl_agency_name", lang),
          info_popover(
            tr("pop_agency_name", lang),
            "https://gtfs.org/schedule/reference/#agencytxt",
            lang = lang
          )
        ),
        tags$input(
          type = "text",
          id = "inline_ag_agency_name",
          value = if (!is_new) agency$agency_name else NULL,
          placeholder = if (is_new) {
            tr("agency_ph_name", lang)
          } else {
            NULL
          }
        ),
        tags$label(
          tr("lbl_agency_url", lang),
          info_popover(
            tr("pop_agency_url", lang),
            "https://gtfs.org/schedule/reference/#agencytxt",
            lang = lang
          )
        ),
        tags$input(
          type = "text",
          id = "inline_ag_agency_url",
          value = if (!is_new) agency$agency_url else NULL,
          placeholder = if (is_new) tr("agency_ph_url", lang) else NULL
        ),
        tags$label(
          tr("lbl_agency_tz", lang),
          info_popover(
            tr("pop_agency_tz", lang),
            "https://gtfs.org/schedule/reference/#agencytxt",
            lang = lang
          )
        ),
        tags$input(
          type = "text",
          id = "inline_ag_agency_timezone",
          value = if (!is_new) agency$agency_timezone else default_tz,
          placeholder = if (is_new) tr("agency_ph_tz", lang) else NULL
        ),
        div(
          class = "btn-row",
          tags$button(
            class = "btn-save",
            onclick = "saveAgencyFromForm()",
            if (is_new) {
              tr("btn_create", lang)
            } else {
              tagList(htmltools::HTML("&#10003;"), tr("btn_save", lang))
            }
          ),
          tags$button(
            class = "btn-cancel",
            onclick = "cancelAgencyEdit()",
            tr("btn_cancel", lang)
          )
        )
      )
    }

    # -- Render the agency list UI --
    output$agency_list_ui <- renderUI({
      current_data <- ssfs()
      current_lang <- lang()
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
          rows[[length(rows) + 1]] <- build_agency_row(ag, current_lang)

          # If editing this row, show form directly below
          if (is_editing_this) {
            rows[[length(rows) + 1]] <- build_agency_form(
              ag,
              lang = current_lang
            )
          }
        }
      }

      # "Add new agency" row or add form
      if (is_adding) {
        rows[[length(rows) + 1]] <- build_agency_form(
          default_tz = isolate(ag_last_tz()),
          lang = current_lang
        )
      } else {
        rows[[length(rows) + 1]] <- div(
          class = "stop-list-row add-row",
          onclick = "startAddingAgency()",
          tags$button(
            class = "stop-action-btn add-btn",
            onclick = "event.stopPropagation(); startAddingAgency()",
            title = tr("agency_add_new", current_lang),
            htmltools::HTML("+")
          ),
          span(
            style = "margin-left: 8px;",
            tr("agency_add_new", current_lang)
          )
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
        showNotification(tr("notif_agency_id_empty", lang()), type = "warning")
        return()
      }

      current_data <- ssfs()

      if (ag_adding()) {
        # -- Adding a new agency --
        if (new_agency_id %in% current_data$agency$agency_id) {
          showNotification(
            tr("notif_agency_id_exists", lang()),
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

        showNotification(tr("notif_agency_added", lang()), type = "message")
      } else if (!is.null(ag_editing_id())) {
        # ── Editing an existing agency ──
        old_agency_id <- ag_editing_id()
        idx <- which(current_data$agency$agency_id == old_agency_id)

        if (length(idx) == 0) {
          showNotification(tr("notif_agency_not_found", lang()), type = "error")
          return()
        }

        # If agency_id changed, check for conflicts
        if (new_agency_id != old_agency_id) {
          other_ids <- current_data$agency$agency_id[-idx]
          if (new_agency_id %in% other_ids) {
            showNotification(
              tr("notif_agency_id_exists", lang()),
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

        showNotification(tr("notif_agency_updated", lang()), type = "message")
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
          sprintf(tr("notif_agency_cant_delete", lang()), agency_to_delete),
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

      showNotification(tr("notif_agency_deleted", lang()), type = "message")
    })

    # -- Helper: build a translated instructions bullet list --
    build_instr_list <- function(step, n_items, lang) {
      items <- lapply(seq_len(n_items), function(i) {
        tags$li(tr(sprintf("instr_s%d_li%d", step, i), lang))
      })
      p(tags$ul(items))
    }

    # -- Render the instructions panel (server-side for i18n) --
    output$home_instructions_ui <- renderUI({
      current_lang <- lang()

      tagList(
        h3(tr("instr_title", current_lang)),
        p(tr("instr_intro", current_lang)),
        h4(tr("instr_s1", current_lang)),
        build_instr_list(1, 3, current_lang),
        h4(tr("instr_s2", current_lang)),
        build_instr_list(2, 3, current_lang),
        h4(tr("instr_s3", current_lang)),
        build_instr_list(3, 6, current_lang),
        h4(tr("instr_s4", current_lang)),
        build_instr_list(4, 7, current_lang),
        h4(
          tr("instr_s5_pre", current_lang),
          icon("floppy-disk", class = "fa-solid"),
          tr("instr_s5_post", current_lang)
        )
      )
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
      reactive(input$settings_osm_provider),
      lang
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
      reactive(input$settings_routing_server),
      lang
    )

    #   #   #
    #
    ## SCHEDULE MODULE---------
    #
    #   #   #

    scheduleServer("schedule", ssfs, map_center, lang)

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
