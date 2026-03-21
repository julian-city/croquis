#' Croquis transit sketch planning Shiny app
#'
#' @returns
#'
#' @export
#' @examples
croquis <- function(ssfs = NULL) {
  # Validate input ssfs (and change name to avoid name collision in the server)

  input_ssfs <- NULL

  if (!is.null(ssfs)) {
    validate_ssfs(ssfs) # throws informative error if invalid
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

  # UI functions

  # Info icon with popover - uses Bootstrap 3 popovers (already bundled with Shiny)
  info_popover <- function(text, link) {
    tags$span(
      class = "info-icon",
      `data-toggle` = "popover",
      `data-trigger` = "click",
      `data-html` = "true",
      `data-placement` = "right",
      `data-content` = paste0(
        text,
        "<br><a href='",
        link,
        "' target='_blank'>Read more</a>"
      ),
      tabindex = "0",
      icon("info-circle", class = "text-muted")
    )
  }

  # UI Definition
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    # CSS and JavaScript in the head
    tags$head(
      tags$style(
        htmltools::HTML(
          "
      /* Light mode (default) */
      :root {
        --bg-color: #F9F5ED;
        --text-color: #333333;
        --panel-bg: #F1EBE3;
        --input-bg: #F9F5ED;
        --border-color: #5E5E5E;
        --hover-color: #F1EBE3;
        --btn-default-bg: #F1EBE3;
        --btn-default-color: #333333;
      }

      /* Dark mode */
      :root[data-bs-theme='dark'] {
        --bg-color: #1a1a1a;
        --text-color: #ffffff;
        --panel-bg: #2d2d2d;
        --input-bg: #3d3d3d;
        --border-color: #404040;
        --hover-color: #404040;
        --btn-default-bg: #404040;
        --btn-default-color: #ffffff;
      }

      /* Apply variables */
      body {
        background-color: var(--bg-color);
        color: var(--text-color);
      }

      .well, .panel {
        background-color: var(--panel-bg);
        border-color: var(--border-color);
      }

      .form-control {
        background-color: var(--input-bg);
        color: var(--text-color);
        border-color: var(--border-color);
      }

      .form-control:focus {
        background-color: var(--input-bg);
        color: var(--text-color);
      }

      .btn-default {
        background-color: var(--btn-default-bg);
        color: var(--btn-default-color);
        border-color: var(--border-color);
      }

      .dataTables_wrapper {
        color: var(--text-color);
      }

      .dataTable {
        color: var(--text-color);
        background-color: var(--panel-bg);
      }

      .dataTable tbody tr {
        background-color: var(--panel-bg) !important;
        color: var(--text-color) !important;
      }

      .dataTable tbody tr:hover {
        background-color: var(--hover-color) !important;
      }

      .navbar {
        background-color: var(--panel-bg);
        border-color: var(--border-color);
      }

      .navbar-default .navbar-nav > li > a {
        color: var(--text-color);
      }

      .navbar-default .navbar-nav > .active > a {
        background-color: var(--hover-color);
        color: var(--text-color);
      }

      /* Loading indicator styles */
    #loading-content {
      position: fixed;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      display: none;
      background-color: rgba(0, 0, 0, 0.5);
      z-index: 10000;
      display: flex;
      justify-content: center;
      align-items: center;
    }

    .loading-spinner {
      width: 50px;
      height: 50px;
      border: 5px solid #f3f3f3;
      border-top: 5px solid #3498db;
      border-radius: 50%;
      animation: spin 1s linear infinite;
    }

    @keyframes spin {
      0% { transform: rotate(0deg); }
      100% { transform: rotate(360deg); }
    }
    
    /* Floating panel styles for map-centric modules */
      .map-container {
        position: relative;
        width: 100%;
        height: calc(100vh - 120px);
        min-height: 600px;
        border: 1px solid var(--border-color);
        border-radius: 4px;
        overflow: hidden;
      }

      .map-container .leaflet-container {
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        z-index: 1;
      }

      .floating-panel {
        position: absolute;
        z-index: 1000;
        background-color: var(--panel-bg);
        border-radius: 8px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.2);
        padding: 15px;
        max-height: calc(100vh - 160px);
        overflow-y: auto;
      }

      .floating-panel-left {
        top: 10px;
        left: 10px;
        width: 320px;
      }

      .floating-panel-right {
        top: 10px;
        right: 10px;
        width: 320px;
      }

.floating-panel-bottom-left {
        bottom: 10px;
        left: 10px;
        width: 320px;
      }

      .floating-panel-bottom-right {
        bottom: 10px;
        right: 10px;
        width: 320px;
      }

      /* Import/Export panel specific styles */
      
      .import-export-panel .form-group {
        margin-bottom: 10px;
      }

      .import-export-panel .btn {
        margin-top: 5px;
        margin-right: 5px;
      }

      .import-export-panel hr {
        margin: 12px 0;
        border-color: var(--border-color);
      }

      .import-export-panel .selectize-input {
        min-height: 32px;
        padding: 4px 8px;
      }
      
      /* Fix fileInput alignment */
      .import-export-panel .shiny-input-container {
        width: 100% !important;
      }

      .import-export-panel .input-group {
        display: flex;
        width: 100%;
      }

      .import-export-panel .input-group .form-control {
        flex: 1;
        height: 34px;
      }

      .import-export-panel .input-group .input-group-btn {
        width: auto;
      }

      .import-export-panel .input-group .input-group-btn .btn {
        margin-top: 0;
        height: 34px;
      }

      /* Collapsible panel header */
      .floating-panel-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        cursor: pointer;
        margin-bottom: 10px;
        padding-bottom: 10px;
        border-bottom: 1px solid var(--border-color);
      }

      .floating-panel-header h4 {
        margin: 0;
      }

      .floating-panel-toggle {
        background: none;
        border: none;
        font-size: 18px;
        cursor: pointer;
        color: var(--text-color);
        padding: 0 5px;
      }

      .floating-panel.collapsed .floating-panel-content {
        display: none;
      }

      .floating-panel.collapsed {
        max-height: none;
        overflow: visible;
      }
      
       /* Stop List Panel Styles */
      .stop-list-container {
        max-height: calc(100vh - 350px);
        overflow-y: auto;
        margin-bottom: 10px;
      }

      .stop-list-row {
        display: flex;
        align-items: center;
        padding: 8px 10px;
        border-bottom: 1px solid var(--border-color);
        cursor: pointer;
        transition: background-color 0.15s ease;
      }

      .stop-list-row:hover {
        background-color: var(--hover-color);
      }

      .stop-list-row.editing {
        background-color: rgba(178, 24, 43, 0.1);
        border-left: 3px solid #B2182B;
      }

      .stop-list-row.add-row {
        justify-content: center;
        color: #666;
        font-style: italic;
      }

      .stop-list-row.add-row:hover {
        color: #333;
      }

      .stop-info {
        flex: 1;
        min-width: 0;
        overflow: hidden;
      }

      .stop-info-display {
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      .stop-name {
        font-weight: 500;
        font-size: 13px;
      }

      .stop-id-display {
        font-size: 11px;
        color: #888;
        margin-left: 4px;
      }

      .stop-actions {
        display: flex;
        gap: 6px;
        margin-left: 8px;
        flex-shrink: 0;
      }

      .stop-action-btn {
        width: 28px;
        height: 28px;
        border: none;
        border-radius: 4px;
        cursor: pointer;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 14px;
        transition: all 0.15s ease;
        background-color: transparent;
        color: var(--text-color);
      }

      .stop-action-btn:hover {
        background-color: var(--hover-color);
      }

      .stop-action-btn.save-btn {
        color: #2B5D2C;
      }

      .stop-action-btn.save-btn:hover {
        background-color: rgba(40, 167, 69, 0.15);
      }

      .stop-action-btn.cancel-btn {
        color: #B2182B;
      }

      .stop-action-btn.cancel-btn:hover {
        background-color: rgba(220, 53, 69, 0.15);
      }

      .stop-action-btn.edit-btn {
        color: #6c757d;
      }

      .stop-action-btn.edit-btn:hover {
        background-color: rgba(108, 117, 125, 0.15);
        color: #495057;
      }

      .stop-action-btn.delete-btn {
        color: #444;
      }

      .stop-action-btn.delete-btn:hover {
        background-color: #888;
      }

      .stop-action-btn.add-btn {
        color: #2B5D2C;
        font-size: 18px;
        font-weight: bold;
      }

      .stop-edit-inputs {
        display: flex;
        flex-direction: column;
        gap: 4px;
        flex: 1;
      }

      .stop-edit-inputs input {
        padding: 4px 8px;
        border: 1px solid var(--border-color);
        border-radius: 4px;
        font-size: 12px;
        background-color: var(--input-bg);
        color: var(--text-color);
      }

      .stop-edit-inputs input:focus {
        outline: none;
        border-color: #B2182B;
      }

      .stop-search-container {
        position: relative;
        margin-bottom: 12px;
      }

      .stop-search-container input {
        width: 100%;
        padding: 8px 12px;
        border: 1px solid var(--border-color);
        border-radius: 6px;
        font-size: 13px;
        background-color: var(--input-bg);
        color: var(--text-color);
      }

      .stop-search-container input:focus {
        outline: none;
        border-color: #B2182B;
      }

      .editing-instruction {
        font-size: 12px;
        color: #B2182B;
        margin-top: 8px;
        margin-bottom: 8px;
        padding: 8px;
        background-color: rgba(178, 24, 43, 0.08);
        border-radius: 4px;
        text-align: center;
      }

      /* Route List Panel Styles */
      .route-list-container {
        max-height: calc(100vh - 300px);
        overflow-y: auto;
        margin-bottom: 10px;
      }

      .route-list-row {
        display: flex;
        align-items: center;
        padding: 8px 10px;
        border-bottom: 1px solid var(--border-color);
        cursor: pointer;
        transition: background-color 0.15s ease;
      }

      .route-list-row:hover {
        background-color: var(--hover-color);
      }

      .route-list-row.expanded {
        background-color: rgba(5, 174, 239, 0.06);
        border-left: 3px solid #05AEEF;
      }

      .route-list-row.editing-route {
        background-color: rgba(178, 24, 43, 0.1);
        border-left: 3px solid #B2182B;
      }

      .route-color-badge {
        width: 24px;
        height: 24px;
        border-radius: 4px;
        border: 1px solid #ccc;
        margin-right: 8px;
        flex-shrink: 0;
      }

      .route-info {
        flex: 1;
        min-width: 0;
        overflow: hidden;
      }

      .route-info-display {
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      .route-short-name {
        font-weight: 600;
        font-size: 13px;
      }

      .route-long-name {
        font-size: 11px;
        color: #888;
        margin-left: 4px;
      }

      .route-actions {
        display: flex;
        gap: 4px;
        margin-left: 8px;
        flex-shrink: 0;
      }

      .route-action-btn {
        width: 26px;
        height: 26px;
        border: none;
        border-radius: 4px;
        cursor: pointer;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 13px;
        transition: all 0.15s ease;
        background-color: transparent;
        color: var(--text-color);
      }

      .route-action-btn:hover {
        background-color: var(--hover-color);
      }

      .route-action-btn.delete-btn {
        color: #444;
      }

      .route-action-btn.delete-btn:hover {
        background-color: #888;
      }

      .route-action-btn.expand-btn {
        font-size: 10px;
      }

      /* Itinerary sub-rows */
      .itin-list-container {
        padding-left: 20px;
        border-left: 2px solid var(--border-color);
        margin-left: 12px;
        margin-bottom: 4px;
      }

      .itin-list-row {
        display: flex;
        align-items: center;
        padding: 6px 8px;
        border-bottom: 1px solid var(--border-color);
        cursor: pointer;
        transition: background-color 0.15s ease;
        font-size: 12px;
      }

      .itin-list-row:hover {
        background-color: var(--hover-color);
      }

      .itin-list-row.active-itin {
        background-color: rgba(178, 24, 43, 0.1);
        border-left: 3px solid #B2182B;
      }

      .itin-info {
        flex: 1;
        min-width: 0;
        overflow: hidden;
      }

      .itin-info-display {
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      .itin-headsign {
        font-weight: 500;
        font-size: 12px;
      }

      .itin-id-display {
        font-size: 10px;
        color: #888;
        margin-left: 4px;
      }

      .itin-direction-badge {
        font-size: 10px;
        padding: 1px 5px;
        border-radius: 3px;
        background-color: var(--hover-color);
        margin-right: 6px;
        flex-shrink: 0;
      }

      /* Inline route edit form */
      .route-edit-form {
        padding: 10px;
        border: 1px solid var(--border-color);
        border-radius: 6px;
        margin: 4px 0;
        background-color: var(--panel-bg);
      }

      .route-edit-form label {
        font-size: 11px;
        font-weight: 500;
        margin-bottom: 2px;
        display: block;
      }

      .route-edit-form input,
      .route-edit-form select {
        width: 100%;
        padding: 4px 8px;
        border: 1px solid var(--border-color);
        border-radius: 4px;
        font-size: 12px;
        background-color: var(--input-bg);
        color: var(--text-color);
        margin-bottom: 6px;
      }

      .route-edit-form input:focus,
      .route-edit-form select:focus {
        outline: none;
        border-color: #B2182B;
      }

      .route-edit-form .btn-row {
        display: flex;
        gap: 6px;
        margin-top: 6px;
      }

      .route-edit-form .btn-row button {
        flex: 1;
        padding: 4px 8px;
        font-size: 12px;
        border: none;
        border-radius: 4px;
        cursor: pointer;
      }

      .route-edit-form .btn-save {
        background-color: #2B5D2C;
        color: white;
      }

      .route-edit-form .btn-cancel {
        background-color: #6c757d;
        color: white;
      }

      /* Inline itin edit form */
      .itin-edit-form {
        padding: 8px;
        border: 1px solid var(--border-color);
        border-radius: 6px;
        margin: 4px 0;
        background-color: var(--panel-bg);
      }

      .itin-edit-form label {
        font-size: 11px;
        font-weight: 500;
        margin-bottom: 2px;
        display: block;
      }

      .itin-edit-form input,
      .itin-edit-form select {
        width: 100%;
        padding: 4px 8px;
        border: 1px solid var(--border-color);
        border-radius: 4px;
        font-size: 12px;
        background-color: var(--input-bg);
        color: var(--text-color);
        margin-bottom: 6px;
      }

      .itin-edit-form .btn-row {
        display: flex;
        gap: 6px;
        margin-top: 4px;
      }

      .itin-edit-form .btn-row button {
        flex: 1;
        padding: 4px 8px;
        font-size: 11px;
        border: none;
        border-radius: 4px;
        cursor: pointer;
      }

      .itin-edit-form .btn-save {
        background-color: #2B5D2C;
        color: white;
      }

      .itin-edit-form .btn-cancel {
        background-color: #6c757d;
        color: white;
      }

      /* Stop sequence floating panel */
      .floating-panel-top-right {
        top: 10px;
        right: 10px;
        width: 280px;
        max-height: 50vh;
      }

      .info-icon {
        cursor: pointer;
        margin-left: 4px;
        font-size: 13px;
        color: var(--text-color);
        opacity: 0.5;
      }
      .info-icon:hover {
        opacity: 1;
      }
      .popover {
        background-color: var(--panel-bg);
        color: var(--text-color);
        border-color: var(--border-color);
      }
      .popover .arrow::after {
        border-right-color: var(--panel-bg);
      }
      .popover a {
        color: #337ab7;
      }
    "
        )
      ),
      tags$script(htmltools::HTML(
        "
    function toggleTheme() {
      const root = document.documentElement;
      const currentTheme = root.getAttribute('data-bs-theme');
      const newTheme = currentTheme === 'dark' ? 'light' : 'dark';
      root.setAttribute('data-bs-theme', newTheme);
      
      // Store the preference
      localStorage.setItem('theme', newTheme);
      
      // Update button text
      const btn = document.getElementById('theme-toggle');
      btn.innerHTML = '\u25d1';
    }

    // Set initial theme from stored preference
    document.addEventListener('DOMContentLoaded', function() {
      const storedTheme = localStorage.getItem('theme') || 'light';
      document.documentElement.setAttribute('data-bs-theme', storedTheme);
      const btn = document.getElementById('theme-toggle');
      btn.innerHTML = '\u25d1';
      
      // Add right-click handling for the routes map (add this part)
      $(document).on('contextmenu', '#routes_map', function(e) {
        e.preventDefault();
        return false;
      });
    });

    // Loading indicator JavaScript with delay
    var loadingTimeout;
    
    $(document).on('shiny:busy', function() {
      // Only show the loading indicator if the app stays busy for more than 1 second
      loadingTimeout = setTimeout(function() {
        $('#loading-content').show();
      }, 1000); // 1000ms = 1 second delay
    });
    
    $(document).on('shiny:idle', function() {
      // Clear the timeout if the app becomes idle before the delay expires
      clearTimeout(loadingTimeout);
      // Hide the loading indicator
      $('#loading-content').hide();
    });
    
  // City selection functions
  function selectCity(cityName) {
    Shiny.setInputValue('selected_city_name', cityName);
  }
  
  // Custom message handlers for suggestions
  Shiny.addCustomMessageHandler('showSuggestions', function(html) {
    $('#city_suggestions').html(html).show();
  });
  
  Shiny.addCustomMessageHandler('hideSuggestions', function(message) {
    $('#city_suggestions').hide();
  });
  
  // Hide suggestions when clicking outside
  $(document).on('click', function(e) {
    if (!$(e.target).closest('#city_suggestions, #city_search').length) {
      $('#city_suggestions').hide();
    }
  });
  
  // Toggle floating panel collapse
    function togglePanel(panelId) {
      var panel = document.getElementById(panelId);
      var icon = panel.querySelector('.floating-panel-toggle');
      if (panel.classList.contains('collapsed')) {
        panel.classList.remove('collapsed');
        icon.innerHTML = '\u2212';
      } else {
        panel.classList.add('collapsed');
        icon.innerHTML = '+';
      }
    }
  
  // Function to handle stop search input changes
  $(document).on('input', '#stop_search', function() {
    Shiny.setInputValue('stop_search_term', $(this).val(), {priority: 'event'});
  });

  // Function to start editing a stop from the list
  function editStopFromList(stopId) {
    Shiny.setInputValue('stop_list_edit_click', stopId, {priority: 'event'});
  }

  // Function to start adding a new stop
  function startAddingStop() {
    Shiny.setInputValue('stop_list_add_click', Math.random(), {priority: 'event'});
  }

    // Function to save the currently editing stop with input values
  function saveEditingStop() {
    var stopId = document.getElementById('inline_stop_id') ? document.getElementById('inline_stop_id').value : '';
    var stopName = document.getElementById('inline_stop_name') ? document.getElementById('inline_stop_name').value : '';
    Shiny.setInputValue('stop_list_save_data', {
      stop_id: stopId,
      stop_name: stopName,
      timestamp: Math.random()
    }, {priority: 'event'});
  }

  // Function to cancel editing
  function cancelEditingStop() {
    Shiny.setInputValue('stop_list_cancel_click', Math.random(), {priority: 'event'});
  }
  
  // Function to view/focus on a stop without editing
  function viewStopFromList(stopId) {
    Shiny.setInputValue('stop_list_view_click', stopId, {priority: 'event'});
  }

  // Delete stop (trash icon on stop row)
  function deleteStopFromList(stopId) {
    if (confirm('This stop will be deleted if it is not associsted with any itineraries.')) {
      Shiny.setInputValue('stop_list_delete_click', {id: stopId, ts: Math.random()}, {priority: 'event'});
    }
  }

  // Routes module JS functions

  // Toggle route expand/collapse
  function toggleRouteExpand(routeId) {
    Shiny.setInputValue('route_list_toggle_expand', {id: routeId, ts: Math.random()}, {priority: 'event'});
  }

  // Edit route details (pencil icon on route row)
  function editRouteFromList(routeId) {
    Shiny.setInputValue('route_list_edit_click', {id: routeId, ts: Math.random()}, {priority: 'event'});
  }

  // Delete route (trash icon on route row)
  function deleteRouteFromList(routeId) {
    if (confirm('Delete this route? Itineraries must be deleted first.')) {
      Shiny.setInputValue('route_list_delete_click', {id: routeId, ts: Math.random()}, {priority: 'event'});
    }
  }

  // Start adding a new route
  function startAddingRoute() {
    Shiny.setInputValue('route_list_add_click', Math.random(), {priority: 'event'});
  }

  // Save route from inline form
  function saveRouteFromForm() {
    var data = {
      route_id: document.getElementById('inline_route_id') ? document.getElementById('inline_route_id').value : '',
      agency_id: document.getElementById('inline_agency_id') ? document.getElementById('inline_agency_id').value : '',
      short_name: document.getElementById('inline_route_short_name') ? document.getElementById('inline_route_short_name').value : '',
      long_name: document.getElementById('inline_route_long_name') ? document.getElementById('inline_route_long_name').value : '',
      route_type: document.getElementById('inline_route_type') ? document.getElementById('inline_route_type').value : '3',
      route_color: document.getElementById('inline_route_color') ? document.getElementById('inline_route_color').value : '#92C5DE',
      route_text_color: document.getElementById('inline_route_text_color') ? document.getElementById('inline_route_text_color').value : '#000000',
      ts: Math.random()
    };
    Shiny.setInputValue('route_list_save_data', data, {priority: 'event'});
  }

  // Cancel route editing
  function cancelRouteEdit() {
    Shiny.setInputValue('route_list_cancel_click', Math.random(), {priority: 'event'});
  }

  // Click on itinerary row (view/center on map)
  function viewItinFromList(itinId) {
    Shiny.setInputValue('itin_list_view_click', {id: itinId, ts: Math.random()}, {priority: 'event'});
  }

  // Edit itinerary (pencil icon - loads for map editing)
  function editItinFromList(itinId) {
    Shiny.setInputValue('itin_list_edit_click', {id: itinId, ts: Math.random()}, {priority: 'event'});
  }

  // Delete itinerary (trash icon)
  function deleteItinFromList(itinId) {
    if (confirm('Delete this itinerary and its associated data?')) {
      Shiny.setInputValue('itin_list_delete_click', {id: itinId, ts: Math.random()}, {priority: 'event'});
    }
  }

  // Copy/duplicate itinerary
  function copyItinFromList(itinId) {
    Shiny.setInputValue('itin_list_copy_click', {id: itinId, ts: Math.random()}, {priority: 'event'});
  }

  // Start adding a new itinerary under a route
  function startAddingItin(routeId) {
    Shiny.setInputValue('itin_list_add_click', {id: routeId, ts: Math.random()}, {priority: 'event'});
  }

  // Save itinerary details from inline form
  function saveItinFromForm() {
    var data = {
      itin_id: document.getElementById('inline_itin_id') ? document.getElementById('inline_itin_id').value : '',
      direction_id: document.getElementById('inline_direction_id') ? document.getElementById('inline_direction_id').value : '0',
      trip_headsign: document.getElementById('inline_trip_headsign') ? document.getElementById('inline_trip_headsign').value : '',
      ts: Math.random()
    };
    Shiny.setInputValue('itin_list_save_data', data, {priority: 'event'});
  }

  // When direction_id changes in inline form, ask server to recalculate itin_id
  function onDirectionChanged() {
    var directionId = document.getElementById('inline_direction_id') ? document.getElementById('inline_direction_id').value : '0';
    Shiny.setInputValue('inline_direction_changed', {direction_id: directionId, ts: Math.random()}, {priority: 'event'});
  }

    // Handler for server-driven update of inline_itin_id field
  Shiny.addCustomMessageHandler('updateInlineItinId', function(newId) {
    var el = document.getElementById('inline_itin_id');
    if (el) {
      el.value = newId;
    }
  });

  // Cancel itinerary editing
  function cancelItinEdit() {
    Shiny.setInputValue('itin_list_cancel_click', Math.random(), {priority: 'event'});
  }
  
  // JS initializers for icon popups
    $(document).on('shown.bs.tab change', function(){ 
      $('[data-toggle=\"popover\"]').popover({container: 'body'}); 
    });
    $(document).ready(function(){ 
      $('[data-toggle=\"popover\"]').popover({container: 'body'}); 
    });

    // Re-initialize popovers after any Shiny output re-renders (covers renderUI)
    $(document).on('shiny:value', function() {
      setTimeout(function() {
        $('[data-toggle=\"popover\"]').popover({container: 'body'});
      }, 100);
    });

    // Dismiss popovers on click-away
    $(document).on('mousedown', function(e) {
      var $target = $(e.target);
      // If click is not inside a popover or on an info-icon, hide all popovers
      if (!$target.closest('.popover').length && !$target.closest('.info-icon').length) {
        $('[data-toggle=\"popover\"]').popover('hide');
      }
    });

    // Dismiss popovers on tab change
    $(document).on('shown.bs.tab', function() {
      $('[data-toggle=\"popover\"]').popover('hide');
    });

  "
      ))
    ),

    #loading indicator div
    div(id = "loading-content", div(class = "loading-spinner")),

    #Module architecture
    navbarPage(
      title = "Croquis",
      tags$script(
        "
    $(document).on('keydown', function(e) {
      if (e.key === 'Backspace') {
        var tag = e.target.tagName.toLowerCase();
        var isEditable = (tag === 'input' || tag === 'textarea' || tag === 'select' || e.target.isContentEditable);
        if (!isEditable) {
          Shiny.setInputValue('backspace_pressed', Math.random());
        }
      }
    });
  "
      ),
      # div for the theme toggle
      header = div(
        style = "position: absolute; right: 10px; top: 10px; z-index: 1000;",
        tags$button(
          id = "theme-toggle",
          onclick = "toggleTheme()",
          class = "btn btn-default btn-sm",
          htmltools::HTML("&#9680;")
        )
      ),

      #home tab
      tabPanel(
        tags$span(htmltools::HTML("&#127968;")),
        #unicode house emoji
        fluidPage(
          titlePanel("Create or edit a GTFS"),

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
              "You can load an existing GTFS here (WARNING uploading a GTFS with more than 5 routes may take several minutes)"
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

      tabPanel(
        "stops",
        fluidPage(
          titlePanel("stops"),
          # Map container with floating panels
          div(
            class = "map-container",
            # Full-width map
            leaflet::leafletOutput(
              "stops_map",
              height = "100%",
              width = "100%"
            ),

            # Floating control panel (left side)
            div(
              id = "stops-control-panel",
              class = "floating-panel floating-panel-left",
              div(
                class = "floating-panel-header",
                onclick = "togglePanel('stops-control-panel')",
                h4("Stops"),
                tags$button(
                  class = "floating-panel-toggle",
                  htmltools::HTML("&minus;")
                )
              ),
              div(
                class = "floating-panel-content",
                # Search bar
                div(
                  class = "stop-search-container",
                  tags$input(
                    type = "text",
                    id = "stop_search",
                    placeholder = "Search stops..."
                  )
                ),

                # Editing instruction (shown when editing)
                uiOutput("stops_editing_instruction"),

                # Stop list
                div(class = "stop-list-container", uiOutput("stop_list_ui"))
              )
            ),

            # Import/Export floating panel (bottom-right)
            div(
              id = "stops-import-export-panel",
              class = "floating-panel floating-panel-bottom-right",
              div(
                class = "floating-panel-header",
                onclick = "togglePanel('stops-import-export-panel')",
                h4("Import / Export"),
                tags$button(
                  class = "floating-panel-toggle",
                  htmltools::HTML("&minus;")
                )
              ),
              div(
                class = "floating-panel-content",
                # Import section
                h5("Import Stops"),
                fileInput(
                  "stops_import_file",
                  label = NULL,
                  accept = c(".geojson", ".kml"),
                  placeholder = "GeoJSON or KML file"
                ),
                actionButton(
                  "stops_import_confirm",
                  "Import",
                  class = "btn-success btn-sm"
                ),
                hr(),
                # Export section
                h5("Export Stops"),
                selectInput(
                  "stops_export_format",
                  label = NULL,
                  choices = c(
                    "GeoJSON" = "geojson",
                    "KML" = "kml",
                    "Shapefile" = "shp"
                  ),
                  selected = "geojson"
                ),
                downloadButton(
                  "stops_export_download",
                  "Download",
                  class = "btn-primary btn-sm"
                )
              )
            )
          )
        )
      ),

      #routes tab (consolidated routes + itineraries)
      tabPanel(
        "routes",
        fluidPage(
          titlePanel("routes"),
          # Map container with floating panels
          div(
            class = "map-container",
            # Full-width map
            leaflet::leafletOutput(
              "routes_map",
              height = "100%",
              width = "100%"
            ),

            # Floating panel: Routes list (top-left)
            div(
              id = "routes-control-panel",
              class = "floating-panel floating-panel-left",
              div(
                class = "floating-panel-header",
                onclick = "togglePanel('routes-control-panel')",
                h4("Routes"),
                tags$button(
                  class = "floating-panel-toggle",
                  htmltools::HTML("&minus;")
                )
              ),
              div(
                class = "floating-panel-content",
                # Instruction when editing itinerary
                uiOutput("routes_editing_instruction"),
                # Route list
                div(class = "route-list-container", uiOutput("route_list_ui"))
              )
            ),

            # Floating panel: Drawing Mode (bottom-left)
            div(
              id = "routes-drawing-panel",
              class = "floating-panel floating-panel-bottom-left",
              div(
                class = "floating-panel-header",
                onclick = "togglePanel('routes-drawing-panel')",
                h4("Drawing Mode"),
                tags$button(
                  class = "floating-panel-toggle",
                  htmltools::HTML("&minus;")
                )
              ),
              div(
                class = "floating-panel-content",
                radioButtons(
                  "drawing_mode",
                  NULL,
                  choices = c(
                    "Road Network" = "network",
                    "Free Drawing" = "free"
                  ),
                  selected = "network"
                ),
                tags$small(
                  "Network mode routes along streets. Free mode draws straight lines."
                )
              )
            ),

            # Floating panel: Stop Sequence (top-right)
            div(
              id = "routes-stopseq-panel",
              class = "floating-panel floating-panel-top-right",
              div(
                class = "floating-panel-header",
                onclick = "togglePanel('routes-stopseq-panel')",
                h4("Stop Sequence"),
                tags$button(
                  class = "floating-panel-toggle",
                  htmltools::HTML("&minus;")
                )
              ),
              div(
                class = "floating-panel-content",
                DT::DTOutput("selected_stops_table")
              )
            )
          )
        )
      ),

      #calendar tab
      tabPanel(
        "calendar",
        fluidPage(
          titlePanel("calendar"),
          sidebarLayout(
            sidebarPanel(
              textInput(
                "service_id",
                label = tagList(
                  "Service ID",
                  info_popover(
                    "Identifies a set of dates when service is available for one or more routes.",
                    "https://gtfs.org/schedule/reference/#calendartxt"
                  )
                ),
                placeholder = "Enter service ID"
              ),

              h4("Days of Operation"),
              fluidRow(
                column(
                  6,
                  checkboxInput("monday", "Monday", value = FALSE),
                  checkboxInput("tuesday", "Tuesday", value = FALSE),
                  checkboxInput("wednesday", "Wednesday", value = FALSE),
                  checkboxInput("thursday", "Thursday", value = FALSE)
                ),
                column(
                  6,
                  checkboxInput("friday", "Friday", value = FALSE),
                  checkboxInput("saturday", "Saturday", value = FALSE),
                  checkboxInput("sunday", "Sunday", value = FALSE)
                )
              ),

              h4("Service Period"),
              dateInput(
                "start_date",
                "Start date",
                value = "2000-01-01",
                min = "1970-01-01",
                max = "2099-12-31",
                format = "yyyy-mm-dd"
              ),
              dateInput(
                "end_date",
                "End Date",
                value = "2099-12-31",
                min = "1970-01-01",
                max = "2099-12-31",
                format = "yyyy-mm-dd"
              ),

              actionButton("add_service", "Add service", class = "btn-success"),
              actionButton("clear_service", "Clear form", class = "btn-warning")
            ),
            mainPanel(
              DT::DTOutput("calendar_table"),
              actionButton(
                "delete_selected_service",
                "Delete selected service",
                class = "btn-danger"
              )
            )
          )
        )
      ),

      #spans tab
      tabPanel(
        "spans",
        fluidPage(
          titlePanel("spans"),
          sidebarLayout(
            sidebarPanel(
              selectInput("span_itin_id", "Route itinerary", choices = NULL),
              selectInput("span_service_id", "Service ID", choices = NULL),

              # Service window display (read-only, similar to variant_ID in itineraries)
              tags$div(
                tags$label("Service Window"),
                textOutput("service_window_display", inline = FALSE),
                style = "margin-bottom: 15px;"
              ),

              h4("Service Times"),
              textInput(
                "first_dep",
                "First departure",
                value = "05:00:00",
                placeholder = "HH:MM:SS"
              ),
              textInput(
                "last_dep",
                "Last departure",
                value = "23:00:00",
                placeholder = "HH:MM:SS"
              ),

              actionButton(
                "add_span",
                "Add service span",
                class = "btn-success"
              ),
              actionButton("clear_span", "Clear form", class = "btn-warning"),
              hr(),
              actionButton(
                "delete_selected_span",
                "Delete selected row",
                class = "btn-danger"
              )
            ),
            mainPanel(DT::DTOutput("spans_table"))
          )
        )
      ),

      #Service patterns tab
      tabPanel(
        "headway presets",
        fluidPage(
          titlePanel("headway presets"),
          sidebarLayout(
            sidebarPanel(
              width = 4,
              h4("Manage Headway Presets"),
              helpText(
                "Headway presets define hour-by-hour headways that can be applied to route itinerary and service combinations in the headways module."
              ),
              hr(),

              # Mode selection: view/edit existing OR create new
              radioButtons(
                "sp_mode",
                "Mode:",
                choices = c(
                  "View/Edit Existing" = "edit",
                  "Create New" = "new"
                ),
                selected = "edit"
              ),

              # Conditional panel for editing existing patterns
              conditionalPanel(
                condition = "input.sp_mode == 'edit'",
                selectInput(
                  "sp_select",
                  "Select headway preset:",
                  choices = NULL
                ),
                actionButton(
                  "sp_load",
                  "Load selected preset",
                  class = "btn-info"
                ),
                hr(),
                # Edit form (shown when editing)
                conditionalPanel(
                  condition = "output.editing_sp == true",
                  h4("Edit Row"),
                  numericInput(
                    "sp_edit_headway",
                    "Headway (minutes)",
                    value = 15,
                    min = 1,
                    max = 120
                  ),
                  actionButton(
                    "sp_save_edit",
                    "Save changes",
                    class = "btn-success"
                  ),
                  actionButton(
                    "sp_cancel_edit",
                    "Cancel",
                    class = "btn-warning"
                  )
                ),
                # Action buttons (shown when not editing)
                conditionalPanel(
                  condition = "output.editing_sp == false",
                  actionButton(
                    "sp_edit_row",
                    "Edit selected row",
                    class = "btn-info"
                  ),
                  actionButton(
                    "sp_add_row",
                    "Add new hour",
                    class = "btn-success"
                  ),
                  actionButton(
                    "sp_delete_row",
                    "Delete selected row",
                    class = "btn-danger"
                  )
                ),
                hr(),
                # Add hour form (shown when adding)
                conditionalPanel(
                  condition = "output.adding_sp_hour == true",
                  h4("Add New Hour"),
                  selectInput(
                    "sp_new_hour",
                    "Hour (HH:00:00):",
                    choices = NULL
                  ),
                  numericInput(
                    "sp_new_headway",
                    "Headway (minutes)",
                    value = 15,
                    min = 1,
                    max = 120
                  ),
                  actionButton(
                    "sp_confirm_add",
                    "Add hour",
                    class = "btn-success"
                  ),
                  actionButton("sp_cancel_add", "Cancel", class = "btn-warning")
                )
              ),

              # Conditional panel for creating new patterns
              conditionalPanel(
                condition = "input.sp_mode == 'new'",
                textInput(
                  "sp_new_name",
                  "Pattern name:",
                  placeholder = "e.g., Evening Service"
                ),
                helpText("A unique pattern ID will be assigned automatically."),
                hr(),
                h4("Add Hours to New Pattern"),
                selectInput(
                  "sp_create_hour",
                  "Hour (HH:00:00):",
                  choices = sprintf("%02d:00:00", 0:30)
                ),
                numericInput(
                  "sp_create_headway",
                  "Headway (minutes)",
                  value = 15,
                  min = 1,
                  max = 120
                ),
                actionButton(
                  "sp_create_add_hour",
                  "Add hour to pattern",
                  class = "btn-primary"
                ),
                actionButton(
                  "sp_create_remove_hour",
                  "Remove selected hour",
                  class = "btn-warning"
                ),
                hr(),
                actionButton(
                  "sp_save_new",
                  "Save new pattern",
                  class = "btn-success"
                ),
                actionButton(
                  "sp_clear_new",
                  "Clear / Start over",
                  class = "btn-danger"
                )
              )
            ),
            mainPanel(
              width = 8,
              DT::DTOutput("sp_table"),
              conditionalPanel(
                condition = "input.sp_mode == 'new'",
                helpText(
                  "Preview of your new service pattern. Add hours using the form on the left, then click Save to create the pattern."
                )
              )
            )
          )
        )
      ),

      #headways hsh tab
      tabPanel(
        "headways",
        fluidPage(
          titlePanel("headways & speeds by hour"),
          sidebarLayout(
            sidebarPanel(
              width = 3,
              selectInput("hsh_itin_id", "Route itinerary", choices = NULL),
              selectInput("hsh_service_id", "Service ID", choices = NULL),
              actionButton("load_hsh", "Load", class = "btn-info"),
              hr(),
              h4("Apply Service Pattern"),
              helpText(
                "Apply a pre-defined service pattern to the currently selected route itinerary and service combination."
              ),
              selectInput(
                "hsh_apply_pattern",
                "Select pattern to apply:",
                choices = NULL
              ),
              actionButton(
                "hsh_apply_pattern_btn",
                "Apply pattern",
                class = "btn-primary"
              ),
              hr(),
              h4("Set speed for all hours"),
              helpText(
                "Apply a single speed to all hours for the current selection."
              ),
              numericInput(
                "hsh_batch_speed",
                "Speed (km/h)",
                value = 20,
                min = 5,
                max = 431
              ),
              actionButton(
                "hsh_apply_batch_speed",
                "Apply to all hours",
                class = "btn-primary"
              ),
              hr(),
              # Form for adding/editing rows
              conditionalPanel(
                condition = "output.editing_hsh == true",
                h4("Edit Row"),
                textInput("edit_hour_dep", "Hour (HH:00:00)", ""),
                numericInput(
                  "edit_headway",
                  "Headway (minutes)",
                  value = 12,
                  min = 1,
                  max = 60
                ),
                numericInput(
                  "edit_speed",
                  "Speed (km/h)",
                  value = 20,
                  min = 5, #5km/h is approximately walking speed
                  max = 431 #431km/h is the speed of the fastest commercial high speed rail service (Shanghai Maglev)
                ),
                actionButton(
                  "save_hsh_edit",
                  "Save changes",
                  class = "btn-success"
                ),
                actionButton("cancel_hsh_edit", "Cancel", class = "btn-warning")
              ),
              conditionalPanel(
                condition = "output.editing_hsh == false",
                actionButton(
                  "edit_hsh_row",
                  "Edit selected row",
                  class = "btn-info"
                ),
                actionButton(
                  "add_hsh_row",
                  "Add new row",
                  class = "btn-success"
                ),
                actionButton(
                  "delete_selected_hsh",
                  "Delete selected",
                  class = "btn-danger"
                )
              )
            ),
            mainPanel(DT::DTOutput("hsh_table"))
          )
        )
      ),

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
      )
    )
  )

  #SERVER-------------------------

  server <- function(input, output, session) {
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

    # Load cities data from GitHub
    cities_data <- reactiveVal(NULL)

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

    # Load cities data from GitHub on app startup
    observe({
      tryCatch(
        {
          url <- "https://github.com/julian-city/gtfsforge/raw/refs/heads/main/cities_db.rds"

          # Create a temporary file
          temp_file <- tempfile(fileext = ".rds")

          # Download the file
          download.file(url, temp_file, mode = "wb")

          # Load the file
          cities_df <- readRDS(temp_file)

          # Clean up
          unlink(temp_file)

          cities_data(cities_df)
          showNotification(
            "Cities database loaded successfully",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Error loading cities database:", e$message),
            type = "warning"
          )
          # Create empty fallback
          cities_data(data.frame(
            name = character(),
            lat = numeric(),
            long = numeric(),
            tz = character()
          ))
        }
      )
    })

    # Helper function for marker size calculation for stops on maps
    calculateMarkerSize <- function(zoom) {
      base_size <- 2
      adjusted_size <- base_size * (1.2^(zoom - 10))
      return(min(max(adjusted_size, 4), 15))
    }

    #current zoom reactive value
    current_zoom <- reactiveVal(10)

    #function for adding base maps
    addBaseMaps <- function(map) {
      map |>
        leaflet::addProviderTiles("CartoDB.Positron", group = "Positron") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addProviderTiles("OpenStreetMap.HOT", group = "OSM") |>
        leaflet::addLayersControl(
          baseGroups = c("Positron", "Satellite", "OSM"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )
    }

    # Function to update any map with current ssfs data
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
        for (i in 1:nrow(current_data$itin)) {
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

      return(proxy)
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

          validate_ssfs(loaded_ssfs)

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

          loaded_ssfs <- gtfs_to_ssfs(loaded_gtfs)

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
    observeEvent(input$load_yellowline_ssfs, {
      tryCatch(
        {
          # URL to your raw GitHub file
          url <- "https://github.com/julian-city/gtfsforge/raw/refs/heads/main/sample_networks/ligne_jaune_v2.rds"

          # Create a temporary file
          temp_file <- tempfile(fileext = ".rds")

          # Download the file
          download.file(url, temp_file, mode = "wb")

          # Load the file
          ljaune_ssfs <- readRDS(temp_file)

          # Clean up
          unlink(temp_file)

          #adjust ssfs to data structure (stop names in stop seq,
          #FOR 3.0 NO SPEED FACTOR, 3.1 will incorporate this)

          stop_id_to_stopname <-
            ljaune_ssfs$stops |> as.data.frame() |> select(stop_id, stop_name)

          ljaune_ssfs$stop_seq <-
            ljaune_ssfs$stop_seq |>
            left_join(stop_id_to_stopname, by = "stop_id")

          ljaune_ssfs$itin <-
            ljaune_ssfs$itin |>
            st_transform(4326)

          ljaune_ssfs$stops <-
            ljaune_ssfs$stops |>
            st_transform(4326)

          ssfs(ljaune_ssfs)

          #update map center

          map_center(list(lng = -73.567, lat = 45.5017)) # Montreal default

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
          url <- "https://github.com/julian-city/gtfsforge/raw/refs/heads/main/sample_networks/metro_v2.rds"

          # Create a temporary file
          temp_file <- tempfile(fileext = ".rds")

          # Download the file
          download.file(url, temp_file, mode = "wb")

          # Load the file
          mtlmetro_ssfs <- readRDS(temp_file)

          # Clean up
          unlink(temp_file)

          #adjust ssfs to data structure (stop names in stop seq,
          #FOR 3.0 NO SPEED FACTOR, 3.1 will incorporate this)

          stop_id_to_stopname <-
            mtlmetro_ssfs$stops |>
            as.data.frame() |>
            select(stop_id, stop_name)

          mtlmetro_ssfs$stop_seq <-
            mtlmetro_ssfs$stop_seq |>
            left_join(stop_id_to_stopname, by = "stop_id")

          mtlmetro_ssfs$itin <-
            mtlmetro_ssfs$itin |>
            st_transform(4326)

          mtlmetro_ssfs$stops <-
            mtlmetro_ssfs$stops |>
            st_transform(4326)

          ssfs(mtlmetro_ssfs)

          #update map center

          map_center(list(lng = -73.567, lat = 45.5017)) # Montreal default

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
          url <- "https://github.com/julian-city/gtfsforge/raw/refs/heads/main/sample_networks/mileend_v2.rds"

          # Create a temporary file
          temp_file <- tempfile(fileext = ".rds")

          # Download the file
          download.file(url, temp_file, mode = "wb")

          # Load the file
          mtlmileend_ssfs <- readRDS(temp_file)

          # Clean up
          unlink(temp_file)

          #adjust ssfs to data structure (stop names in stop seq,
          #FOR 3.0 NO SPEED FACTOR, 3.1 will incorporate this)

          stop_id_to_stopname <-
            mtlmileend_ssfs$stops |>
            as.data.frame() |>
            select(stop_id, stop_name)

          mtlmileend_ssfs$stop_seq <-
            mtlmileend_ssfs$stop_seq |>
            left_join(stop_id_to_stopname, by = "stop_id")

          mtlmileend_ssfs$itin <-
            mtlmileend_ssfs$itin |>
            st_transform(4326)

          mtlmileend_ssfs$stops <-
            mtlmileend_ssfs$stops |>
            st_transform(4326)

          ssfs(mtlmileend_ssfs)

          #update map center

          map_center(list(lng = -73.567, lat = 45.5017)) # Montreal default

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
          url <- "https://github.com/julian-city/gtfsforge/raw/refs/heads/main/sample_networks/ttcsubway_v2.rds"

          # Create a temporary file
          temp_file <- tempfile(fileext = ".rds")

          # Download the file
          download.file(url, temp_file, mode = "wb")

          # Load the file
          ttcsubway_ssfs <- readRDS(temp_file)

          # Clean up
          unlink(temp_file)

          #adjust ssfs to data structure (stop names in stop seq,
          #FOR 3.0 NO SPEED FACTOR, 3.1 will incorporate this)

          stop_id_to_stopname <-
            ttcsubway_ssfs$stops |>
            as.data.frame() |>
            select(stop_id, stop_name)

          ttcsubway_ssfs$stop_seq <-
            ttcsubway_ssfs$stop_seq |>
            left_join(stop_id_to_stopname, by = "stop_id")

          ttcsubway_ssfs$itin <-
            ttcsubway_ssfs$itin |>
            st_transform(4326)

          ttcsubway_ssfs$stops <-
            ttcsubway_ssfs$stops |>
            st_transform(4326)

          ssfs(ttcsubway_ssfs)

          #update <- of map

          bbox <- st_bbox(ttcsubway_ssfs$stops)

          # Calculate center point
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
      if (!is.null(cities_data()) && nrow(cities_data()) > 0) {
        search_term <- input$city_search

        if (nchar(search_term) >= 2) {
          # Filter cities that match the search term (case insensitive)
          #matches <- cities_data()[grepl(search_term, cities_data()$name, ignore.case = TRUE), ]

          matches <- cities_data() |>
            filter(str_detect(tolower(name), tolower(str_escape(search_term))))

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

      if (is.null(cities_data()) || nrow(cities_data()) == 0) {
        showNotification("Cities database not loaded", type = "error")
        return()
      }

      # Find exact matches (case insensitive)
      exact_matches <- cities_data()[
        tolower(cities_data()$name) == tolower(search_term),
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
          selection = 'single',
          rownames = FALSE,
          options = list(
            pageLength = 10,
            ordering = FALSE,
            dom = 't'
          )
        ))
      }

      DT::datatable(
        current_data$agency,
        selection = 'single',
        rownames = FALSE,
        options = list(
          pageLength = 10,
          ordering = FALSE,
          dom = 't'
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

    # Reactive values for stops editing state
    stops_temp_point <- reactiveVal(NULL)
    stops_editing_id <- reactiveVal(NULL)
    stops_adding_new <- reactiveVal(FALSE)
    stops_search_term <- reactiveVal("")

    # Temporary storage for edit field values
    stops_edit_stop_id <- reactiveVal("")
    stops_edit_stop_name <- reactiveVal("")

    # Handle stop search input
    observeEvent(
      input$stop_search_term,
      {
        stops_search_term(input$stop_search_term)
      },
      ignoreNULL = FALSE
    )

    # Initialize stops map
    output$stops_map <- leaflet::renderLeaflet({
      center <- map_center()
      leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE)) |>
        addBaseMaps() |>
        leaflet::setView(lng = center$lng, lat = center$lat, zoom = 12) |>
        htmlwidgets::onRender(
          "
      function(el, x) {
        this.on('zoomend', function(e) {
          Shiny.setInputValue('stops_map_zoom', this.getZoom());
        });
      }
    "
        )
    })

    # Observer for level of zoom on stops map
    observeEvent(input$stops_map_zoom, {
      current_zoom(input$stops_map_zoom)
    })

    # Update stops map content
    observe({
      current_data <- ssfs()
      temp <- stops_temp_point()
      editing_id <- stops_editing_id()

      proxy <- leaflet::leafletProxy("stops_map") |>
        leaflet::clearMarkers() |>
        leaflet::clearShapes()

      # Add shapes if they exist
      if (nrow(current_data$itin) > 0) {
        for (i in 1:nrow(current_data$itin)) {
          line_coords <- st_coordinates(current_data$itin$geometry[i])

          # Get route_color from routes table based on route_id
          route_id_i <- current_data$itin$route_id[i]
          route_color_i <- current_data$routes$route_color[
            current_data$routes$route_id == route_id_i
          ]

          # Use route color if found, otherwise fallback to default
          line_color <- if (
            length(route_color_i) > 0 &&
              !is.na(route_color_i[1]) &&
              nchar(route_color_i[1]) > 0
          ) {
            paste0("#", route_color_i[1])
          } else {
            "#92C5DE"
          }

          proxy <- proxy |>
            leaflet::addPolylines(
              lng = line_coords[, 1],
              lat = line_coords[, 2],
              group = "shapes",
              color = line_color,
              weight = 3,
              opacity = 0.5
            )
        }
      }

      # Add stops (excluding the one being edited)
      if (nrow(current_data$stops) > 0) {
        stops_to_show <- current_data$stops
        if (!is.null(editing_id)) {
          stops_to_show <- stops_to_show[stops_to_show$stop_id != editing_id, ]
        }

        if (nrow(stops_to_show) > 0) {
          # Build hover labels with stop info and associated itin_ids
          stop_itin_lookup <- current_data$stop_seq |>
            group_by(stop_id) |>
            summarise(
              itin_ids = paste(unique(itin_id), collapse = ", "),
              .groups = "drop"
            )

          label_data <- merge(
            as.data.frame(stops_to_show)[, "stop_id", drop = FALSE],
            stop_itin_lookup,
            by = "stop_id",
            all.x = TRUE
          )

          hover_labels <- lapply(seq_len(nrow(stops_to_show)), function(i) {
            sid <- stops_to_show$stop_id[i]
            sname <- stops_to_show$stop_name[i]
            itins <- label_data$itin_ids[label_data$stop_id == sid]
            itin_text <- if (is.na(itins) || length(itins) == 0) {
              "None"
            } else {
              itins
            }
            htmltools::HTML(paste0(
              "<span style='font-size:11px;'>",
              "<b>",
              htmltools::htmlEscape(sid),
              "</b> \u2014 ",
              htmltools::htmlEscape(sname),
              "<br>Itineraries: ",
              htmltools::htmlEscape(itin_text),
              "</span>"
            ))
          })

          proxy <- proxy |>
            leaflet::addCircleMarkers(
              data = stops_to_show,
              layerId = ~stop_id,
              color = "white",
              weight = 1,
              stroke = TRUE,
              fillColor = "#7f7f7f",
              fillOpacity = 0.7,
              radius = calculateMarkerSize(current_zoom()),
              label = hover_labels,
              labelOptions = leaflet::labelOptions(
                style = list("font-size" = "11px", "padding" = "3px 6px"),
                direction = "top",
                offset = c(0, -8)
              )
            )
        }
      }

      # Add temporary point (red draggable marker) if in editing mode
      if (!is.null(temp)) {
        # Calculate icon size based on zoom
        icon_size <- as.integer((calculateMarkerSize(current_zoom()) + 2) * 2)

        # Create SVG circle as data URI
        svg_string <- sprintf(
          '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d"><circle cx="%d" cy="%d" r="%d" fill="#B2182B" stroke="white" stroke-width="2"/></svg>',
          icon_size,
          icon_size,
          as.integer(icon_size / 2),
          as.integer(icon_size / 2),
          as.integer((icon_size / 2) - 2)
        )

        icon_url <- paste0(
          "data:image/svg+xml,",
          URLencode(svg_string, reserved = TRUE)
        )

        red_circle_icon <- leaflet::makeIcon(
          iconUrl = icon_url,
          iconWidth = icon_size,
          iconHeight = icon_size,
          iconAnchorX = as.integer(icon_size / 2),
          iconAnchorY = as.integer(icon_size / 2)
        )

        proxy |>
          leaflet::addMarkers(
            lng = temp[1],
            lat = temp[2],
            layerId = "temp_drag",
            icon = red_circle_icon,
            options = leaflet::markerOptions(draggable = TRUE)
          )
      }
    })

    # Render editing instruction
    output$stops_editing_instruction <- renderUI({
      if (!is.null(stops_editing_id()) || stops_adding_new()) {
        if (is.null(stops_temp_point())) {
          div(
            class = "editing-instruction",
            "Click on the map to place the stop"
          )
        } else {
          div(
            class = "editing-instruction",
            "Drag the marker to adjust position"
          )
        }
      } else {
        NULL
      }
    })

    # Render stop list UI
    output$stop_list_ui <- renderUI({
      current_data <- ssfs()
      editing_id <- stops_editing_id()
      adding_new <- stops_adding_new()
      edit_id_val <- stops_edit_stop_id()
      edit_name_val <- stops_edit_stop_name()
      search_term <- stops_search_term()

      rows <- list()

      # FIRST: Add new stop row OR add form at the top (always visible)
      if (adding_new) {
        # Show the add form at the top (same style as route edit form)
        rows[[length(rows) + 1]] <- div(
          class = "route-edit-form",
          tags$label(
            "Stop ID",
            info_popover(
              "Unique identifier for a stop, station or platform.",
              "https://gtfs.org/schedule/reference/#stopstxt"
            )
          ),
          tags$input(
            type = "text",
            id = "inline_stop_id",
            placeholder = "e.g., S001",
            value = edit_id_val
          ),
          tags$label(
            "Stop name",
            info_popover(
              "Name of the stop, station or platform. It should match the agency's rider-facing name for the location as printed on a timetable, published online, or represented on signage.",
              "https://gtfs.org/schedule/reference/#stopstxt"
            )
          ),
          tags$input(
            type = "text",
            id = "inline_stop_name",
            placeholder = "e.g., Main St Station",
            value = edit_name_val
          ),
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
      } else {
        # Show the "Add new stop" button at the top
        rows[[length(rows) + 1]] <- div(
          class = "stop-list-row add-row",
          onclick = "startAddingStop()",
          tags$button(
            class = "stop-action-btn add-btn",
            onclick = "event.stopPropagation(); startAddingStop()",
            title = "Add new stop",
            htmltools::HTML("+")
          ),
          span(style = "margin-left: 8px;", "Add new stop")
        )
      }

      # SECOND: If editing an existing stop, render its edit form immediately (always visible)
      if (!is.null(editing_id) && !adding_new) {
        rows[[length(rows) + 1]] <- div(
          class = "route-edit-form",
          tags$label(
            "Stop ID",
            info_popover(
              "Unique identifier for a stop, station or platform.",
              "https://gtfs.org/schedule/reference/#stopstxt"
            )
          ),
          tags$input(type = "text", id = "inline_stop_id", value = edit_id_val),
          tags$label(
            "Stop name",
            info_popover(
              "Name of the stop, station or platform. It should match the agency's rider-facing name for the location as printed on a timetable, published online, or represented on signage.",
              "https://gtfs.org/schedule/reference/#stopstxt"
            )
          ),
          tags$input(
            type = "text",
            id = "inline_stop_name",
            value = edit_name_val
          ),
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

      # THEN: Add existing stops below (filtered by search term, excluding editing stop)
      if (nrow(current_data$stops) > 0) {
        stops_df <- current_data$stops |> as.data.frame()

        # Filter stops if search term is provided
        if (!is.null(search_term) && search_term != "") {
          search_lower <- tolower(search_term)
          stops_df <- stops_df |>
            filter(
              grepl(search_lower, tolower(stop_name), fixed = TRUE) |
                grepl(search_lower, tolower(stop_id), fixed = TRUE)
            )
        }

        # Exclude the stop being edited (it's already rendered at the top)
        if (!is.null(editing_id)) {
          stops_df <- stops_df |> filter(stop_id != editing_id)
        }

        if (nrow(stops_df) > 0) {
          for (i in 1:nrow(stops_df)) {
            stop <- stops_df[i, ]

            # All stops here are non-editing (editing stop is rendered above)
            rows[[length(rows) + 1]] <- div(
              class = "stop-list-row",
              onclick = sprintf("viewStopFromList('%s')", stop$stop_id),
              div(
                class = "stop-info",
                div(
                  class = "stop-info-display",
                  span(class = "stop-name", stop$stop_name),
                  span(
                    class = "stop-id-display",
                    paste0("(", stop$stop_id, ")")
                  )
                )
              ),
              div(
                class = "stop-actions",
                tags$button(
                  class = "stop-action-btn edit-btn",
                  onclick = sprintf(
                    "event.stopPropagation(); editStopFromList('%s')",
                    stop$stop_id
                  ),
                  title = "Edit",
                  htmltools::HTML("&#9998;")
                ),
                tags$button(
                  class = "stop-action-btn delete-btn",
                  onclick = sprintf(
                    "event.stopPropagation(); deleteStopFromList('%s')",
                    stop$stop_id
                  ),
                  title = "Delete stop",
                  htmltools::HTML('<i class="fa-solid fa-trash"></i>')
                )
              )
            )
          }
        } else if (
          !is.null(search_term) && search_term != "" && is.null(editing_id)
        ) {
          # Show "no results" message only when search returns nothing AND not editing
          rows[[length(rows) + 1]] <- div(
            class = "stop-list-row",
            style = "justify-content: center; color: #888; font-style: italic;",
            "No stops match your search"
          )
        }
      }

      do.call(tagList, rows)
    })

    # Handle "Add new stop" click
    observeEvent(input$stop_list_add_click, {
      stops_editing_id(NULL)
      stops_temp_point(NULL)
      stops_adding_new(TRUE)
      stops_edit_stop_id("")
      stops_edit_stop_name("")
    })

    # Handle "Edit stop" click from list
    observeEvent(input$stop_list_edit_click, {
      stop_id <- input$stop_list_edit_click
      current_data <- ssfs()

      selected_stop <- current_data$stops[
        current_data$stops$stop_id == stop_id,
      ]
      if (nrow(selected_stop) > 0) {
        point_lng <- st_coordinates(selected_stop$geometry)[[1]]
        point_lat <- st_coordinates(selected_stop$geometry)[[2]]

        stops_adding_new(FALSE)
        stops_editing_id(stop_id)
        stops_temp_point(c(point_lng, point_lat))
        stops_edit_stop_id(selected_stop$stop_id)
        stops_edit_stop_name(selected_stop$stop_name)

        # Center map on the stop with good zoom level
        edit_zoom <- max(current_zoom(), 16) # At least zoom 16 for editing
        leaflet::leafletProxy("stops_map") |>
          leaflet::setView(lng = point_lng, lat = point_lat, zoom = edit_zoom)
      }
    })

    # Handle "View stop" click from list row (just centers map, no edit mode)
    observeEvent(input$stop_list_view_click, {
      stop_id <- input$stop_list_view_click
      current_data <- ssfs()

      selected_stop <- current_data$stops[
        current_data$stops$stop_id == stop_id,
      ]
      if (nrow(selected_stop) > 0) {
        point_lng <- st_coordinates(selected_stop$geometry)[[1]]
        point_lat <- st_coordinates(selected_stop$geometry)[[2]]

        # Center map on the stop with good zoom level (but don't enter edit mode)
        view_zoom <- max(current_zoom(), 16)
        leaflet::leafletProxy("stops_map") |>
          leaflet::setView(lng = point_lng, lat = point_lat, zoom = view_zoom)
      }
    })

    # Handle map clicks
    observeEvent(input$stops_map_click, {
      click <- input$stops_map_click

      if (!is.null(stops_editing_id()) || stops_adding_new()) {
        stops_temp_point(c(click$lng, click$lat))
      }
    })

    # Handle drag end for the temporary stop marker
    observeEvent(input$stops_map_marker_dragend, {
      drag_event <- input$stops_map_marker_dragend

      # Only respond to drags on the temp_drag marker
      if (!is.null(drag_event$id) && drag_event$id == "temp_drag") {
        stops_temp_point(c(drag_event$lng, drag_event$lat))
      }
    })

    # Handle existing stop marker clicks (enters edit mode)
    observeEvent(input$stops_map_marker_click, {
      click <- input$stops_map_marker_click

      if (!is.null(click$id) && click$id != "temp") {
        current_data <- ssfs()
        selected_stop <- current_data$stops[
          current_data$stops$stop_id == click$id,
        ]

        if (nrow(selected_stop) > 0) {
          point_lng <- st_coordinates(selected_stop$geometry)[[1]]
          point_lat <- st_coordinates(selected_stop$geometry)[[2]]

          stops_adding_new(FALSE)
          stops_editing_id(click$id)
          stops_temp_point(c(point_lng, point_lat))
          stops_edit_stop_id(selected_stop$stop_id)
          stops_edit_stop_name(selected_stop$stop_name)

          # Center map on the stop with good zoom level
          edit_zoom <- max(current_zoom(), 16)
          leaflet::leafletProxy("stops_map") |>
            leaflet::setView(lng = point_lng, lat = point_lat, zoom = edit_zoom)

          # Scroll stop list to top so editing stop is visible
          shinyjs::runjs(
            "document.querySelector('.stop-list-container').scrollTop = 0;"
          )
        }
      }
    })

    # Handle save click - receives data directly from JavaScript
    observeEvent(input$stop_list_save_data, {
      temp <- stops_temp_point()

      # Get values directly from the JavaScript payload
      save_data <- input$stop_list_save_data
      stop_id_val <- trimws(save_data$stop_id)
      stop_name_val <- trimws(save_data$stop_name)

      if (is.null(temp)) {
        showNotification(
          "Please click on the map to place the stop",
          type = "warning"
        )
        return()
      }

      if (is.null(stop_id_val) || stop_id_val == "") {
        showNotification("Please enter a Stop ID", type = "warning")
        return()
      }

      if (is.null(stop_name_val) || stop_name_val == "") {
        showNotification("Please enter a Stop Name", type = "warning")
        return()
      }

      current_data <- ssfs()
      editing_id <- stops_editing_id()
      adding_new <- stops_adding_new()

      # Check for duplicate stop_id
      if (adding_new || (!is.null(editing_id) && stop_id_val != editing_id)) {
        if (stop_id_val %in% current_data$stops$stop_id) {
          showNotification("A stop with this ID already exists", type = "error")
          return()
        }
      }

      new_stop <- st_sf(
        stop_id = stop_id_val,
        stop_name = stop_name_val,
        geometry = st_sfc(st_point(c(temp[1], temp[2])), crs = 4326),
        stringsAsFactors = FALSE
      )

      if (adding_new) {
        current_data$stops <- rbind(current_data$stops, new_stop)
        showNotification(paste("Stop", stop_id_val, "added"), type = "message")
      } else if (!is.null(editing_id)) {
        current_data$stops <- current_data$stops[
          current_data$stops$stop_id != editing_id,
        ]
        current_data$stops <- rbind(current_data$stops, new_stop)

        # Update stop_seq references if stop_id changed
        if (editing_id != stop_id_val) {
          current_data$stop_seq$stop_id[
            current_data$stop_seq$stop_id == editing_id
          ] <- stop_id_val
          if ("stop_name" %in% names(current_data$stop_seq)) {
            current_data$stop_seq$stop_name[
              current_data$stop_seq$stop_id == stop_id_val
            ] <- stop_name_val
          }
        } else if ("stop_name" %in% names(current_data$stop_seq)) {
          current_data$stop_seq$stop_name[
            current_data$stop_seq$stop_id == stop_id_val
          ] <- stop_name_val
        }

        showNotification(
          paste("Stop", stop_id_val, "updated"),
          type = "message"
        )
      }

      ssfs(current_data)
      stops_editing_id(NULL)
      stops_adding_new(FALSE)
      stops_temp_point(NULL)
      stops_edit_stop_id("")
      stops_edit_stop_name("")
    })

    # Handle cancel click
    observeEvent(input$stop_list_cancel_click, {
      stops_editing_id(NULL)
      stops_adding_new(FALSE)
      stops_temp_point(NULL)
      stops_edit_stop_id("")
      stops_edit_stop_name("")
    })

    # Handle stop deletion
    observeEvent(input$stop_list_delete_click, {
      stop_to_delete <- input$stop_list_delete_click$id
      current_data <- ssfs()

      # Check if stop is referenced in stop_seq
      if (
        nrow(current_data$stop_seq) > 0 &&
          stop_to_delete %in% current_data$stop_seq$stop_id
      ) {
        associated_itins <- paste(
          unique(current_data$stop_seq$itin_id[
            current_data$stop_seq$stop_id == stop_to_delete
          ]),
          collapse = ", "
        )
        showNotification(
          paste0(
            "Cannot delete stop '",
            stop_to_delete,
            "'. It is used in itineraries: ",
            associated_itins,
            ". Remove it from those itineraries first."
          ),
          type = "error",
          duration = 5
        )
        return()
      }

      current_data$stops <- current_data$stops[
        current_data$stops$stop_id != stop_to_delete,
      ]
      ssfs(current_data)

      # Clear editing state if this stop was being edited
      if (
        !is.null(stops_editing_id()) && stops_editing_id() == stop_to_delete
      ) {
        stops_editing_id(NULL)
        stops_temp_point(NULL)
        stops_edit_stop_id("")
        stops_edit_stop_name("")
      }

      showNotification("Stop deleted successfully", type = "message")
    })

    ###STOPS IMPORT/EXPORT FUNCTIONALITY ----

    # Handle stops import
    observeEvent(input$stops_import_confirm, {
      req(input$stops_import_file)

      file_path <- input$stops_import_file$datapath
      file_name <- input$stops_import_file$name
      file_ext <- tolower(tools::file_ext(file_name))

      # Validate file extension
      if (!file_ext %in% c("geojson", "kml")) {
        showNotification(
          "Invalid file format. Please upload a GeoJSON or KML file.",
          type = "error"
        )
        return()
      }

      tryCatch(
        {
          # Read the file
          imported_sf <- st_read(file_path, quiet = TRUE)

          # Transform to CRS 4326
          imported_sf <- st_transform(imported_sf, 4326)

          # Check for point geometries only
          geom_types <- unique(st_geometry_type(imported_sf))
          if (!all(geom_types %in% c("POINT", "MULTIPOINT"))) {
            showNotification(
              "Only point geometries can be imported.",
              type = "error"
            )
            return()
          }

          # Handle MULTIPOINT by converting to POINT (take first point)
          if ("MULTIPOINT" %in% geom_types) {
            imported_sf <- st_cast(imported_sf, "POINT")
          }

          # Check row count limit
          if (nrow(imported_sf) > 99999) {
            showNotification(
              "File contains more than 99,999 features. Please reduce the file size.",
              type = "error"
            )
            return()
          }

          # Get column names (excluding geometry)
          col_names <- setdiff(names(imported_sf), "geometry")
          imported_df <- as.data.frame(imported_sf)

          # ---- IDENTIFY stop_id ----
          # Look for a unique ID field with no spaces
          stop_id_col <- NULL

          for (col in col_names) {
            col_values <- imported_df[[col]]
            # Check if column has no spaces in values and all unique
            if (
              is.character(col_values) ||
                is.numeric(col_values) ||
                is.integer(col_values)
            ) {
              col_as_char <- as.character(col_values)
              # Check for no spaces and uniqueness
              has_no_spaces <- !any(grepl("\\s", col_as_char, perl = TRUE))
              is_unique <- length(unique(col_as_char)) == length(col_as_char)

              if (has_no_spaces && is_unique && length(col_as_char) > 0) {
                stop_id_col <- col
                break
              }
            }
          }

          # If no suitable column found, generate stop_ids
          if (is.null(stop_id_col)) {
            imported_sf$stop_id <- sprintf("%05d", seq_len(nrow(imported_sf)))
          } else {
            imported_sf$stop_id <- as.character(imported_df[[stop_id_col]])
          }

          # ---- IDENTIFY stop_name ----
          # Look for another character field (not the one used for stop_id)
          stop_name_col <- NULL

          for (col in col_names) {
            if (!is.null(stop_id_col) && col == stop_id_col) {
              next
            }

            col_values <- imported_df[[col]]
            if (is.character(col_values)) {
              stop_name_col <- col
              break
            }
          }

          # Set stop_name
          if (is.null(stop_name_col)) {
            imported_sf$stop_name <- imported_sf$stop_id
          } else {
            imported_sf$stop_name <- as.character(imported_df[[stop_name_col]])
          }

          # Select only required columns
          imported_stops <- imported_sf |>
            select(stop_id, stop_name, geometry)

          # Check for duplicates with existing stops
          current_data <- ssfs()
          existing_ids <- current_data$stops$stop_id
          incoming_ids <- imported_stops$stop_id

          duplicate_ids <- intersect(existing_ids, incoming_ids)

          if (length(duplicate_ids) > 0) {
            # Filter out duplicates
            imported_stops <- imported_stops |>
              filter(!stop_id %in% duplicate_ids)

            if (nrow(imported_stops) == 0) {
              showNotification(
                paste(
                  "All",
                  length(duplicate_ids),
                  "stops have duplicate IDs and were not imported."
                ),
                type = "warning"
              )
              return()
            }

            showNotification(
              paste(
                length(duplicate_ids),
                "stops with duplicate IDs were skipped."
              ),
              type = "warning"
            )
          }

          # Append to existing stops
          current_data$stops <- rbind(current_data$stops, imported_stops)
          ssfs(current_data)

          showNotification(
            paste("Successfully imported", nrow(imported_stops), "stops."),
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Error importing file:", e$message),
            type = "error"
          )
        }
      )
    })

    # Handle stops export/download
    output$stops_export_download <- downloadHandler(
      filename = function() {
        format <- input$stops_export_format
        base_name <- "stops"

        switch(
          format,
          "geojson" = paste0(base_name, ".geojson"),
          "kml" = paste0(base_name, ".kml"),
          "shp" = paste0(base_name, ".zip")
        )
      },
      content = function(file) {
        current_data <- ssfs()

        if (nrow(current_data$stops) == 0) {
          showNotification("No stops to export.", type = "warning")
          return()
        }

        format <- input$stops_export_format

        tryCatch(
          {
            if (format == "shp") {
              # Shapefile requires writing to a directory, then zipping
              temp_dir <- tempdir()
              shp_base <- file.path(temp_dir, "stops")

              # Write shapefile (creates .shp, .shx, .dbf, .prj files)
              st_write(
                current_data$stops,
                paste0(shp_base, ".shp"),
                delete_layer = TRUE,
                quiet = TRUE
              )

              # Get all shapefile component files
              shp_files <- list.files(
                temp_dir,
                pattern = "^stops\\.",
                full.names = TRUE
              )

              # Create zip file
              zip(file, files = shp_files, flags = "-j")
            } else if (format == "kml") {
              # KML requires special handling for Name and Description fields
              kml_stops <- current_data$stops |>
                mutate(
                  Name = stop_name,
                  Description = stop_id
                ) |>
                select(Name, Description, geometry)

              st_write(
                kml_stops,
                file,
                driver = "KML",
                delete_dsn = TRUE,
                quiet = TRUE
              )
            } else {
              # GeoJSON - direct write
              st_write(
                current_data$stops,
                file,
                driver = "GeoJSON",
                delete_dsn = TRUE,
                quiet = TRUE
              )
            }
          },
          error = function(e) {
            showNotification(
              paste("Error exporting file:", e$message),
              type = "error"
            )
          }
        )
      }
    )

    #   #   #
    #
    ##   ROUTES MODULE--------
    #
    #   #   #

    # Reactive values for routes list panel
    routes_editing_id <- reactiveVal(NULL) # route_id being edited (inline form)
    routes_adding_new <- reactiveVal(FALSE) # TRUE when adding a new route
    routes_expanded_id <- reactiveVal(NULL) # route_id whose itineraries are expanded

    # Reactive values replacing old selectInput/textInput for itinerary details
    active_route_id <- reactiveVal(NULL) # replaces input$route_id
    active_direction_id <- reactiveVal(0L) # replaces input$direction_id
    active_trip_headsign <- reactiveVal("") # replaces input$trip_headsign

    # Reactive values for inline itinerary editing
    itin_editing_id <- reactiveVal(NULL) # itin_id being edited in panel
    itin_adding_for_route <- reactiveVal(NULL) # route_id for which we're adding a new itin

    # Render editing instruction for route itinerary drawing
    output$routes_editing_instruction <- renderUI({
      is_editing <- !is.null(active_itin_id()) &&
        (editing_existing_itin() || !is.null(itin_adding_for_route()))
      if (is_editing) {
        itin_id_display <- active_itin_id()
        div(
          class = "editing-instruction",
          paste0("Editing: ", itin_id_display),
          tags$br(),
          tags$small("Click stops to build sequence. Right-click to remove.")
        )
      } else {
        NULL
      }
    })

    # Render the routes list UI
    output$route_list_ui <- renderUI({
      current_data <- ssfs()
      editing_route_id <- routes_editing_id()
      adding_new <- routes_adding_new()
      expanded_route <- routes_expanded_id()
      current_active_itin <- active_itin_id()
      editing_itin <- itin_editing_id()
      adding_itin_route <- itin_adding_for_route()

      rows <- list()

      # "Add new route" button / form at the top
      if (adding_new) {
        # Inline add form
        agency_options <- ""
        if (nrow(current_data$agency) > 0) {
          agency_options <- paste0(
            sapply(1:nrow(current_data$agency), function(i) {
              paste0(
                '<option value="',
                current_data$agency$agency_id[i],
                '">',
                current_data$agency$agency_id[i],
                '</option>'
              )
            }),
            collapse = ""
          )
        }

        route_type_options <- paste0(
          '<option value="3" selected>Bus</option>',
          '<option value="0">Tram</option>',
          '<option value="1">Metro</option>',
          '<option value="2">Rail</option>',
          '<option value="4">Ferry</option>',
          '<option value="5">Cable tram</option>',
          '<option value="6">Gondola</option>',
          '<option value="7">Funicular</option>',
          '<option value="11">Trolleybus</option>',
          '<option value="12">Monorail</option>'
        )

        rows[[length(rows) + 1]] <- div(
          class = "route-edit-form",
          tags$label(
            "Route ID",
            info_popover(
              "Unique identifier for route.",
              "https://gtfs.org/schedule/reference/#routestxt"
            )
          ),
          tags$input(
            type = "text",
            id = "inline_route_id",
            placeholder = "e.g., 14"
          ),
          tags$label(
            "Agency",
            info_popover(
              "Agency for specified route.",
              "https://gtfs.org/schedule/reference/#routestxt"
            )
          ),
          htmltools::HTML(paste0(
            '<select id="inline_agency_id">',
            agency_options,
            '</select>'
          )),
          tags$label(
            "Short name",
            info_popover(
              "Short name of a route. Often a short, abstract identifier (e.g., '32', '100X', 'Green') that riders use to identify a route.",
              "https://gtfs.org/schedule/reference/#routestxt"
            )
          ),
          tags$input(
            type = "text",
            id = "inline_route_short_name",
            placeholder = "e.g., 14"
          ),
          tags$label(
            "Long name",
            info_popover(
              "Full name of a route. This name is generally more descriptive than the route_short_name and often includes the route's destination or stop.",
              "https://gtfs.org/schedule/reference/#routestxt"
            )
          ),
          tags$input(
            type = "text",
            id = "inline_route_long_name",
            placeholder = "e.g., Hastings / UBC"
          ),
          tags$label(
            "Route type",
            info_popover(
              "Indicates the type of transportation used on a route.",
              "https://gtfs.org/schedule/reference/#routestxt"
            )
          ),
          htmltools::HTML(paste0(
            '<select id="inline_route_type">',
            route_type_options,
            '</select>'
          )),
          tags$label(
            "Route colour",
            info_popover(
              "Route colour designation that matches public facing material.",
              "https://gtfs.org/schedule/reference/#routestxt"
            )
          ),
          tags$input(
            type = "color",
            id = "inline_route_color",
            value = "#92C5DE",
            style = "height: 30px; padding: 2px;"
          ),
          tags$label(
            "Text colour",
            info_popover(
              "Legible color to use for text drawn against a background of route_color.",
              "https://gtfs.org/schedule/reference/#routestxt"
            )
          ),
          tags$input(
            type = "color",
            id = "inline_route_text_color",
            value = "#000000",
            style = "height: 30px; padding: 2px;"
          ),
          div(
            class = "btn-row",
            tags$button(
              class = "btn-save",
              onclick = "saveRouteFromForm()",
              htmltools::HTML("&#10003; Save")
            ),
            tags$button(
              class = "btn-cancel",
              onclick = "cancelRouteEdit()",
              "Cancel"
            )
          )
        )
      } else {
        rows[[length(rows) + 1]] <- div(
          class = "stop-list-row add-row",
          onclick = "startAddingRoute()",
          tags$button(
            class = "stop-action-btn add-btn",
            onclick = "event.stopPropagation(); startAddingRoute()",
            title = "Add new route",
            htmltools::HTML("+")
          ),
          span(style = "margin-left: 8px;", "Add new route")
        )
      }

      # Route rows
      if (nrow(current_data$routes) > 0) {
        for (i in 1:nrow(current_data$routes)) {
          route <- current_data$routes[i, ]
          is_expanded <- !is.null(expanded_route) &&
            expanded_route == route$route_id
          is_editing <- !is.null(editing_route_id) &&
            editing_route_id == route$route_id

          if (is_editing) {
            # Inline edit form for this route
            agency_options_edit <- ""
            if (nrow(current_data$agency) > 0) {
              agency_options_edit <- paste0(
                sapply(1:nrow(current_data$agency), function(j) {
                  sel <- if (
                    current_data$agency$agency_id[j] == route$agency_id
                  ) {
                    ' selected'
                  } else {
                    ''
                  }
                  paste0(
                    '<option value="',
                    current_data$agency$agency_id[j],
                    '"',
                    sel,
                    '>',
                    current_data$agency$agency_id[j],
                    '</option>'
                  )
                }),
                collapse = ""
              )
            }

            route_types <- c(
              "3" = "Bus",
              "0" = "Tram",
              "1" = "Metro",
              "2" = "Rail",
              "4" = "Ferry",
              "5" = "Cable tram",
              "6" = "Gondola",
              "7" = "Funicular",
              "11" = "Trolleybus",
              "12" = "Monorail"
            )
            rt_options <- paste0(
              sapply(names(route_types), function(val) {
                sel <- if (as.character(route$route_type) == val) {
                  ' selected'
                } else {
                  ''
                }
                paste0(
                  '<option value="',
                  val,
                  '"',
                  sel,
                  '>',
                  route_types[val],
                  '</option>'
                )
              }),
              collapse = ""
            )

            rows[[length(rows) + 1]] <- div(
              class = "route-edit-form",
              tags$label(
                "Route ID",
                info_popover(
                  "Unique identifier for route.",
                  "https://gtfs.org/schedule/reference/#routestxt"
                )
              ),
              tags$input(
                type = "text",
                id = "inline_route_id",
                value = route$route_id
              ),
              tags$label(
                "Agency",
                info_popover(
                  "Agency for specified route.",
                  "https://gtfs.org/schedule/reference/#routestxt"
                )
              ),
              htmltools::HTML(paste0(
                '<select id="inline_agency_id">',
                agency_options_edit,
                '</select>'
              )),
              tags$label(
                "Short name",
                info_popover(
                  "Short name of a route. Often a short, abstract identifier (e.g., '32', '100X', 'Green') that riders use to identify a route.",
                  "https://gtfs.org/schedule/reference/#routestxt"
                )
              ),
              tags$input(
                type = "text",
                id = "inline_route_short_name",
                value = route$route_short_name
              ),
              tags$label(
                "Long name",
                info_popover(
                  "Full name of a route. This name is generally more descriptive than the route_short_name and often includes the route's destination or stop.",
                  "https://gtfs.org/schedule/reference/#routestxt"
                )
              ),
              tags$input(
                type = "text",
                id = "inline_route_long_name",
                value = route$route_long_name
              ),
              tags$label(
                "Route type",
                info_popover(
                  "Indicates the type of transportation used on a route.",
                  "https://gtfs.org/schedule/reference/#routestxt"
                )
              ),
              htmltools::HTML(paste0(
                '<select id="inline_route_type">',
                rt_options,
                '</select>'
              )),
              tags$label(
                "Route colour",
                info_popover(
                  "Route colour designation that matches public facing material.",
                  "https://gtfs.org/schedule/reference/#routestxt"
                )
              ),
              tags$input(
                type = "color",
                id = "inline_route_color",
                value = paste0("#", route$route_color),
                style = "height: 30px; padding: 2px;"
              ),
              tags$label(
                "Text colour",
                info_popover(
                  "Legible color to use for text drawn against a background of route_color.",
                  "https://gtfs.org/schedule/reference/#routestxt"
                )
              ),
              tags$input(
                type = "color",
                id = "inline_route_text_color",
                value = paste0("#", route$route_text_color),
                style = "height: 30px; padding: 2px;"
              ),
              div(
                class = "btn-row",
                tags$button(
                  class = "btn-save",
                  onclick = "saveRouteFromForm()",
                  "Save"
                ),
                tags$button(
                  class = "btn-cancel",
                  onclick = "cancelRouteEdit()",
                  "Cancel"
                )
              )
            )
          } else {
            # Normal route row
            expand_icon <- if (is_expanded) {
              htmltools::HTML("&#9660;")
            } else {
              htmltools::HTML("&#9654;")
            }

            rows[[length(rows) + 1]] <- div(
              class = paste0(
                "route-list-row",
                if (is_expanded) " expanded" else ""
              ),
              onclick = sprintf("toggleRouteExpand('%s')", route$route_id),
              tags$button(
                class = "route-action-btn expand-btn",
                onclick = sprintf(
                  "event.stopPropagation(); toggleRouteExpand('%s')",
                  route$route_id
                ),
                expand_icon
              ),
              div(
                class = "route-color-badge",
                style = paste0("background-color: #", route$route_color, ";")
              ),
              div(
                class = "route-info",
                div(
                  class = "route-info-display",
                  span(class = "route-short-name", route$route_short_name),
                  span(class = "route-long-name", route$route_long_name)
                )
              ),
              div(
                class = "route-actions",
                tags$button(
                  class = "route-action-btn edit-btn",
                  onclick = sprintf(
                    "event.stopPropagation(); editRouteFromList('%s')",
                    route$route_id
                  ),
                  title = "Edit route",
                  htmltools::HTML("&#9998;")
                ),
                tags$button(
                  class = "route-action-btn delete-btn",
                  onclick = sprintf(
                    "event.stopPropagation(); deleteRouteFromList('%s')",
                    route$route_id
                  ),
                  title = "Delete route",
                  htmltools::HTML('<i class="fa-solid fa-trash"></i>')
                )
              )
            )
          }

          # If expanded, show itineraries for this route
          if (is_expanded && !is_editing) {
            route_itins <- current_data$itin[
              current_data$itin$route_id == route$route_id,
            ]

            itin_rows <- list()

            # "Add itinerary" row at top of sub-list
            if (
              !is.null(adding_itin_route) && adding_itin_route == route$route_id
            ) {
              # Inline add itin form
              # Use active_direction_id so re-renders preserve direction changes
              current_dir <- as.integer(active_direction_id())
              existing_itins_for_dir <- current_data$itin[
                current_data$itin$route_id == route$route_id &
                  current_data$itin$direction_id == current_dir,
              ]
              if (nrow(existing_itins_for_dir) == 0) {
                variant_num <- 1
              } else {
                variant_nums <- sapply(
                  existing_itins_for_dir$itin_id,
                  function(id) {
                    parts <- strsplit(id, "_")[[1]]
                    if (length(parts) >= 3) as.integer(parts[3]) else 0
                  }
                )
                variant_num <- max(variant_nums) + 1
              }
              default_itin_id <- paste0(
                route$route_id,
                "_",
                current_dir,
                "_",
                variant_num
              )

              dir_sel_0 <- if (current_dir == 0) ' selected' else ''
              dir_sel_1 <- if (current_dir == 1) ' selected' else ''

              itin_rows[[length(itin_rows) + 1]] <- div(
                class = "itin-edit-form",
                tags$label("Itinerary ID"),
                tags$input(
                  type = "text",
                  id = "inline_itin_id",
                  value = default_itin_id
                ),
                tags$label("Direction"),
                htmltools::HTML(paste0(
                  '<select id="inline_direction_id" onchange="onDirectionChanged()">',
                  '<option value="0"',
                  dir_sel_0,
                  '>0</option>',
                  '<option value="1"',
                  dir_sel_1,
                  '>1</option>',
                  '</select>'
                )),
                tags$label("Trip Headsign"),
                tags$input(
                  type = "text",
                  id = "inline_trip_headsign",
                  placeholder = "e.g., Eastbound"
                ),
                div(
                  class = "btn-row",
                  tags$button(
                    class = "btn-save",
                    onclick = "saveItinFromForm()",
                    "Create"
                  ),
                  tags$button(
                    class = "btn-cancel",
                    onclick = "cancelItinEdit()",
                    "Cancel"
                  )
                )
              )
            } else {
              itin_rows[[length(itin_rows) + 1]] <- div(
                class = "stop-list-row add-row",
                style = "padding: 4px 8px; font-size: 12px;",
                onclick = sprintf("startAddingItin('%s')", route$route_id),
                tags$button(
                  class = "stop-action-btn add-btn",
                  style = "font-size: 14px; width: 22px; height: 22px;",
                  onclick = sprintf(
                    "event.stopPropagation(); startAddingItin('%s')",
                    route$route_id
                  ),
                  title = "Add new itinerary",
                  htmltools::HTML("+")
                ),
                span(
                  style = "margin-left: 6px; font-size: 12px;",
                  "Add itinerary"
                )
              )
            }

            # Existing itinerary rows
            if (nrow(route_itins) > 0) {
              for (j in 1:nrow(route_itins)) {
                itin <- route_itins[j, ]
                is_active <- !is.null(current_active_itin) &&
                  current_active_itin == itin$itin_id
                is_editing_itin <- !is.null(editing_itin) &&
                  editing_itin == itin$itin_id

                if (is_editing_itin) {
                  # Inline edit form for itinerary
                  dir_sel_0 <- if (as.integer(itin$direction_id) == 0) {
                    ' selected'
                  } else {
                    ''
                  }
                  dir_sel_1 <- if (as.integer(itin$direction_id) == 1) {
                    ' selected'
                  } else {
                    ''
                  }

                  itin_rows[[length(itin_rows) + 1]] <- div(
                    class = "itin-edit-form",
                    tags$label("Itinerary ID"),
                    tags$input(
                      type = "text",
                      id = "inline_itin_id",
                      value = itin$itin_id
                    ),
                    tags$label("Direction"),
                    #onchange handler added to direction select element
                    htmltools::HTML(paste0(
                      '<select id="inline_direction_id" onchange="onDirectionChanged()">',
                      '<option value="0"',
                      dir_sel_0,
                      '>0</option>',
                      '<option value="1"',
                      dir_sel_1,
                      '>1</option>',
                      '</select>'
                    )),
                    tags$label("Trip Headsign"),
                    tags$input(
                      type = "text",
                      id = "inline_trip_headsign",
                      value = itin$trip_headsign
                    ),
                    div(
                      class = "btn-row",
                      tags$button(
                        class = "btn-save",
                        onclick = "saveItinFromForm()",
                        htmltools::HTML("&#10003; Save")
                      ),
                      tags$button(
                        class = "btn-cancel",
                        onclick = "cancelItinEdit()",
                        "Cancel"
                      )
                    )
                  )
                } else {
                  itin_rows[[length(itin_rows) + 1]] <- div(
                    class = paste0(
                      "itin-list-row",
                      if (is_active) " active-itin" else ""
                    ),
                    onclick = sprintf("viewItinFromList('%s')", itin$itin_id),
                    span(
                      class = "itin-direction-badge",
                      paste0("D", itin$direction_id)
                    ),
                    div(
                      class = "itin-info",
                      div(
                        class = "itin-info-display",
                        span(class = "itin-headsign", itin$trip_headsign),
                        span(
                          class = "itin-id-display",
                          paste0("(", itin$itin_id, ")")
                        )
                      )
                    ),
                    div(
                      class = "route-actions",
                      tags$button(
                        class = "route-action-btn edit-btn",
                        onclick = sprintf(
                          "event.stopPropagation(); editItinFromList('%s')",
                          itin$itin_id
                        ),
                        title = "Edit itinerary",
                        htmltools::HTML("&#9998;")
                      ),
                      tags$button(
                        class = "route-action-btn",
                        onclick = sprintf(
                          "event.stopPropagation(); copyItinFromList('%s')",
                          itin$itin_id
                        ),
                        title = "Duplicate itinerary",
                        htmltools::HTML('<i class="fa-solid fa-clone"></i>') #fontawesome clone icon
                      ),
                      tags$button(
                        class = "route-action-btn delete-btn",
                        onclick = sprintf(
                          "event.stopPropagation(); deleteItinFromList('%s')",
                          itin$itin_id
                        ),
                        title = "Delete itinerary",
                        htmltools::HTML('<i class="fa-solid fa-trash"></i>')
                      )
                    )
                  )
                }
              }
            }

            rows[[length(rows) + 1]] <- div(
              class = "itin-list-container",
              do.call(tagList, itin_rows)
            )
          }
        }
      }

      do.call(tagList, rows)
    })

    # --- Route list event handlers ---

    # Toggle expand/collapse
    observeEvent(input$route_list_toggle_expand, {
      route_id <- input$route_list_toggle_expand$id
      if (!is.null(routes_expanded_id()) && routes_expanded_id() == route_id) {
        routes_expanded_id(NULL)
      } else {
        routes_expanded_id(route_id)
      }
    })

    # Start adding new route
    observeEvent(input$route_list_add_click, {
      routes_adding_new(TRUE)
      routes_editing_id(NULL)
    })

    # Edit route (pencil icon)
    observeEvent(input$route_list_edit_click, {
      routes_editing_id(input$route_list_edit_click$id)
      routes_adding_new(FALSE)
    })

    # Cancel route edit
    observeEvent(input$route_list_cancel_click, {
      routes_editing_id(NULL)
      routes_adding_new(FALSE)
    })

    # Save route from inline form (handles both add and edit)
    observeEvent(input$route_list_save_data, {
      data <- input$route_list_save_data
      new_route_id <- trimws(data$route_id)

      if (is.null(new_route_id) || new_route_id == "") {
        showNotification("Route ID cannot be empty.", type = "warning")
        return()
      }

      if (is.null(data$agency_id) || data$agency_id == "") {
        showNotification(
          "Please define at least one agency first.",
          type = "warning"
        )
        return()
      }

      current_data <- ssfs()
      route_color <- gsub("^#", "", data$route_color)
      route_text_color <- gsub("^#", "", data$route_text_color)

      if (routes_adding_new()) {
        # Adding new route
        if (new_route_id %in% current_data$routes$route_id) {
          showNotification("This route ID already exists.", type = "warning")
          return()
        }

        new_route <- data.frame(
          route_id = new_route_id,
          agency_id = data$agency_id,
          route_short_name = trimws(data$short_name),
          route_long_name = trimws(data$long_name),
          route_type = as.integer(data$route_type),
          route_color = route_color,
          route_text_color = route_text_color,
          stringsAsFactors = FALSE
        )

        current_data$routes <- rbind(current_data$routes, new_route)
        ssfs(current_data)
        routes_adding_new(FALSE)
        routes_expanded_id(new_route_id)
        showNotification("Route added successfully", type = "message")
      } else if (!is.null(routes_editing_id())) {
        # Editing existing route
        old_route_id <- routes_editing_id()
        idx <- which(current_data$routes$route_id == old_route_id)

        if (length(idx) == 0) {
          showNotification("Route not found.", type = "error")
          return()
        }

        # Check for ID conflict if changed
        if (
          new_route_id != old_route_id &&
            new_route_id %in% current_data$routes$route_id
        ) {
          showNotification("This route ID already exists.", type = "warning")
          return()
        }

        current_data$routes$route_id[idx] <- new_route_id
        current_data$routes$agency_id[idx] <- data$agency_id
        current_data$routes$route_short_name[idx] <- trimws(data$short_name)
        current_data$routes$route_long_name[idx] <- trimws(data$long_name)
        current_data$routes$route_type[idx] <- as.integer(data$route_type)
        current_data$routes$route_color[idx] <- route_color
        current_data$routes$route_text_color[idx] <- route_text_color

        # Update references in itin table if route_id changed
        if (new_route_id != old_route_id && nrow(current_data$itin) > 0) {
          current_data$itin$route_id[
            current_data$itin$route_id == old_route_id
          ] <- new_route_id
        }

        ssfs(current_data)

        # If expanded route was the one being edited, update expanded id
        if (
          !is.null(routes_expanded_id()) && routes_expanded_id() == old_route_id
        ) {
          routes_expanded_id(new_route_id)
        }

        routes_editing_id(NULL)
        showNotification("Route updated successfully", type = "message")
      }
    })

    # Delete route
    observeEvent(input$route_list_delete_click, {
      route_to_delete <- input$route_list_delete_click$id
      current_data <- ssfs()

      # Check if route is referenced in itin table
      if (
        nrow(current_data$itin) > 0 &&
          route_to_delete %in% current_data$itin$route_id
      ) {
        showNotification(
          paste0(
            "Cannot delete route '",
            route_to_delete,
            "'. It is referenced by one or more itineraries. ",
            "Delete the itineraries first."
          ),
          type = "error",
          duration = 5
        )
        return()
      }

      current_data$routes <- current_data$routes[
        current_data$routes$route_id != route_to_delete,
      ]
      ssfs(current_data)

      # Clear expanded if it was this route
      if (
        !is.null(routes_expanded_id()) &&
          routes_expanded_id() == route_to_delete
      ) {
        routes_expanded_id(NULL)
      }

      showNotification("Route deleted successfully", type = "message")
    })

    # --- Itinerary list event handlers ---

    # View/center itinerary on map
    observeEvent(input$itin_list_view_click, {
      itin_id <- input$itin_list_view_click$id
      current_data <- ssfs()

      selected_itin <- current_data$itin[current_data$itin$itin_id == itin_id, ]
      if (nrow(selected_itin) > 0) {
        bbox <- st_bbox(selected_itin$geometry)
        leaflet::leafletProxy("routes_map") |>
          leaflet::fitBounds(
            lng1 = bbox[["xmin"]],
            lat1 = bbox[["ymin"]],
            lng2 = bbox[["xmax"]],
            lat2 = bbox[["ymax"]]
          )
      }
    })

    # Edit itinerary (pencil icon) : loads it for map editing and displays inline edit
    observeEvent(input$itin_list_edit_click, {
      itin_id <- input$itin_list_edit_click$id
      current_ssfs_data <- ssfs()

      selected_itin <- current_ssfs_data$itin[
        current_ssfs_data$itin$itin_id == itin_id,
      ]

      if (nrow(selected_itin) == 0) {
        showNotification("Itinerary not found", type = "error")
        return()
      }

      # Set editing mode : show inline form by setting itin_editing_id
      editing_existing_itin(TRUE)
      itin_editing_id(itin_id) # THIS shows the inline edit form
      itin_adding_for_route(NULL)

      # Set the active reactive values
      active_route_id(selected_itin$route_id)
      active_direction_id(as.integer(selected_itin$direction_id))
      active_trip_headsign(selected_itin$trip_headsign)
      active_itin_id(itin_id)

      # Ensure route is expanded
      routes_expanded_id(selected_itin$route_id)

      # Load stop sequence
      stop_seq <- current_ssfs_data$stop_seq[
        current_ssfs_data$stop_seq$itin_id == itin_id,
      ]
      current_sequence(stop_seq)

      # Load shape and extract nodes
      shape_data <- current_ssfs_data$itin[
        current_ssfs_data$itin$itin_id == itin_id,
      ]

      if (nrow(shape_data) > 0) {
        coords <- st_coordinates(shape_data$geometry)
        full_points <- data.frame(
          index = 1:nrow(coords),
          lng = coords[, 1],
          lat = coords[, 2]
        )
        route_points(full_points)

        nodes_df <- data.frame(
          node_id = integer(),
          lng = numeric(),
          lat = numeric(),
          is_stop = logical(),
          stop_id = character(),
          stop_name = character(),
          speed_factor = double(),
          index = integer(),
          stringsAsFactors = FALSE
        )

        if (nrow(stop_seq) > 0) {
          for (i in 1:nrow(stop_seq)) {
            stop_id <- stop_seq$stop_id[i]
            stop_data <- current_ssfs_data$stops[
              current_ssfs_data$stops$stop_id == stop_id,
            ]

            if (nrow(stop_data) > 0) {
              stop_coords <- st_coordinates(stop_data$geometry)
              distances <- sqrt(
                (full_points$lng - stop_coords[1, 1])^2 +
                  (full_points$lat - stop_coords[1, 2])^2
              )
              closest_idx <- which.min(distances)

              nodes_df <- rbind(
                nodes_df,
                data.frame(
                  node_id = i,
                  lng = stop_coords[1, 1],
                  lat = stop_coords[1, 2],
                  is_stop = TRUE,
                  stop_id = stop_id,
                  stop_name = stop_seq$stop_name[i],
                  speed_factor = stop_seq$speed_factor[i],
                  index = closest_idx,
                  stringsAsFactors = FALSE
                )
              )
            }
          }
          row.names(nodes_df) <- 1:nrow(nodes_df)
        }

        route_nodes(nodes_df)
      }

      # Center map on itinerary
      bbox <- st_bbox(shape_data$geometry)
      leaflet::leafletProxy("routes_map") |>
        leaflet::fitBounds(
          lng1 = bbox[["xmin"]],
          lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]],
          lat2 = bbox[["ymax"]]
        )

      showNotification(paste("Editing itinerary:", itin_id), type = "message")
    })

    # Start adding new itinerary
    observeEvent(input$itin_list_add_click, {
      route_id <- input$itin_list_add_click$id

      # Clear any current editing state
      clearInputs()

      # Set the active route and default direction
      active_route_id(route_id)
      active_direction_id(0L)
      active_trip_headsign("")
      itin_adding_for_route(route_id)
      itin_editing_id(NULL)

      # Calculate next itin_id
      current_data <- ssfs()
      direction_id <- 0L
      existing_itins <- current_data$itin[
        current_data$itin$route_id == route_id,
      ]

      if (nrow(existing_itins) == 0) {
        variant_num <- 1
      } else {
        variant_nums <- sapply(existing_itins$itin_id, function(id) {
          parts <- strsplit(id, "_")[[1]]
          if (length(parts) >= 3) as.integer(parts[3]) else 0
        })
        variant_num <- max(variant_nums) + 1
      }

      new_itin_id <- paste0(route_id, "_", direction_id, "_", variant_num)
      active_itin_id(new_itin_id)
      editing_existing_itin(FALSE)
    })

    # Recalculate itin_id when direction changes in inline form
    observeEvent(input$inline_direction_changed, {
      new_direction <- as.integer(input$inline_direction_changed$direction_id)

      # Determine which route_id applies
      route_id <- NULL
      if (!is.null(itin_adding_for_route())) {
        route_id <- itin_adding_for_route()
      } else if (!is.null(itin_editing_id())) {
        current_data <- ssfs()
        idx <- which(current_data$itin$itin_id == itin_editing_id())
        if (length(idx) > 0) {
          route_id <- current_data$itin$route_id[idx]
        }
      }

      if (is.null(route_id)) {
        return()
      }

      current_data <- ssfs()

      # Get existing itins for this route + new direction
      existing_itins <- current_data$itin[
        current_data$itin$route_id == route_id &
          current_data$itin$direction_id == new_direction,
      ]

      if (nrow(existing_itins) == 0) {
        variant_num <- 1
      } else {
        variant_nums <- sapply(existing_itins$itin_id, function(id) {
          parts <- strsplit(id, "_")[[1]]
          if (length(parts) >= 3) as.integer(parts[3]) else 0
        })
        variant_num <- max(variant_nums) + 1
      }

      new_itin_id <- paste0(route_id, "_", new_direction, "_", variant_num)

      # Update the inline_itin_id field via JS
      session$sendCustomMessage("updateInlineItinId", new_itin_id)

      # Also update the reactive value
      active_direction_id(new_direction)
      active_itin_id(new_itin_id)
    })

    # Save new itinerary details from inline form and saves geometry

    observeEvent(input$itin_list_save_data, {
      data <- input$itin_list_save_data
      new_itin_id <- trimws(data$itin_id)
      new_direction <- as.integer(data$direction_id)
      new_headsign <- trimws(data$trip_headsign)

      if (new_itin_id == "") {
        showNotification("Itinerary ID cannot be empty.", type = "warning")
        return()
      }

      if (new_headsign == "") {
        showNotification("Trip headsign cannot be empty.", type = "warning")
        return()
      }

      current_data <- ssfs()
      curr_points <- route_points()

      if (!is.null(itin_editing_id())) {
        # --- EDITING EXISTING ITINERARY ---
        old_itin_id <- itin_editing_id()

        # Check for ID conflict if changed
        if (
          new_itin_id != old_itin_id &&
            new_itin_id %in% current_data$itin$itin_id
        ) {
          showNotification(
            "This itinerary ID already exists.",
            type = "warning"
          )
          return()
        }

        # Check geometry exists
        if (nrow(curr_points) < 2) {
          showNotification(
            "Itinerary must have at least 2 points.",
            type = "warning"
          )
          return()
        }

        # Get the route_id from the existing itin entry
        route_id <- current_data$itin$route_id[
          current_data$itin$itin_id == old_itin_id
        ]

        # Build geometry
        curr_points_sorted <- curr_points[order(curr_points$index), ]
        coords_matrix <- as.matrix(curr_points_sorted[, c("lng", "lat")])
        line_feature <- st_linestring(coords_matrix)

        new_itin_entry <- st_sf(
          itin_id = new_itin_id,
          route_id = route_id,
          direction_id = new_direction,
          trip_headsign = new_headsign,
          geometry = st_sfc(line_feature, crs = 4326),
          stringsAsFactors = FALSE
        )

        # Set active_itin_id so generateStopSequenceFromNodes uses the correct ID
        active_itin_id(new_itin_id)
        stop_seq <- generateStopSequenceFromNodes()

        # Remove old entries
        current_data$itin <- current_data$itin[
          current_data$itin$itin_id != old_itin_id,
        ]
        current_data$stop_seq <- current_data$stop_seq[
          current_data$stop_seq$itin_id != old_itin_id,
        ]

        # Add new
        current_data$itin <- rbind(current_data$itin, new_itin_entry)
        if (nrow(stop_seq) > 0) {
          current_data$stop_seq <- rbind(current_data$stop_seq, stop_seq)
        }

        # Update references in span, hsh if itin_id changed
        if (new_itin_id != old_itin_id) {
          current_data$span$itin_id[
            current_data$span$itin_id == old_itin_id
          ] <- new_itin_id
          current_data$hsh$itin_id[
            current_data$hsh$itin_id == old_itin_id
          ] <- new_itin_id
        }

        ssfs(current_data)
        clearInputs()
        showNotification("Itinerary saved successfully", type = "message")
      } else if (!is.null(itin_adding_for_route())) {
        # --- ADDING NEW ITINERARY ---

        # Check for ID conflict
        if (new_itin_id %in% current_data$itin$itin_id) {
          showNotification(
            "This itinerary ID already exists.",
            type = "warning"
          )
          return()
        }

        # Check geometry exists
        if (nrow(curr_points) < 2) {
          showNotification(
            "Please draw the route on the map before saving.",
            type = "warning"
          )
          return()
        }

        route_id <- itin_adding_for_route()

        # Build geometry
        curr_points_sorted <- curr_points[order(curr_points$index), ]
        coords_matrix <- as.matrix(curr_points_sorted[, c("lng", "lat")])
        line_feature <- st_linestring(coords_matrix)

        new_itin_entry <- st_sf(
          itin_id = new_itin_id,
          route_id = route_id,
          direction_id = new_direction,
          trip_headsign = new_headsign,
          geometry = st_sfc(line_feature, crs = 4326),
          stringsAsFactors = FALSE
        )

        # Set active_itin_id so generateStopSequenceFromNodes uses the correct ID
        active_itin_id(new_itin_id)
        stop_seq <- generateStopSequenceFromNodes()

        # Add to ssfs
        current_data$itin <- rbind(current_data$itin, new_itin_entry)
        if (nrow(stop_seq) > 0) {
          current_data$stop_seq <- rbind(current_data$stop_seq, stop_seq)
        }

        ssfs(current_data)
        clearInputs()
        showNotification("Itinerary saved successfully", type = "message")
      }
    })

    # Cancel itinerary editing
    observeEvent(input$itin_list_cancel_click, {
      clearInputs()
    })

    # Edit itinerary details (inline form, not map loading)
    # This is triggered by the pencil icon to edit just the metadata
    # For map loading, use itin_list_edit_click above

    # Delete itinerary
    observeEvent(input$itin_list_delete_click, {
      itin_to_delete <- input$itin_list_delete_click$id
      current_data <- ssfs()

      if (!itin_to_delete %in% current_data$itin$itin_id) {
        showNotification("Itinerary not found", type = "error")
        return()
      }

      current_data$itin <- current_data$itin |>
        filter(itin_id != itin_to_delete)
      current_data$stop_seq <- current_data$stop_seq |>
        filter(itin_id != itin_to_delete)
      current_data$span <- current_data$span |>
        filter(itin_id != itin_to_delete)
      current_data$hsh <- current_data$hsh |> filter(itin_id != itin_to_delete)
      ssfs(current_data)

      # Clear editing state if this was the active itinerary
      if (!is.null(active_itin_id()) && active_itin_id() == itin_to_delete) {
        clearInputs()
      }

      showNotification(
        paste("Deleted itinerary:", itin_to_delete),
        type = "message"
      )
    })

    # Copy/duplicate itinerary
    observeEvent(input$itin_list_copy_click, {
      itin_to_copy <- input$itin_list_copy_click$id
      current_data <- ssfs()

      source_itin <- current_data$itin[
        current_data$itin$itin_id == itin_to_copy,
      ]
      if (nrow(source_itin) == 0) {
        showNotification("Itinerary not found", type = "error")
        return()
      }

      route_id <- source_itin$route_id
      direction_id <- as.integer(source_itin$direction_id)

      # Find next available variant number
      existing_itins <- current_data$itin[
        current_data$itin$route_id == route_id &
          current_data$itin$direction_id == direction_id,
      ]
      if (nrow(existing_itins) == 0) {
        variant_num <- 1
      } else {
        variant_nums <- sapply(existing_itins$itin_id, function(id) {
          parts <- strsplit(id, "_")[[1]]
          if (length(parts) >= 3) as.integer(parts[3]) else 0
        })
        variant_num <- max(variant_nums) + 1
      }
      new_itin_id <- paste0(route_id, "_", direction_id, "_", variant_num)

      # Duplicate itin row with new id and headsign
      new_itin_entry <- source_itin
      new_itin_entry$itin_id <- new_itin_id
      new_itin_entry$trip_headsign <- paste0(source_itin$trip_headsign, "_copy")

      # Duplicate stop_seq rows
      source_stop_seq <- current_data$stop_seq[
        current_data$stop_seq$itin_id == itin_to_copy,
      ]
      new_stop_seq <- source_stop_seq
      if (nrow(new_stop_seq) > 0) {
        new_stop_seq$itin_id <- new_itin_id
      }

      current_data$itin <- rbind(current_data$itin, new_itin_entry)
      if (nrow(new_stop_seq) > 0) {
        current_data$stop_seq <- rbind(current_data$stop_seq, new_stop_seq)
      }

      ssfs(current_data)

      # Expand the route so the new itinerary is visible
      routes_expanded_id(route_id)

      showNotification(paste("Duplicated as:", new_itin_id), type = "message")
    })

    # --- Route itinerary geometry and map handlers ---

    # Function to calculate threshold distance from existing points
    calculateThreshold <- function(zoom) {
      # Base threshold at zoom level 10 is 0.02
      base_threshold <- 0.02
      # Adjust threshold exponentially based on zoom difference from base level
      # Smaller number when zoomed in, larger when zoomed out
      adjusted_threshold <- base_threshold * (2^(10 - zoom))
      # Clamp the threshold to reasonable limits
      return(min(max(adjusted_threshold, 0.0001), 0.1))
    }

    # Modified function to generate partial route segments between nodes
    generateRouteSegment <- function(
      from_point,
      to_point,
      drawing_mode = "network"
    ) {
      #Initialize result_points
      result_points <- data.frame(lng = numeric(), lat = numeric())

      if (drawing_mode == "network") {
        # Try OSRM routing
        tryCatch(
          {
            from_sf <- st_sf(
              geometry = st_sfc(st_point(from_point), crs = 4326)
            )
            to_sf <- st_sf(geometry = st_sfc(st_point(to_point), crs = 4326))

            route <- osrm::osrmRoute(
              src = from_sf,
              dst = to_sf,
              overview = "full"
            )
            route_coords <- st_coordinates(route$geometry)

            # Add all points from route
            for (j in 1:nrow(route_coords)) {
              result_points <- rbind(
                result_points,
                data.frame(
                  lng = route_coords[j, 1],
                  lat = route_coords[j, 2]
                )
              )
            }
          },
          error = function(e) {
            # If OSRM fails, just add direct line (start and end points)
            result_points <- rbind(
              data.frame(lng = from_point[1], lat = from_point[2]),
              data.frame(lng = to_point[1], lat = to_point[2])
            )
          }
        )
      } else {
        # Free drawing mode - just connect with straight line
        result_points <- rbind(
          data.frame(lng = from_point[1], lat = from_point[2]),
          data.frame(lng = to_point[1], lat = to_point[2])
        )
      }

      return(result_points)
    }

    # Reactive values for the combined routes/shapes functionality
    current_sequence <- reactiveVal(data.frame(
      itin_id = character(),
      stop_id = character(),
      stop_sequence = integer(),
      speed_factor = double(),
      stop_name = character(),
      stringsAsFactors = FALSE
    ))

    route_nodes <- reactiveVal(data.frame(
      node_id = integer(), # Sequential ID for the node
      lng = numeric(), # Longitude
      lat = numeric(), # Latitude
      is_stop = logical(), # Is this node a stop?
      stop_id = character(), # If it's a stop, what's its ID (empty otherwise)
      stop_name = character(), # If it's a stop, what's its name (empty otherwise)
      speed_factor = double(), #if it's a stop, what is its speed factor (empty otherwise)
      index = integer(), # Bridge field with route_points
      stringsAsFactors = FALSE
    ))

    route_points <- reactiveVal(data.frame(
      index = numeric(),
      lng = numeric(),
      lat = numeric()
    ))

    route_editing_mode <- reactiveVal(FALSE)
    selected_point_index <- reactiveVal(NULL)
    active_itin_id <- reactiveVal(NULL)
    editing_existing_itin <- reactiveVal(FALSE)

    # Add a reactive value to track when a marker was last clicked
    last_marker_click_time <- reactiveVal(0)

    # Function to convert nodes to stop sequence
    #VALIDATE : when is this function used ?
    generateStopSequenceFromNodes <- function(nodes = NULL, itin_id = NULL) {
      if (is.null(nodes)) {
        nodes <- route_nodes()
      }
      if (is.null(itin_id)) {
        itin_id <- active_itin_id()
      }

      # Extract nodes that are stops
      stop_nodes <- nodes[nodes$is_stop, ]

      if (nrow(stop_nodes) == 0) {
        return(data.frame())
      }

      # Create stop sequence
      stop_seq <- data.frame(
        itin_id = rep(itin_id, nrow(stop_nodes)),
        stop_id = stop_nodes$stop_id,
        stop_sequence = 1:nrow(stop_nodes),
        speed_factor = stop_nodes$speed_factor,
        stop_name = stop_nodes$stop_name,
        stringsAsFactors = FALSE
      )

      #ensure that (1) the last entry of speed_factor for the itin_id is NA_real_ and
      #(2) all other entries are not NA : those that are should be replaced by 1
      stop_seq <-
        stop_seq |>
        mutate(speed_factor = replace_na(speed_factor, 1)) |>
        mutate(
          speed_factor = if_else(
            stop_sequence == max(stop_sequence),
            NA_real_,
            speed_factor
          )
        )

      return(stop_seq)
    }

    # Initialize routes map
    output$routes_map <- leaflet::renderLeaflet({
      center <- map_center()
      leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE)) |>
        leaflet::addProviderTiles("CartoDB.Positron", group = "Positron") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addProviderTiles("OpenStreetMap.HOT", group = "OSM") |>
        leaflet::setView(lng = center$lng, lat = center$lat, zoom = 12) |>
        leaflet::addLayersControl(
          baseGroups = c("Positron", "Satellite", "OSM"),
          position = "bottomright",
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) |>
        leaflet::showGroup("stops") |>
        leaflet::showGroup("routes") |>
        leaflet::showGroup("current_route") |>
        htmlwidgets::onRender(
          "
      function(el, x) {
        this.on('zoomend', function(e) {
          Shiny.setInputValue('routes_map_zoom', this.getZoom());
        });

        // Capture right-click events
        this.on('contextmenu', function(e) {
          Shiny.setInputValue('routes_map_right_click', {
            lat: e.latlng.lat,
            lng: e.latlng.lng
          }, {priority: 'event'});
        });
      }
    "
        )
    })

    # Update zoom level when map is zoomed
    observeEvent(input$routes_map_zoom, {
      current_zoom(input$routes_map_zoom)
    })

    # observe block that updates map
    observe({
      curr_nodes <- route_nodes()
      curr_points <- route_points()
      current_data <- ssfs()

      proxy <- leaflet::leafletProxy("routes_map") |>
        leaflet::clearGroup("stops") |>
        leaflet::clearGroup("routes") |>
        leaflet::clearGroup("current_route") |>
        leaflet::clearGroup("route_nodes")

      # Add existing routes (except current one being edited)
      if (!is.null(current_data$itin) && nrow(current_data$itin) > 0) {
        for (i in 1:nrow(current_data$itin)) {
          # Skip the current route being edited
          if (
            !is.null(active_itin_id()) &&
              current_data$itin$itin_id[i] == active_itin_id()
          ) {
            next
          }

          line_coords <- st_coordinates(current_data$itin$geometry[i])

          # Get route_color from routes table based on route_id
          route_id_i <- current_data$itin$route_id[i]
          route_color_i <- current_data$routes$route_color[
            current_data$routes$route_id == route_id_i
          ]

          # Use route color if found, otherwise fallback to default
          line_color <- if (
            length(route_color_i) > 0 &&
              !is.na(route_color_i[1]) &&
              nchar(route_color_i[1]) > 0
          ) {
            paste0("#", route_color_i[1])
          } else {
            "#05AEEF"
          }

          proxy <- proxy |>
            leaflet::addPolylines(
              lng = line_coords[, 1],
              lat = line_coords[, 2],
              group = "routes",
              color = line_color,
              weight = 2,
              opacity = 0.6
            )
        }
      }

      # Add all stops
      if (!is.null(current_data$stops) && nrow(current_data$stops) > 0) {
        marker_size <- calculateMarkerSize(current_zoom())

        # Get IDs of stops that are in current nodes
        stop_ids_in_nodes <- curr_nodes$stop_id[curr_nodes$is_stop]

        # Determine colors based on whether stop is in sequence
        fill_colors <- ifelse(
          current_data$stops$stop_id %in% stop_ids_in_nodes,
          "#B2182B",
          "#7f7f7f"
        )

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

      # Add current route being edited
      if (nrow(curr_points) > 1) {
        proxy <- proxy |>
          leaflet::addPolylines(
            lng = curr_points$lng,
            lat = curr_points$lat,
            group = "current_route",
            color = "#B2182B",
            weight = 4,
            opacity = 0.8
          )
      }

      # Add node markers (with different styles for stops vs. waypoints)
      if (nrow(curr_nodes) > 0) {
        # Add stop nodes
        stop_nodes <- curr_nodes[curr_nodes$is_stop, ]
        if (nrow(stop_nodes) > 0) {
          proxy <- proxy |>
            leaflet::addCircleMarkers(
              lng = stop_nodes$lng,
              lat = stop_nodes$lat,
              group = "route_nodes",
              radius = 8,
              color = "#B2182B",
              fillColor = "#B2182B",
              fillOpacity = 0.9,
              stroke = TRUE,
              weight = 2,
              layerId = paste0("stop_", stop_nodes$node_id),
              label = paste0("Stop: ", stop_nodes$stop_name)
            )
        }

        # Add waypoint nodes
        waypoint_nodes <- curr_nodes[!curr_nodes$is_stop, ]
        if (nrow(waypoint_nodes) > 0) {
          proxy <- proxy |>
            leaflet::addCircleMarkers(
              lng = waypoint_nodes$lng,
              lat = waypoint_nodes$lat,
              group = "route_nodes",
              radius = 6,
              color = "orange",
              fillColor = "orange",
              fillOpacity = 0.9,
              stroke = TRUE,
              weight = 2,
              layerId = paste0("waypoint_", waypoint_nodes$node_id),
              label = "Waypoint"
            )
        }

        # Highlight selected node
        if (!is.null(selected_point_index())) {
          selected_node <- curr_nodes[
            curr_nodes$node_id == selected_point_index(),
          ]
          if (nrow(selected_node) > 0) {
            proxy <- proxy |>
              leaflet::addCircleMarkers(
                lng = selected_node$lng,
                lat = selected_node$lat,
                group = "route_nodes",
                radius = 8,
                color = "#FFE999",
                fillColor = "#FFE999",
                fillOpacity = 0.9,
                stroke = TRUE,
                weight = 3,
                layerId = "selected_node"
              )
          }
        }
      }

      return(proxy)
    })

    # Auto-update active_itin_id when active_route_id or active_direction_id changes (for new itineraries)
    observe({
      req(active_route_id())

      if (!editing_existing_itin()) {
        current_data <- ssfs()
        route_id <- active_route_id()
        direction_id <- active_direction_id()

        existing_itins <- current_data$itin |>
          filter(route_id == !!route_id, direction_id == !!direction_id)

        if (nrow(existing_itins) == 0) {
          variant_num <- 1
        } else {
          variant_nums <- sapply(existing_itins$itin_id, function(id) {
            parts <- strsplit(id, "_")[[1]]
            if (length(parts) >= 3) as.integer(parts[3]) else 0
          })
          variant_num <- max(variant_nums) + 1
        }

        new_itin_id <- paste0(route_id, "_", direction_id, "_", variant_num)
        active_itin_id(new_itin_id)
      }
    })

    # Stop click handler - adds stop nodes
    observeEvent(input$routes_map_marker_click, {
      req(active_itin_id())
      click <- input$routes_map_marker_click

      # Set the timestamp of this marker click
      last_marker_click_time(as.numeric(Sys.time()))

      # Check if clicked on a waypoint
      if (!is.null(click) && grepl("^waypoint_", click$id)) {
        # Clicked on a node - select it for potential movement
        node_id <- as.numeric(gsub("waypoint_", "", click$id))

        selected_point_index(node_id)
        showNotification(
          "Waypoint selected. Click on map to move it.",
          type = "message"
        )

        #check if the node clicked is the selected waypoint
      } else if (!is.null(click) && click$id == "selected_node") {
        #deselect the point for movement and notify the user.
        selected_point_index(NULL)
        showNotification(
          "Waypoint deselected. Movement cancelled.",
          type = "message"
        )

        #check if clicked on a stop among current nodes
      } else if (!is.null(click) && grepl("^stop_", click$id)) {
        #nothing happens - notify user that stop is already part of the route
        showNotification(
          "Stop already in route stop sequence. Cannot add stop again.",
          type = "warning"
        )

        #otherwise, it is a click on a stop elsewhere that can be added to the route
      } else if (!is.null(click)) {
        current_data <- ssfs()
        clicked_stop <- current_data$stops[
          current_data$stops$stop_id == click$id,
        ]
        curr_nodes <- route_nodes()
        curr_points <- route_points()

        #if a waypoint is already selected for movement, move the waypoint to the clicked stop
        #give it the attributes of the clicked stop
        if (!is.null(selected_point_index())) {
          #CURR NODES NO, SP NOT NULL
          #(waypoint location moved & given stop attributes, stop added to route, stop sequence recalculated)

          #FIRST verify whether the selected node is the last (or only) of the route
          is_last_node <- (selected_point_index() == nrow(curr_nodes))

          if (is_last_node) {
            # --- LAST NODE CASE: no segment D, only A + new B ---

            #if it's the only point, then update point and node
            if (selected_point_index() == 1) {
              #ACTUALLY this case would never happen as the app requires at least one stop in the sequence
              #before placing a waypoint node

              curr_nodes <- data.frame(
                node_id = 1,
                lng = st_coordinates(clicked_stop)[1],
                lat = st_coordinates(clicked_stop)[2],
                is_stop = TRUE,
                stop_id = clicked_stop$stop_id,
                stop_name = clicked_stop$stop_name,
                speed_factor = 1, #assign speed factor 1 for first stop
                index = 1,
                stringsAsFactors = FALSE
              )
              curr_points <-
                data.frame(
                  index = 1,
                  lng = st_coordinates(clicked_stop)[1],
                  lat = st_coordinates(clicked_stop)[2]
                )
            } else {
              #else it's not the only point, but it's last node

              before_idx <- selected_point_index() - 1

              #segment A : everything before the selected node
              nodes_a <- curr_nodes[1:before_idx, ]
              nodes_a_idx_max <- max(nodes_a$index)
              points_a <- curr_points[1:nodes_a_idx_max, ]

              #generate new segment B from previous node to clicked stop
              from_point <- c(
                curr_nodes[before_idx, ]$lng,
                curr_nodes[before_idx, ]$lat
              )
              to_point <- c(
                st_coordinates(clicked_stop)[1],
                st_coordinates(clicked_stop)[2]
              )

              segment_b <- generateRouteSegment(
                from_point,
                to_point,
                drawing_mode = input$drawing_mode
              )

              points_b <-
                segment_b[2:nrow(segment_b), ] |>
                mutate(index = row_number() + nodes_a_idx_max, .before = "lng")

              points_b_idx_max <- max(points_b$index)

              #create new stop node to replace waypoint
              node_new <- data.frame(
                node_id = selected_point_index(),
                lng = st_coordinates(clicked_stop)[1],
                lat = st_coordinates(clicked_stop)[2],
                is_stop = TRUE,
                stop_id = clicked_stop$stop_id,
                stop_name = clicked_stop$stop_name,
                speed_factor = 1,
                index = points_b_idx_max,
                stringsAsFactors = FALSE
              )

              #reconstitute
              curr_points <- rbind(points_a, points_b)
              curr_nodes <- rbind(nodes_a, node_new)

              row.names(curr_points) <- 1:nrow(curr_points)
              row.names(curr_nodes) <- 1:nrow(curr_nodes)
            }
          } else {
            #else it's not the last node, nor the first one, so it's a node in between

            before_idx <- selected_point_index() - 1
            after_idx <- selected_point_index() + 1

            #divide the nodes and route points into segments a, b, c and d
            #segments b and c (before and after the selected point that is moved to a new point)
            #will be calculated later
            #index values in segment d need to be adjusted based on the difference of
            #segment bc before and after

            #segment A : (from the beginning of the itinerary to the point before the selected one)
            nodes_a <- curr_nodes[1:before_idx, ]
            nodes_a_idx_max <- max(nodes_a$index)

            points_a <- curr_points[1:nodes_a_idx_max, ]

            #segment D : (from the point after the selected one to the end of the itinerary)
            nodes_d <- curr_nodes[after_idx:nrow(curr_nodes), ]
            nodes_d_idx_min <- min(nodes_d$index)

            points_d <- curr_points[nodes_d_idx_min:nrow(curr_points), ]

            #number of points in BC before :
            nb_points_bc_before <-
              min(points_d$index) - max(points_a$index) - 1
            #specifically the points BETWEEN segments a and d,
            #excluding the last point of a and the first point of d

            #generate segment B

            #from the end of segment a to the new stop
            from_point <- c(
              curr_nodes[before_idx, ]$lng,
              curr_nodes[before_idx, ]$lat
            )
            to_point <- c(
              st_coordinates(clicked_stop)[1],
              st_coordinates(clicked_stop)[2]
            )

            segment_b <- generateRouteSegment(
              from_point,
              to_point,
              drawing_mode = input$drawing_mode
            )

            #EXCLUDING the first point which is included in points_a
            points_b <-
              segment_b[2:nrow(segment_b), ] |>
              mutate(index = row_number() + nodes_a_idx_max, .before = "lng")

            #point b index max will be the new index value for the moved node AND added stop
            #and will serve to adjust the index values of points_d and nodes_d

            points_b_idx_max <-
              max(points_b$index)

            #generate segment C

            #from new stop to beginning of segment d

            from_point <- c(
              st_coordinates(clicked_stop)[1],
              st_coordinates(clicked_stop)[2]
            )
            to_point <- c(
              curr_nodes[after_idx, ]$lng,
              curr_nodes[after_idx, ]$lat
            )

            segment_c <- generateRouteSegment(
              from_point,
              to_point,
              drawing_mode = input$drawing_mode
            )

            points_c <-
              segment_c[2:(nrow(segment_c) - 1), ] |> #EXCLUDING the first and the last point,
              #which are included in the other segments already
              mutate(index = row_number() + points_b_idx_max, .before = "lng")

            points_bc <- rbind(points_b, points_c)

            #adjust index values for points and nodes d :

            nb_points_bc_after <- nrow(points_bc)

            adj_index_d <- nb_points_bc_after - nb_points_bc_before
            #this will help add or subtract from the index values, depending on if the new segment bc
            #has more or fewer points than the old segment bc

            points_d <-
              points_d |>
              mutate(index = index + adj_index_d)

            nodes_d <-
              nodes_d |>
              mutate(index = index + adj_index_d)
            #called nodes_d but it's really just the third set of nodes
            #nodes_a, new node, and nodes_d

            #create new stop node based on clicked stop to replace waypoint

            node_bc <-
              data.frame(
                node_id = selected_point_index(),
                lng = st_coordinates(clicked_stop)[1],
                lat = st_coordinates(clicked_stop)[2],
                is_stop = TRUE,
                stop_id = clicked_stop$stop_id,
                stop_name = clicked_stop$stop_name,
                speed_factor = 1, #assign speed factor 1 to new stop in the sequence
                index = points_b_idx_max
              )

            #reconstitute curr_points and curr_nodes

            curr_points <-
              rbind(points_a, points_b, points_c, points_d)

            curr_nodes <-
              rbind(nodes_a, node_bc, nodes_d)

            #rename rows for good form
            row.names(curr_points) <- 1:nrow(curr_points)
            row.names(curr_nodes) <- 1:nrow(curr_nodes)
          }

          #update the reactive values
          route_points(curr_points)
          route_nodes(curr_nodes)
          selected_point_index(NULL)

          showNotification(
            "Waypoint moved to stop & adopted stop properties.",
            type = "message"
          )
        } else {
          #add stop to the end of the route, OR add first node

          # Get stop coordinates
          stop_coords <- st_coordinates(clicked_stop$geometry)

          if (nrow(curr_nodes) >= 1) {
            # Check if this stop is already in the route
            if (clicked_stop$stop_id %in% curr_nodes$stop_id) {
              showNotification(
                "Stop already in route stop sequence. Cannot add stop again.",
                type = "warning"
              )
              return()
            }
            #if there's 1 node or more, add the new node to the existing ones

            #get the existing max index from the nodes
            nodes_a_idx_max <- max(curr_nodes$index)

            #Route a new segment from the last existing node to the new node
            #using the last existing node coords as from_point and
            #stop_coords from clicked stop as to_point
            from_lng <- curr_nodes[nrow(curr_nodes), ]$lng
            from_lat <- curr_nodes[nrow(curr_nodes), ]$lat

            from_point <- c(from_lng, from_lat)
            to_point <- c(stop_coords[1], stop_coords[2])

            new_segment <- generateRouteSegment(
              from_point,
              to_point,
              drawing_mode = input$drawing_mode
            )

            new_points <-
              new_segment[2:nrow(new_segment), ] |> #EXCLUDING the first point
              #which is included in the previous segment already
              mutate(index = row_number() + nodes_a_idx_max, .before = "lng")

            curr_points <- rbind(curr_points, new_points)

            #just in case, rename curr_point row names
            row.names(curr_points) <- 1:nrow(curr_points)

            #max index of new curr_points becomes the index of new node
            #(associated with stop_coords)

            new_node_index <- max(curr_points$index)

            # Create new node
            new_node <- data.frame(
              node_id = max(curr_nodes$node_id) + 1,
              lng = stop_coords[1],
              lat = stop_coords[2],
              is_stop = TRUE,
              stop_id = clicked_stop$stop_id,
              stop_name = clicked_stop$stop_name,
              speed_factor = 1, #assign speed factor of 1 for new stop
              index = new_node_index,
              stringsAsFactors = FALSE
            )

            # Add to nodes
            curr_nodes <- rbind(curr_nodes, new_node)

            #curr_nodes |> st_as_sf(coords=c("lng","lat"),crs=4326) |> mapview()

            route_points(curr_points)
            route_nodes(curr_nodes)
          } else {
            #it's the first node !
            route_nodes(data.frame(
              node_id = 1,
              lng = stop_coords[1],
              lat = stop_coords[2],
              is_stop = TRUE,
              stop_id = clicked_stop$stop_id,
              stop_name = clicked_stop$stop_name,
              speed_factor = 1, #assign speed factor 1 for first stop
              index = 1,
              stringsAsFactors = FALSE
            ))
            route_points(data.frame(
              index = 1,
              lng = stop_coords[1],
              lat = stop_coords[2]
            ))
          }
        }

        # Update current sequence for display
        current_sequence(generateStopSequenceFromNodes())
      }
    })

    # Map click handler
    observeEvent(input$routes_map_click, {
      req(active_itin_id())

      # Check if this map click is too close in time to a marker click
      current_time <- as.numeric(Sys.time())
      time_since_marker_click <- current_time - last_marker_click_time()

      # If a marker was clicked within the last 100ms, ignore this map click
      if (time_since_marker_click < 0.1) {
        return()
      }

      click <- input$routes_map_click
      curr_nodes <- route_nodes()
      curr_points <- route_points()

      #a waypoint is selected for movement : move it to the clicked location
      #and update route points and route nodes
      #based on the number of current nodes
      if (!is.null(selected_point_index())) {
        # Move the selected node to the new location
        idx <- which(curr_nodes$node_id == selected_point_index())

        #only one node and it is moved, replace its geo coords
        if (nrow(curr_nodes) == 1) {
          curr_nodes$lat <- click$lat
          curr_nodes$lng <- click$lng

          curr_points$lat <- click$lat
          curr_points$lng <- click$lng

          #More than one node but the first node is selected for movement
        } else if (idx == 1) {
          #remove and replace the first segment of points

          #how many points in the first segment before the point move?
          nb_points_before <- curr_nodes[2, ]$index - 1

          #recreate new first segment based on new position of first point
          from_point <- c(click$lng, click$lat)
          to_point <- c(curr_nodes[2, ]$lng, curr_nodes[2, ]$lat)

          new_segment <- generateRouteSegment(
            from_point,
            to_point,
            drawing_mode = input$drawing_mode
          )

          #new points to be used to replace previous points
          new_points <-
            new_segment[1:(nrow(new_segment) - 1), ] |> #EXCLUDING the last point,
            #which is included in the retained segment
            mutate(index = row_number(), .before = "lng")

          #nrow(new_points) is used for the new number of points

          adj_index <- nrow(new_points) - nb_points_before

          #define retained points
          curr_points <-
            rbind(
              new_points,
              curr_points[(nb_points_before + 1):nrow(curr_points), ] |>
                mutate(index = index + adj_index)
            )

          #rename rows..
          row.names(curr_points) <- 1:nrow(curr_points)

          #this part will need to be reworked to handle various cases related to stop status...
          curr_nodes[1, ]$lng <- click$lng
          curr_nodes[1, ]$lat <- click$lat

          curr_nodes <-
            rbind(
              curr_nodes[1, ],
              curr_nodes[2:nrow(curr_nodes), ] |>
                mutate(index = index + adj_index)
            )

          #the selected node is the last node
        } else if (idx == nrow(curr_nodes)) {
          #recreate new last segment based on new position of last point
          from_point <- c(curr_nodes[idx - 1, ]$lng, curr_nodes[idx - 1, ]$lat)
          to_point <- c(click$lng, click$lat)

          new_segment <- generateRouteSegment(
            from_point,
            to_point,
            drawing_mode = input$drawing_mode
          )

          #points in retained segment

          nb_points_retained <-
            curr_nodes[idx - 1, ]$index

          #new points to be used to replace previous points
          new_points <-
            new_segment[2:(nrow(new_segment)), ] |> #EXCLUDING the first point,
            #which is included in the retained segment
            mutate(index = row_number() + nb_points_retained, .before = "lng")

          #define new curr points
          curr_points <-
            rbind(
              curr_points[1:nb_points_retained, ],
              new_points
            )

          #rename rows..
          row.names(curr_points) <- 1:nrow(curr_points)

          #this part will need to be reworked to handle various cases related to stop status...
          curr_nodes[idx, ]$lng <- click$lng
          curr_nodes[idx, ]$lat <- click$lat
          curr_nodes[idx, ]$index <- max(curr_points$index)

          #move node selected for movement mid-route
        } else {
          before_idx <- idx - 1
          after_idx <- idx + 1

          #divide the nodes and route points into segments a, b, c and d
          #segments b and c (before and after the selected point that is moved to a new point)
          #will be calculated later
          #index values in segment d need to be adjusted based on the difference of
          #segment bc before and after

          #segment A :
          nodes_a <- curr_nodes[1:before_idx, ]
          nodes_a_idx_max <- max(nodes_a$index)

          points_a <- curr_points[1:nodes_a_idx_max, ]

          #segment D :
          nodes_d <- curr_nodes[after_idx:nrow(curr_nodes), ]
          nodes_d_idx_min <- min(nodes_d$index)

          points_d <- curr_points[nodes_d_idx_min:nrow(curr_points), ]

          #number of points in BC before :
          nb_points_bc_before <-
            min(points_d$index) - max(points_a$index) - 1
          #specifically the points BETWEEN segments a and d,
          #excluding the last point of a and the first point of d

          #generate segment B

          #from the end of segment a to the new stop
          from_point <- c(
            curr_nodes[before_idx, ]$lng,
            curr_nodes[before_idx, ]$lat
          )
          to_point <- c(click$lng, click$lat)

          segment_b <- generateRouteSegment(
            from_point,
            to_point,
            drawing_mode = input$drawing_mode
          )

          #EXCLUDING the first point which is included in points_a
          points_b <-
            segment_b[2:nrow(segment_b), ] |>
            mutate(index = row_number() + nodes_a_idx_max, .before = "lng")

          #point b index max will be the new index value for the moved node AND added stop
          #and will serve to adjust the index values of points_d and nodes_d

          points_b_idx_max <-
            max(points_b$index)

          #generate segment C

          #from new stop to beginning of segment d

          from_point <- c(click$lng, click$lat)
          to_point <- c(
            curr_nodes[after_idx, ]$lng,
            curr_nodes[after_idx, ]$lat
          )

          segment_c <- generateRouteSegment(
            from_point,
            to_point,
            drawing_mode = input$drawing_mode
          )

          points_c <-
            segment_c[2:(nrow(segment_c) - 1), ] |> #EXCLUDING the first and the last point,
            #which are included in the other segments already
            mutate(index = row_number() + points_b_idx_max, .before = "lng")

          points_bc <- rbind(points_b, points_c)

          #adjust index values for points and nodes d :

          nb_points_bc_after <- nrow(points_bc)

          adj_index_d <- nb_points_bc_after - nb_points_bc_before
          #this will help add or subtract from the index values, depending on if the new segment bc
          #has more or fewer points than the old segment bc

          points_d <-
            points_d |>
            mutate(index = index + adj_index_d)

          nodes_d <-
            nodes_d |>
            mutate(index = index + adj_index_d)
          #called nodes_d but it's really just the third set of nodes
          #nodes_a, new node, and nodes_d

          node_bc <-
            data.frame(
              node_id = idx,
              lng = click$lng,
              lat = click$lat,
              is_stop = FALSE, #this is a waypoint if it's selected and being moved
              #not possible to select a stop node for movement
              stop_id = "",
              stop_name = "",
              speed_factor = NA_real_, #setting speed factor to na_real_
              index = points_b_idx_max
            )

          #reconstitute curr_points and curr_nodes

          curr_points <-
            rbind(points_a, points_b, points_c, points_d)

          curr_nodes <-
            rbind(nodes_a, node_bc, nodes_d)

          #rename rows for good form
          row.names(curr_points) <- 1:nrow(curr_points)
          row.names(curr_nodes) <- 1:nrow(curr_nodes)
        }

        #reactive value update, notification, reset selected point index to null
        route_points(curr_points)
        route_nodes(curr_nodes)
        selected_point_index(NULL) # Reset selection
        showNotification("Waypoint moved", type = "message")
      } else if (nrow(curr_nodes) >= 1) {
        if (nrow(curr_nodes) >= 2) {
          #check if click is near a segment, and if so, add point along segment

          point_added <- FALSE

          for (i in 1:(nrow(curr_points) - 1)) {
            # Get segment endpoints
            p1 <- curr_points[i, ]
            p2 <- curr_points[i + 1, ]

            # Calculate distance from click to line segment
            d <- abs(
              (p2$lat - p1$lat) *
                click$lng -
                (p2$lng - p1$lng) * click$lat +
                p2$lng * p1$lat -
                p2$lat * p1$lng
            ) /
              sqrt((p2$lat - p1$lat)^2 + (p2$lng - p1$lng)^2)

            # Also check if click is within the bounding box of the segment
            within_bounds <- (min(p1$lng, p2$lng) <= click$lng &&
              click$lng <= max(p1$lng, p2$lng) &&
              min(p1$lat, p2$lat) <= click$lat &&
              click$lat <= max(p1$lat, p2$lat))

            # If click is close to segment and within bounds
            if (
              d < calculateThreshold(current_zoom()) &&
                within_bounds
            ) {
              new_pt_idx <- p1$index + 1

              # Create new point
              new_point <- data.frame(
                index = new_pt_idx, #index of point before, plus 1
                lng = click$lng,
                lat = click$lat
              )

              # Insert new point between segment endpoints
              new_points <- rbind(
                curr_points[1:i, ],
                new_point,
                curr_points[(i + 1):nrow(curr_points), ]
              )

              # Reindex all points to maintain sequence
              new_points$index <- 1:nrow(new_points)

              curr_points <- new_points

              #create waypoint node for this point

              #first, identify the nodes before this new point

              nodes_a <-
                curr_nodes |>
                filter(index <= i)

              nodes_b <-
                curr_nodes |>
                filter(index > i) |>
                mutate(
                  node_id = node_id + 1, #add one more node
                  index = index + 1
                ) #add one more point

              new_node <-
                data.frame(
                  node_id = max(nodes_a$node_id) + 1,
                  lng = click$lng,
                  lat = click$lat,
                  is_stop = FALSE,
                  stop_id = "",
                  stop_name = "",
                  speed_factor = NA_real_, #assigning na_real_ to speed factor
                  index = new_pt_idx
                )

              curr_nodes <- rbind(nodes_a, new_node, nodes_b)

              #rename rows for good form
              row.names(curr_points) <- 1:nrow(curr_points)
              row.names(curr_nodes) <- 1:nrow(curr_nodes)

              point_added <- TRUE

              #update the reactive values
              route_points(curr_points)
              route_nodes(curr_nodes)

              showNotification("Waypoint added along route", type = "message")

              break
            }
          }

          if (!point_added) {
            #SUPER REDUNDANT ! repeats because I don't know how to better organise the conditions
            #add second waypoint

            #get the existing max index from the nodes
            nodes_a_idx_max <- max(curr_nodes$index)

            #Route a new segment from the last existing node to the new node
            #using the last existing node coords as from_point and
            #stop_coords from clicked stop as to_point
            from_lng <- curr_nodes[nrow(curr_nodes), ]$lng
            from_lat <- curr_nodes[nrow(curr_nodes), ]$lat

            from_point <- c(from_lng, from_lat)
            to_point <- c(click$lng, click$lat)

            new_segment <- generateRouteSegment(
              from_point,
              to_point,
              drawing_mode = input$drawing_mode
            )

            new_points <-
              new_segment[2:nrow(new_segment), ] |> #EXCLUDING the first point
              #which is included in the previous segment already
              mutate(index = row_number() + nodes_a_idx_max, .before = "lng")

            curr_points <- rbind(curr_points, new_points)

            #just in case, rename curr_point row names
            row.names(curr_points) <- 1:nrow(curr_points)

            #max index of new curr_points becomes the index of new node
            #(associated with stop_coords)

            new_node_index <- max(curr_points$index)

            # Create new node
            new_node <- data.frame(
              node_id = max(curr_nodes$node_id) + 1,
              lng = click$lng,
              lat = click$lat,
              is_stop = FALSE,
              stop_id = "",
              stop_name = "",
              speed_factor = NA_real_,
              index = new_node_index,
              stringsAsFactors = FALSE
            )

            # Add to nodes
            curr_nodes <- rbind(curr_nodes, new_node)

            #curr_nodes |> st_as_sf(coords=c("lng","lat"),crs=4326) |> mapview()

            route_points(curr_points)
            route_nodes(curr_nodes)
          }
        } else if (nrow(curr_nodes) == 1) {
          #add second waypoint

          #get the existing max index from the nodes
          nodes_a_idx_max <- max(curr_nodes$index)

          #Route a new segment from the last existing node to the new node
          #using the last existing node coords as from_point and
          #stop_coords from clicked stop as to_point
          from_lng <- curr_nodes[nrow(curr_nodes), ]$lng
          from_lat <- curr_nodes[nrow(curr_nodes), ]$lat

          from_point <- c(from_lng, from_lat)
          to_point <- c(click$lng, click$lat)

          new_segment <- generateRouteSegment(
            from_point,
            to_point,
            drawing_mode = input$drawing_mode
          )

          new_points <-
            new_segment[2:nrow(new_segment), ] |> #EXCLUDING the first point
            #which is included in the previous segment already
            mutate(index = row_number() + nodes_a_idx_max, .before = "lng")

          curr_points <- rbind(curr_points, new_points)

          #just in case, rename curr_point row names
          row.names(curr_points) <- 1:nrow(curr_points)

          #max index of new curr_points becomes the index of new node
          #(associated with stop_coords)

          new_node_index <- max(curr_points$index)

          # Create new node
          new_node <- data.frame(
            node_id = max(curr_nodes$node_id) + 1,
            lng = click$lng,
            lat = click$lat,
            is_stop = FALSE,
            stop_id = "",
            stop_name = "",
            speed_factor = NA_real_,
            index = new_node_index,
            stringsAsFactors = FALSE
          )

          # Add to nodes
          curr_nodes <- rbind(curr_nodes, new_node)

          #curr_nodes |> st_as_sf(coords=c("lng","lat"),crs=4326) |> mapview()

          route_points(curr_points)
          route_nodes(curr_nodes)
        } else {
          #there are no nodes or waypoints yet. Do nothing other than prompt the user
          #to start the route with a stop (routes should start at stops)

          showNotification(
            "Click on a stop to start your route",
            type = "warning"
          )
        }
      }
    })

    # Right-click handler to remove nodes
    observeEvent(input$routes_map_right_click, {
      req(active_itin_id())

      click <- input$routes_map_right_click
      curr_nodes <- route_nodes()
      curr_points <- route_points()

      if (nrow(curr_nodes) == 0) {
        return()
      }

      # Find closest node
      distances <- sqrt(
        (curr_nodes$lng - click$lng)^2 + (curr_nodes$lat - click$lat)^2
      )
      closest_idx <- which.min(distances)

      # If click is close enough to a node, remove it
      if (distances[closest_idx] < calculateThreshold(current_zoom())) {
        # Check node position for specialized handling

        # REMOVING FIRST NODE OR ONLY NODE
        if (closest_idx == 1) {
          #REMOVING FIRST NODE
          if (nrow(curr_nodes) > 1) {
            # Remove the node
            curr_nodes <- curr_nodes[-closest_idx, ]

            # Re-number remaining nodes
            curr_nodes$node_id <- 1:nrow(curr_nodes)

            #determine adjustment of index by number of points
            #based on the current index value for the new first node
            #in order to bring this value to 1 and have the others within nodes and routes_points adjust accordingly

            index_adj <- curr_nodes[1, ]$index - 1

            curr_nodes <- curr_nodes |> mutate(index = index - index_adj)

            curr_points <-
              curr_points[(index_adj + 1):nrow(curr_points), ] |>
              mutate(index = row_number())

            #resetting row names, just in case.

            row.names(curr_nodes) <- 1:nrow(curr_nodes)
            row.names(curr_points) <- 1:nrow(curr_points)

            #update reactive values
            route_nodes(curr_nodes)
            route_points(curr_points)
          } else {
            # Removing only node, clear everything
            route_nodes(data.frame(
              node_id = integer(),
              lng = numeric(),
              lat = numeric(),
              is_stop = logical(),
              stop_id = character(),
              stop_name = character(),
              speed_factor = double(),
              index = integer(),
              stringsAsFactors = FALSE
            ))
            route_points(data.frame(
              index = integer(),
              lng = numeric(),
              lat = numeric()
            ))
          }

          #REMOVING LAST NODE
        } else if (closest_idx == nrow(curr_nodes)) {
          # Remove the node
          curr_nodes <- curr_nodes[-closest_idx, ]

          #(below is recycled from backspace handler)
          #(in principle, the previous condition should handle cases where input is 1 node and output is 0)
          #just in case, rename remaining rows
          row.names(curr_nodes) <- 1:nrow(curr_nodes)

          if (nrow(curr_nodes) > 0) {
            #if at least 1 node is left, then
            #identify new max index on the line and reduce route points to this
            max_index <-
              max(curr_nodes$index)

            curr_points <-
              curr_points |>
              filter(index <= max_index)

            #just in case, rename remaining rows
            row.names(curr_points) <- 1:nrow(curr_points)
          } else {
            #if no nodes left, then clear points
            curr_points <- data.frame(
              index = integer(),
              lng = numeric(),
              lat = numeric()
            )
          }

          route_nodes(curr_nodes)
          route_points(curr_points)

          #REMOVING NODE IN MIDDLE
        } else if (nrow(curr_nodes) > 2) {
          # Identify the nodes before and after this one
          before_idx <- closest_idx - 1
          after_idx <- closest_idx + 1

          #divide the nodes and route points into segments a, b and c
          #segment b is the one that will be calculated after
          #index values in segment c need to be adjusted based on the difference
          #in the number of points in segment b before and after

          #segment A :
          nodes_a <- curr_nodes[1:before_idx, ]
          nodes_a_idx_max <- max(nodes_a$index)

          points_a <- curr_points[1:nodes_a_idx_max, ]

          #segment C :
          nodes_c <- curr_nodes[after_idx:nrow(curr_nodes), ]
          nodes_c_idx_min <- min(nodes_c$index)

          points_c <- curr_points[nodes_c_idx_min:nrow(curr_points), ]

          #number of points in B before :
          nb_points_b_before <-
            min(points_c$index) - max(points_a$index) - 1
          #specifically the points BETWEEN segments a and c,
          #excluding the last point of a and the first point of c

          #generate segment B :

          #from the node before to the node after
          from_point <- c(
            curr_nodes[before_idx, ]$lng,
            curr_nodes[before_idx, ]$lat
          )
          to_point <- c(
            curr_nodes[after_idx, ]$lng,
            curr_nodes[after_idx, ]$lat
          )

          segment_b <- generateRouteSegment(
            from_point,
            to_point,
            drawing_mode = input$drawing_mode
          )

          points_b <-
            segment_b[2:(nrow(segment_b) - 1), ] |> #EXCLUDING the first and the last point,
            #which are included in the other segments already
            mutate(index = row_number() + nodes_a_idx_max, .before = "lng")

          #adjust index for points and nodes in c :

          nb_points_b_after <- nrow(points_b)

          adj_index_c <- nb_points_b_after - nb_points_b_before
          #this will help add or subtract from the index values, depending on if the new segment b
          #has more or fewer points than the old segment b

          points_c <-
            points_c |>
            mutate(index = index + adj_index_c)

          nodes_c <-
            nodes_c |>
            mutate(index = index + adj_index_c)

          #reconstitute curr_points and curr_nodes

          curr_points <-
            rbind(points_a, points_b, points_c)

          curr_nodes <-
            rbind(nodes_a, nodes_c) |>
            mutate(node_id = row_number()) #reorder the node ids

          #rename rows, just in case
          row.names(curr_points) <- 1:nrow(curr_points)
          row.names(curr_nodes) <- 1:nrow(curr_nodes)

          #update the reactive values
          route_points(curr_points)
          route_nodes(curr_nodes)
        }

        # Update stop sequence
        current_sequence(generateStopSequenceFromNodes())

        showNotification("Node removed", type = "message")
      }
    })

    # Backspace key handler
    observeEvent(input$backspace_pressed, {
      curr_nodes <- route_nodes()
      curr_points <- route_points()

      #backspace can only trigger removal of nodes if there is at least one, obviously.
      #could be interesting to develop :
      #condition that backspace + active node selected means that it removes the active node
      #otherwise, it will remove the last point along the sequence.

      if (nrow(curr_nodes) > 0) {
        curr_nodes <-
          curr_nodes[-nrow(curr_nodes), ]

        #just in case, rename remaining rows
        row.names(curr_nodes) <- 1:nrow(curr_nodes)

        if (nrow(curr_nodes) > 0) {
          #if at least 1 node is left, then
          #identify new max index on the line and reduce route points to this
          max_index <-
            max(curr_nodes$index)

          curr_points <-
            curr_points |>
            filter(index <= max_index)
        } else {
          #if no nodes left, then clear points
          curr_points <- data.frame(
            index = integer(),
            lng = numeric(),
            lat = numeric()
          )
        }

        #update reactive values
        route_nodes(curr_nodes)
        route_points(curr_points)

        # Update stop sequence
        current_sequence(generateStopSequenceFromNodes())

        showNotification("Last node removed", type = "message")
      }
    })

    # --- Stop sequence table rendering ---

    # Render stop sequence table
    output$selected_stops_table <- DT::renderDT({
      req(current_sequence())

      DT::datatable(
        current_sequence(),
        selection = 'single',
        rownames = FALSE,
        options = list(
          pageLength = -1,
          dom = 't',
          ordering = FALSE,
          columnDefs = list(
            list(visible = FALSE, targets = c(0:1, 3)), # Hide itin_id and stop_id
            list(visible = TRUE, targets = c(2, 4)) # Show stop_sequence and stop_name
          )
        )
      )
    })

    # --- clear inputs ---

    # Clear all inputs function
    clearInputs <- function() {
      active_route_id(NULL)
      active_direction_id(0L)
      active_trip_headsign("")
      editing_existing_itin(FALSE)
      itin_editing_id(NULL)
      itin_adding_for_route(NULL)

      current_sequence(data.frame(
        itin_id = character(),
        stop_id = character(),
        stop_sequence = integer(),
        stop_name = character(),
        speed_factor = double(),
        stringsAsFactors = FALSE
      ))

      route_points(data.frame(
        index = integer(),
        lng = numeric(),
        lat = numeric()
      ))

      route_nodes(data.frame(
        node_id = integer(),
        lng = numeric(),
        lat = numeric(),
        is_stop = logical(),
        stop_id = character(),
        stop_name = character(),
        speed_factor = double(),
        index = integer(),
        stringsAsFactors = FALSE
      ))

      active_itin_id(NULL)
      selected_point_index(NULL)
    }

    #   #   #
    #
    ##   CALENDAR MODULE--------
    #
    #   #   #

    # Calendar table display
    output$calendar_table <- DT::renderDT({
      current_data <- ssfs()
      DT::datatable(
        current_data$calendar,
        selection = 'single',
        rownames = FALSE,
        options = list(
          pageLength = 10,
          ordering = FALSE,
          dom = 't'
        )
      )
    })

    # Generate next default service_id
    get_next_service_id <- function() {
      current_data <- ssfs()
      if (nrow(current_data$calendar) == 0) {
        return("S1")
      }
      existing_ids <- current_data$calendar$service_id
      numeric_part <- as.integer(gsub("S", "", existing_ids))
      sprintf("S%d", max(numeric_part) + 1)
    }

    # Clear service form
    observeEvent(input$clear_service, {
      updateTextInput(session, "service_id", value = "")
      updateCheckboxInput(session, "monday", value = FALSE)
      updateCheckboxInput(session, "tuesday", value = FALSE)
      updateCheckboxInput(session, "wednesday", value = FALSE)
      updateCheckboxInput(session, "thursday", value = FALSE)
      updateCheckboxInput(session, "friday", value = FALSE)
      updateCheckboxInput(session, "saturday", value = FALSE)
      updateCheckboxInput(session, "sunday", value = FALSE)
      updateDateInput(session, "start_date", value = "2000-01-01")
      updateDateInput(session, "end_date", value = "2099-12-31")
    })

    # Add new service
    observeEvent(input$add_service, {
      current_data <- ssfs()

      # Validate service_id
      service_id <- if (input$service_id == "") {
        get_next_service_id()
      } else {
        input$service_id
      }

      # Check if service_id already exists
      if (service_id %in% current_data$calendar$service_id) {
        showNotification(
          "Service ID already exists. Please use a different ID.",
          type = "warning"
        )
        return()
      }

      # Validate dates
      start_date <- as.character(input$start_date)
      end_date <- as.character(input$end_date)

      if (start_date > end_date) {
        showNotification(
          "Start date must be before or equal to end date",
          type = "warning"
        )
        return()
      }

      # Create new service entry
      new_service <- data.frame(
        service_id = service_id,
        monday = as.integer(input$monday),
        tuesday = as.integer(input$tuesday),
        wednesday = as.integer(input$wednesday),
        thursday = as.integer(input$thursday),
        friday = as.integer(input$friday),
        saturday = as.integer(input$saturday),
        sunday = as.integer(input$sunday),
        start_date = start_date,
        end_date = end_date,
        stringsAsFactors = FALSE
      )

      # Add to calendar table
      current_data$calendar <- rbind(current_data$calendar, new_service)
      ssfs(current_data)

      # Clear form
      updateTextInput(session, "service_id", value = "")
      showNotification("Service added successfully", type = "message")
    })

    # Delete selected service
    observeEvent(input$delete_selected_service, {
      req(input$calendar_table_rows_selected)
      current_data <- ssfs()

      if (length(input$calendar_table_rows_selected) > 0) {
        current_data$calendar <- current_data$calendar[
          -input$calendar_table_rows_selected,
        ]
        ssfs(current_data)
        showNotification("Service deleted successfully", type = "message")
      }
    })

    #   #   #
    #
    ##   SPANS MODULE--------
    #
    #   #   #

    # Function to validate and format time string (from spans standalone app)
    format_time <- function(time_str) {
      # Remove any non-digit or non-colon characters
      clean_str <- gsub("[^0-9:]", "", time_str)

      # Split into components
      parts <- strsplit(clean_str, ":")[[1]]

      if (length(parts) == 1) {
        # Only hours provided
        hours <- as.numeric(parts[1])
        mins <- 0
        secs <- 0
      } else if (length(parts) == 2) {
        # Hours and minutes provided
        hours <- as.numeric(parts[1])
        mins <- as.numeric(parts[2])
        secs <- 0
      } else if (length(parts) == 3) {
        # Hours, minutes, and seconds provided
        hours <- as.numeric(parts[1])
        mins <- as.numeric(parts[2])
        secs <- as.numeric(parts[3])
      } else {
        return(NULL) # Invalid format
      }

      # Validate ranges
      if (
        hours < 0 ||
          hours > 30 ||
          mins < 0 ||
          mins > 59 ||
          secs < 0 ||
          secs > 59
      ) {
        return(NULL)
      }

      # Format as HH:MM:SS
      sprintf("%02d:%02d:%02d", hours, mins, secs)
    }

    # Helper function to get default speed based on route_type for an itin_id
    # Returns 40 for rail/metro (route_type 1, 2, 12), 20 for all others (bus)
    get_speed_for_itin_id <- function(itin_id, current_data) {
      # Get route_id from itin table
      itin_row <- current_data$itin[current_data$itin$itin_id == itin_id, ]
      if (nrow(itin_row) == 0 || !"route_id" %in% names(itin_row)) {
        return(20) # Default to bus speed
      }

      route_id <- itin_row$route_id[1]

      # Get route_type from routes table
      route_row <- current_data$routes[
        current_data$routes$route_id == route_id,
      ]
      if (nrow(route_row) == 0 || !"route_type" %in% names(route_row)) {
        return(20) # Default to bus speed
      }

      route_type <- route_row$route_type[1]

      # Return speed based on route_type
      if (!is.na(route_type) && route_type %in% c(1, 2, 12)) {
        return(40) # Rail/metro speed
      } else {
        return(20) # Bus speed
      }
    }

    # Update itin_id choices based on ssfs$itin
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
      updateSelectInput(session, "span_itin_id", choices = c("", itin_choices))
    })

    # Update service_id choices based on ssfs$calendar
    observe({
      current_data <- ssfs()
      service_choices <- if (nrow(current_data$calendar) > 0) {
        current_data$calendar$service_id
      } else {
        character(0)
      }
      updateSelectInput(
        session,
        "span_service_id",
        choices = c("", service_choices)
      )
    })

    # Spans table display
    output$spans_table <- DT::renderDT({
      current_data <- ssfs()
      DT::datatable(
        current_data$span,
        selection = 'single',
        rownames = FALSE,
        options = list(
          pageLength = -1,
          ordering = FALSE,
          dom = 't'
        )
      )
    })

    # Compute and display the next service_window value
    output$service_window_display <- renderText({
      # Require both inputs
      if (
        is.null(input$span_itin_id) ||
          input$span_itin_id == "" ||
          is.null(input$span_service_id) ||
          input$span_service_id == ""
      ) {
        return("")
      }

      current_data <- ssfs()

      # Filter existing spans for this itin_id + service_id combination
      existing_spans <- current_data$span[
        current_data$span$itin_id == input$span_itin_id &
          current_data$span$service_id == input$span_service_id,
      ]

      if (nrow(existing_spans) == 0) {
        return("1")
      } else {
        # Get the max service_window and add 1
        max_window <- max(existing_spans$service_window, na.rm = TRUE)
        return(as.character(max_window + 1))
      }
    })

    # Clear span form
    observeEvent(input$clear_span, {
      updateSelectInput(session, "span_itin_id", selected = "")
      updateSelectInput(session, "span_service_id", selected = "")
      updateTextInput(session, "first_dep", value = "05:00:00")
      updateTextInput(session, "last_dep", value = "23:00:00")
    })

    # Add new span
    observeEvent(input$add_span, {
      req(input$span_itin_id, input$span_service_id)

      # Validate times
      first_dep <- format_time(input$first_dep)
      last_dep <- format_time(input$last_dep)

      if (is.null(first_dep) || is.null(last_dep)) {
        showNotification(
          "Invalid time format. Use HH:MM:SS (00-30:00-59:00-59).",
          type = "error"
        )
        return()
      }

      if (first_dep >= last_dep) {
        showNotification(
          "First departure must be before last departure",
          type = "warning"
        )
        return()
      }

      current_data <- ssfs()

      # Filter existing spans for this itin_id + service_id combination
      existing_spans <- current_data$span[
        current_data$span$itin_id == input$span_itin_id &
          current_data$span$service_id == input$span_service_id,
      ]

      # Calculate the next service_window value
      if (nrow(existing_spans) == 0) {
        new_service_window <- 1L
      } else {
        max_window <- max(existing_spans$service_window, na.rm = TRUE)
        new_service_window <- as.integer(max_window + 1)

        # Validate that new span doesn't overlap with the previous service window
        # Get the last_dep of the previous window (the one with max service_window)
        prev_window_span <- existing_spans[
          existing_spans$service_window == max_window,
        ]
        prev_last_dep <- prev_window_span$last_dep[1]

        # Parse times for comparison (convert HH:MM:SS to seconds)
        parse_time_to_seconds <- function(time_str) {
          parts <- as.numeric(strsplit(time_str, ":")[[1]])
          return(parts[1] * 3600 + parts[2] * 60 + parts[3])
        }

        prev_last_seconds <- parse_time_to_seconds(prev_last_dep)
        new_first_seconds <- parse_time_to_seconds(first_dep)

        # New service window must start at least 1 minute after previous ends
        if (new_first_seconds <= prev_last_seconds + 59) {
          showNotification(
            paste0(
              "Service window ",
              new_service_window,
              " must start after ",
              prev_last_dep,
              " (the end of service window ",
              max_window,
              ")."
            ),
            type = "error"
          )
          return()
        }
      }

      # Create new span entry with service_window
      new_span <- data.frame(
        itin_id = input$span_itin_id,
        service_id = input$span_service_id,
        service_window = new_service_window,
        first_dep = first_dep,
        last_dep = last_dep,
        stringsAsFactors = FALSE
      )

      # Add to spans table
      current_data$span <- rbind(current_data$span, new_span)

      # --- AUTO-GENERATE HSH ENTRIES FOR THE NEW SPAN ---

      # Extract hours from first_dep and last_dep
      first_dep_hour <- as.numeric(substr(first_dep, 1, 2))
      last_dep_hour <- as.numeric(substr(last_dep, 1, 2))

      # Generate sequence of hours
      hours <- first_dep_hour:last_dep_hour

      # Format hours as HH:00:00
      formatted_hours <- sapply(hours, function(h) {
        sprintf("%02d:00:00", h)
      })

      # Get speed based on route_type (via itin -> routes lookup)
      speed_value <- get_speed_for_itin_id(input$span_itin_id, current_data)

      # Create hsh entries with NA headway
      new_hsh_entries <- data.frame(
        itin_id = rep(input$span_itin_id, length(formatted_hours)),
        service_id = rep(input$span_service_id, length(formatted_hours)),
        hour_dep = formatted_hours,
        headway = NA_integer_,
        speed = speed_value,
        stringsAsFactors = FALSE
      )

      # Add to hsh table
      current_data$hsh <- rbind(current_data$hsh, new_hsh_entries)

      # --- END AUTO-GENERATE HSH ENTRIES ---

      ssfs(current_data)

      showNotification(
        paste0(
          "Service span added (window ",
          new_service_window,
          ") with ",
          length(formatted_hours),
          " headway entries created"
        ),
        type = "message"
      )
    })

    # Delete selected span
    observeEvent(input$delete_selected_span, {
      req(input$spans_table_rows_selected)
      current_data <- ssfs()

      if (length(input$spans_table_rows_selected) > 0) {
        current_data$span <- current_data$span[
          -input$spans_table_rows_selected,
        ]
        ssfs(current_data)
        showNotification("Service span deleted successfully", type = "message")
      }
    })

    #   #   #
    #
    ## HEADWAY PRESETS MODULE-----------
    #
    #   #   #

    #reactive value :

    service_patterns <- reactiveVal(
      list(
        service_patterns = list(
          SP1 = data.frame(
            hour = c(
              "05:00:00",
              "06:00:00",
              "07:00:00",
              "08:00:00",
              "09:00:00",
              "10:00:00",
              "11:00:00",
              "12:00:00",
              "13:00:00",
              "14:00:00",
              "15:00:00",
              "16:00:00",
              "17:00:00",
              "18:00:00",
              "19:00:00",
              "20:00:00",
              "21:00:00",
              "22:00:00",
              "23:00:00",
              "24:00:00",
              "25:00:00"
            ),
            headway = c(
              15,
              5,
              5,
              5,
              5,
              8,
              8,
              8,
              8,
              8,
              5,
              5,
              5,
              5,
              12,
              12,
              15,
              15,
              15,
              15,
              15
            ),
            stringsAsFactors = FALSE
          ),
          SP2 = data.frame(
            hour = c(
              "05:00:00",
              "06:00:00",
              "07:00:00",
              "08:00:00",
              "09:00:00",
              "10:00:00",
              "11:00:00",
              "12:00:00",
              "13:00:00",
              "14:00:00",
              "15:00:00",
              "16:00:00",
              "17:00:00",
              "18:00:00",
              "19:00:00",
              "20:00:00",
              "21:00:00",
              "22:00:00",
              "23:00:00",
              "24:00:00",
              "25:00:00"
            ),
            headway = c(
              30,
              10,
              10,
              10,
              10,
              30,
              30,
              30,
              30,
              30,
              10,
              10,
              10,
              10,
              30,
              30,
              30,
              30,
              30,
              30,
              30
            ),
            stringsAsFactors = FALSE
          )
        ),
        service_pattern_names = data.frame(
          pattern_id = c("SP1", "SP2"),
          pattern_name = c("All Day Frequent", "Peak Frequent"),
          stringsAsFactors = FALSE
        )
      )
    )

    # Reactive value for editing state
    editing_sp <- reactiveVal(FALSE)
    adding_sp_hour <- reactiveVal(FALSE)

    # Make editing states available to UI
    output$editing_sp <- reactive({
      editing_sp()
    })
    shiny::outputOptions(output, "editing_sp", suspendWhenHidden = FALSE)

    output$adding_sp_hour <- reactive({
      adding_sp_hour()
    })
    shiny::outputOptions(output, "adding_sp_hour", suspendWhenHidden = FALSE)

    # Reactive value for the currently loaded pattern data (for viewing/editing)
    loaded_sp_data <- reactiveVal(NULL)
    loaded_sp_id <- reactiveVal(NULL)

    # Reactive value for new pattern being created
    new_sp_data <- reactiveVal(data.frame(
      hour = character(),
      headway = integer(),
      stringsAsFactors = FALSE
    ))

    # Update service pattern dropdown choices
    observe({
      current_data <- service_patterns()
      if (
        !is.null(current_data$service_pattern_names) &&
          nrow(current_data$service_pattern_names) > 0
      ) {
        choices <- setNames(
          current_data$service_pattern_names$pattern_id,
          paste0(
            current_data$service_pattern_names$pattern_id,
            " - ",
            current_data$service_pattern_names$pattern_name
          )
        )
        updateSelectInput(session, "sp_select", choices = choices)
      }
    })

    # Load selected pattern
    observeEvent(input$sp_load, {
      req(input$sp_select)
      current_data <- service_patterns()

      pattern_id <- input$sp_select
      if (pattern_id %in% names(current_data$service_patterns)) {
        loaded_sp_data(current_data$service_patterns[[pattern_id]])
        loaded_sp_id(pattern_id)
        editing_sp(FALSE)
        adding_sp_hour(FALSE)
        showNotification(paste("Loaded pattern:", pattern_id), type = "message")
      }
    })

    # Render service patterns table
    output$sp_table <- DT::renderDT({
      if (input$sp_mode == "edit") {
        # Show loaded pattern data
        data <- loaded_sp_data()
        if (is.null(data) || nrow(data) == 0) {
          return(DT::datatable(
            data.frame(hour = character(), headway = integer()),
            selection = "single",
            rownames = FALSE,
            options = list(pageLength = 25, dom = "t"),
            colnames = c("Hour", "Headway (min)")
          ))
        }

        DT::datatable(
          data,
          selection = "single",
          rownames = FALSE,
          options = list(
            pageLength = 25,
            ordering = TRUE,
            order = list(list(0, "asc")),
            dom = "t"
          ),
          colnames = c("Hour", "Headway (min)")
        )
      } else {
        # Show new pattern being created
        data <- new_sp_data()
        if (nrow(data) == 0) {
          return(DT::datatable(
            data.frame(hour = character(), headway = integer()),
            selection = "single",
            rownames = FALSE,
            options = list(pageLength = 25, dom = "t"),
            colnames = c("Hour", "Headway (min)")
          ))
        }

        DT::datatable(
          data,
          selection = "single",
          rownames = FALSE,
          options = list(
            pageLength = 25,
            ordering = TRUE,
            order = list(list(0, "asc")),
            dom = "t"
          ),
          colnames = c("Hour", "Headway (min)")
        )
      }
    })

    # Edit row button handler
    observeEvent(input$sp_edit_row, {
      req(input$sp_table_rows_selected)
      data <- loaded_sp_data()
      req(data, nrow(data) > 0)

      selected_row <- data[input$sp_table_rows_selected, ]
      updateNumericInput(
        session,
        "sp_edit_headway",
        value = selected_row$headway
      )
      editing_sp(TRUE)
      adding_sp_hour(FALSE)
    })

    # Save edit handler
    observeEvent(input$sp_save_edit, {
      req(input$sp_table_rows_selected)
      req(input$sp_edit_headway >= 1)

      data <- loaded_sp_data()
      pattern_id <- loaded_sp_id()
      req(data, pattern_id)

      # Update the headway value
      data$headway[input$sp_table_rows_selected] <- input$sp_edit_headway

      # Update loaded data
      loaded_sp_data(data)

      # Update service patterns
      current_data <- service_patterns()
      current_data$service_patterns[[pattern_id]] <- data
      service_patterns(current_data)

      editing_sp(FALSE)
      showNotification("Changes saved successfully", type = "message")
    })

    # Cancel edit handler
    observeEvent(input$sp_cancel_edit, {
      editing_sp(FALSE)
    })

    # Add new hour button handler
    observeEvent(input$sp_add_row, {
      data <- loaded_sp_data()

      # Generate list of available hours (not already in the pattern)
      all_hours <- sprintf("%02d:00:00", 0:30)
      if (!is.null(data) && nrow(data) > 0) {
        available_hours <- setdiff(all_hours, data$hour)
      } else {
        available_hours <- all_hours
      }

      if (length(available_hours) == 0) {
        showNotification(
          "All hours (0-30) are already in the pattern",
          type = "warning"
        )
        return()
      }

      updateSelectInput(session, "sp_new_hour", choices = available_hours)
      updateNumericInput(session, "sp_new_headway", value = 15)
      adding_sp_hour(TRUE)
      editing_sp(FALSE)
    })

    # Confirm add hour handler
    observeEvent(input$sp_confirm_add, {
      req(input$sp_new_hour, input$sp_new_headway >= 1)

      data <- loaded_sp_data()
      pattern_id <- loaded_sp_id()
      req(pattern_id)

      # Check if hour already exists
      if (!is.null(data) && input$sp_new_hour %in% data$hour) {
        showNotification(
          "This hour already exists in the pattern",
          type = "error"
        )
        return()
      }

      # Create new row
      new_row <- data.frame(
        hour = input$sp_new_hour,
        headway = as.integer(input$sp_new_headway),
        stringsAsFactors = FALSE
      )

      # Add to data
      if (is.null(data) || nrow(data) == 0) {
        data <- new_row
      } else {
        data <- rbind(data, new_row)
      }

      # Sort by hour
      data <- data[order(data$hour), ]

      # Update loaded data
      loaded_sp_data(data)

      # Update service patterns
      current_data <- service_patterns()
      current_data$service_patterns[[pattern_id]] <- data
      service_patterns(current_data)

      adding_sp_hour(FALSE)
      showNotification("Hour added successfully", type = "message")
    })

    # Cancel add handler
    observeEvent(input$sp_cancel_add, {
      adding_sp_hour(FALSE)
    })

    # Delete selected row handler
    observeEvent(input$sp_delete_row, {
      req(input$sp_table_rows_selected)

      data <- loaded_sp_data()
      pattern_id <- loaded_sp_id()
      req(data, pattern_id, nrow(data) > 0)

      # Remove the selected row
      data <- data[-input$sp_table_rows_selected, ]

      # Update loaded data
      loaded_sp_data(data)

      # Update service patterns
      current_data <- service_patterns()
      current_data$service_patterns[[pattern_id]] <- data
      service_patterns(current_data)

      showNotification("Row deleted successfully", type = "message")
    })

    #CREATE NEW PATTERN HANDLERS

    # Update available hours for new pattern creation
    observe({
      if (input$sp_mode == "new") {
        data <- new_sp_data()
        all_hours <- sprintf("%02d:00:00", 0:30)
        if (nrow(data) > 0) {
          available_hours <- setdiff(all_hours, data$hour)
        } else {
          available_hours <- all_hours
        }
        updateSelectInput(session, "sp_create_hour", choices = available_hours)
      }
    })

    # Add hour to new pattern
    observeEvent(input$sp_create_add_hour, {
      req(input$sp_create_hour, input$sp_create_headway >= 1)

      data <- new_sp_data()

      # Check if hour already exists
      if (input$sp_create_hour %in% data$hour) {
        showNotification(
          "This hour already exists in the pattern",
          type = "error"
        )
        return()
      }

      # Create new row
      new_row <- data.frame(
        hour = input$sp_create_hour,
        headway = as.integer(input$sp_create_headway),
        stringsAsFactors = FALSE
      )

      # Add to data
      if (nrow(data) == 0) {
        data <- new_row
      } else {
        data <- rbind(data, new_row)
      }

      # Sort by hour
      data <- data[order(data$hour), ]

      new_sp_data(data)
      showNotification("Hour added to new pattern", type = "message")
    })

    # Remove hour from new pattern
    observeEvent(input$sp_create_remove_hour, {
      req(input$sp_table_rows_selected)

      data <- new_sp_data()
      req(nrow(data) > 0)

      data <- data[-input$sp_table_rows_selected, ]
      new_sp_data(data)

      showNotification("Hour removed from new pattern", type = "message")
    })

    # Save new pattern
    observeEvent(input$sp_save_new, {
      req(input$sp_new_name)

      data <- new_sp_data()
      if (nrow(data) == 0) {
        showNotification(
          "Please add at least one hour to the pattern",
          type = "error"
        )
        return()
      }

      if (nchar(trimws(input$sp_new_name)) == 0) {
        showNotification("Please provide a pattern name", type = "error")
        return()
      }

      current_data <- service_patterns()

      # Generate new pattern ID
      existing_ids <- names(current_data$service_patterns)
      # Extract numeric parts and find max
      nums <- as.numeric(gsub("SP", "", existing_ids))
      next_num <- max(nums, na.rm = TRUE) + 1
      new_id <- paste0("SP", next_num)

      # Add new pattern to service_patterns list
      current_data$service_patterns[[new_id]] <- data

      # Add to service_pattern_names
      new_name_row <- data.frame(
        pattern_id = new_id,
        pattern_name = trimws(input$sp_new_name),
        stringsAsFactors = FALSE
      )
      current_data$service_pattern_names <- rbind(
        current_data$service_pattern_names,
        new_name_row
      )

      service_patterns(current_data)

      # Clear the new pattern form
      new_sp_data(data.frame(
        hour = character(),
        headway = integer(),
        stringsAsFactors = FALSE
      ))
      updateTextInput(session, "sp_new_name", value = "")

      # Switch to edit mode and load the new pattern
      updateRadioButtons(session, "sp_mode", selected = "edit")

      showNotification(
        paste("Pattern", new_id, "-", input$sp_new_name, "saved successfully"),
        type = "message"
      )
    })

    # Clear new pattern form
    observeEvent(input$sp_clear_new, {
      new_sp_data(data.frame(
        hour = character(),
        headway = integer(),
        stringsAsFactors = FALSE
      ))
      updateTextInput(session, "sp_new_name", value = "")
      showNotification("Form cleared", type = "message")
    })

    # Reset states when switching modes
    observeEvent(input$sp_mode, {
      editing_sp(FALSE)
      adding_sp_hour(FALSE)
      if (input$sp_mode == "new") {
        loaded_sp_data(NULL)
        loaded_sp_id(NULL)
      }
    })

    #   #   #
    #
    ##   HEADWAYS AND SPEEDS BY HOUR MODULE----------
    #
    #   #   #

    # Headways module server functions

    #create reactive value for editing state
    editing_hsh <- reactiveVal(FALSE)

    # Add this to make the editing state visible to the UI
    output$editing_hsh <- reactive({
      editing_hsh()
    })
    shiny::outputOptions(output, "editing_hsh", suspendWhenHidden = FALSE)

    #make the editing state available to the UI
    observe({
      updateQueryString(paste0("?editing_hsh=", editing_hsh()))
    })

    # Update itin_id and service_id choices
    observe({
      current_data <- ssfs()

      # Update itin_id choices, preserving current selection
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
      current_itin <- isolate(input$hsh_itin_id)
      selected_itin <- if (
        !is.null(current_itin) && current_itin %in% itin_choices
      ) {
        current_itin
      } else {
        ""
      }
      updateSelectInput(
        session,
        "hsh_itin_id",
        choices = c("", itin_choices),
        selected = selected_itin
      )

      # Update service_id choices, preserving current selection
      service_choices <- if (nrow(current_data$calendar) > 0) {
        current_data$calendar$service_id
      } else {
        character(0)
      }
      current_service <- isolate(input$hsh_service_id)
      selected_service <- if (
        !is.null(current_service) && current_service %in% service_choices
      ) {
        current_service
      } else {
        ""
      }
      updateSelectInput(
        session,
        "hsh_service_id",
        choices = c("", service_choices),
        selected = selected_service
      )
    })

    # Initialize HSH table
    observeEvent(input$initialize_hsh, {
      current_data <- ssfs()
      if (nrow(current_data$span) == 0) {
        showNotification(
          "No span data available to initialize table",
          type = "warning"
        )
        return()
      }

      # Validate that all itin_ids in span exist in itin table
      invalid_itins <- setdiff(
        current_data$span$itin_id,
        current_data$itin$itin_id
      )
      if (length(invalid_itins) > 0) {
        showNotification(
          sprintf(
            "Some route variants in spans are not defined in routes: %s",
            paste(invalid_itins, collapse = ", ")
          ),
          type = "error"
        )
        return()
      }

      new_hsh <- generate_hsh_table(
        current_data$span,
        current_data$itin,
        input$default_headway
      )

      current_data$hsh <- new_hsh
      ssfs(current_data)
      showNotification(
        "Headways table initialized successfully",
        type = "message"
      )
    })

    # Reactive value for editing state
    editing_hsh <- reactiveVal(FALSE)

    # Load and display HSH data
    observeEvent(input$load_hsh, {
      req(input$hsh_itin_id, input$hsh_service_id)
      editing_hsh(FALSE)
    })

    # Render HSH table
    output$hsh_table <- DT::renderDT({
      req(input$hsh_itin_id, input$hsh_service_id)
      current_data <- ssfs()

      filtered_data <- current_data$hsh[
        current_data$hsh$itin_id == input$hsh_itin_id &
          current_data$hsh$service_id == input$hsh_service_id,
      ]

      # Create display data frame without itin_id and service_id
      display_data <- filtered_data[, c("hour_dep", "headway", "speed")]

      DT::datatable(
        display_data,
        selection = 'single',
        rownames = FALSE,
        options = list(
          pageLength = 24,
          ordering = TRUE,
          order = list(list(0, 'asc')),
          # Order by hour_dep (now index 0)
          dom = 't'
        ),
        colnames = c("Hour", "Headway (min)", "Speed (km/h)")
      )
    })

    # Add new row handler
    observeEvent(input$add_hsh_row, {
      current_data <- ssfs()

      # Get span data for validation
      span_data <- current_data$span[
        current_data$span$itin_id == input$hsh_itin_id &
          current_data$span$service_id == input$hsh_service_id,
      ]

      if (nrow(span_data) == 0) {
        showNotification(
          "No span data found for this route variant and service",
          type = "error"
        )
        return()
      }

      # Clear form inputs
      updateTextInput(session, "edit_hour_dep", value = "")
      updateNumericInput(session, "edit_headway", value = input$default_headway)

      # Get default speed based on route type
      route_type <- current_data$itin$route_type[
        current_data$itin$itin_id == input$hsh_itin_id
      ]
      default_speed <- if (
        length(route_type) > 0 &&
          route_type %in% c(1, 2, 12)
      ) {
        40
      } else {
        20
      }
      updateNumericInput(session, "edit_speed", value = default_speed)

      # Show edit form
      editing_hsh(TRUE)
    })

    # Save edits handler
    observeEvent(input$save_hsh_edit, {
      current_data <- ssfs()

      # Validate hour format is HH:00:00
      if (!grepl("^\\d{2}:00:00$", input$edit_hour_dep)) {
        showNotification("Invalid hour format. Use HH:00:00", type = "error")
        return()
      }

      # Get span data for validation
      span_data <- current_data$span[
        current_data$span$itin_id == input$hsh_itin_id &
          current_data$span$service_id == input$hsh_service_id,
      ]

      # Extract hours for comparison
      edit_hour <- as.numeric(substr(input$edit_hour_dep, 1, 2))
      first_hour <- as.numeric(substr(span_data$first_dep, 1, 2))
      last_hour <- as.numeric(substr(span_data$last_dep, 1, 2))

      # Validate hour is within span range
      if (edit_hour < first_hour || edit_hour > last_hour) {
        showNotification("Hour must be within span range", type = "error")
        return()
      }

      # Create new row
      new_row <- data.frame(
        itin_id = input$hsh_itin_id,
        service_id = input$hsh_service_id,
        hour_dep = input$edit_hour_dep,
        headway = input$edit_headway,
        speed = input$edit_speed,
        stringsAsFactors = FALSE
      )

      # Remove existing row if editing
      if (length(input$hsh_table_rows_selected) > 0) {
        filtered_data <- current_data$hsh[
          current_data$hsh$itin_id == input$hsh_itin_id &
            current_data$hsh$service_id == input$hsh_service_id,
        ]
        row_to_edit <- filtered_data[input$hsh_table_rows_selected, ]

        current_data$hsh <- current_data$hsh[
          !(current_data$hsh$itin_id == input$hsh_itin_id &
            current_data$hsh$service_id == input$hsh_service_id &
            current_data$hsh$hour_dep == row_to_edit$hour_dep),
        ]
      }

      # Add new/updated row
      current_data$hsh <- rbind(current_data$hsh, new_row)
      current_data$hsh <- current_data$hsh[
        order(
          current_data$hsh$itin_id,
          current_data$hsh$service_id,
          current_data$hsh$hour_dep
        ),
      ]

      ssfs(current_data)
      editing_hsh(FALSE)
      showNotification("Changes saved successfully", type = "message")
    })

    # Cancel edit handler
    observeEvent(input$cancel_hsh_edit, {
      editing_hsh(FALSE)
    })

    # Delete selected handler
    observeEvent(input$delete_selected_hsh, {
      req(input$hsh_table_rows_selected)
      current_data <- ssfs()

      filtered_data <- current_data$hsh[
        current_data$hsh$itin_id == input$hsh_itin_id &
          current_data$hsh$service_id == input$hsh_service_id,
      ]

      row_to_delete <- filtered_data[input$hsh_table_rows_selected, ]

      current_data$hsh <- current_data$hsh[
        !(current_data$hsh$itin_id == input$hsh_itin_id &
          current_data$hsh$service_id == input$hsh_service_id &
          current_data$hsh$hour_dep == row_to_delete$hour_dep),
      ]

      ssfs(current_data)
      showNotification("Row deleted successfully", type = "message")
    })

    # Edit row handler
    observeEvent(input$edit_hsh_row, {
      req(input$hsh_table_rows_selected)
      current_data <- ssfs()
      filtered_data <- current_data$hsh[
        current_data$hsh$itin_id == input$hsh_itin_id &
          current_data$hsh$service_id == input$hsh_service_id,
      ]

      # Get values from selected row
      selected_row <- filtered_data[input$hsh_table_rows_selected, ]

      # Update form inputs with current values
      updateTextInput(session, "edit_hour_dep", value = selected_row$hour_dep)
      updateNumericInput(session, "edit_headway", value = selected_row$headway)
      updateNumericInput(session, "edit_speed", value = selected_row$speed)

      # Show edit form
      editing_hsh(TRUE)
    })

    # New code for applying service patterns

    # Update service pattern choices in headways module
    observe({
      current_data <- service_patterns()
      if (
        !is.null(current_data$service_pattern_names) &&
          nrow(current_data$service_pattern_names) > 0
      ) {
        choices <- setNames(
          current_data$service_pattern_names$pattern_id,
          paste0(
            current_data$service_pattern_names$pattern_id,
            " - ",
            current_data$service_pattern_names$pattern_name
          )
        )
        updateSelectInput(
          session,
          "hsh_apply_pattern",
          choices = c("", choices)
        )
      }
    })

    # Apply service pattern to current itin_id x service_id combination
    observeEvent(input$hsh_apply_pattern_btn, {
      req(input$hsh_itin_id, input$hsh_service_id, input$hsh_apply_pattern)

      if (input$hsh_apply_pattern == "") {
        showNotification("Please select a pattern to apply", type = "warning")
        return()
      }

      current_service_patterns <- service_patterns()
      current_data <- ssfs()
      pattern_id <- input$hsh_apply_pattern

      # Validate pattern exists
      if (!pattern_id %in% names(current_service_patterns$service_patterns)) {
        showNotification("Selected pattern not found", type = "error")
        return()
      }

      # Get existing hsh entries for this itin_id x service_id
      existing_hsh <- current_data$hsh[
        current_data$hsh$itin_id == input$hsh_itin_id &
          current_data$hsh$service_id == input$hsh_service_id,
      ]

      if (nrow(existing_hsh) == 0) {
        showNotification(
          "No headway entries found for this combination. Please define spans first.",
          type = "error"
        )
        return()
      }

      # Get the pattern data
      pattern_data <- current_service_patterns$service_patterns[[pattern_id]]

      # Create a lookup of pattern headways by hour
      pattern_headways <- setNames(pattern_data$headway, pattern_data$hour)

      # Find which hours in the existing hsh have a matching hour in the pattern
      hours_to_update <- intersect(existing_hsh$hour_dep, pattern_data$hour)

      if (length(hours_to_update) == 0) {
        showNotification(
          "No hours in the selected pattern overlap with existing headway entries",
          type = "warning"
        )
        return()
      }

      # Update only the headway values for matching hours, preserving everything else
      # First, identify the rows in the main hsh table that match this itin_id x service_id
      match_idx <- which(
        current_data$hsh$itin_id == input$hsh_itin_id &
          current_data$hsh$service_id == input$hsh_service_id
      )

      # For each matching row, check if its hour_dep is in the pattern and update headway if so
      for (idx in match_idx) {
        hour <- current_data$hsh$hour_dep[idx]
        if (hour %in% names(pattern_headways)) {
          current_data$hsh$headway[idx] <- pattern_headways[[hour]]
        }
      }

      ssfs(current_data)

      pattern_name <- current_service_patterns$service_pattern_names$pattern_name[
        current_service_patterns$service_pattern_names$pattern_id == pattern_id
      ]
      showNotification(
        paste(
          "Applied pattern:",
          pattern_name,
          "- Updated",
          length(hours_to_update),
          "hour entries"
        ),
        type = "message"
      )
    })

    # Update batch speed default when loading a new itin_id + service_id
    observeEvent(
      input$load_hsh,
      {
        req(input$hsh_itin_id, input$hsh_service_id)
        current_data <- ssfs()

        # Get current speed from first row of the filtered hsh, fall back to mode-based default
        filtered <- current_data$hsh[
          current_data$hsh$itin_id == input$hsh_itin_id &
            current_data$hsh$service_id == input$hsh_service_id,
        ]

        if (nrow(filtered) > 0 && !is.na(filtered$speed[1])) {
          current_speed <- filtered$speed[1]
        } else {
          current_speed <- get_speed_for_itin_id(
            input$hsh_itin_id,
            current_data
          )
        }

        updateNumericInput(session, "hsh_batch_speed", value = current_speed)
      },
      priority = -1
    )

    # Apply batch speed to all hours for current itin_id + service_id
    observeEvent(input$hsh_apply_batch_speed, {
      req(input$hsh_itin_id, input$hsh_service_id, input$hsh_batch_speed)
      current_data <- ssfs()

      match_idx <- which(
        current_data$hsh$itin_id == input$hsh_itin_id &
          current_data$hsh$service_id == input$hsh_service_id
      )

      if (length(match_idx) == 0) {
        showNotification(
          "No rows found for this selection. Load data first.",
          type = "warning"
        )
        return()
      }

      current_data$hsh$speed[match_idx] <- input$hsh_batch_speed
      ssfs(current_data)

      showNotification(
        paste0(
          "Speed set to ",
          input$hsh_batch_speed,
          " km/h for all ",
          length(match_idx),
          " hours"
        ),
        type = "message"
      )
    })

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
        type = 'scatter',
        mode = 'lines+markers',
        marker = list(size = 12, color = '#124559'),
        line = list(color = '#124559', width = 2)
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
      return(round(values, 1))
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

        current_gtfs <- ssfs_to_gtfs(
          current_ssfs,
          dist_traveled = input$include_dist_traveled
        )

        gtfstools::write_gtfs(current_gtfs, file)
      }
    )
  }

  #APP----------------------------

  #run the app
  shinyApp(ui = ui, server = server)
}
