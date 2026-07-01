// Namespace prefix for routes module (set by server)
var routesNs = '';

Shiny.addCustomMessageHandler('setRoutesNs', function(ns) {
  routesNs = ns;
});

// Prevent default right-click on routes map
$(document).on('contextmenu', '#routes_map', function(e) {
  e.preventDefault();
  return false;
});

// Toggle route expand/collapse
function toggleRouteExpand(routeId) {
  Shiny.setInputValue(routesNs + 'route_list_toggle_expand', {id: routeId, ts: Math.random()}, {priority: 'event'});
}

// Edit route details (pencil icon on route row)
function editRouteFromList(routeId) {
  Shiny.setInputValue(routesNs + 'route_list_edit_click', {id: routeId, ts: Math.random()}, {priority: 'event'});
}

// Duplicate route
function copyRouteFromList(routeId) {
  Shiny.setInputValue(routesNs + 'route_list_copy_click',
    {id: routeId, ts: Math.random()}, {priority: 'event'});
}

// Delete route (trash icon on route row)
function deleteRouteFromList(routeId) {
  if (confirm('Delete this route? Itineraries must be deleted first.')) {
    Shiny.setInputValue(routesNs + 'route_list_delete_click', {id: routeId, ts: Math.random()}, {priority: 'event'});
  }
}

// Start adding a new route
function startAddingRoute() {
  Shiny.setInputValue(routesNs + 'route_list_add_click', Math.random(), {priority: 'event'});
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
  Shiny.setInputValue(routesNs + 'route_list_save_data', data, {priority: 'event'});
}

// Cancel route editing
function cancelRouteEdit() {
  Shiny.setInputValue(routesNs + 'route_list_cancel_click', Math.random(), {priority: 'event'});
}

// Backspace key handler (removes last drawn node)
$(document).on('keydown', function(e) {
  if (e.key === 'Backspace') {
    var tag = e.target.tagName.toLowerCase();
    var isEditable = (tag === 'input' || tag === 'textarea' || tag === 'select' || e.target.isContentEditable);
    if (!isEditable) {
      Shiny.setInputValue(routesNs + 'backspace_pressed', Math.random());
    }
  }
});

// Scroll to a specific route row after list rebuild
Shiny.addCustomMessageHandler('scrollToRoute', function(routeId) {
  setTimeout(function() {
    var el = document.getElementById('route-row-' + routeId);
    if (el) {
      el.scrollIntoView({ behavior: 'smooth', block: 'nearest' });
    }
  }, 100);
});

// Track editing mode to disable undo / redo
Shiny.addCustomMessageHandler('setEditingMode', function(editing) {
  window.croquis_editing_mode = !!editing;
  Shiny.setInputValue('routes_editing_active', !!editing, {priority: 'event'});
});