// View/center itinerary on map
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