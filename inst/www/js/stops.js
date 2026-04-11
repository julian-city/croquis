// Stop search input handler
$(document).on('input', '#stop_search', function() {
  Shiny.setInputValue('stop_search_term', $(this).val(), {priority: 'event'});
});

// Start editing a stop from the list
function editStopFromList(stopId) {
  Shiny.setInputValue('stop_list_edit_click', stopId, {priority: 'event'});
}

// Start adding a new stop
function startAddingStop() {
  Shiny.setInputValue('stop_list_add_click', Math.random(), {priority: 'event'});
}

// Save the currently editing stop with input values
function saveEditingStop() {
  var stopId = document.getElementById('inline_stop_id') ? document.getElementById('inline_stop_id').value : '';
  var stopName = document.getElementById('inline_stop_name') ? document.getElementById('inline_stop_name').value : '';
  Shiny.setInputValue('stop_list_save_data', {
    stop_id: stopId,
    stop_name: stopName,
    timestamp: Math.random()
  }, {priority: 'event'});
}

// Cancel editing
function cancelEditingStop() {
  Shiny.setInputValue('stop_list_cancel_click', Math.random(), {priority: 'event'});
}

// View/focus on a stop without editing
function viewStopFromList(stopId) {
  Shiny.setInputValue('stop_list_view_click', stopId, {priority: 'event'});
}

// Delete stop (trash icon on stop row)
function deleteStopFromList(stopId) {
  if (confirm('This stop will be deleted if it is not associated with any itineraries.')) {
    Shiny.setInputValue('stop_list_delete_click', {id: stopId, ts: Math.random()}, {priority: 'event'});
  }
}