// Agency list interactions

// Edit agency (pencil icon)
function editAgencyFromList(agencyId) {
  Shiny.setInputValue('ag_list_edit_click', {id: agencyId, ts: Math.random()}, {priority: 'event'});
}

// Delete agency (trash icon)
function deleteAgencyFromList(agencyId) {
  if (confirm(jsTr('confirm_delete_agency'))) {
    Shiny.setInputValue('ag_list_delete_click', {id: agencyId, ts: Math.random()}, {priority: 'event'});
  }
}

// Start adding a new agency
function startAddingAgency() {
  Shiny.setInputValue('ag_list_add_click', Math.random(), {priority: 'event'});
}

// Save agency from inline form
function saveAgencyFromForm() {
  var data = {
    agency_id:       document.getElementById('inline_ag_agency_id')       ? document.getElementById('inline_ag_agency_id').value       : '',
    agency_name:     document.getElementById('inline_ag_agency_name')     ? document.getElementById('inline_ag_agency_name').value     : '',
    agency_url:      document.getElementById('inline_ag_agency_url')      ? document.getElementById('inline_ag_agency_url').value      : '',
    agency_timezone: document.getElementById('inline_ag_agency_timezone') ? document.getElementById('inline_ag_agency_timezone').value : '',
    ts: Math.random()
  };
  Shiny.setInputValue('ag_list_save_data', data, {priority: 'event'});
}

// Cancel agency editing
function cancelAgencyEdit() {
  Shiny.setInputValue('ag_list_cancel_click', Math.random(), {priority: 'event'});
}

// Handler: server sends timezone value to fill the form field
Shiny.addCustomMessageHandler('agFillTimezone', function(tz) {
  var el = document.getElementById('inline_ag_agency_timezone');
  if (el) {
    el.value = tz;
  }
});

// Hide agency city suggestions on click-away
$(document).on('click', function(e) {
  if (!$(e.target).closest('#ag_city_suggestions, #inline_ag_city_search').length) {
    var el = document.getElementById('ag_city_suggestions');
    if (el) el.style.display = 'none';
  }
});
