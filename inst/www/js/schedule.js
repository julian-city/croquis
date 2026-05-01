var schedNs = '';

Shiny.addCustomMessageHandler('setSchedNs', function(ns) {
  schedNs = ns;
});

// Click on a route row in the schedule routes panel to highlight
function schedToggleRoute(routeId) {
  Shiny.setInputValue(schedNs + 'sched_route_click',
    {id: routeId, ts: Math.random()}, {priority: 'event'});
}

// Click the pencil icon on a route row to trigger schedule editing
function schedEditRoute(routeId) {
  Shiny.setInputValue(schedNs + 'sched_route_edit_click',
    {id: routeId, ts: Math.random()}, {priority: 'event'});
}

// Click on an itinerary row in the schedule route panel
function schedSelectItin(itinId) {
   Shiny.setInputValue(schedNs + 'sched_itin_select',
     {id: itinId, ts: Math.random()}, {priority: 'event'});
 }

// Click the pencil icon on an itinerary row → edit schedule
function schedEditItin(itinId) {
  Shiny.setInputValue(schedNs + 'sched_itin_edit_click',
    {id: itinId, ts: Math.random()}, {priority: 'event'});
}

// Itinerary-level schedule editing
 function schedEditSpan(idx) {
   Shiny.setInputValue(schedNs + 'sched_span_edit_click',
     {idx: idx, ts: Math.random()}, {priority: 'event'});
 }

 function schedDeleteSpan(idx) {
   if (confirm('Delete this service window and associated headway by hour entries?')) {
     Shiny.setInputValue(schedNs + 'sched_span_delete_click',
       {idx: idx, ts: Math.random()}, {priority: 'event'});
   }
 }

 function schedAddSpan() {
   Shiny.setInputValue(schedNs + 'sched_span_add_click',
     Math.random(), {priority: 'event'});
 }

 function schedSaveSpanEdit() {
   var firstDep = document.getElementById(schedNs + 'sched_span_edit_first_dep');
   var lastDep = document.getElementById(schedNs + 'sched_span_edit_last_dep');
   Shiny.setInputValue(schedNs + 'sched_span_save_edit', {
     first_dep: firstDep ? firstDep.value : '',
     last_dep: lastDep ? lastDep.value : '',
     ts: Math.random()
   }, {priority: 'event'});
 }

 function schedSaveNewSpan() {
   var firstDep = document.getElementById(schedNs + 'sched_span_edit_first_dep');
   var lastDep = document.getElementById(schedNs + 'sched_span_edit_last_dep');
   Shiny.setInputValue(schedNs + 'sched_span_save_new', {
     first_dep: firstDep ? firstDep.value : '',
     last_dep: lastDep ? lastDep.value : '',
     ts: Math.random()
   }, {priority: 'event'});
 }

 function schedCancelSpanEdit() {
   Shiny.setInputValue(schedNs + 'sched_span_cancel_edit',
     Math.random(), {priority: 'event'});
 }

 // Headway and speed editing

 function schedEditHshRow(hour) {
   Shiny.setInputValue(schedNs + 'sched_hsh_edit_click',
     {hour: hour, ts: Math.random()}, {priority: 'event'});
 }

 function schedSaveHshEdit() {
   var hdwy = document.getElementById(schedNs + 'sched_hsh_edit_headway');
   var spd = document.getElementById(schedNs + 'sched_hsh_edit_speed');
   Shiny.setInputValue(schedNs + 'sched_hsh_save_edit', {
     headway: hdwy ? hdwy.value : '',
     speed: spd ? spd.value : '',
     ts: Math.random()
   }, {priority: 'event'});
 }

 function schedCancelHshEdit() {
   Shiny.setInputValue(schedNs + 'sched_hsh_cancel_edit',
     Math.random(), {priority: 'event'});
 }

 // Calendar config

  function schedEditCalendarRow(serviceId) {
   Shiny.setInputValue(schedNs + 'sched_cal_edit_click',
     {id: serviceId, ts: Math.random()}, {priority: 'event'});
 }

 function schedDeleteCalendarRow(serviceId) {
   if (confirm('Delete service "' + serviceId + '"? This will remove all schedule data associated with this route.')) {
     Shiny.setInputValue(schedNs + 'sched_cal_delete_click',
       {id: serviceId, ts: Math.random()}, {priority: 'event'});
   }
 }

 function schedAddCalendarRow() {
   Shiny.setInputValue(schedNs + 'sched_cal_add_click',
     Math.random(), {priority: 'event'});
 }

 function schedSaveCalendarEdit() {
   var sid = document.getElementById(schedNs + 'sched_cal_edit_service_id');
   var mon = document.getElementById(schedNs + 'sched_cal_mon');
   var tue = document.getElementById(schedNs + 'sched_cal_tue');
   var wed = document.getElementById(schedNs + 'sched_cal_wed');
   var thu = document.getElementById(schedNs + 'sched_cal_thu');
   var fri = document.getElementById(schedNs + 'sched_cal_fri');
   var sat = document.getElementById(schedNs + 'sched_cal_sat');
   var sun = document.getElementById(schedNs + 'sched_cal_sun');
   var sd = document.getElementById(schedNs + 'sched_cal_start_date');
   var ed = document.getElementById(schedNs + 'sched_cal_end_date');
   Shiny.setInputValue(schedNs + 'sched_cal_save_edit', {
     service_id: sid ? sid.value : '',
     monday: mon ? (mon.checked ? 1 : 0) : 0,
     tuesday: tue ? (tue.checked ? 1 : 0) : 0,
     wednesday: wed ? (wed.checked ? 1 : 0) : 0,
     thursday: thu ? (thu.checked ? 1 : 0) : 0,
     friday: fri ? (fri.checked ? 1 : 0) : 0,
     saturday: sat ? (sat.checked ? 1 : 0) : 0,
     sunday: sun ? (sun.checked ? 1 : 0) : 0,
     start_date: sd ? sd.value : '',
     end_date: ed ? ed.value : '',
     ts: Math.random()
   }, {priority: 'event'});
 }

 function schedCancelCalendarEdit() {
   Shiny.setInputValue(schedNs + 'sched_cal_cancel_edit',
     Math.random(), {priority: 'event'});
 }

 // --- Service level presets modal ---

 function schedEditPreset(patternId) {
   Shiny.setInputValue(schedNs + 'sched_preset_edit_click',
     {id: patternId, ts: Math.random()}, {priority: 'event'});
 }

 function schedDeletePreset(patternId) {
   if (confirm('Delete preset "' + patternId + '"?')) {
     Shiny.setInputValue(schedNs + 'sched_preset_delete_click',
       {id: patternId, ts: Math.random()}, {priority: 'event'});
   }
 }

 function schedAddPreset() {
   Shiny.setInputValue(schedNs + 'sched_preset_add_click',
     Math.random(), {priority: 'event'});
 }

 function schedSavePresetName() {
   var nameInput = document.getElementById(schedNs + 'sched_preset_name_input');
   Shiny.setInputValue(schedNs + 'sched_preset_save_name', {
     name: nameInput ? nameInput.value : '',
     ts: Math.random()
   }, {priority: 'event'});
 }

  function schedEditPresetHour(hour) {
   Shiny.setInputValue(schedNs + 'sched_preset_hour_edit_click',
     {hour: hour, ts: Math.random()}, {priority: 'event'});
 }

 function schedDeletePresetHour(hour) {
   Shiny.setInputValue(schedNs + 'sched_preset_hour_delete_click',
     {hour: hour, ts: Math.random()}, {priority: 'event'});
 }

 function schedAddPresetHour() {
   Shiny.setInputValue(schedNs + 'sched_preset_hour_add_click',
     Math.random(), {priority: 'event'});
 }

 function schedSavePresetHourEdit() {
   var hdwy = document.getElementById(schedNs + 'sched_preset_hour_edit_headway');
   Shiny.setInputValue(schedNs + 'sched_preset_hour_save_edit', {
     headway: hdwy ? hdwy.value : '',
     ts: Math.random()
   }, {priority: 'event'});
 }

 function schedSavePresetNewHour() {
   var hourSel = document.getElementById(schedNs + 'sched_preset_hour_new_hour');
   var hdwy = document.getElementById(schedNs + 'sched_preset_hour_edit_headway');
   Shiny.setInputValue(schedNs + 'sched_preset_hour_save_new', {
     hour: hourSel ? hourSel.value : '',
     headway: hdwy ? hdwy.value : '10',
     ts: Math.random()
   }, {priority: 'event'});
 }

 function schedCancelPresetHourEdit() {
   Shiny.setInputValue(schedNs + 'sched_preset_hour_cancel_edit',
     Math.random(), {priority: 'event'});
 }