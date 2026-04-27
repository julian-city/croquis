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
   if (confirm('Delete this service window? Associated headway entries will also be removed.')) {
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