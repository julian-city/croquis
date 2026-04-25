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