// Click on a route row in the schedule routes panel to highlight
function schedToggleRoute(routeId) {
  Shiny.setInputValue('sched_route_click',
    {id: routeId, ts: Math.random()}, {priority: 'event'});
}

// Click the pencil icon on a route row to trigger schedule editing
function schedEditRoute(routeId) {
  Shiny.setInputValue('sched_route_edit_click',
    {id: routeId, ts: Math.random()}, {priority: 'event'});
}