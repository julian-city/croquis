// Initialize popovers on tab change
$(document).on('shown.bs.tab change', function() {
  $('[data-toggle="popover"]').popover({container: 'body'});
});

// Initialize popovers on document ready
$(document).ready(function() {
  $('[data-toggle="popover"]').popover({container: 'body'});
});

// Re-initialize popovers after any Shiny output re-renders (covers renderUI)
$(document).on('shiny:value', function() {
  setTimeout(function() {
    $('[data-toggle="popover"]').popover({container: 'body'});
  }, 100);
});

// Dismiss popovers on click-away
$(document).on('mousedown', function(e) {
  var $target = $(e.target);
  if (!$target.closest('.popover').length && !$target.closest('.info-icon').length) {
    $('[data-toggle="popover"]').popover('hide');
  }
});

// Dismiss popovers on tab change
$(document).on('shown.bs.tab', function() {
  $('[data-toggle="popover"]').popover('hide');
});