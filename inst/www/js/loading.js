// Loading indicator
var loadingTimeout;

$(document).on('shiny:busy', function() {
    loadingTimeout = setTimeout(function() {
        $('#loading-content').css('display', 'flex');
    }, 1000); // Show after 1000ms
});

$(document).on('shiny:idle', function() {
    clearTimeout(loadingTimeout);
    $('#loading-content').hide();
});

// City selection
function selectCity(cityName) {
  Shiny.setInputValue('selected_city_name', cityName);
}

// Custom message handlers for city suggestions
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