// == Croquis i18n - client-side mirror ================================
//
// Maintains a dictionary of strings that live in static UI (panel
// headers, tab labels, placeholders, button labels, confirm dialogs)
// and cannot be translated server-side via renderUI.
//
// Usage:
//   jsTr("key")                        → translated string
//   jsTr("key", {name: "value"})       → with {name} placeholder replaced
//   updateI18n()                        → re-scans DOM for data-i18n attrs
//   detectBrowserLang()                 → returns best matching lang code
//
// The active language (croquisLang) is set at page load via an inline
// <script> tag and synced from the R server via shinyjs::runjs()
// whenever lang() changes.
//
// -- Adding a new language --
//
//   1. Add translations for every key below.
//   2. Add the same language code to SUPPORTED_LANGS in R/i18n.R.
//   3. Nothing else changes — the dropdown, validation, and sync all
//      derive from SUPPORTED_LANGS on the R side.

var croquisLang = 'en';

// ── Supported language codes (mirrored from R/i18n.R SUPPORTED_LANGS) ──
// Used by detectBrowserLang() to validate against available translations.
var supportedLangs = ['en', 'fr', 'es'];


var jsI18n = {

  // -- Navigation tabs --
  tab_stops:    { en: 'stops',    fr: 'arr\u00eats',  es: 'paradas'  },
  tab_routes:   { en: 'routes',   fr: 'lignes',       es: 'rutas'    },
  tab_schedule: { en: 'schedule', fr: 'horaires',     es: 'horarios' },

  // -- Home module : title ---

  home_title: { en: "Home", fr: "Accueil", es: "Inicio"},

  // ── Home module: Load Network section ──
  load_network:      { en: 'Load Network',    fr: 'Charger un r\u00e9seau',    es: 'Cargar una red' },
  load_gtfs:         { en: 'Load a GTFS',     fr: 'Charger un GTFS',           es: 'Cargar un GTFS' },
  load_croquis:      { en: 'Load your croquis', fr: 'Charger votre croquis',   es: 'Cargar su croquis' },
  load_sample:       { en: 'Load a sample transit network', fr: 'Charger un r\u00e9seau de d\u00e9monstration', es: 'Cargar una red de transporte de ejemplo' },
  load_gtfs_desc:    { en: 'You can load an existing GTFS here.', fr: 'Chargez un GTFS existant ici.', es: 'Se puede cargar un GTFS existente aqu\u00ed.' },
  load_gtfs_size:    { en: 'Larger files may take several minutes (maximum size: 100MB).', fr: 'Les fichiers volumineux peuvent prendre plusieurs minutes (taille maximale\u00a0: 100\u00a0Mo).', es: 'Los archivos grandes pueden tardar varios minutos (tama\u00f1o m\u00e1ximo: 100\u00a0MB).' },
  load_gtfs_note:    { en: 'Uploading a GTFS here will convert it to an editable format in Croquis', fr: 'Le t\u00e9l\u00e9versement d\u2019un GTFS le convertira en un format modifiable dans Croquis', es: 'Al subir un GTFS, se convertir\u00e1 a un formato editable en Croquis' },
  load_croquis_desc: { en: 'To continue working on a previous croquis, upload your .rds file:', fr: 'Pour continuer \u00e0 travailler sur un croquis pr\u00e9c\u00e9dent, t\u00e9l\u00e9versez votre fichier .rds\u00a0:', es: 'Para continuar trabajando en un croquis anterior, suba su archivo .rds:' },
  load_croquis_note: { en: 'Upload a transit model .rds file previously created with Croquis', fr: 'T\u00e9l\u00e9versez un fichier .rds de mod\u00e8le de transport cr\u00e9\u00e9 pr\u00e9c\u00e9demment avec Croquis', es: 'Suba un archivo .rds de modelo de transporte creado previamente con Croquis' },
  load_sample_desc:  { en: 'To explore this tool, you can get started by loading a sample network. The Ligne Jaune model is the simplest and will help you familiarize yourself with how Croquis works.', fr: 'Pour d\u00e9couvrir cet outil, vous pouvez commencer en chargeant un r\u00e9seau de d\u00e9monstration. Le mod\u00e8le Ligne Jaune est le plus simple et vous aidera \u00e0 vous familiariser avec le fonctionnement de Croquis.', es: 'Para explorar esta herramienta, puede comenzar cargando una red de ejemplo. El modelo Ligne Jaune es el m\u00e1s sencillo y le ayudar\u00e1 a familiarizarse con el funcionamiento de Croquis.' },

// -- Home module: intro panel --
  // Split *_pre / *_post fragments sit on either side of an inline icon or
  // link; see R/i18n.R for the full assembled sentences. Strings must match
  // R/i18n.R exactly to avoid visible text changes on language sync.
  intro_tagline: {
    en: 'Croquis (crow-KEY) is a transit sketch planning tool and GTFS creator.',
    fr: 'Croquis est un outil de planification du transport collectif et un cr\u00e9ateur de GTFS.',
    es: 'Croquis es una herramienta de planificaci\u00f3n del transporte p\u00fablico y un creador de GTFS.'
  },
  intro_tabs: {
    en: 'The stops, routes and schedule tabs above allow you to manage all these aspects of your transit network model.',
    fr: 'Les onglets arr\u00eats, lignes et horaires ci-dessus permettent de g\u00e9rer tous ces aspects de votre mod\u00e8le de r\u00e9seau de transport.',
    es: 'Las pesta\u00f1as paradas, rutas y horarios de arriba permiten gestionar todos estos aspectos de su modelo de red de transporte.'
  },
  intro_get_started: {
    en: 'Get started on this page by loading an existing network, or by creating the agency details and project location if starting from scratch.',
    fr: 'Commencez sur cette page en chargeant un r\u00e9seau existant, ou en cr\u00e9ant les d\u00e9tails de l\'agence et l\'emplacement du projet si vous partez de z\u00e9ro.',
    es: 'Comience en esta p\u00e1gina cargando una red existente, o creando los detalles de la agencia y la ubicaci\u00f3n del proyecto si empieza desde cero.'
  },
  intro_save_pre: {
    en: 'This open-source software was developed in R Shiny. It is in active development. Save your work often by clicking the Save',
    fr: 'Ce logiciel libre a \u00e9t\u00e9 d\u00e9velopp\u00e9 en R Shiny. Il est en d\u00e9veloppement actif. Enregistrez souvent votre travail en cliquant sur l\'ic\u00f4ne Enregistrer',
    es: 'Este software de c\u00f3digo abierto fue desarrollado en R Shiny. Est\u00e1 en desarrollo activo. Guarde su trabajo con frecuencia haciendo clic en el \u00edcono Guardar'
  },
  intro_save_post: {
    en: 'icon above and exporting your project file.',
    fr: 'ci-dessus et en exportant votre fichier de projet.',
    es: 'de arriba y exportando su archivo de proyecto.'
  },
  intro_report_pre: {
    en: 'Please report any bugs and provide your ideas for improvement by submitting an',
    fr: 'Veuillez signaler tout bogue et proposer vos id\u00e9es d\'am\u00e9lioration en soumettant une',
    es: 'Reporte cualquier error y comparta sus ideas de mejora enviando un'
  },
  intro_report_link: {
    en: 'issue on GitHub',
    fr: 'issue sur GitHub',
    es: 'issue en GitHub'
  },

// -- Home module: file inputs --
  btn_browse: {
    en: 'Browse...',
    fr: 'Parcourir...',
    es: 'Examinar...'
  },

  // -- Home module: Project Location panel --
  // (loc_search_ph is build-time only and lives solely in R/i18n.R)
  loc_title: {
    en: 'Project Location',
    fr: 'Emplacement du projet',
    es: 'Ubicaci\u00f3n del proyecto'
  },
  loc_search_label: {
    en: 'Search for a city',
    fr: 'Rechercher une ville',
    es: 'Buscar una ciudad'
  },
  btn_select_city: {
    en: 'Select City',
    fr: 'Choisir la ville',
    es: 'Seleccionar ciudad'
  },
  loc_updates_note: {
    en: 'Updates the map center and fetches timezone',
    fr: 'Met \u00e0 jour le centre de la carte et r\u00e9cup\u00e8re le fuseau horaire',
    es: 'Actualiza el centro del mapa y obtiene la zona horaria'
  },
  loc_manual_title: {
    en: '...Or set project coordinates manually',
    fr: '...Ou d\u00e9finissez les coordonn\u00e9es du projet manuellement',
    es: '...O defina las coordenadas del proyecto manualmente'
  },
  lbl_latitude: {
    en: 'Latitude',
    fr: 'Latitude',
    es: 'Latitud'
  },
  lbl_longitude: {
    en: 'Longitude',
    fr: 'Longitude',
    es: 'Longitud'
  },

  // -- Home module: Agencies panel (static header + confirm dialog) --
  agencies_title: {
    en: 'Agencies',
    fr: 'Agences',
    es: 'Agencias'
  },
  confirm_delete_agency: {
    en: 'Delete this agency? Routes referencing it must be removed first.',
    fr: 'Supprimer cette agence\u00a0? Les lignes qui y font r\u00e9f\u00e9rence doivent d\'abord \u00eatre supprim\u00e9es.',
    es: '\u00bfEliminar esta agencia? Las rutas que la referencian deben eliminarse primero.'
  },

  // -- Popover content (static UI) --
  lbl_read_more: {
    en: 'Read more',
    fr: 'En savoir plus',
    es: 'Leer m\u00e1s'
  },
  pop_city_search: {
    en: 'Start typing a city name and select city, if starting project from scratch. If you are not able to find your city, you may need to set coordinates manually below.',
    fr: 'Commencez \u00e0 saisir le nom d\'une ville et s\u00e9lectionnez-la si vous d\u00e9marrez un projet de z\u00e9ro. Si vous ne trouvez pas votre ville, vous devrez peut-\u00eatre d\u00e9finir les coordonn\u00e9es manuellement ci-dessous.',
    es: 'Empiece a escribir el nombre de una ciudad y selecci\u00f3nela si inicia un proyecto desde cero. Si no encuentra su ciudad, puede que necesite definir las coordenadas manualmente abajo.'
  },
  stops_autogen_pop: {
en: 'Automatically generate stops within a drawn zone. Stops are placed at intersections based on the minimum spacing defined in the {icon} settings.',
    fr: 'G\u00e9n\u00e9rez automatiquement des arr\u00eats dans une zone dessin\u00e9e. Les arr\u00eats sont plac\u00e9s aux intersections en fonction de l\'espacement minimal d\u00e9fini dans les {icon} param\u00e8tres.',
    es: 'Genere autom\u00e1ticamente paradas dentro de una zona dibujada. Las paradas se colocan en las intersecciones seg\u00fan el espaciamiento m\u00ednimo definido en la {icon} configuraci\u00f3n.'

  },

  // -- Live-switchable placeholders (tagged via i18n_placeholder in R) --
  file_placeholder: {
    en: 'Drag and drop or click to select file',
    fr: 'Glissez-d\u00e9posez ou cliquez pour s\u00e9lectionner un fichier',
    es: 'Arrastre y suelte o haga clic para seleccionar un archivo'
  },
  loc_search_ph: {
    en: 'Type city name...',
    fr: 'Saisissez le nom d\'une ville...',
    es: 'Escriba el nombre de la ciudad...'
  },
  stops_import_ph: {
    en: 'GeoJSON or KML file',
    fr: 'Fichier GeoJSON ou KML',
    es: 'Archivo GeoJSON o KML'
  },

  // -- Stops module: static panel chrome --
  stops_title:         { en: 'stops',    fr: 'arr\u00eats',  es: 'paradas'  },
  stops_panel_title:   { en: 'Stops',    fr: 'Arr\u00eats',  es: 'Paradas'  },
  stops_search:        { en: 'Search stops...', fr: 'Rechercher des arr\u00eats...', es: 'Buscar paradas...' },
  stops_ie_title:      { en: 'Import / Export / Generate', fr: 'Importer / Exporter / G\u00e9n\u00e9rer', es: 'Importar / Exportar / Generar' },
  stops_import_title:  { en: 'Import Stops', fr: 'Importer des arr\u00eats', es: 'Importar paradas' },
  stops_export_title:  { en: 'Export Stops', fr: 'Exporter les arr\u00eats', es: 'Exportar paradas' },
  stops_autogen_title: { en: 'Auto-generate stops', fr: 'G\u00e9n\u00e9ration automatique d\u2019arr\u00eats', es: 'Generaci\u00f3n autom\u00e1tica de paradas' },
  btn_import:          { en: 'Import',   fr: 'Importer',        es: 'Importar'  },
  btn_download:        { en: 'Download', fr: 'T\u00e9l\u00e9charger', es: 'Descargar' },

  // -- Stops module: JS confirm dialogs --
  confirm_delete_stop: {
    en: 'This stop will be deleted if it is not associated with any itineraries.',
    fr: 'Cet arr\u00eat sera supprim\u00e9 s\u2019il n\u2019est associ\u00e9 \u00e0 aucun itin\u00e9raire.',
    es: 'Esta parada ser\u00e1 eliminada si no est\u00e1 asociada a ning\u00fan itinerario.'
  }

  // -- Add keys for other modules here as they are migrated --
};


// === Core lookup ====================================

// Look up a translated string.
// Falls back to English, then to the raw key.
// Optional `replacements` object substitutes {placeholder} tokens.
function jsTr(key, replacements) {
  var entry = jsI18n[key];
  if (!entry) return key;
  var str = entry[croquisLang] || entry.en || key;
  if (replacements) {
    for (var k in replacements) {
      if (replacements.hasOwnProperty(k)) {
        str = str.replace('{' + k + '}', replacements[k]);
      }
    }
  }
  return str;
}

// === DOM scanner ==============================

// Scan the DOM for elements tagged with data-i18n attributes and replace
// their visible text (or placeholder, or title) with the translated
// string.
//
// Supported attributes:
//   data-i18n="key"              → sets element's textContent
//   data-i18n-placeholder="key"  → sets element's placeholder
//   data-i18n-title="key"        → sets element's title attribute
//   data-i18n-popover="key"      → rebuilds Bootstrap popover content
//     (with data-i18n-popover-link and data-i18n-popover-token-* support)
//
// Called automatically when the server syncs croquisLang, and also
// after Shiny re-renders UI (via shiny:value event).
function updateI18n() {
  // textContent
  document.querySelectorAll('[data-i18n]').forEach(function(el) {
    var key = el.getAttribute('data-i18n');
    var translated = jsTr(key);
    if (translated !== key) {
      el.textContent = translated;
    }
  });

  // placeholder
  document.querySelectorAll('[data-i18n-placeholder]').forEach(function(el) {
    var key = el.getAttribute('data-i18n-placeholder');
    var translated = jsTr(key);
    if (translated !== key) {
      el.placeholder = translated;
    }
  });

// title (tooltip)
  document.querySelectorAll('[data-i18n-title]').forEach(function(el) {
    var key = el.getAttribute('data-i18n-title');
    var translated = jsTr(key);
    if (translated !== key) {
      el.title = translated;
    }
  });

  // popover content (data-content attribute + initialized BS3 instance)
  document.querySelectorAll('[data-i18n-popover]').forEach(function(el) {
    var key = el.getAttribute('data-i18n-popover');
    var replacements = null;
    for (var i = 0; i < el.attributes.length; i++) {
      var a = el.attributes[i];
      if (a.name.indexOf('data-i18n-popover-token-') === 0) {
        replacements = replacements || {};
        replacements[a.name.substring(24)] = a.value;
      }
    }
    var translated = jsTr(key, replacements);
    if (translated === key) {
      return;
    }
    var link = el.getAttribute('data-i18n-popover-link');
    var content = translated;
    if (link) {
      content += "<br><a href='" + link + "' target='_blank'>" +
        jsTr('lbl_read_more') + '</a>';
    }
    el.setAttribute('data-content', content);
    var pop = $(el).data('bs.popover');
    if (pop) {
      pop.options.content = content;
    }
  });
}

// Re-run after Shiny rebuilds dynamic UI so that any new data-i18n
// elements in renderUI output are also translated.
$(document).on('shiny:value', function() {
  setTimeout(updateI18n, 120);
});

// === Browser locale detection =====================================

// CURRENTLY UNUSED - can implement later for browser based deployments
// Returns the best-matching supported language code based on the
// browser's language preferences.  Falls back to 'en' if no match.
//
// Usage (e.g. in a hosted SaaS app.R):
//   Shiny.setInputValue('browser_lang', detectBrowserLang());
//
// This function is deliberately not called automatically — the host
// application decides whether to use it (e.g. to pre-populate the
// lang parameter on first visit).
function detectBrowserLang() {
  var langs = navigator.languages || [navigator.language || navigator.userLanguage || 'en'];
  for (var i = 0; i < langs.length; i++) {
    var code = langs[i].substring(0, 2).toLowerCase();
    if (supportedLangs.indexOf(code) !== -1) return code;
  }
  return 'en';
}
