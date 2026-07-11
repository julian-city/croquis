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
  load_sample_desc:  { en: 'To explore this tool, you can get started by loading a sample network. The Ligne Jaune model is the simplest and will help you familiarize yourself with how Croquis works.', fr: 'Pour d\u00e9couvrir cet outil, vous pouvez commencer en chargeant un r\u00e9seau de d\u00e9monstration. Le mod\u00e8le Ligne Jaune est le plus simple et vous aidera \u00e0 vous familiariser avec le fonctionnement de Croquis.', es: 'Para explorar esta herramienta, puede comenzar cargando una red de ejemplo. El modelo L\u00ednea Amarilla (Ligne Jaune) es el m\u00e1s sencillo y le ayudar\u00e1 a familiarizarse con el funcionamiento de Croquis.' },

  // -- Stops module: static panel chrome --
  stops_title:         { en: 'stops',    fr: 'arr\u00eats',  es: 'paradas'  },
  stops_panel_title:   { en: 'Stops',    fr: 'Arr\u00eats',  es: 'Paradas'  },
  stops_search:        { en: 'Search stops...', fr: 'Rechercher un arr\u00eats...', es: 'Buscar una parada...' },
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
