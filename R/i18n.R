# ── Croquis i18n infrastructure ─────────────────────────────────────────
#
# Custom dictionary-based internationalisation for the Croquis Shiny app.
# Each translatable string is stored as a named character vector keyed by
# ISO 639-1 language code (en / fr / es / ...).  The helper `tr()` performs
# the lookup at render time.
#
# ── Adding a new language ──
#
#   1. Add the language code and display label to `SUPPORTED_LANGS` below.
#   2. Add a new element to every vector in `i18n_dict` with the translated
#      string  (e.g.  pt = "...").  Keys with no translation for the new
#      language fall back to English automatically.
#   3. Mirror the new entries in `inst/www/js/i18n.js` (only the keys used
#      in static UI and JS confirm dialogs need to be mirrored).
#   4. No other code changes are needed — the language selector, `match.arg`
#      validation, and JS sync all derive from `SUPPORTED_LANGS`.
#
# ── Adding a new translatable string ──
#
#   1. Pick a snake_case key.
#   2. Add it to `i18n_dict` with at least an `en` entry.
#   3. Use `tr("key", lang)` in R code or `jsTr("key")` in JS code.
#   4. Untranslated keys render as [key] — easy to spot during development.
#
# ── R package notes (per https://r-pkgs.org/code.html) ──
#
#   • All non-ASCII characters use \uXXXX escapes for CRAN portability.
#   • `i18n_dict` and `SUPPORTED_LANGS` are top-level objects evaluated at
#     build time.  This is intentional and correct: the dictionary is static
#     data that does not depend on the user's system.
#   • `tr()` is a function and executes at run time, which is the correct
#     pattern for code that reads from the dictionary.
#   • Neither `i18n_dict`, `SUPPORTED_LANGS`, nor `tr()` are exported.
#     They are package-internal, used only by the Shiny UI layer.
#

# ── Supported languages ─────────────────────────────────────────────────
#
# Central registry of supported languages.  Used by:
#   • `croquis(lang = ...)` — `match.arg()` validation
#   • The language `<select>` in the navbar — option generation
#   • JS `croquisLang` initialisation
#
# To add a language, add one entry here (code = "LABEL") and translations
# to `i18n_dict`.  Everything else adapts automatically.

#' Supported UI languages
#'
#' Named character vector: codes are ISO 639-1, values are the display
#' labels shown in the language selector dropdown.
#'
#' @keywords internal
#' @noRd
SUPPORTED_LANGS <- c(
  en = "EN",
  fr = "FR",
  es = "ES"
)

# ── Translation dictionary ──────────────────────────────────────────────

#' i18n translation dictionary
#'
#' Named list of character vectors.
#' Each key maps to a named vector of translations keyed by language code.
#'
#' @keywords internal
#' @noRd
i18n_dict <- list(
  # ════════════════════════════════════════════════════════════════════════
  # Navigation tabs
  # ════════════════════════════════════════════════════════════════════════
  tab_stops = c(en = "stops", fr = "arr\u00eats", es = "paradas"),
  tab_routes = c(en = "routes", fr = "lignes", es = "rutas"),
  tab_schedule = c(en = "schedule", fr = "horaires", es = "horarios"),

  # ════════════════════════════════════════════════════════════════════════
  # Shared / common
  # ════════════════════════════════════════════════════════════════════════
  btn_save = c(en = "Save", fr = "Enregistrer", es = "Guardar"),
  btn_cancel = c(en = "Cancel", fr = "Annuler", es = "Cancelar"),
  btn_create = c(en = "Create", fr = "Cr\u00e9er", es = "Crear"),
  btn_apply = c(en = "Apply", fr = "Appliquer", es = "Aplicar"),
  btn_delete = c(en = "Delete", fr = "Supprimer", es = "Eliminar"),
  btn_import = c(en = "Import", fr = "Importer", es = "Importar"),
  btn_download = c(
    en = "Download",
    fr = "T\u00e9l\u00e9charger",
    es = "Descargar"
  ),
  btn_complete = c(en = "Complete", fr = "Terminer", es = "Completar"),
  btn_generate = c(en = "Generate", fr = "G\u00e9n\u00e9rer", es = "Generar"),
  lbl_none = c(en = "None", fr = "Aucun", es = "Ninguno"),
  lbl_edit = c(en = "Edit", fr = "Modifier", es = "Editar"),
  lbl_itineraries = c(
    en = "Itineraries",
    fr = "Parcours types",
    es = "Itinerarios"
  ),

  # ════════════════════════════════════════════════════════════════════════
  # home module (static UI, translated JS updateI18n)
  # ════════════════════════════════════════════════════════════════════════

  home_title = c(en = "Home", fr = "Accueil", es = "Inicio"),

  # Section headers
  load_network = c(
    en = "Load Network",
    fr = "Charger un r\u00e9seau",
    es = "Cargar una red"
  ),
  load_gtfs = c(
    en = "Load a GTFS",
    fr = "Charger un GTFS",
    es = "Cargar un GTFS"
  ),
  load_croquis = c(
    en = "Load your croquis",
    fr = "Charger votre croquis",
    es = "Cargar su croquis"
  ),
  load_sample = c(
    en = "Load a sample transit network",
    fr = "Charger un r\u00e9seau de d\u00e9monstration",
    es = "Cargar una red de transporte de ejemplo"
  ),

  # Descriptive text - Load a GTFS
  load_gtfs_desc = c(
    en = "You can load an existing GTFS here.",
    fr = "Chargez un GTFS existant ici.",
    es = "Se puede cargar un GTFS existente aqu\u00ed."
  ),
  load_gtfs_size = c(
    en = "Larger files may take several minutes (maximum size: 100MB).",
    fr = "Les fichiers volumineux peuvent prendre plusieurs minutes (taille maximale\u00a0: 100\u00a0Mo).",
    es = "Los archivos grandes pueden tardar varios minutos (tama\u00f1o m\u00e1ximo: 100\u00a0MB)."
  ),
  load_gtfs_note = c(
    en = "Uploading a GTFS here will convert it to an editable format in Croquis",
    fr = "Le t\u00e9l\u00e9versement d'un GTFS le convertira en un format modifiable dans Croquis",
    es = "Al subir un GTFS, se convertir\u00e1 a un formato editable en Croquis"
  ),

  # Descriptive text - Load your croquis
  load_croquis_desc = c(
    en = "To continue working on a previous croquis, upload your .rds file:",
    fr = "Pour continuer \u00e0 travailler sur un croquis pr\u00e9c\u00e9dent, t\u00e9l\u00e9versez votre fichier .rds\u00a0:",
    es = "Para continuar trabajando en un croquis anterior, suba su archivo .rds:"
  ),
  load_croquis_note = c(
    en = "Upload a transit model .rds file previously created with Croquis",
    fr = "T\u00e9l\u00e9versez un fichier .rds de mod\u00e8le de transport cr\u00e9\u00e9 pr\u00e9c\u00e9demment avec Croquis",
    es = "Suba un archivo .rds de modelo de transporte creado previamente con Croquis"
  ),

  # Descriptive text - Sample networks
  load_sample_desc = c(
    en = "To explore this tool, you can get started by loading a sample network. The Ligne Jaune model is the simplest and will help you familiarize yourself with how Croquis works.",
    fr = "Pour d\u00e9couvrir cet outil, vous pouvez commencer en chargeant un r\u00e9seau de d\u00e9monstration. Le mod\u00e8le Ligne Jaune est le plus simple et vous aidera \u00e0 vous familiariser avec le fonctionnement de Croquis.",
    es = "Para explorar esta herramienta, puede comenzar cargando una red de ejemplo. El modelo L\u00ednea Amarilla (Ligne Jaune) es el m\u00e1s sencillo y le ayudar\u00e1 a familiarizarse con el funcionamiento de Croquis."
  ),

  # ── Project Location panel ─────────────────────────────────────────────
  loc_title = c(
    en = "Project Location",
    fr = "Emplacement du projet",
    es = "Ubicaci\u00f3n del proyecto"
  ),
  loc_search_label = c(
    en = "Search for a city",
    fr = "Rechercher une ville",
    es = "Buscar una ciudad"
  ),
  # Build-time only (textInput placeholder accepts character only);
  # deliberately absent from inst/www/js/i18n.js.
  loc_search_ph = c(
    en = "Type city name...",
    fr = "Saisissez le nom d'une ville...",
    es = "Escriba el nombre de la ciudad..."
  ),
  btn_select_city = c(
    en = "Select City",
    fr = "Choisir la ville",
    es = "Seleccionar ciudad"
  ),
  loc_updates_note = c(
    en = "Updates the map center and fetches timezone",
    fr = "Met \u00e0 jour le centre de la carte et r\u00e9cup\u00e8re le fuseau horaire",
    es = "Actualiza el centro del mapa y obtiene la zona horaria"
  ),
  loc_manual_title = c(
    en = "...Or set project coordinates manually",
    fr = "...Ou d\u00e9finissez les coordonn\u00e9es du projet manuellement",
    es = "...O defina las coordenadas del proyecto manualmente"
  ),
  lbl_latitude = c(
    en = "Latitude",
    fr = "Latitude",
    es = "Latitud"
  ),
  lbl_longitude = c(
    en = "Longitude",
    fr = "Longitude",
    es = "Longitud"
  ),

  # Notifications - Project Location
  notif_center_from_stops = c(
    en = "The map center is set from the loaded network's stops. Remove all stops to set a city manually.",
    fr = "Le centre de la carte est d\u00e9fini \u00e0 partir des arr\u00eats du r\u00e9seau charg\u00e9. Supprimez tous les arr\u00eats pour d\u00e9finir une ville manuellement.",
    es = "El centro del mapa se define a partir de las paradas de la red cargada. Elimine todas las paradas para definir una ciudad manualmente."
  ),
  notif_city_empty = c(
    en = "Please enter a city name",
    fr = "Veuillez renseigner le nom d'une ville",
    es = "Ingrese un nombre de ciudad"
  ),
  notif_city_not_found = c(
    en = "City not found. Please select from the suggestions.",
    fr = "Ville introuvable. Veuillez choisir parmi les suggestions.",
    es = "Ciudad no encontrada. Seleccione una de las sugerencias."
  ),
  #stale notification ? city database does not have duplicate names
  notif_city_multiple = c(
    en = "Multiple cities found with that name. Please be more specific.",
    fr = "Plusieurs villes portent ce nom. Veuillez \u00eatre plus pr\u00e9cis.",
    es = "Se encontraron varias ciudades con ese nombre. Sea m\u00e1s espec\u00edfico."
  ),
  notif_city_set = c(
    en = "City set to: %s",
    fr = "Ville d\u00e9finie\u00a0: %s",
    es = "Ciudad definida: %s"
  ),
  notif_coords_range = c(
    en = "Latitude must be between -90 and 90, longitude between -180 and 180",
    fr = "La latitude doit \u00eatre comprise entre -90 et 90, la longitude entre -180 et 180",
    es = "La latitud debe estar entre -90 y 90, la longitud entre -180 y 180"
  ),

  # Notifications - loading
  notif_gtfs_loaded = c(
    en = "GTFS loaded successfully",
    fr = "GTFS charg\u00e9 avec succ\u00e8s",
    es = "GTFS cargado con \u00e9xito"
  ),
  notif_project_loaded = c(
    en = "Transit system loaded successfully",
    fr = "Syst\u00e8me de transport charg\u00e9 avec succ\u00e8s",
    es = "Sistema de transporte cargado con \u00e9xito"
  ),
  notif_sample_loaded = c(
    en = "%s loaded successfully",
    fr = "%s charg\u00e9 avec succ\u00e8s",
    es = "%s cargado con \u00e9xito"
  ),
  notif_load_file_error = c(
    en = "Error loading file: %s",
    fr = "Erreur lors du chargement du fichier\u00a0: %s",
    es = "Error al cargar el archivo: %s"
  ),
  notif_load_sample_error = c(
    en = "Error loading %s: %s",
    fr = "Erreur lors du chargement de %s\u00a0: %s",
    es = "Error al cargar %s: %s"
  ),

  # ── Agencies panel ─────────────────────────────────────────────────────
  # Static header (data-i18n + JS mirror)
  agencies_title = c(en = "Agencies", fr = "Agences", es = "Agencias"),

  # Dynamic list and inline form (reactive tr(lang()) in renderUI; R-only)
  agency_add_new = c(
    en = "Add new agency",
    fr = "Ajouter une nouvelle agence",
    es = "Agregar nueva agencia"
  ),
  agency_edit_title = c(
    en = "Edit agency",
    fr = "Modifier l'agence",
    es = "Editar agencia"
  ),
  agency_delete_title = c(
    en = "Delete agency",
    fr = "Supprimer l'agence",
    es = "Eliminar agencia"
  ),
  lbl_agency_id = c(
    en = "Agency ID",
    fr = "ID de l'agence",
    es = "ID de agencia"
  ),
  lbl_agency_name = c(
    en = "Agency name",
    fr = "Nom de l'agence",
    es = "Nombre de agencia"
  ),
  lbl_agency_url = c(
    en = "Agency URL",
    fr = "URL de l'agence",
    es = "URL de agencia"
  ),
  lbl_agency_tz = c(
    en = "Agency timezone",
    fr = "Fuseau horaire de l'agence",
    es = "Zona horaria de agencia"
  ),
  agency_ph_id = c(
    en = "e.g., STM",
    fr = "p.\u00a0ex. STM",
    es = "p.\u00a0ej. STM"
  ),
  agency_ph_name = c(
    en = "e.g., Soci\u00e9t\u00e9 de transport de Montr\u00e9al",
    fr = "p.\u00a0ex. Soci\u00e9t\u00e9 de transport de Montr\u00e9al",
    es = "p.\u00a0ej. Soci\u00e9t\u00e9 de transport de Montr\u00e9al"
  ),
  agency_ph_url = c(
    en = "e.g., http://www.stm.info",
    fr = "p.\u00a0ex. http://www.stm.info",
    es = "p.\u00a0ej. http://www.stm.info"
  ),
  agency_ph_tz = c(
    en = "e.g., America/Montreal",
    fr = "p.\u00a0ex. America/Montreal",
    es = "p.\u00a0ej. America/Montreal"
  ),

  # JS confirm dialog (mirrored in inst/www/js/i18n.js)
  confirm_delete_agency = c(
    en = "Delete this agency? Routes referencing it must be removed first.",
    fr = "Supprimer cette agence\u00a0? Les lignes qui y font r\u00e9f\u00e9rence doivent d'abord \u00eatre supprim\u00e9es.",
    es = "\u00bfEliminar esta agencia? Las rutas que la referencian deben eliminarse primero."
  ),

  # Notifications — Agencies
  notif_agency_id_empty = c(
    en = "Agency ID cannot be empty.",
    fr = "L'ID de l'agence ne peut pas \u00eatre vide.",
    es = "El ID de agencia no puede estar vac\u00edo."
  ),
  notif_agency_id_exists = c(
    en = "This agency ID already exists. Please use a different ID.",
    fr = "Cet ID d'agence existe d\u00e9j\u00e0. Veuillez utiliser un ID diff\u00e9rent.",
    es = "Este ID de agencia ya existe. Use un ID diferente."
  ),
  notif_agency_not_found = c(
    en = "Agency not found.",
    fr = "Agence introuvable.",
    es = "Agencia no encontrada."
  ),
  notif_agency_added = c(
    en = "Agency added successfully",
    fr = "Agence ajout\u00e9e avec succ\u00e8s",
    es = "Agencia agregada con \u00e9xito"
  ),
  notif_agency_updated = c(
    en = "Agency updated successfully",
    fr = "Agence mise \u00e0 jour avec succ\u00e8s",
    es = "Agencia actualizada con \u00e9xito"
  ),
  notif_agency_deleted = c(
    en = "Agency deleted successfully",
    fr = "Agence supprim\u00e9e avec succ\u00e8s",
    es = "Agencia eliminada con \u00e9xito"
  ),
  notif_agency_cant_delete = c(
    en = "Cannot delete agency '%s'. It is referenced by one or more routes. Delete or reassign the routes first.",
    fr = "Impossible de supprimer l'agence \u00ab\u00a0%s\u00a0\u00bb. Une ou plusieurs lignes y font r\u00e9f\u00e9rence. Supprimez ou r\u00e9assignez d'abord les lignes.",
    es = "No se puede eliminar la agencia '%s'. Una o m\u00e1s rutas la referencian. Elimine o reasigne las rutas primero."
  ),

  # ════════════════════════════════════════════════════════════════════════
  # Stops module - panel chrome (static UI, translated via JS updateI18n)
  # ════════════════════════════════════════════════════════════════════════
  stops_title = c(en = "stops", fr = "arr\u00eats", es = "paradas"),
  stops_panel_title = c(en = "Stops", fr = "Arr\u00eats", es = "Paradas"),
  stops_search = c(
    en = "Search stops...",
    fr = "Rechercher des arr\u00eats...",
    es = "Buscar paradas..."
  ),
  stops_ie_title = c(
    en = "Import / Export / Generate",
    fr = "Importer / Exporter / G\u00e9n\u00e9rer",
    es = "Importar / Exportar / Generar"
  ),
  stops_import_title = c(
    en = "Import Stops",
    fr = "Importer des arr\u00eats",
    es = "Importar paradas"
  ),
  stops_export_title = c(
    en = "Export Stops",
    fr = "Exporter les arr\u00eats",
    es = "Exportar paradas"
  ),
  stops_autogen_title = c(
    en = "Auto-generate stops",
    fr = "G\u00e9n\u00e9ration automatique d'arr\u00eats",
    es = "Generaci\u00f3n autom\u00e1tica de paradas"
  ),
  stops_autogen_pop_1 = c(
    en = "Automatically generate stops at road intersections within a drawn zone using OpenStreetMap data. Stops are placed at intersections based on minimum stop spacing set in",
    fr = "G\u00e9n\u00e9rer automatiquement des arr\u00eats aux intersections routi\u00e8res dans une zone dessin\u00e9e \u00e0 partir des donn\u00e9es OpenStreetMap. Les arr\u00eats sont plac\u00e9s aux intersections en fonction de l'espacement minimal d\u00e9fini dans les",
    es = "Generar autom\u00e1ticamente paradas en las intersecciones viales dentro de una zona dibujada usando datos de OpenStreetMap. Las paradas se ubican en intersecciones seg\u00fan el espaciamiento m\u00ednimo definido en la"
  ),
  stops_autogen_pop_2 = c(
    en = "Settings.",
    fr = "param\u00e8tres.",
    es = "configuraci\u00f3n."
  ),

  # ════════════════════════════════════════════════════════════════════════
  # Stops module — dynamic content (translated via tr() in renderUI)
  # ════════════════════════════════════════════════════════════════════════

  # Stop list
  stops_add_new = c(
    en = "Add new stop",
    fr = "Ajouter un nouvel arr\u00eat",
    es = "Agregar nueva parada"
  ),
  stops_no_match = c(
    en = "No stops match your search",
    fr = "Aucun arr\u00eat ne correspond \u00e0 votre recherche",
    es = "Ninguna parada coincide con su b\u00fasqueda"
  ),
  stops_delete_title = c(
    en = "Delete stop",
    fr = "Supprimer l'arr\u00eat",
    es = "Eliminar parada"
  ),

  # Editing instructions
  stops_click_to_place = c(
    en = "Click on the map to place the stop",
    fr = "Cliquez sur la carte pour placer l'arr\u00eat",
    es = "Haga clic en el mapa para ubicar la parada"
  ),
  stops_drag_to_adjust = c(
    en = "Drag the marker to adjust position",
    fr = "D\u00e9placez le marqueur pour ajuster la position",
    es = "Arrastre el marcador para ajustar la posici\u00f3n"
  ),

  # Stop form labels
  lbl_stop_id = c(en = "Stop ID", fr = "ID arr\u00eat", es = "ID parada"),
  lbl_stop_name = c(
    en = "Stop name",
    fr = "Nom d'arr\u00eat",
    es = "Nombre de parada"
  ),
  pop_stop_id = c(
    en = "Unique identifier for a stop, station or platform.",
    fr = "Identifiant unique d'un arr\u00eat, d'une gare ou d'un quai.",
    es = "Identificador \u00fanico de una parada, estaci\u00f3n o plataforma."
  ),
  pop_stop_name = c(
    en = "Name of the stop, station or platform. It should match the agency's rider-facing name for the location as printed on a timetable, published online, or represented on signage.",
    fr = "Nom de l'arr\u00eat, de la gare ou du quai. Il doit correspondre au nom utilis\u00e9 par l'agence pour les usagers, tel qu'imprim\u00e9 sur un horaire, publi\u00e9 en ligne ou affich\u00e9 sur la signal\u00e9tique.",
    es = "Nombre de la parada, estaci\u00f3n o plataforma. Debe coincidir con el nombre que la agencia usa de cara al usuario, tal como aparece en horarios impresos, en l\u00ednea o en se\u00f1alizaci\u00f3n."
  ),
  stop_ph_id = c(
    en = "e.g., S001",
    fr = "p. ex., S001",
    es = "p. ej., S001"
  ),
  stop_ph_name = c(
    en = "e.g., Main St Station",
    fr = "p. ex., Gare Centrale",
    es = "p. ej., Salto del Agua"
  ),

  # Hover labels
  stops_hover_itins = c(
    en = "Itineraries",
    fr = "Parcours types",
    es = "Itinerarios"
  ),

  # Stop generation UI
  stops_gen_drawing = c(
    en = "Click on the map to draw the zone",
    fr = "Cliquez sur la carte pour dessiner la zone",
    es = "Haga clic en el mapa para dibujar la zona"
  ),
  stops_gen_vertex = c(en = "vertex", fr = "sommet", es = "v\u00e9rtice"),
  stops_gen_vertices = c(en = "vertices", fr = "sommets", es = "v\u00e9rtices"),
  stops_gen_zone_drawn = c(
    en = "Zone drawn. Click Generate below to confirm",
    fr = "Zone dessin\u00e9e. Cliquez sur G\u00e9n\u00e9rer pour confirmer",
    es = "Zona dibujada. Haga clic en Generar para confirmar"
  ),
  stops_gen_clear = c(
    en = "Clear zone",
    fr = "Effacer la zone",
    es = "Borrar zona"
  ),
  stops_gen_draw_btn = c(
    en = "Draw zone on map",
    fr = "Dessiner une zone sur la carte",
    es = "Dibujar zona en el mapa"
  ),

  # =============
  # Descriptive text — Intro panel
  # ========
  intro_tagline = c(
    en = "Croquis (crow-KEY) is a transit sketch planning tool and GTFS creator.",
    fr = "Croquis est un outil de planification du transport collectif et un cr\u00e9ateur de GTFS.",
    es = "Croquis es una herramienta de planificaci\u00f3n del transporte p\u00fablico y un creador de GTFS."
  ),
  intro_tabs = c(
    en = "The stops, routes and schedule tabs above allow you to manage all these aspects of your transit network model.",
    fr = "Les onglets arr\u00eats, lignes et horaires ci-dessus permettent de g\u00e9rer tous ces aspects de votre mod\u00e8le de r\u00e9seau de transport.",
    es = "Las pesta\u00f1as paradas, rutas y horarios de arriba permiten gestionar todos estos aspectos de su modelo de red de transporte."
  ),
  intro_get_started = c(
    en = "Get started on this page by loading an existing network, or by creating the agency details and project location if starting from scratch.",
    fr = "Commencez sur cette page en chargeant un r\u00e9seau existant, ou en cr\u00e9ant les d\u00e9tails de l'agence et l'emplacement du projet si vous partez de z\u00e9ro.",
    es = "Comience en esta p\u00e1gina cargando una red existente, o creando los detalles de la agencia y la ubicaci\u00f3n del proyecto si empieza desde cero."
  ),
  # Split pair. Full sentence (en): "This open-source software was developed
  # in R Shiny. It is in active development. Save your work often by clicking
  # the Save [floppy-disk icon] icon above and exporting your project file."
  intro_save_pre = c(
    en = "This open-source software was developed in R Shiny. It is in active development. Save your work often by clicking the Save",
    fr = "Ce logiciel libre a \u00e9t\u00e9 d\u00e9velopp\u00e9 en R Shiny. Il est en d\u00e9veloppement actif. Enregistrez souvent votre travail en cliquant sur l'ic\u00f4ne Enregistrer",
    es = "Este software de c\u00f3digo abierto fue desarrollado en R Shiny. Est\u00e1 en desarrollo activo. Guarde su trabajo con frecuencia haciendo clic en el \u00edcono Guardar"
  ),
  intro_save_post = c(
    en = "icon above and exporting your project file.",
    fr = "ci-dessus et en exportant votre fichier de projet.",
    es = "de arriba y exportando su archivo de proyecto."
  ),
  # Split pair. Full sentence (en): "Please report any bugs and provide your
  # ideas for improvement by submitting an [issue on GitHub]." The article at
  # the end of *_pre agrees in gender/number with intro_report_link.
  intro_report_pre = c(
    en = "Please report any bugs and provide your ideas for improvement by submitting an",
    fr = "Veuillez signaler tout bogue et proposer vos id\u00e9es d'am\u00e9lioration en soumettant une",
    es = "Reporte cualquier error y comparta sus ideas de mejora enviando un"
  ),
  intro_report_link = c(
    en = "issue on GitHub",
    fr = "issue sur GitHub",
    es = "issue en GitHub"
  ),

  # File inputs. btn_browse is live-switchable (data-i18n span);
  # file_placeholder is build-time only (fileInput placeholder accepts
  # character only) and is deliberately absent from inst/www/js/i18n.js.
  btn_browse = c(
    en = "Browse...",
    fr = "Parcourir...",
    es = "Examinar..."
  ),
  file_placeholder = c(
    en = "Drag and drop or click to select file",
    fr = "Glissez-d\u00e9posez ou cliquez pour s\u00e9lectionner un fichier",
    es = "Arrastre y suelte o haga clic para seleccionar un archivo"
  ),

  # Notifications ===============
  notif_stop_added = c(
    en = "Stop %s added",
    fr = "Arr\u00eat %s ajout\u00e9",
    es = "Parada %s agregada"
  ),
  notif_stop_updated = c(
    en = "Stop %s updated",
    fr = "Arr\u00eat %s mis \u00e0 jour",
    es = "Parada %s actualizada"
  ),
  notif_stop_deleted = c(
    en = "Stop deleted successfully",
    fr = "Arr\u00eat supprim\u00e9 avec succ\u00e8s",
    es = "Parada eliminada exitosamente"
  ),
  notif_stop_cant_delete = c(
    en = "Cannot delete stop '%s'. It is used in itineraries: %s. Remove it from those itineraries first.",
    fr = "Impossible de supprimer l'arr\u00eat \u00ab\u00a0%s\u00a0\u00bb. Il est utilis\u00e9 dans les parcours types\u00a0: %s. Retirez-le de ces parcours types d'abord.",
    es = "No se puede eliminar la parada '%s'. Se usa en los itinerarios: %s. Ret\u00edrela de esos itinerarios primero."
  ),
  notif_draw_3pts = c(
    en = "Draw at least 3 points to define a zone.",
    fr = "Dessinez au moins 3 points pour d\u00e9finir une zone.",
    es = "Dibuje al menos 3 puntos para definir una zona."
  ),
  notif_draw_zone_first = c(
    en = "Draw a zone on the map first.",
    fr = "Dessinez d'abord une zone sur la carte.",
    es = "Dibuje primero una zona en el mapa."
  ),
  notif_gen_progress = c(
    en = "Downloading OSM data and generating stops. This may take a while depending on region and OSM provider (manage in Settings).",
    fr = "T\u00e9l\u00e9chargement des donn\u00e9es OSM et g\u00e9n\u00e9ration des arr\u00eats. Cela peut prendre un moment selon la r\u00e9gion et le fournisseur OSM (g\u00e9rer dans les param\u00e8tres).",
    es = "Descargando datos OSM y generando paradas. Esto puede tardar seg\u00fan la regi\u00f3n y el proveedor OSM (administrar en configuraci\u00f3n)."
  ),
  notif_gen_none = c(
    en = "No eligible stop locations found in this zone.",
    fr = "Aucun emplacement d'arr\u00eat admissible trouv\u00e9 dans cette zone.",
    es = "No se encontraron ubicaciones de paradas elegibles en esta zona."
  ),
  notif_gen_added = c(
    en = "%s stops generated and added.",
    fr = "%s arr\u00eats g\u00e9n\u00e9r\u00e9s et ajout\u00e9s.",
    es = "%s paradas generadas y agregadas."
  ),
  notif_gen_failed = c(
    en = "Stop generation failed, %s ...Try changing OSM Provider in Settings.",
    fr = "\u00c9chec de la g\u00e9n\u00e9ration d'arr\u00eats, %s ...Essayez de changer le fournisseur OSM dans les param\u00e8tres.",
    es = "Fallo en la generaci\u00f3n de paradas, %s ...Intente cambiar el proveedor OSM en la configuraci\u00f3n."
  ),
  notif_import_bad_format = c(
    en = "Invalid file format. Please upload a GeoJSON or KML file.",
    fr = "Format de fichier invalide. Veuillez t\u00e9l\u00e9verser un fichier GeoJSON ou KML.",
    es = "Formato de archivo inv\u00e1lido. Suba un archivo GeoJSON o KML."
  ),
  notif_import_pts_only = c(
    en = "Only point geometries can be imported.",
    fr = "Seules les g\u00e9om\u00e9tries ponctuelles peuvent \u00eatre import\u00e9es.",
    es = "Solo se pueden importar geometr\u00edas de punto."
  ),
  notif_import_too_many = c(
    en = "File contains more than 99,999 features. Please reduce the file size.",
    fr = "Le fichier contient plus de 99\u00a0999 entit\u00e9s. Veuillez r\u00e9duire la taille du fichier.",
    es = "El archivo contiene m\u00e1s de 99.999 entidades. Reduzca el tama\u00f1o del archivo."
  ),
  notif_import_no_cols = c(
    en = "File must contain 'stop_id' and 'stop_name' columns.",
    fr = "Le fichier doit contenir les colonnes \u00ab\u00a0stop_id\u00a0\u00bb et \u00ab\u00a0stop_name\u00a0\u00bb.",
    es = "El archivo debe contener las columnas 'stop_id' y 'stop_name'."
  ),
  notif_import_all_dup = c(
    en = "All stops in the file have IDs that already exist. No new stops imported.",
    fr = "Tous les arr\u00eats du fichier ont des identifiants d\u00e9j\u00e0 existants. Aucun nouvel arr\u00eat import\u00e9.",
    es = "Todas las paradas del archivo tienen IDs que ya existen. No se importaron nuevas paradas."
  ),
  notif_import_dup_skip = c(
    en = "%s stops with duplicate IDs were skipped.",
    fr = "%s arr\u00eats avec des identifiants en double ont \u00e9t\u00e9 ignor\u00e9s.",
    es = "%s paradas con IDs duplicados fueron omitidas."
  ),
  notif_import_success = c(
    en = "Successfully imported %s stops.",
    fr = "%s arr\u00eats import\u00e9s avec succ\u00e8s.",
    es = "%s paradas importadas exitosamente."
  ),
  notif_import_error = c(
    en = "Error importing file: %s",
    fr = "Erreur lors de l'importation du fichier\u00a0: %s",
    es = "Error al importar el archivo: %s"
  ),
  notif_export_empty = c(
    en = "No stops to export.",
    fr = "Aucun arr\u00eat \u00e0 exporter.",
    es = "No hay paradas para exportar."
  ),
  notif_export_error = c(
    en = "Error exporting file: %s",
    fr = "Erreur lors de l'exportation du fichier\u00a0: %s",
    es = "Error al exportar el archivo: %s"
  )
)


# ── Helper: build language selector options ──────────────────────────────
#
# Generates a list of tags$option(...) elements from SUPPORTED_LANGS,
# marking the one matching `selected` as the default.
# Used in croquis-app.R to build the <select> dropdown.

#' Build language selector option tags
#'
#' @param selected Character language code to pre-select.
#' @return A list of [htmltools::tags]`$option` elements.
#' @keywords internal
#' @noRd
build_lang_options <- function(selected = "en") {
  lapply(names(SUPPORTED_LANGS), function(code) {
    htmltools::tags$option(
      value = code,
      selected = if (code == selected) NA else NULL,
      SUPPORTED_LANGS[[code]]
    )
  })
}


#' Translate a UI string
#'
#' Looks up `key` in [i18n_dict] and returns the value for `lang`,
#' falling back to English if the requested language is missing.
#' Returns `[key]` when the key itself is not found, making
#' untranslated strings visually obvious during development.
#'
#' For sprintf-style templates (keys containing `%s`), call
#' `sprintf(tr("key", lang), ...)` at the call site.
#'
#' @param key Character string. A key in `i18n_dict`.
#' @param lang Character string. Language code (`"en"`, `"fr"`, `"es"`, ...).
#' @return The translated string.
#' @keywords internal
#' @noRd
tr <- function(key, lang = "en") {
  entry <- i18n_dict[[key]]
  if (is.null(entry)) {
    return(paste0("[", key, "]"))
  }
  out <- entry[[lang]]
  if (is.null(out) || is.na(out)) {
    out <- entry[["en"]]
  }
  out
}
