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
    es = "Para explorar esta herramienta, puede comenzar cargando una red de ejemplo. El modelo Ligne Jaune es el m\u00e1s sencillo y le ayudar\u00e1 a familiarizarse con el funcionamiento de Croquis."
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
  # Set at build time; tagged for live switching via i18n_placeholder()
  # and mirrored in inst/www/js/i18n.js.
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

  # ── Popover content ────────────────────────────────────────────────────
  # lbl_read_more is shared app-wide (used inside info_popover for the
  # link line); mirrored in inst/www/js/i18n.js.
  lbl_read_more = c(
    en = "Read more",
    fr = "En savoir plus",
    es = "Leer m\u00e1s"
  ),
  # Static UI popover (data-i18n-popover; mirrored in JS)
  pop_city_search = c(
    en = "Start typing a city name and select city, if starting project from scratch. If you are not able to find your city, you may need to set coordinates manually below.",
    fr = "Commencez \u00e0 saisir le nom d'une ville et s\u00e9lectionnez-la si vous d\u00e9marrez un projet de z\u00e9ro. Si vous ne trouvez pas votre ville, vous devrez peut-\u00eatre d\u00e9finir les coordonn\u00e9es manuellement ci-dessous.",
    es = "Empiece a escribir el nombre de una ciudad y selecci\u00f3nela si inicia un proyecto desde cero. Si no encuentra su ciudad, puede que necesite definir las coordenadas manualmente abajo."
  ),
  # Dynamic renderUI popovers (re-translated by re-render; R-only)
  pop_agency_id = c(
    en = "Identifies a unique transit agency or transit brand.",
    fr = "Identifie une agence de transport ou une marque de transport unique.",
    es = "Identifica una agencia de transporte o una marca de transporte \u00fanica."
  ),
  pop_agency_name = c(
    en = "Full name of the transit agency.",
    fr = "Nom complet de l'agence de transport.",
    es = "Nombre completo de la agencia de transporte."
  ),
  pop_agency_url = c(
    en = "URL of the transit agency.",
    fr = "URL de l'agence de transport.",
    es = "URL de la agencia de transporte."
  ),
  pop_agency_tz = c(
    en = "Timezone in IANA tz database format.",
    fr = "Fuseau horaire au format de la base de donn\u00e9es tz de l'IANA.",
    es = "Zona horaria en el formato de la base de datos tz de la IANA."
  ),

  # Stops import file input placeholder (live-switchable via
  # i18n_placeholder(); mirrored in inst/www/js/i18n.js)
  stops_import_ph = c(
    en = "GeoJSON or KML file",
    fr = "Fichier GeoJSON ou KML",
    es = "Archivo GeoJSON o KML"
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
    es = "Agregar una nueva agencia"
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

  # ── Instructions panel ─────────────────────────────────────────────────
  # Rendered fully server-side (output$home_instructions_ui) via the
  # reactive tr(lang()) pattern; R-only, no JS mirror. Bullet keys follow
  # the strict naming scheme instr_s{step}_li{n} consumed programmatically
  # by build_instr_list().
  instr_title = c(
    en = "Instructions",
    fr = "Instructions",
    es = "Instrucciones"
  ),
  instr_intro = c(
    en = "Build your transit system model by following these steps:",
    fr = "Construisez votre mod\u00e8le de r\u00e9seau de transport en suivant ces \u00e9tapes\u00a0:",
    es = "Construya su modelo de red de transporte siguiendo estos pasos:"
  ),
  instr_s1 = c(
    en = "1. Get started here by loading an existing network or specifying agency details for a new one",
    fr = "1. Commencez ici en chargeant un r\u00e9seau existant ou en pr\u00e9cisant les d\u00e9tails de l'agence pour un nouveau r\u00e9seau",
    es = "1. Comience aqu\u00ed cargando una red existente o especificando los detalles de la agencia para una nueva"
  ),
  instr_s1_li1 = c(
    en = "Load a GTFS or a network that you've previously worked on in Croquis",
    fr = "Chargez un GTFS ou un r\u00e9seau sur lequel vous avez d\u00e9j\u00e0 travaill\u00e9 dans Croquis",
    es = "Cargue un GTFS o una red en la que haya trabajado previamente en Croquis"
  ),
  instr_s1_li2 = c(
    en = "Set the location of your network, if you're starting a network from scratch",
    fr = "D\u00e9finissez l'emplacement de votre r\u00e9seau si vous partez de z\u00e9ro",
    es = "Defina la ubicaci\u00f3n de su red si empieza desde cero"
  ),
  instr_s1_li3 = c(
    en = "View and edit agency details.",
    fr = "Consultez et modifiez les d\u00e9tails de l'agence.",
    es = "Consulte y edite los detalles de la agencia."
  ),
  instr_s2 = c(
    en = "2. Create and edit stops in the stops module",
    fr = "2. Cr\u00e9ez et modifiez des arr\u00eats dans le module des arr\u00eats",
    es = "2. Cree y edite paradas en el m\u00f3dulo de paradas"
  ),
  instr_s2_li1 = c(
    en = "Manage and create stops using the left-hand panel",
    fr = "G\u00e9rez et cr\u00e9ez des arr\u00eats \u00e0 l'aide du panneau de gauche",
    es = "Gestione y cree paradas con el panel izquierdo"
  ),
  instr_s2_li2 = c(
    en = "When creating or editing a stop, click on the map or drag the stop to set its location.",
    fr = "Lors de la cr\u00e9ation ou de la modification d'un arr\u00eat, cliquez sur la carte ou d\u00e9placez l'arr\u00eat pour d\u00e9finir son emplacement.",
    es = "Al crear o editar una parada, haga clic en el mapa o arrastre la parada para definir su ubicaci\u00f3n."
  ),
  instr_s2_li3 = c(
    en = "Provide unique stop IDs and stop names for each stop",
    fr = "Attribuez un ID et un nom d'arr\u00eat uniques \u00e0 chaque arr\u00eat",
    es = "Asigne un ID y un nombre de parada \u00fanicos a cada parada"
  ),
  instr_s3 = c(
    en = "3. Create your routes and route itineraries in the routes module",
    fr = "3. Cr\u00e9ez vos lignes et leurs parcours types dans le module des lignes",
    es = "3. Cree sus rutas y sus itinerarios en el m\u00f3dulo de rutas"
  ),
  instr_s3_li1 = c(
    en = "Create routes with their details (mode, colours) and define route itineraries within each route.",
    fr = "Cr\u00e9ez des lignes avec leurs d\u00e9tails (mode, couleurs) et d\u00e9finissez des parcours types au sein de chaque ligne.",
    es = "Cree rutas con sus detalles (modo, colores) y defina itinerarios dentro de cada ruta."
  ),
  instr_s3_li2 = c(
    en = "A route itinerary corresponds to a unique stop pattern for trips. Each itinerary is associated with a stop sequence and a shape.",
    fr = "Un parcours type correspond \u00e0 un encha\u00eenement d'arr\u00eats unique pour les voyages. Chaque parcours type est associ\u00e9 \u00e0 une s\u00e9quence d'arr\u00eats et \u00e0 un trac\u00e9.",
    es = "Un itinerario corresponde a un patr\u00f3n de paradas \u00fanico para los viajes. Cada itinerario est\u00e1 asociado a una secuencia de paradas y a un trazado."
  ),
  instr_s3_li3 = c(
    en = "Create and edit route geometries by selecting stops in the desired order and by creating waypoints by clicking on the map and along the route. You may delete waypoints or remove stops from a route itinerary by right-clicking.",
    fr = "Cr\u00e9ez et modifiez les g\u00e9om\u00e9tries des lignes en s\u00e9lectionnant les arr\u00eats dans l'ordre souhait\u00e9 et en cr\u00e9ant des points de passage en cliquant sur la carte et le long de la ligne. Vous pouvez supprimer des points de passage ou retirer des arr\u00eats d'un parcours type par un clic droit.",
    es = "Cree y edite las geometr\u00edas de las rutas seleccionando las paradas en el orden deseado y creando puntos de paso haciendo clic en el mapa y a lo largo de la ruta. Puede eliminar puntos de paso o quitar paradas de un itinerario con clic derecho."
  ),
  instr_s3_li4 = c(
    en = "Move a waypoint by clicking on it and activating editing mode. Click on the desired location on the map or on a stop to move the waypoint there. If clicked on a stop, it will be added to the sequence.",
    fr = "D\u00e9placez un point de passage en cliquant dessus pour activer le mode d'\u00e9dition. Cliquez sur l'emplacement souhait\u00e9 sur la carte ou sur un arr\u00eat pour y d\u00e9placer le point de passage. Si vous cliquez sur un arr\u00eat, celui-ci sera ajout\u00e9 \u00e0 la s\u00e9quence.",
    es = "Mueva un punto de paso haciendo clic en \u00e9l para activar el modo de edici\u00f3n. Haga clic en la ubicaci\u00f3n deseada en el mapa o en una parada para mover el punto de paso all\u00ed. Si hace clic en una parada, esta se agregar\u00e1 a la secuencia."
  ),
  instr_s3_li5 = c(
    en = "Toggle between network and simple drawing modes. Network drawing mode calculates the path along the OpenStreetMap road network between stops and waypoints.",
    fr = "Basculez entre les modes de dessin r\u00e9seau et simple. Le mode r\u00e9seau calcule le trajet le long du r\u00e9seau routier d'OpenStreetMap entre les arr\u00eats et les points de passage.",
    es = "Alterne entre los modos de dibujo de red y simple. El modo de red calcula el trayecto a lo largo de la red vial de OpenStreetMap entre paradas y puntos de paso."
  ),
  instr_s3_li6 = c(
    en = "Toggle between prepending and appending stops when drawing a route itinerary. Prepend mode adds stops clicked to the beginning of the stop sequence (the default is that stops clicked are added to the end).",
    fr = "Basculez entre l'ajout des arr\u00eats au d\u00e9but ou \u00e0 la fin lors du dessin d'un itin\u00e9raire. Le mode d'ajout au d\u00e9but ins\u00e8re les arr\u00eats cliqu\u00e9s au d\u00e9but de la s\u00e9quence d'arr\u00eats (par d\u00e9faut, les arr\u00eats cliqu\u00e9s sont ajout\u00e9s \u00e0 la fin).",
    es = "Alterne entre agregar paradas al inicio o al final al dibujar un itinerario. El modo de inserci\u00f3n al inicio agrega las paradas seleccionadas al principio de la secuencia (por defecto, las paradas se agregan al final)."
  ),
  instr_s4 = c(
    en = "4. Define and edit service levels and speeds for routes in the schedule module",
    fr = "4. D\u00e9finissez et modifiez les niveaux de service et les vitesses des lignes dans le module des horaires",
    es = "4. Defina y edite los niveles de servicio y las velocidades de las rutas en el m\u00f3dulo de horarios"
  ),
  instr_s4_li1 = c(
    en = "Bulk apply preset service levels (e.g. all-day frequent or peak frequent), speeds and operating hours to routes by service.",
    fr = "Appliquez en lot des niveaux de service pr\u00e9d\u00e9finis (p.\u00a0ex. fr\u00e9quent toute la journ\u00e9e ou fr\u00e9quent en pointe), des vitesses et des heures d'exploitation aux lignes par service.",
    es = "Aplique en bloque niveles de servicio predefinidos (p.\u00a0ej. frecuente todo el d\u00eda o frecuente en hora punta), velocidades y horas de operaci\u00f3n a las rutas por servicio."
  ),
  instr_s4_li2 = c(
    en = "View cumulative service-level by route segment by hour by clicking on the map.",
    fr = "Consultez le niveau de service cumul\u00e9 par segment de ligne et par heure en cliquant sur la carte.",
    es = "Consulte el nivel de servicio acumulado por segmento de ruta y por hora haciendo clic en el mapa."
  ),
  instr_s4_li3 = c(
    en = "Apply preset service levels, speeds and operating hours for individual route itineraries.",
    fr = "Appliquez des niveaux de service, des vitesses et des heures d'exploitation pr\u00e9d\u00e9finis aux parcours types individuels.",
    es = "Aplique niveles de servicio, velocidades y horas de operaci\u00f3n predefinidos a itinerarios individuales."
  ),
  instr_s4_li4 = c(
    en = "Define and edit headways and speeds by hour in detail for individual route itineraries, if desired.",
    fr = "D\u00e9finissez et modifiez en d\u00e9tail les intervalles et les vitesses par heure pour les parcours types individuels, si d\u00e9sir\u00e9.",
    es = "Defina y edite en detalle los intervalos y las velocidades por hora para itinerarios individuales, si lo desea."
  ),
  instr_s4_li5 = c(
    en = "View and toggle interstop speeds at the route itinerary level, if desired.",
    fr = "Consultez et ajustez les vitesses interarr\u00eats au niveau du parcours type, si d\u00e9sir\u00e9.",
    es = "Consulte y ajuste las velocidades entre paradas a nivel de itinerario, si lo desea."
  ),
  instr_s4_li6 = c(
    en = "Manage service level presets, create them from scratch, or create them based on the service level of an existing route itinerary.",
    fr = "G\u00e9rez les niveaux de service pr\u00e9d\u00e9finis, cr\u00e9ez-les de toutes pi\u00e8ces ou cr\u00e9ez-les \u00e0 partir du niveau de service d'un parcours type existant.",
    es = "Gestione los niveles de servicio predefinidos, cr\u00e9elos desde cero o cr\u00e9elos a partir del nivel de servicio de un itinerario existente."
  ),
  instr_s4_li7 = c(
    en = "Manage service calendar, including start and end dates for services defined by day of the week active (e.g. weekday vs. weekend service).",
    fr = "G\u00e9rez le calendrier de service, y compris les dates de d\u00e9but et de fin des services d\u00e9finis par jours de la semaine actifs (p.\u00a0ex. service en semaine ou de fin de semaine).",
    es = "Gestione el calendario de servicio, incluidas las fechas de inicio y fin de los servicios definidos por los d\u00edas de la semana activos (p.\u00a0ej. servicio entre semana o de fin de semana)."
  ),
  # Split pair. Full sentence (en): "5. Click the save [floppy-disk icon]
  # icon to export a GTFS or save your croquis in .rds format to work on
  # it later"
  instr_s5_pre = c(
    en = "5. Click the save",
    fr = "5. Cliquez sur l'ic\u00f4ne Enregistrer",
    es = "5. Haga clic en el \u00edcono Guardar"
  ),
  instr_s5_post = c(
    en = "icon to export a GTFS or save your croquis in .rds format to work on it later",
    fr = "pour exporter un GTFS ou sauvegarder votre croquis au format .rds afin d'y travailler plus tard",
    es = "para exportar un GTFS o guardar su croquis en formato .rds para trabajar en \u00e9l m\u00e1s tarde"
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
  # Popover content for the auto-generate section. {icon} is replaced by
  # the gear icon HTML at build time (info_popover) and at language
  # switch (jsTr token replacement). Mirrored in inst/www/js/i18n.js.
  stops_autogen_pop = c(
    en = "Automatically generate stops within a drawn zone. Stops are placed at intersections based on the minimum spacing defined in the {icon} settings.",
    fr = "G\u00e9n\u00e9rez automatiquement des arr\u00eats dans une zone dessin\u00e9e. Les arr\u00eats sont plac\u00e9s aux intersections en fonction de l'espacement minimal d\u00e9fini dans les {icon} param\u00e8tres.",
    es = "Genere autom\u00e1ticamente paradas dentro de una zona dibujada. Las paradas se colocan en las intersecciones seg\u00fan el espaciamiento m\u00ednimo definido en la {icon} configuraci\u00f3n."
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

  # File inputs. btn_browse is live-switchable (data-i18n span).
  # file_placeholder is set at build time and tagged for live switching
  # via i18n_placeholder() (see R/utils-ui.R); mirrored in i18n.js.
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
