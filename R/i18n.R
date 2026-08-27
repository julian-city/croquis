# == Croquis i18n infrastructure ===================================
#
# Custom dictionary-based internationalisation for the Croquis Shiny app.
# Each translatable string is stored as a named character vector keyed by
# ISO 639-1 language code (en / fr / es / ...).  The helper `tr()` performs
# the lookup at render time.
#
# -- Adding a new language --
#
#   1. Add the language code and display label to `SUPPORTED_LANGS` below.
#   2. Add a new element to every vector in `i18n_dict` with the translated
#      string  (e.g.  pt = "...").  Keys with no translation for the new
#      language fall back to English automatically.
#   3. Mirror the new entries in `inst/www/js/i18n.js` (only the keys used
#      in static UI and JS confirm dialogs need to be mirrored).
#   4. No other code changes are needed - the language selector, `match.arg`
#      validation, and JS sync all derive from `SUPPORTED_LANGS`.
#
# -- Adding a new translatable string --
#
#   1. Pick a snake_case key.
#   2. Add it to `i18n_dict` with at least an `en` entry.
#   3. Use `tr("key", lang)` in R code or `jsTr("key")` in JS code.
#   4. Untranslated keys render as [key] - easy to spot during development.
#
# --- R package notes (per https://r-pkgs.org/code.html) ---
#
#   * All non-ASCII characters use \uXXXX escapes for CRAN portability.
#   * `i18n_dict` and `SUPPORTED_LANGS` are top-level objects evaluated at
#     build time.  This is intentional and correct: the dictionary is static
#     data that does not depend on the user's system.
#   * `tr()` is a function and executes at run time, which is the correct
#     pattern for code that reads from the dictionary.
#   * Neither `i18n_dict`, `SUPPORTED_LANGS`, nor `tr()` are exported.
#     They are package-internal, used only by the Shiny UI layer.
#

# --- Supported languages --------------
#
# Central registry of supported languages.  Used by:
#   * `croquis(lang = ...)` - `match.arg()` validation
#   * The language `<select>` in the navbar - option generation
#   * JS `croquisLang` initialisation
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

# --- Translation dictionary --------------------------------

#' i18n translation dictionary
#'
#' Named list of character vectors.
#' Each key maps to a named vector of translations keyed by language code.
#'
#' @keywords internal
#' @noRd
i18n_dict <- list(
  # ========================================================================
  # Navigation tabs
  # ========================================================================
  tab_stops = c(en = "stops", fr = "arr\u00eats", es = "paradas"),
  tab_routes = c(en = "routes", fr = "lignes", es = "rutas"),
  tab_schedule = c(en = "schedule", fr = "horaires", es = "horarios"),

  # ========================================================================
  # Shared / common
  # ========================================================================
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

  # ========================================================================
  # home module (static UI, translated JS updateI18n)
  # ========================================================================

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

  # -- Project Location panel --------------
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

  # -- Popover content ------------------------------------------------
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

  # -- Agencies panel -----------------------------------------------
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

  # Notifications - Agencies
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

  # -- Instructions panel -------------------------------------------------
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

  # ========================================================================
  # Stops module - panel chrome (static UI, translated via JS updateI18n)
  # ========================================================================
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

  # ========================================================================
  # Stops module - dynamic content (translated via tr() in renderUI)
  # ========================================================================

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
  # Descriptive text - Intro panel
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
  ),

  # ========================================================================
  # Routes module - panel chrome (static UI, translated via JS updateI18n)
  # ========================================================================
  routes_title = c(
    en = "routes",
    fr = "lignes",
    es = "rutas"
  ),
  routes_panel_title = c(
    en = "Routes",
    fr = "Lignes",
    es = "Rutas"
  ),
  routes_drawing_title = c(
    en = "Drawing Mode",
    fr = "Mode de dessin",
    es = "Modo de dibujo"
  ),
  routes_stopseq_title = c(
    en = "Stop Sequence",
    fr = "S\u00e9quence d'arr\u00eats",
    es = "Secuencia de paradas"
  ),
  routes_drawing_desc = c(
    en = "Network mode routes along streets. Free mode draws straight lines between stops and waypoints.",
    fr = "Le mode r\u00e9seau trace le parcours le long des rues. Le mode libre dessine des lignes droites entre les arr\u00eats et les points de passage.",
    es = "El modo red traza la ruta a lo largo de las calles. El modo libre dibuja l\u00edneas rectas entre paradas y puntos de paso."
  ),

  # ========================================================================
  # Routes module - dynamic content (translated via tr() in renderUI)
  # ========================================================================

  # Route list
  routes_add_new = c(
    en = "Add new route",
    fr = "Ajouter une nouvelle ligne",
    es = "Agregar nueva ruta"
  ),
  routes_add_itin = c(
    en = "Add new itinerary",
    fr = "Ajouter un nouveau parcours type",
    es = "Agregar nuevo itinerario"
  ),
  routes_edit_title = c(
    en = "Edit route",
    fr = "Modifier la ligne",
    es = "Editar ruta"
  ),
  routes_copy_title = c(
    en = "Duplicate route",
    fr = "Dupliquer la ligne",
    es = "Duplicar ruta"
  ),
  routes_delete_title = c(
    en = "Delete route",
    fr = "Supprimer la ligne",
    es = "Eliminar ruta"
  ),
  lbl_copy = c(
    en = "Copy",
    fr = "Copier",
    es = "Copiar"
  ),
  itin_edit_title = c(
    en = "Edit itinerary",
    fr = "Modifier le parcours type",
    es = "Editar itinerario"
  ),
  itin_copy_title = c(
    en = "Duplicate itinerary",
    fr = "Dupliquer le parcours type",
    es = "Duplicar itinerario"
  ),
  itin_delete_title = c(
    en = "Delete itinerary",
    fr = "Supprimer le parcours type",
    es = "Eliminar itinerario"
  ),
  #Itinerary detail preview (routes panel)
  itin_detail_metrics = c(
    en = "%s stops - %s km",
    fr = "%s arr\u00eats - %s km",
    es = "%s paradas - %s km"
  ),

  # Editing instructions
  routes_editing_itin = c(
    en = "Editing: %s",
    fr = "\u00c9dition\u00a0: %s",
    es = "Editando: %s"
  ),
  routes_prepend_msg = c(
    en = "Prepend mode: next stop clicks will be added to the START of the sequence.",
    fr = "Mode d'insertion au d\u00e9but\u00a0: les prochains arr\u00eats cliqu\u00e9s seront ajout\u00e9s au D\u00c9BUT de la s\u00e9quence.",
    es = "Modo de inserci\u00f3n al inicio: las pr\u00f3ximas paradas seleccionadas se agregar\u00e1n al INICIO de la secuencia."
  ),
  routes_click_to_build = c(
    en = "Click stops to build sequence. Right-click to remove.",
    fr = "Cliquez sur les arr\u00eats pour construire la s\u00e9quence. Clic droit pour supprimer.",
    es = "Haga clic en las paradas para construir la secuencia. Clic derecho para eliminar."
  ),
  routes_prepend_label = c(
    en = "Prepend stops to start of sequence",
    fr = "Ins\u00e9rer les arr\u00eats au d\u00e9but de la s\u00e9quence",
    es = "Insertar paradas al inicio de la secuencia"
  ),

  # Drawing mode toggle
  routes_mode_network = c(
    en = "Road Network",
    fr = "R\u00e9seau routier",
    es = "Red vial"
  ),
  routes_mode_free = c(
    en = "Free Drawing",
    fr = "Dessin libre",
    es = "Dibujo libre"
  ),

  # -- Route form labels --------------------------------------------------
  lbl_route_id = c(
    en = "Route ID",
    fr = "ID de la ligne",
    es = "ID de ruta"
  ),
  lbl_agency = c(
    en = "Agency",
    fr = "Agence",
    es = "Agencia"
  ),
  lbl_short_name = c(
    en = "Short name",
    fr = "Nom court",
    es = "Nombre corto"
  ),
  lbl_long_name = c(
    en = "Long name",
    fr = "Nom long",
    es = "Nombre largo"
  ),
  lbl_route_type = c(
    en = "Route type",
    fr = "Type de ligne",
    es = "Tipo de ruta"
  ),
  lbl_route_colour = c(
    en = "Route colour",
    fr = "Couleur de la ligne",
    es = "Color de ruta"
  ),
  lbl_text_colour = c(
    en = "Text colour",
    fr = "Couleur du texte",
    es = "Color del texto"
  ),

  # -- Route form popovers ------------------------------------------------
  pop_route_id = c(
    en = "Unique identifier for route.",
    fr = "Identifiant unique de la ligne.",
    es = "Identificador \u00fanico de la ruta."
  ),
  pop_route_agency = c(
    en = "Agency for specified route.",
    fr = "Agence de la ligne sp\u00e9cifi\u00e9e.",
    es = "Agencia de la ruta especificada."
  ),
  pop_route_short_name = c(
    en = "Short name of a route. Often a short, abstract identifier (e.g., '32', '100X', 'Green') that riders use to identify a route.",
    fr = "Nom court d'une ligne. Souvent un identifiant court et abstrait (p.\u00a0ex. \u00ab\u00a032\u00a0\u00bb, \u00ab\u00a0100X\u00a0\u00bb, \u00ab\u00a0Verte\u00a0\u00bb) utilis\u00e9 par les usagers pour identifier une ligne.",
    es = "Nombre corto de una ruta. A menudo un identificador breve y abstracto (p.\u00a0ej. '32', '100X', 'Verde') que los usuarios usan para identificar una ruta."
  ),
  pop_route_long_name = c(
    en = "Full name of a route. This name is generally more descriptive than the route_short_name and often includes the route's destination or stop.",
    fr = "Nom complet d'une ligne. Ce nom est g\u00e9n\u00e9ralement plus descriptif que le nom court et inclut souvent la destination ou l'arr\u00eat terminal de la ligne.",
    es = "Nombre completo de una ruta. Este nombre es generalmente m\u00e1s descriptivo que el nombre corto y suele incluir el destino o la parada terminal de la ruta."
  ),
  pop_route_type = c(
    en = "Indicates the mode of transportation used on a route.",
    fr = "Indique le mode de transport utilis\u00e9 sur une ligne.",
    es = "Indica el tipo de transporte utilizado en una ruta."
  ),
  pop_route_colour = c(
    en = "Route colour designation that matches public facing material.",
    fr = "D\u00e9signation de la couleur de la ligne correspondant au mat\u00e9riel destin\u00e9 au public.",
    es = "Designaci\u00f3n de color de ruta que corresponde al material de cara al p\u00fablico."
  ),
  pop_route_text_colour = c(
    en = "Legible color to use for text drawn against a background of route_color.",
    fr = "Couleur lisible \u00e0 utiliser pour le texte affich\u00e9 sur un fond de la couleur de la ligne.",
    es = "Color legible para el texto presentado sobre un fondo del color de la ruta."
  ),

  # -- Route form placeholders --------------------------------------------
  route_ph_id = c(
    en = "e.g., 14",
    fr = "p.\u00a0ex. 18",
    es = "p.\u00a0ej. 14"
  ),
  route_ph_short_name = c(
    en = "e.g., 14",
    fr = "p.\u00a0ex. 18",
    es = "p.\u00a0ej. 14"
  ),
  route_ph_long_name = c(
    en = "e.g., Hastings / UBC",
    fr = "p.\u00a0ex. Beaubien",
    es = "p.\u00a0ej. Hastings / UBC"
  ),

  # -- Route type options -------------------------------------------------
  route_type_bus = c(
    en = "Bus",
    fr = "Bus",
    es = "Autob\u00fas"
  ),
  route_type_tram = c(
    en = "Tram",
    fr = "Tramway",
    es = "Tranv\u00eda"
  ),
  route_type_metro = c(
    en = "Metro",
    fr = "M\u00e9tro",
    es = "Metro"
  ),
  route_type_rail = c(
    en = "Rail",
    fr = "Train",
    es = "Tren"
  ),
  route_type_ferry = c(
    en = "Ferry",
    fr = "Traversier",
    es = "Ferry"
  ),
  route_type_cable_tram = c(
    en = "Cable tram",
    fr = "Tramway \u00e0 c\u00e2ble",
    es = "Tranv\u00eda de cable"
  ),
  route_type_gondola = c(
    en = "Gondola",
    fr = "T\u00e9l\u00e9ph\u00e9rique",
    es = "Telef\u00e9rico"
  ),
  route_type_funicular = c(
    en = "Funicular",
    fr = "Funiculaire",
    es = "Funicular"
  ),
  route_type_trolleybus = c(
    en = "Trolleybus",
    fr = "Trolleybus",
    es = "Troleb\u00fas"
  ),
  route_type_monorail = c(
    en = "Monorail",
    fr = "Monorail",
    es = "Monorriel"
  ),

  # -- Itinerary form labels ----------------------------------------------
  lbl_itin_id = c(
    en = "Itinerary ID",
    fr = "ID du parcours type",
    es = "ID del itinerario"
  ),
  lbl_direction = c(
    en = "Direction",
    fr = "Direction",
    es = "Direcci\u00f3n"
  ),
  lbl_trip_headsign = c(
    en = "Trip Headsign",
    fr = "Girouette",
    es = "Letrero de destino"
  ),

  # -- Itinerary form popovers -------------------------------------------
  pop_itin_id = c(
    en = "Unique ID for this itinerary or variant of the route. Will be used as the trip_id prefix in exported GTFS for trips of this itinerary.",
    fr = "Identifiant unique de ce parcours type ou variante de la ligne. Sera utilis\u00e9 comme pr\u00e9fixe de trip_id dans le GTFS export\u00e9 pour les voyages de ce parcours type.",
    es = "Identificador \u00fanico de este itinerario o variante de la ruta. Se usar\u00e1 como prefijo de trip_id en el GTFS exportado para los viajes de este itinerario."
  ),
  pop_direction = c(
    en = "Indicates the direction of travel for a trip. Routes generally have at least one outbound (e.g. Northbound or Eastbound) variant and at least one inbound or return variant (e.g. Southbound or Westbound). Outbound corresponds to 0 and Inbound corresponds to 1 in exported GTFS.",
    fr = "Indique la direction de d\u00e9placement pour un voyage. Les lignes ont g\u00e9n\u00e9ralement au moins une variante aller (p.\u00a0ex. vers le nord ou l'est) et au moins une variante retour (p.\u00a0ex. vers le sud ou l'ouest). Aller correspond \u00e0 0 et Retour correspond \u00e0 1 dans le GTFS export\u00e9.",
    es = "Indica la direcci\u00f3n de viaje. Las rutas generalmente tienen al menos una variante de ida (p.\u00a0ej. hacia el norte o el este) y al menos una variante de vuelta (p.\u00a0ej. hacia el sur o el oeste). Ida corresponde a 0 y Vuelta corresponde a 1 en el GTFS exportado."
  ),
  pop_trip_headsign = c(
    en = "Text that appears on signage identifying the trip's destination to riders.",
    fr = "Texte affich\u00e9 sur la signalisation identifiant la destination du voyage pour les usagers.",
    es = "Texto que aparece en la se\u00f1alizaci\u00f3n identificando el destino del viaje para los usuarios."
  ),

  # -- Direction options --------------------------------------------------
  lbl_outbound = c(
    en = "Outbound",
    fr = "Aller",
    es = "Ida"
  ),
  lbl_inbound = c(
    en = "Inbound",
    fr = "Retour",
    es = "Vuelta"
  ),
  lbl_dir_out = c(en = "Out", fr = "Aller", es = "Ida"),
  lbl_dir_in = c(en = "In", fr = "Retour", es = "Vuelta"),

  # -- Itinerary placeholder ----------------------------------------------
  itin_ph_headsign = c(
    en = "e.g., Eastbound",
    fr = "p.\u00a0ex. Est",
    es = "p.\u00a0ej. Hacia el este"
  ),

  # -- Stop sequence table for selected itinerary -------------------------

  lbl_stop_name_col = c(
    en = "Stop name",
    fr = "Nom d'arr\u00eat",
    es = "Nombre de parada"
  ),
  dt_empty_table = c(
    en = "No data available in table",
    fr = "Aucune donn\u00e9e disponible dans le tableau",
    es = "No hay datos disponibles en la tabla"
  ),

  # -- JS confirm dialogs (mirrored in inst/www/js/i18n.js) ---------------
  confirm_delete_route = c(
    en = "Delete this route? Itineraries must be deleted first.",
    fr = "Supprimer cette ligne\u00a0? Les parcours types doivent d'abord \u00eatre supprim\u00e9s.",
    es = "\u00bfEliminar esta ruta? Los itinerarios deben eliminarse primero."
  ),
  confirm_delete_itin = c(
    en = "Delete this itinerary and its associated data?",
    fr = "Supprimer ce parcours type et ses donn\u00e9es associ\u00e9es\u00a0?",
    es = "\u00bfEliminar este itinerario y sus datos asociados?"
  ),

  # -- Notifications - Routes ---------------------------------------------
  notif_route_id_empty = c(
    en = "Route ID cannot be empty.",
    fr = "L'ID de la ligne ne peut pas \u00eatre vide.",
    es = "El ID de ruta no puede estar vac\u00edo."
  ),
  notif_route_id_exists = c(
    en = "This route ID already exists.",
    fr = "Cet ID de ligne existe d\u00e9j\u00e0.",
    es = "Este ID de ruta ya existe."
  ),
  notif_route_agency_first = c(
    en = "Please define at least one agency first.",
    fr = "Veuillez d'abord d\u00e9finir au moins une agence.",
    es = "Defina al menos una agencia primero."
  ),
  notif_route_added = c(
    en = "Route added successfully",
    fr = "Ligne ajout\u00e9e avec succ\u00e8s",
    es = "Ruta agregada con \u00e9xito"
  ),
  notif_route_updated = c(
    en = "Route updated successfully",
    fr = "Ligne mise \u00e0 jour avec succ\u00e8s",
    es = "Ruta actualizada con \u00e9xito"
  ),
  notif_route_not_found = c(
    en = "Route not found.",
    fr = "Ligne introuvable.",
    es = "Ruta no encontrada."
  ),
  notif_route_deleted = c(
    en = "Route deleted successfully",
    fr = "Ligne supprim\u00e9e avec succ\u00e8s",
    es = "Ruta eliminada con \u00e9xito"
  ),
  notif_route_cant_delete = c(
    en = "Cannot delete route '%s'. It is referenced by one or more itineraries. Delete the itineraries first.",
    fr = "Impossible de supprimer la ligne \u00ab\u00a0%s\u00a0\u00bb. Un ou plusieurs parcours types y font r\u00e9f\u00e9rence. Supprimez d'abord les parcours types.",
    es = "No se puede eliminar la ruta '%s'. Uno o m\u00e1s itinerarios la referencian. Elimine primero los itinerarios."
  ),
  notif_route_duplicated = c(
    en = "Duplicated route as: %s",
    fr = "Ligne dupliqu\u00e9e sous\u00a0: %s",
    es = "Ruta duplicada como: %s"
  ),

  # -- Notifications - Itineraries -----------------------------------------
  notif_itin_id_empty = c(
    en = "Itinerary ID cannot be empty.",
    fr = "L'ID du parcours type ne peut pas \u00eatre vide.",
    es = "El ID del itinerario no puede estar vac\u00edo."
  ),
  notif_itin_headsign_empty = c(
    en = "Trip headsign cannot be empty.",
    fr = "L'enseigne de destination ne peut pas \u00eatre vide.",
    es = "El letrero de destino no puede estar vac\u00edo."
  ),
  notif_itin_id_exists = c(
    en = "This itinerary ID already exists.",
    fr = "Cet ID de parcours type existe d\u00e9j\u00e0.",
    es = "Este ID de itinerario ya existe."
  ),
  notif_itin_min_points = c(
    en = "Itinerary must have at least 2 points.",
    fr = "Le parcours type doit avoir au moins 2 points.",
    es = "El itinerario debe tener al menos 2 puntos."
  ),
  notif_itin_draw_first = c(
    en = "Please draw the route on the map before saving.",
    fr = "Veuillez tracer le parcours sur la carte avant d'enregistrer.",
    es = "Dibuje la ruta en el mapa antes de guardar."
  ),
  notif_itin_saved = c(
    en = "Itinerary saved successfully",
    fr = "Parcours type enregistr\u00e9 avec succ\u00e8s",
    es = "Itinerario guardado con \u00e9xito"
  ),
  notif_itin_not_found = c(
    en = "Itinerary not found",
    fr = "Parcours type introuvable",
    es = "Itinerario no encontrado"
  ),
  notif_itin_deleted = c(
    en = "Deleted itinerary: %s",
    fr = "Parcours type supprim\u00e9\u00a0: %s",
    es = "Itinerario eliminado: %s"
  ),
  notif_itin_duplicated = c(
    en = "Duplicated as: %s",
    fr = "Dupliqu\u00e9 sous\u00a0: %s",
    es = "Duplicado como: %s"
  ),
  notif_node_removed = c(
    en = "Node removed",
    fr = "N\u0153ud supprim\u00e9",
    es = "Nodo eliminado"
  ),
  notif_last_node_removed = c(
    en = "Last node removed",
    fr = "Dernier n\u0153ud supprim\u00e9",
    es = "\u00daltimo nodo eliminado"
  ),

  # -- Notifications - Waypoints -------------------------------------------
  notif_wp_gone = c(
    en = "Selected waypoint no longer exists.",
    fr = "Le point de passage s\u00e9lectionn\u00e9 n'existe plus.",
    es = "El punto de paso seleccionado ya no existe."
  ),
  notif_wp_moved = c(
    en = "Waypoint moved",
    fr = "Point de passage d\u00e9plac\u00e9",
    es = "Punto de paso movido"
  ),
  notif_wp_deselected = c(
    en = "Waypoint deselected. Movement cancelled.",
    fr = "Point de passage d\u00e9s\u00e9lectionn\u00e9. D\u00e9placement annul\u00e9.",
    es = "Punto de paso deseleccionado. Movimiento cancelado."
  ),
  notif_wp_selected = c(
    en = "Waypoint selected. Drag it or click on the map to move it.",
    fr = "Point de passage s\u00e9lectionn\u00e9. D\u00e9placez-le ou cliquez sur la carte pour le d\u00e9placer.",
    es = "Punto de paso seleccionado. Arr\u00e1strelo o haga clic en el mapa para moverlo."
  ),
  notif_wp_to_stop = c(
    en = "Waypoint moved to stop & adopted stop properties.",
    fr = "Point de passage d\u00e9plac\u00e9 vers l'arr\u00eat et propri\u00e9t\u00e9s de l'arr\u00eat adopt\u00e9es.",
    es = "Punto de paso movido a la parada y propiedades de parada adoptadas."
  ),

  # -- Notifications - Stop interaction (routes context) -------------------
  notif_stop_in_seq = c(
    en = "Stop already in route stop sequence. Cannot add stop again.",
    fr = "L'arr\u00eat est d\u00e9j\u00e0 dans la s\u00e9quence d'arr\u00eats de la ligne. Impossible de l'ajouter \u00e0 nouveau.",
    es = "La parada ya est\u00e1 en la secuencia de paradas de la ruta. No se puede agregar de nuevo."
  ),
  notif_editing_itin = c(
    en = "Editing itinerary: %s",
    fr = "\u00c9dition du parcours type\u00a0: %s",
    es = "Editando itinerario: %s"
  ),

  # ========================================================================
  # Schedule module - panel chrome (static UI, translated via JS updateI18n)
  # ========================================================================
  sched_title = c(
    en = "schedule",
    fr = "horaires",
    es = "horarios"
  ),
  sched_filter_title = c(
    en = "Service & Hour",
    fr = "Service et heure",
    es = "Servicio y hora"
  ),
  sched_lbl_service = c(
    en = "Service",
    fr = "Service",
    es = "Servicio"
  ),
  sched_lbl_hour = c(
    en = "Hour",
    fr = "Heure",
    es = "Hora"
  ),
  sched_filter_desc = c(
    en = "Click on any route segment on the map to view cumulative service level for this service and hour.",
    fr = "Cliquez sur n'importe quel segment de ligne sur la carte pour afficher le niveau de service cumulatif pour ce service et cette heure.",
    es = "Haga clic en cualquier segmento de ruta en el mapa para ver el nivel de servicio acumulado para este servicio y esta hora."
  ),
  sched_btn_calendar = c(
    en = "Configure service calendar",
    fr = "Configurer le calendrier de service",
    es = "Configurar el calendario de servicio"
  ),
  sched_btn_presets = c(
    en = "Manage service level presets",
    fr = "G\u00e9rer les niveaux de service pr\u00e9d\u00e9finis",
    es = "Gestionar niveles de servicio predefinidos"
  ),

  # -- Schedule module - route-level editing panel -------------------

  sched_empty_editing = c(
    en = "Click on a route to edit its schedule.",
    fr = "Cliquez sur une ligne pour modifier ses horaires.",
    es = "Haga clic en una ruta para editar sus horarios."
  ),
  sched_no_routes = c(
    en = "No routes defined. Add routes in the Routes module.",
    fr = "Aucune ligne d\u00e9finie. Ajoutez des lignes dans le module Lignes.",
    es = "No hay rutas definidas. Agregue rutas en el m\u00f3dulo Rutas."
  ),
  sched_no_itins = c(
    en = "No itineraries for this route.",
    fr = "Aucun parcours type pour cette ligne.",
    es = "No hay itinerarios para esta ruta."
  ),
  sched_schedule_prefix = c(
    en = "Schedule: %s",
    fr = "Horaires\u00a0: %s",
    es = "Horarios: %s"
  ),
  pop_sched_service = c(
    en = "A service is a set of dates and days of the week during which different route schedules operate (e.g. weekday service vs. weekend), as configured in the Service Calendar (bottom left of this module).",
    fr = "Un service est un ensemble de dates et de jours de la semaine pendant lesquels diff\u00e9rents horaires de lignes s'appliquent (p. ex. service en semaine vs. fin de semaine), tel que configur\u00e9 dans le Calendrier de service (en bas \u00e0 gauche de ce module).",
    es = "Un servicio es un conjunto de fechas y d\u00edas de la semana durante los cuales operan diferentes horarios de rutas (p. ej. servicio entre semana vs. fin de semana), seg\u00fan la configuraci\u00f3n del Calendario de servicio (abajo a la izquierda de este m\u00f3dulo)."
  ),
  pop_sched_itineraries = c(
    en = "Each itinerary consists of a unique stop pattern or variant for trips for this route",
    fr = "Chaque parcours type consiste en un motif d'arr\u00eats unique ou une variante pour les voyages de cette ligne",
    es = "Cada itinerario consiste en un patr\u00f3n de paradas \u00fanico o una variante para los viajes de esta ruta"
  ),
  sched_batch_span_title = c(
    en = "Apply span to all route itineraries",
    fr = "Appliquer la plage \u00e0 tous les parcours types de la ligne",
    es = "Aplicar el per\u00edodo a todos los itinerarios de la ruta"
  ),
  lbl_first_dep = c(
    en = "First departure",
    fr = "Premier d\u00e9part",
    es = "Primera salida"
  ),
  lbl_last_dep = c(
    en = "Last departure",
    fr = "Dernier d\u00e9part",
    es = "\u00daltima salida"
  ),
  sched_batch_preset_title = c(
    en = "Apply service level preset to all route itineraries",
    fr = "Appliquer le niveau de service pr\u00e9d\u00e9fini \u00e0 tous les parcours types de la ligne",
    es = "Aplicar el nivel de servicio predefinido a todos los itinerarios de la ruta"
  ),
  pop_sched_batch_preset = c(
    en = "A service level preset defines a headway pattern by hour of day, reusable across itineraries. Applying one here will overwrite the hourly headways of all itineraries on this route for the selected service. The presets manager is at the bottom right of this module.",
    fr = "Un niveau de service pr\u00e9d\u00e9fini s'agit d'une combinaison d'intervalles par heure de la journ\u00e9e, r\u00e9utilisable entre les parcours types. Son application ici \u00e9crasera les intervalles horaires de tous les parcours types de cette ligne pour le service s\u00e9lectionn\u00e9. Le gestionnaire de niveaux de service pr\u00e9d\u00e9finis se trouve en bas \u00e0 droite de ce module.",
    es = "Un nivel de servicio predefinido es un patr\u00f3n de intervalos por hora del d\u00eda, reutilizable entre itinerarios. Aplicarlo aqu\u00ed sobrescribir\u00e1 los intervalos por hora de todos los itinerarios de esta ruta para el servicio seleccionado. El gestor de niveles de servicio predefinidos se encuentra abajo a la derecha de este m\u00f3dulo."
  ),
  sched_batch_hsh_title = c(
    en = "Apply headway and speed to all route itineraries",
    fr = "Appliquer l'intervalle et la vitesse \u00e0 tous les parcours types de la ligne",
    es = "Aplicar el intervalo y la velocidad a todos los itinerarios de la ruta"
  ),
  lbl_headway_min = c(
    en = "Headway (min)",
    fr = "Intervalle (min)",
    es = "Intervalo (min)"
  ),
  lbl_speed_kmh = c(
    en = "Speed (km/h)",
    fr = "Vitesse (km/h)",
    es = "Velocidad (km/h)"
  ),
  sched_cost_vh = c(
    en = "Daily vehicle-hours (in service): %s",
    fr = "Heures de service quotidiennes : %s",
    es = "Veh\u00edculos-hora diarios (en servicio): %s"
  ),
  sched_cost_vkm = c(
    en = "Daily vehicle-km (in service): %s",
    fr = "V\u00e9hicules-km quotidiens (en service)\u00a0: %s",
    es = "Veh\u00edculos-km diarios (en servicio): %s"
  ),
  sched_itin_stops_lbl = c(
    en = "stops",
    fr = "arr\u00eats",
    es = "paradas"
  ),
  sched_itin_trips_lbl = c(
    en = "trips",
    fr = "voyages",
    es = "viajes"
  ),
  sched_itin_sort_label = c(
    en = "Sort:",
    fr = "Trier\u00a0:",
    es = "Ordenar:"
  ),
  sched_itin_sort_default = c(
    en = "Default",
    fr = "Par d\u00e9faut",
    es = "Predeterminado"
  ),
  sched_itin_sort_start = c(
    en = "Start stop",
    fr = "Arr\u00eat de d\u00e9part",
    es = "Parada de inicio"
  ),
  sched_itin_sort_end = c(
    en = "End stop",
    fr = "Arr\u00eat d'arriv\u00e9e",
    es = "Parada final"
  ),
  # -- Schedule module - itinerary-level editing panel --------------------

  sched_empty_itin_editing = c(
    en = "Click on an itinerary to edit its headways and speeds.",
    fr = "Cliquez sur un parcours type pour modifier ses intervalles et vitesses.",
    es = "Haga clic en un itinerario para editar sus intervalos y velocidades."
  ),
  sched_itin_prefix = c(
    en = "Itinerary: %s - %s",
    fr = "Parcours type\u00a0: %s - %s",
    es = "Itinerario: %s - %s"
  ),
  sched_service_windows = c(
    en = "Service windows",
    fr = "Plages de service",
    es = "Ventanas de servicio"
  ),
  pop_sched_service_window = c(
    en = "A service window defines a time span during which a given itinerary operates for a specific service, defined by a first departure time and a last departure time.",
    fr = "Une plage de service d\u00e9finit une p\u00e9riode pendant laquelle un parcours type op\u00e8re pour un service donn\u00e9, d\u00e9finie par un premier d\u00e9part et un dernier d\u00e9part.",
    es = "Una ventana de servicio define un per\u00edodo durante el cual un itinerario opera para un servicio espec\u00edfico, definida por una primera salida y una \u00faltima salida."
  ),
  sched_sw_label = c(
    en = "Service window %s",
    fr = "Plage de service %s",
    es = "Ventana de servicio %s"
  ),
  sched_sw_short_label = c(
    en = "Window %s",
    fr = "Plage %s",
    es = "Ventana %s"
  ),
  sched_edit_sw_title = c(
    en = "Edit service window",
    fr = "Modifier la plage de service",
    es = "Editar ventana de servicio"
  ),
  sched_delete_sw_title = c(
    en = "Delete service window",
    fr = "Supprimer la plage de service",
    es = "Eliminar ventana de servicio"
  ),
  sched_add_sw = c(
    en = "Add new service window",
    fr = "Ajouter une nouvelle plage de service",
    es = "Agregar nueva ventana de servicio"
  ),
  sched_itin_apply_preset = c(
    en = "Apply service level preset",
    fr = "Appliquer le profil de niveau de service",
    es = "Aplicar el perfil de nivel de servicio"
  ),
  pop_sched_itin_preset = c(
    en = "A service level preset defines a headway pattern by hour of day, reusable across itineraries. Applying one here will overwrite the hourly headways of this itinerary for the selected service. The presets manager is at the bottom right of this module.",
    fr = "Un niveau de service pr\u00e9d\u00e9fini s'agit d'une combinaison d'intervalles par heure de la journ\u00e9e, r\u00e9utilisable entre les parcours types. Son application ici \u00e9crasera les intervalles horaires de ce parcours type pour le service s\u00e9lectionn\u00e9. Le gestionnaire de niveaux de service pr\u00e9d\u00e9finis se trouve en bas \u00e0 droite de ce module.",
    es = "Un nivel de servicio predefinido es un patr\u00f3n de intervalos por hora del d\u00eda, reutilizable entre itinerarios. Aplicarlo aqu\u00ed sobrescribir\u00e1 los intervalos por hora de este itinerario para el servicio seleccionado. El gestor de niveles de servicio predefinidos se encuentra abajo a la derecha de este m\u00f3dulo."
  ),
  sched_apply_hdwy_label = c(
    en = "Apply headway to all hours (min)",
    fr = "Appliquer l'intervalle \u00e0 toutes les heures (min)",
    es = "Aplicar el intervalo a todas las horas (min)"
  ),
  sched_apply_speed_label = c(
    en = "Apply speed to all hours (km/h)",
    fr = "Appliquer la vitesse \u00e0 toutes les heures (km/h)",
    es = "Aplicar la velocidad a todas las horas (km/h)"
  ),
  sched_recalc_title = c(
    en = "Speed recalculator",
    fr = "Recalculateur de vitesse",
    es = "Recalculador de velocidad"
  ),
  pop_sched_recalc = c(
    en = "Adjust speeds for a range of hours by modifying speed directly or by changing the runtime. Supports absolute values and percentages. Changes are applied to the selected itinerary and service only.",
    fr = "Ajustez les vitesses pour une plage d'heures en modifiant directement la vitesse ou en modifiant le temps de parcours. Prend en charge les valeurs absolues et les pourcentages. Les modifications s'appliquent uniquement au parcours type et au service s\u00e9lectionn\u00e9s.",
    es = "Ajuste las velocidades para un rango de horas modificando la velocidad directamente o cambiando la duraci\u00f3n del recorrido. Admite valores absolutos y porcentajes. Los cambios se aplican solo al itinerario y servicio seleccionados."
  ),
  sched_recalc_increase = c(
    en = "Increase",
    fr = "Augmenter",
    es = "Aumentar"
  ),
  sched_recalc_decrease = c(
    en = "Decrease",
    fr = "Diminuer",
    es = "Disminuir"
  ),
  sched_recalc_runtime = c(
    en = "runtime",
    fr = "temps de parcours",
    es = "duraci\u00f3n"
  ),
  sched_recalc_speed = c(
    en = "speed",
    fr = "vitesse",
    es = "velocidad"
  ),
  sched_recalc_by = c(
    en = "by",
    fr = "de",
    es = "en"
  ),
  sched_recalc_from = c(
    en = "from",
    fr = "de",
    es = "de"
  ),
  sched_recalc_to = c(
    en = "to",
    fr = "\u00e0",
    es = "a"
  ),
  sched_recalc_unit_minutes = c(
    en = "minutes",
    fr = "minutes",
    es = "minutos"
  ),
  sched_recalc_unit_kmh = c(
    en = "km/h",
    fr = "km/h",
    es = "km/h"
  ),
  notif_sched_recalc_invalid_value = c(
    en = "The recalculator value must be a positive number.",
    fr = "La valeur du recalculateur doit \u00eatre un nombre positif.",
    es = "El valor del recalculador debe ser un n\u00famero positivo."
  ),
  notif_sched_recalc_error = c(
    en = "Speed recalculation failed: %s",
    fr = "\u00c9chec du recalcul de vitesse\u00a0: %s",
    es = "Error en el rec\u00e1lculo de velocidad: %s"
  ),
  notif_sched_recalc_ok = c(
    en = "Applied: %s %s by %s %s from %s to %s",
    fr = "Appliqu\u00e9\u00a0: %s %s de %s %s de %s \u00e0 %s",
    es = "Aplicado: %s %s en %s %s de %s a %s"
  ),
  sched_hsh_title = c(
    en = "Headways & speeds by hour",
    fr = "Intervalles et vitesses par heure",
    es = "Intervalos y velocidades por hora"
  ),
  pop_sched_hsh = c(
    en = "A headway is the interval or duration between trips. Headways and speeds specified here are used to create trips and scheduled stop times based on distances between stops along the routes defined in the Routes module.",
    fr = "Un intervalle est la dur\u00e9e entre les voyages. Les intervalles et vitesses sp\u00e9cifi\u00e9s ici servent \u00e0 cr\u00e9er les voyages et les horaires d'arr\u00eats planifi\u00e9s bas\u00e9s sur les distances entre les arr\u00eats le long des lignes d\u00e9finies dans le module Lignes.",
    es = "Un intervalo es la duraci\u00f3n entre viajes. Los intervalos y velocidades especificados aqu\u00ed se usan para crear viajes y horarios de paradas programados basados en las distancias entre paradas a lo largo de las rutas definidas en el m\u00f3dulo Rutas."
  ),
  sched_hsh_runtime = c(
    en = "Runtime (mins)",
    fr = "Dur\u00e9e (min)",
    es = "Duraci\u00f3n (min)"
  ),
  sched_hsh_edit_title = c(
    en = "Edit row",
    fr = "Modifier la ligne",
    es = "Editar fila"
  ),
  sched_no_hsh = c(
    en = "No headway entries. Add a service window first.",
    fr = "Aucune entr\u00e9e d'intervalle. Ajoutez d'abord une plage de service.",
    es = "No hay entradas de intervalo. Agregue primero una ventana de servicio."
  ),
  sched_save_preset_label = c(
    en = "Save current headways as a new service level preset",
    fr = "Enregistrer les intervalles actuels en tant que nouveau profil de niveau de service",
    es = "Guardar los intervalos actuales como un nuevo perfil de nivel de servicio"
  ),
  pop_sched_save_preset = c(
    en = "Saves the hourly headways currently defined for this itinerary and service as a reusable preset that can be applied to other itineraries.",
    fr = "Enregistre les intervalles horaires actuellement d\u00e9finis pour ce parcours type et ce service en tant que profil r\u00e9utilisable applicable \u00e0 d'autres parcours types.",
    es = "Guarda los intervalos por hora actualmente definidos para este itinerario y servicio como un perfil reutilizable que se puede aplicar a otros itinerarios."
  ),
  sched_save_as_preset = c(
    en = "Save as preset",
    fr = "Enregistrer comme profil",
    es = "Guardar como perfil"
  ),
  # -- Schedule module - Calendar and Presets modals ----------------------

  # Day abbreviations (used in calendar table headers and checkbox labels)
  day_mon = c(en = "Mon", fr = "Lun", es = "Lun"),
  day_tue = c(en = "Tue", fr = "Mar", es = "Mar"),
  day_wed = c(en = "Wed", fr = "Mer", es = "Mi\u00e9"),
  day_thu = c(en = "Thu", fr = "Jeu", es = "Jue"),
  day_fri = c(en = "Fri", fr = "Ven", es = "Vie"),
  day_sat = c(en = "Sat", fr = "Sam", es = "S\u00e1b"),
  day_sun = c(en = "Sun", fr = "Dim", es = "Dom"),

  # Calendar modal
  sched_cal_title = c(
    en = "Service Calendar",
    fr = "Calendrier de service",
    es = "Calendario de servicio"
  ),
  btn_close = c(
    en = "Close",
    fr = "Fermer",
    es = "Cerrar"
  ),
  lbl_service_id = c(
    en = "Service ID",
    fr = "ID de service",
    es = "ID de servicio"
  ),
  lbl_days_of_operation = c(
    en = "Days of operation",
    fr = "Jours d'exploitation",
    es = "D\u00edas de operaci\u00f3n"
  ),
  lbl_start_date = c(
    en = "Start date",
    fr = "Date de d\u00e9but",
    es = "Fecha de inicio"
  ),
  lbl_end_date = c(
    en = "End date",
    fr = "Date de fin",
    es = "Fecha de fin"
  ),
  sched_cal_edit_title = c(
    en = "Edit service",
    fr = "Modifier le service",
    es = "Editar servicio"
  ),
  sched_cal_delete_title = c(
    en = "Delete service",
    fr = "Supprimer le service",
    es = "Eliminar servicio"
  ),
  sched_cal_add = c(
    en = "Add new service",
    fr = "Ajouter un nouveau service",
    es = "Agregar nuevo servicio"
  ),
  sched_cal_cost_title = c(
    en = "Total daily service cost",
    fr = "Co\u00fbt de service quotidien total",
    es = "Costo de servicio diario total"
  ),
  sched_cal_cost_desc = c(
    en = "Calculate total daily vehicle-km and vehicle-hours for all routes and itineraries on a selected service. This may take several minutes for larger networks.",
    fr = "Calculez les v\u00e9hicules-km et v\u00e9hicules-heures quotidiens totaux pour toutes les lignes et parcours types d'un service s\u00e9lectionn\u00e9. Le calcul peut prendre plusieurs minutes pour les r\u00e9seaux plus importants.",
    es = "Calcule los veh\u00edculos-km y veh\u00edculos-hora diarios totales para todas las rutas e itinerarios de un servicio seleccionado. Esto puede tardar varios minutos para redes m\u00e1s grandes."
  ),
  btn_calculate = c(
    en = "Calculate",
    fr = "Calculer",
    es = "Calcular"
  ),
  sched_cal_cost_empty = c(
    en = "Add a service above to calculate costs.",
    fr = "Ajoutez un service ci-dessus pour calculer les co\u00fbts.",
    es = "Agregue un servicio arriba para calcular los costos."
  ),
  lbl_vehicle_km = c(
    en = "Vehicle-km",
    fr = "V\u00e9hicules-km",
    es = "Veh\u00edculos-km"
  ),
  lbl_vehicle_hours = c(
    en = "Vehicle-hours",
    fr = "V\u00e9hicules-heures",
    es = "Veh\u00edculos-hora"
  ),
  lbl_total = c(
    en = "Total",
    fr = "Total",
    es = "Total"
  ),

  # Presets modal
  sched_presets_title = c(
    en = "Service Level Presets",
    fr = "Niveaux de service pr\u00e9d\u00e9finis",
    es = "Niveles de servicio predefinidos"
  ),
  sched_preset_edit_title = c(
    en = "Edit preset",
    fr = "Modifier le profil",
    es = "Editar perfil"
  ),
  sched_preset_delete_title = c(
    en = "Delete preset",
    fr = "Supprimer le profil",
    es = "Eliminar perfil"
  ),
  sched_preset_add = c(
    en = "Add new service level preset",
    fr = "Ajouter un nouveau profil de niveau de service",
    es = "Agregar nuevo perfil de nivel de servicio"
  ),
  lbl_preset_name = c(
    en = "Preset name",
    fr = "Nom du profil",
    es = "Nombre del perfil"
  ),
  btn_save_preset = c(
    en = "Save preset",
    fr = "Enregistrer le profil",
    es = "Guardar perfil"
  ),
  btn_rename = c(
    en = "Rename",
    fr = "Renommer",
    es = "Renombrar"
  ),
  lbl_hours = c(
    en = "Hours",
    fr = "Heures",
    es = "Horas"
  ),
  sched_preset_hint = c(
    en = "Add hours to build the preset.",
    fr = "Ajoutez des heures pour construire le profil.",
    es = "Agregue horas para construir el perfil."
  ),
  sched_preset_add_hour = c(
    en = "Add new hour",
    fr = "Ajouter une nouvelle heure",
    es = "Agregar nueva hora"
  ),
  sched_preset_ph_name = c(
    en = "e.g. Peak Frequent",
    fr = "p. ex. Fr\u00e9quente en pointe",
    es = "p. ej. Frecuente en hora punta"
  ),

  # -- Schedule module - speed profile + map popup ------------------------

  sched_sp_prefix = c(
    en = "Speed profile: %s",
    fr = "Profil de vitesse\u00a0: %s",
    es = "Perfil de velocidad: %s"
  ),
  sched_sp_info = c(
    en = "Speed factors are defined once per itinerary and apply to all services and hours. Changing hour only changes the displayed speeds (km/h)",
    fr = "Les facteurs de vitesse sont d\u00e9finis une seule fois par parcours type et s'appliquent \u00e0 tous les services et heures. Changer l'heure ne modifie que les vitesses affich\u00e9es (km/h)",
    es = "Los factores de velocidad se definen una sola vez por itinerario y se aplican a todos los servicios y horas. Cambiar la hora solo cambia las velocidades mostradas (km/h)"
  ),
  sched_sp_toggle = c(
    en = "Adjust speed factors",
    fr = "Ajuster les facteurs de vitesse",
    es = "Ajustar los factores de velocidad"
  ),
  sched_sp_from_stop = c(
    en = "From stop",
    fr = "Depuis l'arr\u00eat",
    es = "Desde la parada"
  ),
  sched_sp_sequence = c(
    en = "Sequence",
    fr = "S\u00e9quence",
    es = "Secuencia"
  ),
  sched_sp_factor = c(
    en = "Speed factor",
    fr = "Facteur de vitesse",
    es = "Factor de velocidad"
  ),
  sched_sp_adjust = c(
    en = "Adjust",
    fr = "Ajuster",
    es = "Ajustar"
  ),
  sched_sp_reset = c(
    en = "Reset all to 1.0",
    fr = "R\u00e9initialiser tout \u00e0 1,0",
    es = "Restablecer todo a 1,0"
  ),
  sched_sp_axis_seq = c(
    en = "Stop sequence",
    fr = "S\u00e9quence d'arr\u00eats",
    es = "Secuencia de paradas"
  ),
  sched_sp_hover = c(
    en = "Stop: %s (seq %s)\nSpeed: %s km/h\nFactor: %s",
    fr = "Arr\u00eat\u00a0: %s (s\u00e9q. %s)\nVitesse\u00a0: %s km/h\nFacteur\u00a0: %s",
    es = "Parada: %s (sec. %s)\nVelocidad: %s km/h\nFactor: %s"
  ),

  # Map popup
  sched_popup_route = c(
    en = "Route",
    fr = "Ligne",
    es = "Ruta"
  ),
  sched_popup_itin = c(
    en = "Itinerary",
    fr = "Parcours type",
    es = "Itinerario"
  ),
  sched_popup_headway = c(
    en = "Headway",
    fr = "Intervalle",
    es = "Intervalo"
  ),
  sched_popup_trips = c(
    en = "Trips/h",
    fr = "Voyages/h",
    es = "Viajes/h"
  ),

  # Stop hover label
  sched_hover_itins = c(
    en = "Itineraries: ",
    fr = "Parcours types\u00a0: ",
    es = "Itinerarios: "
  ),

  # ========================================================================
  # Schedule module - notifications
  # ========================================================================

  # --- Span CRUD -------------
  notif_sched_invalid_time = c(
    en = "Invalid time format. Use HH:MM:SS (00-30:00-59:00-59).",
    fr = "Format d'heure invalide. Utilisez HH:MM:SS (00-30:00-59:00-59).",
    es = "Formato de hora inv\u00e1lido. Use HH:MM:SS (00-30:00-59:00-59)."
  ),
  notif_sched_first_before_last = c(
    en = "First departure must be before last departure.",
    fr = "Le premier d\u00e9part doit \u00eatre avant le dernier d\u00e9part.",
    es = "La primera salida debe ser antes de la \u00faltima salida."
  ),
  notif_sched_sw_updated = c(
    en = "Service window updated.",
    fr = "Plage de service mise \u00e0 jour.",
    es = "Ventana de servicio actualizada."
  ),
  notif_sched_sw_added = c(
    en = "Service window %s added (%s - %s) with %s headway entries created.",
    fr = "Plage de service %s ajout\u00e9e (%s - %s) avec %s entr\u00e9es d'intervalle cr\u00e9\u00e9es.",
    es = "Ventana de servicio %s agregada (%s - %s) con %s entradas de intervalo creadas."
  ),
  notif_sched_sw_deleted = c(
    en = "Service window %s deleted.",
    fr = "Plage de service %s supprim\u00e9e.",
    es = "Ventana de servicio %s eliminada."
  ),
  notif_sched_sw_not_found = c(
    en = "Span not found.",
    fr = "Plage introuvable.",
    es = "Per\u00edodo no encontrado."
  ),
  notif_sched_sw_overlap = c(
    en = "Service window %s must start after %s (the end of service window %s).",
    fr = "La plage de service %s doit commencer apr\u00e8s %s (la fin de la plage de service %s).",
    es = "La ventana de servicio %s debe comenzar despu\u00e9s de %s (el fin de la ventana de servicio %s)."
  ),
  notif_sched_sw_start_after = c(
    en = "Must start after %s (end of previous window).",
    fr = "Doit commencer apr\u00e8s %s (fin de la plage pr\u00e9c\u00e9dente).",
    es = "Debe comenzar despu\u00e9s de %s (fin de la ventana anterior)."
  ),
  notif_sched_sw_end_before = c(
    en = "Must end before %s (start of next window).",
    fr = "Doit se terminer avant %s (d\u00e9but de la plage suivante).",
    es = "Debe terminar antes de %s (inicio de la ventana siguiente)."
  ),

  # -- Batch actions (route-level) ----------------------------------------
  notif_sched_batch_span = c(
    en = "Span %s - %s applied to %s itinerary(ies) for service %s",
    fr = "Plage %s - %s appliqu\u00e9e \u00e0 %s parcours type(s) pour le service %s",
    es = "Per\u00edodo %s - %s aplicado a %s itinerario(s) para el servicio %s"
  ),
  notif_sched_batch_preset = c(
    en = "Applied '%s' to %s itinerary(ies). %s hour entries updated.",
    fr = "\u00ab\u00a0%s\u00a0\u00bb appliqu\u00e9 \u00e0 %s parcours type(s). %s entr\u00e9es horaires mises \u00e0 jour.",
    es = "'%s' aplicado a %s itinerario(s). %s entradas por hora actualizadas."
  ),
  notif_sched_no_hsh = c(
    en = "No headway entries found. Define spans first.",
    fr = "Aucune entr\u00e9e d'intervalle trouv\u00e9e. D\u00e9finissez d'abord des plages de service.",
    es = "No se encontraron entradas de intervalo. Defina primero ventanas de servicio."
  ),
  notif_sched_batch_hdwy = c(
    en = "Headway set to %s min for %s entries across %s itinerary(ies).",
    fr = "Intervalle r\u00e9gl\u00e9 \u00e0 %s min pour %s entr\u00e9es sur %s parcours type(s).",
    es = "Intervalo establecido a %s min para %s entradas en %s itinerario(s)."
  ),
  notif_sched_batch_speed = c(
    en = "Speed set to %s km/h for %s entries across %s itinerary(ies).",
    fr = "Vitesse r\u00e9gl\u00e9e \u00e0 %s km/h pour %s entr\u00e9es sur %s parcours type(s).",
    es = "Velocidad establecida a %s km/h para %s entradas en %s itinerario(s)."
  ),
  notif_sched_preset_not_found = c(
    en = "Selected preset not found.",
    fr = "Profil s\u00e9lectionn\u00e9 introuvable.",
    es = "Perfil seleccionado no encontrado."
  ),

  # -- Itinerary-level actions --------------------------------------------
  notif_sched_itin_preset = c(
    en = "Applied '%s' to %s. %s hour entries updated.",
    fr = "\u00ab\u00a0%s\u00a0\u00bb appliqu\u00e9 \u00e0 %s. %s entr\u00e9es horaires mises \u00e0 jour.",
    es = "'%s' aplicado a %s. %s entradas por hora actualizadas."
  ),
  notif_sched_itin_hdwy = c(
    en = "Headway set to %s min for %s entries on %s.",
    fr = "Intervalle r\u00e9gl\u00e9 \u00e0 %s min pour %s entr\u00e9es sur %s.",
    es = "Intervalo establecido a %s min para %s entradas en %s."
  ),
  notif_sched_itin_speed = c(
    en = "Speed set to %s km/h for %s entries on %s.",
    fr = "Vitesse r\u00e9gl\u00e9e \u00e0 %s km/h pour %s entr\u00e9es sur %s.",
    es = "Velocidad establecida a %s km/h para %s entradas en %s."
  ),

  # -- HSH editing --------------------------------------------------------
  notif_sched_hdwy_range = c(
    en = "Headway must be between 1 and 119 minutes.",
    fr = "L'intervalle doit \u00eatre entre 1 et 119 minutes.",
    es = "El intervalo debe estar entre 1 y 119 minutos."
  ),
  notif_sched_hdwy_invalid = c(
    en = "Invalid headway value.",
    fr = "Valeur d'intervalle invalide.",
    es = "Valor de intervalo inv\u00e1lido."
  ),
  notif_sched_speed_range = c(
    en = "Speed must be between 5 and 431 km/h.",
    fr = "La vitesse doit \u00eatre entre 5 et 431 km/h.",
    es = "La velocidad debe estar entre 5 y 431 km/h."
  ),
  notif_sched_row_not_found = c(
    en = "Row not found.",
    fr = "Ligne introuvable.",
    es = "Fila no encontrada."
  ),
  notif_sched_hsh_updated = c(
    en = "Updated %s: headway = %s, speed = %s km/h",
    fr = "Mise \u00e0 jour %s\u00a0: intervalle = %s, vitesse = %s km/h",
    es = "Actualizado %s: intervalo = %s, velocidad = %s km/h"
  ),

  # -- Save as preset ----------------------------------------------------
  notif_sched_preset_name_empty = c(
    en = "Preset name cannot be empty.",
    fr = "Le nom du profil ne peut pas \u00eatre vide.",
    es = "El nombre del perfil no puede estar vac\u00edo."
  ),
  notif_sched_no_hdwy_for_preset = c(
    en = "No headway values defined. Set headways before saving as preset.",
    fr = "Aucune valeur d'intervalle d\u00e9finie. D\u00e9finissez des intervalles avant d'enregistrer comme profil.",
    es = "No hay valores de intervalo definidos. Establezca intervalos antes de guardar como perfil."
  ),
  notif_sched_saved_preset = c(
    en = "Saved as '%s - %s' with %s hours.",
    fr = "Enregistr\u00e9 sous \u00ab\u00a0%s - %s\u00a0\u00bb avec %s heures.",
    es = "Guardado como '%s - %s' con %s horas."
  ),

  # -- Calendar CRUD ------------------------------------------------------
  notif_sched_cal_id_empty = c(
    en = "Service ID cannot be empty.",
    fr = "L'ID de service ne peut pas \u00eatre vide.",
    es = "El ID de servicio no puede estar vac\u00edo."
  ),
  notif_sched_cal_dates_required = c(
    en = "Start and end dates are required.",
    fr = "Les dates de d\u00e9but et de fin sont obligatoires.",
    es = "Las fechas de inicio y fin son obligatorias."
  ),
  notif_sched_cal_date_order = c(
    en = "Start date must be before end date.",
    fr = "La date de d\u00e9but doit \u00eatre avant la date de fin.",
    es = "La fecha de inicio debe ser antes de la fecha de fin."
  ),
  notif_sched_cal_id_exists = c(
    en = "Service ID already exists. Please use a different ID.",
    fr = "Cet ID de service existe d\u00e9j\u00e0. Veuillez utiliser un autre ID.",
    es = "Este ID de servicio ya existe. Use un ID diferente."
  ),
  notif_sched_cal_created = c(
    en = "Service '%s' created.",
    fr = "Service \u00ab\u00a0%s\u00a0\u00bb cr\u00e9\u00e9.",
    es = "Servicio '%s' creado."
  ),
  notif_sched_cal_not_found = c(
    en = "Service not found.",
    fr = "Service introuvable.",
    es = "Servicio no encontrado."
  ),
  notif_sched_cal_updated = c(
    en = "Service '%s' updated.",
    fr = "Service \u00ab\u00a0%s\u00a0\u00bb mis \u00e0 jour.",
    es = "Servicio '%s' actualizado."
  ),
  notif_sched_cal_deleted = c(
    en = "Service '%s' deleted with associated spans and headway entries.",
    fr = "Service \u00ab\u00a0%s\u00a0\u00bb supprim\u00e9 avec les plages et entr\u00e9es d'intervalle associ\u00e9es.",
    es = "Servicio '%s' eliminado con las ventanas y entradas de intervalo asociadas."
  ),
  notif_sched_no_routes_defined = c(
    en = "No routes defined.",
    fr = "Aucune ligne d\u00e9finie.",
    es = "No hay rutas definidas."
  ),
  notif_sched_no_sw_for_service = c(
    en = "No service windows defined for '%s'.",
    fr = "Aucune plage de service d\u00e9finie pour \u00ab\u00a0%s\u00a0\u00bb.",
    es = "No hay ventanas de servicio definidas para '%s'."
  ),
  notif_sched_calculating = c(
    en = "Calculating service cost...",
    fr = "Calcul du co\u00fbt de service en cours\u2026",
    es = "Calculando costo de servicio\u2026"
  ),
  notif_sched_cost_done = c(
    en = "Service cost calculated.",
    fr = "Co\u00fbt de service calcul\u00e9.",
    es = "Costo de servicio calculado."
  ),
  notif_sched_error = c(
    en = "Error: %s",
    fr = "Erreur\u00a0: %s",
    es = "Error: %s"
  ),

  # -- Preset CRUD --------------------------------------------------------
  notif_sched_preset_created = c(
    en = "Preset '%s - %s' created.",
    fr = "Profil \u00ab\u00a0%s - %s\u00a0\u00bb cr\u00e9\u00e9.",
    es = "Perfil '%s - %s' creado."
  ),
  notif_sched_preset_renamed = c(
    en = "Preset renamed to '%s'.",
    fr = "Profil renomm\u00e9 \u00ab\u00a0%s\u00a0\u00bb.",
    es = "Perfil renombrado a '%s'."
  ),
  notif_sched_preset_deleted = c(
    en = "Preset '%s' deleted.",
    fr = "Profil \u00ab\u00a0%s\u00a0\u00bb supprim\u00e9.",
    es = "Perfil '%s' eliminado."
  ),
  notif_sched_save_name_first = c(
    en = "Please save the preset name first.",
    fr = "Veuillez d'abord enregistrer le nom du profil.",
    es = "Guarde primero el nombre del perfil."
  ),
  notif_sched_select_hour = c(
    en = "Please select an hour.",
    fr = "Veuillez s\u00e9lectionner une heure.",
    es = "Seleccione una hora."
  ),
  notif_sched_hdwy_range_blank = c(
    en = "Headway must be between 1 and 119, or left blank.",
    fr = "L'intervalle doit \u00eatre entre 1 et 119, ou laiss\u00e9 vide.",
    es = "El intervalo debe estar entre 1 y 119, o dejarse en blanco."
  ),
  notif_sched_hour_exists = c(
    en = "This hour already exists in the preset.",
    fr = "Cette heure existe d\u00e9j\u00e0 dans le profil.",
    es = "Esta hora ya existe en el perfil."
  ),
  notif_sched_hour_added = c(
    en = "Hour %s added.",
    fr = "Heure %s ajout\u00e9e.",
    es = "Hora %s agregada."
  ),
  notif_sched_hour_updated = c(
    en = "Hour updated.",
    fr = "Heure mise \u00e0 jour.",
    es = "Hora actualizada."
  ),
  notif_sched_hour_removed = c(
    en = "Hour %s removed.",
    fr = "Heure %s supprim\u00e9e.",
    es = "Hora %s eliminada."
  ),

  # -- JS confirm dialogs (mirrored in i18n.js) --------------------------
  confirm_delete_sw = c(
    en = "Delete this service window and associated headway by hour entries?",
    fr = "Supprimer cette plage de service et les entr\u00e9es d'intervalle par heure associ\u00e9es\u00a0?",
    es = "\u00bfEliminar esta ventana de servicio y las entradas de intervalo por hora asociadas?"
  ),
  confirm_delete_cal_service = c(
    en = "Delete service \"{id}\"? This will remove all schedule data associated with this route.",
    fr = "Supprimer le service \u00ab\u00a0{id}\u00a0\u00bb\u00a0? Cela supprimera toutes les donn\u00e9es d'horaire associ\u00e9es \u00e0 cette ligne.",
    es = "\u00bfEliminar el servicio \"{id}\"? Esto eliminar\u00e1 todos los datos de horario asociados a esta ruta."
  ),
  confirm_delete_sched_preset = c(
    en = "Delete preset \"{id}\"?",
    fr = "Supprimer le profil \u00ab\u00a0{id}\u00a0\u00bb\u00a0?",
    es = "\u00bfEliminar el perfil \"{id}\"?"
  ),
  # ========================================================================
  # Save / Export tab
  # ========================================================================
  export_title = c(
    en = "export or save your project",
    fr = "exporter ou sauvegarder votre projet",
    es = "exportar o guardar su proyecto"
  ),
  export_gtfs_title = c(
    en = "Export GTFS",
    fr = "Exporter le GTFS",
    es = "Exportar GTFS"
  ),
  lbl_filename = c(
    en = "Filename:",
    fr = "Nom du fichier\u00a0:",
    es = "Nombre del archivo:"
  ),
  export_dist_traveled = c(
    en = "Include shape_dist_traveled",
    fr = "Inclure shape_dist_traveled",
    es = "Incluir shape_dist_traveled"
  ),
  export_dist_desc = c(
    en = "When checked, adds shape_dist_traveled to shapes and stop_times tables. This increases export time.",
    fr = "Lorsque coch\u00e9, ajoute shape_dist_traveled aux tables shapes et stop_times. Cela augmente le temps d'exportation.",
    es = "Cuando est\u00e1 marcado, agrega shape_dist_traveled a las tablas shapes y stop_times. Esto aumenta el tiempo de exportaci\u00f3n."
  ),
  export_download_gtfs = c(
    en = "Download GTFS",
    fr = "T\u00e9l\u00e9charger le GTFS",
    es = "Descargar GTFS"
  ),
  export_save_title = c(
    en = "Save your project to work on it later",
    fr = "Sauvegardez votre projet pour y travailler plus tard",
    es = "Guarde su proyecto para trabajar en \u00e9l m\u00e1s tarde"
  ),
  export_save_desc = c(
    en = "This saves the raw Croquis (SSFS) file as a .rds:",
    fr = "Ceci sauvegarde le fichier brut Croquis (SSFS) au format .rds\u00a0:",
    es = "Esto guarda el archivo bruto Croquis (SSFS) como .rds:"
  ),
  export_download_croquis = c(
    en = "Download Croquis file",
    fr = "T\u00e9l\u00e9charger le fichier Croquis",
    es = "Descargar archivo Croquis"
  ),
  export_save_note = c(
    en = "Your transit system will be saved as an .rds file that you can reload later.",
    fr = "Votre r\u00e9seau de transport sera sauvegard\u00e9 sous forme de fichier .rds que vous pourrez recharger plus tard.",
    es = "Su red de transporte se guardar\u00e1 como un archivo .rds que puede recargar m\u00e1s tarde."
  ),

  # ========================================================================
  # Settings tab
  # ========================================================================
  settings_title = c(
    en = "settings",
    fr = "param\u00e8tres",
    es = "configuraci\u00f3n"
  ),

  # -- Feed info ----------------------------------------------------------
  settings_feed_info = c(
    en = "Feed info",
    fr = "Informations du flux",
    es = "Informaci\u00f3n del feed"
  ),
  lbl_publisher_name = c(
    en = "Publisher name",
    fr = "Nom de l'\u00e9diteur",
    es = "Nombre del editor"
  ),
  pop_publisher_name = c(
    en = "Full name of the organization that publishes the feed.",
    fr = "Nom complet de l'organisme qui publie le flux.",
    es = "Nombre completo de la organizaci\u00f3n que publica el feed."
  ),
  lbl_publisher_url = c(
    en = "Publisher URL",
    fr = "URL de l'\u00e9diteur",
    es = "URL del editor"
  ),
  pop_publisher_url = c(
    en = "URL of the feed publishing organization's website.",
    fr = "URL du site web de l'organisme publiant le flux.",
    es = "URL del sitio web de la organizaci\u00f3n que publica el feed."
  ),
  lbl_feed_lang = c(
    en = "Feed language",
    fr = "Langue du flux",
    es = "Idioma del feed"
  ),
  pop_feed_lang = c(
    en = "Default language used for text in this dataset (IETF BCP 47 language code).",
    fr = "Langue par d\u00e9faut utilis\u00e9e pour le texte dans ce jeu de donn\u00e9es (code IETF BCP 47).",
    es = "Idioma predeterminado usado para el texto en este conjunto de datos (c\u00f3digo de idioma IETF BCP 47)."
  ),
  lbl_contact_email = c(
    en = "Contact email",
    fr = "Courriel de contact",
    es = "Correo de contacto"
  ),
  pop_contact_email = c(
    en = "Email address for communication regarding the GTFS dataset and data publishing practices.",
    fr = "Adresse courriel pour les communications concernant le jeu de donn\u00e9es GTFS et les pratiques de publication.",
    es = "Direcci\u00f3n de correo para comunicaciones sobre el conjunto de datos GTFS y las pr\u00e1cticas de publicaci\u00f3n."
  ),
  lbl_feed_version = c(
    en = "Version",
    fr = "Version",
    es = "Versi\u00f3n"
  ),
  pop_feed_version = c(
    en = "String that indicates the current version of their GTFS dataset.",
    fr = "Cha\u00eene indiquant la version actuelle du jeu de donn\u00e9es GTFS.",
    es = "Cadena que indica la versi\u00f3n actual del conjunto de datos GTFS."
  ),

  # -- Advanced settings --------------------------------------------------
  settings_advanced = c(
    en = "Advanced settings",
    fr = "Param\u00e8tres avanc\u00e9s",
    es = "Configuraci\u00f3n avanzada"
  ),
  lbl_routing_server = c(
    en = "Default routing server",
    fr = "Serveur de routage par d\u00e9faut",
    es = "Servidor de rutas por defecto"
  ),
  pop_routing_server = c(
    en = "Routing server used to draw segments along the road network between stops and waypoints in the routes module.",
    fr = "Serveur de routage utilis\u00e9 pour tracer les segments le long du r\u00e9seau routier entre les arr\u00eats et les points de passage dans le module Lignes.",
    es = "Servidor de rutas utilizado para trazar segmentos a lo largo de la red vial entre paradas y puntos de paso en el m\u00f3dulo Rutas."
  ),
  lbl_gtfs_workers = c(
    en = "GTFS import workers",
    fr = "Processus d'importation GTFS",
    es = "Procesos de importaci\u00f3n GTFS"
  ),
  pop_gtfs_workers = c(
    en = "Number of worker processes to use during GTFS to SSFS conversion. Values above 1 speed up imports on Linux servers; Windows falls back to a single worker.",
    fr = "Nombre de processus \u00e0 utiliser lors de la conversion GTFS vers SSFS. Les valeurs sup\u00e9rieures \u00e0 1 acc\u00e9l\u00e8rent l'importation sur les serveurs Linux\u00a0; Windows utilise un seul processus.",
    es = "N\u00famero de procesos a usar durante la conversi\u00f3n de GTFS a SSFS. Valores mayores a 1 aceleran la importaci\u00f3n en servidores Linux; Windows usa un solo proceso."
  ),
  lbl_gtfs_max_date = c(
    en = "Specify GTFS import reference date",
    fr = "Sp\u00e9cifier une date de r\u00e9f\u00e9rence pour l'importation GTFS",
    es = "Especificar fecha de referencia para la importaci\u00f3n GTFS"
  ),
  pop_gtfs_max_date = c(
    en = "When checked, the GTFS import will only consider service within the 7 days preceding the specified date. When unchecked, the last 7 days of service defined in the feed are used.",
    fr = "Lorsque coch\u00e9, l'importation GTFS ne consid\u00e9rera que le service dans les 7 jours pr\u00e9c\u00e9dant la date sp\u00e9cifi\u00e9e. Sinon, les 7 derniers jours de service d\u00e9finis dans le flux sont utilis\u00e9s.",
    es = "Cuando est\u00e1 marcado, la importaci\u00f3n GTFS solo considerar\u00e1 el servicio dentro de los 7 d\u00edas anteriores a la fecha especificada. Si no, se usan los \u00faltimos 7 d\u00edas de servicio definidos en el feed."
  ),
  lbl_min_stop_dist = c(
    en = "Minimum stop spacing (m)",
    fr = "Espacement minimal des arr\u00eats (m)",
    es = "Espaciamiento m\u00ednimo de paradas (m)"
  ),
  pop_min_stop_dist = c(
    en = "Minimum distance in metres between auto-generated stops. Also used as the buffer distance around existing stops when determining eligible locations for new stops.",
    fr = "Distance minimale en m\u00e8tres entre les arr\u00eats g\u00e9n\u00e9r\u00e9s automatiquement. \u00c9galement utilis\u00e9e comme distance tampon autour des arr\u00eats existants pour d\u00e9terminer les emplacements admissibles.",
    es = "Distancia m\u00ednima en metros entre paradas generadas autom\u00e1ticamente. Tambi\u00e9n se usa como distancia de amortiguamiento alrededor de las paradas existentes para determinar ubicaciones elegibles."
  ),
  lbl_osm_provider = c(
    en = "OSM data provider",
    fr = "Fournisseur de donn\u00e9es OSM",
    es = "Proveedor de datos OSM"
  ),
  pop_osm_provider = c(
    en = "OpenStreetMap data provider used when generating stops from road network data. Different providers have different regional coverage.",
    fr = "Fournisseur de donn\u00e9es OpenStreetMap utilis\u00e9 lors de la g\u00e9n\u00e9ration d'arr\u00eats \u00e0 partir des donn\u00e9es du r\u00e9seau routier. Les fournisseurs offrent des couvertures r\u00e9gionales diff\u00e9rentes.",
    es = "Proveedor de datos OpenStreetMap usado al generar paradas a partir de datos de la red vial. Diferentes proveedores tienen diferente cobertura regional."
  ),
  lbl_carto_key = c(
    en = "CARTO basemap API key",
    fr = "Cl\u00e9 API CARTO (fonds de carte)",
    es = "Clave API de CARTO (mapas base)"
  ),
  pop_carto_key = c(
    en = "Optional API key for CARTO Positron basemap tiles. Free keys (5 million tiles/month) can be requested at carto.com/basemaps/apikey. When empty, a free alternative basemap is used.",
    fr = "Cl\u00e9 API optionnelle pour les tuiles du fond de carte CARTO Positron. Des cl\u00e9s gratuites (5 millions de tuiles/mois) peuvent \u00eatre demand\u00e9es sur carto.com/basemaps/apikey. Si vide, un fond de carte alternatif gratuit est utilis\u00e9.",
    es = "Clave API opcional para las teselas del mapa base CARTO Positron. Se pueden solicitar claves gratuitas (5 millones de teselas/mes) en carto.com/basemaps/apikey. Si se deja vac\u00edo, se usa un mapa base alternativo gratuito."
  )
)


# -- Helper: build language selector options -------------------------
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
#' Looks up `key` in `i18n_dict` and returns the value for `lang`,
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
