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
    fr: 'Cet arr\u00eat sera supprim\u00e9 s\u2019il n\u2019est associ\u00e9 \u00e0 aucun parcours type.',
    es: 'Esta parada ser\u00e1 eliminada si no est\u00e1 asociada a ning\u00fan itinerario.'
  },

  // -- Routes module: static panel chrome --
  routes_title:         { en: 'routes',    fr: 'lignes',       es: 'rutas'    },
  routes_panel_title:   { en: 'Routes',    fr: 'Lignes',       es: 'Rutas'    },
  routes_drawing_title: { en: 'Drawing Mode', fr: 'Mode de dessin', es: 'Modo de dibujo' },
  routes_stopseq_title: { en: 'Stop Sequence', fr: 'S\u00e9quence d\u2019arr\u00eats', es: 'Secuencia de paradas' },
  routes_drawing_desc:  {
    en: 'Network mode routes along streets. Free mode draws straight lines between stops and waypoints.',
    fr: 'Le mode r\u00e9seau trace le parcours le long des rues. Le mode libre dessine des lignes droites entre les arr\u00eats et les points de passage.',
    es: 'El modo red traza la ruta a lo largo de las calles. El modo libre dibuja l\u00edneas rectas entre paradas y puntos de paso.'
  },

  // -- Routes module: JS confirm dialogs --
  confirm_delete_route: {
    en: 'Delete this route? Itineraries must be deleted first.',
    fr: 'Supprimer cette ligne\u00a0? Les parcours types doivent d\u2019abord \u00eatre supprim\u00e9s.',
    es: '\u00bfEliminar esta ruta? Los itinerarios deben eliminarse primero.'
  },
  confirm_delete_itin: {
    en: 'Delete this itinerary and its associated data?',
    fr: 'Supprimer ce parcours type et ses donn\u00e9es associ\u00e9es\u00a0?',
    es: '\u00bfEliminar este itinerario y sus datos asociados?'
  },

  // -- Schedule module: static panel chrome --
  sched_title:        { en: 'schedule',  fr: 'horaires',   es: 'horarios'  },
  sched_filter_title: { en: 'Service & Hour', fr: 'Service et heure', es: 'Servicio y hora' },
  sched_lbl_service:  { en: 'Service',  fr: 'Service',    es: 'Servicio'  },
  sched_lbl_hour:     { en: 'Hour',     fr: 'Heure',      es: 'Hora'      },
  sched_filter_desc:  {
    en: 'Click on any route segment on the map to view cumulative service level for this service and hour.',
    fr: 'Cliquez sur n\u2019importe quel segment de ligne sur la carte pour afficher le niveau de service cumulatif pour ce service et cette heure.',
    es: 'Haga clic en cualquier segmento de ruta en el mapa para ver el nivel de servicio acumulado para este servicio y esta hora.'
  },
  sched_btn_calendar: { en: 'Configure service calendar', fr: 'Configurer le calendrier de service', es: 'Configurar el calendario de servicio' },
  sched_btn_presets:  { en: 'Manage service level presets', fr: 'G\u00e9rer les niveaux de service pr\u00e9d\u00e9finis', es: 'Gestionar niveles de servicio predefinidos' },

  // -- Schedule module: JS confirm dialogs --
  confirm_delete_sw: {
    en: 'Delete this service window and associated headway by hour entries?',
    fr: 'Supprimer cette plage de service et les entr\u00e9es d\u2019intervalle par heure associ\u00e9es\u00a0?',
    es: '\u00bfEliminar esta ventana de servicio y las entradas de intervalo por hora asociadas?'
  },
  confirm_delete_cal_service: {
    en: 'Delete service "{id}"? This will remove all schedule data associated with this route.',
    fr: 'Supprimer le service \u00ab\u00a0{id}\u00a0\u00bb\u00a0? Cela supprimera toutes les donn\u00e9es d\u2019horaire associ\u00e9es \u00e0 cette ligne.',
    es: '\u00bfEliminar el servicio "{id}"? Esto eliminar\u00e1 todos los datos de horario asociados a esta ruta.'
  },
  confirm_delete_sched_preset: {
    en: 'Delete preset "{id}"?',
    fr: 'Supprimer le profil \u00ab\u00a0{id}\u00a0\u00bb\u00a0?',
    es: '\u00bfEliminar el perfil "{id}"?'
  },
  sched_recalc_unit_minutes: {
  en: 'minutes',
  fr: 'minutes',
  es: 'minutos'
},
sched_recalc_unit_kmh: {
  en: 'km/h',
  fr: 'km/h',
  es: 'km/h'
},

  // -- Save / Export tab --
  export_title:           { en: 'export or save your project', fr: 'exporter ou sauvegarder votre projet', es: 'exportar o guardar su proyecto' },
  export_gtfs_title:      { en: 'Export GTFS', fr: 'Exporter le GTFS', es: 'Exportar GTFS' },
  lbl_filename:           { en: 'Filename:', fr: 'Nom du fichier\u00a0:', es: 'Nombre del archivo:' },
  export_dist_traveled:   { en: 'Include shape_dist_traveled', fr: 'Inclure shape_dist_traveled', es: 'Incluir shape_dist_traveled' },
  export_dist_desc:       { en: 'When checked, adds shape_dist_traveled to shapes and stop_times tables. This increases export time.', fr: 'Lorsque coch\u00e9, ajoute shape_dist_traveled aux tables shapes et stop_times. Cela augmente le temps d\u2019exportation.', es: 'Cuando est\u00e1 marcado, agrega shape_dist_traveled a las tablas shapes y stop_times. Esto aumenta el tiempo de exportaci\u00f3n.' },
  export_download_gtfs:   { en: 'Download GTFS', fr: 'T\u00e9l\u00e9charger le GTFS', es: 'Descargar GTFS' },
  export_save_title:      { en: 'Save your project to work on it later', fr: 'Sauvegardez votre projet pour y travailler plus tard', es: 'Guarde su proyecto para trabajar en \u00e9l m\u00e1s tarde' },
  export_save_desc:       { en: 'This saves the raw Croquis (SSFS) file as a .rds:', fr: 'Ceci sauvegarde le fichier brut Croquis (SSFS) au format .rds\u00a0:', es: 'Esto guarda el archivo bruto Croquis (SSFS) como .rds:' },
  export_download_croquis:{ en: 'Download Croquis file', fr: 'T\u00e9l\u00e9charger le fichier Croquis', es: 'Descargar archivo Croquis' },
  export_save_note:       { en: 'Your transit system will be saved as an .rds file that you can reload later.', fr: 'Votre r\u00e9seau de transport sera sauvegard\u00e9 sous forme de fichier .rds que vous pourrez recharger plus tard.', es: 'Su red de transporte se guardar\u00e1 como un archivo .rds que puede recargar m\u00e1s tarde.' },

  // -- Settings tab --
  settings_title:         { en: 'settings', fr: 'param\u00e8tres', es: 'configuraci\u00f3n' },
  settings_feed_info:     { en: 'Feed info', fr: 'Informations du flux', es: 'Informaci\u00f3n del feed' },
  lbl_publisher_name:     { en: 'Publisher name', fr: 'Nom de l\u2019\u00e9diteur', es: 'Nombre del editor' },
  pop_publisher_name:     { en: 'Full name of the organization that publishes the feed.', fr: 'Nom complet de l\u2019organisme qui publie le flux.', es: 'Nombre completo de la organizaci\u00f3n que publica el feed.' },
  lbl_publisher_url:      { en: 'Publisher URL', fr: 'URL de l\u2019\u00e9diteur', es: 'URL del editor' },
  pop_publisher_url:      { en: 'URL of the feed publishing organization\u2019s website.', fr: 'URL du site web de l\u2019organisme publiant le flux.', es: 'URL del sitio web de la organizaci\u00f3n que publica el feed.' },
  lbl_feed_lang:          { en: 'Feed language', fr: 'Langue du flux', es: 'Idioma del feed' },
  pop_feed_lang:          { en: 'Default language used for text in this dataset (IETF BCP 47 language code).', fr: 'Langue par d\u00e9faut utilis\u00e9e pour le texte dans ce jeu de donn\u00e9es (code IETF BCP 47).', es: 'Idioma predeterminado usado para el texto en este conjunto de datos (c\u00f3digo de idioma IETF BCP 47).' },
  lbl_contact_email:      { en: 'Contact email', fr: 'Courriel de contact', es: 'Correo de contacto' },
  pop_contact_email:      { en: 'Email address for communication regarding the GTFS dataset and data publishing practices.', fr: 'Adresse courriel pour les communications concernant le jeu de donn\u00e9es GTFS et les pratiques de publication.', es: 'Direcci\u00f3n de correo para comunicaciones sobre el conjunto de datos GTFS y las pr\u00e1cticas de publicaci\u00f3n.' },
  lbl_feed_version:       { en: 'Version', fr: 'Version', es: 'Versi\u00f3n' },
  pop_feed_version:       { en: 'String that indicates the current version of their GTFS dataset.', fr: 'Cha\u00eene indiquant la version actuelle du jeu de donn\u00e9es GTFS.', es: 'Cadena que indica la versi\u00f3n actual del conjunto de datos GTFS.' },
  settings_advanced:      { en: 'Advanced settings', fr: 'Param\u00e8tres avanc\u00e9s', es: 'Configuraci\u00f3n avanzada' },
  lbl_routing_server:     { en: 'Default routing server', fr: 'Serveur de routage par d\u00e9faut', es: 'Servidor de rutas por defecto' },
  pop_routing_server:     { en: 'Routing server used to draw segments along the road network between stops and waypoints in the routes module.', fr: 'Serveur de routage utilis\u00e9 pour tracer les segments le long du r\u00e9seau routier entre les arr\u00eats et les points de passage dans le module Lignes.', es: 'Servidor de rutas utilizado para trazar segmentos a lo largo de la red vial entre paradas y puntos de paso en el m\u00f3dulo Rutas.' },
  lbl_gtfs_workers:       { en: 'GTFS import workers', fr: 'Processus d\u2019importation GTFS', es: 'Procesos de importaci\u00f3n GTFS' },
  pop_gtfs_workers:       { en: 'Number of worker processes to use during GTFS to SSFS conversion. Values above 1 speed up imports on Linux servers; Windows falls back to a single worker.', fr: 'Nombre de processus \u00e0 utiliser lors de la conversion GTFS vers SSFS. Les valeurs sup\u00e9rieures \u00e0 1 acc\u00e9l\u00e8rent l\u2019importation sur les serveurs Linux\u00a0; Windows utilise un seul processus.', es: 'N\u00famero de procesos a usar durante la conversi\u00f3n de GTFS a SSFS. Valores mayores a 1 aceleran la importaci\u00f3n en servidores Linux; Windows usa un solo proceso.' },
  lbl_gtfs_max_date:      { en: 'Specify GTFS import reference date', fr: 'Sp\u00e9cifier une date de r\u00e9f\u00e9rence pour l\u2019importation GTFS', es: 'Especificar fecha de referencia para la importaci\u00f3n GTFS' },
  pop_gtfs_max_date:      { en: 'When checked, the GTFS import will only consider service within the 7 days preceding the specified date. When unchecked, the last 7 days of service defined in the feed are used.', fr: 'Lorsque coch\u00e9, l\u2019importation GTFS ne consid\u00e9rera que le service dans les 7 jours pr\u00e9c\u00e9dant la date sp\u00e9cifi\u00e9e. Sinon, les 7 derniers jours de service d\u00e9finis dans le flux sont utilis\u00e9s.', es: 'Cuando est\u00e1 marcado, la importaci\u00f3n GTFS solo considerar\u00e1 el servicio dentro de los 7 d\u00edas anteriores a la fecha especificada. Si no, se usan los \u00faltimos 7 d\u00edas de servicio definidos en el feed.' },
  lbl_min_stop_dist:      { en: 'Minimum stop spacing (m)', fr: 'Espacement minimal des arr\u00eats (m)', es: 'Espaciamiento m\u00ednimo de paradas (m)' },
  pop_min_stop_dist:      { en: 'Minimum distance in metres between auto-generated stops. Also used as the buffer distance around existing stops when determining eligible locations for new stops.', fr: 'Distance minimale en m\u00e8tres entre les arr\u00eats g\u00e9n\u00e9r\u00e9s automatiquement. \u00c9galement utilis\u00e9e comme distance tampon autour des arr\u00eats existants pour d\u00e9terminer les emplacements admissibles.', es: 'Distancia m\u00ednima en metros entre paradas generadas autom\u00e1ticamente. Tambi\u00e9n se usa como distancia de amortiguamiento alrededor de las paradas existentes para determinar ubicaciones elegibles.' },
  lbl_osm_provider:       { en: 'OSM data provider', fr: 'Fournisseur de donn\u00e9es OSM', es: 'Proveedor de datos OSM' },
  pop_osm_provider:       { en: 'OpenStreetMap data provider used when generating stops from road network data. Different providers have different regional coverage.', fr: 'Fournisseur de donn\u00e9es OpenStreetMap utilis\u00e9 lors de la g\u00e9n\u00e9ration d\u2019arr\u00eats \u00e0 partir des donn\u00e9es du r\u00e9seau routier. Les fournisseurs offrent des couvertures r\u00e9gionales diff\u00e9rentes.', es: 'Proveedor de datos OpenStreetMap usado al generar paradas a partir de datos de la red vial. Diferentes proveedores tienen diferente cobertura regional.' }
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
