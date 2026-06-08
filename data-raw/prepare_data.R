# data-raw/prepare_data.R

#Translink Vancouver GTFS (June 2026)

translink_filepath <- "https://files.mobilitydatabase.org/mdb-696/mdb-696-202605020004/mdb-696-202605020004.zip"
#filepath retrieved on Monday May 4th

translink_gtfs <- gtfstools::read_gtfs(translink_filepath)
translink <- gtfs_to_ssfs(translink_gtfs)

#STM GTFS (mile end, STM metro and ligne jaune): May-June 2025 schedule (before bus network redesign)

stm_filepath <- "https://files.mobilitydatabase.org/mdb-2126/mdb-2126-202504170018/mdb-2126-202504170018.zip"
#filepath retrieved on Monday May 4th 2026

stm_gtfs <- gtfstools::read_gtfs(stm_filepath)

mileend <- gtfs_to_ssfs(
  stm_gtfs,
  routes = c("30", "31", "46", "51", "55", "80", "160", "161", "480")
)
stm_metro <- gtfs_to_ssfs(stm_gtfs, routes = c("1", "2", "4", "5"))
ligne_jaune <- gtfs_to_ssfs(stm_gtfs, routes = "4")

#TTC subway: May-June 2026 schedule
ttc_filepath <- "https://files.mobilitydatabase.org/mdb-732/mdb-732-202604250134/mdb-732-202604250134.zip"
#Filepath retrieved on May 4th, 2026

ttc_gtfs <- gtfstools::read_gtfs(ttc_filepath)
ttc_subway <- gtfs_to_ssfs(ttc_gtfs, routes = c("1", "2", "4", "5", "6"))

usethis::use_data(
  translink,
  ligne_jaune,
  stm_metro,
  mileend,
  ttc_subway,
  overwrite = TRUE
)

# Cities database (internal)
# Original code for this database required

# Helper to download an rds from GitHub
download_rds <- function(url) {
  temp <- tempfile(fileext = ".rds")
  download.file(url, temp, mode = "wb")
  obj <- readRDS(temp)
  unlink(temp)
  obj
}

base_url <- "https://github.com/julian-city/gtfsforge/raw/refs/heads/main"

cities_db <- download_rds(paste0(base_url, "/cities_db.rds"))

usethis::use_data(cities_db, internal = TRUE, overwrite = TRUE)

# Railway City Transit GTFS (St. Thomas, Ontario)

gtfs_rct <- gtfstools::read_gtfs(
  "https://files.mobilitydatabase.org/tld-4746/tld-4746-202605300110/tld-4746-202605300110.zip"
)
# retrieved 2 June 2026

# ssfs_rct2: Railway City Transit Scenario SSFS

# Created in Croquis using :
# ssfs_rct <- gtfs_to_ssfs(gtfs_rct,max_date=as.Date("2026-06-08"))
# croquis(ssfs_rct)
# Export stops
# croquis() then import stops
# manually draw routes
#import ssfs_rct2
#e.g. ssfs_rct2 <- readRDS("~/Downloads/ssfs_rct2.rds")
