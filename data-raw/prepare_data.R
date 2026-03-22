# data-raw/prepare_data.R

# Helper to download an rds from GitHub
download_rds <- function(url) {
  temp <- tempfile(fileext = ".rds")
  download.file(url, temp, mode = "wb")
  obj <- readRDS(temp)
  unlink(temp)
  obj
}

base_url <- "https://github.com/julian-city/gtfsforge/raw/refs/heads/main"

# Sample networks (exported)
ligne_jaune <- download_rds(paste0(
  base_url,
  "/sample_networks/ligne_jaune_v2.rds"
))
stm_metro <- download_rds(paste0(base_url, "/sample_networks/metro_v2.rds"))
mileend <- download_rds(paste0(base_url, "/sample_networks/mileend_v2.rds"))
ttc_subway <- download_rds(paste0(
  base_url,
  "/sample_networks/ttcsubway_v2.rds"
))

usethis::use_data(ligne_jaune, stm_metro, mileend, ttc_subway, overwrite = TRUE)

# Cities database (internal)
cities_db <- download_rds(paste0(base_url, "/cities_db.rds"))

usethis::use_data(cities_db, internal = TRUE, overwrite = TRUE) ## code to prepare `prepare_data` dataset goes here
