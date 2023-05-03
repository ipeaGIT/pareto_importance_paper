# grid_path <- tar_read(rio_grid)
generate_od_points <- function(grid_path) {
  grid <- data.table::setDT(readRDS(grid_path))
  grid <- grid[population > 0 | income_per_capita > 0 | jobs > 0]
  
  # get centroids and format as required by r5r
  # suppressed warning:
  #  st_centroid assumes attributes are constant over geometries
  
  centroids <- sf::st_sf(grid)
  centroids <- suppressWarnings(sf::st_centroid(centroids))
  
  coords <- data.table::as.data.table(sf::st_coordinates(centroids))
  
  centroids <- data.table::setDT(cbind(centroids, coords))
  centroids[, setdiff(names(centroids), c("id", "X", "Y")) := NULL]
  data.table::setnames(centroids, old = c("X", "Y"), new = c("lon", "lat"))
  
  centroids[]
}

# gtfs_fetranspor_url <- tar_read(gtfs_fetranspor_url)
# gtfs_supervia_url <- tar_read(gtfs_supervia_url)
# street_network_url <- tar_read(rio_street_network_url)
# topography_url <- tar_read(rio_topography_url)
build_transport_network <- function(gtfs_fetranspor_url,
                                    gtfs_supervia_url,
                                    street_network_url,
                                    topography_url) {
  if (!dir.exists("r5")) dir.create("r5")
  
  gtfs_fetransport_path <- file.path("r5", "gtfs_fetranspor.zip")
  httr::GET(
    gtfs_fetranspor_url,
    httr::write_disk(gtfs_fetransport_path, overwrite = TRUE)
  )
  
  gtfs_supervia_path <- file.path("r5", "gtfs_supervia.zip")
  httr::GET(
    gtfs_supervia_url,
    httr::write_disk(gtfs_supervia_path, overwrite = TRUE)
  )
  
  street_network_path <- file.path("r5", "rio_street_network.osm.pbf")
  httr::GET(
    street_network_url,
    httr::write_disk(street_network_path, overwrite = TRUE)
  )
  
  topography_path <- file.path("r5", "rio_topography.tif")
  httr::GET(topography_url, httr::write_disk(topography_path, overwrite = TRUE))
  
  r5r::setup_r5(
    "r5",
    verbose = getOption("VERBOSE_R5R", default = FALSE),
    overwrite = TRUE
  )
  
  file.path("r5")
}