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