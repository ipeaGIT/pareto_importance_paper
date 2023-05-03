options(
  java.parameters = "-Xmx50G",
  N_CORES = 30L,
  SHOW_R5R_PROGRESS = TRUE,
  VERBOSE_R5R = FALSE
)

suppressPackageStartupMessages({
  library(targets)
  library(sf)
})

source("R/1_calculate_matrix.R", encoding = "UTF-8")

list(
  # data targets
  tar_target(rio_grid, "data-raw/rio_grid.rds", format = "file"),
  tar_target(rio_fare_structure, "data-raw/rio_fares.zip", format = "file"),
  tar_target(od_points, generate_od_points(rio_grid)),
  tar_target(
    gtfs_fetranspor_url,
    "https://github.com/ipeaGIT/pareto_importance_paper/releases/download/data/gtfs_fetranspor.zip",
    format = "url"
  ),
  tar_target(
    gtfs_supervia_url,
    "https://github.com/ipeaGIT/pareto_importance_paper/releases/download/data/gtfs_supervia.zip",
    format = "url"
  ),
  tar_target(
    rio_street_network_url,
    "https://github.com/ipeaGIT/pareto_importance_paper/releases/download/data/rio_street_network.osm.pbf",
    format = "url"
  ),
  tar_target(
    rio_topography_url,
    "https://github.com/ipeaGIT/pareto_importance_paper/releases/download/data/rio_topography.tif",
    format = "url"
  ),
  
  # code-generated targets
  tar_target(
    routing_dir,
    build_transport_network(
      gtfs_fetranspor_url,
      gtfs_supervia_url,
      rio_street_network_url,
      rio_topography_url
    ),
    format = "file"
  )
)