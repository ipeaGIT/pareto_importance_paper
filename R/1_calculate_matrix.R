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

# routing_dir <- tar_read(routing_dir)
# od_points <- tar_read(od_points)
# rio_fare_structure_path <- tar_read(rio_fare_structure)
calculate_frontier <- function(routing_dir,
                               od_points,
                               rio_fare_structure_path) {
  # calculating the matrix can take a very long time and require lots of RAM and
  # processing power, so we control whether to actually calculate it with an
  # option
  # if we are not calculating it, we download it from github. similarly, we have
  # to upload the matrix to the data github release when we calculating it
  
  if (!getOption("CALCULATE_FRONTIER", default = FALSE)) {
    frontier_url <- "https://github.com/ipeaGIT/pareto_importance_paper/releases/download/data/rio_pareto_frontier.rds"
    tmpfile <- tempfile()
    
    httr::GET(frontier_url, httr::write_disk(tmpfile))
    
    frontier <- readRDS(tmpfile)
  } else {
    r5r_core <- r5r::setup_r5(routing_dir, verbose = FALSE)
    
    # to calculate the frontier, we have to specify the monetary cost cutoffs.
    # we pick values to use as cutoffs based on rio's fare values (we limit
    # these values to 15 BRL, which includes the large majority of meaningful
    # transit trips in rio) (in a previuos experiment, we saw that calculating a
    # pareto frontier while limiting fares to 15 BRL results in a frontier with
    # 96% of the trips that result from calculating a pareto frontier with a
    # fare limit of 24 BRL)
    
    max_rides <- 4
    
    rio_fare_structure <- r5r::read_fare_structure(rio_fare_structure_path)
    possible_fare_values <- generate_possible_fare_values(
      rio_fare_structure,
      max_value = 15,
      max_rides = max_rides
    )
    
    walking_speed <- 3.6
    
    frontier <- r5r::pareto_frontier(
      r5r_core,
      origins = od_points,
      destinations = od_points,
      mode = c("WALK", "TRANSIT"),
      departure_datetime = as.POSIXct(
        "08-01-2020 07:00:00",
        format = "%d-%m-%Y %H:%M:%S"
      ),
      time_window = 60,
      max_trip_duration = 120,
      max_walk_time = 30,
      max_rides = max_rides,
      walk_speed = walking_speed,
      fare_cutoffs = possible_fare_values,
      fare_structure = rio_fare_structure,
      n_threads = getOption("N_CORES"),
      verbose = FALSE,
      progress = getOption("SHOW_R5R_PROGRESS")
    )
    frontier[, percentile := NULL]
  
    # uploading frontier to data release, so we can later use it without having
    # to recalculate the matrix in a lower resources environment
    # ps: if the upload eventually fail it might be because the github PAT set
    # in .Renviron has expired
    
    tmpfile <- tempfile("rio_pareto_frontier", fileext = ".rds")
    saveRDS(frontier, tmpfile)
    
    piggyback::pb_upload(
      tmpfile,
      repo = "ipeaGIT/pareto_importance_paper",
      tag = "data",
      name = "rio_pareto_frontier.rds"
    )
  }
  
  frontier[]
}

# rio_grid_path <- tar_read(rio_grid)
adjust_grid_income <- function(rio_grid_path) {
  # the income data in our grid comes from 2010 census, whereas the transit
  # fares we use to calculate the cost of each trip are relative to the 2019
  # transit network. therefore, we adjust the income based on brazilian
  # inflation. to do this, we download a data series that shows how prices
  # historically evolved when adjusted by the ipca index
  
  response <- httr::GET(
    "http://ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='PRECOS12_IPCA12')"
  )
  response <- httr::content(response)$value
  
  values <- data.table::rbindlist(response)
  values[, setdiff(names(values), c("VALDATA", "VALVALOR")) := NULL]
  values[, VALDATA := as.Date(VALDATA)]
  
  value_2010 <- values[VALDATA == as.Date("2010-10-01")]$VALVALOR
  value_2019 <- values[VALDATA == as.Date("2019-10-01")]$VALVALOR
  
  adjustment_rate <- value_2019 / value_2010
  
  grid <- readRDS(rio_grid_path)
  grid[, income_per_capita := income_per_capita * adjustment_rate]
  grid[, setdiff(names(grid), c("id", "income_per_capita")) := NULL]
  
  grid[]
}

# absolute_frontier <- tar_read(frontier)
# origin_income <- tar_read(adjusted_income)
calculate_affordability_frontier <- function(absolute_frontier, origin_income) {
  afford_frontier <- absolute_frontier[order(from_id, to_id, travel_time)]
  afford_frontier[
    origin_income,
    on = c(from_id = "id"),
    income_per_capita := i.income_per_capita
  ]
  
  afford_frontier[
    ,
    relative_monthly_cost := monetary_cost * 44 / income_per_capita
  ]
  afford_frontier[is.nan(relative_monthly_cost), relative_monthly_cost := 0]
  afford_frontier[, income_per_capita := NULL]
  
  # we also bind to this frontier the matrices representing the business as
  # usual cases, in which only the fastest trip between points is present in the
  # matrices (one case with their costs in the matrix, the other with all trips
  # being free, representing the scenario where monetary costs are overlooked)
  
  fastest_matrix <- afford_frontier[order(from_id, to_id, travel_time)]
  fastest_matrix <- fastest_matrix[
    fastest_matrix[, .I[1], by = .(from_id, to_id)]$V1
  ]
  
  fastest_free_matrix <- data.table::copy(fastest_matrix)
  fastest_free_matrix[, c("monetary_cost", "relative_monthly_cost") := 0]
  
  merged_frontiers <- rbind(
    afford_frontier,
    fastest_matrix,
    fastest_free_matrix,
    idcol = "method"
  )
  merged_frontiers[
    ,
    method := factor(
      method,
      levels = 1:3,
      labels = c("pareto_frontier", "fastest_cost", "free_fastest")
    )
  ]
  
  merged_frontiers[]
}

generate_possible_fare_values <- function(rio_fare_structure,
                                          max_value,
                                          max_rides) {
  values <- c(
    0,
    rio_fare_structure$fares_per_type$fare,
    rio_fare_structure$fares_per_transfer$fare
  )
  values <- unique(values)
  values <- lapply(
    1:max_rides,
    function(m) {
      list_of_values <- rep(list(values), m)
      combinations <- do.call(expand.grid, list_of_values)
      combinations <- rowSums(combinations)
    }
  )
  values <- unlist(values)
  values <- unique(values)
  values <- values[values <= max_value]
  values <- values[order(values)]
  
  return(values)
}
