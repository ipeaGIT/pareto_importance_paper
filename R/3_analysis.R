create_heatmap_theme <- function() {
  theme_minimal() +
    theme(
      panel.grid = element_blank(),
      legend.position = "right",
      strip.text = element_text(size = 11)
    )
}

# access <- tar_read(absolute_accessibility)
# grid_path <- tar_read(rio_grid)
create_absolute_map <- function(access, grid_path) {
  grid <- readRDS(grid_path)
  
  access <- access[travel_time == 75]
  access <- access[absolute == 5]
  
  access[grid, on = "id", geometry := i.geometry]
  
  # remove grid object to reduce plot object size
  rm(grid)
  
  panels_titles <- c(
    free_fastest = "No-cost\nmethod (A)",
    fastest_cost = "Fastest-trip-cost\nmethod (C)",
    pareto_frontier = "Pareto-frontier\nmethod (B)",
    free_fastest_diff = "Difference:\nA - B",
    fastest_cost_diff = "Difference:\nC - B"
  )
  
  max_access <- max(access$access)
  
  make_row <- function(to_compare = c("free_fastest", "fastest_cost")) {
    access_dist <- access[method %in% c(to_compare, "pareto_frontier")]
    
    distribution <- ggplot(sf::st_sf(access_dist)) +
      geom_sf(aes(fill = access), color = NA) +
      facet_wrap( ~ method, labeller = as_labeller(panels_titles)) +
      scale_fill_viridis_c(
        name = "Accessibility",
        labels = scales::label_number(scale = 1 / 1000, suffix = "k"),
        limits = c(0, max_access)
      ) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = 11)
      )
    
    access_diff <- data.table::copy(access_dist)
    access_diff[
      ,
      diff := (access - data.table::shift(access, n = 1)) * -1,
      by = id
    ]
    access_diff <- access_diff[!is.na(diff)]
    access_diff[, method := paste0(to_compare, "_diff")]
    
    # remove access_dist object to reduce plot object size
    rm(access_dist)
    
    difference <- ggplot(sf::st_sf(access_diff)) +
      geom_sf(aes(fill = diff), color = NA) +
      facet_wrap( ~ method, labeller = as_labeller(panels_titles)) +
      scale_fill_viridis_c(
        name = "Difference",
        labels = scales::label_number(scale = 1 / 1000, suffix = "k"),
        option = ifelse(to_compare == "free_fastest", "cividis", "inferno"),
        direction = ifelse(to_compare == "free_fastest", 1, -1)
      ) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = 11)
      )
    
    # remove access_diff object to reduce plot object size
    rm(access_diff)
    
    row <- cowplot::plot_grid(
      distribution,
      difference,
      nrow = 1,
      rel_widths = c(1.96, 1)
    )
    
    row
  }
  
  first_row <- make_row("free_fastest")
  second_row <- make_row("fastest_cost")
  
  p <- cowplot::plot_grid(first_row, second_row, nrow = 2)
  
  p
}

# access <- tar_read(absolute_accessibility)
# grid_path <- tar_read(rio_grid)
# heatmap_theme <- tar_read(heatmap_theme)
# to_compare <- "free_fastest"
create_absolute_heatmap <- function(access,
                                    grid_path,
                                    heatmap_theme,
                                    to_compare = c(
                                      "free_fastest",
                                      "fastest_cost"
                                    )) {
  grid <- readRDS(grid_path)
  
  access <- access[method %in% c(to_compare, "pareto_frontier")]
  access[grid, on = "id", population := i.population]
  access <- access[
    ,
    .(avg_access = weighted.mean(access, w = population)),
    keyby = .(method, travel_time, cost_cutoff = absolute)
  ]
  
  # remove grid object to reduce plot object size
  rm(grid)
  
  panels_titles <- c(
    free_fastest = "No-cost\nmethod (A)",
    fastest_cost = "Fastest-trip-cost\nmethod (A)",
    pareto_frontier = "Pareto-frontier\nmethod (B)",
    difference = "Difference (C)\nA - B"
  )
  
  distribution <- ggplot(access) + 
    geom_tile(aes(travel_time, cost_cutoff, fill = avg_access)) +
    facet_wrap(~ method, labeller = as_labeller(panels_titles)) +
    scale_fill_viridis_c(
      name = "Average\naccessibility\n",
      labels = scales::label_number(scale = 1 / 1000, suffix = "k")
    ) +
    scale_x_continuous(
      name = "Travel time threshold",
      breaks = c(0, 30, 60, 90)
    ) +
    scale_y_continuous(
      name = "Absolute monetary cost threshold (BRL)",
      breaks = c(0, 5, 10, 15)
    ) +
    heatmap_theme
  
  access_diff <- data.table::dcast(
    access,
    travel_time + cost_cutoff ~ method,
    value.var = "avg_access"
  )
  access_diff[, diff := (get(to_compare) - pareto_frontier) / pareto_frontier]
  access_diff[is.nan(diff), diff := 0]
  access_diff[, c(to_compare, "pareto_frontier") := NULL]
  access_diff[, method := "difference"]
  access_diff[diff > 1, diff := 1]
  
  # remove access object to reduce plot object size
  rm(access)
  
  difference <- ggplot(access_diff) + 
    geom_tile(aes(travel_time, cost_cutoff, fill = diff)) +
    facet_wrap(~ method, labeller = as_labeller(panels_titles)) +
    scale_fill_viridis_c(
      name = "Accessibility\ndifference\n(% of Pareto)",
      labels = label_percent_100plus,
      option = ifelse(to_compare == "free_fastest", "cividis", "inferno"),
      direction = ifelse(to_compare == "free_fastest", 1, -1)
    ) +
    scale_x_continuous(
      name = "Travel time threshold",
      breaks = c(0, 30, 60, 90)
    ) +
    scale_y_continuous(
      name = "Absolute monetary cost threshold (BRL)",
      breaks = c(0, 5, 10, 15)
    ) +
    heatmap_theme +
    theme(axis.title.y = element_blank())
  
  # remove access_diff object to reduce plot object size
  rm(access_diff)
  
  p <- cowplot::plot_grid(
    distribution,
    difference,
    nrow = 1,
    rel_widths = c(1.6, 1)
  )
  
  p
}

# access <- tar_read(affordability_accessibility)
# grid_path <- tar_read(rio_grid)
# heatmap_theme <- tar_read(heatmap_theme)
# to_compare <- "free_fastest"
create_affordability_heatmap <- function(access,
                                         grid_path,
                                         heatmap_theme,
                                         to_compare = c(
                                           "free_fastest",
                                           "fastest_cost"
                                         )) {
  grid <- readRDS(grid_path)
  
  access <- access[method %in% c(to_compare, "pareto_frontier")]
  access[grid, on = "id", population := i.population]
  access <- access[
    ,
    .(avg_access = weighted.mean(access, w = population)),
    keyby = .(method, travel_time, cost_cutoff = affordability)
  ]
  
  # remove grid object to reduce plot object size
  rm(grid)
  
  panels_titles <- c(
    free_fastest = "No-cost\nmethod (A)",
    fastest_cost = "Fastest-trip-cost\nmethod (A)",
    pareto_frontier = "Pareto-frontier\nmethod (B)",
    difference = "Difference (C)\nA - B"
  )
  
  distribution <- ggplot(access) + 
    geom_tile(aes(travel_time, cost_cutoff, fill = avg_access)) +
    facet_wrap(~ method, labeller = as_labeller(panels_titles)) +
    scale_fill_viridis_c(
      name = "Average\naccessibility\n",
      labels = scales::label_number(scale = 1 / 1000, suffix = "k")
    ) +
    scale_x_continuous(
      name = "Travel time threshold",
      breaks = c(0, 30, 60, 90)
    ) +
    scale_y_continuous(
      name = "Relative monetary cost threshold\n(% of monthly budget)",
      breaks = c(0, 0.1, 0.2, 0.3, 0.4),
      labels = scales::label_percent()
    ) +
    heatmap_theme
  
  access_diff <- data.table::dcast(
    access,
    travel_time + cost_cutoff ~ method,
    value.var = "avg_access"
  )
  access_diff[, diff := (get(to_compare) - pareto_frontier) / pareto_frontier]
  access_diff[is.nan(diff), diff := 0]
  access_diff[, c(to_compare, "pareto_frontier") := NULL]
  access_diff[, method := "difference"]
  access_diff[diff > 1, diff := 1]
  
  # remove access object to reduce plot object size
  rm(access)
  
  difference <- ggplot(access_diff) + 
    geom_tile(aes(travel_time, cost_cutoff, fill = diff)) +
    facet_wrap(~ method, labeller = as_labeller(panels_titles)) +
    scale_fill_viridis_c(
      name = "Accessibility\ndifference\n(% of Pareto)",
      labels = label_percent_100plus,
      option = ifelse(to_compare == "free_fastest", "cividis", "inferno"),
      direction = ifelse(to_compare == "free_fastest", 1, -1)
    ) +
    scale_x_continuous(
      name = "Travel time threshold",
      breaks = c(0, 30, 60, 90)
    ) +
    scale_y_continuous(
      name = "Relative monetary cost threshold\n(% of monthly budget)",
      breaks = c(0, 0.1, 0.2, 0.3, 0.4),
      labels = scales::label_percent()
    ) +
    heatmap_theme +
    theme(axis.title.y = element_blank())
  
  # remove access_diff object to reduce plot object size
  rm(access_diff)
  
  p <- cowplot::plot_grid(
    distribution,
    difference,
    nrow = 1,
    rel_widths = c(1.6, 1)
  )
  
  return(p)
}

# access <- tar_read(affordability_accessibility)
# grid_path <- tar_read(rio_grid)
# heatmap_theme <- tar_read(heatmap_theme)
# to_compare <- "free_fastest"
create_afford_per_group_heatmap <- function(access,
                                            grid_path,
                                            heatmap_theme,
                                            to_compare = c(
                                              "free_fastest",
                                              "fastest_cost"
                                            )) {
  grid <- readRDS(grid_path)
  
  access <- access[method %in% c(to_compare, "pareto_frontier")]
  access[grid, on = "id", `:=`(population = i.population, decile = i.decile)]
  access[decile == 10, group := "richest_10"]
  access[decile %in% 1:4, group := "poorest_40"]
  access <- access[!is.na(group)]
  
  access <- access[
    ,
    .(avg_access = weighted.mean(access, w = population)),
    keyby = .(method, travel_time, cost_cutoff = affordability, group)
  ]
  
  # remove grid object to reduce plot object size
  rm(grid)
  
  panels_titles <- c(
    free_fastest = "No-cost\nmethod (A)",
    fastest_cost = "Fastest-trip-cost\nmethod (A)",
    pareto_frontier = "Pareto-frontier\nmethod (B)",
    richest_10 = "Wealthiest 10%",
    poorest_40 = "Poorest 40%",
    difference = "Difference (C)\nA - B"
  )
  
  distribution <- ggplot(access) + 
    geom_tile(aes(travel_time, cost_cutoff, fill = avg_access)) +
    facet_grid(group ~ method, labeller = as_labeller(panels_titles)) +
    scale_fill_viridis_c(
      name = "Average\naccessibility\n",
      labels = scales::label_number(scale = 1 / 1000, suffix = "k")
    ) +
    scale_x_continuous(
      name = "Travel time threshold",
      breaks = c(0, 30, 60, 90)
    ) +
    scale_y_continuous(
      name = "Relative monetary cost threshold\n(% of monthly budget)",
      breaks = c(0, 0.1, 0.2, 0.3, 0.4),
      labels = scales::label_percent()
    ) +
    heatmap_theme
  
  access_diff <- data.table::dcast(
    access,
    travel_time + cost_cutoff + group ~ method,
    value.var = "avg_access"
  )
  access_diff[, diff := (get(to_compare) - pareto_frontier) / pareto_frontier]
  access_diff[is.nan(diff), diff := 0]
  access_diff[, c(to_compare, "pareto_frontier") := NULL]
  access_diff[, method := "difference"]
  access_diff[diff > 1, diff := 1]
  
  # remove access object to reduce plot object size
  rm(access)
  
  difference <- ggplot(access_diff) + 
    geom_tile(aes(travel_time, cost_cutoff, fill = diff)) +
    facet_grid(group ~ method, labeller = as_labeller(panels_titles)) +
    scale_fill_viridis_c(
      name = "Accessibility\ndifference\n(% of Pareto)",
      labels = label_percent_100plus,
      option = ifelse(to_compare == "free_fastest", "cividis", "inferno"),
      direction = ifelse(to_compare == "free_fastest", 1, -1)
    ) +
    scale_x_continuous(
      name = "Travel time threshold",
      breaks = c(0, 30, 60, 90)
    ) +
    scale_y_continuous(
      name = "Relative monetary cost threshold\n(% of monthly budget)",
      breaks = c(0, 0.1, 0.2, 0.3, 0.4),
      labels = scales::label_percent()
    ) +
    heatmap_theme +
    theme(axis.title.y = element_blank())
  
  # remove access_diff object to reduce plot object size
  rm(access_diff)
  
  p <- cowplot::plot_grid(
    distribution,
    difference,
    nrow = 1,
    rel_widths = c(1.6, 1)
  )
  
  p
}

# frontier <- tar_read(frontier_with_afford)
# routing_dir <- tar_read(routing_dir)
# od_points <- tar_read(od_points)
create_frontier_plot <- function(frontier, routing_dir, od_points) {
  pareto_frontier <- frontier[method == "pareto_frontier"]
  pareto_frontier[
    ,
    time_gap := max(travel_time) - min(travel_time),
    by = .(from_id, to_id)
  ]
  
  n_transfers <- pareto_frontier[
    ,
    .(.N, time_gap = time_gap[1], min_travel_time = min(travel_time)),
    by = .(from_id, to_id)
  ]
  
  # choosing N = 7 because seems to fill the plot well. want a reasonably large
  # time gap as well, with a min travel time not too high
  
  seven_rides <- n_transfers[N == 7]
  
  chosen_pair <- seven_rides[time_gap == 37 & min_travel_time == 58]
  
  chosen_frontier <- pareto_frontier[
    from_id == chosen_pair$from_id & to_id == chosen_pair$to_id
  ]
  
  cols_to_remove <- setdiff(
    names(chosen_frontier),
    c("from_id", "to_id", "travel_time", "monetary_cost")
  )
  chosen_frontier[, (cols_to_remove) := NULL]
  
  # calculating the walking trip between the chosen pair, as it is not included
  # in the frontier (wouldn't satisfy the max_walk_time = 30)
  
  r5r_core <- r5r::setup_r5(routing_dir)
  
  walking_trip <- r5r::travel_time_matrix(
    r5r_core,
    origins = od_points[id == chosen_pair$from_id],
    destinations = od_points[id == chosen_pair$to_id],
    mode = "WALK",
    departure_datetime = as.POSIXct(
      "08-01-2020 07:00:00",
      format = "%d-%m-%Y %H:%M:%S"
    ),
    time_window = 60,
    max_trip_duration = 240,
    walk_speed = 3.6,
    n_threads = 1,
    verbose = FALSE,
    progress = FALSE
  )
  walking_trip[, monetary_cost := 0]
  data.table::setnames(walking_trip, "travel_time_p50", "travel_time")
  
  # bind walking trip and itineraries information to chosen_frontier to create
  # the dataset used in the plot
  
  chosen_frontier <- rbind(chosen_frontier, walking_trip)
  
  chosen_frontier$itinerary <- c(
    "Bus \U2192 Subway \U2192 Bus",
    "BRT \U2192 Rail \U2192 Bus",
    "BRT \U2192 Subway \U2192 Bus",
    "Subway \U2192 Bus",
    "BRT \U2192 Rail",
    "BRT \U2192 Bus \U2192 Bus",
    "Bus \U2192 Bus",
    "Walk"
  )
  
  # remove objects to reduce plot object size
  rm(chosen_pair, frontier, n_transfers, od_points, pareto_frontier, r5r_core,
     seven_rides, walking_trip)
  
  p <- ggplot(chosen_frontier) +
    geom_step(aes(x = monetary_cost, y = travel_time), linetype = "longdash") +
    geom_point(aes(x = monetary_cost, y = travel_time)) +
    geom_segment(
      aes(x = 13.1, y = 58, xend = 16, yend = 58),
      linetype = "longdash"
    ) +
    geom_text(
      aes(x = monetary_cost, y = travel_time, label = itinerary),
      hjust = 0,
      angle = 45,
      nudge_x = 0.1,
      nudge_y = 5,
      size = 3
    ) +
    scale_y_continuous(
      name = "Total travel time (minutes)",
      limits = c(0, 250),
      expand = c(0, 0),
      breaks = c(0, 40, 80, 120, 160, 200, 240)
    ) +
    scale_x_continuous(
      name = "Total fare (BRL)",
      limits = c(0, 16),
      expand = c(0, 0),
      breaks = c(0, 5, 10, 15)
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank(), axis.line = element_line())
  
  p
}

label_percent_100plus <- function(x) {
  lab <- scales::label_percent()(x)
  lab <- sub("100%", "100%+", lab)
  return(lab)
}