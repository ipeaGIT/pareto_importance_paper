# frontier <- tar_read(frontier_with_afford)
# grid_path <- tar_read(rio_grid)
# od_points <- tar_read(od_points)
# type <- "absolute"
calculate_accessibility <- function(frontier,
                                    grid_path,
                                    od_points,
                                    type = c("absolute", "affordability")) {
  grid <- readRDS(grid_path)
  frontier[grid, on = c(to_id = "id"), dest_jobs := i.jobs]
  
  monetary_cutoffs <- if (type == "affordability") {
    seq(0, 0.4, 0.01)
  } else {
    seq(0, 15, 0.2)
  }
  
  iterator <- expand.grid(tt = seq(1, 90, 1), mc = monetary_cutoffs)
  
  monetary_column <- ifelse(
    type == "affordability",
    "relative_monthly_cost",
    "monetary_cost"
  )
  
  future::plan(future::multisession, workers = getOption("N_CORES") / 2)
  
  accessibility <- furrr::future_pmap(
    iterator,
    function(tt, mc) {
      loadNamespace("data.table")
      sum_opp <- frontier[travel_time <= tt][get(monetary_column) <= mc]
      sum_opp <- sum_opp[sum_opp[, .I[1], by = .(method, from_id, to_id)]$V1]
      sum_opp <- sum_opp[
        ,
        .(sum_opp = sum(dest_jobs)),
        keyby = .(method, from_id)
      ]
      
      access <- data.table::data.table(
        method = rep(
          factor(c("pareto_frontier", "fastest_cost", "free_fastest")),
          length(od_points$id)
        ),
        from_id = rep(od_points$id, each = 3)
      )
      access[sum_opp, on = c("method", "from_id"), access := i.sum_opp]
      access[is.na(access), access := 0]
      access[, `:=`(travel_time = tt, monetary_limit = mc)]

      data.table::setnames(access, old = c("from_id"), new = c("id"))
      
      access
    }
  )
  
  future::plan(future::sequential)
  
  accessibility <- data.table::rbindlist(accessibility)
  accessibility <- accessibility[order(method, id)]
  accessibility[
    ,
    method := factor(
      method,
      levels = c("free_fastest", "fastest_cost", "pareto_frontier")
    )
  ]
  data.table::setnames(accessibility, old = "monetary_limit", type)
  
  accessibility[]
}