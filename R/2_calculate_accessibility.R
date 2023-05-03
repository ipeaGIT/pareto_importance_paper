# frontier <- tar_read(absolute_frontier)
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
  
  future::plan(future::multisession, workers = getOption("N_CORES") / 3)
  
  accessibility <- furrr::future_pmap(
    iterator,
    function(tt, mc) {
      loadNamespace("data.table")
      sum_opp <- frontier[travel_time <= tt][get(monetary_column) <= mc]
      sum_opp <- sum_opp[sum_opp[, .I[1], by = .(from_id, to_id)]$V1]
      sum_opp <- sum_opp[, .(sum_opp = sum(dest_jobs)), keyby = from_id]
      
      origins_without_access <- setdiff(od_points$id, sum_opp$from_id)
      zero_access <- data.table::data.table(
        from_id = origins_without_access,
        sum_opp = rep(0, length(origins_without_access))
      )
      
      access <- rbind(sum_opp, zero_access)
      data.table::setnames(access, new = c("id", "access"))
      
      access[, `:=`(travel_time = tt, monetary_limit = mc)]
      access
    }
  )
  
  future::plan(future::sequential)
  
  accessibility <- data.table::rbindlist(accessibility)
  data.table::setnames(accessibility, old = "monetary_limit", type)
  
  accessibility[]
}