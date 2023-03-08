


discover_parallels_complete <- function(eventlog, all, potential = NULL) {


  act_b <- all
  if(is.null(potential)) {
    act_a <- all
  } else{
    act_a <- potential
  }

  # eventlog %>%
  #   filter(!(.data[[activity_id(eventlog)]] %in% c("START","END"))) %>%
  #   pull(orig_name) %>%
  #   unique() -> orig_act_names

  output <- list_along(1:(length(all)-1))

  for(i in 1:(length(all)-1)) {
    if(all[i] %in% act_a) {
      #cli::cli_alert_info(glue::glue("Checking parallelism: {all[i]}"))
      output[[i]] <- discover_parallels_one_to_many(eventlog, all[i], act_b[-1:-which(act_b == all[i])])
      # output[[i]] <- discover_parallels_one_to_many(eventlog, all[i], act_b)
    }
  }
  output %>%
    bind_rows() %>%
    return()
}
