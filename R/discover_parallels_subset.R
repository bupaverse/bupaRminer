
discover_parallels_subset <- function(eventlog, subset) {

  eventlog %>%
    filter(!(!!sym(activity_colname) %in% c("START","END"))) %>%
    pull(orig_name) %>%
    unique() -> orig_act_names

  output <- list_along(1:length(subset))

  for(A in 1:(length(subset))) {

    output[[A]] <- discover_parallels_one_to_many(eventlog, subset, orig_act_names[!(orig_act_names %in% subset[1:A])])

  }
  output %>%
    bind_rows() %>%
    return()
}
