


discover_parallels_complete <- function(eventlog) {

  eventlog %>%
    filter(!(.data[[activity_id(eventlog)]] %in% c("START","END"))) %>%
    pull(orig_name) %>%
    unique() -> orig_act_names

  output <- list_along(1:length(orig_act_names))

  for(A in 1:(length(orig_act_names)-1)) {

    output[[A]] <- discover_parallels_one_to_many(eventlog, orig_act_names[A], orig_act_names[(-1:-A)])

      }
  output %>%
    bind_rows() %>%
    return()
}
