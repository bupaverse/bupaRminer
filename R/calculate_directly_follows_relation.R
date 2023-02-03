calculate_directly_follows_relation <- function(
    actA,
    actB,
    cases_with_A,
    afterA_event_log,
    nr_cases,
    ev_log){

  activity_colname <- activity_id(ev_log)
  activity_instance_colname <- activity_instance_id(ev_log)
  case_colname <- case_id(ev_log)
  timestamp_colname <- timestamp(ev_log)
  lifecycle_colname <- lifecycle_id(ev_log)

  score <- 0

  B_happens_directly_after <- afterA_event_log %>%
    as_tibble() %>%
    filter(!!sym(lifecycle_colname) == "start",
           !!sym(activity_colname) != actA) %>%
    arrange(!!sym(case_colname), !!sym(timestamp_colname)) %>%
    group_by(!!sym(case_colname)) %>%
    filter(row_number() == 1) %>%
    filter(!!sym(activity_colname) == actB)

  B_happens_directly_after_count <- B_happens_directly_after %>%
    pull(!!sym(case_colname)) %>% n_distinct()

  score <- (B_happens_directly_after_count) /
    (cases_with_A %>%
       pull(!!sym(case_colname)) %>%
       n_distinct)

  DF_importance <- B_happens_directly_after_count / nr_cases

  DF_return <- list(
    "score" = score,
    "importance" = DF_importance
  )

  return(DF_return)
}
