calculate_eventually_follows_relation <- function(
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

  B_happens_after <- afterA_event_log %>%
    filter(!!sym(activity_colname) == actB,
           !!sym(lifecycle_colname) == "start")

  score <- (B_happens_after %>%
              n_cases) /
    (cases_with_A %>%
       pull(!!sym(case_colname)) %>%
       n_distinct)

  EF_importance <- (B_happens_after %>%
                      n_cases) / nr_cases

  EF_return <- list(
    "score" = score,
    "importance" = EF_importance
  )

  return(EF_return)
}
