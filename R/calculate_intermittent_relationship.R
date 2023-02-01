calculate_intermittent_relationship <- function(
    act_A,
    act_B,
    cases_with_A,
    cases_with_B,
    ev_log
){
  activity_colname <- activity_id(ev_log)
  activity_instance_colname <- activity_instance_id(ev_log)
  case_colname <- case_id(ev_log)
  timestamp_colname <- timestamp(ev_log)
  lifecycle_colname <- lifecycle_id(ev_log)


  events_started_during_A <- cases_with_A %>%
    filter(!!sym(timestamp_colname) > reference_timestamp_start,
           !!sym(timestamp_colname) < reference_timestamp_end,
           !!sym(lifecycle_colname) == "start")

  B_started_during_A <- events_started_during_A %>%
    as_tibble() %>%
    filter(!!sym(activity_colname) == act_B)

  DURING_score <- (B_started_during_A %>%
                     pull(!!sym(case_colname)) %>%
                     n_distinct) /
    (cases_with_B %>%
       pull(!!sym(case_colname)) %>%
       n_distinct)

  return(DURING_score)
}
