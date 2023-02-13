calculate_terminating_relationship <- function(
    act_A,
    act_B,
    cases_with_A,
    cases_with_B,
    interrupting_theta,
    ev_log
){

  activity_colname <- activity_id(ev_log)
  activity_instance_colname <- activity_instance_id(ev_log)
  case_colname <- case_id(ev_log)
  timestamp_colname <- timestamp(ev_log)
  lifecycle_colname <- lifecycle_id(ev_log)


  events_killing_A <- cases_with_A %>%
    filter(!!sym(timestamp_colname) >= reference_timestamp_end - interrupting_theta,
           !!sym(timestamp_colname) <= reference_timestamp_end + interrupting_theta,
           !!sym(lifecycle_colname) == "start")

  B_killing_A <- events_killing_A %>%
    as_tibble() %>%
    filter(!!sym(activity_colname) == act_B)

  INTERRUPTING_score <- (B_killing_A %>%
                           pull(!!sym(case_colname)) %>%
                           n_distinct) /
    (cases_with_B %>%
       pull(!!sym(case_colname)) %>%
       n_distinct)

  return(INTERRUPTING_score)
}
