calculate_requirement_score <- function(
    act_A,
    act_B,
    cases_with_A,
    nr_cases,
    ev_log){

  activity_colname <- activity_id(ev_log)
  activity_instance_colname <- activity_instance_id(ev_log)
  case_colname <- case_id(ev_log)
  timestamp_colname <- timestamp(ev_log)
  lifecycle_colname <- lifecycle_id(ev_log)

  B_before_A <- cases_with_A %>%
    filter(
      !!sym(activity_colname) == act_B,
      !!sym(lifecycle_colname) == "complete",
      !!sym(timestamp_colname) <= reference_timestamp_start) %>%
    n_cases()

  REQ_score <- B_before_A / cases_with_A %>% n_cases()

  REQ_importance <- B_before_A / nr_cases

  REQ_return <- list(
    "score" = REQ_score,
    "importance" = REQ_importance
  )


  return(REQ_return)
}
