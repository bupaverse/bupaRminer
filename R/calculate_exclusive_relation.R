calculate_exclusive_relation <- function(
    act_A,
    act_B,
    cases_with_A,
    cases_with_B,
    exclusive_thres,
    nr_cases,
    ev_log
){

  activity_colname <- activity_id(ev_log)
  activity_instance_colname <- activity_instance_id(ev_log)
  case_colname <- case_id(ev_log)
  timestamp_colname <- timestamp(ev_log)
  lifecycle_colname <- lifecycle_id(ev_log)

  occurs_together <- cases_with_A %>%
    filter(!!sym(activity_colname) == act_B) %>%
    n_cases

  expected_together <- ( cases_with_B %>%
                           n_cases )

  if(occurs_together > (exclusive_thres * expected_together) ){
    EXCL_score <- 0
  } else {
    EXCL_score <- 1 - ( occurs_together / ( cases_with_A %>% n_cases ) )
  }

  EXCL_importance <- (1- occurs_together) / nr_cases

  EXCL_return <- list(
    "score" = EXCL_score,
    "importance" = EXCL_importance
  )

  return(EXCL_return)
}
