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

  occurs_together <- n_distinct(cases_with_A[AID == act_B]$CID)

  expected_together_ab <- n_distinct(cases_with_B$CID)
  expected_together_ba <- n_distinct(cases_with_A$CID)

  if(occurs_together > (exclusive_thres * expected_together_ab) ){
    EXCL_score_ab <- 0
  } else {
    EXCL_score_ab <- 1 - ( occurs_together /  expected_together_ba  )
  }

  if(occurs_together > (exclusive_thres * expected_together_ba) ){
    EXCL_score_ba <- 0
  } else {
    EXCL_score_ba <- 1 - ( occurs_together / expected_together_ab )
  }


  EXCL_importance <- (1- occurs_together) / nr_cases
  EXCL_importance

  tribble(~antecedent,~consequent,~rel,~score,~importance,
          act_A, act_B, RScoreDict$MUTUALLY_EXCLUSIVE, EXCL_score_ab, EXCL_importance,
          act_B, act_A, RScoreDict$MUTUALLY_EXCLUSIVE, EXCL_score_ba, EXCL_importance)

}
