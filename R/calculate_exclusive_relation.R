calculate_exclusive_relation <- function(
    act_A,
    act_B,
    nr_cases_with_A,
    nr_cases_with_B,
    nr_cases_with_A_B,
    exclusive_thres,
    nr_cases,
    ev_log
){

  if(nr_cases_with_A_B > (exclusive_thres * nr_cases_with_B) ){
    EXCL_score_ab <- 0
  } else {
    EXCL_score_ab <- 1 - ( nr_cases_with_A_B /  nr_cases_with_A  )
  }

  if(nr_cases_with_A_B > (exclusive_thres * nr_cases_with_A) ){
    EXCL_score_ba <- 0
  } else {
    EXCL_score_ba <- 1 - ( nr_cases_with_A_B / nr_cases_with_B )
  }


  EXCL_importance <- (1- nr_cases_with_A_B) / nr_cases

  tribble(~antecedent,~consequent,~rel,~score,~importance,
          act_A, act_B, RScoreDict$MUTUALLY_EXCLUSIVE, EXCL_score_ab, EXCL_importance,
          act_B, act_A, RScoreDict$MUTUALLY_EXCLUSIVE, EXCL_score_ba, EXCL_importance)

}
