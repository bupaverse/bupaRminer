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
  chance_of_a <- nr_cases_with_A / nr_cases
  chance_of_b <- nr_cases_with_B / nr_cases
  
  if(nr_cases_with_A_B > (exclusive_thres * nr_cases * chance_of_a * chance_of_b) ){
    EXCL_score_ab <- 0
  } else {
    EXCL_score_ab <- 1 - ( nr_cases_with_A_B /  nr_cases_with_A  )
  }

  if(nr_cases_with_A_B > (exclusive_thres * nr_cases * chance_of_a * chance_of_b) ){
    EXCL_score_ba <- 0
  } else {
    EXCL_score_ba <- 1 - ( nr_cases_with_A_B / nr_cases_with_B )
  }


  EXCL_importance <- (1- nr_cases_with_A_B) / nr_cases

  tribble(~antecedent,~consequent,~rel,~score,~importance,
          act_A, act_B, RScoreDict$MUTUALLY_EXCLUSIVE, EXCL_score_ab, EXCL_importance,
          act_B, act_A, RScoreDict$MUTUALLY_EXCLUSIVE, EXCL_score_ba, EXCL_importance)

}
