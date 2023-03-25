calculate_requirement_score <- function(
    act_A,
    act_B,
    cases_with_A,
    cases_with_B,
    nr_cases_with_A,
    nr_cases_with_B,
    nr_cases,
    ev_log,
    case_count_list){



  B_before_A <- cases_with_A[AID == act_B & LC == "complete" & TS <= reference_timestamp_start]
  A_before_B <- cases_with_B[AID == act_A & LC == "complete" & TS <= reference_timestamp_start]

  REQ_score_ab <- N_CASES(B_before_A$CID, case_count_list) / nr_cases_with_A
  REQ_importance_ab <- N_CASES(B_before_A$CID, case_count_list) / nr_cases
  REQ_score_ba <- N_CASES(A_before_B$CID, case_count_list) / nr_cases_with_B
  REQ_importance_ba <- N_CASES(A_before_B$CID, case_count_list) / nr_cases

  tribble(~antecedent,~consequent,~rel,~score,~importance,
                        act_A, act_B, RScoreDict$REQUIRES, REQ_score_ab, REQ_importance_ab,
                        act_B, act_A, RScoreDict$REQUIRES, REQ_score_ba, REQ_importance_ba)

}
