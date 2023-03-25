calculate_eventually_follows_relation <- function(
    actA,
    actB,
    nr_cases_with_A,
    nr_cases_with_B,
    afterA_event_log,
    afterB_event_log,
    nr_cases,
    ev_log,
    case_count_list){

  B_happens_after <- afterA_event_log[AID == actB & LC == "start",]
  A_happens_after <- afterB_event_log[AID == actA & LC == "start",]

  EF_score_ab <- N_CASES(B_happens_after$CID, case_count_list)  / nr_cases_with_A
  EF_importance_ab <- N_CASES(B_happens_after$CID, case_count_list) / nr_cases
  EF_score_ba <- N_CASES(A_happens_after$CID, case_count_list)  / nr_cases_with_B
  EF_importance_ba <- N_CASES(A_happens_after$CID, case_count_list) / nr_cases

  tribble(~antecedent,~consequent,~rel,~score,~importance,
          actA, actB, RScoreDict$EVENTUALLY_FOLLOWS, EF_score_ab, EF_importance_ab,
          actB, actA, RScoreDict$EVENTUALLY_FOLLOWS, EF_score_ba, EF_importance_ba)


}
