calculate_eventually_follows_relation <- function(
    actA,
    actB,
    cases_with_A,
    cases_with_B,
    afterA_event_log,
    afterB_event_log,
    nr_cases,
    ev_log){

  B_happens_after <- afterA_event_log[AID == actB & LC == "start",]
  A_happens_after <- afterB_event_log[AID == actA & LC == "start",]

  EF_score_ab <- n_distinct(B_happens_after$CID)  / n_distinct(cases_with_A$CID)
  EF_importance_ab <- n_distinct(B_happens_after$CID) / nr_cases
  EF_score_ba <- n_distinct(A_happens_after$CID)  / n_distinct(cases_with_B$CID)
  EF_importance_ba <- n_distinct(A_happens_after$CID) / nr_cases

  tribble(~antecedent,~consequent,~rel,~score,~importance,
          actA, actB, RScoreDict$EVENTUALLY_FOLLOWS, EF_score_ab, EF_importance_ab,
          actB, actA, RScoreDict$EVENTUALLY_FOLLOWS, EF_score_ba, EF_importance_ba)


}
