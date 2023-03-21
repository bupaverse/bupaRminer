calculate_sometime_follows_relation <- function(
    act_A,
    act_B,
    nr_cases_with_A,
    nr_cases_with_B,
    fromA_event_log,
    fromB_event_log,
    cases_before_B,
    cases_before_A,
    nr_cases,
    ev_log,
    EF_results){
  
  cases_with_A <- ev_log[AID  == act_A]
  cases_with_A_and_B <- ev_log[CID  %chin% unique(cases_with_A$CID) & AID == act_B]
  nr_cases_with_A_and_B <- n_distinct(cases_with_A_and_B$CID)
  
  B_happens_after <- cases_before_B[AID == act_A]
  
  SOMETIMES_FOL_ab <- n_distinct(B_happens_after$CID) / nr_cases_with_A_and_B
  
  A_happens_after <- cases_before_A[AID == act_B]
  
  SOMETIMES_FOL_ba <- n_distinct(A_happens_after$CID) / nr_cases_with_A_and_B
  
  tribble(~antecedent,~consequent,~rel,~score,~importance,
          act_A, act_B, RScoreDict$MAYBE_EVENTUALLY_FOLLOWS, SOMETIMES_FOL_ab, EF_results$importance[1],
          act_B, act_A, RScoreDict$MAYBE_EVENTUALLY_FOLLOWS, SOMETIMES_FOL_ba, EF_results$importance[2])
}



calculate_sometime_follows_relation_backup <- function(
    act_A,
    act_B,
    nr_cases_with_A,
    nr_cases_with_B,
    fromA_event_log,
    fromB_event_log,
    cases_before_B,
    cases_before_A,
    nr_cases,
    ev_log,
    EF_results){

  B_happens_after <- cases_before_B[AID == act_A]
  A_happens_before <- fromA_event_log[AID == act_B]

  SOMETIMES_FOL_1 <- n_distinct(B_happens_after$CID) / nr_cases_with_B
  SOMETIMES_FOL_2 <- n_distinct(A_happens_before$CID) / nr_cases_with_A
  SOMETIMES_FOL_3 <- nr_cases_with_B / nr_cases

  SOMETIMES_FOL_ab <- SOMETIMES_FOL_1 * (1 - abs(SOMETIMES_FOL_2 - SOMETIMES_FOL_3))

  A_happens_after <- cases_before_A[AID == act_B]
  B_happens_before <- fromB_event_log[AID == act_A]

  SOMETIMES_FOL_1 <- n_distinct(A_happens_after$CID) / nr_cases_with_A
  SOMETIMES_FOL_2 <- n_distinct(B_happens_before$CID) / nr_cases_with_B
  SOMETIMES_FOL_3 <- nr_cases_with_A / nr_cases

  SOMETIMES_FOL_ba <- SOMETIMES_FOL_1 * (1 - abs(SOMETIMES_FOL_2 - SOMETIMES_FOL_3))

  tribble(~antecedent,~consequent,~rel,~score,~importance,
          act_A, act_B, RScoreDict$MAYBE_EVENTUALLY_FOLLOWS, SOMETIMES_FOL_ab, EF_results$importance[1],
          act_B, act_A, RScoreDict$MAYBE_EVENTUALLY_FOLLOWS, SOMETIMES_FOL_ba, EF_results$importance[2])
}
