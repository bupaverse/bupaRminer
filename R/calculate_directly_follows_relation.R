calculate_directly_follows_relation <- function(
    actA,
    actB,
    nr_cases_with_A,
    nr_cases_with_B,
    afterA_event_log,
    afterB_event_log,
    nr_cases,
    ev_log) {


  afterA_event_log[LC == "start" & AID != actA][order(CID, TS)] -> tmp_dt
  B_happens_directly_after <- tmp_dt[tmp_dt[,.I[1], by = CID]$V1][AID == actB]

  B_happens_directly_after_count <- n_distinct(B_happens_directly_after$CID)

  DF_score_ab <- (B_happens_directly_after_count) / nr_cases_with_A
  DF_importance_ab <- B_happens_directly_after_count / nr_cases

  afterB_event_log[LC == "start" & AID != actB][order(CID, TS)] -> tmp_dt
  A_happens_directly_after <- tmp_dt[tmp_dt[,.I[1], by = CID]$V1][AID == actA]

  A_happens_directly_after_count <- n_distinct(A_happens_directly_after$CID)

  DF_score_ba <- (A_happens_directly_after_count) / nr_cases_with_B
  DF_importance_ba <- A_happens_directly_after_count / nr_cases


  tribble(~antecedent,~consequent,~rel,~score,~importance,
          actA, actB, RScoreDict$DIRECTLY_FOLLOWS, DF_score_ab, DF_importance_ab,
          actB, actA, RScoreDict$DIRECTLY_FOLLOWS, DF_score_ba, DF_importance_ba)

}
