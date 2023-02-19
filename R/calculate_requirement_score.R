calculate_requirement_score <- function(
    act_A,
    act_B,
    cases_with_A,
    cases_with_B,
    nr_cases,
    ev_log){

  activity_colname <- activity_id(ev_log)
  timestamp_colname <- timestamp(ev_log)
  lifecycle_colname <- lifecycle_id(ev_log)

  B_before_A <- cases_with_A %>%
    filter(
      !!sym(activity_colname) == act_B,
      !!sym(lifecycle_colname) == "complete",
      !!sym(timestamp_colname) <= reference_timestamp_start)

  A_before_B <- cases_with_B %>%
    filter(
      !!sym(activity_colname) == act_A,
      !!sym(lifecycle_colname) == "complete",
      !!sym(timestamp_colname) <= reference_timestamp_start)

  REQ_score_ab <- n_distinct(B_before_A$CID) / n_distinct(cases_with_A$CID)
  REQ_importance_ab <- n_distinct(B_before_A$CID) / nr_cases
  REQ_score_ba <- n_distinct(A_before_B$CID) / n_distinct(cases_with_B$CID)
  REQ_importance_ba <- n_distinct(A_before_B$CID) / nr_cases

  tribble(~antecedent,~consequent,~rel,~score,~importance,
                        act_A, act_B, RScoreDict$REQUIRES, REQ_score_ab, REQ_importance_ab,
                        act_B, act_A, RScoreDict$REQUIRES, REQ_score_ba, REQ_importance_ba)

}
