calculate_intermittent_relationship <- function(
    act_A,
    act_B,
    cases_with_A,
    cases_with_B,
    nr_cases_with_B,
    ev_log
){

  events_started_during_A <- cases_with_A[ TS > reference_timestamp_start &
           TS < reference_timestamp_end &
           LC == "start"]

  B_started_during_A <- events_started_during_A[AID == act_B]


  DURING_score <- (sum(B_started_during_A$CASE_COUNT)) / nr_cases_with_B

  return(DURING_score)
}
