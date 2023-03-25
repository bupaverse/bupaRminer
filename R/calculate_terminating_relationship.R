calculate_terminating_relationship <- function(
    act_A,
    act_B,
    cases_with_A,
    cases_with_B,
    nr_cases_with_B,
    interrupting_theta,
    ev_log
){




  events_killing_A <- cases_with_A[ TS >= reference_timestamp_end - interrupting_theta &
                                      TS <= reference_timestamp_end + interrupting_theta & LC == "start"]


  B_killing_A <- events_killing_A[AID == act_B]

  INTERRUPTING_score <- sum(B_killing_A$CASE_COUNT) / nr_cases_with_B

  return(INTERRUPTING_score)
}
