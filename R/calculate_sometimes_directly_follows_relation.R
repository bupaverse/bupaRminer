
calculate_sometimes_directly_follows_relation <- function(
    act_A,
    act_B,
    nr_cases_with_A,
    nr_cases_with_B,
    afterA_event_log,
    afterB_event_log,
    cases_before_B,
    cases_before_A,
    par_relationships_B,
    par_relationships_A,
    nr_cases,
    ev_log,
    DF_results,
    case_count_list){

  ## What is the chance that A just completed
  ## if we know that B is about to start
  ## (The change that there is an A_complete
  ## without any events between A_complete and B_start
  ## given that there is a B_start)

  cases_before_B[AID != act_B & LC == "complete" & !(AID %chin% par_relationships_B),][order(CID,TS, decreasing = TRUE),] -> tmp_dt

  A_happens_directly_before <- tmp_dt[tmp_dt[,.I[1], by = CID]$V1][AID == act_A]

  ## If B happens, how often does it happen right after A
  SOMETIMES_DIRECT_ab <- N_CASES(A_happens_directly_before$CID, case_count_list) / nr_cases_with_B


  cases_before_A[AID != act_A & LC == "complete" & !(AID %chin% par_relationships_A),][order(CID,TS, decreasing = TRUE),] -> tmp_dt

  B_happens_directly_before <- tmp_dt[tmp_dt[,.I[1], by = CID]$V1][AID == act_B]

  ## If B happens, how often does it happen right after A
  SOMETIMES_DIRECT_ba <- N_CASES(B_happens_directly_before$CID, case_count_list) / nr_cases_with_A

  tribble(~antecedent,~consequent,~rel,~score,~importance,
          act_A, act_B, RScoreDict$MAYBE_DIRECTLY_FOLLOWS, SOMETIMES_DIRECT_ab, DF_results$importance[1],
          act_B, act_A, RScoreDict$MAYBE_DIRECTLY_FOLLOWS, SOMETIMES_DIRECT_ba, DF_results$importance[2])
}




calculate_sometimes_directly_follows_relation_backup <- function(
    act_A,
    act_B,
    nr_cases_with_A,
    nr_cases_with_B,
    afterA_event_log,
    afterB_event_log,
    cases_before_B,
    cases_before_A,
    par_relationships_B,
    par_relationships_A,
    nr_cases,
    ev_log,
    DF_results){


  cases_before_B[LC == "complete" & !(AID %chin% par_relationships_B),][order(CID,TS, decreasing = TRUE),] -> tmp_dt

  A_happens_directly_before <- tmp_dt[tmp_dt[,.I[1], by = CID]$V1][AID == act_A]

  ## If B happens, how often does it happen right after A
  SOMETIMES_DIRECT_1 <- n_distinct(A_happens_directly_before$CID) / nr_cases_with_B

  ## If A happens, how often is it directly followed
  ## by B
  afterA_event_log[LC == "start" & AID != act_A][order(CID, TS)] -> tmp_dt
  B_happens_directly_after <- tmp_dt[tmp_dt[,.I[1], by = CID]$V1][AID == act_B]

  SOMETIMES_DIRECT_2 <- n_distinct(B_happens_directly_after$CID) / nr_cases_with_A
  ## How often do we expect B
  SOMETIMES_DIRECT_3 <-  nr_cases_with_B / nr_cases

  ## The first factor is high when A is often the activity before B
  ## the second factor is high when the number of times we observe B right after A
  ## is similar to the number of times we observe B at all.
  SOMETIMES_DIRECT_ab <- SOMETIMES_DIRECT_1 * (1 - abs(SOMETIMES_DIRECT_2 - SOMETIMES_DIRECT_3))



  cases_before_A[LC == "complete" & !(AID %chin% par_relationships_A),][order(CID,TS, decreasing = TRUE),] -> tmp_dt

  B_happens_directly_before <- tmp_dt[tmp_dt[,.I[1], by = CID]$V1][AID == act_B]

  ## If B happens, how often does it happen right after A
  SOMETIMES_DIRECT_1 <- n_distinct(B_happens_directly_before$CID) / nr_cases_with_A

  ## If A happens, how often is it directly followed
  ## by B
  afterB_event_log[LC == "start" & AID != act_B][order(CID, TS)] -> tmp_dt
  A_happens_directly_after <- tmp_dt[tmp_dt[,.I[1], by = CID]$V1][AID == act_A]

  SOMETIMES_DIRECT_2 <- n_distinct(A_happens_directly_after$CID) / nr_cases_with_B
  ## How often do we expect B
  SOMETIMES_DIRECT_3 <-  nr_cases_with_A / nr_cases

  ## The first factor is high when A is often the activity before B
  ## the second factor is high when the number of times we observe B right after A
  ## is similar to the number of times we observe B at all.
  SOMETIMES_DIRECT_ba <- SOMETIMES_DIRECT_1 * (1 - abs(SOMETIMES_DIRECT_2 - SOMETIMES_DIRECT_3))


  tribble(~antecedent,~consequent,~rel,~score,~importance,
          act_A, act_B, RScoreDict$MAYBE_DIRECTLY_FOLLOWS, SOMETIMES_DIRECT_ab, DF_results$importance[1],
          act_B, act_A, RScoreDict$MAYBE_DIRECTLY_FOLLOWS, SOMETIMES_DIRECT_ba, DF_results$importance[2])
}
