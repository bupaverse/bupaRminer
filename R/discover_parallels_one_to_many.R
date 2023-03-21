

discover_parallels_one_to_many <- function(eventlog,
                                           one,
                                           many) {
  if(length(many) == 0) {
    return(tibble())
  }

  act_A <- one

  eventlog[AID == one & LC == "start",.(CID, reference_timestamp_start = TS)] -> events_A

  merge(eventlog, events_A, by = "CID") -> cases_with_A


  output <- list_along(many)

  for(B in 1:length(many)) {

    act_B <- as.character(many[B])

    unique(cases_with_A[AID == act_B][["CID"]]) -> case_list_with_A_B

    cases_with_A[CID %in% case_list_with_A_B] -> cases_with_A_and_B

    if(cases_with_A_and_B %>% nrow() == 0){
      full_par_score <- 0
      par_score <- 0
    } else {


      n_distinct(cases_with_A_and_B[TS >= reference_timestamp_start & AID == act_B & LC == "complete"][["CID"]]) -> A_starts_before_B_ends

      n_distinct(cases_with_A_and_B[TS <= reference_timestamp_start & AID == act_B & LC == "start"][["CID"]]) -> A_starts_after_B_starts


      par_score <- 1 - ( abs(A_starts_before_B_ends - A_starts_after_B_starts) / length(case_list_with_A_B))

      ## We lower the R score as A and B are less likely to occur together
      modifier <- ( length(case_list_with_A_B) ) / (n_distinct(cases_with_A$CID))
      full_par_score <- par_score * modifier
    }

    new_row <- tibble(
      "antecedent" = c(act_A, act_B, act_A, act_B),
      "consequent" = c(act_B, act_A, act_B, act_A),
      "rel" = c(RScoreDict$ALWAYS_PARALLEL, RScoreDict$ALWAYS_PARALLEL, RScoreDict$PARALLEL_IF_PRESENT, RScoreDict$PARALLEL_IF_PRESENT),
      "score" = c(full_par_score, full_par_score, par_score, par_score),
      "importance" = c(full_par_score, full_par_score, par_score, par_score))

    output[[B]] <- new_row
  }

  output %>%
    bind_rows() %>%
    return()
}
