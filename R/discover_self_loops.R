discover_self_loops <- function(
    ev_log,
    duplicated_activities,
    rel_par_df,
    parallel_thres = 0.95){

  ## Check if there are immediate repeat activities

  renamed_entries <- tibble()

  nr_cases <- n_distinct(ev_log$CID)


  for(dup_act in duplicated_activities){

    #cli::cli_alert_info(glue::glue("Checking self-loops: {dup_act}"))

    par_relationships <- rel_par_df %>%
      filter(antecedent == dup_act,
             score >= parallel_thres) %>%
      pull(consequent)


    cases_with_dup_act <- unique(ev_log[AID == dup_act][["CID"]])


    ev_log[AID == dup_act][, .(reference_timestamp_first_start = min(TS),
                               reference_timestamp_last_end = max(TS)),
                           by = CID] -> reference_TS

    tmp <- merge(ev_log, reference_TS, by = "CID")

    tmp[orig_name == dup_act & is_repeat > 1,][["CID"]] -> case_ids_with_repeated_A

    cases_between_As <- tmp[CID %in% case_ids_with_repeated_A & TS >= reference_timestamp_first_start & TS <= reference_timestamp_last_end]

    suppressWarnings({

      cases_between_As[LC == "start", is_not_act_A := orig_name != dup_act][order(CID, TS), acts_in_between := cumsum(is_not_act_A), by = CID][,
                                                                                                                                               lag_acts_in_between := lag(acts_in_between), by = CID][
                                                                                                                                                 , part_of_chain := lag_acts_in_between == acts_in_between, by = CID
                                                                                                                                               ] -> tmp2

      tmp2 %>%
        fill(part_of_chain, .direction = "up") -> tmp3


      tmp3[, start_of_chain := (part_of_chain == TRUE) & ( lag(part_of_chain) == FALSE | is.na(lag(part_of_chain))),
           by = CID][,
                     chain_nr := cumsum(start_of_chain), by = CID][part_of_chain == TRUE & !is.na(part_of_chain)][, new_concatenated_name := ifelse(part_of_chain==TRUE,
                                                                                                                                                    paste(orig_name,"REP",chain_nr, sep = "_"),
                                                                                                                                                    as.character(orig_name))] -> tmp4
    })

    renamed_entry <- tmp4[,.(CID, AIID,new_concatenated_name)]

    renamed_entries <- renamed_entries %>%
      bind_rows(renamed_entry)
  }

  return(renamed_entries)
}
