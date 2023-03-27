calculate_relationships <- function(ev_log,
                                    skip_self_loops = FALSE){

  unique(ev_log[, .(CID, CASE_COUNT)]) -> case_count_list

  rel_df <- discover_parallels_complete(ev_log,
                                        unique(ev_log[!(AID %chin% c("START","END")),][["orig_name"]]),
                                        case_count_list = case_count_list)

  ## Retrieve activities that have duplicates

  duplicated_activities <- unique(ev_log[orig_name != new_act_name][["orig_name"]])


  par_thres <- 0.80

  if(!skip_self_loops && length(duplicated_activities) > 0){
    ## Check if there are immediate repeat activities

    renamed_entries <- discover_self_loops(
      ev_log,
      duplicated_activities,
      rel_df %>% filter(rel %in% c(RScoreDict$PARALLEL_IF_PRESENT, RScoreDict$ALWAYS_PARALLEL)),
      parallel_thres = par_thres)

    ## Merge self_loops into one entry with start and end
    if(renamed_entries %>% nrow > 0){
      ev_log %>%
        inner_join(renamed_entries,
                   by = c("CID",
                          "AIID")) %>%
        as_tibble() %>%
        mutate(AID = as.character(AID)) %>%
        mutate(AID := new_concatenated_name) %>%
        group_by(CID, AID, LC) %>%
        arrange(TS) %>%
        mutate(rep_occurence = row_number()) %>%
        mutate(AIID = min(AIID)) %>%
        filter((LC=="start" & rep_occurence == min(rep_occurence)) |
                 (LC=="complete" & rep_occurence == max(rep_occurence))) %>%
        ungroup() %>%
        as.data.table() -> adjusted_log

      ev_log %>%
        anti_join(renamed_entries, by = c("CID", "AIID")) -> old_log


      bind_rows(adjusted_log, old_log) %>%
        arrange(CID,TS) %>%
        as.data.table -> ev_log

    }

  ## Check again for parallel relationships, but now taking the self loops into account

  }
  rel_df_temp <- discover_parallels_complete(ev_log,
                                             ev_log %>%
                                               filter(!(AID %in% c("START","END"))) %>%
                                               pull(AID) %>% unique,
                                             case_count_list,
                                             ev_log %>%
                                               filter(AID != orig_name) %>%
                                               pull(AID) %>% unique)
  rel_df <- rel_df %>%
    bind_rows(rel_df_temp)


  ## Then we calculate the other scores
  # cases_with_act_memory <- obtain_case_ids_per_activity(ev_log)

  all_activities <- unique(ev_log[["AID"]]) %>%
    as.character() %>%
    setdiff(c("START","END"))

  if("START" %in% unique(ev_log[["AID"]])){
    all_activities <- c("START",all_activities)
  }

  rel_df_2 <- discover_R_sequence_relations(
    ev_log,
    all_activities,
    rel_df,
    parallel_thres = par_thres,
    case_count_list = case_count_list
  )

  if("END" %in% unique(ev_log[["AID"]])){
    always_end <- tibble(
      antecedent = all_activities,
      consequent = "END",
      rel="R2",
      score = 1,
      importance = 0.01
    )


    rel_df <- rel_df %>%
      bind_rows(rel_df_2) %>%
      bind_rows(always_end)
  } else{

    rel_df <- rel_df %>%
      bind_rows(rel_df_2)

  }

  act_frequency <- ev_log[, .(N_AIID = n_distinct(AIID), CASE_COUNT = first(CASE_COUNT)), by = .(AID, trace)][, freq := CASE_COUNT*N_AIID][, .(absolute_frequency = sum(freq)), by = AID]

  rel_df <- rel_df %>%
    left_join(act_frequency, by =c("antecedent"="AID")) %>%
    mutate(freq = absolute_frequency / sum(case_count_list$CASE_COUNT)) %>%
    mutate(freq = ifelse(is.na(freq),0,freq)) %>%
    mutate(or_importance = importance) %>%
    mutate(importance = importance * score * freq)

  rel_df <- rel_df %>%
    filter(!startsWith(antecedent, paste(consequent,"REP", sep = "_"))) %>%
    filter(!startsWith(consequent, paste(antecedent,"REP", sep = "_")))

  return(rel_df)
}
