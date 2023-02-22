
calculate_relationship_scores <- function(ev_log){

  orig_mapping <- mapping(ev_log)

  new_mapping <- list(case_identifier = "CID",
                      activity_identifier = "AID",
                      activity_instance_identifier = "AIID",
                      timestamp_identifier = "TS",
                      lifecycle_identifier = "LC",
                      resource_identifier = resource_id(ev_log))
  class(new_mapping) <- c("eventlog_mapping", class(new_mapping))

  ev_log %>%
    rename(AID = .data[[activity_id(ev_log)]],
           AIID = .data[[activity_instance_id(ev_log)]],
           CID = .data[[case_id(ev_log)]],
           TS = .data[[timestamp(ev_log)]],
           LC =.data[[lifecycle_id(ev_log)]]) %>%
    re_map(new_mapping) -> ev_log

  ev_log_copy <- ev_log

  ev_log <- as.data.table(ev_log)

  rel_df <- discover_parallels_complete(ev_log,
                                        unique(ev_log[AID %chin% c("START","END"),][["orig_name"]]))


  ## Retrieve activities that have duplicates

  duplicated_activities <- unique(ev_log[orig_name != new_act_name][["orig_name"]])


  ## Check if there are immediate repeat activities
  par_thres <- 0.80

  renamed_entries <- discover_self_loops(
    ev_log,
    duplicated_activities,
    rel_df %>% filter(rel %in% c(RScoreDict$PARALLEL_IF_PRESENT, RScoreDict$ALWAYS_PARALLEL)),
    parallel_thres = par_thres)

  ## Merge self_loops into one entry with start and end
  if(renamed_entries %>% nrow > 0){
    ev_log %>%
      inner_join(renamed_entries,
                 by = c(case_colname, activity_instance_colname)) %>%
      as_tibble() %>%
      mutate(!!sym(activity_colname) := as.character(!!sym(activity_colname))) %>%
      mutate(!!sym(activity_colname) := new_concatenated_name) %>%
      group_by(!!sym(case_colname), !!sym(activity_colname),!!sym(lifecycle_colname)) %>%
      arrange(!!sym(timestamp_colname)) %>%
      mutate(rep_occurence = row_number()) %>%
      mutate(!!sym(activity_instance_colname) := paste(!!sym(activity_instance_colname),collapse="_")) %>%
      filter((!!sym(lifecycle_colname)=="start" & rep_occurence == min(rep_occurence)) |
               (!!sym(lifecycle_colname)=="complete" & rep_occurence == max(rep_occurence))) %>%
      ungroup() -> adjusted_log

    ev_log %>%
      anti_join(renamed_entries) -> old_log


    bind_rows(adjusted_log, old_log) %>%
      re_map(mapping(ev_log)) -> ev_log
  }

  ## Check again for parallel relationships, but now taking the self loops into account
  rel_df_temp <- discover_parallels_complete(ev_log,
                                             ev_log %>%
                                               filter(!(AID %in% c("START","END"))) %>%
                                               pull(AID) %>% unique,
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

  all_activities <- c("START",all_activities)

  rel_df_2 <- discover_R_sequence_relations(
    ev_log,
    all_activities,
    rel_df,
    parallel_thres = 0.90
  )

  ## There is always an end
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

  act_frequency <- ev_log[, .(absolute_frequency = n_distinct(AIID)), by = AID]

  rel_df <- rel_df %>%
    left_join(act_frequency, by =c("antecedent"="AID")) %>%
    mutate(freq = absolute_frequency / n_distinct(ev_log$CID)) %>%
    mutate(freq = ifelse(is.na(freq),0,freq)) %>%
    mutate(or_importance = importance) %>%
    mutate(importance = importance * score * freq)

  rel_df <- rel_df %>%
    filter(!startsWith(antecedent, paste(consequent,"REP", sep = "_"))) %>%
    filter(!startsWith(consequent, paste(antecedent,"REP", sep = "_")))

  return(rel_df)
}
