
calculate_relationship_scores <- function(ev_log){

  activity_colname <- activity_id(ev_log)
  activity_instance_colname <- activity_instance_id(ev_log)
  case_colname <- case_id(ev_log)
  timestamp_colname <- timestamp(ev_log)
  lifecycle_colname <- lifecycle_id(ev_log)

  ## We compute the correlations between acitivities
  ## based on how ften they repeay in the same case.
  ## If they often repeat together, then we assume
  ## that they are part of a repeating block.

  # repeat_correlations <- ev_log %>%
  #   as_tibble() %>% filter(!!sym(lifecycle_colname) == "start") %>%
  #   count(!!sym(case_colname), orig_name) %>%
  #   group_by(orig_name) %>%
  #   filter(max(n) > 1) %>%
  #   ungroup %>%
  #   pivot_wider(names_from = orig_name,
  #               values_from = n,
  #               values_fill = 0) %>%
  #   select(-!!sym(case_colname)) %>%
  #   cor %>%
  #   as_tibble() %>%
  #   mutate(., antecedent = colnames(.)) %>%
  #   select(antecedent, everything())  %>%
  #   pivot_longer(
  #     cols = -c(antecedent),
  #     names_to = "consequent",
  #     values_to = "score"
  #   ) %>%
  #   filter(antecedent != consequent) %>%
  #   mutate(rel = RScoreDict$LOOP_BLOCK)

  ## First we must establish parallel activities as they clutter
  ## our perception of the sequence of activities.
  rel_df <- discover_parallels_from_log(ev_log,
                                        ev_log %>%
                                          filter(!(!!sym(activity_colname) %in% c("START","END"))) %>%
                                          pull(orig_name) %>%
                                          unique)

  ## Retrieve activities that have duplicates
  duplicated_activities <- ev_log %>%
    as_tibble() %>%
    count(orig_name,
          new_act_name) %>%
    filter(as.character(orig_name) != as.character(new_act_name)) %>%
    pull(orig_name) %>%
    unique()


  ## Check if there are immediate repeat activities
  par_thres <- 0.80

  renamed_entries <- discover_self_loops(
    ev_log,
    duplicated_activities,
    rel_df %>% filter(rel %in% c(RScoreDict$PARALLEL_IF_PRESENT, RScoreDict$ALWAYS_PARALLEL)),
    parallel_thres = par_thres)

  ## Merge self_loops into one entry with start and end
  if(renamed_entries %>% nrow > 0){
    ev_log <- ev_log %>%
      left_join(renamed_entries,
                by = c(case_colname, activity_instance_colname)) %>%
      as_tibble() %>%
      mutate(!!sym(activity_colname) := as.character(!!sym(activity_colname))) %>%
      mutate(!!sym(activity_colname) := ifelse(is.na(new_concatenated_name),
                                               !!sym(activity_colname),
                                               new_concatenated_name)) %>%
      group_by(!!sym(case_colname), !!sym(activity_colname),!!sym(lifecycle_colname)) %>%
      arrange(!!sym(timestamp_colname)) %>%
      mutate(rep_occurence = row_number()) %>%
      mutate(!!sym(activity_instance_colname) := paste(!!sym(activity_instance_colname),collapse="_")) %>%
      filter((!!sym(lifecycle_colname)=="start" & rep_occurence == min(rep_occurence)) |
               (!!sym(lifecycle_colname)=="complete" & rep_occurence == max(rep_occurence))) %>%
      ungroup() %>%
      re_map(mapping(ev_log))
  }

  ## Check again for parallel relationships, but now taking the self loops into account
  rel_df_temp <- discover_parallels_from_log(ev_log,
                                             ev_log %>%
                                               filter(!(!!sym(activity_colname) %in% c("START","END"))) %>%
                                               pull(!!sym(activity_colname)) %>% unique,
                                             ev_log %>%
                                               filter(!!sym(activity_colname) != orig_name) %>%
                                               pull(!!sym(activity_colname)) %>% unique)

  rel_df <- rel_df %>%
    bind_rows(rel_df_temp)

  ## Then we calculate the other scores
  cases_with_act_memory <- obtain_case_ids_per_activity(ev_log)


  all_activities <- ev_log %>%
    activities %>%
    filter(!(!!sym(activity_colname) %in% c("START","END"))) %>%
    pull(!!sym(activity_colname)) %>%
    as.character() %>%
    unique()

  all_activities <- c("START",all_activities)

  rel_df_2 <- discover_R_sequence_relations(
    ev_log,
    all_activities,
    rel_df,
    parallel_thres = 0.90,
    cases_per_act_memory = cases_with_act_memory
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

  act_frequency <- ev_log %>%
    activities

  rel_df <- rel_df %>%
    left_join(act_frequency %>% select(!!sym(activity_colname), absolute_frequency), by =c("antecedent"=activity_colname)) %>%
    mutate(freq = absolute_frequency / ev_log %>% n_cases) %>%
    mutate(freq = ifelse(is.na(freq),0,freq)) %>%
    mutate(or_importance = importance) %>%
    mutate(importance = importance * score * freq)

  rel_df <- rel_df %>%
    filter(!startsWith(antecedent, paste(consequent,"REP", sep = "_"))) %>%
    filter(!startsWith(consequent, paste(antecedent,"REP", sep = "_")))

  return(rel_df)
}
