discover_self_loops <- function(
    ev_log,
    duplicated_activities,
    rel_par_df,
    parallel_thres = 0.95){

  ## Check if there are immediate repeat activities

  renamed_entries <- tibble()

  activity_colname <- activity_id(ev_log)
  activity_instance_colname <- activity_instance_id(ev_log)
  case_colname <- case_id(ev_log)
  timestamp_colname <- timestamp(ev_log)
  lifecycle_colname <- lifecycle_id(ev_log)

  nr_cases <- ev_log %>%
    n_cases

  for(dup_act in duplicated_activities){

    print(dup_act)

    par_relationships <- rel_par_df %>%
      filter(antecedent == dup_act,
             score >= parallel_thres) %>%
      pull(consequent)

    cases_with_repeated_A <- ev_log %>%
      filter_activity_presence(dup_act) %>%
      as_tibble() %>%
      mutate(reference_timestamp = ifelse(as.character(orig_name) == as.character(dup_act),
                                          !!sym(timestamp_colname),
                                          NA)) %>%
      group_by(!!sym(case_colname)) %>%
      mutate(reference_timestamp_first_start = min(reference_timestamp, na.rm = TRUE),
             reference_timestamp_last_end = max(reference_timestamp, na.rm = TRUE)) %>%
      filter(!(orig_name %in% par_relationships)) %>%
      mutate(relevant_repeat = ifelse(orig_name == dup_act, is_repeat - 1, 0)) %>%
      mutate(has_repeated_A = (sum(relevant_repeat) > 0)) %>%
      ungroup() %>%
      filter(has_repeated_A == TRUE) %>%
      re_map(mapping(ev_log))

    ## Autorepetitions
    cases_between_As <- cases_with_repeated_A %>%
      as_tibble() %>%
      filter(!!sym(timestamp_colname) >= reference_timestamp_first_start,
             !!sym(timestamp_colname) <= reference_timestamp_last_end)

    autorepetitions_of_A <- cases_between_As %>%
      filter(!!sym(lifecycle_colname) == "start") %>%
      mutate(is_not_act_A = (orig_name != dup_act)) %>%
      group_by(!!sym(case_colname)) %>%
      arrange(!!sym(case_colname), !!sym(timestamp_colname)) %>%
      mutate(acts_in_between = cumsum(is_not_act_A)) %>%
      mutate(part_of_chain = (lag(acts_in_between) == acts_in_between)) %>%
      fill(part_of_chain, .direction = "up") %>%
      mutate(start_of_chain = (part_of_chain == TRUE) & ( lag(part_of_chain) == FALSE | is.na(lag(part_of_chain))) ) %>%
      mutate(chain_nr = cumsum(start_of_chain)) %>%
      ungroup() %>%
      filter(part_of_chain == TRUE) %>%
      mutate(new_concatenated_name = ifelse(part_of_chain==TRUE,
                                            paste(orig_name,"REP",chain_nr, sep = "_"),
                                            as.character(orig_name)))

    renamed_entry <- autorepetitions_of_A %>%
      select(!!sym(case_colname),
             !!sym(activity_instance_colname),
             new_concatenated_name)

    renamed_entries <- renamed_entries %>%
      bind_rows(renamed_entry)
  }

  return(renamed_entries)
}
