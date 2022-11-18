source("sandbox/discovery_functions.R")
I_WANT_INTERRUPTIONS <- FALSE

rel_notebook_df <- assigned_rel_df %>%
  filter(!(
    rel == RScoreDict$ALWAYS_PARALLEL &
      antecedent %in% c("START", "END") &
      consequent %in% c("START", "END")
  )) %>%
  filter(antecedent != "END",
         consequent != "START") %>%
  mutate(
    score = ifelse(consequent == "END", 0, score),
    importance = ifelse(consequent == "END", 0, importance)
  )

rel_df <- rel_notebook_df

rel_df <- rel_df %>%
  separate(
    antecedent,
    into = c("orig_antecedent", "antec_counter"),
    sep = "_REP_",
    remove = FALSE
  ) %>%
  separate(
    consequent,
    into = c("orig_consequent", "conseq_counter"),
    sep = "_REP_",
    remove = FALSE
  )

repeat_correlations <- event_log %>%
  as_tibble() %>% 
  filter(!!sym(lifecycle_colname) == "start") %>%
  count(!!sym(case_colname), orig_name) %>%
  group_by(orig_name) %>%
  filter(max(n) > 1)

current_dict <- list()

if (repeat_correlations %>% nrow > 0 & repeat_correlations$orig_name %>% unique %>% length > 1) {
  repeat_correlations <- repeat_correlations %>%
    ungroup %>%
    pivot_wider(
      names_from = orig_name,
      values_from = n,
      values_fill = 0
    ) %>%
    select(-!!sym(case_colname)) %>%
    cor %>%
    as_tibble() %>%
    mutate(., antecedent = colnames(.)) %>%
    select(antecedent, everything())  %>%
    pivot_longer(
      cols = -c(antecedent),
      names_to = "consequent",
      values_to = "score"
    ) %>%
    filter(antecedent != consequent) %>%
    mutate(rel = RScoreDict$LOOP_BLOCK)
  
  
  loop_blocks <- repeat_correlations %>%
    filter(score > 0) %>%
    mutate(loop_block_id = 0)
  
  cluster_counter <- 1
  while (loop_blocks %>% filter(loop_block_id == 0) %>% nrow > 0) {
    most_connected_antecedent <- loop_blocks %>%
      filter(loop_block_id == 0) %>%
      count(antecedent) %>%
      head(1) %>%
      pull(antecedent)
    
    direct_connections <- loop_blocks %>%
      filter(antecedent == most_connected_antecedent) %>%
      pull(consequent)
    
    extended_connections <- loop_blocks %>%
      filter(antecedent %in% c(most_connected_antecedent, direct_connections)) %>%
      pull(consequent) %>%
      unique
    
    loop_blocks <- loop_blocks %>%
      mutate(loop_block_id = ifelse(
        (
          antecedent %in% extended_connections |
            consequent %in% extended_connections
        ),
        cluster_counter,
        loop_block_id
      ))
    
    cluster_counter <<- cluster_counter + 1
  }
  
  number_of_loop_blocks <- cluster_counter - 1
  
  for (loop_block in c(1:number_of_loop_blocks)) {
    looped_activities <- loop_blocks %>%
      filter(loop_block_id == loop_block) %>%
      pull(antecedent) %>%
      unique
    
    
    looped_rel_df <- rel_df %>%
      filter(
        orig_antecedent %in% looped_activities,
        orig_consequent %in% looped_activities,
        antec_counter == 1,
        conseq_counter == 1,
      )
    
    discovered_outcome <- discover_process(looped_rel_df,
                                           current_dict)
    
    discovered_snippet <- discovered_outcome$snippet
    current_dict[[discovered_snippet]] <-
      discovered_outcome$dict[[discovered_snippet]]
    current_dict[[discovered_snippet]] <-
      add_loop_back(current_dict[[discovered_snippet]])
    
    rel_df <- rel_df %>%
      anti_join(looped_rel_df, by = c("antecedent", "consequent")) %>%
      mutate(
        antecedent = ifelse(
          antec_counter == 1 &
            orig_antecedent %in% looped_activities,
          discovered_snippet,
          antecedent
        )
      ) %>%
      mutate(antecedent = ifelse(is.na(antecedent), orig_antecedent, antecedent )) %>%
      mutate(
        orig_antecedent = ifelse(
          antec_counter == 1 &
            orig_antecedent  %in% looped_activities,
          discovered_snippet,
          orig_antecedent
        )
      ) %>%
      mutate(
        consequent = ifelse(
          conseq_counter == 1 &
            orig_consequent %in% looped_activities,
          discovered_snippet,
          consequent
        )
      ) %>%
      mutate(consequent = ifelse(is.na(consequent), orig_consequent, consequent )) %>%
      mutate(
        orig_consequent = ifelse(
          conseq_counter == 1 &
            orig_consequent %in% looped_activities,
          discovered_snippet,
          orig_consequent
        )
      ) %>%
      group_by(antecedent, consequent, orig_antecedent, orig_consequent) %>%
      summarize(
        rel = min(rel, na.rm = TRUE),
        score = min(score, na.rm = TRUE),
        importance = min(importance, na.rm = TRUE),
        antec_counter = min(antec_counter, na.rm = TRUE),
        conseq_counter  = min(conseq_counter, na.rm = TRUE)
      ) %>%
      ungroup
    
    print("Loops added")
  }
  
  
  rel_df <- rel_df %>%
    filter(!(antec_counter >= 1 & orig_antecedent %in% looped_activities)) %>%
    filter(!(conseq_counter >= 1 & orig_consequent %in% looped_activities)) %>%
    # rowwise() %>%
    # filter(antecedent %in% names(current_dict) ||
    #          !any(startsWith(
    #            antecedent, loop_blocks$antecedent %>% unique
    #          ))) %>%
    # rowwise() %>%
    # filter(consequent %in% names(current_dict) ||
    #          !any(startsWith(
    #            consequent, loop_blocks$antecedent %>% unique
    #          ))) %>%
    # ungroup() %>%
    filter(antecedent != consequent)
  
}

discovered_outcome <- discover_process(rel_df,
                                       current_dict)

snippet_dictionary <- discovered_outcome$dict
