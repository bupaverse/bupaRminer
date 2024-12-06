check_forced_choice <- function(
  XOR_branches,
  rel_df,
  construction_context = list(
    snippet_dictionary = list(),
    trace_log = NULL
  )){
  snippet_dict <- construction_context$snippet_dictionary
  trace_log <- construction_context$trace_log
  
  if(is.null(trace_log)){
    return(rel_df)
  }
  
  activities_in_XOR <- c()
  
  for(branch in XOR_branches){
    branch <- decode_task(branch, snippet_dict,"START","END")
    if(is.list(branch)){
      activities_in_XOR <- c(activities_in_XOR,
                             branch$tasks$name %>% unique)
    } else {
      activities_in_XOR <- c(activities_in_XOR,
                             branch)
    }
  }
  
  traces_with_at_least_one <- trace_log %>%
    filter(AID %in% activities_in_XOR) %>%
    pull(CID) %>%
    unique()
  
  new_rel_df <- rel_df
  
  ## First we check if we have to convert
  ## maybe follows relations to definitely follows
  
  maybe_follows_branches <- rel_df %>%
    filter(consequent %in% XOR_branches, 
           rel %in% c(
             RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
             RScoreDict$MAYBE_EVENTUALLY_FOLLOWS
           ))
  
  maybe_follows_activities <- maybe_follows_branches$antecedent %>%
    unique
  
  for(act in maybe_follows_activities){
    relevant_trace_cids <- trace_log %>%
      filter(AID == act) %>%
      pull(CID) %>%
      unique
    
    n_cases_with_act <- trace_log %>%
      filter(CID %in% relevant_trace_cids) %>%
      select(CID, CASE_COUNT) %>%
      unique() %>%
      pull(CASE_COUNT) %>%
      sum
    
    n_cases_with_branch <- trace_log %>%
      filter(CID %in% relevant_trace_cids,
             CID %in% traces_with_at_least_one) %>%
      select(CID, CASE_COUNT) %>%
      unique() %>%
      pull(CASE_COUNT) %>%
      sum
    
    forced_choice_score <- round(n_cases_with_branch / n_cases_with_act,1)
    
    new_relations <- maybe_follows_branches %>%
      filter(
        antecedent == act,
        score <= forced_choice_score
      ) %>%
      mutate(rel = factor(RScoreDict$EVENTUALLY_FOLLOWS, levels=levels(maybe_follows_branches$rel), ordered=TRUE),
             score = forced_choice_score)
    
    if(new_relations %>% nrow > 0){
      new_rel_df <- new_rel_df %>%
        anti_join(new_relations %>% select(antecedent, consequent), by=c("antecedent","consequent")) %>%
        bind_rows(new_relations)
    }
  }
  
  
  ## Then we check if we have to convert
  ## ¨PIP to PAR
  new_relations <- tibble()
  
  pip_branches <- rel_df %>%
    filter(
      !(antecedent %in% XOR_branches),
      consequent %in% XOR_branches, 
           rel  == RScoreDict$PARALLEL_IF_PRESENT)
  
  if(pip_branches %>% nrow > 0){
    pip_activities <- pip_branches$antecedent %>%
      unique
    
    for(act in pip_activities){
      
      new_relations <- replace_rel_if_needed(
        rel_df,
        trace_log,
        traces_with_at_least_one,
        act,
        pip_branches,
        factor(RScoreDict$ALWAYS_PARALLEL, levels=levels(rel_df$rel), ordered=TRUE)
      )
      
      if(new_relations %>% nrow > 0){
        reverse_relations <- rel_df %>%
          inner_join(new_relations %>% select(antecedent, consequent), 
                     by=c("antecedent"="consequent",
                          "consequent"="antecedent")) %>%
          mutate(rel = factor(RScoreDict$ALWAYS_PARALLEL, 
                              levels=levels(rel_df$rel), 
                              ordered=TRUE))
        
        new_relations <- new_relations %>%
          bind_rows(reverse_relations)
        
        new_rel_df <- new_rel_df %>%
          anti_join(new_relations %>% select(antecedent, consequent), by=c("antecedent","consequent")) %>%
          bind_rows(new_relations)
      }
    }
  }
  
  return(new_rel_df)
}

replace_rel_if_needed <- function(
    rel_df,
    trace_log,
    traces_with_at_least_one,
    act,
    relevant_branches_df,
    new_rel
    ){
  
  relevant_trace_cids <- trace_log %>%
    filter(AID == act) %>%
    pull(CID) %>%
    unique
  
  n_cases_with_act <- trace_log %>%
    filter(CID %in% relevant_trace_cids) %>%
    select(CID, CASE_COUNT) %>%
    unique() %>%
    pull(CASE_COUNT) %>%
    sum
  
  n_cases_with_branch <- trace_log %>%
    filter(CID %in% relevant_trace_cids,
           CID %in% traces_with_at_least_one) %>%
    select(CID, CASE_COUNT) %>%
    unique() %>%
    pull(CASE_COUNT) %>%
    sum
  
  forced_choice_score <- round(n_cases_with_branch / n_cases_with_act,1)
  
  new_relations <- relevant_branches_df %>%
    filter(
      antecedent == act,
      score <= forced_choice_score
    ) %>%
    mutate(rel = new_rel,
           score = forced_choice_score)
  
  
  return(new_relations)
}