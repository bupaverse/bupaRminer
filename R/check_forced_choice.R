check_forced_choice <- function(
  XOR_branches,
  rel_df,
  construction_context = list(
    snippet_dictionary = list(),
    trace_log = NULL
  )){
  
  ## Extract infrmation from context
  snippet_dict <- construction_context$snippet_dictionary
  trace_log <- construction_context$trace_log
  
  
  ## If no log of traces, i.e. a preprocessed event log,
  # has been provided, then we cannot do this check.
  if(is.null(trace_log)){
    return(rel_df)
  }
  
  
  ## First, we need all the activities as they occurred in the original log
  ## This means decoding all the snippets. F.e. we must know that snippet A -> B
  ## contains the activities A and B
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
  
  
  ## If there are not at least two activities,
  ## then this entire check makes no sense
  if(length(activities_in_XOR) < 2){
    return(rel_df)
  }
  
  ## We pull the case IDs of all the templates
  ## that contain at least one of the activities from
  ## the branch
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
             RScoreDict$MAYBE_DIRECTLY_FOLtaffiLOWS,
             RScoreDict$MAYBE_EVENTUALLY_FOLLOWS
           ))
  
  maybe_follows_snippets <- maybe_follows_branches$antecedent %>%
    unique
  
  
  for(snippet in maybe_follows_snippets){
    ## These snippets may have to be decoded as well
    prec_act <- decode_task(snippet, snippet_dict,"START","END")
    maybe_follows_activities <- c()
    if(is.list(prec_act)){
      maybe_follows_activities <- prec_act$tasks$name %>% unique
    } else {
      maybe_follows_activities <- prec_act
    }
    
    new_forced_choice_score <- 0
    
    ## Now we check for each preceeding activiy if they preceed
    ## one of the potentially forced choice branches
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
      
      if(!is.na(forced_choice_score) & !is.null(forced_choice_score)){
        new_forced_choice_score <- pmax(new_forced_choice_score, forced_choice_score)
      }
      
    }
    
    ## We now have a score that reflects the likelihood that both activities occur together
    ## if this score is better than their maybe follows score, we convert the relationship
    ## to an eventually follows
    new_relations <- maybe_follows_branches %>%
      filter(
        antecedent == snippet,
        score <= new_forced_choice_score
      ) %>%
      mutate(rel = factor(RScoreDict$EVENTUALLY_FOLLOWS, levels=levels(maybe_follows_branches$rel), ordered=TRUE),
             score = new_forced_choice_score)
    
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
    pip_snippets <- pip_branches$antecedent %>%
      unique
    
    for(pip_snippet in pip_snippets){
      
      pip_act <- decode_task(pip_snippet, snippet_dict,"START","END")
      pip_activities <- c()
      if(is.list(pip_act)){
        pip_activities <- pip_act$tasks$name %>% unique
      } else {
        pip_activities <- pip_act
      }
      relevant_trace_cids <- trace_log %>%
        filter(AID %in% pip_activities) %>%
        pull(CID) %>%
        unique
      
      n_cases_with_both <- trace_log %>%
        filter(CID %in% relevant_trace_cids,
               CID %in% traces_with_at_least_one) %>%
        select(CID, CASE_COUNT) %>%
        unique() %>%
        pull(CASE_COUNT) %>%
        sum
      
      n_cases_with_act <- trace_log %>%
        filter(CID %in% relevant_trace_cids) %>%
        select(CID, CASE_COUNT) %>%
        unique() %>%
        pull(CASE_COUNT) %>%
        sum
      
      n_cases_with_XOR <- trace_log %>%
        filter(CID %in% traces_with_at_least_one) %>%
        select(CID, CASE_COUNT) %>%
        unique() %>%
        pull(CASE_COUNT) %>%
        sum
      
      
      forced_choice_score <- round(n_cases_with_both / pmax(n_cases_with_act,n_cases_with_XOR),1)
      
      new_relations <- pip_branches %>%
        filter(
          antecedent == pip_snippet,
          score <= forced_choice_score
        ) %>%
        mutate(rel = factor(RScoreDict$ALWAYS_PARALLEL, levels=levels(rel_df$rel), ordered=TRUE),
               score = forced_choice_score)
      
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
  
  ## We also check if REQ relations need to be added
  ## we only do this for perfect scores
  
  ## Then we check if we have to convert
  ## ¨PIP to PAR
  new_relations <- tibble()
  
  related_antecedents <- new_rel_df %>%
    filter(
      !(antecedent %in% XOR_branches),
      consequent %in% XOR_branches)
  
  all_antecedents <- new_rel_df %>%
    filter(!(antecedent %in% XOR_branches)) %>%
    pull(antecedent) %>%
    unique
  
  unrelated_antecedents <- all_antecedents[!(all_antecedents %in% related_antecedents)]
  
  for(unrel_antec in unrelated_antecedents){
    
    unrel_act <- decode_task(unrel_antec, snippet_dict,"START","END")
    unrel_activities <- c()
    if(is.list(unrel_act)){
      unrel_activities <- unrel_act$tasks$name %>% unique
    } else {
      unrel_activities <- unrel_act
    }
    relevant_trace_cids <- trace_log %>%
      filter(AID %in% unrel_activities) %>%
      pull(CID) %>%
      unique
    
    n_cases_with_both <- trace_log %>%
      filter(CID %in% relevant_trace_cids,
             CID %in% traces_with_at_least_one) %>%
      select(CID, CASE_COUNT) %>%
      unique() %>%
      pull(CASE_COUNT) %>%
      sum
    
    n_cases_with_other <- trace_log %>%
      filter(CID %in% relevant_trace_cids) %>%
      select(CID, CASE_COUNT) %>%
      unique() %>%
      pull(CASE_COUNT) %>%
      sum
    
    
    forced_choice_score <- round(n_cases_with_both/n_cases_with_other,1)
    if(is.na(forced_choice_score)){
      forced_choice_score <- 0
    }
    if(forced_choice_score == 1){
      reverse_rels <- new_rel_df %>%
        filter(antecedent %in% XOR_branches,
               consequent == unrel_antec,
               rel != RScoreDict$REQUIRES,
               !startsWith(consequent, "START"))
      
      if(reverse_rels %>% nrow > 0){
        
        
        new_rels <- reverse_rels %>%
          mutate(temp = antecedent) %>%
          mutate(antecedent = consequent) %>%
          mutate(consequent = temp) %>%
          select(-temp) %>%
          mutate(rel = factor(RScoreDict$REQUIRES, 
                              levels=levels(rel_df$rel), 
                              ordered=TRUE),
                 score = forced_choice_score)
        
        new_rel_df <- new_rel_df %>%
          bind_rows(new_rels)
      }
        
    }
    
  }
  
  return(new_rel_df)
}