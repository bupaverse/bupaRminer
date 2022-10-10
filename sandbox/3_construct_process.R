source("sandbox/construction_functions.R")

I_WANT_INTERRUPTIONS <- TRUE

rel_notebook_df <- assigned_rel_df %>%
  filter(!(rel == RScoreDict$ALWAYS_PARALLEL &
          antecedent %in% c("START","END") &
          consequent %in% c("START","END"))) %>%
  filter(antecedent != "END",
         consequent != "START") %>%
  mutate(
    score=ifelse(consequent=="END",0,score),
    importance=ifelse(consequent=="END",0,importance)
    )

R_levels <- c(RScoreDict$DIRECT_JOIN,
              RScoreDict$DIRECTLY_FOLLOWS,
              RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
              RScoreDict$EVENTUALLY_FOLLOWS,
              RScoreDict$PARALLEL_IF_PRESENT,
              RScoreDict$ALWAYS_PARALLEL,
              RScoreDict$TERMINATING,
              RScoreDict$HAPPENS_DURING,
              RScoreDict$MUTUALLY_EXCLUSIVE,
              RScoreDict$REQUIRES,
              RScoreDict$MAYBE_EVENTUALLY_FOLLOWS, 
              "R6-")

solve_apriori_conflicts <- function(rel_df){
  
  ## Solve preliminary conflicts
  follows_rel <- rel_df %>%
    filter(rel %in% c(
      RScoreDict$DIRECT_JOIN,
      RScoreDict$DIRECTLY_FOLLOWS,
      RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
      RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
      RScoreDict$TERMINATING,
      RScoreDict$HAPPENS_DURING,
      RScoreDict$EVENTUALLY_FOLLOWS))
  
  follows_rel <- follows_rel %>%
    inner_join(follows_rel, by = c("antecedent"="consequent",'consequent'="antecedent")) %>%
    mutate(prevailing_rel = pmin(rel.x, rel.y)) %>%
    mutate(must_remove = (rel.x != prevailing_rel))
  
  removed_rels <- follows_rel %>%
    filter(must_remove == TRUE)
  
  rel_df <- rel_df %>%
    anti_join(removed_rels, by=c("antecedent","consequent"))
  
  conflict_rel <- follows_rel %>%
    filter(rel.x == rel.y) %>%
    mutate(rel = RScoreDict$PARALLEL_IF_PRESENT,
           importance = 0,
           score = 0.5) %>%
    select(antecedent, consequent, rel)
  
  rel_df <- rel_df %>%
    anti_join(conflict_rel, by=c("antecedent","consequent")) %>%
    bind_rows(conflict_rel)
  
  return(rel_df)
}

rel_notebook_df <- solve_apriori_conflicts(rel_notebook_df)

completed_interrupt <- FALSE
while(rel_notebook_df %>% 
      filter(rel %in% c(
        RScoreDict$TERMINATING,
        RScoreDict$HAPPENS_DURING)) %>% 
      nrow() > 0 & completed_interrupt == FALSE){
  sampled_pair <- sample_pair(
    rel_notebook_df,
    c(
      RScoreDict$TERMINATING,
      RScoreDict$HAPPENS_DURING))
  
  result <- solve_interrupt_relationship(
    sampled_pair,
    rel_notebook_df
  )
  rel_notebook_df <- result$rel_df
  
  print(result$messages)
  
  if(!is.null(result$snippet)){
    rel_notebook_df <- merge_relationships(
      result$snippet,
      result$activities,
      rel_notebook_df
    )
  }
  
  if(I_WANT_INTERRUPTIONS){
    readline(prompt="Press [enter] to continue")
  }
  
}

completed_DF <- FALSE
while(rel_notebook_df %>% 
      filter(rel == RScoreDict$DIRECTLY_FOLLOWS) %>% 
      nrow() > 0 & completed_DF == FALSE){
  
  sampled_pair <- sample_pair(
    rel_notebook_df,
    RScoreDict$DIRECTLY_FOLLOWS)
  
  result <- solve_DF_relationship(
    sampled_pair,
    rel_notebook_df
  )
  rel_notebook_df <- result$rel_df
  
  print(result$messages)
  
  if(!is.null(result$snippet)){
    rel_notebook_df <- merge_relationships(
      result$snippet,
      result$activities,
      rel_notebook_df
    )
  }
  
  if(I_WANT_INTERRUPTIONS){
    readline(prompt="Press [enter] to continue")
  }
  
}

completed_PAR <- FALSE
while(rel_notebook_df %>% 
      filter(rel == RScoreDict$ALWAYS_PARALLEL) %>% 
      nrow() > 0 & completed_PAR == FALSE){
  sampled_pair <- sample_pair(
    rel_notebook_df,
    RScoreDict$ALWAYS_PARALLEL)
  
  result <- solve_PAR_relationship(
    sampled_pair,
    rel_notebook_df
  )
  rel_notebook_df <- result$rel_df
  
  print(result$messages)
  
  if(!is.null(result$snippet)){
    rel_notebook_df <- merge_relationships(
      result$snippet,
      result$activities,
      rel_notebook_df
    )
  }
  
  if(I_WANT_INTERRUPTIONS){
    readline(prompt="Press [enter] to continue")
  }
  
}

completed_FOL <- FALSE
while(rel_notebook_df %>% 
      filter(rel %in% c(RScoreDict$DIRECTLY_FOLLOWS,
                        RScoreDict$EVENTUALLY_FOLLOWS,
                        RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                        RScoreDict$MAYBE_EVENTUALLY_FOLLOWS) ) %>% 
      nrow() > 0 & completed_FOL == FALSE){
  
  result <- explore_soft_PAR_relationship(rel_notebook_df)
  
  if(!is.null(result$snippet)){
    rel_notebook_df <- result$rel_df
    
    rel_notebook_df <- merge_relationships(
      result$snippet,
      result$activities,
      rel_notebook_df
    )
    
    print("SOFT PAR ESTABLISHED")
    print(result$messages)
    
    if(I_WANT_INTERRUPTIONS){
      readline(prompt="Press [enter] to continue")
    }
  }
  
  result <- NULL
  
  ## We fetch early activities in branches
  relevant_antec <- fetch_sequence_antecedents(rel_notebook_df)
  
  ## We sample any pair between an early activity
  ## and any follows or eventually follows relationship
  sampled_pair <- sample_pair(
    rel_notebook_df %>% filter(antecedent %in% relevant_antec),
    c(RScoreDict$DIRECTLY_FOLLOWS,
      RScoreDict$EVENTUALLY_FOLLOWS,
      RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
      RScoreDict$MAYBE_EVENTUALLY_FOLLOWS))
  
  result <- solve_sequence_relationship(
    sampled_pair,
    rel_notebook_df
  )
  
  if(!is.null(result)){
    
    rel_notebook_df <- result$rel_df
    
    print(result$messages)
  } else {
    print("---- No result for sample")
  }
  
  if(!is.null(result$snippet)){
    rel_notebook_df <- merge_relationships(
      result$snippet,
      result$activities,
      rel_notebook_df
    )
  }
  
  if(rel_notebook_df %>%
      filter(
        rel %in% c(RScoreDict$DIRECTLY_FOLLOWS,
                   RScoreDict$EVENTUALLY_FOLLOWS,
                   RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                   RScoreDict$MAYBE_EVENTUALLY_FOLLOWS),
        antecedent == "START",
        consequent != "END") %>% nrow() == 1){
    completed_FOL = TRUE
  } 
  
  if(I_WANT_INTERRUPTIONS){
    readline(prompt="Press [enter] to continue")
  }
  
}


completed_RxREQ <- FALSE
while(rel_notebook_df %>% 
      filter(rel %in%  c(RScoreDict$DIRECT_JOIN,
                         RScoreDict$REQUIRES)) %>% 
      nrow() > 0 & completed_RxREQ == FALSE){
  
  ## We sample any pair between an early activity
  ## and any follows or eventually follows relationship
  sampled_pair <- rel_notebook_df %>% 
    filter(rel %in%  c(RScoreDict$DIRECT_JOIN,
                       RScoreDict$REQUIRES) )
  
  if(sampled_pair$rel == RScoreDict$REQUIRES){
    seq_pair <- tibble(
      antecedent = sampled_pair$consequent,
      consequent = sampled_pair$antecedent,
      rel = RScoreDict$DIRECTLY_FOLLOWS,
      score = NA
    )
  } else {
    seq_pair <- sampled_pair %>%
      mutate(rel = RScoreDict$DIRECTLY_FOLLOWS)
  }
  
  result <- solve_directly_follows(
    seq_pair,
    seq_pair
  )
  
  if(!is.null(result)){
    print(result$messages)
  } else {
    print("---- No result for sample")
  }
  
  if(!is.null(result$snippet)){
    rel_notebook_df <- merge_relationships(
      result$snippet,
      result$activities,
      rel_notebook_df
    )
  }
  
  if(I_WANT_INTERRUPTIONS){
    readline(prompt="Press [enter] to continue")
  }
  
}
