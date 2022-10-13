source("sandbox/construction_functions.R")

I_WANT_INTERRUPTIONS <- FALSE

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

rel_notebook_df <- solve_apriori_conflicts(rel_notebook_df)

update_rel_notebook <- function(
    constrc_result,
    rel_df,
    verbose = TRUE){
  
  if(verbose ==  TRUE & length(constrc_result$messages) > 0){
    print(constrc_result$messages)
  }
  
  if(!is.null(constrc_result)){
    rel_df <- constrc_result$rel_df
  }
    
  if(!is.null(constrc_result$snippet)){
    rel_df <- merge_relationships(
      constrc_result$snippet,
      constrc_result$activities,
      rel_df
    )
  }
  
  if(I_WANT_INTERRUPTIONS){
    readline(prompt="Press [enter] to continue")
  }
  
  return(rel_df)
}

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
  
  rel_notebook_df <- update_rel_notebook(
    result,
    rel_notebook_df
  )
  
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
  
  rel_notebook_df <- update_rel_notebook(
    result,
    rel_notebook_df
  )
  
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
  
  rel_notebook_df <- update_rel_notebook(
    result,
    rel_notebook_df
  )
  
}

completed_FOL <- FALSE
while(rel_notebook_df %>% 
      filter(rel %in% c(RScoreDict$DIRECTLY_FOLLOWS,
                        RScoreDict$EVENTUALLY_FOLLOWS,
                        RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                        RScoreDict$MAYBE_EVENTUALLY_FOLLOWS) ) %>% 
      nrow() > 0 & completed_FOL == FALSE){
  
  SOFT_PAR_POSSIBLE <- TRUE
  
  while(SOFT_PAR_POSSIBLE & rel_notebook_df %>% count(rel) %>% filter(rel == RScoreDict$PARALLEL_IF_PRESENT) %>% nrow > 0){
    
    result <- explore_soft_PAR_relationship(rel_notebook_df)
    
    rel_notebook_df <- update_rel_notebook(
      result,
      rel_notebook_df
    )
    
    if(is.null(result$snippet)){
      SOFT_PAR_POSSIBLE <- FALSE
    }
  }
  
  result <- NULL
  
  ## We fetch early activities in branches
  relevant_antec <- fetch_sequence_antecedents(rel_notebook_df)
  
  ## We sample any pair between an early activity
  ## and any follows or eventually follows relationship
  sampled_pair <- sample_pair(
    rel_notebook_df, # %>% filter(antecedent %in% relevant_antec),
    c(RScoreDict$DIRECTLY_FOLLOWS,
      RScoreDict$EVENTUALLY_FOLLOWS,
      RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
      RScoreDict$MAYBE_EVENTUALLY_FOLLOWS))
  
  result <- solve_sequence_relationship(
    sampled_pair,
    rel_notebook_df
  )
  
  if(is.null(result)){
    print("---- No result for sample")
  }
  
  rel_notebook_df <- update_rel_notebook(
    result,
    rel_notebook_df
  )
  
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
  
  if(is.null(result)){
    print("---- No result for sample")
  }
  
  rel_notebook_df <- update_rel_notebook(
    result,
    rel_notebook_df
  )
  
  
}
