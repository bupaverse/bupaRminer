# 
# R_levels <- c(RScoreDict$DIRECT_JOIN,
#               RScoreDict$DIRECTLY_FOLLOWS,
#               RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
#               RScoreDict$EVENTUALLY_FOLLOWS,
#               RScoreDict$PARALLEL_IF_PRESENT,
#               RScoreDict$ALWAYS_PARALLEL,
#               RScoreDict$TERMINATING,
#               RScoreDict$HAPPENS_DURING,
#               RScoreDict$MUTUALLY_EXCLUSIVE,
#               RScoreDict$REQUIRES,
#               RScoreDict$MAYBE_EVENTUALLY_FOLLOWS
# )
# 
# 
# R_levels <- c(RScoreDict$DIRECT_JOIN,
#               RScoreDict$DIRECTLY_FOLLOWS,
#               RScoreDict$PARALLEL_IF_PRESENT,
#               RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
#               RScoreDict$EVENTUALLY_FOLLOWS,
#               RScoreDict$REQUIRES,
#               RScoreDict$MUTUALLY_EXCLUSIVE,
#               RScoreDict$ALWAYS_PARALLEL,
#               RScoreDict$TERMINATING,
#               RScoreDict$HAPPENS_DURING,
#               RScoreDict$MAYBE_EVENTUALLY_FOLLOWS
# )

R_levels <- c(RScoreDict$DIRECT_JOIN,
              RScoreDict$DIRECTLY_FOLLOWS,
              RScoreDict$PARALLEL_IF_PRESENT,
              RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
              RScoreDict$EVENTUALLY_FOLLOWS,
              RScoreDict$ALWAYS_PARALLEL,
              RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
              RScoreDict$REQUIRES,
              RScoreDict$MUTUALLY_EXCLUSIVE,
              RScoreDict$TERMINATING,
              RScoreDict$HAPPENS_DURING
)

INTERRUPTING_RELS <- c(RScoreDict$TERMINATING,
                       RScoreDict$HAPPENS_DURING)
FOLLOWS_RELS <- c(RScoreDict$DIRECTLY_FOLLOWS,
                  RScoreDict$EVENTUALLY_FOLLOWS,
                  RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                  RScoreDict$MAYBE_EVENTUALLY_FOLLOWS)
OTHER_RELS <- c(RScoreDict$REQUIRES,
                RScoreDict$DIRECT_JOIN)

merge_relationships <- function(
    snippet_name,
    activities,
    rel_df
){
  ## Reset factor levels
  rel_df <- rel_df %>%
    mutate(rel = factor(rel, levels = R_levels, ordered = TRUE))
  
  ## We do not want to remove the END event from the log
  real_activities <- activities[! activities %in% c('END')]
  
  HAS_END <- "END" %in% activities | endsWith(snippet_name, " END")
  HAS_START <- "START" %in% activities | startsWith(snippet_name, "START ")
  
  ## If we have added an END event, we want to remove
  ## all relationships between the activities in the block
  ## and the end event
  if(HAS_END){
    rel_df <- rel_df %>%
      filter(!(antecedent %in% real_activities & consequent == "END") )
  }
  
  
  if(HAS_START){
    rel_df <- rel_df %>%
      filter(!(consequent %in% real_activities & rel != RScoreDict$REQUIRES))
    rel_df <- rel_df %>%
      filter(!(antecedent %in% real_activities & rel == RScoreDict$REQUIRES))
  }
  
  
  ## Merging relationships with new snippet as consequent
  consequent_rel <- rel_df %>% 
    filter(consequent %in% real_activities,
           !(antecedent %in% real_activities) ) 
  
  ## Merging relationships with new snippet as antecedent
  antecedent_rel <- rel_df %>% 
    filter(antecedent %in% real_activities,
           !(consequent %in% real_activities) ) 
  
  ## Remove old relationships
  original_rel_df <- rel_df
  rel_df <- rel_df %>%
    filter(!(antecedent %in% real_activities),
           !(consequent %in% real_activities))
  
  if(consequent_rel %>% nrow() > 0){
    
    ## If we only have one activity,
    ## then we have to reduce the importance
    ## further in otder to avoid a loop.
    if(length(activities[activities != ""])==1){
      
      direct_antecedents <- original_rel_df %>%
        filter(consequent %in% activities,
               rel %in% c(RScoreDict$DIRECTLY_FOLLOWS, RScoreDict$DIRECT_JOIN,
                          RScoreDict$EVENTUALLY_FOLLOWS, RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                          RScoreDict$MAYBE_EVENTUALLY_FOLLOWS))
      
      closest_direct_antecedents <- original_rel_df %>%
        filter(consequent %in% c(direct_antecedents$antecedent, activities),
               rel %in% c(RScoreDict$DIRECTLY_FOLLOWS, RScoreDict$DIRECT_JOIN,
                          RScoreDict$EVENTUALLY_FOLLOWS, RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                          RScoreDict$MAYBE_EVENTUALLY_FOLLOWS)) %>%
        group_by(antecedent) %>%
        mutate(n = n()) %>%
        filter(n == min(n))
      
      closest_direct_XOR_roots <- closest_direct_antecedents %>%
        filter(rel %in% c(RScoreDict$MAYBE_DIRECTLY_FOLLOWS, RScoreDict$MAYBE_EVENTUALLY_FOLLOWS))
      
      if(closest_direct_XOR_roots %>% nrow > 0){
        print("CHANGE HAD EFFECT")
        
        closest_direct_XOR_roots <- closest_direct_XOR_roots %>%
          mutate(rel = RScoreDict$DIRECT_JOIN)
        
        consequent_rel <- consequent_rel %>%
          filter(!(antecedent %in% closest_direct_XOR_roots$antecedent & consequent  %in% activities)) %>%
          bind_rows(closest_direct_XOR_roots)
        
      } else {
        consequent_rel <- consequent_rel %>%
          mutate(importance = importance/10)
        
      }
      
    }
    
    consequent_rel <- consequent_rel %>%
      group_by(antecedent) %>%
      summarize(rel = min(rel, na.rm = TRUE),
                score = min(score, na.rm=TRUE),
                importance = min(importance, na.rm =TRUE)) %>%
      ungroup() %>%
      mutate(consequent = snippet_name)
    
    rel_df <- rel_df %>%
      bind_rows(consequent_rel)
  }
  
  if(antecedent_rel %>% nrow() > 0){
    antecedent_rel <- antecedent_rel %>% 
      group_by(consequent) %>%
      summarize(rel = min(rel, na.rm = TRUE),
                score = min(score, na.rm=TRUE),
                importance = min(importance, na.rm =TRUE)) %>%
      ungroup() %>%
      mutate(antecedent = snippet_name)
    
    rel_df <- rel_df %>%
      bind_rows(antecedent_rel)
  }
  
  ## Reset factor levels
  rel_df <- rel_df %>%
    mutate(rel = factor(rel, levels = R_levels, ordered = TRUE))
  
  if(rel_df %>% nrow == 0){
    rel_df <- tibble(
      antecedent = snippet_name,
      consequent = NA,
      rel = NA,
      score = NA,
      importance = NA
    )
  }
  
  return(rel_df)
}

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


determine_rels_in_focus <- function(rel_df){
  rel_summary <- rel_df %>%
    count(rel)
  
  if(any(INTERRUPTING_RELS %in% rel_summary$rel)){
    return(INTERRUPTING_RELS)
  } else if(RScoreDict$DIRECTLY_FOLLOWS %in% rel_summary$rel){
    return(RScoreDict$DIRECTLY_FOLLOWS)
  } else if(RScoreDict$ALWAYS_PARALLEL %in% rel_summary$rel){
    return(RScoreDict$ALWAYS_PARALLEL)
  } else {
    return(NULL)
  }
}