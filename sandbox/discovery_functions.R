source("sandbox/construction_functions.R")
source("sandbox/merge_functions.R")

discover_process <- function(
    rel_df,
    snippet_dictionary = list()){
  
  rel_notebook_df <- solve_apriori_conflicts(rel_df, strict = FALSE)
  
  RELS_IN_FOCUS <- determine_rels_in_focus(
    rel_notebook_df
  )
  
  
  HAS_COMPLETED <- FALSE
  while(!is.null(RELS_IN_FOCUS) & HAS_COMPLETED == FALSE){
    
    if(any(INTERRUPTING_RELS %in% RELS_IN_FOCUS)){
      rel_solver_function <- solve_interrupt_relationship
    } else if(RELS_IN_FOCUS == RScoreDict$DIRECTLY_FOLLOWS){
      rel_solver_function <- solve_DF_relationship
    } else if(RELS_IN_FOCUS == RScoreDict$ALWAYS_PARALLEL){
      rel_solver_function <- solve_PAR_relationship
    }
    
    sampled_pair <- sample_pair(
      rel_notebook_df,
      RELS_IN_FOCUS)
    
    result <- rel_solver_function(
      sampled_pair,
      rel_notebook_df,
      snippet_dictionary
    )
    
    rel_notebook_df <- update_rel_notebook(
      result,
      rel_notebook_df
    )
    
    snippet_dictionary <- result$snippet_dictionary
    
    RELS_IN_FOCUS <- determine_rels_in_focus(
      rel_notebook_df
    )
    
    if(is.null(RELS_IN_FOCUS)){
      HAS_COMPLETED <- TRUE
    }
  }
  
  completed_FOL <- FALSE
  while(rel_notebook_df %>% 
        filter(rel %in% FOLLOWS_RELS ) %>% 
        nrow() > 0 & completed_FOL == FALSE){
    
    SOFT_PAR_POSSIBLE <- TRUE
    
    while(SOFT_PAR_POSSIBLE & rel_notebook_df %>% count(rel) %>% filter(rel == RScoreDict$PARALLEL_IF_PRESENT) %>% nrow > 0){
      
      result <- explore_soft_PAR_relationship(rel_notebook_df,
                                              snippet_dictionary)
      
      rel_notebook_df <- update_rel_notebook(
        result,
        rel_notebook_df
      )
      
      if(is.null(result$snippet)){
        SOFT_PAR_POSSIBLE <- FALSE
      } else {
        snippet_dictionary <- result$snippet_dictionary
      }
    }
    
    result <- NULL
    
    ## We fetch early activities in branches
    relevant_antec <- fetch_sequence_antecedents(rel_notebook_df)
    
    ## We sample any pair between an early activity
    ## and any follows or eventually follows relationship
    sampled_pair <- sample_pair(
      rel_notebook_df, # %>% filter(antecedent %in% relevant_antec),
      FOLLOWS_RELS)
    
    result <- solve_sequence_relationship(
      sampled_pair,
      rel_notebook_df,
      snippet_dictionary,
      reset = TRUE
    )
    
    if(is.null(result)){
      print("---- No result for sample")
    } else {
      snippet_dictionary <- result$snippet_dictionary
    }
    
    rel_notebook_df <- update_rel_notebook(
      result,
      rel_notebook_df
    )
    
    if(rel_notebook_df %>%
       filter(
         rel %in% FOLLOWS_RELS,
         antecedent == "START",
         consequent != "END") %>% nrow() == 1){
      completed_FOL = TRUE
    } 
    
  }
  
  
  SOFT_PAR_POSSIBLE <- TRUE
  while(SOFT_PAR_POSSIBLE & rel_notebook_df %>% count(rel) %>% filter(rel == RScoreDict$PARALLEL_IF_PRESENT) %>% nrow > 0){
    
    result <- explore_soft_PAR_relationship(rel_notebook_df,
                                            snippet_dictionary)
    
    rel_notebook_df <- update_rel_notebook(
      result,
      rel_notebook_df
    )
    
    if(is.null(result$snippet)){
      SOFT_PAR_POSSIBLE <- FALSE
    } else {
      snippet_dictionary <- result$snippet_dictionary
    }
  }
  
  completed_RxREQ <- FALSE
  while(rel_notebook_df %>% 
        filter(rel %in%  OTHER_RELS) %>% 
        nrow() > 0 & completed_RxREQ == FALSE){
    
    ## We sample any pair between an early activity
    ## and any follows or eventually follows relationship
    sampled_pair <- rel_notebook_df %>% sample_pair(
      c(RScoreDict$DIRECT_JOIN,
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
      seq_pair,
      snippet_dictionary
    )
    
    if(is.null(result)){
      print("---- No result for sample")
    } else {
      snippet_dictionary <- result$snippet_dictionary
    }
    
    rel_notebook_df <- update_rel_notebook(
      result,
      rel_notebook_df
    )
    
    
  }
  
  return_list <- list(
    snippet = rel_notebook_df$antecedent[1],
    dict = snippet_dictionary[rel_notebook_df$antecedent[1]]
  )
  
  return(return_list)
}