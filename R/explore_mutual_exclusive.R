explore_mutual_exclusive_relationship <- function(
    rel_df,
    snippet_dict){
  
  return_list <- list(
    snippet = NULL,
    activities = c(),
    rel_df = rel_df,
    snippet_dictionary = snippet_dict,
    messages = c()
  )
  
  mutual_exclusives <- fetch_mutual_exclude(rel_df)
  
  if(mutual_exclusives %>% nrow > 0){
    
    found_none = TRUE
    while(found_none & mutual_exclusives %>% nrow() > 0){
      sampled_exclude <- mutual_exclusives %>%
        sample_pair(RScoreDict$MUTUALLY_EXCLUSIVE)
      
      mutual_exclude_activities <- c(sampled_exclude$antecedent, sampled_exclude$consequent)
      
      mutual_exclusives <- mutual_exclusives %>%
        filter(!(antecedent %in% mutual_exclude_activities),
               !(consequent) %in% mutual_exclude_activities)
      
      other_relations_as_antec <- rel_df %>%
        filter(antecedent %in% mutual_exclude_activities) %>%
        filter(!(consequent %in%mutual_exclude_activities) ) %>%
        count(consequent,rel) 
      
      ## We consider activities that have a SOFTPAR relation
      ## with some, but follows relations with others
      ## not as conflicts 
      removed_acts <- c()
      if(other_relations_as_antec %>% 
         filter(rel==RScoreDict$PARALLEL_IF_PRESENT) %>% nrow > 0){
        par_acts <- other_relations_as_antec %>% 
          filter(rel==RScoreDict$PARALLEL_IF_PRESENT) %>%
          pull(consequent)
        
        par_acts <- other_relations_as_antec %>% 
          filter(rel %in% MERGE_FOLLOWS_RELS,
                 consequent %in% par_acts) %>%
          pull(consequent)
        
        if(length(par_acts) > 0){
          other_relations_as_antec <- other_relations_as_antec %>%
            filter(!(consequent %in% par_acts))
          
          removed_acts <- par_acts
        }
      }
      
      if(other_relations_as_antec %>% 
         filter(n < length(mutual_exclude_activities)) %>%
         nrow == 0){
        WHAT_FOLLOWS_IS_SAME <- TRUE
      } else {
        WHAT_FOLLOWS_IS_SAME <- FALSE
      }
      
      other_relations_as_consequent <- rel_df %>%
        filter(consequent %in% mutual_exclude_activities) %>%
        filter(!(antecedent %in%mutual_exclude_activities) ) %>%
        count(antecedent,rel) 
      
      
      ## We consider activities that have a SOFTPAR relation
      ## with some, but follows relations with others
      ## not as conflicts 
      if(other_relations_as_consequent %>% 
         filter(rel==RScoreDict$PARALLEL_IF_PRESENT) %>% nrow > 0){
        par_acts <- other_relations_as_consequent %>% 
          filter(rel==RScoreDict$PARALLEL_IF_PRESENT) %>%
          pull(antecedent)
        
        par_acts <- other_relations_as_consequent %>% 
          filter(rel %in% MERGE_FOLLOWS_RELS,
                 antecedent %in% par_acts) %>%
          pull(antecedent)
        
        if(length(c(par_acts, removed_acts)) > 0){
          other_relations_as_consequent <- other_relations_as_consequent %>%
            filter(!(antecedent %in% c(par_acts, removed_acts)))
        }
      }
      
      if(other_relations_as_consequent %>% 
         filter(n < length(mutual_exclude_activities)) %>%
         nrow == 0){
        WHAT_PRECEDES_IS_SAME <- TRUE
      } else {
        WHAT_PRECEDES_IS_SAME <- FALSE
      }
      
      
      if(WHAT_FOLLOWS_IS_SAME && WHAT_PRECEDES_IS_SAME){
        
        return_list <- solve_XOR_relationship(
          XOR_root = "",
          XOR_branches = mutual_exclude_activities,
          rel_df,
          snippet_dict,
          split_symbol = ">X>")
        
        found_none <- FALSE
      }
      
      if(other_relations_as_consequent %>% filter(rel == RScoreDict$REQUIRES) %>% nrow > 0){
        REQ_activities <- other_relations_as_consequent %>% 
          filter(rel == RScoreDict$REQUIRES)
        
        REQ_excluded <- other_relations_as_consequent %>%
          filter(antecedent %in% REQ_activities$antecedent,
                 rel == RScoreDict$MUTUALLY_EXCLUSIVE)
        
        if(REQ_excluded %>% nrow > 0){
          selected_pair <- rel_df %>%
            filter(antecedent %in% mutual_exclude_activities,
                   consequent %in% REQ_excluded$antecedent) %>%
            sample_pair(MERGE_FOLLOWS_RELS)
          
          if(!is.null(selected_pair) & selected_pair %>% nrow > 0){
            
            return_list <- solve_sequence_relationship(
              selected_pair,
              rel_df,
              snippet_dict
            )
            
            found_none <- FALSE
          }
        }
      }
      
      if(other_relations_as_antec %>% filter(rel == RScoreDict$REQUIRES) %>% nrow > 0){
        REQ_activities <- other_relations_as_antec %>% 
          filter(rel == RScoreDict$REQUIRES)
        
        REQ_excluded <- other_relations_as_antec %>%
          filter(consequent %in% REQ_activities$consequent,
                 rel == RScoreDict$MUTUALLY_EXCLUSIVE)
        
        if(REQ_excluded %>% nrow > 0){
          
          
          selected_pair <- rel_df %>%
            filter(antecedent %in% REQ_excluded$consequent,
                   consequent %in% mutual_exclude_activities) %>%
            sample_pair(MERGE_FOLLOWS_RELS)
          
          if(!is.null(selected_pair) & selected_pair %>% nrow > 0){
            
            return_list <- solve_sequence_relationship(
              selected_pair,
              rel_df,
              snippet_dict
            )
            
            found_none <- FALSE
          }
        }
      }
    }
  }
  return(return_list)
}

fetch_mutual_exclude <- function(
    rel_df){
  exclude_df <- rel_df %>%
    filter(rel == RScoreDict$MUTUALLY_EXCLUSIVE)
  
  exclude_df <- exclude_df %>%
    inner_join(exclude_df %>% select(antecedent, consequent),
               c("antecedent"="consequent","consequent"="antecedent"))
  
  return(exclude_df)
}