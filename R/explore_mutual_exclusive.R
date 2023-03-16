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
        arrange(
          -importance,
          -score
        ) %>%
        head(1)
      
      mutual_exclusives <- mutual_exclusives %>%
        filter(!(antecedent == sampled_exclude$antecedent & consequent == sampled_exclude$consequent))%>%
        filter(!(consequent == sampled_exclude$antecedent & antecedent == sampled_exclude$consequent))
      
      mutual_excludes <- mutual_exclusives %>%
        filter((antecedent %in% c(sampled_exclude$antecedent, sampled_exclude$consequent)) |
                 (consequent %in% c(sampled_exclude$antecedent, sampled_exclude$consequent)))
      
      mutual_exclude_activities <- c(sampled_exclude$antecedent, sampled_exclude$consequent)
      
      other_relations_as_antec <- rel_df %>%
        filter(antecedent %in% mutual_exclude_activities) %>%
        filter(!(consequent %in%mutual_exclude_activities) ) %>%
        count(consequent,rel) 
      
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