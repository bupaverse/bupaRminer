
explore_soft_PAR_relationship <- function(
    rel_df,
    snippet_dict){
  
  return_list <- list(
    snippet = NULL,
    activities = c(),
    rel_df = rel_df,
    snippet_dictionary = snippet_dict,
    messages = c()
  )
  
  mutual_pars_if_present <- fetch_mutual_par_if_present(rel_df)
  
  if(mutual_pars_if_present %>% nrow > 0){
    
    found_none = TRUE
    while(found_none & mutual_pars_if_present %>% nrow() > 0){
      sampled_soft_par <- mutual_pars_if_present %>%
        arrange(
          -importance.x,
          -score.x
        ) %>%
        head(1)
      
      
      mutual_soft_par <- rel_df %>%
        filter(rel == RScoreDict$PARALLEL_IF_PRESENT) %>%
        filter((antecedent %in% c(sampled_soft_par$antecedent, sampled_soft_par$consequent)) |
                 (consequent %in% c(sampled_soft_par$antecedent, sampled_soft_par$consequent)))
      
      potential_pars <- mutual_soft_par %>%
        inner_join(mutual_soft_par,
                   c("antecedent"="consequent","consequent"="antecedent")) %>%
        pull(antecedent) %>%
        unique
      
      mutual_pars_if_present <- mutual_pars_if_present %>%
        filter(!c(antecedent %in% potential_pars & consequent %in% potential_pars))
      
      mutual_exclusives <- rel_df %>%
        filter(antecedent %in% potential_pars,
               consequent %in% potential_pars,
               rel == RScoreDict$MUTUALLY_EXCLUSIVE)
      
      if(mutual_exclusives %>% nrow > 0 & 
         mutual_exclusives %>% 
         inner_join(mutual_exclusives, 
                    by=c("antecedent"="consequent","consequent"="antecedent")) %>% 
         nrow == mutual_exclusives %>% nrow){
        
        selected_pair <- mutual_exclusives %>%
          arrange(-importance) %>%
          head(1)
        
        selected_exclusives <- mutual_exclusives %>%
          filter(antecedent %in% c(selected_pair$antecedent, selected_pair$consequent))
        
        return_list <- solve_XOR_relationship(
          XOR_root = "",
          XOR_branches = unique(selected_exclusives$antecedent),
          rel_df = rel_df,
          snippet_dict,
          split_symbol = ">X>")
        
        found_none = FALSE
        
        return(return_list)
      }
      
      
      other_relations <- rel_df %>%
        filter(antecedent %in% potential_pars) %>%
        filter(!(rel %in% c(RScoreDict$MUTUALLY_EXCLUSIVE)) ) %>%
        count(consequent,rel) %>%
        ungroup() %>%
        count(consequent)
      
      if(other_relations %>% pull(n) %>% max == 1){
        rel_df <- rel_df %>% remember_pair(
          sampled_soft_par,
          "OR"
        )
        
        return_list <- solve_PAR_relationship(
          sampled_soft_par,
          rel_df,
          snippet_dict,
          mode="SOFT"
        )
        
        found_none = FALSE
      }
      
    }
  }
  return(return_list)
}

fetch_mutual_par_if_present <- function(
    rel_df){
  par_if_present_df <- rel_df %>%
    filter(rel == RScoreDict$PARALLEL_IF_PRESENT)
  
  mutual_par_if_present <- par_if_present_df %>%
    inner_join(par_if_present_df,
               c("antecedent"="consequent","consequent"="antecedent"))
  
  return(mutual_par_if_present)
}