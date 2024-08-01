extract_branch_names <- function(branches, 
                                 rel_df){
  branch_names <- rep("", length(branches))
  
  if(rel_df %>% nrow == 0){
    return(branch_names)
  }
  if(length(branches) < 2){
    return(branch_names)
  }
  
  excl_relations <- rel_df %>%
    filter(rel==RScoreDict$MUTUALLY_EXCLUSIVE) %>%
    filter(antecedent %in% branches,
           !(consequent %in% branches))
  
  exclusive_excl_rel <- excl_relations %>%
    group_by(consequent) %>%
    mutate(n = n()) %>%
    filter(n==1)
  
  req_relations <- rel_df %>%
    filter(rel==RScoreDict$REQUIRES) %>%
    filter(antecedent %in% branches)
  
  exclusive_req_rel <- req_relations %>%
    group_by(consequent) %>%
    mutate(n = n()) %>%
    filter(n==1)
  
  if(exclusive_excl_rel %>% nrow > 0 || exclusive_req_rel %>% nrow > 0){
    b_counter <- 0
    for(branch in branches){
      b_counter <- b_counter+1
      branch_req_rels <- exclusive_req_rel %>%
        filter(antecedent == branch)
      
      if(branch_req_rels %>% nrow > 0){
        branch_names[b_counter] <- paste(
          branch_names[b_counter], 
          paste("ONLY AFTER", branch_req_rels$consequent, sep = " "),
          collapse = " ",)
      }
      
      branch_excl_rels <- exclusive_excl_rel %>%
        filter(antecedent == branch)
      
      if(branch_excl_rels %>% nrow > 0){
        branch_names[b_counter] <- paste(
          branch_names[b_counter], 
          paste("NEVER WITH", branch_req_rels$consequent, sep = " "),
          collapse = " ",)
      }
    }
  }
  
  return(branch_names)
}