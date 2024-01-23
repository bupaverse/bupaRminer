explore_branch_pair <- function(
    branch_pair,
    rel_df){
  
  exploration_result <- list(
    pair = NULL,
    rel_type  = c("SEQ","PAR","PIP","XOR","AND",'OR'),
    branch_acts = c()
  )
  
  ## We register the pair to avoid infintie loops
  rel_df <- rel_df %>% 
    remember_pair(
      branch_pair,
      "BRANCH")
  
  all_mutual_branches <- fetch_mutual_branch_relationships(rel_df)
  rel_in_focus <- branch_pair %>%
    pull(rel)
  
  antecedent_contra_branch <- all_mutual_branches %>%
    filter(antecedent == branch_pair$antecedent,
           rel.x == rel_in_focus)
  consequent_contra_branch <- all_mutual_branches %>%
    filter(antecedent == branch_pair$consequent,
           rel.x == rel_in_focus)
  
  mutual_activity_set <-  
    c(branch_pair$antecedent, 
      branch_pair$consequent,
      antecedent_contra_branch %>%
        filter(consequent %in% consequent_contra_branch$consequent) %>%
        pull(consequent))
  
  
  activities_splitted_from_individual_branch <- rel_df %>%
    filter(!(antecedent %in% mutual_activity_set),
           consequent %in% mutual_activity_set,
           rel == rel_in_focus) %>%
    pull(antecedent)
  
  relationships_to_other_branches <- rel_df %>%
    filter(antecedent %in% activities_splitted_from_individual_branch,
           consequent %in% mutual_activity_set,
           rel != rel_in_focus)
  
  if(relationships_to_other_branches %>% nrow() > 0){
      
    sampled_pair <- relationships_to_other_branches %>%
      sample_pair(c())
    
    new_rel_in_focus <- sampled_pair %>%
      pull(rel)
    
    exploration_result$pair <- sampled_pair
    
    if(sampled_pair$rel %in% c(
      RScoreDict$DIRECTLY_FOLLOWS,
      RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
      RScoreDict$EVENTUALLY_FOLLOWS,
      RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
      RScoreDict$DIRECT_JOIN
    )){
      exploration_result$rel_type <- "SEQ"
      return(exploration_result)
    }
    if(sampled_pair$rel %in% c(RScoreDict$MUTUALLY_EXCLUSIVE,
                               RScoreDict$ALWAYS_PARALLEL,
                               RScoreDict$PARALLEL_IF_PRESENT)){
      ## Check reverse relation
      reverse_pair <- rel_df %>%
        filter(antecedent == sampled_pair$consequent,
               consequent == sampled_pair$antecedent,
               rel == sampled_pair$rel)
      
      if(reverse_pair %>% nrow > 0){
        ## If we have already been through this loop,
        ## then we will force the resolution
        ## otherwise, we explore further
        
        if(branch_pair %>% filter(inspection_sequence >= 2) %>% 
           nrow == 0){
          exploration_result <- explore_branch_pair(
            sampled_pair,
            rel_df
          )
          return(exploration_result)
        }
      }
    }
  }
  
  ## We have to examine the activities before and after the branches
  ## It is possible that we need to explore within a branch
  ## before we create the XOR, OR or AND split.
  follows_act_in_branch <- rel_df %>%
    filter(antecedent %in% mutual_activity_set,
           rel %in% c(RScoreDict$DIRECT_JOIN,
                      RScoreDict$DIRECTLY_FOLLOWS,
                      RScoreDict$EVENTUALLY_FOLLOWS))
  
  follows_only_one <- follows_act_in_branch %>%
    count(consequent) %>%
    filter(n == 1) %>%
    pull(consequent)
  
  
  if(length(follows_only_one) > 0){
    sequence_pair <- rel_df %>%
      filter(antecedent %in% mutual_activity_set,
             consequent %in% follows_only_one) %>%
      sample_pair(rel_vect = c(RScoreDict$DIRECTLY_FOLLOWS,
                               RScoreDict$DIRECT_JOIN,
                               RScoreDict$EVENTUALLY_FOLLOWS))
    
    exploration_result$pair <- sequence_pair
    exploration_result$rel_type <- "SEQ"
    return(exploration_result)
  }
  
  ## If we have activities that require only one of the
  ##branches, then we can connect them first
  ## but only if there is no long-term dependency
  acts_requiring_branch <- rel_df %>%
    filter(consequent %in% mutual_activity_set,
           rel == RScoreDict$REQUIRES)
  
  acts_requiring_single_branch <- acts_requiring_branch %>%
    count(antecedent) %>%
    filter(n == 1)
  
  while(acts_requiring_single_branch %>% nrow > 0){
    seq_pair <- rel_df %>%
      filter(
        consequent %in% acts_requiring_single_branch$antecedent,
        antecedent %in% mutual_activity_set) %>%
      sample_pair(c(
        RScoreDict$DIRECTLY_FOLLOWS,
        RScoreDict$EVENTUALLY_FOLLOWS,
        RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
        RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
        RScoreDict$DIRECT_JOIN
      ))
    
    if(is.null(seq_pair)){
      seq_pair <- tibble()
      acts_requiring_single_branch <- tibble()
    }
    
    if(seq_pair %>% nrow > 0){
      closest_antecedents <- get_closest_antecedents(
        seq_pair,
        rel_df
      )
      ## If there is nothing between the branch and
      ## the REQ consequent, then we can create the
      ## connection
      if(closest_antecedents %>% 
         filter(!antecedent %in% mutual_activity_set) %>% 
         nrow == 0){
        exploration_result$pair <- seq_pair
        exploration_result$rel_type <- "SEQ"
        return(exploration_result)
      }
      acts_requiring_single_branch <- acts_requiring_single_branch %>%
        filter(antecedent != seq_pair$consequent)
    }
  }
  
  
  exploration_result$pair <- branch_pair
  exploration_result$rel_type <- branch_pair$rel
  exploration_result$branch_acts <- c(
    branch_pair$antecedent,
    branch_pair$consequent
  )
  return(exploration_result)
}

solve_branch_pair <- function(
    exploration_result,
    rel_df,
    snippet_dict
){
  return_list <- list(
    snippet = NULL,
    activities = c(),
    rel_df = rel_df,
    snippet_dictionary = snippet_dict,
    messages = c()
  )
  
  branch_pair <- exploration_result$pair 
  if(exploration_result$rel_type == "SEQ"){
    return_list <- solve_sequence_relationship(
      branch_pair,
      rel_df,
      snippet_dict
    )
  } else if(exploration_result$rel_type %in% 
            c(RScoreDict$PARALLEL_IF_PRESENT, RScoreDict$ALWAYS_PARALLEL)){
    if(exploration_result$rel_type==RScoreDict$PARALLEL_IF_PRESENT){
      mode <- "SOFT"
    } else {
      mode <- "HARD"
    }
    par_pair <- exploration_result$pair
    return_list <- solve_PAR_relationship(
      par_pair,
      rel_df,
      snippet_dict,
      mode = mode
    )
  } else if(exploration_result$rel_type == RScoreDict$MUTUALLY_EXCLUSIVE){
    branches <- exploration_result$branch_acts
    return_list <- solve_XOR_relationship(
      NULL,
      branches,
      rel_df,
      snippet_dict)
  }else {
    print(exploration_result)
  }
  return(return_list)
}

fetch_mutual_branch_relationships <- function(
    rel_df,
    starting_pairs_df = NULL){
  
  if(!is.null(starting_pairs_df)){
    rel_df <- rel_df %>%
      anti_join(starting_pairs_df,
                by=c("antecedent"="antecedent","consequent"="consequent"))
  }
  
  branch_rel_df <- rel_df %>%
    filter(rel %in% c(RScoreDict$PARALLEL_IF_PRESENT,
                      RScoreDict$ALWAYS_PARALLEL,
                      RScoreDict$MUTUALLY_EXCLUSIVE))
  
  if(branch_rel_df %>% nrow == 0){
    return(branch_rel_df)
  }
  
  mutual_branch_rel_df <- branch_rel_df %>%
    inner_join(branch_rel_df,
               c("antecedent"="consequent","consequent"="antecedent")) %>%
    filter(rel.x == rel.y)
  
  return(mutual_branch_rel_df)
}