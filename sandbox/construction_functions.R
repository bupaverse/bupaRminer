sample_pair <- function(
    rel_df,
    rel_vect){
  
  domain <- rel_df %>% 
    filter(rel %in% c(rel_vect)) 
  
  sampled_pair <- NULL
  if(domain %>% nrow() > 0){
    sampled_pair <- domain %>%
      arrange(-importance,
              -score) %>%
      head(1)
  }
  
  return(sampled_pair)
}


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


solve_interrupt_relationship <- function(
    rel_pair,
    rel_df){
  
  return_list <- list(
    snippet = NULL,
    activities = c(),
    rel_df = rel_df,
    messages = c()
  )
  
  antec <- rel_pair$antecedent
  conseq <- rel_pair$consequent
  
  if(rel_pair$rel == RScoreDict$TERMINATING){
    intermed_symbol <- " >oo> "
  }
  if(rel_pair$rel == RScoreDict$HAPPENS_DURING){
    intermed_symbol <- " >o> "
  }
    
  snippet_name <- paste(antec, intermed_symbol, conseq, " >O> ", sep = "")
    
  msg <- paste("Created process snippet:", snippet_name, sep = " ")
    
  return_list <- list(
    snippet = snippet_name,
    activities = c(antec, conseq),
    rel_df = rel_df,
    messages = msg
  )
  
  return(return_list)
}

solve_DF_relationship <- function(
    rel_pair,
    rel_df){
  
  return_list <- list(
    snippet = NULL,
    activities = c(),
    rel_df = rel_df,
    messages = c()
  )
  
  antec <- rel_pair$antecedent
  conseq <- rel_pair$consequent
  
  ## We need to check if other important
  ## relationships towards B exist.
  ## F.e. if there are 2 DF relationships,
  ## Then there should be a join before B
  ## and o direct flow between A and B.
  other_conseq_rels <- rel_df %>% 
    filter(consequent == conseq)
  
  ## We also need to check the relation
  ## of B towards A.
  relation_from_b_to_a <- rel_df %>% 
    filter(
      antecedent == conseq,
      consequent == antec) %>%
    pull(rel)
  
  ## If we have multiple DF relationships towards 
  ## the same consequent, then this means
  ## that we actually are performing a join.
  ## If A -> B = DF and B->A != REQ, then this
  ## means that A is a rare optional activity
  ## that directly preceeds B if it happens.
  if(other_conseq_rels %>%
     filter(rel == RScoreDict$DIRECTLY_FOLLOWS) %>%
     nrow() > 1  || ( length(relation_from_b_to_a) > 0 && relation_from_b_to_a != RScoreDict$REQUIRES)){
    
    ## ifelse functions hate factors
    ## therefore, we use a detour
    
    new_rows <- other_conseq_rels %>%
      filter(rel == RScoreDict$DIRECTLY_FOLLOWS) %>%
      mutate(rel = RScoreDict$DIRECT_JOIN )
    rel_df <- rel_df %>%
      filter(!(consequent == conseq & rel == RScoreDict$DIRECTLY_FOLLOWS )) %>%
      bind_rows(new_rows)
    
    msg <- (paste("Morphed", other_conseq_rels %>%
                    filter(rel ==RScoreDict$DIRECTLY_FOLLOWS) %>%
                    nrow(), RScoreDict$DIRECTLY_FOLLOWS, "relationships to", RScoreDict$DIRECT_JOIN))
    
    return_list <- list(
      snippet = NULL,
      activities = c(),
      rel_df = rel_df,
      messages = msg
    )
    
  } else{
    
    snippet_name <- paste(antec, conseq, sep = " >> ")
    
    msg <- paste("Created process snippet:", snippet_name, sep = " ")
    
    return_list <- list(
      snippet = snippet_name,
      activities = c(antec, conseq),
      rel_df = rel_df,
      messages = msg
    )
  }
}

explore_soft_PAR_relationship <- function(rel_df){
  
  return_list <- list(
    snippet = NULL,
    activities = c(),
    rel_df = rel_df,
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
      
      other_relations <- rel_df %>% 
        filter(antecedent %in% potential_pars) %>% 
        filter(!(rel %in% c(RScoreDict$MUTUALLY_EXCLUSIVE)) ) %>%
        count(consequent,rel) %>%
        ungroup() %>%
        count(consequent)
      
      if(other_relations %>% pull(n) %>% max == 1){
        return_list <- solve_PAR_relationship(
          sampled_soft_par,
          rel_df,
          mode="SOFT"
        )
        
        found_none = FALSE
      }
      
    }
  }
  return(return_list)
}

solve_PAR_relationship <- function(
    rel_pair,
    rel_df,
    mode = "HARD"
){
  
  return_list <- list(
    snippet = NULL,
    activities = c(),
    rel_df = rel_df,
    messages = c()
  )
  
  valid_relationships <- c(RScoreDict$ALWAYS_PARALLEL)
  
  if(mode == "SOFT"){
    valid_relationships <- c(RScoreDict$ALWAYS_PARALLEL, RScoreDict$PARALLEL_IF_PRESENT)
  }
  
  antec <- rel_pair$antecedent
  conseq <- rel_pair$consequent
  
  ## Check all PAR rels of A
  R6_with_antec <- rel_df %>% 
    filter(antecedent == antec,
           rel %in% valid_relationships) %>%
    pull(consequent)
  
  ## Check all PAR rels of B
  R6_with_conseq <- rel_df %>% 
    filter(antecedent == conseq,
           rel %in% valid_relationships) %>%
    pull(consequent)
  
  ## We create a vector of all activities that
  ## happen in parallel (R6) with both A and B
  R6_acts <- c(antec,conseq,intersect(R6_with_antec, R6_with_conseq))
  
  ## We can't just put the R6_acts in a parallel block
  ## we need to check whether any of these activities
  ## is actually part of a branch with multiple
  ## Activities in it. Theregore, for each activity in
  ## the split, we need to check if there are more 
  ## down the line
  acts_happening_after_split <- rel_df %>%
    filter(antecedent %in% R6_acts, 
           rel %in% c(RScoreDict$DIRECTLY_FOLLOWS,
                      RScoreDict$EVENTUALLY_FOLLOWS,
                      RScoreDict$MAYBE_DIRECTLY_FOLLOWS))
  
  acts_after_split <- acts_happening_after_split %>%
    pull(consequent) %>%
    unique
  
  ## If none of the branches has a follows-relationship,
  ## the split can be created otherwise, we have to check.
  if(acts_happening_after_split %>% nrow > 0){
    
    relations_after_split <- acts_happening_after_split %>% 
      count(consequent, rel) %>% 
      group_by(consequent) %>% 
      mutate(min_n = min(n), max_n = max(n)) %>% 
      mutate(all_same = (n == length(R6_acts)) & (min_n == max_n))
    
    ## If all follows relationships are the same for all branches
    ## then those branches can be merged first into a split
    ## Otherwise, we have to check.
    if(relations_after_split %>% filter(all_same == FALSE) %>% nrow() > 0){
      mutual_relationships <- rel_df %>%
        filter(antecedent %in% R6_acts, 
               consequent %in% acts_after_split)
      
      ## If there are only follows relationships, then we assume we can proceed, 
      ## otherwise we have to check 
      if(mutual_relationships %>% 
         count(consequent, rel) %>% 
         filter(rel %in% c(RScoreDict$ALWAYS_PARALLEL,
                           RScoreDict$PARALLEL_IF_PRESENT,
                           RScoreDict$MUTUALLY_EXCLUSIVE), n > 0) %>% 
         nrow() > 0){
        
        ## If many of the follows relationships from 1 branch of the split
        ## Can't happen together with other branches from the split.
        ## then we have reason to doubt whether the split should actually
        ## be created.
        if(mutual_relationships %>% 
           count(consequent, rel) %>% 
           filter(rel == RScoreDict$MUTUALLY_EXCLUSIVE, n >= (length(R6_acts) / 2)) %>%
           filter(n > 1) %>% 
           nrow() > 0){
          
          return_list$messages <- c(return_list$messages, 
                                    "TOO MANY CONFLICTS. REL will change.")
          
          ## We need tp remove the R6 relationships that we are doubting.
          ## We do this only for the activities that created the conflict
          ## Or, if the split consisted of just two activities, for both.
          if(length(R6_acts) == 2){
            invalid_R6_acts <- R6_acts
          } else {
            invalid_R6_acts <- mutual_relationships %>% 
              filter(rel == RScoreDict$MUTUALLY_EXCLUSIVE) %>%
              pull(antecedent) %>%
              unique
          }
          
          
          ## Create the new rows and update the rel_df
          new_rows <- rel_df %>%
            filter(antecedent %in% invalid_R6_acts,
                   consequent %in% R6_acts) %>%
            mutate(rel = RScoreDict$PARALLEL_IF_PRESENT)
          rel_df <- rel_df %>%
            filter(!(antecedent %in% invalid_R6_acts & consequent %in% R6_acts)) %>%
            bind_rows(new_rows)
          
          return_list$messages <- c(return_list$messages, 
                                    paste("Morphed", 
                                          new_rows %>%
                                            nrow(),
                                          RScoreDict$ALWAYS_PARALLEL,
                                          "relationships to SOFT PAR."))
          
          return_list$rel_df <- rel_df
          
          return(return_list)
          
        }
        
        ## Suppose a potential parallel split over A, B and C.
        ## Suppose an activity D that has an R2 or R4 relationship
        ## with A and B.
        ## If activity D has an R5 relationship with C,
        ## then we raise a conflict but we will create the split
        ## anyway.
        mutual_relationships_overview <- mutual_relationships %>% 
          group_by(consequent, rel) %>%
          mutate(count_of_this_rel = n()) %>%
          mutate(
            EXCL_count = ifelse(
              as.character(rel) == RScoreDict$MUTUALLY_EXCLUSIVE, 
              count_of_this_rel, 0),
            PAR_count = ifelse(
              as.character(rel) == RScoreDict$ALWAYS_PARALLEL, 
              count_of_this_rel, 0),
          ) %>% 
          mutate(
            has_EXCL = sum(EXCL_count) > 0,
            has_PAR = sum(PAR_count) > 0) %>%
          ungroup()
        
        rels_with_conflicts <- mutual_relationships_overview %>%
          filter(has_EXCL == TRUE, has_PAR == FALSE) %>%
          filter(rel == RScoreDict$MUTUALLY_EXCLUSIVE)
        
        return_list$messages <- c(return_list$messages,
                                  paste("CONFLICT ----",
                                        rels_with_conflicts$antecedent, 
                                        " AND ",
                                        rels_with_conflicts$consequent,
                                        " not expected together.", sep = ""))
        
      }
      
    }
  }
  
  new_acts <- c()
  
  for(act in R6_acts){
    if(startsWith(act,"++[") & endsWith(act,"]++")){
      new_acts <- c(new_acts, gsub('^\\+\\+\\[|\\]\\+\\+$', '', act))
    } else{
      new_acts <- c(new_acts, act)
    }
  }
  
  ## If we want to parallellize with an already existing parallel split
  ## we can just incorporate the new addition into the existing one.
  
  PAR_SYMBOL_START = "++["
  PAR_SYMBOL_END = "]++"
  
  if(mode == "SOFT"){
    ## branches that aren't required from START are made optional
    rels_from_start <- rel_df %>%
      filter((antecedent == "START" | startsWith(antecedent, "START ") ),
             consequent %in% R6_acts) %>%
      filter(rel == RScoreDict$EVENTUALLY_FOLLOWS)
    
    required_from_start <- rels_from_start$consequent
    
    if(length(required_from_start) > 0){
      optional_acts <- setdiff(R6_acts, required_from_start)
      
      if(length(optional_acts) > 0){
        optional_acts <- paste(">X>[",optional_acts,"]>X>", sep="")
        new_acts <- c(required_from_start, optional_acts)
      }
    } else {
      PAR_SYMBOL_START = ">O>["
      PAR_SYMBOL_END = "]>O>"
    }
  }
  
  return_list$snippet <- paste(PAR_SYMBOL_START,paste(new_acts, collapse = ","),PAR_SYMBOL_END,sep="")
  return_list$activities <- R6_acts
  return_list$rel_df <- rel_df
  return_list$messages <- c(return_list$messages,
                            paste("Created process snippet:", 
                                  return_list$snippet, sep = " "))
  
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

fetch_sequence_antecedents <- function(
    rel_df){
  
  relevant_antec <- c()
  
  ## We are searching for starting points of a sequence relationship
  
  ## We start by fetching the starting points (antecedents) of R2 relationships
  if(rel_df %>%
     filter(rel == RScoreDict$EVENTUALLY_FOLLOWS) %>%
     nrow() > 0){
    
    ## We fetch all starting points of an R2 relationship
    all_antec_of_R2 <- rel_df %>%
      filter(rel == RScoreDict$EVENTUALLY_FOLLOWS) %>%
      pull(antecedent) %>%
      unique
    
    ## We check which of the R2 starting points (antecedentà are the end point 
    ## (consequent) of any follows relationship themselves.
    ## This indicates that these are not the first acitivities in a branch. 
    all_antecedents_with_preceeding_R2 <- rel_df %>%
      filter(consequent %in% all_antec_of_R2,
             rel %in% c(RScoreDict$DIRECTLY_FOLLOWS,
                        RScoreDict$EVENTUALLY_FOLLOWS,
                        RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                        RScoreDict$MAYBE_EVENTUALLY_FOLLOWS)) %>%
      count(consequent, rel) %>%
      pull(consequent)
    
    ## We retain all starting points (antecedents) that are not an ending point (consequent)
    ## themselves. You can consider these activities the "earliest" activitities in a branch
    relevant_antec <- all_antec_of_R2[! all_antec_of_R2 %in% all_antecedents_with_preceeding_R2]
  }
  
  ## If we do not find this for R2 activities,
  ## then we do this for any follows or eventually follows relationship.
  if(length(relevant_antec) == 0){
    
    all_antec_of_R2 <- rel_df %>%
      filter(rel %in% c(RScoreDict$DIRECTLY_FOLLOWS,
                        RScoreDict$EVENTUALLY_FOLLOWS,
                        RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                        RScoreDict$MAYBE_EVENTUALLY_FOLLOWS)) %>%
      pull(antecedent) %>%
      unique
    
    all_antecedents_with_preceeding_R2 <- rel_df %>%
      filter(consequent %in% all_antec_of_R2,
             rel %in% c(RScoreDict$DIRECTLY_FOLLOWS,
                        RScoreDict$EVENTUALLY_FOLLOWS,
                        RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                        RScoreDict$MAYBE_EVENTUALLY_FOLLOWS)) %>%
      count(consequent, rel) %>%
      pull(consequent)
    
    relevant_antec <- all_antec_of_R2[! all_antec_of_R2 %in% all_antecedents_with_preceeding_R2]
    
  }
  
  return(relevant_antec)
}


solve_sequence_relationship <- function(
    rel_pair,
    rel_df
){
  
  return_list <- list(
    snippet = NULL,
    activities = c(),
    rel_df = rel_df,
    messages = c()
  )
  
  antec <- rel_pair$antecedent
  conseq <- rel_pair$consequent
  relevant_relation <- rel_pair$rel
  
  ## We check if what activities are likely to preceed
  ## the current end_point
  others_preceeding_conseq <- rel_df %>%
    filter(consequent == conseq) %>%
    filter(rel %in% c(RScoreDict$DIRECTLY_FOLLOWS,
                      RScoreDict$EVENTUALLY_FOLLOWS,
                      RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                      RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
                      RScoreDict$DIRECT_JOIN))
  
  SEQ_FOUND <- TRUE
  ## If there is only one predecessor, then antec and conseq 
  ## are the pair that we want to connect
  ## If not, we need to perform further analysis
  if(others_preceeding_conseq %>% nrow() > 1){
    SEQ_FOUND <- FALSE
    ## If there are more predecessors, we have to fetch the closest
    ## predecessor. This is the antecedent that is the predecessor of
    ## none of the other antecedents.
    
    ## So, first we need the antecedents of the antecedent
    new_end_points <- others_preceeding_conseq$antecedent %>% unique
    current_end_point <- c(conseq, new_end_points)
    
    others_preceeding_conseq <- rel_df %>%
      filter(consequent %in% current_end_point) %>%
      filter(rel %in% c(RScoreDict$DIRECTLY_FOLLOWS,
                        RScoreDict$EVENTUALLY_FOLLOWS,
                        RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                        RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
                        RScoreDict$DIRECT_JOIN))
    
    ## Then we count how often an activity is an antecedent .
    ## The activity that is directly before the original end_point
    ## will only be the antecedent to 1 activity, namely the endpoint 
    closest_antecedents <- others_preceeding_conseq %>% 
      group_by(antecedent) %>%
      mutate(nr_connections = n()) %>%
      filter(consequent == conseq) %>%
      ungroup() %>%
      filter(nr_connections == 1)    
    
    ## If there are no closest conseqs, this
    ## means that the metrics indicate that
    ## A -> B and that B -> A. Which is a contradiction.
    ## In this case, we allow the strongest relationship
    ## to override. F.e. A R2 B is stronger than B R4 A.
    ## So we drop B R4 A.
    if(closest_antecedents %>% nrow() == 0){
      ## We search for the antecedents that are least often
      #" an antecedent.
      closest_antecedents <- others_preceeding_conseq %>% 
        group_by(antecedent) %>%
        mutate(nr_connections = n()) %>%
        filter(consequent == conseq) %>%
        ungroup() %>%
        filter(nr_connections == min(nr_connections))
      
      if(closest_antecedents %>% nrow() > 1){
        mutual_relationships <-  others_preceeding_conseq %>%
          filter(antecedent %in% closest_antecedents$antecedent,
                 consequent %in% closest_antecedents$antecedent)
        
        if(mutual_relationships %>% nrow == 1){
          return_list <- solve_sequence_relationship(
            mutual_relationships,
            rel_df
          )
          return(return_list)
        }
        
        ## And we allow the strongest relationship to prevail
        strongest_mutual_relationship <- mutual_relationships %>%
          filter(rel == min(rel))
        
        ## And we perform the operation again
        closest_antecedents <- others_preceeding_conseq %>% 
          filter(!(antecedent %in% closest_antecedents$antecedent &
                     consequent %in% closest_antecedents$antecedent)) %>%
          bind_rows(strongest_mutual_relationship) %>%
          group_by(antecedent) %>%
          mutate(nr_connections = n()) %>%
          filter(consequent == conseq) %>%
          ungroup() %>%
          filter(nr_connections == 1)
      }
      
      if(closest_antecedents %>% nrow() == 1){
        antec <- closest_antecedents$antecedent
        conseq <- closest_antecedents$consequent
        relevant_relation <- closest_antecedents$rel
        SEQ_FOUND <- TRUE
      } else {
        ## If there is still no clear closest conseq, 
        ## then A and B habe the same follows relationship towards
        ## each other. THis is a contradiction.
        ## We solve it by considereing A and B as a parallel path
        closest_antecedents <- others_preceeding_conseq %>% 
          group_by(antecedent) %>%
          mutate(nr_connections = n()) %>%
          filter(consequent == conseq) %>%
          ungroup() %>%
          filter(nr_connections == min(nr_connections))
        
        relevant_pairs <- others_preceeding_conseq %>%
          filter(antecedent %in% closest_antecedents$antecedent,
                 consequent %in% closest_antecedents$antecedent) %>%
          filter(rel == min(rel)) %>%
          mutate(rel = RScoreDict$ALWAYS_PARALLEL)
        
        return_list <- solve_PAR_relationship(
          relevant_pairs %>% arrange(
            -importance,
            -score
            ) %>%
            head(1),
          relevant_pairs
        )
        
        ## We only examined on a partial log.
        ## We need to maintain the entire log though
        return_list$rel_df <- rel_df
        return(return_list)
      }
      
    }
    
    ## If we have multiple direct predecessors
    ## then we have to examine 
    if(closest_antecedents %>% nrow() > 1){
      ## If all direct precedents are Rx
      ## Then we actually need to look for a split
      ## We try to create a new pair using one of the branches.
      if(closest_antecedents %>% count(rel) %>% filter(rel != RScoreDict$DIRECT_JOIN) %>% nrow == 0){
        new_pair <- rel_df %>%
          filter(consequent %in% closest_antecedents$antecedent,
                 rel %in% c(RScoreDict$DIRECTLY_FOLLOWS,
                            RScoreDict$EVENTUALLY_FOLLOWS,
                            RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                            RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
                            RScoreDict$DIRECT_JOIN)) %>%
          arrange(-importance,
                  -score
                  ) %>%
          head(1)
        return_list <- solve_sequence_relationship(
          new_pair,
          rel_df)
        return(return_list)
      }
      
      ## We will give preference to R1, R3 relationships if
      ## they exist
      
      R2_closest <- closest_antecedents %>%
        filter(rel == RScoreDict$EVENTUALLY_FOLLOWS)
      
      if(R2_closest %>% nrow > 0) closest_antecedents <- R2_closest
      
      R13_closest <- closest_antecedents %>%
        filter(rel %in% c(RScoreDict$DIRECTLY_FOLLOWS,
                          RScoreDict$MAYBE_DIRECTLY_FOLLOWS))
      
      if(R13_closest %>% nrow > 0) closest_antecedents <- R13_closest
      
      if(closest_antecedents %>% nrow == 1){
        seq_pair <- closest_antecedents
        
        return_list <- solve_directly_follows(
          seq_pair,
          closest_antecedents
        )
        
        return_list$rel_df <- rel_df
        
        return(return_list)
      } else {
        ## Otherwise we must examine the mutual relationship
        ## between the closest antecedents.
        mutual_antec_relations <- rel_df %>%
          filter(antecedent %in% closest_antecedents$antecedent,
                 consequent %in% closest_antecedents$antecedent)
        
        ## If the antecedents are mutually exclusive
        ## XOR them together
        if(mutual_antec_relations %>% 
           count(rel) %>% 
           filter(rel != RScoreDict$MUTUALLY_EXCLUSIVE) %>% 
           nrow== 0){
          
          return_list <- solve_XOR_relationship("",
                                                mutual_antec_relations$antecedent %>% unique,
                                                mutual_antec_relations)
          return_list$rel_df <- rel_df
          return(return_list)
        }
        
        if(RScoreDict$DIRECT_JOIN %in% mutual_antec_relations$rel){
          
          selected_branches <- mutual_antec_relations %>%
            filter(rel == RScoreDict$DIRECT_JOIN) %>%
            arrange(-importance,
                    -score
                    )
            head(1)
          
          selected_branches <- mutual_antec_relations %>%
            filter(rel == RScoreDict$DIRECT_JOIN,
                   consequent %in% selected_branches$consequent)
          
          return_list <- solve_XOR_relationship("",
                                                selected_branches$antecedent %>% unique,
                                                mutual_antec_relations)
          return_list$rel_df <- rel_df
          return(return_list)
        }
        
        ## If they have a mutual soft parallel relationship
        ## we can soft par them together 
        if(mutual_antec_relations %>% 
           count(rel) %>% 
           filter(rel != RScoreDict$PARALLEL_IF_PRESENT) %>% 
           nrow== 0){
          
          sampled_soft_par <- mutual_antec_relations %>%
            arrange(-importance,
                    -score
                    ) %>%
            head(1)
          
          return_list <- solve_PAR_relationship(
            sampled_soft_par,
            rel_df,
            mode="SOFT"
          )
          
          return(return_list)
        }
        
        ## If one requires the other, we first need to create a split on the
        ## consequent
        if(RScoreDict$REQUIRES %in% mutual_antec_relations$rel){
          REQ_pair <- mutual_antec_relations %>% 
            filter(rel == RScoreDict$REQUIRES) %>%
            arrange(-importance,
                    -score
                    ) %>%
            head(1)
          
          SEQ_pair <- rel_df %>%
            filter(antecedent == REQ_pair$consequent,
                   consequent == REQ_pair$antecedent)
          
          if(SEQ_pair %>% nrow() == 0){
            SEQ_pair <- tibble(
              antecedent = REQ_pair$consequent,
              consequent = REQ_pair$antecedent,
              rel = RScoreDict$MAYBE_EVENTUALLY_FOLLOWS
            )
            rel_df <- rel_df %>%
              bind_rows(SEQ_pair)
          }
          if(SEQ_pair$rel %in% c(RScoreDict$MUTUALLY_EXCLUSIVE, RScoreDict$PARALLEL_IF_PRESENT)){
            SEQ_pair$rel <- RScoreDict$MAYBE_EVENTUALLY_FOLLOWS
            rel_df <- rel_df %>%
              filter(!(antecedent == SEQ_pair$antecedent & consequent == SEQ_pair$consequent)) %>%
              bind_rows(SEQ_pair)
          }
          if(SEQ_pair$rel %in% c(RScoreDict$ALWAYS_PARALLEL, RScoreDict$REQUIRES)){
            return_list <- solve_PAR_relationship(
              SEQ_pair %>% mutate(rel == RScoreDict$ALWAYS_PARALLEL),
              rel_df %>%
                filter(antecedent %in% c(SEQ_pair$antecedent, SEQ_pair$consequent),
                       consequent %in% c(SEQ_pair$antecedent, SEQ_pair$consequent)) %>%
                mutate(rel = RScoreDict$ALWAYS_PARALLEL)
            )
            return_list$rel_df <- rel_df
            return(return_list)
          }
          antec <- SEQ_pair$antecedent
          conseq <- SEQ_pair$consequent
          relevant_relation <- SEQ_pair$rel
          SEQ_FOUND <- TRUE
        }
        
        ## If they are concurrent or do not agree
        ## among each other
        ## AND them together
      }
    }
    
    
    ## If there is only 1 closest conseq, then
    ## we have our pair.
    if(closest_antecedents %>% nrow() == 1){
      antec <- closest_antecedents$antecedent
      conseq <- closest_antecedents$consequent
      relevant_relation <- closest_antecedents$rel
      SEQ_FOUND <- TRUE
    }
    
    
  }
  
  if(!SEQ_FOUND){
    return_list$messages <- "--------TODO--------- No logic implemented when no seq is found."
    return_list$rel_df <- rel_df
    return(return_list) 
  }
  
  if(SEQ_FOUND & relevant_relation %in% c(RScoreDict$DIRECTLY_FOLLOWS, 
                                          RScoreDict$EVENTUALLY_FOLLOWS)){
    seq_pair <- rel_df %>%
      filter(antecedent == antec,
             consequent == conseq)
    
    return_list <- solve_directly_follows(
      seq_pair,
      rel_df
    )
    
    return(return_list)
  }
  
  if(SEQ_FOUND & relevant_relation %in% c(RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                                          RScoreDict$MAYBE_EVENTUALLY_FOLLOWS)){
    XOR_pair <- rel_df %>% filter(antecedent == antec,
                                  consequent == conseq)
    return_list <- explore_XOR_split(
      XOR_pair,
      rel_df,
      XOR_rels = c(RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                   RScoreDict$MAYBE_EVENTUALLY_FOLLOWS)
    )
    return(return_list) 
  }
  
  if(SEQ_FOUND & relevant_relation == RScoreDict$DIRECT_JOIN){
    join_pair <- rel_df %>%
      filter(antecedent == antec,
             consequent == conseq)
    
    return_list <- solve_join(
      join_pair,
      rel_df
    )
    
    return(return_list)
  }
}

solve_join <- function(
  join_pair,
  rel_df
){
  reverse_rel <- rel_df %>%
    filter(antecedent == join_pair$consequent,
           consequent == join_pair$antecedent)
  
  if(reverse_rel %>% nrow == 0){
    reverse_rel <- ""
  } else {
    reverse_rel <- reverse_rel$rel
  }
  
  if(reverse_rel %in% c("", RScoreDict$MUTUALLY_EXCLUSIVE)){
    return_list <- solve_XOR_relationship(
      XOR_root = "",
      XOR_branches = join_pair$antecedent,
      rel_df = rel_df
    )
    return(return_list)
  }
  
  if(reverse_rel == RScoreDict$REQUIRES){
    return_list <- solve_directly_follows(
      join_pair %>%
        mutate(rel == RScoreDict$DIRECTLY_FOLLOWS),
      join_pair %>%
        mutate(rel == RScoreDict$DIRECTLY_FOLLOWS)
    )
    return_list$rel_df <- rel_df
    return(return_list)
  }
  
  if(reverse_rel == RScoreDict$MAYBE_EVENTUALLY_FOLLOWS){
    rel_df <- rel_df %>%
      filter(!(antecedent == join_pair$consequent &
               consequent == join_pair$antecedent))
    
    return_list$rel_df <- rel_df
    return_list$messages <- c(return_list$messages,"Removed conflicting MAYBE EVENTUALLY FOLLOWS relation")
    
    return(return_list)
  }
}

solve_directly_follows <- function(
    seq_pair,
    rel_df){
  
  act_a <- seq_pair$antecedent
  act_b <- seq_pair$consequent
  
  
  return_list <- list(
    snippet = NULL,
    activities = c(),
    rel_df = rel_df,
    messages = c()
  )
  
  
  if(act_a == "START" & act_b == "END"){
    
    return_list <- list(
      snippet = NULL,
      activities = c(),
      rel_df = rel_df %>%
        filter(!(antecedent == "START" & consequent == "END")),
      messages = "Attempt to directly sequence START and END event canceled."
    )
    return(return_list)
  }
  
  if(startsWith(act_a, "START") & act_b == "END" & rel_df %>% nrow > 2){
    
    return_list <- list(
      snippet = NULL,
      activities = c(),
      messages = "Attempt to append END event canceled.",
      rel_df = rel_df %>%
        filter(!(antecedent == act_a & consequent == act_b))
    )
    return(return_list)
  }
  
  reverse_rel <- rel_df %>%
    filter(antecedent == act_b,
           consequent == act_a) %>%
    pull(rel)
  
  if(rel_df %>%
     filter(antecedent == act_b,
            consequent == act_a) %>% nrow == 0) reverse_rel <- ""
  
  if(reverse_rel %in% c("",RScoreDict$REQUIRES)){
    snippet_name <- paste(act_a, act_b, sep = " >> ")
    
    msg <- paste("Created process snippet:", snippet_name, sep = " ")
    
    return_list <- list(
      snippet = snippet_name,
      activities = c(act_a, act_b),
      rel_df = rel_df,
      messages = msg
    )
    
    return(return_list)
  }
  
  if(reverse_rel %in% c(RScoreDict$MAYBE_EVENTUALLY_FOLLOWS, RScoreDict$MUTUALLY_EXCLUSIVE)){
    new_rows <- seq_pair %>%
      mutate(rel = RScoreDict$DIRECT_JOIN)
    
    rel_df <- rel_df %>%
      filter(!(consequent == act_b & antecedent == act_a)) %>%
      bind_rows(new_rows)
    
    msg <- (paste("Morphed", new_rows %>%
                    nrow(), "relationships to", RScoreDict$DIRECT_JOIN))
    
    return_list <- list(
      snippet = NULL,
      activities = c(),
      rel_df = rel_df,
      messages = msg
    )
    
    return(return_list)
  }
  
  if(reverse_rel %in% c(seq_pair$rel, RScoreDict$ALWAYS_PARALLEL, RScoreDict$PARALLEL_IF_PRESENT)){
    AND_pair <- seq_pair
    
    relevant_pairs <- rel_df %>%
      filter(antecedent %in% c(AND_pair$antecedent, AND_pair$consequent),
             consequent %in% c(AND_pair$antecedent, AND_pair$consequent)) %>%
      mutate(rel = RScoreDict$ALWAYS_PARALLEL)
    
    PAR_mode <- "HARD"
    if(reverse_rel != RScoreDict$ALWAYS_PARALLEL){
      PAR_mode == "SOFT"
    }
    
    return_list <- solve_PAR_relationship(
      AND_pair,
      relevant_pairs,
      mode = PAR_mode
    )
    
    return_list$messages <- c(return_list$messages,
                              paste("CONFLICT ----",
                                    act_a, 
                                    " AND ",
                                    act_b,
                                    " now considered in parallel.", sep = ""))
    
    return_list$rel_df <- rel_df
    return(return_list)
  }
  
  if(reverse_rel == RScoreDict$DIRECT_JOIN){
    ## Check if there are others that need to join here
    other_pre_joins <- rel_df %>%
      filter(consequent == act_a,
             rel == RScoreDict$DIRECT_JOIN)
    
    if(other_pre_joins %>% nrow > 0){
      mutual_pre_join_rels <- rel_df %>%
        filter(antecedent %in% other_pre_joins$antecedent,
               consequent %in% other_pre_joins$antecedent
               )
      
      mutual_join_rel_count <- mutual_pre_join_rels %>%
        count(rel)
      
      if(mutual_join_rel_count %>% filter(rel != RScoreDict$MUTUALLY_EXCLUSIVE) %>% nrow == 0){
        return_list <- solve_XOR_relationship(
          XOR_root="",
          XOR_branches=other_pre_joins$antecedent,
          rel_df)
        
        return(return_list)
      }
      
      if(mutual_join_rel_count %>% filter(rel != RScoreDict$PARALLEL_IF_PRESENT) %>% nrow == 0){
        return_list <- solve_XOR_relationship(
          XOR_root="",
          XOR_branches=other_pre_joins$antecedent,
          rel_df,
          split_symbol = ">O>")
        
        return(return_list)
      }
      
      if(mutual_join_rel_count$rel %in% 
         c(RScoreDict$DIRECTLY_FOLLOWS,
           RScoreDict$EVENTUALLY_FOLLOWS,
           RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
           RScoreDict$MAYBE_EVENTUALLY_FOLLOWS)){
        seq_pair <- mutual_pre_join_rels %>% 
          filter(rel %in% c(RScoreDict$DIRECTLY_FOLLOWS,
                            RScoreDict$EVENTUALLY_FOLLOWS,
                            RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                            RScoreDict$MAYBE_EVENTUALLY_FOLLOWS)) %>%
          arrange(-importance, -score) %>%
          head(1)
        
        return_list <- solve_sequence_relationship(
          seq_pair,
          rel_df
        )
        
        return(return_list)
      }
      
    }
  }
  msg <- "UNABLE TO ESTABLISH DIRECTLY FOLLOWS RELATIONSHIP"
  
  return_list <- list(
    snippet = NULL,
    activities = c(),
    rel_df = rel_df,
    messages = msg
  )
  
  return(return_list)
  
}


explore_XOR_split <- function(
    XOR_pair,
    rel_df,
    XOR_rels = c(RScoreDict$MAYBE_DIRECTLY_FOLLOWS),
    split_symbol = ">X>"){
  
  return_list <- list(
    snippet = NULL,
    activities = c(),
    rel_df = rel_df,
    messages = c()
  )
  
  other_branches <- rel_df %>%
    filter(
      antecedent == XOR_pair$antecedent,
      rel %in% XOR_rels)
  
  branch_names <- other_branches$consequent
  
  ## If there is only 1 branch, we need to check
  ## the reverse relationship
  if(length(branch_names) == 1){
    reverse_rel <- rel_df %>%
      filter(
        antecedent == branch_names,
        consequent == XOR_pair$antecedent)
    
    if(reverse_rel %>% nrow == 0){
      reverse_rel <- ""
    } else {
      reverse_rel <- reverse_rel$rel
    }
    
    if(reverse_rel %in% c("",RScoreDict$REQUIRES,RScoreDict$PARALLEL_IF_PRESENT)){
      ## If the reverse relation is a REQ, we can
      ## simply concatenate
      return_list <- solve_XOR_relationship(
        XOR_pair$antecedent,
        branch_names,
        rel_df)
      
    }
    
    if(reverse_rel == RScoreDict$MUTUALLY_EXCLUSIVE){
      ## If the reverse relation is an R5, then 
      ## we change the R3/4 relation to an Rx and we
      ## create a split only in the branch if that did 
      ## not already happen
      if(!startsWith(branch_names, split_symbol)){
        snippet_name <- paste(split_symbol,"[", branch_names, "]", split_symbol, sep ="")
        return_list$snippet <-  snippet_name
        
        return_list$activities <- branch_names
        
        return_list$messages <- c(return_list$messages,
                                  paste("Created process snippet ", snippet_name, sep = ""))
      }
      
      new_rows <- rel_df %>%
        filter(antecedent == XOR_pair$antecedent,
               consequent %in% branch_names) %>%
        mutate(rel = RScoreDict$DIRECT_JOIN)
      rel_df <- rel_df %>%
        filter(!(antecedent == XOR_pair$antecedent &
                   consequent %in% branch_names)) %>%
        bind_rows(new_rows)
      
      return_list$rel_df <- rel_df
      
      return_list$messages <- c(return_list$messages,
                                paste("Morphed ", new_rows %>% nrow(), " relationships to ", RScoreDict$DIRECT_JOIN, sep = ""))
    }
    ## If the reverse relation also indicates a follows
    ## then we assume an inclusive OR split between the XOR pairs
    if(reverse_rel %in% c(RScoreDict$DIRECTLY_FOLLOWS,
                          RScoreDict$EVENTUALLY_FOLLOWS,
                          RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                          RScoreDict$MAYBE_EVENTUALLY_FOLLOWS)){
      return_list <- solve_XOR_relationship(
        "",
        c(XOR_pair$antecedent, XOR_pair$consequent),
        tibble(
          antecedent = c(XOR_pair$antecedent, XOR_pair$consequent),
          consequent = c(XOR_pair$consequent, XOR_pair$antecedent),
          rel =c(RScoreDict$MUTUALLY_EXCLUSIVE,
                 RScoreDict$MAYBE_EVENTUALLY_FOLLOWS)
        ),
        split_symbol = ">O>")
      
      return_list$rel_df <- rel_df
    }
        
    
    return(return_list)
  }
  
  mutual_branch_relationships <- rel_df %>%
    filter(antecedent %in% branch_names,
           consequent %in% branch_names)
  
  ## If all branches are mutually exclusive, then we can create a XOR split
  ## on them.
  if(mutual_branch_relationships %>%
     count(rel) %>%
     filter(rel != RScoreDict$MUTUALLY_EXCLUSIVE) %>% nrow() == 0){
    
    return_list <- solve_XOR_relationship(
      XOR_pair$antecedent,
      branch_names,
      rel_df)
    
    return(return_list)
    
  }
  
  ## If there are always follows relationships
  ## we will try to solve them first
  if(mutual_branch_relationships %>% 
     filter(rel %in% c(RScoreDict$DIRECTLY_FOLLOWS,
                       RScoreDict$EVENTUALLY_FOLLOWS)) %>% nrow() > 0){
    
    seq_pair <- mutual_branch_relationships %>% 
      filter(rel %in% c(RScoreDict$DIRECTLY_FOLLOWS,
                        RScoreDict$EVENTUALLY_FOLLOWS)) %>%
      arrange(-importance, -score) %>%
      head(1)
    
    return_list <- solve_sequence_relationship(
      seq_pair,
      rel_df
    )
    
    return(return_list)
  }
  
  if(mutual_branch_relationships %>% 
     filter(rel == RScoreDict$REQUIRES) %>% nrow() > 0){
    
    REQ_pair <- mutual_branch_relationships %>% 
      filter(rel == RScoreDict$REQUIRES) %>%
      arrange(-importance,
              -score) %>%
      head(1)
    
    seq_pair <- rel_df %>%
      filter(antecedent == REQ_pair$consequent,
             consequent == REQ_pair$antecedent)
    
    if(seq_pair %>% nrow == 0){
      seq_pair <- tibble(
        antecedent = REQ_pair$consequent,
        consequent = REQ_pair$antecedent,
        rel = RScoreDict$MAYBE_EVENTUALLY_FOLLOWS
      )
      
      rel_df <- rel_df %>% 
        bind_rows(seq_pair)
    } else if(seq_pair$rel == RScoreDict$MUTUALLY_EXCLUSIVE){
      seq_pair <- seq_pair %>%
        mutate(rel = RScoreDict$MAYBE_EVENTUALLY_FOLLOWS)
      
      rel_df <- rel_df %>% 
        filter(!(antecedent == seq_pair$antecedent &
                   consequent == seq_pair$consequent)) %>%
        bind_rows(seq_pair)
    }
    
    return_list <- solve_sequence_relationship(
      seq_pair,
      rel_df
    )
    
    return(return_list)
  }
  
  if(mutual_branch_relationships %>% 
     filter(rel == RScoreDict$DIRECT_JOIN) %>% nrow() > 0){
    
    ## We must check whether we have to join withing
    ## the branch or whether we have to join on the root
    ## If the root is not required for the Rx element,
    ## then the root itseld should have an RX to the branch.
    
    seq_pair <- mutual_branch_relationships %>% 
      filter(rel == RScoreDict$DIRECT_JOIN) %>%
      arrange(-importance,
              -score
             ) %>%
      head(1)
    
    relation_to_root <- rel_df %>%
      filter(
        antecedent == seq_pair$consequent,
        consequent == XOR_pair$antecedent)
    
    if(relation_to_root %>% nrow == 0){
      relation_to_root <- ""
    } else {
      relation_to_root <- relation_to_root$rel
    }
    
    if(relation_to_root != RScoreDict$REQUIRES){
      
      rel_df <- rel_df %>%
        filter(!(antecedent == XOR_pair$antecedent & consequent == seq_pair$consequent)) %>%
        bind_rows(
            tibble(
              antecedent = XOR_pair$antecedent,
              consequent = seq_pair$consequent,
              rel = RScoreDict$DIRECT_JOIN,
              importanxe = 0,
              score = 0.5,
            ))
      
      return_list$rel_df <- rel_df
      return_list$messages <- c(return_list$messages, "Morphed XOR root relationship to join")
      
      return(return_list)
    } else {
      
      return_list <- solve_sequence_relationship(
        seq_pair,
        rel_df
      )
      
      return(return_list)
      
    }
  }
  
  if(other_branches %>% 
     filter(rel %in% c(RScoreDict$MAYBE_DIRECTLY_FOLLOWS)) %>% nrow() > 0){
    R3_branches <- other_branches %>% 
      filter(rel %in% c(RScoreDict$MAYBE_DIRECTLY_FOLLOWS))
    
    
    mutual_R3branch_relationships <- rel_df %>%
      filter(antecedent %in% R3_branches$consequent,
             consequent %in% R3_branches$consequent)
    
    if(mutual_R3branch_relationships %>%
       filter(rel == RScoreDict$MUTUALLY_EXCLUSIVE) %>% nrow == mutual_R3branch_relationships %>% nrow){
      return_list <- solve_XOR_relationship(
        XOR_root = R3_branches$antecedent %>% unique,
        XOR_branches = R3_branches$consequent %>% unique,
        rel_df = rel_df,
        split_symbol = ">X>")
      return(return_list)
    } else {
      return_list <- solve_XOR_relationship(
        XOR_root = R3_branches$antecedent %>% unique,
        XOR_branches = R3_branches$consequent %>% unique,
        rel_df = rel_df,
        split_symbol = ">O>")
      return(return_list)
    }
  }
  
  if(mutual_branch_relationships %>%
     filter(rel %in% c(RScoreDict$MAYBE_DIRECTLY_FOLLOWS)) %>% nrow() > 0){
    
    XOR_pair <- mutual_branch_relationships %>% 
      filter(rel %in% c(RScoreDict$MAYBE_DIRECTLY_FOLLOWS)) %>%
      arrange(-importance,
              -score
              ) %>%
      head(1)
    
    R3_branches <- rel_df %>%
      filter(rel == RScoreDict$MAYBE_DIRECTLY_FOLLOWS)
    
    return_list <- solve_XOR_relationship(
      XOR_pair$antecedent,
      R3_branches$consequent,
      rel_df)
    
    return(return_list)
    }
  
  
  branches_with_only_mutual_relations <- mutual_branch_relationships %>%
    count(antecedent, rel) %>%
    filter(rel != RScoreDict$MUTUALLY_EXCLUSIVE)
  
  branches_with_only_mutual_relations <- mutual_branch_relationships %>%
    filter(! antecedent %in% branches_with_only_mutual_relations$antecedent,
           ! consequent %in% branches_with_only_mutual_relations$antecedent)
  
  if(branches_with_only_mutual_relations %>% nrow > 0){
    
    return_list <- solve_XOR_relationship(
      XOR_pair$antecedent,
      branches_with_only_mutual_relations$antecedent %>% unique,
      rel_df)
    
    return(return_list)
  }
  
  ## If not everything is mutually exclusive, then we need to examine
  ## if there are any contradictions in the branches.
  mutual_branch_relationships <- mutual_branch_relationships %>%
    full_join(mutual_branch_relationships, by = c("antecedent"="consequent", "consequent"="antecedent")) %>%
    mutate(has_conflict = rel.x != rel.y)
  
  conflicted_relations <- mutual_branch_relationships %>%
    filter(has_conflict == TRUE)
  
  contradicting_sequences <- mutual_branch_relationships %>%
    filter(rel.x != "R5") %>%
    filter(has_conflict == FALSE)
  
  ## If there are contradicting sequences, then we assume that they
  ## should be executed n parallel.
  if(contradicting_sequences %>% nrow > 0){
    AND_pair <- contradicting_sequences %>%
      arrange(-importance,
              -score) %>%
      head(1)
    
    relevant_pairs <- rel_df %>%
      filter(antecedent %in% c(AND_pair$antecedent, AND_pair$consequent),
             consequent %in% c(AND_pair$antecedent, AND_pair$consequent)) %>%
      mutate(rel = RScoreDict$ALWAYS_PARALLEL)
    
    return_list <- solve_PAR_relationship(
      AND_pair,
      relevant_pairs
    )
    return_list$rel_df <- rel_df
    return(return_list)
  }
  
  
  ## If there are conflicts, we have to examine them.
  if(conflicted_relations %>% nrow() > 0){
    
    ## If one has a conditional relationship, and the other an exclude
    ## Then this means that the condition is probably very rare
    ## And a sub-branch XOR should be considered.
    if(conflicted_relations %>%
       filter(rel.x %in% c(RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                           RScoreDict$MAYBE_EVENTUALLY_FOLLOWS),
              rel.y == RScoreDict$MUTUALLY_EXCLUSIVE) %>% 
       nrow() > 0){
      sampled_conflict <- conflicted_relations %>%
        filter(rel.x %in% c(RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                            RScoreDict$MAYBE_EVENTUALLY_FOLLOWS),
               rel.y == RScoreDict$MUTUALLY_EXCLUSIVE) %>%
        arrange(-importance.x,
               -score.x) %>%
        head(1)
    } else if(conflicted_relations %>%
              filter(rel.x == RScoreDict$DIRECT_JOIN,
                     rel.y == RScoreDict$MUTUALLY_EXCLUSIVE) %>%
              nrow() > 0){
      ## If A should join on B but B is not expected to occur toegether with A
      ## then A is a rare occurrence before B.
      sampled_conflict <- conflicted_relations %>%
        filter(rel.x == RScoreDict$DIRECT_JOIN,
               rel.y == RScoreDict$MUTUALLY_EXCLUSIVE) %>%
        arrange(-importance.x,
                -score.x)
        head(1)
      
      return_list <- solve_XOR_relationship("",
                             sampled_conflict$antecedent,
                             rel_df)
      
      return(return_list)
    }
    
    sampled_conflict <- rel_df %>%
      filter(antecedent == sampled_conflict$antecedent,
             consequent == sampled_conflict$consequent)
    
    return_list <- explore_XOR_split(
      sampled_conflict,
      rel_df,
      XOR_rels = c(RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                   RScoreDict$MAYBE_EVENTUALLY_FOLLOWS))
    
    return(return_list)
  }
  
  ## Otherwise, we will create an INCLUSIVE OR gateway
  return_list <- solve_XOR_relationship(
    XOR_pair$antecedent,
    mutual_branch_relationships$antecedent %>% unique,
    rel_df,
    split_symbol = ">O>"
  )
  
  return(return_list)
}

solve_XOR_relationship <- function(
    XOR_root,
    XOR_branches,
    rel_df,
    split_symbol = ">X>"){
  
  return_list <- list(
    snippet = NULL,
    activities = c(),
    rel_df = rel_df,
    messages = c()
  )
  
  ## Sanity check
  ## The root should be REQUIRED for the branches
  ## Given a root A and branches B, C, D and E:
  ## If B and C require A but D and E do not,
  ## then we create a structure
  ## >X>[A >X>[B,C]>X> ]>X> >X>[D, E]>X>
  ## If neither of B, C, D or E require A
  ## then we create a structure
  ## >X>[A]>X> >X>[B,C,D,E]>X>
  ## If B, C, D and E require A,
  ## then we create
  ## A >X>[B,C,D,E]>X>
  
  OPTIONAL_ROOT <- FALSE
  
  if(is.null(XOR_root)) XOR_root <- ""
  
  if(XOR_root != ""){
    reverse_branch_relations <- rel_df %>%
      filter(antecedent %in% XOR_branches,
             consequent == XOR_root)
    
    branches_requiring_root <- reverse_branch_relations %>%
      filter(rel == RScoreDict$REQUIRES) %>%
      pull(antecedent)
    
    branches_not_requiring_root <- XOR_branches[!(XOR_branches %in% branches_requiring_root)]
    
    XOR_branches <- branches_requiring_root
    
    if(length(XOR_branches) == 0){
      XOR_branches <- branches_not_requiring_root
      
      OPTIONAL_ROOT <- TRUE
    }
  }
  
  
  acts <- XOR_branches
  
  closing_snippet <- ""
  
  ## If all branches join on the same point
  ## then we can include that in our XOR block
  join_points <- rel_df %>%
    filter(antecedent %in% XOR_branches,
           rel == RScoreDict$DIRECT_JOIN)
  
  orig_join_points <- join_points
  
  ## Unless that join point also has joins from
  ## other branches
  if(join_points$consequent %>% unique %>% length > 0){
    other_joins <- rel_df %>% 
      filter(
        ! antecedent %in% XOR_branches, 
        consequent %in% join_points$consequent,
        rel == RScoreDict$DIRECT_JOIN)
    
    
    join_points <- join_points %>%
      filter(! consequent %in% other_joins$consequent)
  }
  
  if(length(XOR_branches) == 1 && startsWith(XOR_branches, ">X>") && endsWith(XOR_branches, ">X>") && join_points %>% nrow == 0){
    join_points <- orig_join_points
  }
  
  if(join_points %>% nrow > 0){
    XOR_branches <- XOR_branches[XOR_branches %in% join_points$antecedent]
    
    closing_snippet <- paste(split_symbol, join_points$consequent[1], sep = " ")
    
    acts <- c(XOR_branches, join_points$consequent[1])
  } else {
    closing_snippet <- split_symbol
  }
  
  new_branches <- XOR_branches
  
  XOR_split_snippet = paste(split_symbol, "[",paste(new_branches, collapse = ","),"]", sep = "")
  snippet_name <- paste(XOR_split_snippet, closing_snippet, sep = "")
  
  
  other_branches_from_root <- rel_df %>%
    filter(antecedent == XOR_root,
           rel %in% c(RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                      RScoreDict$MAYBE_EVENTUALLY_FOLLOWS)) %>%
    filter(! consequent %in% XOR_branches)
  
  if(length(XOR_branches) == 1 || other_branches_from_root %>% nrow == 0){
    
    root_snippet <- XOR_root
    
    if(OPTIONAL_ROOT==TRUE & XOR_root != "" & !startsWith(XOR_root, "START ")){
      root_snippet <- paste(">X>[", root_snippet, "]>X>")
    }
    
    snippet_name <- paste(root_snippet, snippet_name, sep = " ")
    acts <- c(acts, XOR_root)
  }
  
  return_list <- list(
    snippet = snippet_name,
    activities = acts,
    rel_df = rel_df,
    messages = paste("Created process snippet:", snippet_name, sep = " ")
  )
  
  return(return_list)
  
}