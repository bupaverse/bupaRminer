explore_XOR_split <- function(
    XOR_pair,
    rel_df,
    snippet_dict,
    XOR_rels = c(RScoreDict$MAYBE_DIRECTLY_FOLLOWS),
    split_symbol = ">X>"){
  
  return_list <- list(
    snippet = NULL,
    activities = c(),
    rel_df = rel_df,
    snippet_dictionary = snippet_dict,
    messages = c()
  )
  
  rel_df <- rel_df %>% remember_pair(
    XOR_pair,
    "XOR"
  )

  other_branches <- rel_df %>%
    filter(
      antecedent == XOR_pair$antecedent,
      rel %in% XOR_rels)

  branch_names <- other_branches$consequent
  
  ## If there is only 1 branch, we need to check
  ## the reverse relationship
  if(length(branch_names) == 1){
    
    if(check_split(branch_names, snippet_dict)){
      XOR_turned_seq <-XOR_pair %>% mutate(rel = RScoreDict$EVENTUALLY_FOLLOWS)
      return_list <- solve_directly_follows(
        XOR_turned_seq,
        rel_df %>%
          anti_join(XOR_pair, by = c("antecedent","consequent")) %>%
          bind_rows(XOR_turned_seq),
        snippet_dict
      )
      
      return(return_list)
    }
    
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
        rel_df,
        snippet_dict)
    }

    if(reverse_rel == RScoreDict$MUTUALLY_EXCLUSIVE){
      ## If the reverse relation is an R5, then
      ## we change the R3/4 relation to an Rx and we
      ## create a split only in the branch if that did
      ## not already happen
      if(!startsWith(branch_names, split_symbol)){
        snippet_name <- paste(split_symbol,"[", branch_names, "]", split_symbol, sep ="")

        snippet_acts = list()
        i <- 1
        for(act in branch_names){
          snippet_acts[i] <- act
          i <- i+1
        }
        branch_names <- extract_branch_names(snippet_acts, rel_df)
        snippet_dict[[snippet_name]] <-
          create_snippet(
            NULL,
            NULL,
            snippet_acts,
            if(split_symbol == ">O>") "OR" else "XOR",
            snippet_dict,
            seq_name = branch_names
          )

        return_list$snippet <-  snippet_name

        return_list$activities <- branch_names

        return_list$messages <- c(return_list$messages,
                                  paste("Created process snippet ", snippet_name, sep = ""))

        return_list$snippet_dictionary <- snippet_dict
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
        c(XOR_pair$antecedent, XOR_pair$consequent) %>% unique,
        tibble(
          antecedent = c(XOR_pair$antecedent, XOR_pair$consequent),
          consequent = c(XOR_pair$consequent, XOR_pair$antecedent),
          rel =c(RScoreDict$MUTUALLY_EXCLUSIVE,
                 RScoreDict$MAYBE_EVENTUALLY_FOLLOWS)
        ),
        snippet_dict,
        split_symbol = ">O>")

      return_list$rel_df <- rel_df
    }


    return(return_list)
  }

  mutual_branch_relationships <- rel_df %>%
    filter(antecedent %in% branch_names,
           consequent %in% branch_names)
  
  ## If there are no circular relationships between the branches
  ## then we can select one of the pairs and construct a relationship
  ## between them
  if(mutual_branch_relationships %>% filter(rel != RScoreDict$REQUIRES) %>% count(antecedent) %>% pull(n) %>% max == 1){
    if(mutual_branch_relationships %>% filter(rel != RScoreDict$REQUIRES) %>% count(consequent) %>% pull(n) %>% max == 1){
      
      if(mutual_branch_relationships %>% filter(rel %in% MERGE_FOLLOWS_RELS) %>% nrow > 0){
        
        
        branch_pair <- mutual_branch_relationships %>%
          sample_pair(MERGE_FOLLOWS_RELS)
        
        reverse_pair <- mutual_branch_relationships %>%
          inner_join(branch_pair %>% select(antecedent, consequent),
                     by = c("antecedent"="consequent","consequent"="antecedent"))
        
        if(reverse_pair %>% nrow == 0 || reverse_pair$rel != RScoreDict$MUTUALLY_EXCLUSIVE){
          
          return_list <- solve_sequence_relationship(
            branch_pair,
            rel_df,
            snippet_dict
          )
          return(return_list)
        }
      }
    }
  }
  
  ## If there are no mutual relations, then the branches appear separate from each other
  ## and should not be merged into the same split
  if(mutual_branch_relationships %>% nrow == 0){
    new_pair <- other_branches %>%
      anti_join(XOR_pair, by=c("antecedent","consequent")) %>%
      sample_pair(MERGE_FOLLOWS_RELS)
    
    return_list <- solve_sequence_relationship(
      new_pair,
      rel_df,
      snippet_dict
    )
    return(return_list)
    }

  ## If all branches are mutually exclusive, then we can create a XOR split
  ## on them.
  if(mutual_branch_relationships %>%
     count(rel) %>%
     filter(rel != RScoreDict$MUTUALLY_EXCLUSIVE) %>% nrow() == 0){

    exclusive_branch_relationships <- mutual_branch_relationships %>% 
      filter(rel == RScoreDict$MUTUALLY_EXCLUSIVE) %>% 
      inner_join(mutual_branch_relationships %>%
                   select(antecedent, consequent),
                 by = c("antecedent"="consequent","consequent"="antecedent"))
    ## First we check how oftern an activity occurs as
    ## mutually exclusive
    mutual_occurrences <- exclusive_branch_relationships %>%
      count(antecedent)
    
    while(mutual_occurrences %>% nrow > 0 &
          mutual_occurrences %>% pull(n) %>% max != mutual_occurrences %>% pull(n) %>% min){
      thrown_activities <- mutual_occurrences %>%
        filter(n == min(n)) %>%
        pull(antecedent)
      exclusive_branch_relationships <- exclusive_branch_relationships %>%
        filter(!(antecedent %in% thrown_activities),
               !(consequent %in% thrown_activities))
      mutual_occurrences <- exclusive_branch_relationships %>%
        count(antecedent)
    }
    
    if(mutual_occurrences %>% nrow > 0){
      
      return_list <- solve_XOR_relationship(
        XOR_pair$antecedent,
        mutual_occurrences$antecedent,
        rel_df,
        snippet_dict)
      
      return(return_list)
    }

  }

  ## If there are always follows relationships
  ## we will try to solve them first
  if(mutual_branch_relationships %>%
     filter(rel %in% c(RScoreDict$DIRECTLY_FOLLOWS,
                       RScoreDict$EVENTUALLY_FOLLOWS)) %>% nrow() > 0){

    seq_pair <- mutual_branch_relationships %>%
      sample_pair(c(RScoreDict$DIRECTLY_FOLLOWS,
                    RScoreDict$EVENTUALLY_FOLLOWS))

    return_list <- solve_sequence_relationship(
      seq_pair,
      rel_df,
      snippet_dict = snippet_dict
    )

    return(return_list)
  }

  if(mutual_branch_relationships %>%
     filter(rel == RScoreDict$REQUIRES) %>% nrow() > 0){
    
    REQ_pair <- mutual_branch_relationships %>%
      sample_pair(RScoreDict$REQUIRES)

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

    if(seq_pair$rel == RScoreDict$DIRECT_JOIN){
      
      return_list <- solve_join(
        seq_pair,
        rel_df,
        snippet_dict
      )

      return(return_list)
    }

    return_list <- solve_sequence_relationship(
      seq_pair,
      rel_df,
      snippet_dict
    )

    return(return_list)
  }

  if(mutual_branch_relationships %>%
     filter(rel == RScoreDict$DIRECT_JOIN) %>% nrow() > 0){

    ## We must check whether we have to join within
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

    if(relation_to_root != RScoreDict$REQUIRES & check_start(XOR_pair$antecedent, snippet_dict)){

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
        rel_df,
        snippet_dict
      )

      return(return_list)

    }
  }
  
  ## If there are mutual soft pars, then we create them first
  if(mutual_branch_relationships %>%
     filter(rel %in% c(RScoreDict$ALWAYS_PARALLEL,RScoreDict$PARALLEL_IF_PRESENT )) %>% nrow() > 0){
    
    par_branches <- mutual_branch_relationships %>%
      filter(rel %in% c(RScoreDict$ALWAYS_PARALLEL,RScoreDict$PARALLEL_IF_PRESENT ))
    
    sampled_par <- par_branches %>%
      sample_pair(c(RScoreDict$ALWAYS_PARALLEL,RScoreDict$PARALLEL_IF_PRESENT ))
    
    return_list <- solve_PAR_relationship(
      sampled_par,
      par_branches,
      snippet_dict,
      mode = "SOFT"
    )
    return_list$rel_df <- rel_df %>% remember_pair(
      sampled_par,
      "OR"
    )
    return(return_list)
  }
  
  ## If some branches are exclusive from all others,
  ## but the others have more complex relationships between them, 
  ## then we must try to solve the other part of the branch first
  if(mutual_branch_relationships %>%
     filter(rel == RScoreDict$MUTUALLY_EXCLUSIVE) %>% nrow() > 0){
    exclusive_relations <- mutual_branch_relationships %>%
      filter(rel == RScoreDict$MUTUALLY_EXCLUSIVE)
    
    not_fully_exclusive <- mutual_branch_relationships %>%
      filter(rel != RScoreDict$MUTUALLY_EXCLUSIVE) %>%
      filter(antecedent %in% c(exclusive_relations$antecedent, exclusive_relations$consequent)
             | consequent %in% c(exclusive_relations$antecedent, exclusive_relations$consequent))
    
    full_exclusive_relations <- unique(exclusive_relations$antecedent, exclusive_relations$consequent)
    full_exclusive_relations <- intersect(full_exclusive_relations,
                                          c(not_fully_exclusive$antecedent,
                                            not_fully_exclusive$consequent))
    
    branch_in_focus <- mutual_branch_relationships %>%
      filter(!(antecedent %in% full_exclusive_relations),
             !(consequent %in% full_exclusive_relations)) 
    
    if(branch_in_focus %>% filter(rel %in% MERGE_FOLLOWS_RELS) %>% nrow > 0){
      sampled_pair <- branch_in_focus %>%
        sample_pair(MERGE_FOLLOWS_RELS)
      
      return_list <- solve_sequence_relationship(
        sampled_pair,
        rel_df,
        snippet_dict
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
        snippet_dict,
        split_symbol = ">X>")
      return(return_list)
    } else {
      return_list <- solve_XOR_relationship(
        XOR_root = R3_branches$antecedent %>% unique,
        XOR_branches = R3_branches$consequent %>% unique,
        rel_df = rel_df,
        snippet_dict,
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
      rel_df,
      snippet_dict)

     return(return_list)
  }

  
  branches_with_req_to_root <- rel_df %>%
    filter(antecedent %in% branch_names,
           consequent == XOR_pair$antecedent,
           rel == RScoreDict$REQUIRES) 
  
  if(branches_with_req_to_root %>% nrow > 0){
    
    return_list <- solve_XOR_relationship(
      XOR_pair$antecedent,
      branches_with_req_to_root$antecedent %>% unique,
      rel_df,
      snippet_dict)
    
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
      rel_df,
      snippet_dict)

    return(return_list)
  }

  ## If not everything is mutually exclusive, then we need to examine
  ## if there are any contradictions in the branches.
  mutual_branch_relationships <- mutual_branch_relationships %>%
    full_join(select(mutual_branch_relationships, -score, -importance), by = c("antecedent"="consequent", "consequent"="antecedent")) %>%
    mutate(has_conflict = rel.x != rel.y)

  conflicted_relations <- mutual_branch_relationships %>%
    filter(has_conflict == TRUE | is.na(has_conflict))

  contradicting_sequences <- mutual_branch_relationships %>%
    filter(rel.x != RScoreDict$MUTUALLY_EXCLUSIVE) %>%
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
      relevant_pairs,
      snippet_dict
    )
    return_list$rel_df <- rel_df %>%
      remember_pair(
        AND_pair,
        "AND"
      )
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
        arrange(-importance,
                -score) %>%
        head(1)
    } else if(conflicted_relations %>%
              filter(rel.x == RScoreDict$DIRECT_JOIN,
                     rel.y == RScoreDict$MUTUALLY_EXCLUSIVE) %>%
              nrow() > 0){
      ## If A should join on B but B is not expected to occur together with A
      ## then A is a rare occurrence before B.
      sampled_conflict <- conflicted_relations %>%
        filter(rel.x == RScoreDict$DIRECT_JOIN,
               rel.y == RScoreDict$MUTUALLY_EXCLUSIVE) %>%
        arrange(-importance,
                -score) %>% 
        head(1)
      
      return_list <- solve_XOR_relationship("",
                                            sampled_conflict$antecedent %>% unique,
                                            rel_df,
                                            snippet_dict)

      return(return_list)
    } else if(conflicted_relations %>%
              filter(rel.x == RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
                     is.na(rel.y)) %>%
              nrow() > 0){
      ## If one is SEF and the other has no relation,
      ## then we assume that they are not part of the 
      ## same split.
      
      ## We then have to check which one is most likely
      ## to split of from the root
      
      sampled_conflict <- conflicted_relations %>%
        filter(rel.x == RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
               is.na(rel.y)) %>%
        arrange(-importance,
                -score) %>%
        head(1)
      
    } else {
      sampled_conflict <- conflicted_relations %>%
        arrange(-importance,
                -score) %>%
        head(1)
    }
    
    
    ## If one requires the root and the other does not,
    ## then we have our split. 
    reqs_for_branches <- rel_df %>%
      filter(antecedent %in% c(sampled_conflict$antecedent,
                               sampled_conflict$consequent),
             rel == RScoreDict$REQUIRES)
    
    reqs_root <- reqs_for_branches %>%
      filter(consequent == XOR_pair$antecedent)
    
    if(reqs_root %>% nrow == 1){
      return_list <- solve_XOR_relationship(XOR_pair$antecedent,
                                            reqs_root$antecedent %>% unique,
                                            rel_df,
                                            snippet_dict)
      return(return_list)
    }
    
    ## Else, we check which branch has the most corresponding
    ## requires relationships. 
    reqs_for_root <- rel_df %>%
      filter(antecedent == XOR_pair$antecedent,
             rel == RScoreDict$REQUIRES) %>%
      pull(consequent)
    
    corresponding_reqs <- reqs_for_branches %>%
      filter(consequent %in% reqs_for_root) %>%
      count(antecedent) %>%
      rename(same_reqs=n)
    missing_reqs <- reqs_for_branches %>%
      filter(!consequent %in% reqs_for_root) %>%
      count(antecedent)  %>%
      rename(different_reqs=n)
    
    branch_scores <- corresponding_reqs %>%
      full_join(missing_reqs,
                by="antecedent") %>%
      mutate(score = same_reqs - different_reqs)
    
    if(branch_scores %>% filter(score == max(score)) %>% nrow == 1){
      
      return_list <- solve_XOR_relationship(XOR_pair$antecedent,
                                            branch_scores %>%
                                              filter(score==max(score)) %>%
                                              pull(antecedent),
                                            rel_df,
                                            snippet_dict)
      return(return_list)
    }
    
    ##And otherwise, we check which branch has relationships
    ##That resemble the root the most. 
    rels_to_root <- rel_df %>%
      filter(consequent == XOR_pair$antecedent) %>%
      select(antecedent, root_rel = rel)
    rels_to_branches <- rel_df %>%
      filter(consequent %in% c(sampled_conflict$antecedent,
                               sampled_conflict$consequent)) %>%
      left_join(rels_to_root, by="antecedent") %>%
      select(antecedent, consequent, root_rel, rel) %>%
      mutate(is_match = (root_rel == rel)) %>%
      group_by(consequent) %>%
      summarize(matching_antecedents = sum(is_match, na.rm = TRUE))
    
    
    rels_from_root <- rel_df %>%
      filter(antecedent == XOR_pair$antecedent) %>%
      select(consequent, root_rel = rel)
    rels_from_branches <- rel_df %>%
      filter(antecedent %in% c(sampled_conflict$antecedent,
                               sampled_conflict$consequent)) %>%
      left_join(rels_from_root, by="consequent") %>%
      select(antecedent, consequent, root_rel, rel) %>%
      mutate(is_match = (root_rel == rel)) %>%
      group_by(antecedent) %>%
      summarize(matching_consequents = sum(is_match, na.rm = TRUE))
    
    combined_score <- rels_to_branches %>%
      full_join(rels_from_branches, by= c("consequent"="antecedent")) %>%
      mutate(score = matching_antecedents + matching_consequents)
    
    if(combined_score %>% filter(score == max(score)) %>% nrow == 1){
      
      return_list <- solve_XOR_relationship(XOR_pair$antecedent,
                                            combined_score %>%
                                              filter(score==max(score)) %>%
                                              pull(consequent),
                                            rel_df,
                                            snippet_dict)
      return(return_list)
    }
    
    sampled_conflict <- rel_df %>%
      filter(antecedent == sampled_conflict$antecedent,
             consequent == sampled_conflict$consequent)

    return_list <- solve_PAR_relationship(
      sampled_conflict %>% mutate(rel = RScoreDict$PARALLEL_IF_PRESENT),
      rel_df %>%
        filter(antecedent %in% c(sampled_conflict$antecedent, sampled_conflict$consequent),
               consequent %in% c(sampled_conflict$antecedent, sampled_conflict$consequent)) %>%
        mutate(rel = RScoreDict$PARALLEL_IF_PRESENT),
      snippet_dict,
      mode = "SOFT"
    )
    return_list$rel_df <- rel_df
    
    return(return_list)
  }
  
  ## Otherwise, we will create an INCLUSIVE OR gateway
  return_list <- solve_XOR_relationship(
    XOR_pair$antecedent,
    mutual_branch_relationships$antecedent %>% unique,
    rel_df,
    snippet_dict,
    split_symbol = ">O>"
  )

  return(return_list)
}
