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
        snippet_dict[[snippet_name]] <-
          create_snippet(
            NULL,
            NULL,
            snippet_acts,
            if(split_symbol == ">O>") "OR" else "XOR",
            snippet_dict
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

  ## If all branches are mutually exclusive, then we can create a XOR split
  ## on them.
  if(mutual_branch_relationships %>%
     count(rel) %>%
     filter(rel != RScoreDict$MUTUALLY_EXCLUSIVE) %>% nrow() == 0){

    
    return_list <- solve_XOR_relationship(
      XOR_pair$antecedent,
      branch_names,
      rel_df,
      snippet_dict)

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
      rel_df,
      snippet_dict = snippet_dict
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
    
    
    if(XOR_pair$consequent == "A_Cancelled"){
      print(seq_pair %>% select(antecedent, consequent, rel, score))
    }

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

    if(relation_to_root != RScoreDict$REQUIRES & !startsWith(XOR_pair$antecedent, "START")){

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
    filter(has_conflict == TRUE)

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
    }

    sampled_conflict <- rel_df %>%
      filter(antecedent == sampled_conflict$antecedent,
             consequent == sampled_conflict$consequent)

    return_list <- explore_XOR_split(
      sampled_conflict,
      rel_df,
      snippet_dict = snippet_dict,
      XOR_rels = c(RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                   RScoreDict$MAYBE_EVENTUALLY_FOLLOWS))
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
