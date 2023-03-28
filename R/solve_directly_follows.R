solve_directly_follows <- function(
    seq_pair,
    rel_df,
    snippet_dict){

  act_a <- seq_pair$antecedent
  act_b <- seq_pair$consequent
  
  
  rel_df <- rel_df %>% remember_pair(
      seq_pair,
      "DF"
    )

  return_list <- list(
    snippet = NULL,
    activities = c(),
    rel_df = rel_df,
    snippet_dictionary = snippet_dict,
    messages = c()
  )


  if(act_a == "START" & act_b == "END"){

    return_list <- list(
      snippet = NULL,
      activities = c(),
      rel_df = rel_df %>%
        filter(!(antecedent == "START" & consequent == "END")),
      snippet_dictionary = snippet_dict,
      messages = "Attempt to directly sequence START and END event canceled."
    )
    return(return_list)
  }

  if(check_start(act_a, snippet_dict) & act_b == "END" & rel_df %>% nrow > 2){

    return_list <- list(
      snippet = NULL,
      activities = c(),
      snippet_dictionary = snippet_dict,
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
  
  if(reverse_rel == "" && act_a %in% names(snippet_dict)) reverse_rel <- RScoreDict$REQUIRES
  if(reverse_rel == "" && rel_df %>% nrow == 1) reverse_rel <- RScoreDict$REQUIRES

  if(reverse_rel  == RScoreDict$REQUIRES){
    snippet_name <- paste(act_a, act_b, sep = " >> ")

    msg <- paste("Created process snippet:", snippet_name, sep = " ")

    snippet_dict[[snippet_name]] <-
      create_snippet(
        act_a,
        act_b,
        c(),
        "SEQ",
        snippet_dict
      )


    return_list <- list(
      snippet = snippet_name,
      activities = c(act_a, act_b),
      rel_df = rel_df,
      snippet_dictionary = snippet_dict,
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
      snippet_dictionary = snippet_dict,
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
      snippet_dict = snippet_dict,
      mode = PAR_mode
    )

    return_list$messages <- c(return_list$messages,
                              paste("CONFLICT ----",
                                    act_a,
                                    " AND ",
                                    act_b,
                                    " now considered in parallel.", sep = ""))

    return_list$rel_df <- rel_df %>%
      remember_pair(
        AND_pair,
        ifelse(PAR_mode == "SOFT","OR","AND")
      )
    return(return_list)
  }

  if(reverse_rel %in% c("",RScoreDict$DIRECT_JOIN)){
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
          XOR_branches=other_pre_joins$antecedent %>% unique,
          rel_df,
          snippet_dict)

        return(return_list)
      }

      if(mutual_join_rel_count %>% filter(rel != RScoreDict$PARALLEL_IF_PRESENT) %>% nrow == 0){
        return_list <- solve_XOR_relationship(
          XOR_root="",
          XOR_branches=other_pre_joins$antecedent %>% unique,
          rel_df,
          snippet_dict,
          split_symbol = ">O>")

        return(return_list)
      }

      if(mutual_join_rel_count$rel %in%
         c(RScoreDict$DIRECTLY_FOLLOWS,
           RScoreDict$EVENTUALLY_FOLLOWS,
           RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
           RScoreDict$MAYBE_EVENTUALLY_FOLLOWS)){
        seq_pair <- mutual_pre_join_rels %>%
          sample_pair(c(RScoreDict$DIRECTLY_FOLLOWS,
                            RScoreDict$EVENTUALLY_FOLLOWS,
                            RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                            RScoreDict$MAYBE_EVENTUALLY_FOLLOWS)) 

        return_list <- solve_sequence_relationship(
          seq_pair,
          rel_df,
          snippet_dict
        )

        return(return_list)
      }

    }
    
    all_preceeding_relations <- rel_df %>%
      filter(consequent == act_b,
             rel %in% MERGE_FOLLOWS_RELS)
    
    mutual_preceeding_relations <- rel_df %>%
      filter(antecedent %in% all_preceeding_relations$antecedent,
             consequent %in% all_preceeding_relations$antecedent)
    
    if(mutual_preceeding_relations %>%
       filter(rel == RScoreDict$MUTUALLY_EXCLUSIVE) %>% nrow > 0){
      xor_branches <- mutual_preceeding_relations %>%
        sample_pair(RScoreDict$MUTUALLY_EXCLUSIVE)
      
      return_list <- solve_XOR_relationship(
        XOR_root = "",
        XOR_branches = c(xor_branches$antecedent, xor_branches$consequent),
        rel_df,
        snippet_dict,
        split_symbol = ">X>")
        
        return(return_list)
    }
    
    if(mutual_preceeding_relations %>%
       filter(rel %in% c(RScoreDict$ALWAYS_PARALLEL,
                         RScoreDict$PARALLEL_IF_PRESENT)) %>% nrow > 0){
      par_pair <- mutual_preceeding_relations %>%
        sample_pair(c(RScoreDict$ALWAYS_PARALLEL,
                      RScoreDict$PARALLEL_IF_PRESENT))
      
      return_list <- solve_PAR_relationship(
        par_pair,
        rel_df,
        snippet_dict,
        mode = ifelse(par_pair$rel == RScoreDict$ALWAYS_PARALLEL, "HARD", "SOFT")
      )
      
      return(return_list)
    }
    
    sampled_pair <- mutual_preceeding_relations %>%
      sample_pair(MERGE_FOLLOWS_RELS)
    
    if(!is.null(sampled_pair)){
      
      return_list <- solve_sequence_relationship(
        sampled_pair,
        rel_df,
        snippet_dict
      )
    } else {
      
      sampled_pair <- mutual_preceeding_relations %>%
        sample_pair(RScoreDict$REQUIRES)
      
      reverse_pair <- sampled_pair %>%
        mutate(antecedent = sampled_pair$consequent,
               consequent = sampled_pair$antecedent,
               rel = RScoreDict$MAYBE_EVENTUALLY_FOLLOWS)
      
      
      return_list <- solve_sequence_relationship(
        reverse_pair,
        rel_df %>%
          bind_rows(reverse_pair),
        snippet_dict
      )
      
    }
    
    return(return_list)
    
  }
  msg <- "UNABLE TO ESTABLISH DIRECTLY FOLLOWS RELATIONSHIP"

  return_list <- list(
    snippet = NULL,
    activities = c(),
    rel_df = rel_df,
    snippet_dictionary = snippet_dict,
    messages = msg
  )

  return(return_list)

}
