solve_directly_follows <- function(
    seq_pair,
    rel_df,
    snippet_dict,
    sequence_memory){

  act_a <- seq_pair$antecedent
  act_b <- seq_pair$consequent


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
    return(list(return_list, sequence_memory))
  }

  if(startsWith(act_a, "START") & act_b == "END" & rel_df %>% nrow > 2){

    return_list <- list(
      snippet = NULL,
      activities = c(),
      snippet_dictionary = snippet_dict,
      messages = "Attempt to append END event canceled.",
      rel_df = rel_df %>%
        filter(!(antecedent == act_a & consequent == act_b))
    )
    return(list(return_list, sequence_memory))
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

    return(list(return_list, sequence_memory))
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

    return(list(return_list, sequence_memory))
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

    return_list$rel_df <- rel_df
    return(list(return_list, sequence_memory))
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
          XOR_branches=other_pre_joins$antecedent %>% unique,
          rel_df,
          snippet_dict)

        return(list(return_list, sequence_memory))
      }

      if(mutual_join_rel_count %>% filter(rel != RScoreDict$PARALLEL_IF_PRESENT) %>% nrow == 0){
        return_list <- solve_XOR_relationship(
          XOR_root="",
          XOR_branches=other_pre_joins$antecedent %>% unique,
          rel_df,
          snippet_dict,
          split_symbol = ">O>")

        return(list(return_list, sequence_memory))
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

        tmp <- solve_sequence_relationship(
          seq_pair,
          rel_df,
          snippet_dict,
          sequence_memory = sequence_memory
        )
        return_list <- tmp[[1]]
        sequence_memory <- tmp[[2]]

        return(list(return_list, sequence_memory))
      }

    }
  }
  msg <- "UNABLE TO ESTABLISH DIRECTLY FOLLOWS RELATIONSHIP"

  return_list <- list(
    snippet = NULL,
    activities = c(),
    rel_df = rel_df,
    snippet_dictionary = snippet_dict,
    messages = msg
  )

  return(list(return_list, sequence_memory))

}