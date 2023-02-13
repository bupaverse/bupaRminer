
construct_process <- function(assigned_rel_df) {

  pkg.env$end_event_counter <- 1

  sequence_memory <- list(
    sequence_solution_counter = 0,
    sequence_memory_antec = NULL,
    sequence_memory_conseq = NULL)

  rel_notebook_df <- assigned_rel_df %>%
    filter(!(rel == RScoreDict$ALWAYS_PARALLEL &
               antecedent %in% c("START","END") &
               consequent %in% c("START","END"))) %>%
    filter(antecedent != "END",
           consequent != "START") %>%
    mutate(
      score=ifelse(consequent=="END",0,score),
      importance=ifelse(consequent=="END",0,importance)
    )

  rel_notebook_df <- solve_apriori_conflicts(rel_notebook_df, strict = FALSE)

  RELS_IN_FOCUS <- determine_rels_in_focus(
    rel_notebook_df
  )

  snippet_dictionary <- list()

  HAS_COMPLETED <- FALSE
  while(!is.null(RELS_IN_FOCUS) & HAS_COMPLETED == FALSE){

    if(any(MERGE_INTERRUPTING_RELS %in% RELS_IN_FOCUS)){
      rel_solver_function <- solve_interrupt_relationship
    } else if(RELS_IN_FOCUS == RScoreDict$DIRECTLY_FOLLOWS){
      rel_solver_function <- solve_DF_relationship
    } else if(RELS_IN_FOCUS == RScoreDict$ALWAYS_PARALLEL){
      rel_solver_function <- solve_PAR_relationship
    }

    sampled_pair <- sample_pair(
      rel_notebook_df,
      RELS_IN_FOCUS)


    result <- rel_solver_function(
      sampled_pair,
      rel_notebook_df,
      snippet_dictionary
    )

    rel_notebook_df <- update_rel_notebook(
      result,
      rel_notebook_df
    )

    snippet_dictionary <- result$snippet_dictionary

    RELS_IN_FOCUS <- determine_rels_in_focus(
      rel_notebook_df
    )

    if(is.null(RELS_IN_FOCUS)){
      HAS_COMPLETED <- TRUE
    }
  }

  completed_FOL <- FALSE
  while(rel_notebook_df %>%
        filter(rel %in% MERGE_FOLLOWS_RELS ) %>%
        nrow() > 0 & completed_FOL == FALSE){

    SOFT_PAR_POSSIBLE <- TRUE

    while(SOFT_PAR_POSSIBLE & rel_notebook_df %>% count(rel) %>% filter(rel == RScoreDict$PARALLEL_IF_PRESENT) %>% nrow > 0){

      result <- explore_soft_PAR_relationship(rel_notebook_df,
                                              snippet_dictionary)

      rel_notebook_df <- update_rel_notebook(
        result,
        rel_notebook_df
      )

      if(is.null(result$snippet)){
        SOFT_PAR_POSSIBLE <- FALSE
      } else {
        snippet_dictionary <- result$snippet_dictionary
      }
    }

    result <- NULL

    ## We fetch early activities in branches
    relevant_antec <- fetch_sequence_antecedents(rel_notebook_df)

    ## We sample any pair between an early activity
    ## and any follows or eventually follows relationship
    sampled_pair <- sample_pair(
      rel_notebook_df, # %>% filter(antecedent %in% relevant_antec),
      MERGE_FOLLOWS_RELS)

    tmp <- solve_sequence_relationship(
      sampled_pair,
      rel_notebook_df,
      snippet_dictionary,
      reset = TRUE,
      sequence_memory
    )

    result <- tmp[[1]]
    sequence_memory <- tmp[[2]]

    if(is.null(result)){
      print("---- No result for sample")
    } else {
      snippet_dictionary <- result$snippet_dictionary
    }

    rel_notebook_df <- update_rel_notebook(
      result,
      rel_notebook_df
    )

    if(rel_notebook_df %>%
       filter(
         rel %in% MERGE_FOLLOWS_RELS,
         antecedent == "START",
         consequent != "END") %>% nrow() == 1){
      completed_FOL = TRUE
    }

  }

  completed_RxREQ <- FALSE
  while(rel_notebook_df %>%
        filter(rel %in%  MERGE_OTHER_RELS) %>%
        nrow() > 0 & completed_RxREQ == FALSE){

    ## We sample any pair between an early activity
    ## and any follows or eventually follows relationship
    sampled_pair <- rel_notebook_df %>% sample_pair(
      c(RScoreDict$DIRECT_JOIN,
        RScoreDict$REQUIRES) )

    if(sampled_pair$rel == RScoreDict$REQUIRES){
      seq_pair <- tibble(
        antecedent = sampled_pair$consequent,
        consequent = sampled_pair$antecedent,
        rel = RScoreDict$DIRECTLY_FOLLOWS,
        score = NA
      )
    } else {
      seq_pair <- sampled_pair %>%
        mutate(rel = RScoreDict$DIRECTLY_FOLLOWS)
    }

    result <- solve_directly_follows(
      seq_pair,
      seq_pair,
      snippet_dictionary,
      sequence_memory
    )
    sequence_memory <- result[[2]]
    result <- result[[1]]

    if(is.null(result)){
      print("---- No result for sample")
    } else {
      snippet_dictionary <- result$snippet_dictionary
    }

    rel_notebook_df <- update_rel_notebook(
      result,
      rel_notebook_df
    )


  }

  return(snippet_dictionary)

}