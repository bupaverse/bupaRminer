
construct_process <- function(assigned_rel_df,
                              snippet_dictionary = list(),
                              source = "main", id = NULL) {

  n <- nrow(assigned_rel_df)


  pkg.env$end_event_counter <- 1

  rel_notebook_df <- assigned_rel_df %>%
    filter(!(rel == RScoreDict$ALWAYS_PARALLEL &
               antecedent %in% c("START","END") &
               consequent %in% c("START","END"))) %>%
    filter(antecedent != "END",
           !(consequent == "START" & rel != RScoreDict$REQUIRES)) %>%
    mutate(
      score=ifelse(consequent=="END",0,score),
      importance=ifelse(consequent=="END",0,importance)
    )

  rel_notebook_df <- solve_apriori_conflicts(rel_notebook_df, strict = FALSE) %>%
    reset_memory()

  RELS_IN_FOCUS <- determine_rels_in_focus(
    rel_notebook_df
  )
  if(source == "main") {
    cli::cli_progress_step("Constructing process - {n - nrow(rel_notebook_df)}/{n}", spinner = TRUE)
  } else {
    cli::cli_progress_step("[loop block {id}] Constructing process- {n - nrow(rel_notebook_df)}/{n}", spinner = TRUE)
  }

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
    i <- n - nrow(rel_notebook_df)
    cli::cli_progress_update()


    rel_notebook_df <- rel_notebook_df %>%
      reset_memory()

    snippet_dictionary <- result$snippet_dictionary

    # cli::cli_alert_info(names(snippet_dictionary)[length(names(snippet_dictionary))])

    RELS_IN_FOCUS <- determine_rels_in_focus(
      rel_notebook_df
    )

    if(is.null(RELS_IN_FOCUS)){
      HAS_COMPLETED <- TRUE
    }
  }

  completed_FOL <- FALSE
  while(rel_notebook_df %>%
        nrow() > 0 &
        rel_notebook_df %>% filter(is.na(consequent)) %>% nrow < 1 &
        completed_FOL == FALSE){


    EXCLUDE_PAIRS_POSSIBLE <- TRUE

    while(EXCLUDE_PAIRS_POSSIBLE & rel_notebook_df %>% fetch_mutual_exclude() %>% nrow > 0){

      result <- explore_mutual_exclusive_relationship(rel_notebook_df,
                                              snippet_dictionary)

      rel_notebook_df <- update_rel_notebook(
        result,
        rel_notebook_df
      )
      i <- n - nrow(rel_notebook_df)
      cli::cli_progress_update()

      rel_notebook_df <- rel_notebook_df %>%
        reset_memory()

      if(is.null(result$snippet)){
        EXCLUDE_PAIRS_POSSIBLE <- FALSE
      } else {
        snippet_dictionary <- result$snippet_dictionary
        # cli::cli_alert_info(names(snippet_dictionary)[length(names(snippet_dictionary))])
      }
    }

    SOFT_PAR_POSSIBLE <- TRUE

    while(SOFT_PAR_POSSIBLE & rel_notebook_df %>% count(rel) %>% filter(rel == RScoreDict$PARALLEL_IF_PRESENT) %>% nrow > 0){

      result <- explore_soft_PAR_relationship(rel_notebook_df,
                                              snippet_dictionary)


      rel_notebook_df <- update_rel_notebook(
        result,
        rel_notebook_df

      )
      i <- n - nrow(rel_notebook_df)
      cli::cli_progress_update()

      rel_notebook_df <- rel_notebook_df %>%
        reset_memory()

      if(is.null(result$snippet)){
        SOFT_PAR_POSSIBLE <- FALSE
      } else {
        snippet_dictionary <- result$snippet_dictionary
        # cli::cli_alert_info(names(snippet_dictionary)[length(names(snippet_dictionary))])
      }
    }

    if(rel_notebook_df %>%
       filter(rel %in% MERGE_FOLLOWS_RELS) %>%
       nrow() > 0 & completed_FOL == FALSE){

      result <- NULL

      ## We sample any pair between an early activity
      ## and any follows or eventually follows relationship
      sampled_pair <- sample_pair(
        rel_notebook_df,
        MERGE_FOLLOWS_RELS)

      result <- solve_sequence_relationship(
        sampled_pair,
        rel_notebook_df,
        snippet_dictionary
      )

      if(is.null(result)){
        cli::cli_abort("Oops, that's an error. bupaRminer is currently in beta-release, and is still being continiously improved. Contact us at support@bupar.net so that we can investigate and solve this error. ")
      } else {
        snippet_dictionary <- result$snippet_dictionary
        # cli::cli_alert_info(names(snippet_dictionary)[length(names(snippet_dictionary))])
      }

      # print(result$rel_df %>% retrieve_memory_log())

      rel_notebook_df <- update_rel_notebook(
        result,
        rel_notebook_df
      )
      i <- n - nrow(rel_notebook_df)
      cli::cli_progress_update()

      rel_notebook_df <- rel_notebook_df %>%
        reset_memory()
    } else {
      completed_RxREQ <- FALSE
      while(rel_notebook_df %>%
            filter(!is.na(rel)) %>%
            nrow() > 0 & completed_RxREQ == FALSE){

        ## We sample any pair between an early activity
        ## and any follows or eventually follows relationship
        sampled_pair <- rel_notebook_df %>% sample_pair(
          c(RScoreDict$DIRECT_JOIN,
            RScoreDict$REQUIRES) )

        if(!is.null(sampled_pair)){

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
            snippet_dictionary
          )
        } else {
          exclusive_pair <- rel_notebook_df %>%
            sample_pair(c())

          result <- solve_XOR_relationship(
              XOR_root = "",
              c(exclusive_pair$antecedent, exclusive_pair$consequent),
              exclusive_pair,
              snippet_dictionary
            )

        }


        if(is.null(result)){
          print("---- No result for sample")
        } else {
          snippet_dictionary <- result$snippet_dictionary
          # cli::cli_alert_info(names(snippet_dictionary)[length(names(snippet_dictionary))])
        }

        rel_notebook_df <- update_rel_notebook(
          result,
          rel_notebook_df
        )
        i <- n - nrow(rel_notebook_df)
        cli::cli_progress_update()


        rel_notebook_df <- rel_notebook_df %>%
          reset_memory()

      }
    }

    if(rel_notebook_df %>%
       filter(
         rel %in% c(MERGE_FOLLOWS_RELS,
                    RScoreDict$MUTUALLY_EXCLUSIVE,
                    RScoreDict$PARALLEL_IF_PRESENT,
                    RScoreDict$REQUIRES) ) %>% nrow() == 0){
      completed_FOL = TRUE
    }

  }

  return(snippet_dictionary)

}
