#
# MERGE_R_levels <- c(RScoreDict$DIRECT_JOIN,
#               RScoreDict$DIRECTLY_FOLLOWS,
#               RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
#               RScoreDict$EVENTUALLY_FOLLOWS,
#               RScoreDict$PARALLEL_IF_PRESENT,
#               RScoreDict$ALWAYS_PARALLEL,
#               RScoreDict$TERMINATING,
#               RScoreDict$HAPPENS_DURING,
#               RScoreDict$MUTUALLY_EXCLUSIVE,
#               RScoreDict$REQUIRES,
#               RScoreDict$MAYBE_EVENTUALLY_FOLLOWS
# )
#
#
# MERGE_R_levels <- c(RScoreDict$DIRECT_JOIN,
#               RScoreDict$DIRECTLY_FOLLOWS,
#               RScoreDict$PARALLEL_IF_PRESENT,
#               RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
#               RScoreDict$EVENTUALLY_FOLLOWS,
#               RScoreDict$REQUIRES,
#               RScoreDict$MUTUALLY_EXCLUSIVE,
#               RScoreDict$ALWAYS_PARALLEL,
#               RScoreDict$TERMINATING,
#               RScoreDict$HAPPENS_DURING,
#               RScoreDict$MAYBE_EVENTUALLY_FOLLOWS
# )



merge_relationships <- function(
    snippet_name,
    activities,
    rel_df
){
  ## Reset factor levels
  rel_df <- rel_df %>%
    mutate(rel = factor(rel, levels = MERGE_R_levels, ordered = TRUE))

  ## We do not want to remove the END event from the log
  real_activities <- activities[! activities %in% c('END')]

  HAS_END <- "END" %in% activities | endsWith(snippet_name, " END")
  HAS_START <- "START" %in% activities | startsWith(snippet_name, "START ")

  ## If we have added an END event, we want to remove
  ## all relationships between the activities in the block
  ## and the end event
  if(HAS_END){
    rel_df <- rel_df %>%
      filter(!(antecedent %in% real_activities & consequent == "END") )
  }


  if(HAS_START){
    rel_df <- rel_df %>%
      filter(!(consequent %in% real_activities & rel != RScoreDict$REQUIRES))
    rel_df <- rel_df %>%
      filter(!(antecedent %in% real_activities & rel == RScoreDict$REQUIRES)) 
    
  }
  
   
  # direct_joins_from_start <- rel_df %>%
  #   filter(startsWith(antecedent, "START"),
  #          rel == RScoreDict$DIRECT_JOIN) %>%
  #   mutate(rel = RScoreDict$EVENTUALLY_FOLLOWS)
  # 
  # rel_df <- rel_df %>%
  #   anti_join(direct_joins_from_start, by=c("antecedent","consequent")) %>%
  #   bind_rows(direct_joins_from_start)


  ## Merging relationships with new snippet as consequent
  consequent_rel <- rel_df %>%
    filter(consequent %in% real_activities,
           !(antecedent %in% real_activities) )

  ## Merging relationships with new snippet as antecedent
  antecedent_rel <- rel_df %>%
    filter(antecedent %in% real_activities,
           !(consequent %in% real_activities) )

  ## Remove old relationships
  original_rel_df <- rel_df
  rel_df <- rel_df %>%
    filter(!(antecedent %in% real_activities),
           !(consequent %in% real_activities))

  if(consequent_rel %>% nrow() > 0){

    ## If we only have one activity,
    ## then we have to reduce the importance
    ## further in otder to avoid a loop.
    if(length(activities[activities != ""])==1){

      direct_antecedents <- original_rel_df %>%
        filter(consequent %in% activities,
               rel %in% c(RScoreDict$DIRECTLY_FOLLOWS, RScoreDict$DIRECT_JOIN,
                          RScoreDict$EVENTUALLY_FOLLOWS, RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                          RScoreDict$MAYBE_EVENTUALLY_FOLLOWS))

      closest_direct_antecedents <- original_rel_df %>%
        filter(consequent %in% c(direct_antecedents$antecedent, activities),
               rel %in% c(RScoreDict$DIRECTLY_FOLLOWS, RScoreDict$DIRECT_JOIN,
                          RScoreDict$EVENTUALLY_FOLLOWS, RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                          RScoreDict$MAYBE_EVENTUALLY_FOLLOWS)) %>%
        group_by(antecedent) %>%
        mutate(n = n()) %>%
        filter(n == min(n))

      closest_direct_XOR_roots <- closest_direct_antecedents %>%
        filter(rel %in% c(RScoreDict$MAYBE_DIRECTLY_FOLLOWS, RScoreDict$MAYBE_EVENTUALLY_FOLLOWS))

      if(closest_direct_XOR_roots %>% nrow > 0){
        print("CHANGE HAD EFFECT")

        closest_direct_XOR_roots <- closest_direct_XOR_roots %>%
          mutate(rel = RScoreDict$DIRECT_JOIN)

        consequent_rel <- consequent_rel %>%
          filter(!(antecedent %in% closest_direct_XOR_roots$antecedent & consequent  %in% activities)) %>%
          bind_rows(closest_direct_XOR_roots)

      } else {
        consequent_rel <- consequent_rel %>%
          mutate(importance = importance/10)

      }

    }

    consequent_rel <- consequent_rel %>%
      group_by(antecedent) %>%
      summarize(rel = min(rel, na.rm = TRUE),
                score = min(score, na.rm=TRUE),
                importance = min(importance, na.rm =TRUE)) %>%
      ungroup() %>%
      mutate(consequent = snippet_name)

    rel_df <- rel_df %>%
      bind_rows(consequent_rel)
  }

  if(antecedent_rel %>% nrow() > 0){
    antecedent_rel <- antecedent_rel %>%
      group_by(consequent) %>%
      summarize(rel = min(rel, na.rm = TRUE),
                score = min(score, na.rm=TRUE),
                importance = min(importance, na.rm =TRUE)) %>%
      ungroup() %>%
      mutate(antecedent = snippet_name)

    rel_df <- rel_df %>%
      bind_rows(antecedent_rel)
  }

  ## Reset factor levels
  rel_df <- rel_df %>%
    mutate(rel = factor(rel, levels = MERGE_R_levels, ordered = TRUE))

  if(rel_df %>% nrow == 0){
    rel_df <- tibble(
      antecedent = snippet_name,
      consequent = NA,
      rel = NA,
      score = NA,
      importance = NA
    )
  }

  return(rel_df)
}

update_rel_notebook <- function(
    constrc_result,
    rel_df,
    verbose = TRUE){

  if(verbose ==  TRUE & length(constrc_result$messages) > 0){
    print(constrc_result$messages)
  }

  if(!is.null(constrc_result)){
    rel_df <- constrc_result$rel_df
  }

  if(!is.null(constrc_result$snippet)){
    rel_df <- merge_relationships(
      constrc_result$snippet,
      constrc_result$activities,
      rel_df
    )
  }

  if(I_WANT_INTERRUPTIONS){

    bpmn_obj <- constrc_result$snippet_dictionary[[length(constrc_result$snippet_dictionary)]]

    map_to_bpmn <- function(bpmn_obj){
      tasks <- bpmn_obj$tasks %>% as.data.frame()
      seqs <- bpmn_obj$seqs %>% as.data.frame() %>% unique
      gateways <- bpmn_obj$gateways %>% as.data.frame() %>% unique
      start_events <- bpmn_obj$start_events %>% as.data.frame() %>% unique
      end_events <- bpmn_obj$end_events %>% as.data.frame() %>% unique

      bpmn_out <- create_bpmn(tasks, seqs, gateways, start_events, end_events)

      return(bpmn_out)
    }

    bpmn_obj <- add_start_end(bpmn_obj)
    bpmn_out <- map_to_bpmn(bpmn_obj)

    bpmn_out %>%
      write_bpmn("output/snippet.bpmn")
    readline(prompt="Press [enter] to continue")
  }

  return(rel_df)
}


determine_rels_in_focus <- function(rel_df){
  rel_summary <- rel_df %>%
    count(rel)

  if(any(MERGE_INTERRUPTING_RELS %in% rel_summary$rel)){
    return(MERGE_INTERRUPTING_RELS)
  } else if(RScoreDict$DIRECTLY_FOLLOWS %in% rel_summary$rel){
    return(RScoreDict$DIRECTLY_FOLLOWS)
  } else if(RScoreDict$ALWAYS_PARALLEL %in% rel_summary$rel){
    return(RScoreDict$ALWAYS_PARALLEL)
  } else {
    return(NULL)
  }
}