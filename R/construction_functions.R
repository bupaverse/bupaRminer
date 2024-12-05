## List of snippet that have been created
## The index is the symbolic snippet name.
## The value is a list that contains
## the detailed representation of that snippet.

sample_pair <- function(
    rel_df,
    rel_vect){
  
  if(is.null(rel_vect) | length(rel_vect) == 0){
    rel_vect <- rel_df%>%
      pull(rel) %>%
      unique
  }
  
  if(any(rel_vect %in% MERGE_FOLLOWS_RELS)){
    REQ_PREF <- TRUE
  } else {
    REQ_PREF <- FALSE
  }
  REQ_PREF <- FALSE

  domain <- rel_df %>%
    filter(rel %in% c(rel_vect))

  sampled_pair <- NULL
  if(domain %>% nrow() > 0){
    
    sampled_pair <- domain %>%
      arrange(-importance,
              -score) %>%
      head(1)
    
    if(REQ_PREF == TRUE){
      new_domain <- rel_df %>%
        filter(rel == RScoreDict$REQUIRES) %>%
        inner_join(domain %>% select(antecedent, consequent),
                   by=c("antecedent"="consequent",
                        "consequent"="antecedent"))
      
      if(new_domain %>% nrow > 0){
        
        reverse_pair <- new_domain %>%
          arrange(-importance,
                  -score) %>%
          head(1)

        
        sampled_pair <- domain %>%
          filter(
            antecedent == reverse_pair$consequent,
            consequent == reverse_pair$antecedent
          )
      }
    }
    
    
  }

  return(sampled_pair)
}


solve_apriori_conflicts <- function(
    rel_df,
    strict = FALSE){

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

  if(strict == TRUE){
    follows_rel <- follows_rel %>%
      filter(must_remove == FALSE)
  }

  removed_rels <- follows_rel %>%
    filter(must_remove == TRUE)

  rel_df <- rel_df %>%
    anti_join(removed_rels, by=c("antecedent","consequent"))

  conflict_rel <- follows_rel %>%
    filter(rel.x == rel.y) %>%
    mutate(rel = RScoreDict$PARALLEL_IF_PRESENT,
           importance = 0,
           score = 0.5) %>%
    select(antecedent, consequent, rel, importance)

  rel_df <- rel_df %>%
    anti_join(conflict_rel, by=c("antecedent","consequent")) %>%
    bind_rows(conflict_rel)
  
  
  join_with_soft_par <- rel_df %>%
    filter(rel %in% c(RScoreDict$DIRECT_JOIN,
                      RScoreDict$DIRECTLY_FOLLOWS)) %>%
    select(antecedent, consequent) %>%
    inner_join(rel_df %>% filter(rel == RScoreDict$PARALLEL_IF_PRESENT) %>% select(antecedent, consequent),
               by = c("antecedent"="consequent","consequent"="antecedent"))
  
  rel_df <- rel_df %>%
    anti_join(
      join_with_soft_par, 
      by=c("antecedent"="consequent","consequent"="antecedent"))
    
  
  antecedents_with_multiple_DF <- rel_df %>%
    filter(rel == RScoreDict$DIRECTLY_FOLLOWS) %>%
    group_by(antecedent) %>%
    filter(n() > 1)
  
  rel_df <- rel_df %>%
    anti_join(antecedents_with_multiple_DF, by=c("antecedent","consequent")) %>%
    bind_rows(antecedents_with_multiple_DF %>% mutate(rel = RScoreDict$DIRECT_JOIN))
  
  
  ## Select always parallel where opposite is sometimes parallel and
  ## switch them to sometimes parallel
  always_pars <- rel_df %>%
    filter(rel == RScoreDict$ALWAYS_PARALLEL)
  
  if(always_pars %>% nrow > 0){
    reverse_rels <- rel_df %>%
      filter(rel == RScoreDict$PARALLEL_IF_PRESENT) %>%
      inner_join(always_pars %>% select(antecedent, consequent),
                 by = c("antecedent"="consequent",
                        "consequent"="antecedent"))
    
    if(reverse_rels %>% nrow > 0){
      always_pars <- always_pars %>%
        inner_join(reverse_rels %>% select(antecedent, consequent),
                   by = c("antecedent"="consequent",
                          "consequent"="antecedent"))
      rel_df <- rel_df %>%
        anti_join(always_pars %>% select(antecedent, consequent),
                  by=c("antecedent","consequent")) %>%
        bind_rows(always_pars %>% mutate(rel = RScoreDict$PARALLEL_IF_PRESENT))
    }
  }

  return(rel_df)
}


solve_interrupt_relationship <- function(
    rel_pair,
    rel_df,
    construction_context = list(
      snippet_dictionary = list(),
      trace_log = NULL
    )){

  snippet_dict <- construction_context$snippet_dictionary
  return_list <- list(
    snippet = NULL,
    activities = c(),
    rel_df = rel_df,
    snippet_dictionary = snippet_dict,
    messages = c()
  )
  
  rel_df <- rel_df %>% remember_pair(
    rel_pair,
    "BOUNDARY"
  )
  
  ## Check if others interrupt as well
  if(rel_df %>% filter(antecedent == rel_pair$antecedent, rel == rel_pair$rel) %>% nrow > 1){
    ## If so, solve them first
    other_interrupting <- rel_df %>% 
      filter(antecedent == rel_pair$antecedent, rel == rel_pair$rel) %>%
      pull(consequent)
    
    mutual_relationships <- rel_df %>% 
      filter(antecedent %in% other_interrupting,
             consequent %in% other_interrupting)
    
    inter_proc <- construct_process(mutual_relationships, 
                                    snippet_dict, 
                                    source="main")
    
    last_snippet <- inter_proc[length(inter_proc)]
    
    return_list$snippet <- names(last_snippet)
    return_list$activities <- other_interrupting
    snippet_dict[[return_list$snippet]] <- last_snippet[[1]]
    return_list$snippet_dictionary <- snippet_dict
    return(return_list)
  }

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

  if(rel_pair$rel == RScoreDict$TERMINATING){
    
    snippet_dict[[snippet_name]] <-
      create_snippet(
        antec,
        conseq,
        c(),
        "SEQ",
        snippet_dict,
        seq_name = rel_pair$rel
      )
  } else {
    
    snippet_dict[[snippet_name]] <-
      create_snippet(
        NULL,
        NULL,
        c(antec, conseq),
        "AND",
        snippet_dict,
        seq_name = rel_pair$rel
      )
  }

  return_list <- list(
    snippet = snippet_name,
    activities = c(antec, conseq),
    rel_df = rel_df,
    snippet_dictionary = snippet_dict,
    messages = msg
  )

  return(return_list)
}

solve_join <- function(
  join_pair,
  rel_df,
  construction_context = list(
    snippet_dictionary = list(),
    trace_log = NULL
  )
  
){

  snippet_dict <- construction_context$snippet_dictionary 
  return_list <- list(
    snippet = NULL,
    activities = c(),
    rel_df = rel_df,
    snippet_dictionary = snippet_dict,
    messages = c()
  )

  other_joins <- rel_df %>%
    filter(consequent == join_pair$consequent) %>%
    filter(rel == RScoreDict$DIRECT_JOIN)
  
  if(other_joins %>% nrow > 1){
    mutual_relations <- rel_df %>%
      filter(antecedent %in% other_joins$antecedent,
             consequent %in% other_joins$antecedent) 
    
    if(mutual_relations %>% filter(rel %in% MERGE_FOLLOWS_RELS) %>% nrow() > 0){
      seq_pair <- mutual_relations %>%
        sample_pair(MERGE_FOLLOWS_RELS)
      
      return_list <- solve_sequence_relationship(
        seq_pair,
        rel_df,
        construction_context
      )
      return(return_list)
    }
    
    if(mutual_relations %>% filter(rel == RScoreDict$MUTUALLY_EXCLUSIVE) %>% nrow() > 0){
      seq_pair <- mutual_relations %>%
        sample_pair(RScoreDict$MUTUALLY_EXCLUSIVE)
      
      return_list <- solve_XOR_relationship(
        XOR_root = "",
        XOR_branches = c(seq_pair$antecedent, seq_pair$consequent),
        rel_df = rel_df,
        construction_context
      )
      return(return_list)
    }
  }
  
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
      XOR_branches = join_pair$antecedent %>% unique,
      rel_df = rel_df,
      construction_context
    )
    return(return_list)
  }

  if(reverse_rel == RScoreDict$REQUIRES){
    return_list <- solve_directly_follows(
      join_pair %>%
        mutate(rel = RScoreDict$DIRECTLY_FOLLOWS),
      rel_df,
      construction_context
    )

    return_list$rel_df <- rel_df
    return(return_list)
  }

  if(reverse_rel == RScoreDict$MAYBE_EVENTUALLY_FOLLOWS){
    rel_df <- rel_df %>%
      filter(!(antecedent == join_pair$consequent &
               consequent == join_pair$antecedent))

    return_list$snippet_dictionary <- snippet_dict
    return_list$rel_df <- rel_df
    return_list$messages <- c(return_list$messages,"Removed conflicting MAYBE EVENTUALLY FOLLOWS relation")

    return(return_list)
  }


  if(reverse_rel %in% c(RScoreDict$PARALLEL_IF_PRESENT,
                        RScoreDict$ALWAYS_PARALLEL,
                        RScoreDict$DIRECT_JOIN)){
    par_pair <- join_pair %>%
      mutate(rel = RScoreDict$PARALLEL_IF_PRESENT)

    return_list <- solve_PAR_relationship(
      par_pair,
      rel_df %>%
        filter(!(antecedent == par_pair$antecedent & consequent == par_pair$consequent)) %>%
        bind_rows(par_pair),
      construction_context,
      mode = "SOFT"
    )

    return(return_list)
  }
}






