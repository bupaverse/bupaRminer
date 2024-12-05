solve_PAR_relationship <- function(
    rel_pair,
    rel_df,
    construction_context = list(
      snippet_dictionary = list(),
      trace_log = NULL
    ),
    mode = "HARD"
){

  snippet_dict <- construction_context$snippet_dictionary
  if(mode == "SOFT"){
    valid_relationships <- c(RScoreDict$ALWAYS_PARALLEL, RScoreDict$PARALLEL_IF_PRESENT)
  } else{
    valid_relationships <- c(RScoreDict$ALWAYS_PARALLEL)
  }
  
  rel_df <- rel_df %>% remember_pair(
    rel_pair,
    ifelse(mode=="SOFT","OR","AND")
  )
  
  return_list <- list(
    snippet = NULL,
    activities = c(),
    rel_df = rel_df,
    snippet_dictionary = snippet_dict,
    messages = c()
  )

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
  
  ## If some acts require activities that other acts in the branch
  ## do not require, then we can first solve those relations
  ## if the required activities are in parallel with the other
  ## branches
  required_for_split <- rel_df %>%
    filter(antecedent %in% R6_acts,
           rel == RScoreDict$REQUIRES)
  
  if(required_for_split %>% nrow > 0 & 
     required_for_split %>% count(consequent) %>% filter(n != length(R6_acts)) %>% nrow() != 0 ){
    required_but_parallel <- rel_df %>%
      filter(
        antecedent %in% R6_acts,
        rel %in% valid_relationships,
        consequent %in% required_for_split$consequent)
    
    if(required_but_parallel %>% nrow > 0){
      reverse_pairs <- rel_df %>% 
        inner_join(required_but_parallel %>% 
                     select(antecedent, consequent), 
                   by=c("antecedent"="consequent","consequent"="antecedent")) %>%
        filter(rel %in% MERGE_FOLLOWS_RELS)
      if(reverse_pairs%>%
         nrow > 0){
        sampled_pair <- reverse_pairs %>%
          sample_pair(MERGE_FOLLOWS_RELS)
        
        return_list <- solve_sequence_relationship(
          sampled_pair,
          rel_df,
          construction_context
        )
        return(return_list)
      }
    }
  }

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
      different_follows <- relations_after_split %>%
        filter(all_same == FALSE) %>%
        pull(consequent)
      
      relations_with_different_follows <- rel_df %>% 
        filter(consequent %in% different_follows, 
               antecedent %in% R6_acts)
      
      relation_analysis <- relations_with_different_follows %>% 
        count(consequent, rel) %>% 
        group_by(consequent) %>% 
        mutate(has_par = sum(rel %in% c(RScoreDict$ALWAYS_PARALLEL, 
                                        RScoreDict$PARALLEL_IF_PRESENT)), 
               has_follows = sum(rel %in% MERGE_FOLLOWS_RELS), 
               total_relations = sum(n), 
               non_par_relations = total_relations - has_par)
      
      inner_block_candidates <- relation_analysis %>%
        filter(has_par > 0,
               has_follows == non_par_relations,
               has_follows == 1,
               rel %in% MERGE_FOLLOWS_RELS) %>%
        select(consequent, rel)
      
      if(inner_block_candidates %>% nrow > 0){
        new_pair <- relations_with_different_follows %>%
          inner_join(inner_block_candidates, by=c("consequent","rel")) %>%
          sample_pair(MERGE_FOLLOWS_RELS)
        
        if(new_pair %>% nrow > 0){
          return_list <- solve_sequence_relationship(
            new_pair,
            rel_df,
            construction_context
          )
          return(return_list)
        }
      }
      
      new_pair <- acts_happening_after_split %>%
        filter(consequent %in% different_follows) %>%
        sample_pair(MERGE_FOLLOWS_RELS)
      
      if(new_pair %>% nrow > 0){
        return_list <- solve_sequence_relationship(
          new_pair,
          rel_df,
          construction_context
        )
        return(return_list)
      }
      
      
      
      mutual_relationships <- rel_df %>%
        filter(antecedent %in% acts_after_split,
               consequent %in% acts_after_split)
      
      if(mutual_relationships %>% filter(rel %in% c(RScoreDict$DIRECTLY_FOLLOWS,
                                                    RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                                                    RScoreDict$EVENTUALLY_FOLLOWS,
                                                    RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
                                                    RScoreDict$DIRECT_JOIN)) %>% nrow > 0){
        selected_pair <- mutual_relationships %>% 
          sample_pair(c(RScoreDict$DIRECTLY_FOLLOWS,
                        RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                        RScoreDict$EVENTUALLY_FOLLOWS,
                        RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
                        RScoreDict$DIRECT_JOIN))
        
        return_list <- solve_sequence_relationship(
          selected_pair,
          rel_df,
          construction_context
        )
        return(return_list)
      }

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


  ## If we want to parallellize with an already existing parallel split
  ## we can just incorporate the new addition into the existing one.

  PAR_SYMBOL_START = "++["
  PAR_SYMBOL_END = "]++"

  modified_acts <- R6_acts

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
        modified_acts <- c(required_from_start, optional_acts)
      }
    } else {
      PAR_SYMBOL_START = ">O>["
      PAR_SYMBOL_END = "]>O>"
    }
  }

  new_acts <- c()
  for(act in modified_acts){
    if(startsWith(act,PAR_SYMBOL_START) & endsWith(act,PAR_SYMBOL_END)){
      trimmed_act <- gsub('^\\+\\+\\[|\\]\\+\\+$', '', act)
      trimmed_act <- gsub('^>O>\\[|\\]>O>$', '', trimmed_act)

      ## Remove trailing numbers and then remove repetitions
      ## trimmed_act <- gsub("_(0|[1-9][0-9]*)$", "",trimmed_act)
      new_acts <- c(new_acts, trimmed_act)
    } else{
      ## trimmed_act <- gsub("_(0|[1-9][0-9]*)$", "",act)
      new_acts <- c(new_acts, act)
    }
  }

  new_acts <- new_acts %>% unique

  snippet_name <- paste(PAR_SYMBOL_START,paste(new_acts, collapse = ","),PAR_SYMBOL_END,sep="")
  
  snippet_acts = list()
  i <- 1
  for(act in R6_acts){
    snippet_acts[i] <- act
    i <- i+1
  }
  snippet_dict[[snippet_name]] <-
    create_snippet(
      NULL,
      NULL,
      snippet_acts,
      if(mode == "SOFT") "OR" else "AND",
      snippet_dict
    )

  return_list$snippet <- snippet_name
  return_list$activities <- R6_acts
  return_list$rel_df <- rel_df
  return_list$messages <- c(return_list$messages,
                            paste("Created process snippet:",
                                  return_list$snippet, sep = " "))
  return_list$snippet_dictionary = snippet_dict

  return(return_list)

}
