## List of snippet that have been created
## The index is the symbolic snippet name.
## The value is a list that contains
## the detailed representation of that snippet.

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
    select(antecedent, consequent, rel)

  rel_df <- rel_df %>%
    anti_join(conflict_rel, by=c("antecedent","consequent")) %>%
    bind_rows(conflict_rel)

  return(rel_df)
}


solve_interrupt_relationship <- function(
    rel_pair,
    rel_df,
    snippet_dict){

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

  snippet_dict[[snippet_name]] <-
    create_snippet(
      antec,
      conseq,
      c(),
      "SEQ",
      snippet_dict
    )

  return_list <- list(
    snippet = snippet_name,
    activities = c(antec, conseq),
    rel_df = rel_df,
    snippet_dictionary = snippet_dict,
    messages = msg
  )

  return(return_list)
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
    rel_df,
    snippet_dict
){

  return_list <- list(
    snippet = NULL,
    activities = c(),
    rel_df = rel_df,
    snippet_dictionary = snippet_dict,
    messages = c()
  )
  
  if(rel_df %>%
     filter(antecedent == rel_pair$antecedent,
            consequent == rel_pair$consequent,
            inspection_sequence > 0) %>%
     nrow > 0){
    return_list <- solve_sequence_relationship(
      rel_pair,
      rel_pair %>% 
        mutate(inspection_sequence = 0),
      snippet_dict
    )
    
    return_list$rel_df <- rel_df %>% remember_pair(
      rel_pair,
      "SEQ")
    
    return(return_list)
  }
  
  rel_df <- rel_df %>% remember_pair(
    rel_pair,
    "SEQ")

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

        if(mutual_relationships %>% nrow == 0){
          antecedent_rels <- closest_antecedents$rel %>% unique
          if(length(antecedent_rels) == 1 & antecedent_rels %in% c(RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                                                                    RScoreDict$MAYBE_EVENTUALLY_FOLLOWS)){
            return_list <- solve_XOR_relationship("",
                                   closest_antecedents$consequent %>% unique,
                                   rel_df = rel_df,
                                   snippet_dict =  snippet_dict)
            return(return_list)
          } else {
            closest_antecedents <- closest_antecedents %>%
              mutate(rel=RScoreDict$DIRECT_JOIN)

            rel_df <- rel_df %>%
              filter(!(antecedent %in% closest_antecedents$antecedent & consequent == closest_antecedents$consequent)) %>%
              bind_rows(closest_antecedents)

            return_list$rel_df <- rel_df
            return_list$messages <- c(return_list$messages,
                                      paste("Morphed relationships to Rx"))
            return(return_list)
          }
        }
        if(mutual_relationships %>% nrow == 1){
          return_list <- solve_sequence_relationship(
            mutual_relationships,
            rel_df,
            snippet_dict
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
        ## then A and B have the same follows relationship towards
        ## each other. This is a contradiction.
        ## We solve it by considering A and B as a parallel path
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
          relevant_pairs,
          snippet_dict
        )

        ## We only examined on a partial log.
        ## We need to safeguard the entire log though
        return_list$rel_df <- rel_df %>% remember_pair(
            relevant_pairs %>% arrange(
              -importance,
              -score
            ) %>%
              head(1),
            "OR"
          )
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
          rel_df,
          snippet_dict)

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
          closest_antecedents,
          snippet_dict
        )

        return_list$rel_df <- rel_df %>% remember_pair(
            seq_pair,
            "SEQ"
          )

        return(return_list)
      } else {
        ## Otherwise we must examine the mutual relationship
        ## between the closest antecedents.
        mutual_antec_relations <- rel_df %>%
          filter(antecedent %in% closest_antecedents$antecedent,
                 consequent %in% closest_antecedents$antecedent)

        ## If there is no mutual relationship between the antecedents
        ## we will artificially create a join towards the consequent.
        ## If the relationship towards the consequent is a maybe follows
        ## we will also create an exclusive branch for this.
        if(mutual_antec_relations %>% nrow() == 0){
          relevant_relations <- closest_antecedents$rel %>%
            unique

          closest_antecedents <- closest_antecedents %>%
            mutate(rel = RScoreDict$DIRECT_JOIN)

          rel_df <- rel_df %>%
            filter(!(antecedent %in% closest_antecedents$antecedent & consequent %in% closest_antecedents$consequent)) %>%
            bind_rows(closest_antecedents)

          return_list$rel_df <- rel_df

          if(length(relevant_relations)==1){
            if(relevant_relations %in% c(RScoreDict$MAYBE_DIRECTLY_FOLLOWS, RScoreDict$MAYBE_EVENTUALLY_FOLLOWS)){
              return_list <- solve_XOR_relationship("",
                                                    closest_antecedents$consequent %>% unique,
                                                    rel_df,
                                                    snippet_dict)
            }
          }
          return(return_list)
        }

        ## If the antecedents are mutually exclusive
        ## XOR them together
        if(mutual_antec_relations %>%
           count(rel) %>%
           filter(rel != RScoreDict$MUTUALLY_EXCLUSIVE) %>%
           nrow== 0){

          return_list <- solve_XOR_relationship("",
                                                c(mutual_antec_relations$antecedent,
                                                  mutual_antec_relations$consequent) %>% unique,
                                                mutual_antec_relations,
                                                snippet_dict)
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
                                                mutual_antec_relations,
                                                snippet_dict)
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
            snippet_dict,
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
                mutate(rel = RScoreDict$ALWAYS_PARALLEL),
              snippet_dict
            )
            return_list$rel_df <- rel_df %>% remember_pair(
                SEQ_pair,
                "AND"
              )
            return(return_list)
          }

          antec <- SEQ_pair$antecedent
          conseq <- SEQ_pair$consequent
          relevant_relation <- SEQ_pair$rel
          SEQ_FOUND <- TRUE
        }

        ## If there is a combination of parallel and exlusive relations
        ## that do not contain pairwise disagreements, then
        ## we create an inclusive AND that has some mutual exlusive
        ## choices in them.
        if(RScoreDict$MUTUALLY_EXCLUSIVE %in% mutual_antec_relations$rel){
          mutual_exclusions <- mutual_antec_relations %>%
            filter(rel == RScoreDict$MUTUALLY_EXCLUSIVE)

          ## Check if the mutual exclusions are indeed mutual

          non_mutual_exclusions <- mutual_antec_relations %>%
            filter(antecedent %in% c(mutual_exclusions$antecedent, mutual_exclusions$consequent),
                   consequent %in% c(mutual_exclusions$antecedent, mutual_exclusions$consequent)) %>%
            filter(rel != RScoreDict$MUTUALLY_EXCLUSIVE)

          if(non_mutual_exclusions %>% nrow == 0){
            return_list <- solve_XOR_relationship(
              XOR_root = "",
              XOR_branches = mutual_exclusions$antecedent %>% unique,
              rel_df = rel_df,
              snippet_dict
            )
            return(return_list)
          }


          # ## TODO experimental
          # ## If there are multiple direct predecessors
          # ## then the consequent probably occurs at multiple
          # ## places in the process
          # new_consequent_names <- paste(conseq, "FROM", closest_antecedents$antecedent, sep = "_")
          # new_relations <- closest_antecedents
          # new_relations$consequent <- new_consequent_names
          # rel_df <- rel_df %>%
          #   anti_join(closest_antecedents, by = c("antecedent","consequent")) %>%
          #   bind_rows(new_relations)
          #
          # for(new_name in new_consequent_names){
          #   new_antec_rels <- rel_df %>%
          #     filter(antecedent == conseq) %>%
          #     mutate(antecedent = new_name)
          #
          #   new_conseq_rels <- rel_df %>%
          #     filter(consequent == conseq) %>%
          #     mutate(consequent = new_name)
          #
          #   rel_df <- rel_df %>%
          #     bind_rows(new_antec_rels) %>%
          #     bind_rows(new_conseq_rels)
          # }
          #
          # rel_df <- rel_df %>%
          #   filter(antecedent != conseq,
          #          consequent != conseq)
          # return_list$rel_df <- rel_df
          # return(return_list)

          ## If they are concurrent or do not agree
          ## among each other
          ## soft AND them together
          sampled_par_pair <- mutual_antec_relations %>%
            sample_pair(c(RScoreDict$PARALLEL_IF_PRESENT))
          return_list <- solve_PAR_relationship(
            sampled_par_pair,
            mutual_antec_relations %>%
              mutate(rel == RScoreDict$PARALLEL_IF_PRESENT),
            snippet_dict,
            mode = "SOFT"
          )
          return_list$rel_df <- rel_df %>% remember_pair(
              sampled_par_pair,
              "OR"
            )
          return(return_list)
        }
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
      rel_df,
      snippet_dict
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
      snippet_dict,
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
      rel_df,
      snippet_dict
    )

    return(return_list)
  }
}

solve_join <- function(
  join_pair,
  rel_df,
  snippet_dict
){

  return_list <- list(
    snippet = NULL,
    activities = c(),
    rel_df = rel_df,
    snippet_dictionary = snippet_dict,
    messages = c()
  )

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
      snippet_dict
    )
    return(return_list)
  }

  if(reverse_rel == RScoreDict$REQUIRES){
    return_list <- solve_directly_follows(
      join_pair %>%
        mutate(rel == RScoreDict$DIRECTLY_FOLLOWS),
      join_pair %>%
        mutate(rel == RScoreDict$DIRECTLY_FOLLOWS),
      snippet_dict
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
      snippet_dict,
      mode = "SOFT"
    )

    return(return_list)
  }
}






