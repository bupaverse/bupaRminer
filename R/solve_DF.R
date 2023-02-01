solve_DF_relationship <- function(
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

  antec <- rel_pair$antecedent
  conseq <- rel_pair$consequent

  ## We need to check if other important
  ## relationships towards B exist.
  ## F.e. if there are 2 DF relationships,
  ## Then there should be a join before B
  ## and o direct flow between A and B.
  other_conseq_rels <- rel_df %>%
    filter(consequent == conseq)

  ## We also need to check the relation
  ## of B towards A.
  relation_from_b_to_a <- rel_df %>%
    filter(
      antecedent == conseq,
      consequent == antec) %>%
    pull(rel)

  ## If we have multiple DF relationships towards
  ## the same consequent, then this means
  ## that we actually are performing a join.
  ## If A -> B = DF and B->A != REQ, then this
  ## means that A is a rare optional activity
  ## that directly preceeds B if it happens.
  if(other_conseq_rels %>%
     filter(rel == RScoreDict$DIRECTLY_FOLLOWS) %>%
     nrow() > 1  || ( length(relation_from_b_to_a) > 0 && relation_from_b_to_a != RScoreDict$REQUIRES)){

    ## ifelse functions hate factors
    ## therefore, we use a detour

    new_rows <- other_conseq_rels %>%
      filter(rel == RScoreDict$DIRECTLY_FOLLOWS) %>%
      mutate(rel = RScoreDict$DIRECT_JOIN )
    rel_df <- rel_df %>%
      filter(!(consequent == conseq & rel == RScoreDict$DIRECTLY_FOLLOWS )) %>%
      bind_rows(new_rows)

    msg <- (paste("Morphed", other_conseq_rels %>%
                    filter(rel ==RScoreDict$DIRECTLY_FOLLOWS) %>%
                    nrow(), RScoreDict$DIRECTLY_FOLLOWS, "relationships to", RScoreDict$DIRECT_JOIN))

    return_list <- list(
      snippet = NULL,
      activities = c(),
      rel_df = rel_df,
      snippet_dictionary = snippet_dict,
      messages = msg
    )

  } else{

    snippet_name <- paste(antec, conseq, sep = " >> ")

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
  }

  return(return_list)
}
