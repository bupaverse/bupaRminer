assign_relationships <- function(relationships_df,
                                 mode = "variable"){
  
  if(mode == "fixed"){
    # Set all rel values above threshold to 1
    masked_df <- relationships_df %>%
      left_join(smart_thres_df) %>%
      mutate(rounded_score = as.numeric(score >= rel_thres),
             rel = factor(rel, levels = R_levels, ordered = TRUE)) %>%
      select(-rel_thres)
  } else{
    masked_df <- relationships_df %>%
      filter(score > 0) %>%
      group_by(rel) %>%
      mutate(reference_score = mean(score)) %>%
      ungroup() %>%
      mutate(reference_score = ifelse(rel %in% c(RScoreDict$PARALLEL_IF_PRESENT,RScoreDict$ALWAYS_PARALLEL),
                                      mean(score),
                                      reference_score)) %>%
      filter(score >= reference_score) %>%
      mutate(score = round(score,1)) %>%
      group_by(antecedent, consequent) %>%
      mutate(rounded_score = (score == max(score)),
             rel = factor(rel, levels = R_levels, ordered = TRUE)) %>%
      ungroup() %>%
      select(-reference_score)
  }
  
  # ## Filter out dominant relationship pased on R_levels
  assigned_rel_df <- masked_df %>%
    filter(rounded_score == 1) %>%
    group_by(antecedent, consequent) %>%
    arrange(rel) %>%
    filter(row_number() == 1) %>%
    ungroup()
  
  return(assigned_rel_df)
}