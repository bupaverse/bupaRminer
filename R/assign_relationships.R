assign_relationships <- function(relationships_df,
                                 mode = "fixed"){
  
  if(mode == "fixed"){
    # Set all rel values above threshold to 1
    masked_df <- relationships_df %>%
      left_join(smart_thres_df) %>%
      mutate(rounded_score = as.numeric(score >= rel_thres),
             rel = factor(rel, levels = R_levels, ordered = TRUE)) %>%
      select(-rel_thres)
  } else{
    masked_df <- relationships_df %>%
      filter(score > mean(score)) %>%
      mutate(score = round(score,1)) %>%
      # group_by(rel) %>%
      # mutate(rel_thres = max(score)) %>%
      # mutate(rel_thres = rel_thres - sd(rel_thres)) %>%
      # ungroup() %>%
      # mutate(rounded_score = ifelse(score < rel_thres, 0, score),
      #        rel = factor(rel, levels = R_levels, ordered = TRUE)) %>%
      # group_by(antecedent, consequent) %>%
      mutate(rounded_score = (score == max(score))) %>%
      ungroup() %>%
      select(-rel_thres)
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