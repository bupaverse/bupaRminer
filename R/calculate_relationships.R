

calculate_relationships <- function(eventlog,
                                    skip_self_loops = FALSE) {
  rel_df <- calculate_relationship_scores(eventlog,
                                          skip_self_loops)



  # Set all rel values above threshold to 1
  masked_df <- rel_df %>%
    left_join(smart_thres_df) %>%
    mutate(rounded_score = as.numeric(score >= rel_thres),
           rel = factor(rel, levels = R_levels, ordered = TRUE)) %>%
    select(-rel_thres)

  # ## Filter out dominant relationship pased on R_levels
  assigned_rel_df <- masked_df %>%
    filter(rounded_score == 1) %>%
    group_by(antecedent, consequent) %>%
    arrange(rel) %>%
    filter(row_number() == 1) %>%
    ungroup()
}
