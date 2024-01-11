get_closest_antecedents <- function(
    starting_pair,
    rel_df){
  others_preceeding_conseq <- rel_df %>%
    filter(consequent == starting_pair$consequent) %>%
    filter(rel %in% c(RScoreDict$DIRECTLY_FOLLOWS,
                      RScoreDict$EVENTUALLY_FOLLOWS,
                      RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                      RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
                      RScoreDict$DIRECT_JOIN))
  
  new_end_points <- c(
    others_preceeding_conseq$antecedent %>% unique,
    starting_pair$consequent)
  
  others_preceeding_conseq <- rel_df %>%
    filter(consequent %in% new_end_points) %>%
    filter(rel %in% c(RScoreDict$DIRECTLY_FOLLOWS,
                      RScoreDict$EVENTUALLY_FOLLOWS,
                      RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                      RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
                      RScoreDict$DIRECT_JOIN))
  
  closest_antecedents <- others_preceeding_conseq %>%
    group_by(antecedent) %>%
    mutate(nr_connections = n()) %>%
    filter(consequent == starting_pair$consequent) %>%
    ungroup() %>%
    filter(nr_connections == min(nr_connections))
  
  return(closest_antecedents)
}