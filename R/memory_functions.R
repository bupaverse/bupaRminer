remember_pair <- function(selected_pair,
                          inspect_type,
                          rel_df){
  
  rel_df <- rel_df %>%
    mutate(selected_row = (antecedent == selected_pair$antecedent & consequent == selected_pair$consequent)) %>%
    mutate(
      inspection_sequence = ifelse(selected_row, max(rel_df$inspection_sequence)+1, inspection_sequence),
      inspection_type = ifelse(selected_row, inspect_type, inspection_type),
      current_focus = ifelse(selected_row, TRUE, FALSE)
    ) %>%
    select(-selected_row)
  
  # print(rel_df %>%
  #        filter(!is.na(inspection_type)) %>%
  #        select(antecedent, consequent, rel, inspection_sequence, inspection_type, current_focus) %>%
  #        arrange(-inspection_sequence) %>%
  #        head(5))
  return(rel_df)
}

reset_memory <- function(rel_df){
  
  rel_df <- rel_df %>%
    mutate(
      inspection_sequence = 0,
      inspection_type = NA,
      current_focus = FALSE
    )
  
  return(rel_df)
}

retrieve_memory_log <- function(rel_df){
  if(rel_df %>% nrow > 1){
    memory_log <- rel_df %>%
      filter(!is.na(inspection_type)) %>%
      mutate(current_focus = FALSE) %>%
      select(antecedent, consequent, rel, inspection_sequence, inspection_type, current_focus) %>%
      arrange(inspection_sequence)
    return(memory_log)
  } else {
    return(rel_df)
  }
}