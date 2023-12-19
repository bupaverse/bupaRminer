calculate_loop_relations <- function(prep_log){
  repeating_acts <- prep_log %>%
    filter(is_repeat > 1) %>%
    pull(orig_name) %>%
    unique()
  
  if(length(repeating_acts) == 1){
    return(tibble())
  }
  
  repeat_log <- prep_log %>%
    filter(orig_name %in% repeating_acts) %>%
    filter(is_repeat <= 2)
  
  repeat_rels <- repeat_log %>%
    mutate(is_repeat = 1) %>%
    calculate_relationships(source='main')
  
  return(repeat_rels)
}