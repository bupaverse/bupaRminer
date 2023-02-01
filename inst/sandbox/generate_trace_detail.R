library(rlang) 

generate_trace_detail <- function(ev_log){
  
  activity_colname <- activity_id(ev_log)
  activity_instance_colname <- activity_instance_id(ev_log)
  case_colname <- case_id(ev_log)
  timestamp_colname <- timestamp(ev_log)
  lifecycle_colname <- lifecycle_id(ev_log)
  
  ev_log_With_trace <- ev_log %>% 
    as_tibble() %>%
    unite("new_act", c(activity_colname, lifecycle_colname), remove=FALSE) %>% 
    group_by(!!sym(case_colname)) %>% 
    mutate(trace_string = hash(new_act)) %>% 
    # mutate(trace_string = paste(new_act, collapse = "___")) %>% 
    ungroup()
  
  trace_counts  <- ev_log_With_trace  %>% 
    select(!!sym(case_colname), trace_string) %>% 
    unique %>% 
    count(trace_string)
  
  case_numbers <- c()
  
  for(trace in trace_counts$trace_string){
    new_case <- ev_log_With_trace  %>% 
      filter(trace_string == trace) %>% 
      sample_n(1) %>% 
      pull(!!sym(case_colname))
    case_numbers <- c(case_numbers, new_case)
  }
  
  trace_detail <- ev_log_With_trace %>%
    filter(!!sym(case_colname) %in% case_numbers) %>%
    left_join(trace_counts)
  
  trace_detail <- trace_detail %>% 
    select(
      trace_id = trace_string,
      !!sym(activity_colname),
      !!sym(lifecycle_colname),
      example_timestamp = !!sym(timestamp_colname),
      n_occurrences = n
      ) %>%
    mutate(trace_id = as.factor(trace_id)) %>%
    group_by(trace_id) %>%
    arrange(trace_id, example_timestamp) %>%
    mutate(seq = row_number()) %>%
    ungroup() %>%
    arrange(-n_occurrences, trace_id, seq)
  
  return(trace_detail)
}
