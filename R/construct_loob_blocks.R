
construct_loop_blocks <- function(event_log,
                                  mode = c("all","skip")){
  
  if(length(mode) == 1 & mode == "all"){
    repeat_indicator <- 1
  } else {
    repeat_indicator <- 2
  }
  
  snippet_dictionary <- list()
  
  loop_blocks <- discover_loop_block(event_log)
  if(!is.null(loop_blocks) && loop_blocks %>% nrow > 1){
    number_of_blocks <- loop_blocks %>% pull(loop_block_id) %>% max
    for(loop_block in c(1:number_of_blocks)){
      block_activities <- loop_blocks %>%
        filter(loop_block_id == loop_block) %>%
        pull(antecedent) %>%
        unique
      
      block_log <- event_log %>%
        filter(orig_name %in% block_activities,
               is_repeat == repeat_indicator)
      
      cli::cli_alert_info("Calculate block relationships")
      block_relationships <- calculate_relationships(block_log,
                                                     skip_self_loops = TRUE)
      assigned_block_relationships <- assign_relationships(block_relationships)
      cli::cli_alert_info("Construct block process")
      block_process <- construct_process(assigned_block_relationships)
      ## Get relevant snippet
      snippet_name <- names(block_process)[length(block_process)]
      block_bpmn_obj <- block_process[[length(block_process)]]
      ## Add loopback
      block_bpmn_obj <- add_loop_back(block_bpmn_obj)
      
      snippet_dictionary[[snippet_name]] <- block_bpmn_obj
      ## Modify event_log
      new_log <- event_log %>%
        filter(orig_name %in% block_activities,
               is_repeat >= repeat_indicator) %>%
        group_by(!!sym(case_id(event_log)), !!sym(lifecycle_id(event_log))) %>%
        summarise(
          !!sym(activity_id(event_log)) := snippet_name,
          !!sym(timestamp(event_log)) := min(!!sym(timestamp(event_log))),
          early_ts = min(!!sym(timestamp(event_log))),
          late_ts = max(!!sym(timestamp(event_log))),
          !!sym(activity_instance_id(event_log)) := min(!!sym(activity_instance_id(event_log))),
          orig_name = snippet_name,
          new_act_name = snippet_name,
          is_repeat = 1,
          .order = min(.order)
        ) %>%
        ungroup %>%
        mutate(
          !!sym(timestamp(event_log)) := if_else(!!sym(lifecycle_id(event_log)) == "start",
                                                early_ts,
                                                late_ts)
        ) %>% 
        select(-early_ts, -late_ts)
      
      event_log <- event_log %>%
        filter(!(orig_name %in% block_activities & is_repeat >= repeat_indicator)) %>%
        bind_rows(new_log)
    }
  }
  
  return_list <- list(
    new_log = event_log,
    snippet_dictionary = snippet_dictionary
  )
  
}