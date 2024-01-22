
solve_loop_blocks <- function(loop_block_info_df, prep_log){
  
  relevant_snippets <- list()
  
  loop_block_info_df <- loop_block_info_df %>%
    clean_loop_blocks()
  
  if(loop_block_info_df %>% nrow > 0){
    
    loop_blocks <- loop_block_info_df %>%
      pull(loop_block_id) %>%
      max()
    col_names <- colnames(prep_log)
    
    for(loop_block in 1:loop_blocks){
      
      this_loop_block <- loop_block_info_df %>%
        filter(loop_block_id == loop_block)
      
      if(this_loop_block %>% nrow == 0){
        next
      }
      
      loop_log <- prep_log %>%
        filter(orig_name %in% this_loop_block$activity) %>%
        full_join(this_loop_block %>% select(-loop_block_id),
                  by = c("orig_name"="activity")) %>%
        mutate(is_start = ifelse(LC=="start",is_start, FALSE),
               is_end = ifelse(LC=="complete",is_end, FALSE)) %>%
        group_by(CID) %>%
        arrange(TS) %>%
        mutate(
          status = ifelse(is_start, "searching_end",ifelse(is_end,"searching_start",NA))
        ) %>%
        fill(status, .direction = "down") %>%
        fill(status, .direction = "up") %>%
        mutate(next_status = lag(status)) %>%
        mutate(ends_loop = (status != next_status & status == "searching_start" )) %>%
        mutate(ends_loop = ifelse(is.na(ends_loop),FALSE, ends_loop)) %>%
        mutate(starts_new_loop = lag(ends_loop, default = TRUE)) %>%
        mutate(loop_id = cumsum(starts_new_loop)) %>%
        mutate(loop_id = loop_id) %>%
        ungroup()
        
        
         
      
      ## mutate(nr_of_starts = cumsum(is_start)) %>%
      ## mutate(nr_of_ends = cumsum(is_end))
      ## loop_log <- loop_log %>%
      ##  mutate(starts_new_loop = (is_start & lag(is_end, default = TRUE))) %>%
      ##  group_by(CID, orig_name, LC) %>%
      ##  mutate(starts_per_act = cumsum(is_start)) %>%
      ##  group_by(CID,LC) %>%
      ##  mutate(start_counter = lag(cummax(starts_per_act), default=0)) %>%
      ##  mutate(starts_new_loop = (starts_new_loop | starts_per_act > start_counter)) %>%
      ##  group_by(CID) %>%
      ##  mutate(loop_id = cumsum(starts_new_loop)) %>%
      ##  ungroup()
      
      
      first_loop_log <- loop_log %>%
        filter(loop_id == 1,
               is_repeat == 1) %>%
        as.data.table()
      
      if(first_loop_log %>% pull(AID) %>% unique %>% length == 0){
        next
      }
      
      if(first_loop_log %>% pull(AID) %>% unique %>% length == 1){
        sole_activity <- first_loop_log %>% pull(AID) %>% unique
        sole_activity <- decode_task(
          sole_activity, 
          relevant_snippets,
          "START","END"
        )
        first_loop_snippet <- add_loop_back(sole_activity)
        loop_name <- this_loop_block$loop_name[1]
        relevant_snippets[[loop_name]] <- first_loop_snippet 
      } else {
        first_loop_rel <- calculate_relationships(first_loop_log, source = "main") %>%
          assign_relationships()
        
        first_loop_snippet <- construct_process(first_loop_rel,
                                                relevant_snippets, source = "main")
        
        relevant_snippet <- names(first_loop_snippet)[[length(first_loop_snippet)]]
        
        loop_name <- this_loop_block$loop_name[1]
        relevant_snippets[[loop_name]] <- first_loop_snippet[[relevant_snippet]] %>%
          add_loop_back()
      }
      
      
      if(this_loop_block$loop_type %>% unique %in% c("outer")){
        new_log <- loop_log %>%
          group_by(CID) %>%
          filter((LC=="start" & TS == min(TS)) | (LC=="complete" & TS== max(TS))) %>%
          mutate(AID = loop_name) %>%
          mutate(orig_name = AID,
                 new_act_name = AID,
                 AIID = min(AIID),
                 block_content = min(block_content)) %>%
          group_by(CID, LC) %>%
          filter(row_number()  == min(row_number())) %>%
          ungroup() %>%
          mutate(
            act_lc = paste(AID,LC,sep="___"),
            is_repeat = 1
          )
      } else {
        new_log <- loop_log %>%
          group_by(CID, loop_id) %>%
          filter((LC=="start" & TS == min(TS)) | (LC=="complete" & TS== max(TS))) %>%
          mutate(
            AIID = min(AIID),
            block_content = min(block_content)
          )%>%
          group_by(CID, loop_id, LC) %>%
          filter(row_number()  == min(row_number())) %>%
          ungroup() %>%
          mutate(
            AID = loop_name,
            orig_name = loop_name,
            new_act_name = loop_name,
            act_lc = paste(loop_name,LC,sep="___"),
            is_repeat = loop_id
          )
      }
      
      prep_log <- prep_log %>%
        filter(!orig_name %in% unique(loop_log$AID)) %>%
        bind_rows(new_log) %>%
        arrange(CID, TS)
      
      prep_log <- prep_log[,..col_names]
    }
  }
  
  result <- list()
  result$process <- relevant_snippets
  result$log <- prep_log
  
  return(result)
}

clean_loop_blocks <- function(loop_block_df){
  no_end <- loop_block_df %>%
    group_by(loop_block_id) %>%
    summarize(nr_ends = sum(is_end)) %>%
    filter(nr_ends == 0)
  
  no_start <- loop_block_df %>%
    group_by(loop_block_id) %>%
    summarize(nr_starts = sum(is_start)) %>%
    filter(nr_starts == 0)
  
  new_loop_block_df <- loop_block_df %>%
    mutate(is_start = ifelse(loop_block_id %in% no_end$loop_block_id & is_end == 0, 1, is_start))%>%
    mutate(is_start = ifelse(loop_block_id %in% no_start$loop_block_id & is_start == 0, 1, is_end))
  
  return(new_loop_block_df)
}