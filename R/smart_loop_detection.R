## martllops on sys_10_2

my_log <- read_xes("data/system_10_2_4_5_3.xes")

my_log <- my_log %>% preprocess

repeating_acts <- my_log %>%
  filter(is_repeat > 1) %>%
  pull(orig_name) %>%
  unique()

repeat_log <- my_log %>%
  filter(orig_name %in% repeating_acts) %>%
  filter(is_repeat <= 2)

repeat_rels <- repeat_log %>%
  mutate(is_repeat = 1) %>%
  calculate_relationships(source='main')

loop_scores <- tibble()
for(act_a in repeating_acts){
  act_a_rep <- my_log %>%
    filter(orig_name == act_a,
           is_repeat == 2) %>%
    pull(AID) %>%
    unique
  for(act_b in repeating_acts){
    if(act_a == act_b){
      next
    }
    act_b_rep <- my_log %>%
      filter(orig_name == act_b,
             is_repeat == 2) %>%
      pull(AID) %>%
      unique
    print(paste("Analysing", act_a, act_b, sep = " "))
    sef_a_b <- repeat_rels %>%
      filter(rel == RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
             antecedent == act_a,
             consequent == act_b)
    sef_b_arep <- repeat_rels %>%
      filter(rel == RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
             antecedent == act_b,
             consequent == act_a_rep)
    
    scores_a_b <- repeat_rels %>%
      filter(antecedent == act_a,
             consequent == act_b) %>%
      pull(score)
    
    scores_a2_b2 <- repeat_rels %>%
      filter(antecedent == act_a_rep,
             consequent == act_b_rep) %>%
      pull(score)
    
    score_diff <- mean(abs(scores_a_b - scores_a2_b2))
    final_score <- (1-score_diff)*sef_a_b%>% pull(score)*sef_b_arep%>% pull(score)
    
    loop_backs <- my_log %>%
      filter(CID %in% (my_log %>%
               filter(orig_name == act_b,
                      is_repeat == 2) %>%
               pull(CID)))
    
    cutoffs <- loop_backs %>% 
      filter(AID == act_a,
             LC == "complete") %>%
      mutate(ref_timestamp = TS) %>%
      select(CID, ref_timestamp, CASE_COUNT)
    
    loop_backs <- loop_backs %>%
      full_join(cutoffs %>% select(-CASE_COUNT), by = "CID") %>%
      filter(TS > ref_timestamp) %>%
      filter(LC == "start") %>%
      group_by(CID) %>%
      filter(TS== min(TS)) %>%
      ungroup() %>%
      filter(orig_name == act_b,
             is_repeat == 2)
    
    loop_back_score <- (loop_backs %>%
      pull(CASE_COUNT) %>% 
      sum()) / (cutoffs %>% pull(CASE_COUNT) %>% sum())
    
    if(is.na(loop_back_score)){
      loop_back_score <- 0
    }
    
    loop_scores <- loop_scores %>%
      bind_rows(
        tibble(
          antecedent = act_a,
          consequent = act_b,
          rel = c(RScoreDict$LOOP_BLOCK, RScoreDict$LOOP_BACK),
          score = c(final_score, loop_back_score)
        )
      )
  }
  
} 

norm_looped_scores <- loop_scores %>%
  group_by(rel) %>%
  mutate(score = ifelse(score >= mean(score), 1,0)) %>%
  mutate(loop_block_id = 0) %>%
  ungroup()

loop_back_scores <- loop_scores %>% 
  filter(rel == RScoreDict$LOOP_BACK, 
         score > 0)

loop_block_counter <- 0
loop_block_info_df <- tibble()
while(norm_looped_scores %>% filter(rel == RScoreDict$LOOP_BLOCK, score == 1, loop_block_id==0) %>% nrow() > 0){
  loop_block_counter <- loop_block_counter + 1
  
  preceeding_acts <-  norm_looped_scores %>% 
    filter(loop_block_id == 0) %>%
    filter(rel == RScoreDict$LOOP_BLOCK) %>%
    group_by(consequent) %>% 
    summarize(score = sum(score))
  
  most_connected_act <- preceeding_acts %>%
    filter(score == max(score)) %>%
    head(1) %>%
    pull(consequent)
  
  loop_acts <- norm_looped_scores %>% 
    filter(score == 1, 
           rel == RScoreDict$LOOP_BLOCK,
           consequent == most_connected_act) %>%
    pull(antecedent) %>%
    unique %>%
    c(.,most_connected_act)
  
  norm_looped_scores <- norm_looped_scores %>% 
    mutate(loop_block_id = ifelse(antecedent %in% loop_acts & score == 1,
           loop_block_counter,
           loop_block_id))
  
  most_likely_endpoint <- loop_back_scores %>%
    filter(antecedent %in% loop_acts) %>%
    arrange(-score) %>%
    head(1) %>%
    pull(antecedent)
  
  most_likely_start_points <- loop_back_scores %>%
    filter(antecedent == most_likely_endpoint) %>%
    pull(consequent)
  
  loop_acts <- c(
    loop_acts,
    norm_looped_scores %>% 
      filter(
        rel==RScoreDict$LOOP_BLOCK,
        score == 1,
        antecedent %in% most_likely_start_points) %>%
      pull(consequent)
  ) %>%
    unique
  
  remaining_loop_backs <- loop_back_scores %>%
    filter(antecedent %in% loop_acts) %>%
    filter(!(antecedent %in% c(most_likely_endpoint, most_likely_start_points)))
  
  ##If there are still loopbacks, then there are probably myultiple possibel end points
  while(remaining_loop_backs %>% nrow > 0){
    
    second_end_point <- remaining_loop_backs %>%
      arrange(-score) %>%
      head(1) %>%
      pull(antecedent)
    
    print(second_end_point)
    
    second_start_points <- loop_back_scores %>%
      filter(antecedent %in% loop_acts) %>%
      filter(antecedent == second_end_point) %>%
      pull(consequent)
    
    print(second_start_points)
    
    if(setequal(most_likely_start_points, second_start_points)){
      most_likely_endpoint <- c(most_likely_endpoint, second_end_point)
    } else{
      inner_loop_block_info <- tibble(
        loop_block_id  = loop_block_counter,
        activity = loop_acts,
      ) %>%
        mutate(is_start = (activity %in% second_start_points),
               is_end = (activity %in% second_end_point))
      
      loop_block_info_df <- loop_block_info_df %>%
        bind_rows(loop_block_info)
      
      loop_block_counter <- loop_block_counter + 1
    }
    
    remaining_loop_backs <- remaining_loop_backs %>%
      filter(!(antecedent %in% c(second_end_point, second_start_points)))
  }
  
  loop_block_info <- tibble(
    loop_block_id  = loop_block_counter,
    activity = loop_acts,
  ) %>%
    mutate(is_start = (activity %in% most_likely_start_points),
           is_end = (activity %in% most_likely_endpoint))
  
  loop_block_info_df <- loop_block_info_df %>%
    bind_rows(loop_block_info)
}

loop_blocks <- loop_block_info_df %>%
  pull(loop_block_id) %>%
  max()

dup_log <- my_log
relevant_snippets <- list()
for(loop_block in 1:loop_blocks){
  
  this_loop_block <- loop_block_info_df %>% 
    filter(loop_block_id == loop_block)
  
  loop_log <- dup_log %>%
    filter(orig_name %in% this_loop_block$activity) %>%
    full_join(this_loop_block %>% select(-loop_block_id),
              by = c("orig_name"="activity")) %>%
    mutate(is_start = ifelse(LC=="start",is_start, FALSE),
           is_end = ifelse(LC=="complete",is_end, FALSE)) %>%
    group_by(CID) %>%
    mutate(nr_of_starts = cumsum(is_start)) %>%
    mutate(starts_new_loop = (is_start & lag(is_end, default = TRUE))) %>%
    mutate(loop_id = cumsum(starts_new_loop)) %>%
    ungroup()
  
  
  first_loop_log <- loop_log %>%
    filter(loop_id == 1,
           is_repeat == 1) %>%
    as.data.table()
  
  first_loop_rel <- calculate_relationships(first_loop_log, source = "main") %>%
    assign_relationships()
  
  first_loop_snippet <- construct_process(first_loop_rel,
                                          list(), source = "main")
  
  relevant_snippet <- names(first_loop_snippet)[[length(first_loop_snippet)]]
  relevant_snippets[[relevant_snippet]] <- first_loop_snippet[[relevant_snippet]] %>%
    add_loop_back()
  
  new_log <- loop_log %>% 
    group_by(CID) %>% 
    filter((LC=="start" & TS == min(TS)) | (LC=="complete" & TS== max(TS))) %>% 
    mutate(AID = relevant_snippet) %>%
    mutate(orig_name = AID,
           new_act_name = AID,
           AIID = min(AIID),
           block_content = min(block_content)) %>%
    ungroup() %>%
    mutate(
      act_lc = paste(AID,LC,sep="___"),
      is_repeat = 1
    )
  
  my_log <- my_log %>%
    filter(!orig_name %in% unique(loop_log$AID)) %>%
    bind_rows(new_log) %>% 
    arrange(CID, TS)
}

all_rel <- calculate_relationships(my_log, source = "main") %>%
  assign_relationships()

all_snippets <- construct_process(all_rel,
                                  relevant_snippets, source = "main")
