calculate_loop_scores <- function(repeat_rels, prep_log){
  ref_timestamp <- NULL
  
  repeating_acts <- prep_log %>%
    filter(is_repeat > 1) %>%
    pull(orig_name) %>%
    unique()
  
  loop_scores <- tibble()
  for(act_a in repeating_acts){
    act_a_rep <- prep_log %>%
      filter(orig_name == act_a,
             is_repeat == 2) %>%
      pull(AID) %>%
      unique
    for(act_b in repeating_acts){
      if(act_a == act_b){
        next
      }
      act_b_rep <- prep_log %>%
        filter(orig_name == act_b,
               is_repeat == 2) %>%
        pull(AID) %>%
        unique
      
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
        arrange(rel) %>%
        pull(score)
      
      scores_a2_b2 <- repeat_rels %>%
        filter(antecedent == act_a_rep,
               consequent == act_b_rep) %>%
        arrange(rel) %>%
        pull(score)
      
      score_diff <- mean(abs(scores_a_b - scores_a2_b2))
      final_score <- (1-score_diff)*sef_a_b%>% pull(score)*sef_b_arep%>% pull(score)
      
      loop_backs <- prep_log %>%
        filter(CID %in% (prep_log %>%
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
               is_repeat >= 2)
      
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
  return(loop_scores)
}