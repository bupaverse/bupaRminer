## martllops on sys_10_2

# my_log <- read_xes("data/system_10_2_1_1_0.xes")
# my_log <- sepsis
#
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

detect_loop_blocks <- function(loop_scores, repeat_rels){

  PIP <- NULL
  activity <- NULL
  loop_type <- NULL
  new_loop_block_id <- NULL
  is_start <- NULL
  is_end <- NULL
  
  assigned_rels  <- repeat_rels %>% 
    assign_relationships

  ## We want to discard irrelevant/maginal scores
  ## by introducing a threshold per relationship-type
  norm_looped_scores <- loop_scores %>% 
    group_by(antecedent, consequent) %>% 
    mutate(score = ifelse(score == max(score), score,0)) %>%
    group_by(rel) %>%
    mutate(relevant = ifelse(score >= mean(score), TRUE,FALSE)) %>%
    group_by(antecedent) %>%
    mutate(relevant = ifelse(score >= mean(score), TRUE,relevant)) %>%
    group_by(consequent) %>%
    mutate(relevant = ifelse(score >= mean(score), TRUE,relevant)) %>%
    ungroup() %>%
    mutate(score = ifelse(relevant==TRUE,1,0)) %>%
    select(-relevant) %>%
    mutate(loop_block_id = 0)

  loop_back_scores <- loop_scores %>%
    filter(rel == RScoreDict$LOOP_BACK)

  loop_block_counter <- 0
  loop_block_info_df <- tibble()
  
  loop_backs_to_solve <-  loop_back_scores %>% 
    filter(score > 0) %>%
    mutate(assigned = 0)

  ## We keep going as long as there are activities that are not assigned to a loop.
  while(loop_backs_to_solve %>%
        filter(assigned == 0) %>%
        nrow > 0
        ){
    
    print(loop_backs_to_solve)

    ## Initialize parameters for new loop block
    temp_loop_block_info_df <- tibble()
    loop_block_counter <- loop_block_counter + 1
    new_counter <- loop_block_counter

    ## We are looking for the loop block that contains most (unassigned) activities.
    ## We do this by searching for the activity that is part of the loop block of
    ## other activities.
    ## The activity (or activities) that are part of the loop block of the most activities
    ## are necessarily also the "last" activities in the loop block.
    ## F.e. in a looped sequence A-B-C-D, by design, activity D will be the consequent of
    ## a LOOP_BLOCK relation for A, B and C, whereas activity B is only the LOOP_BLOCK
    ## consequent of A. In the following code, we are looking for activity D.
    preceeding_acts <-  norm_looped_scores %>%
      filter(loop_block_id == 0) %>%
      filter(rel == RScoreDict$LOOP_BLOCK) %>%
      group_by(consequent) %>%
      summarize(score = sum(score))

    most_connected_act <- preceeding_acts %>%
      filter(score == max(score)) %>%
      head(1) %>%
      pull(consequent)

    ## Find all activities that are within the loop block.
    ## We assume now that the loop block ends with the
    ## "most connected activity", which is referred to as actiity D
    ## in the previous comment.
    loop_acts <- norm_looped_scores %>%
      filter(score == 1,
             rel == RScoreDict$LOOP_BLOCK,
             consequent == most_connected_act) %>%
      pull(antecedent) %>%
      unique %>%
      c(.,most_connected_act)


    ## There may be nested loops, so we need to check what
    ## each activity within the loop block is most likely
    ## to loop back to.
    ## We selelect the activty woth the strongest
    ## loop_back relation as the new endpoint of our loop.
    most_likely_endpoint <- norm_looped_scores %>% 
      filter(rel == RScoreDict$LOOP_BACK) %>%
      filter(score > 0) %>%
      filter(antecedent %in% loop_acts) %>%
      arrange(-score) %>%
      head(1) %>%
      pull(antecedent)

    ## If there is no loop_back activity, then we are in trouble.
    if(length(most_likely_endpoint) == 0){
      ## We first extend our search to other candidates
      ## further down the line (after the activity that
      ## we assumed to be last)
      extra_acts <- norm_looped_scores %>%
        filter(antecedent %in% loop_acts,
               rel == RScoreDict$LOOP_BLOCK,
               score > 0) %>%
        pull(consequent)

      loop_acts <- c(loop_acts, extra_acts) %>% unique()

      ## We again search for the most likely end point
      most_likely_endpoint <- loop_back_scores %>%
        filter(antecedent %in% loop_acts) %>%
        arrange(-score) %>%
        head(1) %>%
        pull(antecedent)
    }

    ## We want to know what activity to loop back to from our end point
    ## this can only be an activity that had a  LOOP_BACK score with
    ## that end point.
    most_likely_start_points <- loop_back_scores %>%
      filter(score > 0) %>%
      filter(antecedent == most_likely_endpoint)

    ## Id there are multiple potential start points,
    ## then we can downsize the list by adding a threshold
    ## to the required score.
    if(most_likely_start_points %>% nrow > 0){
      most_likely_start_points <- loop_back_scores %>%
        filter(score >= mean(score)) %>%
        filter(antecedent == most_likely_endpoint)
    }

    ## If there is at least 1 candidate, we extract all
    ## of the as potential candidates
    if(most_likely_start_points %>% nrow > 0){
      most_likely_start_points <- most_likely_start_points %>%
        pull(consequent)
    } else {

      ## Else, we are forced to move back to the extended list
      ## and select them all as candidates
      most_likely_start_points <- loop_back_scores %>%
        filter(score > 0) %>%
        filter(antecedent == most_likely_endpoint) %>%
        pull(consequent)
    }

    ### WHAT IF ZERO CANDIDATES?


    ## Gather all activities that we consider to be
    ## part of the loop. We use the most likely
    ## starting activity(es) as reference point
    loop_acts <- generics::intersect(
      norm_looped_scores %>%
        filter(
          rel==RScoreDict$LOOP_BLOCK,
          score == 1,
          antecedent %in% most_likely_start_points) %>%
        pull(consequent),
      norm_looped_scores %>%
        filter(
          rel==RScoreDict$LOOP_BLOCK,
          score == 1,
          consequent %in% most_likely_endpoint) %>%
        pull(antecedent)
    )
    
    priors_to_endpoint <- assigned_rels %>% 
      filter(rel %in% c(RScoreDict$DIRECTLY_FOLLOWS, RScoreDict$EVENTUALLY_FOLLOWS, RScoreDict$MAYBE_DIRECTLY_FOLLOWS, RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,RScoreDict$PARALLEL_IF_PRESENT, RScoreDict$ALWAYS_PARALLEL)) %>% 
      filter(consequent %in% most_likely_endpoint ) %>% 
      pull(antecedent) %>% 
      unique()
    
    loop_acts <- c(
      most_likely_start_points,
      most_likely_endpoint,
      generics::intersect(loop_acts,priors_to_endpoint)
    ) %>% unique
    
    print(loop_acts)
    loop_backs_to_solve <- loop_backs_to_solve %>%
      mutate(assigned = ifelse(
        antecedent %in% most_likely_endpoint | consequent %in% loop_acts,
        1,
        assigned
      ))
    
    ## if all loop acts are already part of a loop, 
    ## then we don't need this loop anymore
    new_loop_acts <- loop_acts[
      ! loop_acts %in% loop_block_info_df$activity
    ]
    
    if(length(new_loop_acts) == 0){
      norm_looped_scores <- norm_looped_scores %>% 
        mutate(score =
                 ifelse(
                   rel == RScoreDict$LOOP_BLOCK & score == 1 & loop_block_id==0 & consequent %in% loop_acts,
                   0,
                   score))
      next
    }
    
    ## We assign those activities to the new loop_block by reference number
    ## norm_looped_scores <- norm_looped_scores %>%
    ##   mutate(loop_block_id = ifelse(antecedent %in% loop_acts & score == 1,
    ##                                loop_block_counter,
    ##                                loop_block_id))
    if(loop_block_info_df %>% nrow > 0){
      
      activities_per_block <- loop_block_info_df %>% 
        count(new_loop_block_id)
      
      loop_blocks_with_act <- loop_block_info_df %>% 
        filter(activity %in% loop_acts) %>%
        count(new_loop_block_id )
      
      detected_inner_loops <- activities_per_block %>% 
        inner_join(loop_blocks_with_act, by="new_loop_block_id") %>%
        filter(n.x == n.y) %>%
        pull(new_loop_block_id)
      
      if(length(detected_inner_loops) == 1){
        inner_loop_name <- loop_block_info_df %>%
          filter(new_loop_block_id %in% detected_inner_loops) %>%
          pull(loop_name) %>%
          unique
        
        inner_loop_acts <- loop_block_info_df %>%
          filter(new_loop_block_id %in% detected_inner_loops) %>%
          pull(activity)
        
        loop_block_info_df <- loop_block_info_df %>%
          mutate(loop_type = ifelse(new_loop_block_id %in% detected_inner_loops,
                                    "inner",
                                    loop_type))
        
        loop_acts <- loop_acts[! loop_acts %in% inner_loop_acts]
        loop_acts <- c(loop_acts, inner_loop_name)
      }
    }
    
    ## If some of these activities are already in another loop_block
    ## in its entirety, then we can refer to that loop_block here
    
    ##If there are still loopbacks, then there are probably multiple possible end points
    remaining_loop_backs <- loop_back_scores %>%
      filter(score >= mean(score)) %>%
      filter(antecedent %in% loop_acts) %>%
      filter(!(antecedent %in% c(most_likely_endpoint, most_likely_start_points)))

    ## But we can disregard the loopback that also have a high Par if present score
    ## Since PIP is high if activities appear in a choice in a loop
    ## (First occurrence of A may always be before or after first occurrence of B)
    remaining_loop_backs <- remaining_loop_backs %>%
      inner_join(repeat_rels %>% filter(rel == RScoreDict$PARALLEL_IF_PRESENT) %>%
                   select(antecedent, consequent, PIP=score),
                 by=c("antecedent","consequent")) %>%
      filter(score >= PIP) %>%
      select(antecedent, consequent, rel, score)

    while(remaining_loop_backs %>% nrow > 0){

      ## Sekect the activity that has the strongest LOOP_BACK score
      ## This is likely an inner loop.
      second_end_point <- remaining_loop_backs %>%
        arrange(-score) %>%
        head(1) %>%
        pull(antecedent)

      ## Select all activities to which the second end point is
      ## looping back.
      second_start_points <- loop_back_scores %>%
        filter(score > 0) %>%
        filter(antecedent == second_end_point)

      ## If there are many candidate starting points for the inner loop
      ## then we can afford to be more stringent in our selection.
      if(second_start_points %>% nrow > 0){
        second_start_points <- loop_back_scores %>%
          filter(score >= mean(score)) %>%
          filter(antecedent == second_end_point)
      }
      ## Otherwise we just select all candidates as inner staring points.
      if(second_start_points %>% nrow > 0){
        second_start_points <- second_start_points %>%
          pull(consequent)
      } else {
        most_likely_start_points <- loop_back_scores %>%
          filter(antecedent == second_end_point) %>%
          pull(consequent)
      }

      ## If there is no difference between the old and the new startpoint,
      ## then we assume that both the old and the new end point are both
      ## considered as loop end point.
      ## Remember that A > B X>{A, C X>[A,..]} is the same as
      ## A > B X>[C, ] X>[A,..]
      if(setequal(most_likely_start_points, second_start_points)){
        most_likely_endpoint <- c(most_likely_endpoint, second_end_point) %>% unique
      } else{
        ## If not, we will construct an inner loop first.
        ## these must contain all activities that are part of
        ## the loop block of the inner starting activity.
        other_loop_acts <- norm_looped_scores %>%
          filter(
            rel==RScoreDict$LOOP_BLOCK,
            score == 1,
            antecedent %in% second_start_points) %>%
          pull(consequent) %>%
          c(.,second_start_points) %>%
          unique


        ## But we do not need any activity that comes
        ## after the inner loop's end point.
        acts_outside_block <- norm_looped_scores %>%
          filter(
            rel==RScoreDict$LOOP_BLOCK,
            score == 1,
            antecedent %in% second_end_point) %>%
          pull(consequent)


        acts_outside_block <- acts_outside_block[!acts_outside_block %in% second_start_points]
        other_loop_acts <- other_loop_acts[!other_loop_acts %in% acts_outside_block]

        ## Create a data structure that contains start and end activity of
        ## the inner loop block, along with the other activities inside that block.
        other_loop_block_info <- tibble(
          loop_block_id  = loop_block_counter,
          activity = other_loop_acts,
        ) %>%
          mutate(is_start = (activity %in% second_start_points),
                 is_end = (activity %in% second_end_point))

        ## Add the block to the temp loop_blocks and augment
        ## the counter.
        temp_loop_block_info_df <- temp_loop_block_info_df %>%
          bind_rows(other_loop_block_info)

        loop_block_counter <- loop_block_counter + 1
      }

      ## Remove the end points from the loopbacks so that the next
      ## one cna be examined.
      remaining_loop_backs <- remaining_loop_backs %>%
        filter(!(antecedent %in% c(second_end_point, second_start_points)))
    }

    ## If inner loop blocks are dealt with, we can collect the activites in the outer loop
    ## in a new datastructure. Start and end points must be indicated.
    loop_block_info <- tibble(
      loop_block_id  = loop_block_counter,
      activity = loop_acts,
    ) %>%
      mutate(is_start = (activity %in% most_likely_start_points),
             is_end = (activity %in% most_likely_endpoint))

    temp_loop_block_info_df <- temp_loop_block_info_df %>%
      bind_rows(loop_block_info) %>%
      mutate(loop_type = NA,
             new_loop_block_id = NA)

    ## Create a summary of outer and inner loop blocks created.
    ## Smallest should be first (remember that outer loops contain
    ## elements of the inner loop). We want to solve the inner loops
    ## first.
    block_counts <- temp_loop_block_info_df %>%
      count(loop_block_id) %>%
      arrange(n)

    ## We wull have to inspect the loop blocks iteratively.
    ## starting with the smallest block.
    while(block_counts %>% nrow > 0){
      shortest_loops <- block_counts %>%
        filter(n == min(n))

      ## There may be an equal number of shortest loops. If so, we have to
      ## deal with that.
      if(shortest_loops %>% nrow > 1){
        ## If activities overlap, we can assume that they are all part of the same loop
        act_occurrence_in_block <- temp_loop_block_info_df %>%
          filter(loop_block_id %in% shortest_loops$loop_block_id) %>%
          count(activity)

        if(act_occurrence_in_block %>% pull(n) %>% max() > 1){
          temp_loop_block_info_df <- temp_loop_block_info_df %>%
            mutate(loop_block_id = ifelse(loop_block_id %in% shortest_loops$loop_block_id,
                                          min(shortest_loops$loop_block_id),
                                          loop_block_id))

          ## The block in examination need not be considered anywmore afterwards.
          block_counts <- block_counts %>%
            filter(!loop_block_id %in% shortest_loops$loop_block_id)

          shortest_loops <- shortest_loops %>%
            filter(loop_block_id == min(loop_block_id))
        } else {
          shortest_loops <- shortest_loops %>%
            filter(loop_block_id == min(loop_block_id))
        }
      }

      ## The block in examination need not be considered anywmore afterwards.
      block_counts <- block_counts %>%
        filter(!loop_block_id %in% shortest_loops$loop_block_id)

      ## As long as there are other blocks left, we are dealing with an
      ## inner loop.
      if(block_counts %>% nrow > 0){
        LOOPTYPE = "inner"
      } else {
        LOOPTYPE = "outer"
      }

      ## Assign loopblock id and looptype to each activity in the data structure
      temp_loop_block_info_df <- temp_loop_block_info_df %>%
        mutate(loop_type = ifelse(loop_block_id == shortest_loops$loop_block_id, LOOPTYPE, loop_type),
               new_loop_block_id = ifelse(loop_block_id == shortest_loops$loop_block_id, new_counter, new_loop_block_id))
      ## We create a name that we can later use for referencing the entire loop
      loop_name <- paste("LOOP",new_counter,sep="__")
      new_counter <- new_counter + 1


      ## Abd we fulter out all activities that have been placed in the loopblock
      acts_to_replace <- temp_loop_block_info_df %>%
        filter(loop_block_id == shortest_loops$loop_block_id) %>%
        pull(activity)

      norm_looped_scores <- norm_looped_scores %>%
        filter(!(antecedent %in% acts_to_replace & consequent %in% acts_to_replace))

      ## In the outer loops, we replace the activity names with a reference to the inner loops
      ## via the loop name.
      temp_loop_block_info_df <- temp_loop_block_info_df %>%
        mutate(activity = ifelse(
          activity %in% acts_to_replace & loop_block_id  != shortest_loops$loop_block_id,
          loop_name,
          activity))
    }

    ## We now have a final data structure containing outer and inner loops with their own labels.
    ## because some activities that are renamed towards their (inner) loop_name, adjustments have to be
    ## made.
    temp_loop_block_info_df <- temp_loop_block_info_df %>%
      group_by(loop_block_id, activity) %>%
      summarize(
        is_start = max(is_start),
        is_end = max(is_end),
        loop_type = max(loop_type),
        new_loop_block_id = max(new_loop_block_id)
      ) %>%
      ungroup() %>%
      mutate(loop_block_id = new_loop_block_id,
             loop_name = paste("LOOP",new_loop_block_id,sep="__"))

    loop_block_info_df <- loop_block_info_df %>%
      bind_rows(temp_loop_block_info_df)
    
    norm_looped_scores <- norm_looped_scores %>%
      select(-loop_block_id) %>%
      left_join(loop_block_info_df %>% select(activity, loop_block_id), 
                by=c("antecedent"="activity")) %>%
      mutate(loop_block_id = ifelse(is.na(loop_block_id),0, loop_block_id))%>%
      mutate(loop_block_id = ifelse(score > 0,loop_block_id, 0))
  }

  return(loop_block_info_df)
}

solve_loop_blocks <- function(loop_block_info_df, prep_log){

  loop_blocks <- loop_block_info_df %>%
    pull(loop_block_id) %>%
    max()
  col_names <- colnames(prep_log)
  relevant_snippets <- list()
  
  has_been_skipped <- FALSE
  
  for(loop_block in 1:loop_blocks){
    
    print(loop_block)
    
    this_loop_block <- loop_block_info_df %>%
      filter(loop_block_id == loop_block)

    if(loop_block == 4){
      write_rds(loop_block_info_df, "loop_block.Rds")
      write_rds(prep_log, "loop_log.Rds")
    }
    
    if(this_loop_block %>% nrow == 0){
      has_been_skipped <<- TRUE
      print("Skip now")
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
      mutate(nr_of_starts = cumsum(is_start)) %>%
      mutate(starts_new_loop = (is_start & lag(is_end, default = TRUE))) %>%
      group_by(CID, orig_name, LC) %>%
      mutate(starts_per_act = cumsum(is_start)) %>%
      group_by(CID,LC) %>%
      mutate(start_counter = lag(cummax(starts_per_act), default=0)) %>%
      mutate(starts_new_loop = (starts_new_loop | starts_per_act > start_counter)) %>%
      group_by(CID) %>%
      mutate(loop_id = cumsum(starts_new_loop)) %>%
      ungroup()


    first_loop_log <- loop_log %>%
      filter(loop_id == 1,
             is_repeat == 1) %>%
      as.data.table()
    
    if(first_loop_log %>% pull(AID) %>% unique %>% length == 0){
      next
    }

    if(first_loop_log %>% pull(AID) %>% unique %>% length == 1){
      sole_activity <- first_loop_log %>% pull(AID) %>% unique
      print(sole_activity)
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

  result <- list()
  result$process <- relevant_snippets
  result$log <- prep_log

  return(result)
}
