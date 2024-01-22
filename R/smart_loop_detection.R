detect_loop_blocks <- function(loop_scores, repeat_rels){

  PIP <- NULL
  activity <- NULL
  loop_type <- NULL
  new_loop_block_id <- NULL
  is_start <- NULL
  is_end <- NULL
  
  assigned_rels  <- repeat_rels %>% 
    assign_relationships
  
  relevant_act_names <- c(
    loop_scores$antecedent, loop_scores$consequent
  ) %>% unique
  
  assigned_rels <- assigned_rels %>%
    filter(
      antecedent %in% relevant_act_names,
      consequent %in% relevant_act_names
    )
  
  ## Clean relationships
  ## If there are mutual relations
  ## then only the strongest should survive
  mutual_rels <- loop_scores %>% 
    inner_join(loop_scores, 
               by=c("antecedent"="consequent",
                    "consequent"="antecedent",
                    "rel"="rel"))
  
  ## Relations should only be maintained
  ## if they are stronger then their 
  ## counterpart
  relevant_rels <- mutual_rels %>%
    filter(score.x >= score.y) %>%
    select(
      antecedent, consequent, rel,
      score = score.x
    )
  
  relevant_rels <- relevant_rels %>%
    group_by(antecedent, consequent) %>%
    mutate(nr_of_rels = n()) %>%
    filter(!(rel == RScoreDict$LOOP_BACK & nr_of_rels > 1)) %>%
    ungroup() %>%
    select(-nr_of_rels)
  
  ## You can only be in a loop_block if your
  ## LBL score is better than your REQ score
  REQ_scores <- repeat_rels %>%
    filter(rel == RScoreDict$REQUIRES) %>%
    select(antecedent, consequent, rel, score)
  
  loop_block_scores <- relevant_rels %>%
    filter(rel == RScoreDict$LOOP_BLOCK) %>%
    select(antecedent, consequent, rel, score)
  
  strong_loop_block_scores <- loop_block_scores %>%
    left_join(REQ_scores,
              by=c("antecedent","consequent")) %>%
    filter(score.x > score.y) %>%
    select(antecedent, consequent)
  
  norm_looped_scores <- relevant_rels %>% 
    inner_join(
      strong_loop_block_scores,
      by=c("antecedent","consequent")
    ) %>%
    mutate(score = 1) %>%
    mutate(loop_block_id = 0)

  loop_back_scores <- relevant_rels %>%
    group_by(antecedent, consequent) %>% 
    filter(score == max(score)) %>%
    ungroup() %>%
    filter(rel == RScoreDict$LOOP_BACK)

  loop_block_counter <- 0
  loop_block_info_df <- tibble()
  
  loop_backs_to_solve <-  loop_back_scores %>% 
    filter(score > 0) %>%
    mutate(assigned = 0)
  
  ## Only loop_back scores that are greater
  ## then follows scores must be resolved
  ## the others are not considered significant
  lb_follows_rels <- loop_backs_to_solve %>%
    select(antecedent, consequent, rel, score) %>%
    left_join(repeat_rels %>%
                filter(rel == RScoreDict$MAYBE_EVENTUALLY_FOLLOWS) %>%
                select(antecedent, consequent, score),
              by=c("antecedent","consequent")) %>%
    filter(score.x >= score.y/2)
  
  loop_backs_to_solve <- lb_follows_rels %>%
    select(antecedent, consequent) %>%
    left_join(loop_backs_to_solve,
              by=c("antecedent","consequent"))

  ## We keep going as long as there are activities that are not assigned to a loop.
  while(loop_backs_to_solve %>%
        filter(assigned == 0) %>%
        nrow > 0
        ){
  
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
    
    strongest_loop_back <- loop_backs_to_solve %>%
      filter(assigned == 0) %>%
      filter(score == max(score)) %>%
      head(1)
    
    likely_end_points <- loop_extract_end_points(
      strongest_loop_back,
      assigned_rels,
      loop_backs_to_solve
    )
    
    likely_start_points <- loop_extract_start_points(
      strongest_loop_back,
      likely_end_points,
      assigned_rels,
      loop_backs_to_solve
    )
    
    loop_acts <- loop_extract_activities(
      likely_start_points,
      likely_end_points,
      norm_looped_scores,
      assigned_rels
    )
    
    likely_end_points <- likely_end_points[likely_end_points %in% loop_acts]
    
    if(length(likely_start_points) > 0 & length(likely_end_points) > 0 ){
      loop_backs_to_solve <- loop_backs_to_solve %>%
        mutate(assigned = ifelse(
          antecedent %in% likely_end_points & consequent %in% likely_start_points,
          1,
          assigned
        ))
    } else {
      loop_backs_to_solve <- loop_backs_to_solve %>%
        mutate(assigned = ifelse(
          antecedent == strongest_loop_back$antecedent & 
            consequent == strongest_loop_back$consequent,
          1,
          assigned
        ))
    }
    
    ## If some of these activities are already assigned
    ## to a loop block, then we need to replace them
    ## by their loop reference number
    
    
    ## if all loop acts are already part of a loop, 
    ## then we don't need this loop anymore
    new_loop_acts <- loop_acts
    
    if(loop_block_info_df %>% nrow > 0){
      new_loop_acts <- loop_acts[! loop_acts %in% loop_block_info_df$activity]
    }
  
    if(length(new_loop_acts) == 0){
      norm_looped_scores <- norm_looped_scores %>% 
        mutate(score =
                 ifelse(
                   rel == RScoreDict$LOOP_BLOCK & score == 1 & loop_block_id==0 & consequent %in% loop_acts,
                   0,
                   score))
      next
    }
    
    orig_loop_acts <- loop_acts
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
        
        start_in_inner <- likely_start_points[likely_start_points %in% inner_loop_acts]
        end_in_inner <- likely_end_points[likely_end_points %in% inner_loop_acts]
        if(length(start_in_inner) > 0){
          likely_start_points <- likely_start_points[!likely_start_points %in% start_in_inner]
          likely_start_points <- c(likely_start_points, inner_loop_name)
        } else if(length(end_in_inner) > 0){
          likely_end_points <- likely_end_points[!likely_end_points %in% end_in_inner]
          likely_end_points <- c(likely_end_points, inner_loop_name)
        }
        
      }
    }
    lost_loop_acts <- orig_loop_acts[!orig_loop_acts %in% loop_acts]
    
    ## If some of these activities are already in another loop_block
    ## in its entirety, then we can refer to that loop_block here
    
    ##If there are still loopbacks, then there are probably multiple possible end points
    remaining_loop_backs <- loop_backs_to_solve %>%
      filter(antecedent %in% loop_acts) %>%
      filter(!(antecedent %in% c(lost_loop_acts,
                                 likely_end_points, 
                                 likely_start_points)),
             !consequent %in% lost_loop_acts)

    while(remaining_loop_backs %>% nrow > 0){

      ## Sekect the activity that has the strongest LOOP_BACK score
      ## This is likely an inner loop.
      strongest_inner_loop_back <- remaining_loop_backs %>%
        arrange(-score) %>%
        head(1)
      
      inner_end_points <- loop_extract_end_points(
        strongest_inner_loop_back,
        assigned_rels,
        loop_backs_to_solve
      )
      
      inner_start_points <- loop_extract_start_points(
        strongest_inner_loop_back,
        inner_end_points,
        assigned_rels,
        loop_backs_to_solve
      )
      
      inner_loop_acts <- loop_extract_activities(
        inner_start_points,
        inner_end_points,
        norm_looped_scores,
        assigned_rels
      )
      
      starts_innerloop <- likely_start_points[!likely_start_points %in% inner_start_points]
      
      if(length(starts_innerloop) > 0){
        inner_loop_acts <- inner_loop_acts[!inner_loop_acts %in% starts_innerloop]
      }
      
      ## Create a data structure that contains start and end activity of
      ## the inner loop block, along with the other activities inside that block.
      other_loop_block_info <- tibble(
        loop_block_id  = loop_block_counter,
        activity = inner_loop_acts,
      ) %>%
        mutate(is_start = (activity %in% inner_start_points),
               is_end = (activity %in% inner_end_points))
      
      ## Add the block to the temp loop_blocks and augment
      ## the counter.
      temp_loop_block_info_df <- temp_loop_block_info_df %>%
        bind_rows(other_loop_block_info)
      
      loop_block_counter <- loop_block_counter + 1

      ## Remove the end points from the loopbacks so that the next
      ## one cna be examined.
      remaining_loop_backs <- remaining_loop_backs %>%
        filter(!(antecedent %in% c(inner_end_points, inner_start_points)))
    }

    ## If inner loop blocks are dealt with, we can collect the activities in the outer loop
    ## in a new datastructure. Start and end points must be indicated.
    loop_block_info <- tibble(
      loop_block_id  = loop_block_counter,
      activity = loop_acts,
    ) %>%
      mutate(is_start = (activity %in% likely_start_points),
             is_end = (activity %in% likely_end_points))

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

  loop_block_info_df <- loop_block_info_df %>%
    arrange(loop_block_id)
  return(loop_block_info_df)
}


loop_extract_end_points <- function(strongest_loop_back, assigned_rels, loop_backs_to_solve){
  
  most_connected_act <- strongest_loop_back %>%
    pull(antecedent)
  
  ## We mus texamine whether there are activities
  #" that happen in parallel or exlcusive to the
  ## selected activity.
  ## PLease note that exclusive activities are
  ## recognised as PIP whe they occur in a loop.
  ## Think about it and you'll understand.
  par_end_acts <- assigned_rels %>%
    filter(antecedent == most_connected_act,
           rel %in% c(RScoreDict$PARALLEL_IF_PRESENT,
                      RScoreDict$ALWAYS_PARALLEL,
                      RScoreDict$MUTUALLY_EXCLUSIVE))
  
  ## Only the ones that also loopback to the same
  ## activity need to be preserved
  par_end_acts <- par_end_acts %>%
    filter(consequent %in% (loop_backs_to_solve %>%
                              filter(
                                assigned == 0,
                                consequent == strongest_loop_back$consequent) %>%
                              pull(antecedent))) %>%
    pull(consequent)
  
  likely_end_points <- c(most_connected_act,
                         par_end_acts) %>% unique
  
  return(likely_end_points)
}

loop_extract_start_points <- function(
    strongest_loop_back,
    likely_end_points,
    assigned_rels,
    loop_backs_to_solve){
  
  ## The same applies to the starting point
  par_start_acts <- assigned_rels %>%
    filter(antecedent == strongest_loop_back$consequent,
           rel %in% c(RScoreDict$PARALLEL_IF_PRESENT,
                      RScoreDict$ALWAYS_PARALLEL,
                      RScoreDict$MUTUALLY_EXCLUSIVE))
  
  ## And we only need the ones where the end points
  ## loop back to
  par_start_acts <- par_start_acts %>%
    filter(consequent %in% (loop_backs_to_solve %>%
                              filter(
                                assigned == 0,
                                antecedent %in% likely_end_points) %>%
                              pull(consequent))) %>%
    pull(consequent)
  
  likely_start_points <- c(strongest_loop_back$consequent,
                           par_start_acts) %>% unique
  
  return(likely_start_points)
}

loop_extract_activities <- function(
    likely_start_points,
    likely_end_points,
    norm_looped_scores,
    assigned_rels
    ) {
  ## We now need to look for the activities that
  #" happen within this loop block that start and
  ## ands with the discoverd activities.
  acts_from_start <- norm_looped_scores %>%
    filter(score == 1,
           rel == RScoreDict$LOOP_BLOCK,
           antecedent %in% likely_start_points) %>%
    pull(consequent) %>%
    unique
  
  
  leads_to_end_pint <- assigned_rels %>% 
    filter(rel %in% c(RScoreDict$DIRECTLY_FOLLOWS, RScoreDict$EVENTUALLY_FOLLOWS, RScoreDict$MAYBE_DIRECTLY_FOLLOWS, RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,RScoreDict$PARALLEL_IF_PRESENT, RScoreDict$ALWAYS_PARALLEL)) %>% 
    filter(consequent %in% likely_end_points ) 
  
  follows_start_point <- assigned_rels %>% 
    filter(rel %in% c(RScoreDict$DIRECTLY_FOLLOWS, RScoreDict$EVENTUALLY_FOLLOWS, RScoreDict$MAYBE_DIRECTLY_FOLLOWS, RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,RScoreDict$PARALLEL_IF_PRESENT, RScoreDict$ALWAYS_PARALLEL)) %>% 
    filter(antecedent %in% likely_start_points ) 
  
  priors_to_endpoint <- leads_to_end_pint %>%
    filter(antecedent %in% follows_start_point$consequent) %>% 
    pull(antecedent) %>% 
    unique()
  
  acts_from_start <- c(
    acts_from_start, priors_to_endpoint
  ) %>% unique
  
  ## But we do not need the activities that go
  ## beyond the end points. (We may be dealing with
  ## an inner loop.
  acts_from_end <- norm_looped_scores %>%
    filter(score == 1,
           rel == RScoreDict$LOOP_BLOCK,
           antecedent %in% likely_end_points) %>%
    pull(consequent) %>%
    unique
  
  acts_from_end_withou_end_points <- acts_from_end[!acts_from_end %in% likely_end_points]
  
  if(length(acts_from_end_withou_end_points) == 0){
    acts_from_end <- acts_from_end_withou_end_points
  }
  
  loop_acts <- acts_from_start[!acts_from_start %in% acts_from_end]
  loop_acts <- c(
    likely_start_points,
    # likely_end_points,
    loop_acts
  ) %>% unique
  
  return(loop_acts)
}
