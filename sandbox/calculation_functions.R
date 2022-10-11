RScoreDict <- list(
  DIRECTLY_FOLLOWS = "R1",
  DIRECT_JOIN = "Rx",
  EVENTUALLY_FOLLOWS = "R2",
  MAYBE_DIRECTLY_FOLLOWS = "R3",
  MAYBE_EVENTUALLY_FOLLOWS = "R4",
  PARALLEL_IF_PRESENT = "R5",
  ALWAYS_PARALLEL = "R6",
  MUTUALLY_EXCLUSIVE = "R7",
  HAPPENS_DURING = "R8",
  TERMINATING = "R9",
  REQUIRES = "REQ"
)


calculate_relationship_scores <- function(ev_log){
  
  activity_colname <- activity_id(ev_log)
  activity_instance_colname <- activity_instance_id(ev_log)
  case_colname <- case_id(ev_log)
  timestamp_colname <- timestamp(ev_log)
  lifecycle_colname <- lifecycle_id(ev_log)
  
  ## First we must establish parallel activities as they clutter 
  ## our perception of the sequence of activities.
  rel_df <- discover_parallels_from_log(ev_log,
                                        ev_log %>% 
                                          filter(!(!!sym(activity_colname) %in% c("START","END"))) %>% 
                                          pull(orig_name) %>% 
                                          unique)
  
  ## Retrieve activities that have duplicates
  duplicated_activities <- ev_log %>%
    as_tibble() %>%
    count(orig_name,
          new_act_name) %>%
    filter(as.character(orig_name) != as.character(new_act_name)) %>%
    pull(orig_name) %>%
    unique()
  
  
  ## Check if there are immediate repeat activities
  par_thres <- 0.80
  
  renamed_entries <- discover_self_loops(
    ev_log,
    duplicated_activities,
    rel_df %>% filter(rel %in% c(RScoreDict$PARALLEL_IF_PRESENT, RScoreDict$ALWAYS_PARALLEL)),
    parallel_thres = par_thres)
  
  ## Merge self_loops into one entry with start and end
  if(renamed_entries %>% nrow > 0){
    ev_log <- ev_log %>% 
      left_join(renamed_entries, 
                by = c(case_colname, activity_instance_colname)) %>% 
      as_tibble() %>% 
      mutate(!!sym(activity_colname) := as.character(!!sym(activity_colname))) %>% 
      mutate(!!sym(activity_colname) := ifelse(is.na(new_concatenated_name),
                                               !!sym(activity_colname),
                                               new_concatenated_name)) %>% 
      group_by(!!sym(case_colname), !!sym(activity_colname),!!sym(lifecycle_colname)) %>% 
      arrange(!!sym(timestamp_colname)) %>% 
      mutate(rep_occurence = row_number()) %>% 
      mutate(!!sym(activity_instance_colname) := paste(!!sym(activity_instance_colname),collapse="_")) %>% 
      filter((!!sym(lifecycle_colname)=="start" & rep_occurence == min(rep_occurence)) | 
               (!!sym(lifecycle_colname)=="complete" & rep_occurence == max(rep_occurence))) %>%
      ungroup() %>%
      re_map(mapping(ev_log))
  }
  
  ## Check again for parallel relationships, but now taking the self loops into account
  rel_df <- discover_parallels_from_log(ev_log,
                                        ev_log %>%
                                          filter(!(!!sym(activity_colname) %in% c("START","END"))) %>%
                                          pull(!!sym(activity_colname)) %>% unique)
  
  
  ## Then we calculate the other scores
  cases_with_act_memory <- obtain_case_ids_per_activity(ev_log)
  
  
  all_activities <- ev_log %>%
    pull(!!sym(activity_colname)) %>%
    unique()
  
  rel_df_2 <- discover_R_sequence_relations(
    ev_log,
    all_activities,
    rel_df,
    parallel_thres = 0.90,
    cases_per_act_memory = cases_with_act_memory
  )
  
  rel_df <- rel_df %>%
    bind_rows(rel_df_2)
  
  return(rel_df)
}

obtain_case_ids_per_activity <- function(ev_log){
  
  activity_colname <- activity_id(ev_log)
  activity_instance_colname <- activity_instance_id(ev_log)
  case_colname <- case_id(ev_log)
  timestamp_colname <- timestamp(ev_log)
  lifecycle_colname <- lifecycle_id(ev_log)
  
  ev_activities <- ev_log %>% 
    activities %>% 
    pull(!!sym(activity_colname))
  
  cases_per_act <- list()
  
  for(act in ev_activities){
    cases_per_act[[act]] <- ev_log %>%
      filter(!!sym(activity_colname) == act) %>%
      pull(!!sym(case_colname)) %>%
      unique
  }
  
  return(cases_per_act)
}

discover_parallels_from_log <- function(
    ev_log,
    ev_activities){
  
  activity_colname <- activity_id(ev_log)
  activity_instance_colname <- activity_instance_id(ev_log)
  case_colname <- case_id(ev_log)
  timestamp_colname <- timestamp(ev_log)
  lifecycle_colname <- lifecycle_id(ev_log)
  
  rel_df <- tibble()
  
  nr_cases <- ev_log %>%
    n_cases
  
  for(A in c(1:(length(ev_activities)-1))){
    act_A <- as.character(ev_activities[A])
    print(act_A)
    
    cases_with_A <-  ev_log %>%
      filter_activity_presence(act_A) %>%
      as_tibble() %>%
      mutate(reference_timestamp = ifelse(!!sym(activity_colname) == act_A,
                                          !!sym(timestamp_colname),
                                          NA)) %>% 
      group_by(!!sym(case_colname)) %>%
      mutate(reference_timestamp_start = min(reference_timestamp, na.rm = TRUE),
             reference_timestamp_end = max(reference_timestamp, na.rm = TRUE)) %>%
      ungroup() %>%
      re_map(mapping(ev_log))
    
    for(B in c((A+1):length(ev_activities))){
      act_B <- as.character(ev_activities[B])
      
      ## Parallel scores - Concurrent executions
      
      cases_with_A_and_B <- cases_with_A %>%
        filter_activity_presence(c(act_A, act_B), method = "all")
      
      if(cases_with_A_and_B %>% nrow() == 0){
        full_par_score <- 0
        par_score <- 0
      } else {
        
        fromA_event_log <- cases_with_A_and_B %>%
          filter(!!sym(timestamp_colname) >= reference_timestamp_start)
        
        untilA_event_log <- cases_with_A_and_B %>%
          filter(!!sym(timestamp_colname) <= reference_timestamp_start)
        
        A_starts_before_B_ends <- fromA_event_log %>%
          filter(!!sym(activity_colname) == act_B,
                 !!sym(lifecycle_colname) == "complete" ) %>%
          pull(case_colname) %>%
          n_distinct()
        
        A_starts_after_B_starts <- untilA_event_log %>%
          filter(!!sym(activity_colname) == act_B,
                 !!sym(lifecycle_colname) == "start" ) %>%
          pull(case_colname) %>%
          n_distinct()
        
        par_score <- 1 - ( abs(A_starts_before_B_ends - A_starts_after_B_starts) / cases_with_A_and_B %>% 
                            pull(!!sym(case_colname)) %>% 
                            n_distinct() )
        
        ## We lower the R score as A and B are less likely to occur together
        modifier <- ( cases_with_A_and_B %>% n_cases ) / ( cases_with_A %>% n_cases )
        full_par_score <- par_score * modifier
      }
      
      new_row <- data.frame(
        "antecedent" = c(act_A, act_B, act_A, act_B),
        "consequent" = c(act_B, act_A, act_B, act_A),
        "rel" = c(RScoreDict$ALWAYS_PARALLEL, RScoreDict$ALWAYS_PARALLEL, RScoreDict$PARALLEL_IF_PRESENT, RScoreDict$PARALLEL_IF_PRESENT),
        "score" = c(full_par_score, full_par_score, par_score, par_score))
      
      rel_df <- rel_df %>%
        bind_rows(new_row)
    }
  
  }
  
  return(rel_df)
  
}

discover_self_loops <- function(
    ev_log,
    duplicated_activities,
    rel_par_df,
    parallel_thres = 0.95){
  
  ## Check if there are immediate repeat activities
  
  renamed_entries <- tibble()
  
  activity_colname <- activity_id(ev_log)
  activity_instance_colname <- activity_instance_id(ev_log)
  case_colname <- case_id(ev_log)
  timestamp_colname <- timestamp(ev_log)
  lifecycle_colname <- lifecycle_id(ev_log)
  
  nr_cases <- ev_log %>%
    n_cases
  
  for(dup_act in duplicated_activities){
    
    print(dup_act)
    
    par_relationships <- rel_par_df %>%
      filter(antecedent == dup_act,
             score >= parallel_thres) %>%
      pull(consequent)
    
    cases_with_repeated_A <- ev_log %>%
      filter_activity_presence(dup_act) %>%
      as_tibble() %>%
      mutate(reference_timestamp = ifelse(as.character(orig_name) == as.character(dup_act),
                                          !!sym(timestamp_colname),
                                          NA)) %>% 
      group_by(!!sym(case_colname)) %>%
      mutate(reference_timestamp_first_start = min(reference_timestamp, na.rm = TRUE),
             reference_timestamp_last_end = max(reference_timestamp, na.rm = TRUE)) %>%
      filter(!(orig_name %in% par_relationships)) %>%
      mutate(relevant_repeat = ifelse(orig_name == dup_act, is_repeat - 1, 0)) %>%
      mutate(has_repeated_A = (sum(relevant_repeat) > 0)) %>%
      ungroup() %>%
      filter(has_repeated_A == TRUE) %>%
      re_map(mapping(ev_log))
    
    ## Autorepetitions
    cases_between_As <- cases_with_repeated_A %>%
      as_tibble() %>%
      filter(!!sym(timestamp_colname) >= reference_timestamp_first_start,
             !!sym(timestamp_colname) <= reference_timestamp_last_end) 
    
    autorepetitions_of_A <- cases_between_As %>%
      filter(!!sym(lifecycle_colname) == "start") %>%
      mutate(is_not_act_A = (orig_name != dup_act)) %>%
      group_by(!!sym(case_colname)) %>%
      arrange(!!sym(case_colname), !!sym(timestamp_colname)) %>% 
      mutate(acts_in_between = cumsum(is_not_act_A)) %>%
      mutate(part_of_chain = (lag(acts_in_between) == acts_in_between)) %>%
      fill(part_of_chain, .direction = "up") %>%
      mutate(start_of_chain = (part_of_chain == TRUE) & ( lag(part_of_chain) == FALSE | is.na(lag(part_of_chain))) ) %>%
      mutate(chain_nr = cumsum(start_of_chain)) %>%
      ungroup() %>%
      filter(part_of_chain == TRUE) %>%
      mutate(new_concatenated_name = ifelse(part_of_chain==TRUE,
                                            paste(orig_name,"REP",chain_nr, sep = "_"),
                                            as.character(orig_name)))
    
    renamed_entry <- autorepetitions_of_A %>%
          select(!!sym(case_colname),
                 !!sym(activity_instance_colname),
                 new_concatenated_name)
    
    renamed_entries <- renamed_entries %>%
      bind_rows(renamed_entry)
  }
  
  return(renamed_entries)
}

calculate_exclusive_relation <- function(
  act_A,
  act_B,
  cases_with_A,
  cases_with_B,
  exclusive_thres,
  nr_cases
){
  occurs_together <- cases_with_A %>%
    filter(!!sym(activity_colname) == act_B) %>%
    n_cases
  
  expected_together <- ( cases_with_B %>%
                           n_cases )
  
  if(occurs_together > (exclusive_thres * expected_together) ){
    EXCL_score <- 0
  } else {
    EXCL_score <- 1 - ( occurs_together / ( cases_with_A %>% n_cases ) )
  }
  
  EXCL_importance <- (1- occurs_together) / nr_cases
  
  EXCL_return <- list(
    "score" = EXCL_score,
    "importance" = EXCL_importance
  )
  
  return(EXCL_return)
}

calculate_requirement_score <- function(
    act_A,
    act_B,
    cases_with_A,
    nr_cases){
  B_before_A <- cases_with_A %>%
    filter(
      !!sym(activity_colname) == act_B,
      !!sym(lifecycle_colname) == "complete",
      !!sym(timestamp_colname) <= reference_timestamp_start) %>%
    n_cases()
  
  REQ_score <- B_before_A / cases_with_A %>% n_cases()
  
  REQ_importance <- B_before_A / nr_cases
  
  REQ_return <- list(
    "score" = REQ_score,
    "importance" = REQ_importance
  )
  
  
  return(REQ_return)
}

calculate_directly_follows_relation <- function(
  actA,
  actB,
  cases_with_A,
  afterA_event_log,
  nr_cases){
  
  score <- 0
  
  B_happens_directly_after <- afterA_event_log %>%
    as_tibble() %>%
    filter(!!sym(lifecycle_colname) == "start") %>%
    group_by(!!sym(case_colname)) %>%
    arrange(!!sym(timestamp_colname)) %>%
    mutate(seq = row_number()) %>%
    ungroup() %>%
    filter(!!sym(activity_colname) == actB,
           seq == 1)
  
  score <- (B_happens_directly_after %>% 
                         pull(!!sym(case_colname)) %>% n_distinct()) / 
    (cases_with_A %>% 
       pull(!!sym(case_colname)) %>% 
       n_distinct)
  
  DF_importance <- (B_happens_directly_after %>% 
                      pull(!!sym(case_colname)) %>% n_distinct()) / nr_cases
  
  DF_return <- list(
    "score" = score,
    "importance" = DF_importance
  )
  
  return(DF_return)
}

calculate_eventually_follows_relation <- function(
    actA,
    actB,
    cases_with_A,
    afterA_event_log,
    nr_cases){
  
  score <- 0
  
  B_happens_after <- afterA_event_log %>%
    filter(!!sym(activity_colname) == actB,
           !!sym(lifecycle_colname) == "start")
  
  score <- (B_happens_after %>% 
                         n_cases) / 
    (cases_with_A %>% 
       pull(!!sym(case_colname)) %>% 
       n_distinct)
  
  EF_importance <- (B_happens_after %>% 
    n_cases) / nr_cases
  
  EF_return <- list(
    "score" = score,
    "importance" = EF_importance
  )
  
  return(EF_return)
}

calculate_sometimes_directly_follows_relation <- function(
    act_A,
    act_B,
    cases_with_A,
    cases_with_B,
    afterA_event_log,
    cases_before_B,
    par_relationships_B,
    nr_cases){
  
  B_happens_directly_after <- cases_before_B %>%
    as_tibble() %>%
    filter(!!sym(lifecycle_colname) == "complete") %>%
    filter(!(!!sym(activity_colname) %in% par_relationships_B)) %>%
    group_by(!!sym(case_colname)) %>%
    arrange(!!sym(timestamp_colname)) %>%
    mutate(seq = row_number()) %>%
    filter(!!sym(activity_colname) == act_A,
           seq == max(seq)) %>%
    ungroup()
  
  SOMETIMES_DIRECT_1 <- (B_happens_directly_after %>% 
                           pull(!!sym(case_colname)) %>% 
                           n_distinct) / 
    (cases_with_B %>% 
       pull(!!sym(case_colname)) %>% 
       n_distinct)
  
  SOMETIMES_DIRECT_2 <- (afterA_event_log %>%
                           as_tibble() %>%
                           filter(!!sym(lifecycle_colname) == "start") %>%
                           group_by(!!sym(case_colname)) %>%
                           arrange(!!sym(timestamp_colname)) %>%
                           mutate(seq = row_number()) %>%
                           ungroup() %>%
                           filter(!!sym(activity_colname) == act_B,
                                  seq == 1) %>%
                           pull(!!sym(case_colname)) %>% 
                           n_distinct) / 
    (cases_with_A %>% 
       pull(!!sym(case_colname)) %>% 
       n_distinct)
  
  SOMETIMES_DIRECT_3 <- (cases_with_B %>%
                           pull(!!sym(case_colname)) %>% 
                           n_distinct) / nr_cases
  
  SOMETIMES_DIRECT <- SOMETIMES_DIRECT_1 * (1 - abs(SOMETIMES_DIRECT_2 - SOMETIMES_DIRECT_3))
  
  return(SOMETIMES_DIRECT)
}

calculate_sometime_follows_relation <- function(
    act_A,
    act_B,
    cases_with_A,
    cases_with_B,
    fromA_event_log,
    cases_before_B,
    nr_cases){
  
  tryCatch(
    {
      SOMETIMES_FOL_1 <- 0
      B_happens_after <- cases_before_B %>%
        filter_activity_presence(act_A)
      
      SOMETIMES_FOL_1 <- (B_happens_after %>% 
                            pull(!!sym(case_colname)) %>% 
                            n_distinct) / 
        (cases_with_B %>% 
           pull(!!sym(case_colname)) %>% 
           n_distinct)
      
    },
    error = function(e){
      SOMETIMES_FOL_1 <- 0
    },
    warning = function(w){
    },
    finally = {
    }
  )
  
  tryCatch(
    {
      SOMETIMES_FOL_2 <- 0
      SOMETIMES_FOL_2 <- (fromA_event_log %>%
                            filter_activity_presence(act_B) %>% 
                            pull(!!sym(case_colname)) %>% 
                            n_distinct) / 
        (cases_with_A %>% 
           pull(!!sym(case_colname)) %>% 
           n_distinct)
    },
    error = function(e){
      SOMETIMES_FOL_2 <- 0
    },
    warning = function(w){
    },
    finally = {
    }
  )
  
  SOMETIMES_FOL_3 <- (cases_with_B %>%
                        pull(!!sym(case_colname)) %>% 
                        n_distinct) / nr_cases
  
  SOMETIMES_FOL <- SOMETIMES_FOL_1 * (1 - abs(SOMETIMES_FOL_2 - SOMETIMES_FOL_3))
  
  return(SOMETIMES_FOL)
}

calculate_terminating_relationship <- function(
  act_A,
  act_B,
  cases_with_A,
  cases_with_B,
  interrupting_theta
){
  events_killing_A <- cases_with_A %>%
    filter(!!sym(timestamp_colname) >= reference_timestamp_end - interrupting_theta,
           !!sym(timestamp_colname) <= reference_timestamp_end + interrupting_theta,
           !!sym(lifecycle_colname) == "start")
  
  B_killing_A <- events_killing_A %>%
    as_tibble() %>%
    filter(!!sym(activity_colname) == act_B)
  
  INTERRUPTING_score <- (B_killing_A %>% 
                           pull(!!sym(case_colname)) %>% 
                           n_distinct) / 
    (cases_with_B %>% 
       pull(!!sym(case_colname)) %>% 
       n_distinct)
  
  return(INTERRUPTING_score)
}

calculate_intermittent_relationship <- function(
    act_A,
    act_B,
    cases_with_A,
    cases_with_B
    ){
  
  events_started_during_A <- cases_with_A %>%
    filter(!!sym(timestamp_colname) > reference_timestamp_start,
           !!sym(timestamp_colname) < reference_timestamp_end,
           !!sym(lifecycle_colname) == "start")
  
  B_started_during_A <- events_started_during_A %>%
    as_tibble() %>%
    filter(!!sym(activity_colname) == act_B)
  
  DURING_score <- (B_started_during_A %>% 
                     pull(!!sym(case_colname)) %>% 
                     n_distinct) / 
    (cases_with_B %>% 
       pull(!!sym(case_colname)) %>% 
       n_distinct)
  
  return(DURING_score)
}

discover_R_sequence_relations <- function(
    ev_log,
    ev_activities,
    rel_par_df,
    parallel_thres = 0.95,
    exclusive_thres = 0.95,
    interrupting_theta = 0,
    cases_per_act_memory = NULL
){
  
  activity_colname <- activity_id(ev_log)
  activity_instance_colname <- activity_instance_id(ev_log)
  case_colname <- case_id(ev_log)
  timestamp_colname <- timestamp(ev_log)
  lifecycle_colname <- lifecycle_id(ev_log)
  
  nr_cases <- ev_log %>%
    n_cases
  
  rel_df <- tibble()
  
  for(A in c(1:(length(ev_activities)-1))){
    
    prec_act <-as.character(ev_activities[[A]])
    print(prec_act)
    
    par_relationships <- rel_par_df %>%
      filter(rel %in% c(RScoreDict$PARALLEL_IF_PRESENT, RScoreDict$ALWAYS_PARALLEL),
             antecedent == prec_act,
             score >= parallel_thres) %>%
      pull(consequent) %>%
      unique
    
    if(!is.null(cases_per_act_memory)){
      cases_with_A <- ev_log %>%
        filter(!!sym(case_colname) %in% cases_per_act_memory[[prec_act]])
    } else {
      cases_with_A <- ev_log %>%
        filter_activity_presence(prec_act)
    }
    
    cases_with_A <- cases_with_A %>%
      as_tibble() %>%
      mutate(reference_timestamp = ifelse(!!sym(activity_colname) == prec_act,
                                          !!sym(timestamp_colname),
                                          NA)) %>% 
      group_by(!!sym(case_colname)) %>%
      mutate(reference_timestamp_start = min(reference_timestamp, na.rm = TRUE),
             reference_timestamp_end = max(reference_timestamp, na.rm = TRUE)) %>%
      ungroup() %>%
      re_map(mapping(ev_log))
    
    fromA_event_log <- cases_with_A %>%
      filter(!!sym(timestamp_colname) >= reference_timestamp_start) %>%
      filter(!(!!sym(activity_colname) %in% par_relationships))
    
    afterA_event_log <- cases_with_A %>%
      filter(!!sym(timestamp_colname) >= reference_timestamp_end) %>%
      filter(!(!!sym(activity_colname) %in% par_relationships))
    
    for(B in c((A+1):length(ev_activities))){
      
      succ_act <- as.character(ev_activities[[B]])
      
      if(!is.null(cases_per_act_memory)){
        cases_with_B <- ev_log %>%
          filter(!!sym(case_colname) %in% cases_per_act_memory[[succ_act]])
      } else {
        cases_with_B <- ev_log %>%
          filter_activity_presence(succ_act)
      }
      
      cases_with_B <- cases_with_B %>%
        as_tibble() %>%
        mutate(reference_timestamp = ifelse(!!sym(activity_colname) == succ_act,
                                            !!sym(timestamp_colname),
                                            NA)) %>% 
        group_by(!!sym(case_colname)) %>%
        mutate(reference_timestamp_start = min(reference_timestamp, na.rm = TRUE),
               reference_timestamp_end = max(reference_timestamp, na.rm = TRUE)) %>%
        ungroup() %>%
        re_map(mapping(ev_log))
      
      ## REQ - The execution of A requires the execution of B as a predecessor
      REQ_score <- calculate_requirement_score(
        prec_act,
        succ_act,
        cases_with_A,
        nr_cases)
      
      REQ_importance <- REQ_score$importance
      REQ_score <- REQ_score$score
      
      REQ_score_reverse <- calculate_requirement_score(
        succ_act,
        prec_act,
        cases_with_B,
        nr_cases)
      
      REQ_importance_reverse <- REQ_score_reverse$importance
      REQ_score_reverse <- REQ_score_reverse$score
      
      ## Mutually exclusive 
      EXCL_score <- calculate_exclusive_relation(
        prec_act,
        succ_act,
        cases_with_A,
        cases_with_B,
        exclusive_thres,
        nr_cases
      )
      
      EXCL_importance <- EXCL_score$importance
      EXCL_score <- EXCL_score$score
      
      EXCL_score_reverse <- calculate_exclusive_relation(
        succ_act,
        prec_act,
        cases_with_B,
        cases_with_A,
        exclusive_thres,
        nr_cases
      )
      
      EXCL_importance_reverse <- EXCL_score_reverse$importance
      EXCL_score_reverse <- EXCL_score_reverse$score
      
      fromB_event_log <- cases_with_B %>%
        filter(!!sym(timestamp_colname) >= reference_timestamp_start) %>%
        filter(!(!!sym(activity_colname) %in% par_relationships))
      
      afterB_event_log <- cases_with_B %>%
        filter(!!sym(timestamp_colname) >= reference_timestamp_end) %>%
        filter(!(!!sym(activity_colname) %in% par_relationships))
      
      ## R2 score - Eventual consequent
      
      EVENTUALLY_score <- calculate_eventually_follows_relation(
        prec_act,
        succ_act,
        cases_with_A,
        afterA_event_log,
        nr_cases
        ) 
      
      EVENTUALLY_importance <- EVENTUALLY_score$importance
      EVENTUALLY_score <- EVENTUALLY_score$score
      
      EVENTUALLY_score_reverse <- calculate_eventually_follows_relation(
        succ_act,
        prec_act,
        cases_with_B,
        afterB_event_log,
        nr_cases
      ) 
      
      EVENTUALLY_importance_reverse <- EVENTUALLY_score_reverse$importance
      EVENTUALLY_score_reverse <- EVENTUALLY_score_reverse$score
      
      ## R1 score - Immediate consequent
      DIRECT_FOL_score <- calculate_directly_follows_relation(
        prec_act,
        succ_act,
        cases_with_A,
        afterA_event_log,
        nr_cases
        )
      
      DIRECT_FOL_importance <- DIRECT_FOL_score$importance
      DIRECT_FOL_score <- DIRECT_FOL_score$score
      
      DIRECT_FOL_score_reverse <- calculate_directly_follows_relation(
        succ_act,
        prec_act,
        cases_with_B,
        afterB_event_log,
        nr_cases
      )
      
      DIRECT_FOL_importance_reverse <- DIRECT_FOL_score_reverse$importance
      DIRECT_FOL_score_reverse <- DIRECT_FOL_score_reverse$score
      
      ## R4 Score - Sometimes eventually happens
      
      par_relationships_B <- rel_par_df %>%
        filter(rel %in% c(RScoreDict$PARALLEL_IF_PRESENT, RScoreDict$ALWAYS_PARALLEL),
               antecedent == succ_act,
               score >= parallel_thres) %>%
        pull(consequent)
      
      cases_before_B <- cases_with_B %>%
        filter(!!sym(timestamp_colname) <= reference_timestamp_start)
      
      cases_before_A <- cases_with_A %>%
        filter(!!sym(timestamp_colname) <= reference_timestamp_start)
      
      SOMETIMES_FOL <- calculate_sometime_follows_relation(
        prec_act,
        succ_act,
        cases_with_A,
        cases_with_B,
        fromA_event_log,
        cases_before_B,
        nr_cases)
      
      SOMETIMES_FOL_reverse <- calculate_sometime_follows_relation(
        succ_act,
        prec_act,
        cases_with_B,
        cases_with_A,
        fromB_event_log,
        cases_before_A,
        nr_cases)
      
      ## R3 Score - Sometimes immediately happens
      
      SOMETIMES_DIRECT <- calculate_sometimes_directly_follows_relation(
        prec_act,
        succ_act,
        cases_with_A,
        cases_with_B,
        afterA_event_log,
        cases_before_B,
        par_relationships_B,
        nr_cases)
      
      SOMETIMES_DIRECT_reverse <- calculate_sometimes_directly_follows_relation(
        succ_act,
        prec_act,
        cases_with_B,
        cases_with_A,
        afterB_event_log,
        cases_before_A,
        par_relationships,
        nr_cases)
      
      ## B interrupts A
      
      INTERRUPTING_score <- calculate_terminating_relationship(
        prec_act,
        succ_act,
        cases_with_A,
        cases_with_B,
        interrupting_theta
      )
      
      INTERRUPTING_score_reverse <- calculate_terminating_relationship(
        succ_act,
        prec_act,
        cases_with_B,
        cases_with_A,
        interrupting_theta
      )
      
      ## B starts during A
      DURING_score <- calculate_intermittent_relationship(
        prec_act,
        succ_act,
        cases_with_A,
        cases_with_B
      )
      DURING_score_reverse <- calculate_intermittent_relationship(
        succ_act,
        prec_act,
        cases_with_B,
        cases_with_A
      )
      
      new_row_AB <- tibble(
        "antecedent" = prec_act,
        "consequent" = succ_act,
        "rel" = c(RScoreDict$DIRECTLY_FOLLOWS, 
                  RScoreDict$EVENTUALLY_FOLLOWS,
                  RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                  RScoreDict$MAYBE_EVENTUALLY_FOLLOWS, 
                  RScoreDict$MUTUALLY_EXCLUSIVE, 
                  RScoreDict$TERMINATING,
                  RScoreDict$HAPPENS_DURING,
                  RScoreDict$REQUIRES),
        "score" = c(DIRECT_FOL_score, 
                    EVENTUALLY_score, 
                    SOMETIMES_DIRECT, 
                    SOMETIMES_FOL, 
                    EXCL_score, 
                    INTERRUPTING_score, 
                    DURING_score, 
                    REQ_score),
        "importance" = c(DIRECT_FOL_importance,
                         EVENTUALLY_importance,
                         DIRECT_FOL_importance,
                         EVENTUALLY_importance,
                         EXCL_importance,
                         DIRECT_FOL_importance,
                         DIRECT_FOL_importance,
                         REQ_importance))
      
      
      new_row_BA <- tibble(
        "antecedent" = succ_act,
        "consequent" = prec_act,
        "rel" = c(RScoreDict$DIRECTLY_FOLLOWS, 
                  RScoreDict$EVENTUALLY_FOLLOWS,
                  RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                  RScoreDict$MAYBE_EVENTUALLY_FOLLOWS, 
                  RScoreDict$MUTUALLY_EXCLUSIVE, 
                  RScoreDict$TERMINATING,
                  RScoreDict$HAPPENS_DURING,
                  RScoreDict$REQUIRES),
        "score" = c(DIRECT_FOL_score_reverse, 
                    EVENTUALLY_score_reverse, 
                    SOMETIMES_DIRECT_reverse, 
                    SOMETIMES_FOL_reverse, 
                    EXCL_score_reverse, 
                    INTERRUPTING_score_reverse, 
                    DURING_score_reverse, 
                    REQ_score_reverse),
        "importance" = c(DIRECT_FOL_importance_reverse,
                         EVENTUALLY_importance_reverse,
                         DIRECT_FOL_importance_reverse,
                         EVENTUALLY_importance_reverse,
                         EXCL_importance_reverse,
                         DIRECT_FOL_importance_reverse,
                         DIRECT_FOL_importance_reverse,
                         REQ_importance_reverse))
      
      rel_df <- rel_df %>%
        bind_rows(new_row_AB) %>%
        bind_rows(new_row_BA)
    }
  }
  
  return(rel_df)
}
