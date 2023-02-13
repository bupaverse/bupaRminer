discover_parallels_from_log <- function(ev_log,
                                        ev_activities,
                                        potential_par_activities = NULL){

  activity_colname <- activity_id(ev_log)
  activity_instance_colname <- activity_instance_id(ev_log)
  case_colname <- case_id(ev_log)
  timestamp_colname <- timestamp(ev_log)
  lifecycle_colname <- lifecycle_id(ev_log)

  rel_df <- tibble()

  nr_cases <- ev_log %>%
    n_cases

  if(is.null(potential_par_activities)){
    A_activities <- ev_activities
    B_activities <- ev_activities
  } else {
    A_activities <- potential_par_activities
    B_activities <- ev_activities
  }


  for(A in 1:(length(ev_activities)-1)){
    act_A <- as.character(ev_activities[A])

    if(!(act_A %in% A_activities)){
      next
    }



    print(act_A)

    events_A <-
      ev_log %>%
      as_tibble() %>%
      filter(!!sym(activity_colname) == act_A) %>%
      select(!!sym(case_colname), !!sym(lifecycle_colname), !!sym(timestamp_colname)) %>%
      pivot_wider(names_from=!!sym(lifecycle_colname), values_from=!!sym(timestamp_colname)) %>%
      rename(reference_timestamp_start = start,
             reference_timestamp_end = complete)


    cases_with_A <-  ev_log %>%
      inner_join(
        events_A,
        by = case_colname,
      ) %>%
      re_map(mapping(ev_log))

    for(B in (A+1):length(ev_activities)){
      act_B <- as.character(ev_activities[B])

      if(!(act_B %in% B_activities)){
        next
      }

      ## Parallel scores - Concurrent executions

      cases_with_A_and_B <- cases_with_A %>%
        filter_activity_presence(act_B)

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
