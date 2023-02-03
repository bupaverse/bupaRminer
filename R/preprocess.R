

preprocess <- function(eventlog) {


    eventlog <- select(eventlog)

    activity_colname <- activity_id(eventlog)
    activity_instance_colname <- activity_instance_id(eventlog)
    case_colname <- case_id(eventlog)
    timestamp_colname <- timestamp(eventlog)
    lifecycle_colname <- lifecycle_id(eventlog)
    
    ## Add opposite lifecycle if not present
    lifecycle_summary <- eventlog %>% 
      as_tibble() %>%
      count(!!sym(lifecycle_colname))
    
    if(lifecycle_summary %>% nrow == 1){
      if(lifecycle_summary %>% pull(!!sym(lifecycle_colname)) == "start"){
        missing_lifecycle_name <- "complete"
      } else {
        missing_lifecycle_name <- "start"
      }
      eventlog <- eventlog %>%
        bind_rows(
          eventlog %>% mutate(!!sym(lifecycle_colname) := missing_lifecycle_name)
        )
    }

    ## Add START and END events
    start_events <- tibble(
        !!sym(case_colname) := eventlog %>% pull(!!sym(case_colname)) %>% unique,
        !!sym(timestamp_colname) := eventlog %>% pull(!!sym(timestamp_colname)) %>% min() - 10000,
        !!sym(activity_instance_colname) := paste(eventlog %>% pull(!!sym(case_colname)) %>% unique,"START", sep="_"),
        !!sym(activity_colname) := "START"
    )

    end_events <- tibble(
        !!sym(case_colname) := eventlog %>% pull(!!sym(case_colname)) %>% unique,
        !!sym(timestamp_colname) := eventlog %>% pull(!!sym(timestamp_colname)) %>% max() + 10000,
        !!sym(activity_instance_colname) := paste(eventlog %>% pull(!!sym(case_colname)) %>% unique,"END", sep="_"),
        !!sym(activity_colname) := "END"
    )

    eventlog <- eventlog %>%
        bind_rows(
            start_events %>% mutate(!!sym(lifecycle_colname) := "start"),
            start_events %>%mutate(!!sym(lifecycle_colname) := "complete"),
            end_events %>% mutate(!!sym(lifecycle_colname) := "start"),
            end_events %>%mutate(!!sym(lifecycle_colname) := "complete")
        )

    completed_only <- eventlog %>%
        filter(!!sym(lifecycle_colname) == "complete") %>%
        re_map(mapping(eventlog))

    ## Calculate time elapsed since first event in case
    ## Assign ordinal timestemp per event in case
    completed_only <- completed_only %>%
        as_tibble() %>%
        group_by(!!sym(case_colname)) %>%
        arrange(!!sym(timestamp_colname)) %>%
        mutate(timestep = row_number()) %>%
        ungroup()

    ## Append suffix _sequence_number to activity names
    completed_only <- completed_only  %>%
        as_tibble() %>%
        group_by(!!sym(case_colname),!!sym(activity_colname)) %>%
        mutate(first_act_occ_step = min(timestep)) %>%
        mutate(is_repeat = (timestep > first_act_occ_step)) %>%
        mutate(is_repeat = cumsum(is_repeat),
               is_repeat = is_repeat + 1) %>%
        ungroup() %>%
        mutate(
            !!sym(activity_colname) := as.character(!!sym(activity_colname) ),
            !!sym(activity_colname) := ifelse(is_repeat > 1,
                                              paste(!!sym(activity_colname), is_repeat, sep = "_"),
                                              !!sym(activity_colname)))

    # activity_occurrences <- completed_only %>%
    #     activities
    #
    # all_activities <- activity_occurrences %>%
    #     pull(!!sym(activity_colname))

    eventlog %>%
        left_join(completed_only %>%
                      as_tibble() %>%
                      select(!!sym(case_colname),
                             !!sym(activity_instance_colname),
                             new_act_name = !!sym(activity_colname),
                             is_repeat),
                  by = c(case_colname, activity_instance_colname)) %>%
        mutate(orig_name = !!sym(activity_colname),
               !!sym(activity_colname) := new_act_name) %>%
      re_map(mapping(eventlog))
}
