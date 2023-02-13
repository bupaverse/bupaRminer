calculate_sometimes_directly_follows_relation <- function(
    act_A,
    act_B,
    cases_with_A,
    cases_with_B,
    afterA_event_log,
    cases_before_B,
    par_relationships_B,
    nr_cases,
    ev_log){


  activity_colname <- activity_id(ev_log)
  activity_instance_colname <- activity_instance_id(ev_log)
  case_colname <- case_id(ev_log)
  timestamp_colname <- timestamp(ev_log)
  lifecycle_colname <- lifecycle_id(ev_log)

  B_happens_directly_after <- cases_before_B %>%
    as_tibble() %>%
    filter(!!sym(lifecycle_colname) == "complete") %>%
    filter(!(!!sym(activity_colname) %in% par_relationships_B)) %>%
    arrange(!!sym(timestamp_colname)) %>%
    group_by(!!sym(case_colname)) %>%
    filter(row_number() == max(row_number())) %>%
    ungroup() %>%
    filter(!!sym(activity_colname) == act_A)

  ## If B happens, how often does it happen right after A
  SOMETIMES_DIRECT_1 <- (B_happens_directly_after %>%
                           pull(!!sym(case_colname)) %>%
                           n_distinct) /
    (cases_with_B %>%
       pull(!!sym(case_colname)) %>%
       n_distinct)

  ## If A happens, how often is it directly followed
  ## by B
  SOMETIMES_DIRECT_2 <- (afterA_event_log %>%
                           as_tibble() %>%
                           filter(!!sym(lifecycle_colname) == "start") %>%
                           arrange(!!sym(timestamp_colname)) %>%
                           group_by(!!sym(case_colname)) %>%
                           filter(row_number() == 1) %>%
                           ungroup() %>%
                           filter(!!sym(activity_colname) == act_B) %>%
                           pull(!!sym(case_colname)) %>%
                           n_distinct) /
    (cases_with_A %>%
       pull(!!sym(case_colname)) %>%
       n_distinct)

  ## How often do we expect B
  SOMETIMES_DIRECT_3 <- (cases_with_B %>%
                           pull(!!sym(case_colname)) %>%
                           n_distinct) / nr_cases

  ## The first factor is high when A is often the activity before B
  ## the second factor is high when the number of times we observe B right after A
  ## is similar to the number of times we observe B at all.
  SOMETIMES_DIRECT <- SOMETIMES_DIRECT_1 * (1 - abs(SOMETIMES_DIRECT_2 - SOMETIMES_DIRECT_3))

  return(SOMETIMES_DIRECT)
}
