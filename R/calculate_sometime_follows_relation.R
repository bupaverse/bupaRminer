calculate_sometime_follows_relation <- function(
    act_A,
    act_B,
    cases_with_A,
    cases_with_B,
    fromA_event_log,
    cases_before_B,
    nr_cases,
    ev_log){

  activity_colname <- activity_id(ev_log)
  activity_instance_colname <- activity_instance_id(ev_log)
  case_colname <- case_id(ev_log)
  timestamp_colname <- timestamp(ev_log)
  lifecycle_colname <- lifecycle_id(ev_log)

  SOMETIMES_FOL_1 <- 0
  B_happens_after <- cases_before_B %>%
    filter_activity_presence(c(act_A, "JIBBERFOETEL"), method = "one_of")

  if(B_happens_after %>% nrow > 0){

    SOMETIMES_FOL_1 <- (B_happens_after %>%
                          pull(!!sym(case_colname)) %>%
                          n_distinct) /
      (cases_with_B %>%
         pull(!!sym(case_colname)) %>%
         n_distinct)
  }

  SOMETIMES_FOL_2 <- 0
  A_happens_before <- fromA_event_log %>%
    filter_activity_presence(c(act_B, "JIBBERFOETEL"), method = "one_of")
  if(A_happens_before %>% nrow > 0){

    SOMETIMES_FOL_2 <- (A_happens_before %>%
                          pull(!!sym(case_colname)) %>%
                          n_distinct) /
      (cases_with_A %>%
         pull(!!sym(case_colname)) %>%
         n_distinct)
  }

  SOMETIMES_FOL_3 <- (cases_with_B %>%
                        pull(!!sym(case_colname)) %>%
                        n_distinct) / nr_cases

  SOMETIMES_FOL <- SOMETIMES_FOL_1 * (1 - abs(SOMETIMES_FOL_2 - SOMETIMES_FOL_3))

  return(SOMETIMES_FOL)
}
