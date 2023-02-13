

discover_parallels_one_to_many <- function(eventlog,
                                           one,
                                           many) {


  activity_colname <- activity_id(eventlog)
  case_colname <- case_id(eventlog)
  timestamp_colname <- timestamp(eventlog)
  lifecycle_colname <- lifecycle_id(eventlog)

  act_A <- one

  eventlog %>%
    filter(.data[[activity_colname]] == one, .data[[lifecycle_colname]] == "start") %>%
    select(.data[[case_colname]], .data[[timestamp_colname]], force_df = T) %>%
    rename(reference_timestamp_start = .data[[timestamp_colname]]) -> events_A

  eventlog %>%
    inner_join(
      events_A,
      by = case_colname) -> cases_with_A

  output <- list_along(many)

  for(B in 1:length(many)) {

    act_B <- as.character(many[B])

    cases_with_A %>%
      filter(.data[[activity_colname]] == act_B) %>%
      pull(.data[[case_colname]]) -> case_list_with_A_B

    cases_with_A %>%
      filter(.data[[case_colname]] %in% case_list_with_A_B) %>%
      re_map(mapping(eventlog)) -> cases_with_A_and_B

    if(cases_with_A_and_B %>% nrow() == 0){
      full_par_score <- 0
      par_score <- 0
    } else {

      A_starts_before_B_ends <- cases_with_A_and_B %>%
        filter(.data[[timestamp_colname]] >= reference_timestamp_start) %>%
        filter(.data[[activity_colname]] == act_B,
               .data[[lifecycle_colname]] == "complete") %>%
        pull(case_colname) %>%
        n_distinct()

      A_starts_after_B_starts <- cases_with_A_and_B %>%
        filter(.data[[timestamp_colname]] <= reference_timestamp_start) %>%
        filter(.data[[activity_colname]]== act_B,
               .data[[lifecycle_colname]] == "start") %>%
        pull(case_colname) %>%
        n_distinct()

      par_score <- 1 - ( abs(A_starts_before_B_ends - A_starts_after_B_starts) / cases_with_A_and_B %>%
                           pull(case_colname) %>%
                           n_distinct() )

      ## We lower the R score as A and B are less likely to occur together
      modifier <- ( cases_with_A_and_B %>% n_cases ) / ( cases_with_A %>% n_cases )
      full_par_score <- par_score * modifier
    }

    new_row <- tibble(
      "antecedent" = c(act_A, act_B, act_A, act_B),
      "consequent" = c(act_B, act_A, act_B, act_A),
      "rel" = c(RScoreDict$ALWAYS_PARALLEL, RScoreDict$ALWAYS_PARALLEL, RScoreDict$PARALLEL_IF_PRESENT, RScoreDict$PARALLEL_IF_PRESENT),
      "score" = c(full_par_score, full_par_score, par_score, par_score))

    output[[B]] <- new_row
  }

  output %>%
    bind_rows() %>%
    return()
}
