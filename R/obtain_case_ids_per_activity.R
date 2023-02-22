
obtain_case_ids_per_activity <- function(ev_log){

  activity_colname <- activity_id(ev_log)
  case_colname <- case_id(ev_log)

  ev_log %>% select(activity_colname, case_colname, force_df = T) %>%
    distinct() %>%
    group_by(!!sym(activity_colname)) %>%
    summarize(data = list(!!sym(case_colname))) -> tttmp

  names(tttmp$data) <- tttmp$activity

  tttmp$data
}


