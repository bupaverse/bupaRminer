
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
