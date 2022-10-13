library(processmapR)
library(bupaR)
library(tidyverse)
library(rlang)

# event_log <- readRDS(file = "data/Road_Traffic_Fine_Management_Process.rds")
event_log <- patients

activity_colname <- activity_id(event_log)
activity_instance_colname <- activity_instance_id(event_log)
case_colname <- case_id(event_log)
timestamp_colname <- timestamp(event_log)
lifecycle_colname <- lifecycle_id(event_log)

MAGIC_NUMBER <- 10000

if(event_log %>% n_cases > MAGIC_NUMBER){
  sampled_cases <- event_log %>% pull(!!sym(case_colname)) %>% unique %>% sample(MAGIC_NUMBER)
  
  event_log <- event_log %>% 
    filter(!!sym(case_colname) %in% sampled_cases)
}

if(all(event_log %>% pull(!!sym(lifecycle_colname)) %>% unique == "complete")){
  event_log <- event_log %>%
    bind_rows((event_log %>% mutate(!!sym(lifecycle_colname) := "start")))
}

if(all(event_log %>% pull(!!sym(lifecycle_colname)) %>% unique == "start")){
  event_log <- event_log %>%
    bind_rows((event_log %>% mutate(!!sym(lifecycle_colname) := "complete")))
}

## Add START and END events
start_events <- tibble(
  !!sym(case_colname) := event_log %>% pull(!!sym(case_colname)) %>% unique,
  !!sym(timestamp_colname) := event_log %>% pull(!!sym(timestamp_colname)) %>% min() - 10000,
  !!sym(activity_instance_colname) := paste(event_log %>% pull(!!sym(case_colname)) %>% unique,"START", sep="_"),
  !!sym(activity_colname) := "START"
)

end_events <- tibble(
  !!sym(case_colname) := event_log %>% pull(!!sym(case_colname)) %>% unique,
  !!sym(timestamp_colname) := event_log %>% pull(!!sym(timestamp_colname)) %>% max() + 10000,
  !!sym(activity_instance_colname) := paste(event_log %>% pull(!!sym(case_colname)) %>% unique,"END", sep="_"),
  !!sym(activity_colname) := "END"
)

event_log <- event_log %>%
  bind_rows(
    start_events %>% mutate(!!sym(lifecycle_colname) := "start"),
    start_events %>%mutate(!!sym(lifecycle_colname) := "complete"),
    end_events %>% mutate(!!sym(lifecycle_colname) := "start"),
    end_events %>%mutate(!!sym(lifecycle_colname) := "complete")
  )

completed_only <- event_log %>%
  filter(!!sym(lifecycle_colname) == "complete") %>%
  re_map(mapping(event_log))

## Calculate time elapsed since first event in case
## Assign ordinal timestemp per event in case
completed_only <- completed_only %>%
  as_tibble() %>%
  group_by(!!sym(case_colname)) %>%
  mutate(first_timestamp_ = min(!!sym(timestamp_colname), na.rm=TRUE)) %>%
  arrange(!!sym(timestamp_colname)) %>%
  mutate(timestep = row_number()) %>%
  ungroup() %>%
  mutate(
    relative_timestamp = 
      !!sym(timestamp_colname) - first_timestamp_) %>%
  re_map(mapping(event_log))

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
                                      !!sym(activity_colname))) %>%
  re_map(mapping(completed_only))

activity_occurrences <- completed_only %>%
  activities

all_activities <- activity_occurrences %>%
  pull(!!sym(activity_colname))

event_log <- event_log %>%
  left_join(completed_only %>%
              as_tibble() %>%
              select(!!sym(case_colname), 
                     !!sym(activity_instance_colname),
                     new_act_name = !!sym(activity_colname),
                     is_repeat),
            by = c(case_colname, activity_instance_colname)) %>%
  mutate(orig_name = !!sym(activity_colname),
         !!sym(activity_colname) := new_act_name)