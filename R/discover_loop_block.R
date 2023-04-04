
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr as_tibble filter count group_by select mutate


discover_loop_block <- function(preproc_event_log,
                                loop_thres = 0.5){


  ## We are only interested in the activities that repeat
  ## An activity repeats if its original name occurs
  ## multiple times in the same case.
  preproc_event_log[LC == "start"][, .N, by = .(CID, orig_name, CASE_COUNT)] -> tmp

  tmp[,.(max_n = max(N)), by = orig_name][max_n > 1] -> repeat_act

  repeat_correlations <- preproc_event_log %>%
    as_tibble() %>%
    filter(LC == "start") %>%
    count(CID, orig_name) %>%
    group_by(orig_name) %>%
    filter(max(n) > 1)

  loop_blocks <- NULL
  ## If we have more than one activity that repeats within a case
  ## then we are interested to see if they form part of a loop
  if (repeat_act$orig_name %>% unique %>% length() > 1) {

    ## We take the activities that repeat within a case.
    ## And tabulate how often (n) each activity repeats per case
    ## This can be done using the pivot_wider.
    ## Then we ditch the case id and calculate correlations between
    ## activities that repeat.
    ## If there is a correlation between the number of times each
    ## activity repeats, then we assume that the activities
    ## occur together in a loop.

    base_cor <- dcast(tmp[orig_name %chin% repeat_act$orig_name], CID + CASE_COUNT ~ orig_name, value.var = "N", fill = 0)
    full_cor <- base_cor[rep(1:.N, CASE_COUNT)]


    repeat_correlations <- full_cor %>%
      select(-CID, -CASE_COUNT) %>%
      cor %>%
      as_tibble() %>%
      mutate(., antecedent = colnames(.)) %>%
      select(antecedent, everything())  %>%
      pivot_longer(
        cols = -c(antecedent),
        names_to = "consequent",
        values_to = "score"
      ) %>%
      filter(antecedent != consequent) %>%
      mutate(rel = RScoreDict$LOOP_BLOCK)


    ## Only correlations above a threshold are
    ## considered as part of a loop block
    ## Now we need to assess what activities belong
    ## together in a block
    ## We initiatie the loop_block_id at 0
    ## The loop_block_id will be used to separate
    ## blocks of activities that happen within a loop.
    ## Acts A, B and C may happen in a loop 1, whereas
    ## acts D and E happen in a different loop 2.
    loop_blocks <- repeat_correlations %>%
      filter(score > loop_thres) %>%
      mutate(loop_block_id = 0)

    ## As long as there are activities that have not been assigned
    ## to a loop block, we keep on looping.
    while (loop_blocks %>% filter(loop_block_id == 0) %>% nrow > 0) {
      cluster_counter <- loop_blocks %>% pull(loop_block_id) %>% max(na.rm=FALSE)
      cluster_counter <- cluster_counter + 1
      ## We start with an activity that has most correlations
      ## with others.We call this the most connected activity
      most_connected_antecedent <- loop_blocks %>%
        filter(loop_block_id == 0) %>%
        count(antecedent) %>%
        head(1) %>%
        pull(antecedent)

      ## For this most connecetd activity, we fetch all
      ## other activities that are correlated with it.
      ## These are the direct, first line, connections.
      direct_connections <- loop_blocks %>%
        filter(antecedent == most_connected_antecedent) %>%
        pull(consequent)

      ## Then we also fetch the extended, second line, connections
      ## by adding all activities that are connected to the actitivities
      ## that are correlated with the most connected actities.
      extended_connections <- loop_blocks %>%
        filter(antecedent %in% c(most_connected_antecedent, direct_connections)) %>%
        pull(consequent) %>%
        unique

      ## And we assign all these activities to the same loop block
      loop_blocks <- loop_blocks %>%
        mutate(loop_block_id = ifelse(
          (
            antecedent %in% extended_connections |
              consequent %in% extended_connections
          ),
          cluster_counter,
          loop_block_id
        ))
    }
  }

  return(loop_blocks)
}
