discover_R_sequence_relations <- function(
    ev_log,
    ev_activities,
    rel_par_df,
    parallel_thres = 0.95,
    exclusive_thres = 0.95,
    interrupting_theta = 0,
    cases_per_act_memory = NULL,
    GENERAL_THRES = 0.95
){

  new_mapping <- list(case_identifier = "CID",
                      activity_identifier = "AID",
                      activity_instance_identifier = "AIID",
                      timestamp_identifier = "TS",
                      lifecycle_identifier = "LC",
                      resource_identifier = resource_id(ev_log))
  class(new_mapping) <- c("eventlog_mapping", class(new_mapping))

  ev_log %>%
    rename(AID = .data[[activity_id(ev_log)]],
           AIID = .data[[activity_instance_id(ev_log)]],
           CID = .data[[case_id(ev_log)]],
           TS = .data[[timestamp(ev_log)]],
           LC =.data[[lifecycle_id(ev_log)]]) %>%
    re_map(new_mapping) -> ev_log

  activity_colname <- activity_id(ev_log)
  activity_instance_colname <- activity_instance_id(ev_log)
  case_colname <- case_id(ev_log)
  timestamp_colname <- timestamp(ev_log)
  lifecycle_colname <- lifecycle_id(ev_log)

  nr_cases <- n_cases(ev_log)

  outer_output <- list_along(1:(length(ev_activities) - 1))

  for(A in c(1:(length(ev_activities)-1))){

    prec_act <-as.character(ev_activities[[A]])
    print(prec_act)

    par_relationships <- rel_par_df %>%
      filter(rel %in% c(RScoreDict$PARALLEL_IF_PRESENT, RScoreDict$ALWAYS_PARALLEL),
             antecedent == prec_act,
             score >= parallel_thres) %>%
      pull(consequent) %>%
      unique()



    events_A <- dcast(as.data.table(ev_log)[AID == prec_act, .(CID, LC, TS)], CID ~ LC, value.var = "TS")[, .(CID, reference_timestamp_start = start, reference_timestamp_end = complete)]

    cases_with_A <-  as.data.table(ev_log) %>%
      merge(
        events_A,
        by = case_colname,
      ) %>%
      re_map(mapping(ev_log))


    fromA_event_log <- cases_with_A %>%
      filter(!!sym(timestamp_colname) >= reference_timestamp_start) %>%
      filter(!(!!sym(activity_colname) %in% par_relationships))

    afterA_event_log <- cases_with_A %>%
      filter(!!sym(timestamp_colname) >= reference_timestamp_end) %>%
      filter(!(!!sym(activity_colname) %in% par_relationships))

    inner_output <- list_along((A+1):length(ev_activities))

    for(B in (A+1):length(ev_activities)){

      succ_act <- ev_activities[[B]]

      if(rel_par_df %>%
         filter(rel %in% c(RScoreDict$PARALLEL_IF_PRESENT, RScoreDict$ALWAYS_PARALLEL),
                antecedent == prec_act,
                consequent == succ_act,
                score > GENERAL_THRES) %>% nrow > 0){
        next
      }


      events_B <- dcast(as.data.table(ev_log)[AID == succ_act, .(CID, LC, TS)], CID ~ LC, value.var = "TS")[, .(CID, reference_timestamp_start = start, reference_timestamp_end = complete)]

      cases_with_B <-  as.data.table(ev_log) %>%
        merge(
          events_B,
          by = case_colname,
        ) %>%
        re_map(mapping(ev_log))



      ## REQ - The execution of A requires the execution of B as a predecessor
      REQ_score <- calculate_requirement_score(
        prec_act,
        succ_act,
        cases_with_A,
        nr_cases,
        ev_log)

      REQ_importance <- REQ_score$importance
      REQ_score <- REQ_score$score

      REQ_score_reverse <- calculate_requirement_score(
        succ_act,
        prec_act,
        cases_with_B,
        nr_cases,
        ev_log)

      REQ_importance_reverse <- REQ_score_reverse$importance
      REQ_score_reverse <- REQ_score_reverse$score

      ## Mutually exclusive
      EXCL_importance <- 0
      EXCL_score <- 0
      EXCL_importance_reverse <- 0
      EXCL_score_reverse <- 0


      if(REQ_score < GENERAL_THRES){

        EXCL_score <- calculate_exclusive_relation(
          prec_act,
          succ_act,
          cases_with_A,
          cases_with_B,
          exclusive_thres,
          nr_cases,
          ev_log
        )

        EXCL_importance <- EXCL_score$importance
        EXCL_score <- EXCL_score$score
      }

      if(REQ_score_reverse < GENERAL_THRES & succ_act != "END"){

        EXCL_score_reverse <- calculate_exclusive_relation(
          succ_act,
          prec_act,
          cases_with_B,
          cases_with_A,
          exclusive_thres,
          nr_cases,
          ev_log
        )

        EXCL_importance_reverse <- EXCL_score_reverse$importance
        EXCL_score_reverse <- EXCL_score_reverse$score
      }

      EVENTUALLY_importance <- 0
      EVENTUALLY_score <- 0
      EVENTUALLY_importance_reverse <- 0
      EVENTUALLY_score_reverse <-

        DIRECT_FOL_importance <- 0
      DIRECT_FOL_score <- 0
      DIRECT_FOL_importance_reverse <- 0
      DIRECT_FOL_score_reverse <- 0

      SOMETIMES_DIRECT <- 0
      SOMETIMES_FOL <- 0
      SOMETIMES_DIRECT_reverse <- 0
      SOMETIMES_FOL_reverse <- 0

      if(EXCL_score < GENERAL_THRES & EXCL_score_reverse < GENERAL_THRES){

        fromB_event_log <- cases_with_B %>%
          filter(!!sym(timestamp_colname) >= reference_timestamp_start) %>%
          filter(!(!!sym(activity_colname) %in% par_relationships))

        afterB_event_log <- cases_with_B %>%
          filter(!!sym(timestamp_colname) >= reference_timestamp_end) %>%
          filter(!(!!sym(activity_colname) %in% par_relationships))

        ## Directly and eventually follows

        if(prec_act != "END" & succ_act != "START"){

          EVENTUALLY_score <- calculate_eventually_follows_relation(
            prec_act,
            succ_act,
            cases_with_A,
            afterA_event_log,
            nr_cases,
            ev_log
          )

          EVENTUALLY_importance <- EVENTUALLY_score$importance
          EVENTUALLY_score <- EVENTUALLY_score$score

          if(EVENTUALLY_score >= 0.75*GENERAL_THRES){

            DIRECT_FOL_score <- calculate_directly_follows_relation(
              prec_act,
              succ_act,
              cases_with_A,
              afterA_event_log,
              nr_cases,
              ev_log
            )

            DIRECT_FOL_importance <- DIRECT_FOL_score$importance
            DIRECT_FOL_score <- DIRECT_FOL_score$score
          }
        }

        if(succ_act != "END" & prec_act != "START"){

          EVENTUALLY_score_reverse <- calculate_eventually_follows_relation(
            succ_act,
            prec_act,
            cases_with_B,
            afterB_event_log,
            nr_cases,
            ev_log
          )
          EVENTUALLY_importance_reverse <- EVENTUALLY_score_reverse$importance
          EVENTUALLY_score_reverse <- EVENTUALLY_score_reverse$score


          if(EVENTUALLY_score_reverse >= 0.75*GENERAL_THRES){

            DIRECT_FOL_score_reverse <- calculate_directly_follows_relation(
              succ_act,
              prec_act,
              cases_with_B,
              afterB_event_log,
              nr_cases,
              ev_log
            )

            DIRECT_FOL_importance_reverse <- DIRECT_FOL_score_reverse$importance
            DIRECT_FOL_score_reverse <- DIRECT_FOL_score_reverse$score
          }
        }

        ## Sometimes directly or eventually happens

        par_relationships_B <- rel_par_df %>%
          filter(rel %in% c(RScoreDict$PARALLEL_IF_PRESENT, RScoreDict$ALWAYS_PARALLEL),
                 antecedent == succ_act,
                 score >= parallel_thres) %>%
          pull(consequent)

        if(DIRECT_FOL_score < GENERAL_THRES | EVENTUALLY_score < GENERAL_THRES){

          cases_before_B <- cases_with_B %>%
            filter(!!sym(timestamp_colname) <= reference_timestamp_start)

          SOMETIMES_FOL <- calculate_sometime_follows_relation(
            prec_act,
            succ_act,
            cases_with_A,
            cases_with_B,
            fromA_event_log,
            cases_before_B,
            nr_cases,
            ev_log)


          if(SOMETIMES_FOL >= 0.75*GENERAL_THRES){

            SOMETIMES_DIRECT <- calculate_sometimes_directly_follows_relation(
              prec_act,
              succ_act,
              cases_with_A,
              cases_with_B,
              afterA_event_log,
              cases_before_B,
              par_relationships_B,
              nr_cases,
              ev_log)

          }

        }

        if(prec_act!= "START" & succ_act != "END" & DIRECT_FOL_score_reverse < GENERAL_THRES & EVENTUALLY_score_reverse < GENERAL_THRES){

          cases_before_A <- cases_with_A %>%
            filter(!!sym(timestamp_colname) <= reference_timestamp_start)

          SOMETIMES_FOL_reverse <- calculate_sometime_follows_relation(
            succ_act,
            prec_act,
            cases_with_B,
            cases_with_A,
            fromB_event_log,
            cases_before_A,
            nr_cases,
            ev_log)


          if(SOMETIMES_FOL_reverse >= 0.75*GENERAL_THRES){

            SOMETIMES_DIRECT_reverse <- calculate_sometimes_directly_follows_relation(
              succ_act,
              prec_act,
              cases_with_B,
              cases_with_A,
              afterB_event_log,
              cases_before_A,
              par_relationships,
              nr_cases,
              ev_log)
          }
        }
      }

      ## B interrupts A

      INTERRUPTING_score <- 0
      INTERRUPTING_score_reverse <- 0
      DURING_score <- 0
      DURING_score_reverse <- 0

      if(!(prec_act %in% c("START","END")) & !(succ_act %in% c("START","END"))){
        INTERRUPTING_score <- calculate_terminating_relationship(
          prec_act,
          succ_act,
          cases_with_A,
          cases_with_B,
          interrupting_theta,
          ev_log
        )

        INTERRUPTING_score_reverse <- calculate_terminating_relationship(
          succ_act,
          prec_act,
          cases_with_B,
          cases_with_A,
          interrupting_theta,
          ev_log
        )

        ## B starts during A
        DURING_score <- calculate_intermittent_relationship(
          prec_act,
          succ_act,
          cases_with_A,
          cases_with_B,
          ev_log
        )
        DURING_score_reverse <- calculate_intermittent_relationship(
          succ_act,
          prec_act,
          cases_with_B,
          cases_with_A,
          ev_log
        )
      }

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


      inner_output[[B-1]] <- bind_rows(new_row_AB, new_row_BA)
    }
    outer_output[[A]] <- inner_output
  }

  rel_df <- bind_rows(outer_output)

  return(rel_df)
}
