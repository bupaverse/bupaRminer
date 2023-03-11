discover_R_sequence_relations <- function(
    ev_log,
    ev_activities,
    rel_par_df,
    parallel_thres = 0.95,
    exclusive_thres = 0.95,
    interrupting_theta = 0,
    GENERAL_THRES = 0.95
){

  nr_cases <- n_distinct(ev_log$CID)

  outer_output <- list_along(1:(length(ev_activities) - 1))


  references <- ev_log[, .(reference_timestamp_start = min(TS),
                           reference_timestamp_end = max(TS)), by = .(AID,CID)]

  for(A in c(1:(length(ev_activities)-1))){

    prec_act <-as.character(ev_activities[[A]])

    cli::cli_alert_info(prec_act)

    unique(as.data.table(rel_par_df)[rel %in% c(RScoreDict$PARALLEL_IF_PRESENT, RScoreDict$ALWAYS_PARALLEL) &
                                antecedent == prec_act &
                                score >= parallel_thres][["consequent"]]) -> par_relationships

    events_A <- references[AID == prec_act, -"AID"]

    cases_with_A <- ev_log %>%
      merge(
        events_A,
        by = "CID",
      )
    nr_cases_with_A <- n_distinct(cases_with_A[["CID"]])

    fromA_event_log <- cases_with_A[TS >= reference_timestamp_start & !(AID %chin% c(prec_act, par_relationships)),]
    afterA_event_log <- cases_with_A[TS >= reference_timestamp_end & !(AID %chin% c(prec_act, par_relationships)),]

    inner_output <- list_along((A+1):length(ev_activities))


    for(B in (A+1):length(ev_activities)){

      succ_act <- ev_activities[[B]]

      if(as.data.table(rel_par_df)[rel %in% c(RScoreDict$PARALLEL_IF_PRESENT, RScoreDict$ALWAYS_PARALLEL) &
                                   antecedent == prec_act &
                                   consequent == succ_act &
                                   score > GENERAL_THRES] %>% nrow() > 0){
        next
      }
      
      unique(as.data.table(rel_par_df)[rel %in% c(RScoreDict$PARALLEL_IF_PRESENT, RScoreDict$ALWAYS_PARALLEL) &
                                         antecedent == succ_act &
                                         score >= parallel_thres][["consequent"]]) -> par_B_relationships
      ## Does not work as expected, so set to empty vector for the moment
      par_B_relationships <- c()
      
      all_par_relationships <- unique(c(par_relationships, par_B_relationships))
      
      # cli::cli_alert_info(glue::glue("{prec_act} ~ {succ_act}"))

      events_B <- references[AID == succ_act, -"AID"]

      cases_with_B <-  ev_log %>%
        merge(
          events_B,
          by = "CID",
        )
      
      nr_cases_with_B <- n_distinct(cases_with_B[["CID"]])

      fromB_event_log <- cases_with_B[TS >= reference_timestamp_start & !(AID %chin% c(succ_act, all_par_relationships)),]
      afterB_event_log <- cases_with_B[TS >= reference_timestamp_end & !(AID %chin% c(succ_act, all_par_relationships)),]

      fromA_event_log_B <- fromA_event_log[!(AID %chin% par_B_relationships),]
      afterA_event_log_B <- afterA_event_log[!(AID %chin% par_B_relationships),]
      
      # cli::cli_alert_info("Requirement")
      ## REQ - The execution of A requires the execution of B as a predecessor
      REQ_results <- calculate_requirement_score(
        prec_act,
        succ_act,
        cases_with_A,
        cases_with_B,
        nr_cases_with_A,
        nr_cases_with_B,
        nr_cases,
        ev_log)

      ## Mutually exclusive


      if(any(REQ_results$score < GENERAL_THRES)){
        # cli::cli_alert_info("Exclusion")

        nr_cases_with_A_B <- n_distinct(cases_with_A[AID == succ_act][["CID"]])


        EXCL_results <- calculate_exclusive_relation(
          prec_act,
          succ_act,
          nr_cases_with_A,
          nr_cases_with_B,
          nr_cases_with_A_B,
          exclusive_thres,
          nr_cases,
          ev_log
        )

      } else {
        EXCL_results <- tribble(~antecedent,~consequent,~rel,~score,~importance, ~comment,
                                prec_act, succ_act, RScoreDict$MUTUALLY_EXCLUSIVE, 0, 0, "skip",
                                succ_act, prec_act, RScoreDict$MUTUALLY_EXCLUSIVE, 0, 0, "skip")

      }

      SOMETIMES_DIRECT <- 0
      SOMETIMES_FOL <- 0
      SOMETIMES_DIRECT_reverse <- 0
      SOMETIMES_FOL_reverse <- 0

      if(any(EXCL_results$score < GENERAL_THRES)){
        ## Directly and eventually follows

        if(prec_act != "END" & succ_act != "START"){
          # cli::cli_alert_info("Eventually follows")

          EF_results <- calculate_eventually_follows_relation(
            prec_act,
            succ_act,
            nr_cases_with_A,
            nr_cases_with_B,
            afterA_event_log_B,
            afterB_event_log,
            nr_cases,
            ev_log
          )

          if(any(EF_results$score >= 0.75*GENERAL_THRES)) {
            # cli::cli_alert_info("Directly follows")

            DF_results <- calculate_directly_follows_relation(
              prec_act,
              succ_act,
              nr_cases_with_A,
              nr_cases_with_B,
              afterA_event_log_B,
              afterB_event_log,
              nr_cases,
              ev_log
            )

          } else {
            DF_results <- tribble(~antecedent,~consequent,~rel,~score,~importance, ~comment,
                                  prec_act, succ_act, RScoreDict$DIRECTLY_FOLLOWS, 0, 0, "skip",
                                  succ_act, prec_act, RScoreDict$DIRECTLY_FOLLOWS, 0, 0, "skip")

          }
        } else {
          EF_results <- tribble(~antecedent,~consequent,~rel,~score,~importance, ~comment,
                                prec_act, succ_act, RScoreDict$EVENTUALLY_FOLLOWS, 0, 0, "skip",
                                succ_act, prec_act, RScoreDict$EVENTUALLY_FOLLOWS, 0, 0, "skip")
        }



        ## Sometimes directly or eventually happens

        par_relationships_B <- as.data.table(rel_par_df)[
          rel %in% c(RScoreDict$PARALLEL_IF_PRESENT, RScoreDict$ALWAYS_PARALLEL) &
                 antecedent == succ_act &
                 score >= parallel_thres][["consequent"]]

        if(any(DF_results$score < GENERAL_THRES) | any(EF_results$score < GENERAL_THRES)){
          # cli::cli_alert_info("Sometimes follows")

          cases_before_B <- cases_with_B[TS <= reference_timestamp_start]
          cases_before_A <- cases_with_A[TS <= reference_timestamp_start]

          MEF_results <- calculate_sometime_follows_relation(
            prec_act,
            succ_act,
            nr_cases_with_A,
            nr_cases_with_B,
            fromA_event_log_B,
            fromB_event_log,
            cases_before_B,
            cases_before_A,
            nr_cases,
            ev_log,
            EF_results)


          if(any(MEF_results$score >= 0.75*GENERAL_THRES)){
            # cli::cli_alert_info("Sometimes directly follows")

            MDF_results <- calculate_sometimes_directly_follows_relation(
              prec_act,
              succ_act,
              nr_cases_with_A,
              nr_cases_with_B,
              afterA_event_log_B,
              afterB_event_log,
              cases_before_B,
              cases_before_A,
              par_relationships_B,
              par_relationships,
              nr_cases,
              ev_log,
              DF_results)
          } else {
            MDF_results <- tribble(~antecedent,~consequent,~rel,~score,~importance, ~comment,
                                   prec_act, succ_act, RScoreDict$MAYBE_DIRECTLY_FOLLOWS , 0, 0, "skip",
                                   succ_act, prec_act, RScoreDict$MAYBE_DIRECTLY_FOLLOWS, 0, 0, "skip")
          }

        } else {
          MEF_results <- tribble(~antecedent,~consequent,~rel,~score,~importance,~comment,
                                 prec_act, succ_act, RScoreDict$MAYBE_EVENTUALLY_FOLLOWS , 0, 0, "skip",
                                 succ_act, prec_act, RScoreDict$MAYBE_EVENTUALLY_FOLLOWS, 0, 0, "skip")
        }
      } else {
        EF_results <- tribble(~antecedent,~consequent,~rel,~score,~importance,~comment,
                              prec_act, succ_act, RScoreDict$EVENTUALLY_FOLLOWS, 0, 0, "skip",
                              succ_act, prec_act, RScoreDict$EVENTUALLY_FOLLOWS, 0, 0, "skip")
        DF_results <- tribble(~antecedent,~consequent,~rel,~score,~importance,~comment,
                             prec_act, succ_act, RScoreDict$DIRECTLY_FOLLOWS, 0, 0, "skip",
                             succ_act, prec_act, RScoreDict$DIRECTLY_FOLLOWS, 0, 0, "skip")
        MEF_results <- tribble(~antecedent,~consequent,~rel,~score,~importance,~comment,
                               prec_act, succ_act, RScoreDict$MAYBE_EVENTUALLY_FOLLOWS , 0, 0, "skip",
                               succ_act, prec_act, RScoreDict$MAYBE_EVENTUALLY_FOLLOWS, 0, 0, "skip")
        MDF_results <- tribble(~antecedent,~consequent,~rel,~score,~importance,~comment,
                               prec_act, succ_act, RScoreDict$MAYBE_DIRECTLY_FOLLOWS , 0, 0, "skip",
                               succ_act, prec_act, RScoreDict$MAYBE_DIRECTLY_FOLLOWS, 0, 0, "skip")
      }
      ## B interrupts A

      INTERRUPTING_score <- 0
      INTERRUPTING_score_reverse <- 0
      DURING_score <- 0
      DURING_score_reverse <- 0

      if(!(prec_act %in% c("START","END")) & !(succ_act %in% c("START","END"))){
        # cli::cli_alert_info("Terminating")

        INTERRUPTING_score <- calculate_terminating_relationship(
          prec_act,
          succ_act,
          cases_with_A,
          cases_with_B,
          nr_cases_with_B,
          interrupting_theta,
          ev_log
        )

        INTERRUPTING_score_reverse <- calculate_terminating_relationship(
          succ_act,
          prec_act,
          cases_with_B,
          cases_with_A,
          nr_cases_with_A,
          interrupting_theta,
          ev_log
        )

        ## B starts during A
        # cli::cli_alert_info("Intermittent")

        DURING_score <- calculate_intermittent_relationship(
          prec_act,
          succ_act,
          cases_with_A,
          cases_with_B,
          nr_cases_with_B,
          ev_log
        )
        DURING_score_reverse <- calculate_intermittent_relationship(
          succ_act,
          prec_act,
          cases_with_B,
          cases_with_A,
          nr_cases_with_A,
          ev_log
        )
      }




      new_row_AB <- tibble(
        "antecedent" = prec_act,
        "consequent" = succ_act,
        "rel" = c(RScoreDict$TERMINATING,
                  RScoreDict$HAPPENS_DURING),
        "score" = c(INTERRUPTING_score,
                    DURING_score),
        "importance" = c(DF_results$importance[1],
                         DF_results$importance[1]))


      new_row_BA <- tibble(
        "antecedent" = succ_act,
        "consequent" = prec_act,
        "rel" = c(RScoreDict$TERMINATING,
                  RScoreDict$HAPPENS_DURING),
        "score" = c(INTERRUPTING_score_reverse,
                    DURING_score_reverse),
        "importance" = c(DF_results$importance[2],
                         DF_results$importance[2]))



      inner_output[[B-1]] <- bind_rows(new_row_AB, new_row_BA, REQ_results, EXCL_results, EF_results, DF_results, MEF_results, MDF_results) %>%
        mutate(type = rep(c("normal","reverse"), length.out = n()))

    }
    outer_output[[A]] <- inner_output
  }

  rel_df <- bind_rows(outer_output)

  return(rel_df)
}
