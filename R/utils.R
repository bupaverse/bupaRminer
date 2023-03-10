RScoreDict <- list(
  DIRECTLY_FOLLOWS = "R1",
  DIRECT_JOIN = "Rx",
  EVENTUALLY_FOLLOWS = "R2",
  MAYBE_DIRECTLY_FOLLOWS = "R3",
  MAYBE_EVENTUALLY_FOLLOWS = "R4",
  PARALLEL_IF_PRESENT = "R5",
  ALWAYS_PARALLEL = "R6",
  MUTUALLY_EXCLUSIVE = "R7",
  HAPPENS_DURING = "R8",
  TERMINATING = "R9",
  LOOP_BACK = "R10",
  LOOP_BLOCK = "LOOP",
  REQUIRES = "REQ"
)

# R_levels <- c(RScoreDict$DIRECT_JOIN,
#               RScoreDict$DIRECTLY_FOLLOWS,
#               RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
#               RScoreDict$ALWAYS_PARALLEL,
#               RScoreDict$TERMINATING,
#               RScoreDict$HAPPENS_DURING,
#               RScoreDict$PARALLEL_IF_PRESENT,
#               RScoreDict$EVENTUALLY_FOLLOWS,
#               RScoreDict$MUTUALLY_EXCLUSIVE,
#               RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
#               RScoreDict$REQUIRES)

R_levels <- c(RScoreDict$DIRECT_JOIN,
              RScoreDict$DIRECTLY_FOLLOWS,
              RScoreDict$TERMINATING,
              RScoreDict$HAPPENS_DURING,
              RScoreDict$ALWAYS_PARALLEL,
              RScoreDict$PARALLEL_IF_PRESENT,
              RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
              RScoreDict$EVENTUALLY_FOLLOWS,
              RScoreDict$MUTUALLY_EXCLUSIVE,
              RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
              RScoreDict$REQUIRES)

smart_thres_df <- tibble(
  rel = c(  RScoreDict$DIRECTLY_FOLLOWS,
            RScoreDict$EVENTUALLY_FOLLOWS,
            RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
            RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
            RScoreDict$PARALLEL_IF_PRESENT,
            RScoreDict$ALWAYS_PARALLEL,
            RScoreDict$TERMINATING,
            RScoreDict$HAPPENS_DURING,
            RScoreDict$MUTUALLY_EXCLUSIVE,
            RScoreDict$REQUIRES),
  rel_thres = c(0.90,
                0.80,
                0.20,
                0.10,
                0.80,
                0.9,
                0.9,
                0.9,
                0.9,
                0.7)
)

MERGE_R_levels <- c(RScoreDict$DIRECT_JOIN,
              RScoreDict$DIRECTLY_FOLLOWS,
              RScoreDict$PARALLEL_IF_PRESENT,
              RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
              RScoreDict$EVENTUALLY_FOLLOWS,
              RScoreDict$ALWAYS_PARALLEL,
              RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
              RScoreDict$REQUIRES,
              RScoreDict$MUTUALLY_EXCLUSIVE,
              RScoreDict$TERMINATING,
              RScoreDict$HAPPENS_DURING
)

pkg.env <- new.env()
pkg.env$end_event_counter <- 1

MERGE_INTERRUPTING_RELS <- c(RScoreDict$TERMINATING,
                       RScoreDict$HAPPENS_DURING)

MERGE_FOLLOWS_RELS <- c(RScoreDict$DIRECTLY_FOLLOWS,
                  RScoreDict$EVENTUALLY_FOLLOWS,
                  RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                  RScoreDict$MAYBE_EVENTUALLY_FOLLOWS)
MERGE_OTHER_RELS <- c(RScoreDict$REQUIRES,
                RScoreDict$DIRECT_JOIN)

I_WANT_INTERRUPTIONS <- FALSE

