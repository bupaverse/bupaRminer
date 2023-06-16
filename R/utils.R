#' @importFrom dplyr tibble

RScoreDict <- list(
  DIRECTLY_FOLLOWS = "DF",
  DIRECT_JOIN = "DJ",
  EVENTUALLY_FOLLOWS = "EF",
  MAYBE_DIRECTLY_FOLLOWS = "SDF",
  MAYBE_EVENTUALLY_FOLLOWS = "SEF",
  PARALLEL_IF_PRESENT = "PIP",
  ALWAYS_PARALLEL = "PAR",
  MUTUALLY_EXCLUSIVE = "EXC",
  HAPPENS_DURING = "DUR",
  TERMINATING = "TER",
  LOOP_BACK = "LBA",
  LOOP_BLOCK = "LBL",
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

# R_levels <- c(RScoreDict$DIRECT_JOIN,
#               RScoreDict$DIRECTLY_FOLLOWS,
#               RScoreDict$TERMINATING,
#               RScoreDict$HAPPENS_DURING,
#               RScoreDict$ALWAYS_PARALLEL,
#               RScoreDict$PARALLEL_IF_PRESENT,
#               RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
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
              RScoreDict$EVENTUALLY_FOLLOWS,
              RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
              RScoreDict$MUTUALLY_EXCLUSIVE,
              RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
              RScoreDict$REQUIRES)

smart_thres_df <- dplyr::tibble(
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

# MERGE_R_levels <- c(RScoreDict$DIRECT_JOIN,
#               RScoreDict$DIRECTLY_FOLLOWS,
#               RScoreDict$PARALLEL_IF_PRESENT,
#               RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
#               RScoreDict$EVENTUALLY_FOLLOWS,
#               RScoreDict$ALWAYS_PARALLEL,
#               RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
#               RScoreDict$REQUIRES,
#               RScoreDict$MUTUALLY_EXCLUSIVE,
#               RScoreDict$TERMINATING,
#               RScoreDict$HAPPENS_DURING
# )
MERGE_R_levels <- c(RScoreDict$PARALLEL_IF_PRESENT,
                    RScoreDict$ALWAYS_PARALLEL,
                    RScoreDict$DIRECT_JOIN,
                    RScoreDict$DIRECTLY_FOLLOWS,
                    RScoreDict$EVENTUALLY_FOLLOWS,
                    RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
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
                  RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
                  RScoreDict$DIRECT_JOIN)
MERGE_OTHER_RELS <- c(RScoreDict$REQUIRES,
                RScoreDict$DIRECT_JOIN)

I_WANT_INTERRUPTIONS <- FALSE

