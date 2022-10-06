library(processmapR)
library(bupaR)
library(tidyverse)
library(rlang)

source("sandbox/calculation_functions.R")

rel_df <- calculate_relationship_scores(event_log)

R_levels <- c(RScoreDict$DIRECT_JOIN, 
              RScoreDict$DIRECTLY_FOLLOWS, 
              RScoreDict$MAYBE_DIRECTLY_FOLLOWS, 
              RScoreDict$ALWAYS_PARALLEL,
              RScoreDict$TERMINATING,
              RScoreDict$HAPPENS_DURING,
              RScoreDict$PARALLEL_IF_PRESENT,
              RScoreDict$EVENTUALLY_FOLLOWS,
              RScoreDict$MAYBE_EVENTUALLY_FOLLOWS,
              RScoreDict$MUTUALLY_EXCLUSIVE,
              RScoreDict$REQUIRES,
              "R6-")

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

## Set all rel values above threshold to 1 
masked_df <- rel_df %>%
  left_join(smart_thres_df) %>%
  mutate(score = as.numeric(score >= rel_thres),
         rel = factor(rel, levels = R_levels, ordered = TRUE)) %>%
  select(-rel_thres)

## Filter out dominant relationship pased on R_levels
assigned_rel_df <- masked_df %>%
  filter(score == 1) %>%
  group_by(antecedent, consequent) %>%
  arrange(rel) %>%
  filter(row_number() == 1) %>%
  ungroup()