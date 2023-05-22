#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import bupaR
#' @importFrom stats cor
#' @importFrom purrr list_along map_chr
#' @importFrom utils head
#' @import bpmnR
#' @importFrom tidyr spread pivot_wider gather unnest
#' @importFrom edeaR filter_activity_presence
#' @importFrom stringr str_replace_all str_detect
#' @import data.table
#' @importFrom forcats fct_recode
#' @importFrom lubridate now
#' @importFrom petrinetR create_marked_PN create_PN
#' @importFrom dplyr tibble bind_rows anti_join case_when distinct full_join everything left_join pull summarize tribble n_distinct n lag inner_join if_else row_number rename_with transmute
## usethis namespace: end
NULL
globalVariables(c(".","AID","AIID","CASE_COUNT",".data","CID","LC","EXCL_count",
                  "N","N_AIID","PAR_count","TS","absolute_frequency",
                  "act_lc", "acts_in_between", "all_same", "antecedent", "block_content", "block_id",
                  "chain_nr", "complete", "consequent",  "count_of_this_rel", "current_focus",
                  "early_ts", "first_act_occ_step", "freq", "gatewayDirection",
                  "gatewayDirection_target", "gatewayType", "gatewayType_target", "has_EXCL",
                  "has_PAR", "has_conflict", "has_follows", "has_par", "id", "importance",
                  "importance.x",  "inspection_sequence", "inspection_type",
                  "is_not_act_A", "is_repeat", "is_repeat_cnt", "lag_acts_in_between", "late_ts",
                  "loop_block_id", "max_n", "min_n", "must_remove", "n_outgoing", "new_act_name",
                  "new_concatenated_name", "non_par_relations", "nr_connections", "orig_name",
                  "part_of_chain", "prevailing_rel", "reference_score", "reference_timestamp_end",
                  "reference_timestamp_first_start", "reference_timestamp_last_end",
                  "reference_timestamp_start", "rel", "rel.x", "rel.y", "rel_thres", "rep_occurence",
                  "rounded_score", "row_number", "score", "score.x", "selected_row", "sourceRef", "start",
                  "start_of_chain", "sym", "targetRef", "timestep", "total_relations"))
