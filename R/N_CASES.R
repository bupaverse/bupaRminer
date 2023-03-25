

N_CASES <- function(CIDS, case_count_list) {

  sum(case_count_list[CID %in% unique(CIDS)][["CASE_COUNT"]])
}
