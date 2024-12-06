




#' Discover process
#'
#' @param log Event log
#'  
#'
#'
#' @export
discover <- function(log) {
  UseMethod("discover")
}


#' @describeIn discover eventlog
#' @export


discover.eventlog <- function(log) {
  
  #TO DO: make sure start and complete are present for all activities
  cli::cli_progress_step("Preprocess log", spinner = TRUE)
  log <- preprocess(log)
  prep_log <- log
  snippet_dictionary <- list()
  cli::cli_progress_step("Checking for loop blocks ", spinner = TRUE)
  if(log %>% pull(is_repeat) %>% max() > 1 ){
    loop_rels <- calculate_loop_relations(log)
    if(loop_rels %>% nrow > 0){
      loop_scores <- calculate_loop_scores(loop_rels, log)
      if(loop_scores %>%
         filter(rel == RScoreDict$LOOP_BACK) %>%
         filter(score > 0) %>% nrow > 0){
        loop_block_df <- detect_loop_blocks(loop_scores, loop_rels)
        loop_result <- solve_loop_blocks(loop_block_df,log)
        snippet_dictionary <- loop_result$process
        log <- loop_result$log
      }
    }
  }
  # i <- 0
  # n <- 100
  # cli::cli_progress_step("Calculating relationships {i}/{n}", spinner = TRUE)
  cli::cli_progress_step("Discover main process", spinner = TRUE)
  log <- log %>% filter(is_repeat <= 2)
  relationships <- calculate_relationships(log, source = "main")
  assigned_relationships <- assign_relationships(relationships)
  # cli::cli_progress_step("Constructing process", spinner = TRUE)
  process <- construct_process(assigned_relationships,
                               construction_context = list(
                                 snippet_dictionary = snippet_dictionary,
                                 trace_log = prep_log
                               ), 
                               source = "main")
  cli::cli_progress_step("Generating BPMN", spinner = TRUE)
  generate_bpmn(process) -> model
  cli::cli_progress_done(result = "done")
  model

}
