




#' Discover process
#'
#' @param log Event log
#' @param loop_strategy The argument loopstrategy indicates how bupaRminer will deal with blocks of activities (or subprocesses) that occur in a loop.
#'  - "none": bupaRminer will not attempt to detect any  looped subprocesses.
#'  - "skip": bupaRminer will detect looped subprocesses. However, the first occurrence of this subprocess just occurs in the regular process flow. The loop subprocess will only appear from the second occurrence onwards. This is used when a block of work is repeated only in certain circumstances.
#'  - "all": bupaRminer will detect looped subprocesses. The subprocess will occur in a loop from the very first occurrence. This setting is useful when the loop is always expected to occur instead of being an exception flow.
#'
#'
#' @export
discover <- function(log, loop_strategy) {
  UseMethod("discover")
}


#' @describeIn discover eventlog
#' @export


discover.eventlog <- function(log,
                              loop_strategy = "skip") {

  ## Loopstrategy "omit" : no loopbacks, "skip": Create loopbacks from second iteration, "all": create loopbacks from first iteration

  #TO DO: make sure start and complete are present for all activities
  cli::cli_alert_info("Preprocess log")
  log <- preprocess(log)
  snippet_dictionary <- list()
  if(loop_strategy %in% c("all","skip")){
    cli::cli_alert_info("Checking for loop blocks")
    loop_result <- construct_loop_blocks(log,
                                         mode = loop_strategy)
    snippet_dictionary <- loop_result$snippet_dictionary
    log <- loop_result$new_log
  }
  cli::cli_alert_info("Calculate relationships")
  log <- log %>% filter(is_repeat <= 2)
  relationships <- calculate_relationships(log)
  assigned_relationships <- assign_relationships(relationships)
  cli::cli_alert_info("Construct process")
  process <- construct_process(assigned_relationships,
                               snippet_dictionary)
  cli::cli_alert_info("Generate BPMN")
  generate_bpmn(process)

}
