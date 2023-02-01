




#' Discover process
#'
#' @param log Event log
#'
#'
#' @export
discover_process <- function(log) {
  UseMethod("discover_process")
}


#' @describeIn discover_process eventlog
#' @export


discover_process.eventlog <- function(log) {

  #TO DO: make sure start and complete are present for all activities
  message("--- Preprocess log ---")
  log <- preprocess(log)
  message("--- Calculate relationships ---")
  relationships <- calculate_relationships(log)
  message("--- Construct process ---")
  process <- construct_process(relationships)
  message("--- Generate BPMN ---")
  generate_bpmn(process)

}
