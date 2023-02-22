




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
  cli::cli_alert_info("Preprocess log")
  log <- preprocess(log)
  cli::cli_alert_info("Calculate relationships")
  relationships <- calculate_relationships(log)
  cli::cli_alert_info("Construct process")
  process <- construct_process(relationships)
  cli::cli_alert_info("Generate BPMN")
  generate_bpmn(process)

}
