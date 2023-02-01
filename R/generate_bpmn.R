


generate_bpmn <- function(snippet_dictionary) {

  bpmn_obj <- snippet_dictionary[[length(snippet_dictionary)]]

  map_to_bpmn <- function(bpmn_obj){
    tasks <- bpmn_obj$tasks %>% as.data.frame()
    seqs <- bpmn_obj$seqs %>% as.data.frame() %>% unique
    gateways <- bpmn_obj$gateways %>% as.data.frame() %>% unique
    start_events <- bpmn_obj$start_events %>% as.data.frame() %>% unique
    end_events <- bpmn_obj$end_events %>% as.data.frame() %>% unique

    bpmn_out <- create_bpmn(tasks, seqs, gateways, start_events, end_events)

    return(bpmn_out)
  }

  bpmn_obj <- add_start_end(bpmn_obj)
  bpmn_out <- map_to_bpmn(bpmn_obj)

  bpmn_out %>%
    render_bpmn() %>%
    print()

  bpmn_out %>%
    write_bpmn("output/model.bpmn")

}
