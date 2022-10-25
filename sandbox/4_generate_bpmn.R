library(bpmnR)

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

add_start_end <- function(bpmn_obj){
  if(bpmn_obj$start_events %>% nrow == 0){
    bpmn_obj$start_events <- data.frame(
      id="START",
      name="START"
    )
  }
  if(bpmn_obj$end_events %>% nrow == 0){
    bpmn_obj$end_events <- data.frame(
      id="END",
      name="END"
    )
  }
  if(!is.null(bpmn_obj$init) & !(bpmn_obj$init %in% bpmn_obj$start_events$id)){
    bpmn_obj$seqs <- bpmn_obj$seqs %>%
      bind_rows(
        data.frame(
          id = "newstart",
          name = "",
          sourceRef = bpmn_obj$start_events$id[1],
          targetRef = bpmn_obj$init)
        )
  }
  if(!is.null(bpmn_obj$close) & !(bpmn_obj$close %in% bpmn_obj$end_events$id)){
    bpmn_obj$seqs <- bpmn_obj$seqs %>%
      bind_rows(
        data.frame(
          id = "newend",
          name = "",
          sourceRef = bpmn_obj$close,
          targetRef = bpmn_obj$end_events$id[bpmn_obj$end_events %>% nrow()]
        )
      )
  }  
  return(bpmn_obj)
}

bpmn_obj <- add_start_end(bpmn_obj)
bpmn_out <- map_to_bpmn(bpmn_obj) 

bpmn_out %>% 
  render_bpmn() %>%
  print()

bpmn_out %>% 
  write_bpmn("output/model.bpmn")