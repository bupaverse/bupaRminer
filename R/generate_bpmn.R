


generate_bpmn <- function(snippet_dictionary) {

  bpmn_obj <- snippet_dictionary[[length(snippet_dictionary)]]
  
  floating_gws <- bpmn_obj$gateways %>%
    filter(!id %in% bpmn_obj$seqs$sourceRef)
  
  if(floating_gws %>% nrow > 0){
    for(floating_gw in floating_gws$id){
      if(bpmn_obj$seqs %>% filter(targetRef == floating_gw) %>% nrow <= 2){
        ## Replace gateway with end event
        new_end <- tibble(
          id = floating_gw,
          name = "END")
        bpmn_obj$end_events <- bpmn_obj$end_events %>%
          bind_rows(new_end)
        
        bpmn_obj$gateways <- bpmn_obj$gateways %>%
          filter(id != floating_gw)
        
        seqs_to_gw <- bpmn_obj$seqs %>% filter(
          targetRef == floating_gw
        ) %>% filter(
          !sourceRef %in% bpmn_obj$tasks$id
        )
        
        if(seqs_to_gw %>% nrow > 0){
          bpmn_obj$seqs <- bpmn_obj$seqs %>%
            filter(!id %in% seqs_to_gw$id)
        }
        
      } else{
        ## Connect end event to gatexay
        new_end <- tibble(
          id = paste("END",floating_gw, sep = "__"),
          name = "END")
        bpmn_obj$end_events <- bpmn_obj$end_events %>%
          bind_rows(new_end)
        
        new_sequence <- establish_sequence(
          floating_gw,
          new_end$id
        )
        bpmn_obj$seqs <- bpmn_obj$seqs %>%
          bind_rows(new_sequence)
      }
    }
  }

  map_to_bpmn <- function(bpmn_obj){
    tasks <- bpmn_obj$tasks %>% as.data.frame()
    seqs <- bpmn_obj$seqs %>% as.data.frame() %>% unique
    gateways <- bpmn_obj$gateways %>% as.data.frame() %>% unique
    start_events <- bpmn_obj$start_events %>% as.data.frame() %>% unique
    end_events <- bpmn_obj$end_events %>% as.data.frame() %>% unique

    if(nrow(gateways) > 0) {

      seqs %>%
        as_tibble() %>%
        left_join(select(gateways, id, gatewayType, gatewayDirection), by = c("sourceRef" = "id"), suffix = c("","_source")) %>%
        left_join(select(gateways, id, gatewayType, gatewayDirection), by = c("targetRef" = "id"), suffix = c("","_target")) %>%
        filter(gatewayType == gatewayType_target, gatewayDirection == "diverging", gatewayDirection_target == "converging") -> candidates_for_removal

      seqs %>%
        count(sourceRef) %>%
        rename(n_outgoing = n) -> outgoing_arcs

      seqs %>%
        count(targetRef) %>%
        rename(n_incoming = n) -> incoming_arcs


      candidates_for_removal %>%
        filter(gatewayType == "ParallelGateway") %>%
        pull(id) -> remove_parallels

      candidates_for_removal %>%
        filter(gatewayType == "ExclusiveGateway") %>%
        left_join(outgoing_arcs, by = "sourceRef") %>%
        left_join(incoming_arcs, by = "targetRef") %>%
        filter(n_outgoing > 2,
               n_incoming > 2) %>%
        pull(id) -> remove_exclusives

      seqs <- seqs %>%
        filter(!(id %in% c(remove_parallels, remove_exclusives)))
    }
    suppressWarnings({
      nodes <- bind_rows(
        tasks %>% mutate(objectType = "task"),
        gateways %>% mutate(objectType = fct_recode(gatewayType , "exclusiveGateway" = "ExclusiveGateway",
                                                    "inclusiveGateway" = "InclusiveGateway",
                                                    "parallelGateway" = "ParallelGateway")) %>%
          select(-gatewayType) %>%
          mutate(gatewayDirection = stringr::str_to_title(gatewayDirection))
      )
    })

    events <- bind_rows(
      start_events %>% mutate(objectType = "startEvent"),
      end_events %>% mutate(objectType = "endEvent")
    )

    flows <- seqs %>%
      mutate(objectType = "sequenceFlow")
   
    bpmnR::create_bpmn(nodes, flows, events)

  }

  map_to_bpmn(bpmn_obj)
}
