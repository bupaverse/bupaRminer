
end_counter <- 1


create_snippet <- function(
    start_point,
    end_point,
    branches = list(),
    connection_type = c("SEQ","XOR","AND","OR","INTER","NONINTER"),
    snippet_dict = list(),
    start_event_name = c("START","__START__"),
    end_event_name = c("END","__END__"),
    seq_name = ""
){
  new_snippet <- list(
    tasks = tibble(),
    seqs = tibble(),
    gateways = tibble(),
    start_events = tibble(),
    end_events = tibble(),
    init = c(),
    close = c()
  )

  start_point <- decode_task(start_point,
                             snippet_dict,
                             start_event_name,
                             end_event_name)
  incoming_connection_A <- start_point$init
  outgoing_connection_A <- start_point$close

  new_snippet <- start_point

  END_SKIPPED <- FALSE
  ## If we already end in an END, then we must create a separate split in front of this END
  if(!is.null(start_point$close) && start_point$end_events %>% nrow > 0 && start_point$close %in% (start_point$end_events$id)){

    END_SKIPPED <- TRUE
    gateway_before_end <- tibble(
      id = paste("XOR_SPLIT_INSERTED", as.numeric(Sys.time()), sep = "_"),
      name = "To end?",
      gatewayType = "ExclusiveGateway",
      gatewayDirection = "diverging"
    )
    new_snippet$gateways <- new_snippet$gateways %>%
      bind_rows(gateway_before_end)
    new_snippet$seqs <- new_snippet$seqs %>%
      mutate(targetRef = ifelse(targetRef == start_point$close, gateway_before_end$id, targetRef))

    new_snippet$seqs <- new_snippet$seqs %>%
      bind_rows(
        tibble(
          id = paste("XOR_SPLIT_INSERTED", as.numeric(Sys.time()), sep = "_"),
          name = "",
          sourceRef = gateway_before_end$id,
          targetRef = start_point$close
        )
      )

    start_point$close <- gateway_before_end$id

    outgoing_connection_A <- start_point$close
  }

  end_point <- decode_task(end_point,
                           snippet_dict,
                           start_event_name,
                           end_event_name)
  incoming_connection_B <- end_point$init
  outgoing_connection_B <- end_point$close

  new_snippet <- new_snippet %>%
    expand_snippet(end_point)

  if(connection_type == "SEQ"){
    new_sequence <- establish_sequence(
      outgoing_connection_A,
      incoming_connection_B,
      seq_name = seq_name
    )

    new_snippet$seqs <- new_snippet$seqs %>%
      bind_rows(new_sequence)
  }

  branch_snippet <- NULL
  if(connection_type %in% c("XOR","AND","OR")){

    gw_type <- case_when(
      connection_type == "XOR" ~"ExclusiveGateway",
      connection_type == "AND" ~"ParallelGateway",
      connection_type == "OR" ~"InclusiveGateway",
      TRUE ~ connection_type
    )

    ## Check if there are branches that start and end
    ## with the same type of gateway. If so, we can merge
    ## them.
    same_type_branches <- list()
    other_type_branches <- list()

    branch_has_start<- FALSE

    for(branch in branches){

      same_type <- FALSE

      branch <- decode_task(branch,
                            snippet_dict,
                            start_event_name,
                            end_event_name)

      if(!is.null(branch$start_events) && branch$start_events %>% nrow > 0){
        branch_has_start <- TRUE
        start_event_id <- branch$start_events %>%
          head(1) %>%
          pull(id)
        artefact_from_start <- branch$seqs %>%
          filter(sourceRef == start_event_id) %>%
          pull(targetRef)

        branch$init <- artefact_from_start
        branch$seqs <- branch$seqs %>%
          filter(sourceRef != start_event_id)
      }
      
      if(dead_end_check(branch) == TRUE & connection_type == "AND"){
        print("END in AND removed")
        branch_end_id <- branch$close
        new_close <- branch$seqs %>%
          filter(targetRef == branch_end_id) %>%
          head(1) %>%
          pull(sourceRef)
        branch$close <- new_close
        branch$seqs <- branch$seqs %>%
          filter(targetRef != branch_end_id,
                 sourceRef != branch_end_id)
        branch$end_events <- branch$end_events  %>%
          filter(id != branch_end_id)
      }

      if(!is.null(branch$gateways) & branch$gateways %>% nrow() > 0){
        branch_gateways_start <- branch$gateways %>%
          filter(gatewayType == gw_type,
                 id == branch$init)

        if (branch_gateways_start %>% nrow() > 0) {
          branch_gateways_end <- branch$gateways %>%
            filter(gatewayType == gw_type,
                   gatewayDirection == "converging",
                   id == branch$close
            )

          if (branch_gateways_end %>% nrow > 0) {
            branch_to_branch <- branch$seqs %>%
              filter(sourceRef == branch_gateways_start$id,
                     targetRef == branch_gateways_end$id)

            if (branch_to_branch %>% nrow > 0) {
              #print("SAME BRANCH CAN BE MERGED")
              same_type_branches[[length(same_type_branches) + 1]] <-
                branch
              same_type <- TRUE
            }
          }
        }
      }

      if(same_type == FALSE){
        other_type_branches[[length(other_type_branches)+1]] <- branch
      }
    }

    new_gateway_A <- tibble(
      id = paste(connection_type, "SPLIT", outgoing_connection_A, as.numeric(Sys.time()), sep = "__"),
      name = "SPLIT",
      gatewayType = gw_type,
      gatewayDirection = "diverging"
    )
    new_gateway_B <- tibble(
      id = paste(connection_type, "MERGE", incoming_connection_B, as.numeric(Sys.time()), sep = "__"),
      name = "MERGE",
      gatewayType = gw_type,
      gatewayDirection = "converging"
    )

    new_snippet$gateways <- new_snippet$gateways %>%
      bind_rows(new_gateway_A) %>%
      bind_rows(new_gateway_B)

    direct_seq <- establish_sequence(
      new_gateway_A$id,
      new_gateway_B$id)

    new_snippet$seqs <- new_snippet$seqs %>%
      bind_rows(direct_seq)

    if(!is.null(outgoing_connection_A)){

      new_sequence <- establish_sequence(
        outgoing_connection_A,
        new_gateway_A$id)

      new_snippet$seqs <- new_snippet$seqs %>%
        bind_rows(new_sequence)

    }

    if(!is.null(incoming_connection_B)){

      new_sequence <- establish_sequence(
        new_gateway_B$id,
        incoming_connection_B)

      new_snippet$seqs <- new_snippet$seqs %>%
        bind_rows(new_sequence)

    }

    branch_snippet <- list()
    for(branch in other_type_branches){

      if(dead_end_check(branch) == FALSE){
        clean_branch <- branch
        if(is.list(clean_branch)){
          clean_branch$seqs <- tibble()
        }

        branch_in <- create_snippet(
          start_point = list(
            init = new_gateway_A$id,
            close = new_gateway_A$id),
          end_point = clean_branch,
          branches = list(),
          connection_type = "SEQ")

        branch_snippet <- branch_snippet %>%
          expand_snippet(branch_in)

        branch_out <- create_snippet(
          start_point = branch,
          end_point = list(
            init = new_gateway_B$id,
            close = new_gateway_B$id),
          branches = list(),
          connection_type = "SEQ")


        branch_snippet <- branch_snippet %>%
          expand_snippet(branch_out)
      } else {

        print("BRANCH DEAD END")

        branch_in <- create_snippet(
          start_point = list(
            init = new_gateway_A$id,
            close = new_gateway_A$id),
          end_point = branch,
          branches = list(),
          connection_type = "SEQ")

        branch_snippet <- branch_snippet %>%
          expand_snippet(branch_in)
      }

      branch_snippet$seqs <- unique(branch_snippet$seqs)

    }

    for(branch in same_type_branches){

      old_start <- branch$init
      old_end <- branch$close


      branch$seqs <- branch$seqs %>%
        filter(!(sourceRef == old_start & targetRef == old_end)) %>%
        mutate(sourceRef = ifelse(sourceRef == old_start, new_gateway_A$id, sourceRef))

      if(dead_end_check(branch) == FALSE){
        branch$seqs <- branch$seqs %>%
          mutate(targetRef = ifelse(targetRef == old_end, new_gateway_B$id, targetRef))
      }

      branch_snippet <- branch_snippet %>%
        expand_snippet(branch)
    }

  }

  if(is.null(incoming_connection_A)){
    new_snippet$init <- new_gateway_A$id
  } else {
    new_snippet$init <- incoming_connection_A
  }

  if(is.null(outgoing_connection_B)){
    new_snippet$close <- new_gateway_B$id
  } else {
    new_snippet$close <- outgoing_connection_B
  }

  if(!is.null(branch_snippet)){
    new_snippet <- new_snippet %>%
      expand_snippet(branch_snippet)

    used_gateways <- c(new_snippet$seqs %>% pull(sourceRef),new_snippet$seqs %>% pull(targetRef) )
    new_snippet$gateways <- new_snippet$gateways %>%
      filter(id %in% used_gateways)

    if(branch_has_start){
      start_id <-  new_snippet$start_events %>%
        head(1) %>%
        pull(id)

      new_sequence <- establish_sequence(
        start_id,
        new_snippet$init
      )

      new_snippet$init <- start_id
      new_snippet$seqs <- new_snippet$seqs %>%
        bind_rows(new_sequence)
    }
  }

  return(new_snippet)
}


decode_task <- function(task_name,
                        snippet_dict,
                        start_event_name,
                        end_event_name){

  if(is.list(task_name)){
    return(task_name)
  }

  if(!is.null(task_name) && task_name %in% names(snippet_dict)){
    task_name <- snippet_dict[[task_name]]
    return(task_name)
  }

  if (is.character(task_name) && task_name != "") {
    if (task_name %in% start_event_name) {
      start_snippet <- list(
        tasks = tibble(),
        seqs = tibble(),
        gateways = tibble(),
        start_events = tibble(id = task_name,
                              name = task_name),
        end_events = tibble(),
        init = task_name,
        close = task_name
      )

      return(start_snippet)
    }

    if (task_name %in% end_event_name) {
      end_id <- paste(task_name, pkg.env$end_event_counter, sep = "_")
      pkg.env$end_event_counter <- pkg.env$end_event_counter + 1

      end_snippet <- list(
        tasks = tibble(),
        seqs = tibble(),
        gateways = tibble(),
        start_events = tibble(),
        end_events = tibble(id = end_id,
                            name = task_name),
        init = end_id,
        close = end_id
      )

      return(end_snippet)
    }
    task_id = str_replace_all(task_name, " ", "_")
    task_id = str_replace_all(task_id, "\\(", "_")
    task_id = str_replace_all(task_id, "\\)", "_")
    task_name = str_replace_all(task_name, "_REP_", "_")
    task_name = str_replace_all(task_name, "_", " ")
    task_name = trimws(task_name)
    task_name = gsub("\\d+$", "", task_name)
    task_name = trimws(task_name)

    atomic_task_snippet <- list(
      tasks = tibble(id = task_id,
                     name = task_name),
      seqs = tibble(),
      gateways = tibble(),
      start_events = tibble(),
      end_events = tibble(),
      init = task_id,
      close = task_id
    )

    return(atomic_task_snippet)
  } else {
    return(
      list(
        init = NULL,
        close = NULL,
        tasks = tibble(),
        seqs = tibble(),
        gateways = tibble(),
        start_events = tibble(),
        end_events = tibble()
      )
    )
  }
}

dead_end_check <- function(snippet){
  dead_end <- FALSE

  if(nrow(snippet$end_events) > 0) {
    if(!is.null(snippet$end_events$id) & snippet$close %in% snippet$end_events$id){
      dead_end <- TRUE
    }
  }

  return(dead_end)
}

establish_sequence <- function(
    out_A,
    in_B,
    seq_name = ""){
  new_sequence <- tibble(
    id = paste(out_A,in_B,sep = "__"),
    name = seq_name,
    sourceRef = out_A,
    targetRef = in_B
  )

  return(new_sequence)

}

expand_snippet <- function(old_snippet, extra_snippet){
  expanded_snippet <- old_snippet

  if(!is.null(extra_snippet$tasks)){
    expanded_snippet$tasks <- old_snippet$tasks %>%
      bind_rows(extra_snippet$tasks)

    expanded_snippet$tasks <- expanded_snippet$tasks %>% unique
  }
  if(!is.null(extra_snippet$seqs)){
    expanded_snippet$seqs <- old_snippet$seqs %>%
      bind_rows(extra_snippet$seqs) %>%
      unique

  }
  if(!is.null(extra_snippet$gateways)){
    expanded_snippet$gateways <- old_snippet$gateways %>%
      bind_rows(extra_snippet$gateways) %>%
      unique
  }
  if(!is.null(extra_snippet$start_events)){
    expanded_snippet$start_events <- old_snippet$start_events %>%
      bind_rows(extra_snippet$start_events) %>%
      unique
  }
  if(!is.null(extra_snippet$end_events)){
    expanded_snippet$end_events <- old_snippet$end_events %>%
      bind_rows(extra_snippet$end_events) %>%
      unique
  }

  return(expanded_snippet)
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

merge_end_events <- function(bpmn_obj){
  if(bpmn_obj$end_events %>% nrow < 2){
    return(bpmn_obj)
  }
  end_ids <- bpmn_obj$end_events %>% pull(id)
  new_gateway <- data.frame(
    id = paste("END_MERGE",as.numeric(Sys.time()), sep = "_"),
    name = "MERGE",
    gatewayType = "ExclusiveGateway",
    gatewayDirection= "converging"
  )
  bpmn_obj$seqs <- bpmn_obj$seqs %>%
    mutate(targetRef = ifelse(targetRef %in% end_ids, 
                              new_gateway,
                              targetRef))
  
  only_end_event <- bpmn_obj$end_events %>%
    head(1)
  bpmn_obj$end_events <- only_end_event
  new_sequence <- data.frame(
    id = paste("MERGE_END",as.numeric(Sys.time()), sep = "_"),
    name = "",
    sourceRef = new_gateway$id,
    targetRef = only_end_event$id
  )
  bpmn_obj$seqs <- bpmn_obj$seqs %>%
    bind_rows(new_sequence)
  bpmn_obj$gateways <- bpmn_obj$gateways %>%
    bind_rows(new_gateway)
  
  return(bpmn_obj)
}

## TODO Check this
add_loop_back <- function(bpmn_obj){
  init_gateway <- data.frame(
    id = paste("LOOPBACK_MERGE",as.numeric(Sys.time()), sep = "_"),
    name = "merge",
    gatewayType = "ExclusiveGateway",
    gatewayDirection= "converging"
  )
  close_gateway <- data.frame(
    id = paste("LOOPBACK",as.numeric(Sys.time()), sep = "_"),
    name = "split",
    gatewayType = "ExclusiveGateway",
    gatewayDirection= "diverging"
  )

  bpmn_obj$gateways <- bpmn_obj$gateways %>%
    bind_rows(init_gateway) %>%
    bind_rows(close_gateway)

  bpmn_obj$seqs <- bpmn_obj$seqs %>%
    bind_rows(
      data.frame(
        id = c(paste("init", as.numeric(Sys.time()), sep = "_"),
               paste("close", as.numeric(Sys.time()), sep = "_"),
               paste("back", as.numeric(Sys.time()), sep = "_")),
        name = c("","",""),
        sourceRef = c(init_gateway$id, bpmn_obj$close, close_gateway$id),
        targetRef = c(bpmn_obj$init, close_gateway$id, init_gateway$id)
      )
    )

  bpmn_obj$init <- init_gateway$id
  bpmn_obj$close <- close_gateway$id

  return(bpmn_obj)
}

check_start <- function(snippet_name, 
                        snippet_dict,
                        start_event_name = c("START","__START__")){
  if(snippet_name %in% start_event_name){
    return(TRUE)
  }
  if(snippet_name %in% names(snippet_dict)){
    snippet <- snippet_dict[[snippet_name]]
    
    if(snippet$start_events %>% nrow > 0){
      if(snippet$init %in% snippet$start_events$id){
        return(TRUE)
      }
    }
    
  }
  return(FALSE)
}

check_split <- function(snippet_name, 
                        snippet_dict){
  if(snippet_name %in% names(snippet_dict)){
    snippet <- snippet_dict[[snippet_name]]
    if(snippet$gateways %>% nrow == 0){
      return(FALSE)
    }
    init_gateway <- snippet$gateways %>%
      filter(id == snippet$init)
    
    if(init_gateway %>% nrow != 1){
      return(FALSE)
    }
    
    init_gateway_sequences <- snippet$seqs %>%
      filter(sourceRef == init_gateway$id)
    
    if(init_gateway_sequences %>% nrow == 2){
      direct_gateway <- snippet$gateways %>%
        filter(id %in% init_gateway_sequences$targetRef,
               gatewayType == init_gateway$gatewayType,
               gatewayDirection == "converging"
        )
      
      if(direct_gateway %>% nrow == 1){
        return(TRUE)
      }
    }
    
  }
  
  return(FALSE)
}