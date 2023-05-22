#' Convert BPMN model to petrinet notation
#'
#' @param bpmn A bpmn model, as discovered using `discover()`
#'
#' @return A marked petrinet, as defined by petrinetR
#' @export
to_petrinet <- function(bpmn) {

  objectType_from <- NULL
  objectType_to <- NULL
  name <- NULL
  objectType <- NULL
  gatewayDirection_from <- NULL
  gatewayDirection_to <- NULL
  old_id <- NULL
  from <- NULL
  to <- NULL
  label <- NULL

  tmp_bpmn <- bpmn

  while(length(get_or_splits(tmp_bpmn)) > 0) {
    tmp_bpmn <- replace_or_split(tmp_bpmn, get_or_splits(tmp_bpmn)[1])
  }





  tmp_bpmn$flows -> seqs

  tibble(data = list(tmp_bpmn$nodes, tmp_bpmn$events)) %>%
    unnest(data) -> nodes

  seqs %>%
    left_join(nodes, by = c("sourceRef" = "id"), suffix = c("_seq","_from")) %>%
    rename_with(.cols = c("gatewayDirection"), .fn = ~paste0(.x, "_from")) %>%
    left_join(nodes, by = c("targetRef" = "id"), suffix = c("_seq","_to"))  %>%
    rename_with(.cols = c("objectType","gatewayDirection"), .fn = ~paste0(.x, "_to")) %>%
    as_tibble() -> data

  data  %>%
    as_tibble() %>%
    count(objectType_from,objectType_to)

  flows <- tibble()

  bind_rows(tmp_bpmn$events) %>%
    rename(label = name) -> places

  tmp_bpmn$nodes %>%
    filter(objectType == "task") %>%
    rename(label = name) -> transitions

  data %>%
    filter(objectType_from == "task",objectType_to == "task") -> task_task

  task_task %>%
    transmute(id = paste(sourceRef, targetRef, sep = "_"),
              label = id) -> new_places


  places %>%
    bind_rows(new_places) -> places

  task_task %>%
    transmute(id = paste(sourceRef, targetRef, sep = "_"),
              sourceRef, targetRef) -> prep_new_flows

  new_flows <- list_along(1:nrow(prep_new_flows))
  for(i in 1:nrow(prep_new_flows)) {
    tibble(from = c(prep_new_flows$sourceRef[i], prep_new_flows$id[i]),
           to = c(prep_new_flows$id[i], prep_new_flows$targetRef[i])) -> new_flows[[i]]
  }
  flows <- bind_rows(flows, bind_rows(new_flows))


  data %>%
    filter(!(objectType_from == "task" & objectType_to == "task")) %>%
    filter(!(gatewayDirection_from == "Diverging" & gatewayDirection_to == "Converging" &
               objectType_from == "parallelGateway" & objectType_to == "parallelGateway") | is.na(objectType_from) | is.na(objectType_to)) -> data


  data %>%
    transmute(from = sourceRef, to = targetRef) %>%
    bind_rows(flows) -> flows

  nodes %>%
    filter(objectType == "exclusiveGateway") %>%
    transmute(id, label = name) %>%
    bind_rows(places) -> places

  nodes %>%
    filter(objectType == "parallelGateway") %>%
    transmute(id, label = name) %>%
    bind_rows(transitions) -> transitions


  data %>%
    filter(objectType_from == "exclusiveGateway", objectType_to == "exclusiveGateway",
           gatewayDirection_from == "Diverging", gatewayDirection_to == "Converging") -> or_or

  or_or %>%
    transmute(sourceRef, targetRef, id = paste(sourceRef, targetRef, sep = "_"),
              label = id) -> new_transitions

  transitions %>%
    bind_rows(new_transitions) -> transitions

  or_or %>%
    transmute(id = paste(sourceRef, targetRef, sep = "_"),
              sourceRef, targetRef) -> prep_new_flows

  if(nrow(prep_new_flows)>0) {
    new_flows <- list_along(1:nrow(prep_new_flows))
    for(i in 1:nrow(prep_new_flows)) {
      tibble(from = c(prep_new_flows$sourceRef[i], prep_new_flows$id[i]),
             to = c(prep_new_flows$id[i], prep_new_flows$targetRef[i])) -> new_flows[[i]]
    }
    flows %>%
      anti_join(or_or, by = c("from" = "sourceRef", "to" = "targetRef")) %>%
      bind_rows(bind_rows(new_flows)) -> flows
  }


  data %>%
    filter(objectType_from == "exclusiveGateway", objectType_to == "exclusiveGateway",
           gatewayDirection_from == gatewayDirection_to) -> or_or

  or_or %>%
    transmute(sourceRef, targetRef, id = paste(sourceRef, targetRef, sep = "_"),
              label = id) -> new_places

  new_places %>%
    gather(key, old_id, sourceRef, targetRef) -> new_places_long

  flows %>%
    anti_join(or_or, by = c("from" = "sourceRef", "to" = "targetRef")) %>%
    left_join(new_places_long, by = c("from"= "old_id")) %>%
    mutate(from = ifelse(is.na(id), from, id)) %>%
    select(-id, -label, -key) %>%
    left_join(new_places_long, by = c("to"= "old_id")) %>%
    mutate(to = ifelse(is.na(id), to, id)) %>%
    select(-id, -label, -key) -> flows

  places %>%
    filter(!(id %in% or_or$sourceRef | id %in% or_or$targetRef)) %>%
    bind_rows(new_places %>% select(id, label)) -> places

  data %>%
    filter(objectType_from == "parallelGateway", objectType_to == "parallelGateway") -> and_and

  and_and %>%
    transmute(id = paste(sourceRef, targetRef, sep = "_"),
              label = id) -> new_places

  places %>%
    bind_rows(new_places) -> places

  and_and %>%
    transmute(id = paste(sourceRef, targetRef, sep = "_"),
              sourceRef, targetRef) -> prep_new_flows

  if(nrow(prep_new_flows) > 0) {
    new_flows <- list_along(1:nrow(prep_new_flows))
    for(i in 1:nrow(prep_new_flows)) {
      tibble(from = c(prep_new_flows$sourceRef[i], prep_new_flows$id[i]),
             to = c(prep_new_flows$id[i], prep_new_flows$targetRef[i])) -> new_flows[[i]]
    }
    flows %>%
      anti_join(and_and, by = c("from" = "sourceRef", "to" = "targetRef")) %>%
      bind_rows(bind_rows(new_flows)) -> flows
  }

  data %>%
    filter(objectType_from == "parallelGateway", gatewayDirection_from == "Diverging", objectType_to == "task") -> and_task

  and_task %>%
    transmute(id = paste(sourceRef, targetRef, sep = "_"),
              label = id) -> new_places

  places %>%
    bind_rows(new_places) -> places

  and_task %>%
    transmute(id = paste(sourceRef, targetRef, sep = "_"),
              sourceRef, targetRef) -> prep_new_flows

  if(nrow(prep_new_flows) > 0) {

    new_flows <- list_along(1:nrow(prep_new_flows))
    for(i in 1:nrow(prep_new_flows)) {
      tibble(from = c(prep_new_flows$sourceRef[i], prep_new_flows$id[i]),
             to = c(prep_new_flows$id[i], prep_new_flows$targetRef[i])) -> new_flows[[i]]
    }

    flows %>%
      anti_join(and_task, by = c("from" = "sourceRef", "to" = "targetRef")) %>%
      bind_rows(bind_rows(new_flows)) -> flows

  }

  data %>%
    filter(objectType_to == "parallelGateway", gatewayDirection_to == "Converging", objectType_from == "task") -> task_and

  task_and %>%
    transmute(id = paste(sourceRef, targetRef, sep = "_"),
              label = id) -> new_places

  places %>%
    bind_rows(new_places) -> places

  task_and %>%
    transmute(id = paste(sourceRef, targetRef, sep = "_"),
              sourceRef, targetRef) -> prep_new_flows

  if(nrow(prep_new_flows) > 0) {
    new_flows <- list_along(1:nrow(prep_new_flows))
    for(i in 1:nrow(prep_new_flows)) {
      tibble(from = c(prep_new_flows$sourceRef[i], prep_new_flows$id[i]),
             to = c(prep_new_flows$id[i], prep_new_flows$targetRef[i])) -> new_flows[[i]]
    }

    flows %>%
      anti_join(task_and, by = c("from" = "sourceRef", "to" = "targetRef")) %>%
      bind_rows(bind_rows(new_flows)) -> flows
  }

  transitions %>%
    mutate(label = ifelse(label %in% c("SPLIT","MERGE"), NA, label)) %>%
    mutate(label = ifelse(str_detect(label, "split|merge"), NA, label)) %>%
    select(label, id) -> transitions

    create_marked_PN(create_PN(as.data.frame(places), as.data.frame(transitions), as.data.frame(flows)), initial_marking =  tmp_bpmn$startEvent$id,  final_marking =  tmp_bpmn$endEvent$id) -> converted_petri

}
