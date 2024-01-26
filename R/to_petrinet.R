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
    mutate(from = sourceRef, to = targetRef) -> flows

  nodes %>%
    filter(objectType %in% c("task","parallelGateway")) %>%
    mutate(label = name) -> transitions

  nodes %>%
    filter(objectType %in% c("startEvent","endEvent","exclusiveGateway")) %>%
    mutate(label = name) -> places


  pt <- bind_rows(mutate(transitions, type = "T"), mutate(places, type = "P"))


  flows %>%
    as_tibble() %>%
    left_join(pt, by = c("from" = "id"), suffix = c("_seq","_from")) %>%
    rename_with(.cols = c("gatewayDirection", "type"), .fn = ~paste0(.x, "_from")) %>%
    left_join(pt, by = c("to" = "id"), suffix = c("_seq","_to"))  %>%
    rename_with(.cols = c("objectType","gatewayDirection", "type"), .fn = ~paste0(.x, "_to")) %>%
    as_tibble() -> data

  data %>%
    filter(type_from != type_to) -> flows

  data %>%
    filter(type_from == "T",type_to == "T") -> TT

  TT %>%
    mutate(id = paste(sourceRef, targetRef, sep = "____"), label = "") -> TT
  TT %>%
    select(id, label) -> new_places

  TT %>%
    select(from = sourceRef, to = id) -> new_flows_in
  TT %>%
    select(from = id, to = targetRef) -> new_flows_out

  flows %>%
    bind_rows(new_flows_in) %>%
    bind_rows(new_flows_out) -> flows

  places %>%
    bind_rows(new_places) -> places

  data %>%
    filter(type_from == "P",type_to == "P") -> PP

  PP %>%
    select(-id) %>%
    distinct() %>%
    mutate(id = paste(sourceRef, targetRef, sep = "____"), label = "") -> PP

  PP %>%
    select(id, label) -> new_transitions

  PP %>%
    select(from = sourceRef, to = id) -> new_flows_in
  PP %>%
    select(from = id, to = targetRef) -> new_flows_out

  flows %>%
    bind_rows(new_flows_in) %>%
    bind_rows(new_flows_out) -> flows

  transitions %>%
    bind_rows(new_transitions) -> transitions


  petrinet_model <- create_marked_PN(create_PN(as.data.frame(places), as.data.frame(transitions), as.data.frame(flows)), initial_marking =  tmp_bpmn$startEvent$id,  final_marking =  tmp_bpmn$endEvent$id) -> converted_petri

  petrinet_model$initial_marking <- tmp_bpmn$events %>% filter(objectType == "startEvent") %>% pull(id)
  petrinet_model$final_marking <- tmp_bpmn$events %>% filter(objectType == "endEvent") %>% pull(id)
  petrinet_model$petrinet$transitions %>% mutate(label = ifelse(objectType == "parallelGateway", NA, label)) -> petrinet_model$petrinet$transitions

  petrinet_model
}
