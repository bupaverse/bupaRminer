

get_or_splits <- function(bpmn) {
  objectType <- NULL
  bpmn$nodes %>%
    filter(gatewayDirection == "Diverging" & objectType == "inclusiveGateway") %>%
    pull(id)
}

find_corresponding_exit <- function(entry_id, bpmn, join_id) {
  next_step <- entry_id

  while(next_step != join_id) {
    exit <-  bpmn$flows %>% filter(sourceRef == next_step)

    if(nrow(exit) > 1) {
      exit %>%
        filter(!str_detect(targetRef, "LOOPBACK_MERGE")) -> exit
    }
    next_step <- exit$targetRef[1]
  }
  return(exit$sourceRef)
}

replace_or_split <- function(bpmn, or_split_id) {
  objectType <- NULL
  diverging <- NULL
  converging <- NULL
  tmp <- NULL


  bpmn$nodes %>%
    filter(gatewayDirection == "Converging" & objectType == "inclusiveGateway") -> or_joins

  current_split <- or_split_id

  bpmn$flows %>%
    filter(sourceRef == current_split) -> outgoing

  outgoing %>%
    filter(targetRef %in% or_joins$id) -> current_join

  outgoing %>%
    filter(!targetRef %in% current_join) -> to_be_replaced


  to_be_replaced %>%
    mutate(corresponding_exit = map_chr(targetRef, find_corresponding_exit, bpmn, current_join$targetRef)) -> to_be_replaced

  bpmn$flows %>%
    filter(targetRef == current_join$targetRef) %>%
    filter(sourceRef != current_split) -> to_be_replaced_2

  new_gateways <- tibble(
    id = paste0(rep(c("new_xor_split","new_xor_join"), times = nrow(to_be_replaced)),
                as.character(as.numeric(lubridate::now())),
                "__",
                rep(to_be_replaced$targetRef, each = 2)),
    name = rep(c("SPLIT","MERGE"), times = nrow(to_be_replaced)),
    objectType = "exclusiveGateway",
    gatewayDirection = rep(c("Diverging","Converging"), times = nrow(to_be_replaced)),
    tmp = rep(to_be_replaced$targetRef, each = 2)
  )

  new_entries <- tibble(sourceRef = current_split, name = "", targetRef = new_gateways %>% filter(gatewayDirection == "Diverging") %>% pull(id))
  new_exit <- tibble(targetRef = current_join$targetRef, name = "", sourceRef = new_gateways %>% filter(gatewayDirection == "Converging") %>% pull(id))
  new_gateways %>%
    select(id, tmp, gatewayDirection) %>%
    spread(gatewayDirection, id) %>%
    rename(sourceRef = Diverging, targetRef = Converging) %>%
    select(-tmp) %>%
    mutate(name = "") -> new_skips

  tibble(sourceRef = new_gateways %>% filter(gatewayDirection == "Diverging") %>% pull(id), targetRef = to_be_replaced$targetRef, name = "") -> new_xor_split_exits

  tibble(sourceRef = to_be_replaced$corresponding_exit, targetRef = new_gateways %>% filter(gatewayDirection == "Converging") %>% pull(id), name = "") -> new_xor_join_entries

  bind_rows(new_entries, new_exit, new_skips, new_xor_join_entries, new_xor_split_exits) %>%
    mutate(id = paste0(sourceRef, "______", targetRef)) -> new_sequence_flows

  bpmn$flows %>% filter(!id %in% c(to_be_replaced$id, to_be_replaced_2$id)) %>%
    bind_rows(new_sequence_flows) -> bpmn$flows

  bpmn$nodes %>%
    mutate(objectType = if_else(id %in% c(current_split, current_join$targetRef), "parallelGateway", objectType)) %>%
    bind_rows(select(new_gateways, -tmp)) -> bpmn$nodes

  create_bpmn(bpmn$nodes, bpmn$flows, bpmn$events)
}




