init_empty_tasks <- function(){
    tibble(
      id = character(),
      name = character()
    )
  }

init_empty_seqs <- function(){
  tibble(
    id = character(),
    name = character(),
    sourceRef = character(),
    targetRef = character()
  )
}

init_empty_gateways <- function(){
  tibble(
    id = character(),
    name = character(),
    gatewayType = character(),
    gatewayDirection = character()
  )
}

init_empty_events <- function(){
  tibble(
    id=character(),
    name=character()
    )
}