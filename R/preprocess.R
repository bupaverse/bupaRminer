
preprocess <- function(eventlog) {

  ev_log <- select(eventlog) %>%
    as.data.table() %>%
    rename(AID = .data[[activity_id(eventlog)]],
           AIID = .data[[activity_instance_id(eventlog)]],
           CID = .data[[case_id(eventlog)]],
           TS = .data[[timestamp(eventlog)]],
           LC =.data[[lifecycle_id(eventlog)]]) %>%
    mutate(AIID:= as.character(AIID)) %>%
    as.data.table()

  endpoints <- melt(ev_log[, .(CID, START = min(TS) - 10000, END = max(TS) + 10000)],
       id.vars = "CID", measure.vars = c("START","END"), variable.name = "AID", value.name = "TS")

  endpoints[, AIID := paste(CID, AID, sep = "_")]
  endpoints[, LC := "complete"]

  rbindlist(list(ev_log, endpoints), fill = TRUE) -> ev_log


  lc <- melt(ev_log[,.(start = min(TS), complete = max(TS)), by = AIID], id.vars = "AIID", measure.vars = c("start","complete"),
             variable.name = "LC", value.name = "TS")


  merge(unique(ev_log[, .(CID,AID,AIID)]), lc) -> ev_log

  ev_log[LC == "complete", ] -> completes

  completes[order(TS), timestep := 1:.N, by = CID][, first_act_occ_step := min(timestep), by = .(CID, AID)][, is_repeat := timestep > first_act_occ_step][order(TS), is_repeat_cnt := cumsum(is_repeat) + 1, by = .(CID,AID)]

  completes[, new_act_name := AID]
  completes[is_repeat_cnt > 1, new_act_name := paste(AID, is_repeat_cnt, sep = "_")]
  completes[, .(CID, AIID, new_act_name, is_repeat_cnt)] -> completes

  merge(ev_log, completes, by = c("CID","AIID")) -> ev_log

  ev_log %>%
    rename(orig_name = AID,
           is_repeat = is_repeat_cnt) %>%
    mutate(AID = new_act_name) %>%
    as.data.table() -> ev_log


  ev_log[,act_lc := paste(AID, LC, sep = "___")]
  # ev_log[,TS := as.numeric(TS)]
  ev_log[,block_content := paste(sort(act_lc), collapse = "|||"), by = .(TS, CID)]

  ev_log[, .N , by = block_content][,block_id := 1:.N][,.(block_content, block_id)] -> blocks



  unique(merge(ev_log, blocks, by = "block_content")[,.(CID, TS, block_content)])[order(TS), .(trace = paste(block_content, collapse = "+++")), by = CID] -> traces

  traces[, .(CID = first(CID), CASE_COUNT = .N), by = trace] -> unique_traces
  ev_log <- merge(ev_log, unique_traces, by = "CID")

  # ev_log[["CASE_COUNT"]] <- 1
  # ev_log[["trace"]] <- ev_log[["CID"]]
  ev_log[, AID := as.character(AID)]

  ev_log[, orig_name := as.character(orig_name)]
  ev_log[, new_act_name := as.character(new_act_name)]
  ev_log[, LC := as.character(LC)]

}
