# Population analysis: frequency tables (5b) and row/column removal (5e)
population_table <- function(assess, ref, req) {
  a <- assess[!is.na(assess$current_capacity_kwh) & !is.na(assess$selected_model), ]   # 5e: drop incomplete records
  if (nrow(a) == 0) return(a)
  a <- add_retention(a, ref)
  a$pathway <- factor(vapply(seq_len(nrow(a)), function(i)
    as.character(screen_battery(as.list(a[i, ]), req)$pathway), ""), levels = PATHWAY_LEVELS)
  a
}
freq_table <- function(x) { t <- as.data.frame(table(x), stringsAsFactors = FALSE); names(t) <- c("Category", "Count"); t }
comparison_view <- function(pop, ref, ids) {
  v <- merge(pop[pop$battery_id %in% ids, ], ref, by.x = "selected_model", by.y = "battery_key")
  v <- v[, c("battery_id","manufacturer","vehicle_model","chemistry","rated_capacity_kwh.x","nominal_voltage_v","power_kw",
             "age_years","current_capacity_kwh","state_of_health_percent","cycle_count","pathway")]   # 5e: drop unneeded columns
  names(v)[names(v) == "rated_capacity_kwh.x"] <- "rated_capacity_kwh"; v
}
