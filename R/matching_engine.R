# Transparent rule-based screening. No ML, no hidden scores.
validate_assessment <- function(a) {
  m <- character()
  neg <- function(x, nm) if (!is_blank(x) && x < 0) m <<- c(m, paste(nm, "cannot be negative."))
  neg(a$age_years, "Age"); neg(a$cycle_count, "Cycle count"); neg(a$current_capacity_kwh, "Current capacity")
  if (!is_blank(a$state_of_health_percent) && (a$state_of_health_percent < 0 || a$state_of_health_percent > 100))
    m <- c(m, "State of health must be between 0 and 100 %.")
  m
}
check_one <- function(r, a) {
  v <- a[[r$parameter]]
  if (!is.na(r$required_category)) {
    if (is_blank(v) || v %in% c("Not entered", "Unknown")) return(c("Missing", "User Input Required"))
    return(if (v == r$required_category) c("Met", paste("Reported:", v)) else c("Not met", paste0("Reported '", v, "', criterion requires '", r$required_category, "'")))
  }
  if (is_blank(v)) return(c("Missing", "User Input Required"))
  ok <- v >= r$minimum_value && (is.na(r$maximum_value) || v <= r$maximum_value)
  c(if (ok) "Met" else "Not met", paste0("Entered ", v, " ", r$unit, "; criterion minimum ", r$minimum_value, " ", r$unit))
}
screen_battery <- function(a, req) {
  det <- do.call(rbind, lapply(seq_len(nrow(req)), function(i) {
    r <- check_one(req[i, ], a)
    data.frame(application = req$application[i], parameter = req$parameter[i], status = r[1], detail = r[2], stringsAsFactors = FALSE)
  }))
  sm <- do.call(rbind, lapply(split(det, det$application), function(d) {
    met <- sum(d$status == "Met"); no <- sum(d$status == "Not met"); mi <- sum(d$status == "Missing")
    res <- if (mi == nrow(d)) "Insufficient Information" else if (no > 0) "Recycling Assessment" else if (mi > 0) "Further Assessment Required" else "Potential Match"
    why <- switch(res,
      "Potential Match" = "All screening criteria for this application are satisfied by the entered information. Professional assessment is still required before any reuse.",
      "Further Assessment Required" = paste("Missing information:", paste(d$parameter[d$status == "Missing"], collapse = ", ")),
      "Recycling Assessment" = paste("Criteria not met:", paste(d$parameter[d$status == "Not met"], collapse = ", "), "- no second-life pathway supported for this application under the project criteria."),
      "Insufficient Information" = "Not enough information entered to screen this application.")
    data.frame(application = d$application[1], result = res, met = met, not_met = no, missing = mi, explanation = why, stringsAsFactors = FALSE)
  }))
  rownames(sm) <- NULL
  list(details = det, summary = sm, pathway = overall_pathway(sm$result))
}
overall_pathway <- function(results) {
  p <- if (all(results == "Insufficient Information")) "Insufficient Information"
       else if (any(results == "Potential Match")) "Potential Second Life"
       else if (any(results == "Further Assessment Required")) "Further Assessment"
       else "Recycling Assessment"
  factor(p, levels = PATHWAY_LEVELS)
}
