PATHWAY_LEVELS   <- c("Potential Second Life","Further Assessment","Recycling Assessment","Insufficient Information")
CONDITION_LEVELS <- c("Good","Fair","Poor","Unknown")
DISCLAIMER <- "This system provides preliminary screening and planning information based on available battery data. It does not certify battery safety, suitability, performance, or compliance. Professional testing, inspection, safety evaluation, and engineering assessment may be required before any real-world reuse decision."
is_blank <- function(x) is.null(x) || length(x) == 0 || is.na(x) || (is.character(x) && !nzchar(trimws(x)))
fmt <- function(x, unit = "") if (is_blank(x)) "Not available" else paste0(x, if (nzchar(unit)) paste0(" ", unit) else "")
