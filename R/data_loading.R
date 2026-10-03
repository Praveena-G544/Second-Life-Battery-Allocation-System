# 5a: read CSVs into data frames; 5c: factors with custom levels
load_all <- function(dir = "data") {
  ref <- read.csv(file.path(dir, "battery_reference.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))
  ref$battery_key <- paste(ref$manufacturer, ref$vehicle_model, ref$battery_variant)
  req <- read.csv(file.path(dir, "application_requirements.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))
  list(ref = ref, req = req, assess = load_assessment(dir))
}
load_assessment <- function(dir = "data") {
  a <- read.csv(file.path(dir, "battery_assessment.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))
  a$physical_condition <- factor(ifelse(is.na(a$physical_condition), "Unknown", a$physical_condition), levels = CONDITION_LEVELS)
  a
}
save_assessment <- function(row, dir = "data") {
  f <- file.path(dir, "battery_assessment.csv")
  old <- read.csv(f, stringsAsFactors = FALSE)
  row$physical_condition <- as.character(row$physical_condition)
  write.csv(rbind(old, row[names(old)]), f, row.names = FALSE, na = "")
}
