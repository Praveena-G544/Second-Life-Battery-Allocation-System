# 5d: mathematical operations on data-frame columns
add_retention <- function(df, ref) {
  df$rated_capacity_kwh <- ref$rated_capacity_kwh[match(df$selected_model, ref$battery_key)]
  df$capacity_retention_pct <- round(df$current_capacity_kwh / df$rated_capacity_kwh * 100, 1)
  df$capacity_loss_kwh <- df$rated_capacity_kwh - df$current_capacity_kwh
  df
}
cost_summary <- function(reuse, testing, install, other, replacement) {
  v <- function(x) if (is_blank(x)) 0 else x
  reuse_total <- v(reuse) + v(testing) + v(install) + v(other)
  diff <- reuse_total - v(replacement)
  data.frame(reuse_total = reuse_total, replacement = v(replacement), difference = diff,
             pct_difference = if (v(replacement) > 0) round(diff / replacement * 100, 1) else NA)
}
