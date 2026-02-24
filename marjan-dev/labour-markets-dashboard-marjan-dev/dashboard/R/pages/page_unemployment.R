# R/pages/page_unemployment.R

source("R/pages/labour_market_helpers.R")

unemployment_age_codes <- data.frame(
  age_group = AGE_CHOICES,
  level_code = c("MGSC", "LF2I", "YBVH", "YBVN", "YCGM", "YCGS", "LF28", "K5HU"),
  rate_code  = c("MGSX", "LF2Q", "YBVK", "YBVQ", "YCGP", "YCGV", "LF2E", "K5HW"),
  stringsAsFactors = FALSE
)

stacked_unemployment_codes <- data.frame(
  age_group = AGE_STACK,
  code = c("YBVH", "YBVN", "YCGM", "YCGS", "LF28", "K5HU"),
  stringsAsFactors = FALSE
)

#UI & Server
unemployment_ui <- function(id) {
  labour_metric_ui(id, "Unemployment", "#d4351c", "#f47738")
}

unemployment_server <- function(id) {
  labour_metric_server(id, "Unemployment", unemployment_age_codes, stacked_unemployment_codes,
                       "#d4351c", "#f47738", invert = TRUE)
}