user_library <- Sys.getenv("R_LIBS_USER")
if (nzchar(user_library) && dir.exists(user_library)) {
  .libPaths(unique(c(user_library, .libPaths())))
}

options(shiny.autoreload = TRUE)

source("R/sample_data.R", encoding = "UTF-8")
source("R/data_access.R", encoding = "UTF-8")
source("R/metrics_market.R", encoding = "UTF-8")
source("R/metrics_company.R", encoding = "UTF-8")
source("R/metrics_development.R", encoding = "UTF-8")
source("R/metrics_quality.R", encoding = "UTF-8")
source("R/placeholder_data.R", encoding = "UTF-8")
source("R/semantic_data.R", encoding = "UTF-8")
source("R/ui_components.R", encoding = "UTF-8")
source("R/chart_market.R", encoding = "UTF-8")
source("R/chart_company.R", encoding = "UTF-8")
source("R/chart_development.R", encoding = "UTF-8")
source("R/chart_quality.R", encoding = "UTF-8")
source("R/placeholder_charts.R", encoding = "UTF-8")
source("R/mod_market_position.R", encoding = "UTF-8")
source("R/mod_company_profile.R", encoding = "UTF-8")
source("R/mod_market_development.R", encoding = "UTF-8")
source("R/mod_market_quality.R", encoding = "UTF-8")

app_theme <- bslib::bs_theme(
  version = 5,
  primary = "#005BAC",
  bg = "#F3F6FA",
  fg = "#0B2A5B",
  "border-radius" = "12px"
)

dashboard_data <- load_dashboard_data()
dashboard_page_models <- build_dashboard_page_models(
  standard_data = dashboard_data,
  mode = dashboard_presentation_mode()
)
