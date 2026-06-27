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
source("R/process_basic_data.R", encoding = "UTF-8")

# 自动检测：如果 Excel 比 processed CSV 新，自动重新处理
excel_path <- "data/raw/上市公司基本情况.xlsx"
kpi_path   <- "data/processed/market_position_kpi.csv"
if (file.exists(excel_path) &&
    (!file.exists(kpi_path) || file.info(excel_path)$mtime > file.info(kpi_path)$mtime)) {
  message("检测到 Excel 已更新，自动重新生成 processed 数据...")
  tryCatch(process_basic_data(), error = function(e) {
    message("自动处理失败：", e$message, "，将使用旧数据。")
  })
}

app_theme <- bslib::bs_theme(
  version = 5,
  primary = "#005BAC",
  bg = "#F3F7FC",
  fg = "#0B2A5B",
  "border-radius" = "6px"
)

dashboard_data <- load_dashboard_data()
dashboard_page_models <- build_dashboard_page_models(
  standard_data = dashboard_data,
  mode = dashboard_presentation_mode()
)
