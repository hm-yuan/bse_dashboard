build_semantic_data <- function(standard_data) {
  list(
    home = list(kpis = calc_home_overview_kpis(standard_data), insights = calc_home_overview_insights(standard_data)),
    market_position = list(
      kpis = calc_market_position_kpis(standard_data),
      insights = calc_market_position_insights(standard_data),
      table = utils::head(calc_market_company_detail(standard_data), 12)
    ),
    company_profile = list(
      kpis = calc_company_profile_kpis(standard_data),
      insights = calc_company_profile_insights(standard_data),
      table = utils::head(calc_company_detail(standard_data), 12)
    ),
    market_development = list(
      kpis = calc_development_kpis(standard_data),
      insights = calc_development_insights(standard_data),
      table = utils::head(calc_development_detail(standard_data), 12)
    ),
    market_quality = list(
      kpis = calc_quality_kpis(standard_data),
      insights = calc_quality_insights(standard_data),
      table = utils::head(calc_quality_company_detail(standard_data), 12)
    )
  )
}

build_semantic_page_models <- function(standard_data, config = load_page_blocks()) {
  models <- build_placeholder_page_models(config)
  semantic <- build_semantic_data(standard_data)
  for (page_id in intersect(names(models), names(semantic))) {
    source <- semantic[[page_id]]
    if (is.data.frame(source$kpis) && nrow(source$kpis) > 0L) models[[page_id]]$kpis <- source$kpis
    if (length(source$insights) > 0L) models[[page_id]]$insights <- source$insights
    if (is.data.frame(source$table) && nrow(source$table) > 0L) models[[page_id]]$table <- source$table
    models[[page_id]]$mode <- "semantic"
  }
  models
}

build_dashboard_page_models <- function(standard_data, mode = dashboard_presentation_mode(), config = load_page_blocks()) {
  if (identical(mode, "semantic")) build_semantic_page_models(standard_data, config) else build_placeholder_page_models(config)
}
