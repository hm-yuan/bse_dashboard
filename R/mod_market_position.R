marketPositionUI <- function(id) {
  kpis <- calc_market_position_kpis(dashboard_data)
  bubble <- calc_market_position_bubble(dashboard_data)
  industry_structure <- calc_market_industry_structure(dashboard_data)
  insights <- calc_market_position_insights(dashboard_data)
  detail <- utils::head(calc_market_company_detail(dashboard_data), 12)

  dashboard_sheet(
    class = "market-position-page",
    page_header("市场定位画像", "以规模、活跃度、估值和产业结构识别本所在多层次资本市场中的位置。", section = "01  对标分析", meta = "市场对标视角"),
    kpi_grid(kpis),
    dashboard_grid(
      class = "main-grid main-grid-wide-left",
      chart_card("本所处于怎样的规模与活跃度位置？", "横轴为总市值，纵轴为日均成交额，气泡大小代表上市公司数量。", plot_market_position_bubble(bubble), class = "chart-card-main"),
      chart_card("本所市值主要集中在哪些行业？", "按行业汇总公司市值，解释市场定位背后的产业结构。", plot_market_industry_structure(industry_structure), class = "chart-card-side")
    ),
    dashboard_grid(
      class = "bottom-grid bottom-grid-compact",
      insight_card(insights),
      summary_card("定位解读", c("本所气泡在成长型市场区间内保持清晰识别。", "制造业和信息技术行业贡献主要市值与成交。")),
      detail_card("公司市值明细", detail, "当前展示市值排名靠前公司，后续可扩展筛选和钻取。")
    )
  )
}

marketPositionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}
