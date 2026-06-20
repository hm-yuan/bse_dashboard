marketQualityUI <- function(id) {
  kpis <- calc_quality_kpis(dashboard_data)
  quality_matrix <- calc_quality_status_matrix(dashboard_data)
  risk_heatmap <- calc_risk_industry_heatmap(dashboard_data)
  insights <- calc_quality_insights(dashboard_data)
  detail <- utils::head(calc_quality_company_detail(dashboard_data), 12)

  dashboard_sheet(
    class = "market-quality-page",
    page_header("市场质量画像", "以流动性、估值、财务、合规和退市风险构建持续监测与重点追踪界面。", section = "04  风险驾驶舱", meta = "风险监测视角"),
    kpi_grid(kpis, class = "quality-kpi-grid"),
    dashboard_grid(
      class = "main-grid main-grid-wide-left",
      chart_card("哪些公司处于流动性和基本面双重观察区？", "横轴为流动性，纵轴为基本面质量，颜色代表风险等级。", plot_quality_status_matrix(quality_matrix), class = "chart-card-main"),
      chart_card("风险主要集中在哪些行业和类型？", "颜色越深代表风险公司数量越多，高风险以红色强化。", plot_risk_industry_heatmap(risk_heatmap), class = "chart-card-side")
    ),
    dashboard_grid(
      class = "bottom-grid bottom-grid-compact",
      insight_card(insights),
      summary_card("风险跟踪", c("低流动性仍是质量监测的首要维度。", "监管措施数量上升，应补充处置进展跟踪。")),
      detail_card("重点风险公司明细", detail, "展示公司、风险类型、风险等级和关键风险原因。")
    )
  )
}

marketQualityServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}
