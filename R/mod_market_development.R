marketDevelopmentUI <- function(id) {
  kpis <- calc_development_kpis(dashboard_data)
  listing_trend <- calc_listing_financing_trend(dashboard_data)
  trading_trend <- calc_trading_ecosystem_trend(dashboard_data)
  insights <- calc_development_insights(dashboard_data)
  detail <- utils::head(calc_development_detail(dashboard_data), 12)

  dashboard_sheet(
    class = "market-development-page",
    page_header("市场发展画像", "沿上市、融资、交易和生态四条主线，观察市场功能的阶段性增强。", section = "03  时间推进", meta = "发展动能视角"),
    kpi_grid(kpis),
    development_timeline(listing_trend),
    dashboard_grid(
      class = "main-grid main-grid-equal",
      chart_card("上市扩容和融资功能是否同步增强？", "柱状图展示新增上市，折线展示 IPO 与再融资金额。", plot_listing_financing_trend(listing_trend), class = "chart-card-main"),
      chart_card("交易活跃度和市场生态是否改善？", "观察成交金额、活跃公司、平均换手率和关键事件。", plot_trading_ecosystem_trend(trading_trend), class = "chart-card-main")
    ),
    dashboard_grid(
      class = "bottom-grid bottom-grid-compact",
      insight_card(insights),
      summary_card("发展动能", c("上市扩容和在审储备形成阶段性支撑。", "交易生态改善需要结合产品和做市机制跟踪。")),
      detail_card("年度发展明细", detail, "按年度汇总上市数量、融资金额、活跃公司与生态事件。")
    )
  )
}

marketDevelopmentServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}
