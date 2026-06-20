homeUI <- function(id) {
  home_kpis <- calc_home_overview_kpis(dashboard_data)
  home_insights <- calc_home_overview_insights(dashboard_data)

  home_detail <- data.frame(
    画像 = c("市场定位", "上市公司", "市场发展", "市场质量"),
    当前关注 = c("规模、活跃度与行业集中度", "成长性、盈利质量与创新属性", "上市融资与交易生态", "流动性、估值、财务与合规风险"),
    主要信号 = c("总市值和成交额", "盈利公司占比和研发费用率", "新增上市和 IPO 融资", "高估值低增长与连续亏损"),
    check.names = FALSE
  )

  dashboard_sheet(
    class = "home-page",
    page_header("交易所画像总览", "从市场定位、公司结构、发展动能到质量风险，形成一页式观察入口。", meta = "数据更新：当前 processed 批次"),
    kpi_grid(home_kpis),
    dashboard_grid(
      class = "home-entry-grid",
      portrait_entry_card("市场定位画像", "对标多层次资本市场，识别本所规模、活跃度与估值位置。", href = "#shiny-tab-market_position"),
      portrait_entry_card("上市公司画像", "观察行业结构、经营贡献、成长性与盈利能力分布。", href = "#shiny-tab-company_profile"),
      portrait_entry_card("市场发展画像", "跟踪上市融资、交易活跃度与市场生态持续变化。", href = "#shiny-tab-market_development"),
      portrait_entry_card("市场质量画像", "聚焦流动性、财务、合规和退市风险的集中区域。", href = "#shiny-tab-market_quality")
    ),
    dashboard_grid(
      class = "home-bottom-grid",
      insight_card(home_insights),
      summary_card(
        "本期重点变化",
        c("新增上市公司与交易活跃度同步改善。", "盈利公司占比提升，研发投入维持较高水平。", "低流动性风险缓解但仍需持续跟踪。")
      ),
      detail_card("画像钻取入口", home_detail, "从总览进入四个画像页面，查看对应指标、主图与公司明细。")
    )
  )
}

homeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}
