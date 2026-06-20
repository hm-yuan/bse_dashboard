companyProfileUI <- function(id) {
  kpis <- calc_company_profile_kpis(dashboard_data)
  contribution <- calc_company_industry_contribution(dashboard_data)
  quadrant <- calc_company_quality_quadrant(dashboard_data)
  insights <- calc_company_profile_insights(dashboard_data)
  detail <- utils::head(calc_company_detail(dashboard_data), 12)

  dashboard_sheet(
    class = "company-profile-page",
    page_header("上市公司画像", "从经营贡献、成长性和创新投入，识别本所服务企业的群体特征。", section = "02  公司画像", meta = "经营质量视角"),
    kpi_grid(kpis),
    dashboard_grid(
      class = "main-grid company-main-grid",
      chart_card("哪些行业贡献了主要经营与研发？", "矩阵对比公司数、市值、营收、净利润和研发费用占比。", plot_company_industry_matrix(contribution), class = "chart-card-main"),
      chart_card("哪些公司同时具备成长性和盈利能力？", "横轴为营收增长，纵轴为 ROE，气泡大小代表市值。", plot_company_quality_quadrant(quadrant), class = "chart-card-main")
    ),
    dashboard_grid(
      class = "bottom-grid bottom-grid-compact",
      insight_card(insights),
      summary_card("公司结构观察", c("制造业贡献较高，是当前公司画像主线。", "成长性与盈利能力分化明显，适合四象限表达。")),
      detail_card("公司经营明细", detail, "当前展示营收排名靠前公司，后续可扩展到公司画像钻取。")
    )
  )
}

companyProfileServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}
