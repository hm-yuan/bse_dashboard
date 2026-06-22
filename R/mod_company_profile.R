# 用途：从公司画像页面模型中按图表类型提取内容块。
# 输入来源：`dashboard_page_models$company_profile$charts`。
company_profile_chart_block <- function(type) {
  model <- dashboard_page_models$company_profile
  blocks <- Filter(function(block) identical(block$type, type), model$charts)
  if (length(blocks) == 0L) return(NULL)
  blocks[[1L]]
}

# 用途：公司画像 UI 模块函数，按“地图+矩阵 / 四象限+洞察+行业树图”组织三行布局。
# 输入来源：`id` 参数（Shiny 模块命名空间）和页面模型中的内容块配置。
companyProfileUI <- function(id) {
  ns <- shiny::NS(id)
  model <- dashboard_page_models$company_profile
  map_block <- company_profile_chart_block("company_geo_map")
  heatmap_block <- company_profile_chart_block("company_heatmap")
  quadrant_block <- company_profile_chart_block("company_quadrant")

  dashboard_sheet(
    class = "portrait-page company-profile-page",
    hero_card(model$judgment),
    kpi_grid(model$kpis),
    dashboard_grid(
      class = "chart-grid company-profile-main-grid",
      if (!is.null(map_block)) chart_card(map_block),
      if (!is.null(heatmap_block)) chart_card(heatmap_block)
    ),
    dashboard_grid(
      class = "bottom-grid company-profile-bottom-grid",
      if (!is.null(quadrant_block)) chart_card(quadrant_block),
      market_industry_distribution_card(ns),
      market_industry_treemap_card(ns)
    )
  )
}

# 用途：公司画像服务端模块函数，处理行业矩形树图的点击、返回和单位切换。
# 输入来源：行业色块点击事件、按钮事件和全局 `dashboard_data`。
companyProfileServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    selected_industry <- shiny::reactiveVal(NULL)
    size_by <- shiny::reactiveVal("count")

    shiny::observeEvent(input$industry_treemap_click, {
      selected_industry(as.character(input$industry_treemap_click))
    }, ignoreNULL = TRUE)

    shiny::observeEvent(input$treemap_back, {
      selected_industry(NULL)
    })

    shiny::observeEvent(input$treemap_unit, {
      next_unit <- if (identical(size_by(), "count")) "market_cap" else "count"
      size_by(next_unit)
      shiny::updateActionButton(
        session,
        "treemap_unit",
        label = if (identical(next_unit, "count")) "切换为公司市值" else "切换为公司家数"
      )
    })

    output$treemap_state <- shiny::renderText({
      selected <- selected_industry()
      level <- if (is.null(selected)) "行业分布" else paste0("公司明细：", if (identical(selected, "__other__")) "其他行业" else selected)
      unit <- if (identical(size_by(), "count")) "按公司家数" else "按公司市值"
      paste(level, "·", unit)
    })

    output$industry_treemap <- highcharter::renderHighchart({
      plot_market_industry_treemap_drill(
        data = dashboard_data,
        selected_industry = selected_industry(),
        size_by = size_by(),
        click_input_id = session$ns("industry_treemap_click")
      )
    })

    output$industry_distribution_chart <- highcharter::renderHighchart({
      plot_market_industry_distribution(
        calc_market_industry_distribution(input$market_select, input$industry_metric),
        metric = input$industry_metric
      )
    })
  })
}
