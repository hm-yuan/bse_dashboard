# 用途：创建页面外层容器，包裹单页全部内容。
# 输入来源：`...` 子元素参数。
page_shell <- function(...) div(class = "page-shell", ...)

# 用途：创建仪表板内容面板，可附加自定义样式类。
# 输入来源：`...` 子元素参数、`class` 参数。
dashboard_sheet <- function(..., class = NULL) {
  div(class = paste(c("dashboard-sheet", class), collapse = " "), ...)
}

# 用途：创建网格布局容器，用于排列卡片等组件。
# 输入来源：`...` 子元素参数、`class` 参数。
dashboard_grid <- function(..., class = NULL) {
  div(class = paste(c("dashboard-grid", class), collapse = " "), ...)
}

# 用途：创建顶部核心判断卡片，展示页面主标题与核心观点。
# 输入来源：`text` 参数、`label` 参数。
hero_card <- function(text, label = "核心判断") {
  div(class = "hero-card", div(class = "hero-label", label), div(class = "hero-text", text))
}

# 用途：创建单个 KPI 指标卡片，展示指标名称、数值、单位与变动。
#       主数字使用 3 秒数字翻牌器动画。
# 输入来源：`label`、`value`、`unit`、`change`、`status` 参数。
kpi_card <- function(label, value, unit = NULL, change = NULL, status = "neutral") {
  numeric_value <- kpi_parse_flip_value(value)
  has_numeric <- is.finite(numeric_value)

  div(
    class = paste("kpi-card", paste0("kpi-", status)),
    div(class = "kpi-label", label),
    div(
      class = "kpi-value-row",
      span(class = "kpi-value", `data-target` = if (has_numeric) numeric_value else NA_real_, value),
      if (!is.null(unit)) span(class = "kpi-unit", unit)
    ),
    if (!is.null(change)) div(class = "kpi-change", change)
  )
}

# 用途：解析 KPI 主数字字符串为原始数值，供前端翻牌器动画使用。
#       仅去除千分位逗号并尝试转换为数值；单位为“%”时不再额外乘 100，
#       因为数据已按百分比展示。
# 输入来源：`value`（展示字符串）。
kpi_parse_flip_value <- function(value) {
  raw <- suppressWarnings(as.numeric(gsub(",", "", as.character(value))))
  if (!is.finite(raw)) return(NA_real_)
  raw
}

# 用途：根据页面标识生成 KPIboard 左侧语义标题。
# 输入来源：`page_id` 与 `model$title`。
kpi_board_label <- function(page_id, model = NULL) {
  switch(
    page_id,
    market_position = "主阵地",
    company_profile = "创新型 ",
    market_development = "生态",
    market_quality = "质量",
    if (!is.null(model$title)) model$title else "画像"
  )
}

# 用途：根据 KPI 数据框批量渲染 KPIboard。
# 输入来源：`kpis` 参数（数据框）、`board_label` 左侧语义标题。
kpi_grid <- function(kpis, board_label = NULL) {
  metric_items <- lapply(seq_len(nrow(kpis)), function(i) {
    kpi_card(kpis$label[[i]], kpis$value[[i]], kpis$unit[[i]], kpis$change[[i]], kpis$status[[i]])
  })

  if (is.null(board_label)) {
    return(div(class = "kpi-grid", metric_items))
  }

  div(
    class = paste("kpi-board", paste0("metric-count-", nrow(kpis))),
    div(class = "kpi-board-title", board_label),
    div(class = "kpi-board-metrics kpi-grid", metric_items)
  )
}

# 用途：创建状态徽章，用于展示文本状态标签。
# 输入来源：`label` 参数、`status` 参数。
status_badge <- function(label, status = "neutral") {
  span(class = paste("status-badge", paste0("status-", status)), label)
}

# 用途：根据风险等级文本（低/中/高）映射为对应样式徽章。
# 输入来源：`level` 参数。
risk_badge <- function(level) {
  status <- switch(as.character(level), "低" = "success", "中" = "warning", "高" = "danger", "neutral")
  status_badge(level, status)
}

# 用途：创建卡片标题区域，包含主标题与可选说明文字。
# 输入来源：`title` 参数、`note` 参数。
card_heading <- function(title, note = NULL) {
  div(class = "card-heading", h3(title), if (!is.null(note)) p(note))
}

# 用途：创建图表空状态提示，用于数据缺失或占位场景。
# 输入来源：`message` 参数。
chart_empty_state <- function(message = "暂无可展示内容") {
  div(class = "chart-empty-state", div(class = "placeholder-title", message), div(class = "placeholder-note", "演示内容将在此位置展示。"))
}

# 用途：将图表组件包装为统一的 chart-widget 容器。
# 输入来源：`widget` 参数。
chart_widget <- function(widget) div(class = "chart-widget", widget)

# 用途：当 highcharter 不可用时，以表格形式展示图表数据。
# 输入来源：`title` 参数、`data` 参数、`note` 参数。
chart_fallback_table <- function(title, data, note = "当前环境未启用 highcharter。") {
  detail_card(title, if (is.data.frame(data)) utils::head(data, 8) else data.frame(提示 = note, check.names = FALSE), note)
}

# 用途：根据图表类型调度对应的数据计算与绘图函数。
# 输入来源：`type` 参数、全局对象 `dashboard_data`。
render_data_chart <- function(type, data = dashboard_data) {
  switch(type,
    market_bubble = plot_market_position_bubble(calc_market_position_bubble(data)),
    industry_treemap = plot_market_industry_treemap(data),
    company_geo_map = plot_company_geography_map(
      calc_company_geography(data),
      bse_geo = calc_company_geography(data, board_filter = c("北证", "北交所"))
    ),
    company_heatmap = plot_company_industry_matrix(calc_company_industry_contribution(data)),
    company_quadrant = plot_company_quality_quadrant(calc_company_quality_quadrant(data)),
    listing_financing = plot_listing_financing_trend(calc_listing_financing_trend(data)),
    trading_ecosystem = plot_index_trend(calc_index_trend(data)),
    quality_matrix = plot_quality_status_matrix(calc_quality_status_matrix(data)),
    risk_heatmap = plot_risk_industry_heatmap(calc_risk_industry_heatmap(data)),
    company_revenue_profit = plot_company_revenue_profit_scatter(calc_company_revenue_profit_scatter(data)),
    render_placeholder_chart(type)
  )
}

# 用途：根据页面块配置创建图表卡片，包含标题与图表。
# 输入来源：`block` 参数（页面块配置对象）。
chart_card <- function(block) {
  widget <- render_data_chart(block$type)
  div(class = paste("content-card", "chart-card", paste0("chart-span-", block$span %||% "equal")), card_heading(block$title, block$note), div(class = "chart-content", chart_widget(widget)))
}

# 用途：创建关键洞察列表卡片。
# 输入来源：`items` 参数（洞察文本向量）。
insight_card <- function(items) {
  div(
    class = "content-card insight-card",
    card_heading("关键洞察", "用于说明图表的后续分析方向"),
    div(class = "insight-list", lapply(seq_along(items), function(i) div(class = "insight-item", span(class = "insight-index", sprintf("%02d", i)), span(items[[i]]))))
  )
}

# 用途：创建内容摘要列表卡片。
# 输入来源：`title` 参数、`items` 参数（摘要文本向量）。
summary_card <- function(title, items) {
  div(class = "content-card summary-card", card_heading(title), div(class = "summary-list", lapply(items, function(item) div(class = "summary-item", span(class = "summary-dot"), span(item)))))
}

# 用途：将数据框渲染为带数字右对齐与状态徽章的明细表格。
# 输入来源：`data` 参数（数据框）。
detail_table <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0L) data <- data.frame(提示 = "暂无明细", check.names = FALSE)
  numeric_columns <- vapply(data, is.numeric, logical(1))
  tags$table(
    class = "detail-table",
    tags$thead(tags$tr(lapply(names(data), tags$th))),
    tags$tbody(lapply(seq_len(nrow(data)), function(i) {
      tags$tr(lapply(seq_along(data), function(j) {
        column <- names(data)[[j]]
        value <- data[[j]][[i]]
        content <- if (column %in% c("风险等级", "risk_level")) risk_badge(value) else if (column %in% c("展示状态", "处置状态", "status")) status_badge(value) else as.character(value)
        tags$td(class = if (numeric_columns[[j]]) "numeric-cell" else NULL, content)
      }))
    }))
  )
}

# 用途：创建带标题的明细数据卡片。
# 输入来源：`title` 参数、`data` 参数、`note` 参数。
detail_card <- function(title, data, note = NULL) {
  div(class = "content-card detail-card", card_heading(title, note), div(class = "table-wrap", detail_table(data)))
}

# 用途：创建画像入口链接卡片，用于首页跳转到各画像页。
# 输入来源：`title` 参数、`text` 参数、`href` 参数。
portrait_entry_card <- function(title, text, href) {
  tags$a(class = "portrait-entry-card", href = href, div(class = "portrait-entry-title", title), div(class = "portrait-entry-text", text), span(class = "portrait-entry-action", "查看画像"))
}

home_entry_cards <- function() {
  items <- list(
    list("市场定位", "对标规模、活跃度和行业市值结构", "#shiny-tab-market_position"),
    list("公司画像", "经营贡献、成长性与盈利能力分布", "#shiny-tab-company_profile"),
    list("市场生态", "上市融资、交易活跃与市场生态变化", "#shiny-tab-market_development"),
    list("市场质量画像", "流动性、财务、合规与退市风险观察", "#shiny-tab-market_quality")
  )
  div(class = "portrait-entry-grid", lapply(items, function(item) portrait_entry_card(item[[1]], item[[2]], item[[3]])))
}

# 用途：根据页面模型渲染完整页面 UI，包括核心判断、KPI、图表与明细。
# 输入来源：`page_id` 参数、全局对象 `dashboard_page_models`、
#         `exclude_charts`（要排除的图表类型向量）、
#         `extra_chart_cards`（要追加到 chart-grid 的额外卡片列表）。
page_model_ui <- function(page_id, exclude_charts = NULL, extra_chart_cards = list(), bottom_left = NULL, bottom_right = NULL, bottom_items = NULL, exclude_summary = FALSE) {
  model <- dashboard_page_models[[page_id]]
  if (is.null(model)) return(div(class = "page-shell", "页面模型未加载。"))

  common <- list(
    kpi_grid(model$kpis, kpi_board_label(page_id, model))
  )

  if (identical(page_id, "home")) {
    return(dashboard_sheet(class = "home-page", c(common, list(home_entry_cards(), dashboard_grid(class = "bottom-grid", insight_card(model$insights), summary_card("页面结构", model$summary), detail_card(model$table_title, model$table, model$table_note))))))
  }

  chart_cards <- lapply(model$charts, function(block) {
    if (!is.null(exclude_charts) && block$type %in% exclude_charts) return(NULL)
    chart_card(block)
  })
  chart_cards <- Filter(Negate(is.null), chart_cards)
  chart_cards <- c(chart_cards, extra_chart_cards)

  if (!is.null(bottom_items)) {
    bottom_grid <- if (length(bottom_items) > 0L) {
      do.call(dashboard_grid, c(list(class = "bottom-grid bottom-grid-custom"), bottom_items))
    } else {
      NULL
    }
  } else {
    bottom_left_card <- if (!is.null(bottom_left)) bottom_left else insight_card(model$insights)
    bottom_right_card <- if (!is.null(bottom_right)) bottom_right else detail_card(model$table_title, model$table, model$table_note)

    if (isTRUE(exclude_summary)) {
      bottom_grid <- dashboard_grid(class = "bottom-grid bottom-grid-two-col", bottom_left_card, bottom_right_card)
    } else {
      bottom_grid <- dashboard_grid(class = "bottom-grid", bottom_left_card, summary_card("内容要素", model$summary), bottom_right_card)
    }
  }

  dashboard_sheet(
    class = paste("portrait-page", paste0("layout-", model$layout)),
    c(common, list(dashboard_grid(class = "chart-grid", chart_cards)), if (!is.null(bottom_grid)) list(bottom_grid) else list())
  )
}
