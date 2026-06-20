page_shell <- function(...) div(class = "page-shell", ...)

dashboard_sheet <- function(..., class = NULL) {
  div(class = paste(c("dashboard-sheet", class), collapse = " "), ...)
}

dashboard_grid <- function(..., class = NULL) {
  div(class = paste(c("dashboard-grid", class), collapse = " "), ...)
}

page_header <- function(title, subtitle = NULL, section = NULL, meta = "演示数据") {
  div(
    class = "page-header",
    div(class = "page-kicker", section),
    div(class = "page-title-row", h1(title), span(class = "page-meta", meta)),
    if (!is.null(subtitle)) p(class = "page-subtitle", subtitle)
  )
}

hero_card <- function(text, label = "核心判断") {
  div(class = "hero-card", div(class = "hero-label", label), div(class = "hero-text", text))
}

kpi_card <- function(label, value, unit = NULL, change = NULL, status = "neutral") {
  div(
    class = paste("kpi-card", paste0("kpi-", status)),
    div(class = "kpi-label", label),
    div(class = "kpi-value-row", span(class = "kpi-value", value), if (!is.null(unit)) span(class = "kpi-unit", unit)),
    if (!is.null(change)) div(class = "kpi-change", change)
  )
}

kpi_grid <- function(kpis) {
  div(
    class = "kpi-grid",
    lapply(seq_len(nrow(kpis)), function(i) kpi_card(kpis$label[[i]], kpis$value[[i]], kpis$unit[[i]], kpis$change[[i]], kpis$status[[i]]))
  )
}

status_badge <- function(label, status = "neutral") {
  span(class = paste("status-badge", paste0("status-", status)), label)
}

risk_badge <- function(level) {
  status <- switch(as.character(level), "低" = "success", "中" = "warning", "高" = "danger", "neutral")
  status_badge(level, status)
}

card_heading <- function(title, note = NULL) {
  div(class = "card-heading", h3(title), if (!is.null(note)) p(note))
}

chart_empty_state <- function(message = "暂无可展示内容") {
  div(class = "chart-empty-state", div(class = "placeholder-title", message), div(class = "placeholder-note", "演示内容将在此位置展示。"))
}

chart_widget <- function(widget) div(class = "chart-widget", widget)

chart_fallback_table <- function(title, data, note = "当前环境未启用 highcharter。") {
  detail_card(title, if (is.data.frame(data)) utils::head(data, 8) else data.frame(提示 = note, check.names = FALSE), note)
}

chart_card <- function(block) {
  widget <- render_placeholder_chart(block$type)
  div(class = paste("content-card", "chart-card", paste0("chart-span-", block$span %||% "equal")), card_heading(block$title, block$note), div(class = "chart-content", chart_widget(widget)), div(class = "card-footnote", "演示数据，待接入业务口径"))
}

insight_card <- function(items) {
  div(
    class = "content-card insight-card",
    card_heading("关键洞察", "用于说明图表的后续分析方向"),
    div(class = "insight-list", lapply(seq_along(items), function(i) div(class = "insight-item", span(class = "insight-index", sprintf("%02d", i)), span(items[[i]]))))
  )
}

summary_card <- function(title, items) {
  div(class = "content-card summary-card", card_heading(title), div(class = "summary-list", lapply(items, function(item) div(class = "summary-item", span(class = "summary-dot"), span(item)))))
}

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

detail_card <- function(title, data, note = NULL) {
  div(class = "content-card detail-card", card_heading(title, note), div(class = "table-wrap", detail_table(data)), div(class = "card-footnote", "演示数据，待接入业务口径"))
}

portrait_entry_card <- function(title, text, href) {
  tags$a(class = "portrait-entry-card", href = href, div(class = "portrait-entry-title", title), div(class = "portrait-entry-text", text), span(class = "portrait-entry-action", "查看画像"))
}

home_entry_cards <- function() {
  items <- list(
    list("市场定位画像", "对标规模、活跃度和行业市值结构", "#shiny-tab-market_position"),
    list("上市公司画像", "经营贡献、成长性与盈利能力分布", "#shiny-tab-company_profile"),
    list("市场发展画像", "上市融资、交易活跃与市场生态变化", "#shiny-tab-market_development"),
    list("市场质量画像", "流动性、财务、合规与退市风险观察", "#shiny-tab-market_quality")
  )
  div(class = "portrait-entry-grid", lapply(items, function(item) portrait_entry_card(item[[1]], item[[2]], item[[3]])))
}

page_model_ui <- function(page_id) {
  model <- dashboard_page_models[[page_id]]
  if (is.null(model)) return(div(class = "page-shell", "页面模型未加载。"))

  common <- list(
    page_header(model$title, model$subtitle, model$section, if (identical(model$mode, "semantic")) "数据模式：semantic" else "数据模式：演示占位"),
    hero_card(model$judgment),
    kpi_grid(model$kpis)
  )

  if (identical(page_id, "home")) {
    return(dashboard_sheet(class = "home-page", c(common, list(home_entry_cards(), dashboard_grid(class = "bottom-grid", insight_card(model$insights), summary_card("页面结构", model$summary), detail_card(model$table_title, model$table, model$table_note))))))
  }

  chart_cards <- lapply(model$charts, chart_card)
  dashboard_sheet(
    class = paste("portrait-page", paste0("layout-", model$layout)),
    c(common, list(
      dashboard_grid(class = "chart-grid", chart_cards),
      dashboard_grid(class = "bottom-grid", insight_card(model$insights), summary_card("内容要素", model$summary), detail_card(model$table_title, model$table, model$table_note))
    ))
  )
}
