page_shell <- function(...) {
  div(class = "page-shell", ...)
}

dashboard_sheet <- function(..., class = NULL) {
  div(class = paste(c("dashboard-sheet", class), collapse = " "), ...)
}

dashboard_row <- function(..., class = NULL) {
  div(class = paste(c("dashboard-row", class), collapse = " "), ...)
}

dashboard_grid <- function(..., class = NULL) {
  div(class = paste(c("dashboard-grid", class), collapse = " "), ...)
}

page_header <- function(title, subtitle = NULL, section = "BSE-Dashboard", meta = NULL) {
  div(
    class = "page-header",
    div(class = "page-kicker", section),
    div(
      class = "page-title-row",
      h1(title),
      if (!is.null(meta)) span(class = "page-meta", meta)
    ),
    if (!is.null(subtitle)) p(class = "page-subtitle", subtitle)
  )
}

hero_card <- function(text, label = "核心判断") {
  div(
    class = "hero-card",
    div(class = "hero-label", label),
    div(class = "hero-text", text)
  )
}

kpi_icon_name <- function(label) {
  if (grepl("上市|公司|在审|活跃", label)) return("building")
  if (grepl("市值|融资|营收|利润|成交", label)) return("chart-line")
  if (grepl("PE|ROE|研发|募集|换手", label)) return("gauge-high")
  if (grepl("风险|亏损|监管|异常|退市", label)) return("shield-halved")
  "chart-column"
}

kpi_card <- function(label, value, unit = NULL, change = NULL, status = "neutral") {
  div(
    class = paste("kpi-card", paste0("kpi-", status)),
    div(
      class = "kpi-card-top",
      div(class = "kpi-icon", shiny::icon(kpi_icon_name(label))),
      div(class = "kpi-label", label)
    ),
    div(
      class = "kpi-value-row",
      span(class = "kpi-value", value),
      if (!is.null(unit)) span(class = "kpi-unit", unit)
    ),
    if (!is.null(change)) div(class = "kpi-change", change)
  )
}

kpi_grid <- function(kpis, class = NULL) {
  cards <- lapply(seq_len(nrow(kpis)), function(i) {
    kpi_card(
      label = kpis$label[[i]],
      value = kpis$value[[i]],
      unit = kpis$unit[[i]],
      change = kpis$change[[i]],
      status = kpis$status[[i]]
    )
  })

  div(class = paste(c("kpi-grid", class), collapse = " "), cards)
}

section_title <- function(title, note = NULL) {
  div(
    class = "section-title",
    h2(title),
    if (!is.null(note)) span(note)
  )
}

status_badge <- function(label, status = "neutral") {
  span(class = paste("status-badge", paste0("status-", status)), label)
}

risk_badge <- function(level) {
  status <- switch(as.character(level), "低" = "success", "中" = "warning", "高" = "danger", "neutral")
  span(class = paste("risk-badge", paste0("risk-", status)), as.character(level))
}

chart_card <- function(title, note = NULL, placeholder = "主图占位", class = NULL) {
  has_chart_content <- !is.character(placeholder)
  placeholder_title <- if (!has_chart_content && grepl("占位", placeholder, fixed = TRUE)) "图表待接入" else placeholder

  div(
    class = paste(c("chart-card", class), collapse = " "),
    div(
      class = "card-heading",
      h3(title),
      if (!is.null(note)) p(note)
    ),
    if (has_chart_content) {
      div(class = "chart-content", placeholder)
    } else {
      div(
        class = "chart-placeholder",
        div(class = "placeholder-mark", ""),
        div(class = "placeholder-title", placeholder_title),
        div(class = "placeholder-note", "后续阶段接入真实图表函数和指标计算。")
      )
    }
  )
}

chart_fallback_table <- function(title, data, note = "当前环境未启用 highcharter，已显示简化数据表。") {
  rows <- if (is.data.frame(data) && nrow(data) > 0L) utils::head(data, 8) else data.frame(提示 = "暂无可展示数据", check.names = FALSE)

  div(
    class = "chart-fallback",
    div(class = "chart-fallback-title", title),
    div(class = "chart-fallback-note", note),
    detail_table(rows)
  )
}

chart_empty_state <- function(message = "暂无可展示数据") {
  div(
    class = "chart-empty-state",
    div(class = "placeholder-mark", ""),
    div(class = "placeholder-title", message),
    div(class = "placeholder-note", "请检查 metrics 输出数据。")
  )
}

chart_widget <- function(widget) {
  div(class = "chart-widget", widget)
}

insight_card <- function(items) {
  div(
    class = "insight-card",
    div(class = "card-heading", h3("关键洞察")),
    div(
      class = "insight-list",
      lapply(seq_along(items), function(i) {
        insight_title <- sub("[，,。].*$", "", items[[i]])
        div(
          class = "insight-item",
          span(class = "insight-index", sprintf("%02d", i)),
          span(
            class = "insight-copy",
            span(class = "insight-title", insight_title),
            span(class = "insight-text", items[[i]])
          )
        )
      })
    )
  )
}

summary_card <- function(title, items) {
  div(
    class = "summary-card",
    div(class = "card-heading", h3(title)),
    div(
      class = "summary-list",
      lapply(items, function(item) {
        div(
          class = "summary-item",
          span(class = "summary-dot", ""),
          span(class = "summary-text", item)
        )
      })
    )
  )
}

development_timeline <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0L || !all(c("year", "new_listing_count") %in% names(data))) {
    return(NULL)
  }

  points <- data[order(data$year), , drop = FALSE]
  div(
    class = "development-timeline",
    lapply(seq_len(nrow(points)), function(i) {
      amount <- if ("ipo_financing_amount_yi" %in% names(points)) points$ipo_financing_amount_yi[[i]] else NA_real_
      div(
        class = "timeline-step",
        div(class = "timeline-dot", ""),
        div(class = "timeline-year", points$year[[i]]),
        div(class = "timeline-value", paste0(points$new_listing_count[[i]], " 家新增上市")),
        div(class = "timeline-note", if (is.finite(amount)) paste0("IPO ", format(round(amount, 1), trim = TRUE), " 亿元") else "融资观察")
      )
    })
  )
}

portrait_entry_card <- function(title, text, status = "查看画像", href = NULL) {
  content <- div(
    class = "portrait-entry-card-content",
    div(class = "portrait-entry-top", span(title), span(class = "status-pill", status)),
    div(class = "portrait-entry-text", text)
  )

  if (!is.null(href)) {
    return(tags$a(class = "portrait-entry-link", href = href, content))
  }

  div(class = "portrait-entry-card", content)
}

detail_table <- function(data) {
  is_numeric_column <- vapply(data, is.numeric, logical(1))
  header <- tags$tr(lapply(names(data), tags$th))
  rows <- lapply(seq_len(nrow(data)), function(i) {
    cells <- lapply(seq_along(data), function(j) {
      column_name <- names(data)[[j]]
      value <- data[[j]][[i]]
      content <- if (identical(column_name, "risk_level")) {
        risk_badge(value)
      } else if (identical(column_name, "status")) {
        status_badge(value, "neutral")
      } else {
        as.character(value)
      }
      tags$td(
        class = paste(c(if (is_numeric_column[[j]]) "numeric-cell" else NULL, if (column_name %in% c("company_name", "company_code")) "key-cell" else NULL), collapse = " "),
        content
      )
    })
    tags$tr(cells)
  })

  div(
    class = "table-wrap",
    tags$table(class = "detail-table", tags$thead(header), tags$tbody(rows))
  )
}

detail_card <- function(title, data, note = NULL) {
  div(
    class = "detail-card",
    div(
      class = "card-heading",
      h3(title),
      if (!is.null(note)) p(note)
    ),
    detail_table(data)
  )
}

two_column_grid <- function(left, right) {
  div(class = "two-column-grid", left, right)
}
