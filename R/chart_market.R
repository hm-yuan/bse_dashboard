# Chart helpers are defined here because this file is sourced before the other
# chart modules. Chart functions consume metrics outputs only.

chart_has_highcharter <- function() {
  requireNamespace("highcharter", quietly = TRUE)
}

chart_colors <- function() {
  list(
    bse_blue = "#005BAC",
    bse_blue_soft = "#6EA8DE",
    bse_cyan = "#0E9FB2",
    navy = "#12365D",
    slate = "#7A8A9E",
    grid = "#E6EDF5",
    success = "#248A5A",
    warning = "#D97706",
    danger = "#D64545",
    risk_orange = "#E97539",
    risk_light = "#FCE7D8"
  )
}

chart_safe_number <- function(x) {
  suppressWarnings(as.numeric(x))
}

chart_format_pct <- function(x, digits = 1) {
  value <- chart_safe_number(x)
  ifelse(is.finite(value), paste0(format(round(value * 100, digits), trim = TRUE, nsmall = digits), "%"), "-")
}

chart_top_n <- function(df, column, n = 12L) {
  if (!is.data.frame(df) || !column %in% names(df)) {
    return(df)
  }
  df[order(chart_safe_number(df[[column]]), decreasing = TRUE, na.last = TRUE)[seq_len(min(nrow(df), n))], , drop = FALSE]
}

chart_hc_base <- function(type = NULL, height = 318) {
  colors <- chart_colors()
  highcharter::highchart() |>
    highcharter::hc_chart(
      type = type,
      backgroundColor = "transparent",
      style = list(fontFamily = "'Microsoft YaHei', 'PingFang SC', 'Segoe UI', sans-serif")
    ) |>
    highcharter::hc_size(height = height) |>
    highcharter::hc_title(text = NULL) |>
    highcharter::hc_credits(enabled = FALSE) |>
    highcharter::hc_exporting(enabled = FALSE) |>
    highcharter::hc_add_dependency("highcharts-more.js") |>
    highcharter::hc_add_dependency("modules/heatmap.js") |>
    highcharter::hc_legend(
      itemStyle = list(color = colors$navy, fontSize = "11px", fontWeight = "500"),
      itemHoverStyle = list(color = colors$bse_blue),
      symbolRadius = 2
    ) |>
    highcharter::hc_tooltip(
      backgroundColor = "#FFFFFF",
      borderColor = "#C9D8E8",
      borderRadius = 4,
      shadow = FALSE,
      style = list(color = colors$navy, fontSize = "12px")
    )
}

chart_hc_axis <- function(title, categories = NULL, opposite = FALSE, min = NULL, max = NULL, plot_lines = NULL) {
  colors <- chart_colors()
  axis <- list(
    title = list(text = title, style = list(color = "#60758D", fontSize = "11px", fontWeight = "500")),
    categories = categories,
    opposite = opposite,
    lineColor = "#C8D5E3",
    tickColor = "#C8D5E3",
    gridLineColor = colors$grid,
    labels = list(style = list(color = "#60758D", fontSize = "11px")),
    plotLines = plot_lines
  )
  if (!is.null(min)) axis$min <- min
  if (!is.null(max)) axis$max <- max
  axis
}

chart_bubble_points <- function(df, x, y, z, name, color = NULL, extra = NULL) {
  lapply(seq_len(nrow(df)), function(i) {
    point <- list(
      name = as.character(df[[name]][[i]]),
      x = chart_safe_number(df[[x]][[i]]),
      y = chart_safe_number(df[[y]][[i]]),
      z = max(chart_safe_number(df[[z]][[i]]), 0)
    )
    if (!is.null(color)) point$color <- as.character(df[[color]][[i]])
    if (!is.null(extra)) point <- c(point, extra(df, i))
    point
  })
}

# Input: market-position bubble metrics. Output: a highcharter htmlwidget or a
# safe fallback table when the optional plotting package is unavailable.
plot_market_position_bubble <- function(df) {
  required <- c("market", "total_market_cap_yi", "avg_daily_turnover_yi", "listed_company_count")
  if (!is.data.frame(df) || nrow(df) == 0L || !all(required %in% names(df))) {
    return(chart_empty_state("暂无市场定位气泡图数据"))
  }
  if (!chart_has_highcharter()) {
    return(chart_fallback_table("多市场定位对比", df, "未检测到 highcharter，已显示市场对比数据。"))
  }

  colors <- chart_colors()
  plot_df <- df
  plot_df$marker_color <- ifelse(plot_df$market %in% c("北交所", "本所"), colors$bse_blue, "#9AA9B8")
  points <- chart_bubble_points(
    plot_df, "total_market_cap_yi", "avg_daily_turnover_yi", "listed_company_count", "market", "marker_color",
    extra = function(data, i) {
      label <- as.character(data$market[[i]])
      if (label %in% c("北交所", "本所")) {
        list(dataLabels = list(enabled = TRUE, format = label, style = list(color = colors$bse_blue, fontWeight = "700", textOutline = "none")))
      } else {
        list()
      }
    }
  )

  chart_widget(
    chart_hc_base("bubble") |>
      highcharter::hc_xAxis(xAxis = chart_hc_axis("总市值（亿元）")) |>
      highcharter::hc_yAxis(yAxis = chart_hc_axis("日均成交额（亿元）")) |>
      highcharter::hc_plotOptions(bubble = list(minSize = 12, maxSize = 62, marker = list(fillOpacity = 0.72))) |>
      highcharter::hc_add_series(name = "市场对比", type = "bubble", data = points) |>
      highcharter::hc_tooltip(pointFormat = "<b>{point.name}</b><br/>总市值：{point.x:,.0f} 亿元<br/>日均成交额：{point.y:,.1f} 亿元<br/>上市公司：{point.z:,.0f} 家")
  )
}

# Input: industry-market-cap metrics. Output: a horizontal highcharter bar chart.
plot_market_industry_structure <- function(df) {
  required <- c("industry", "market_cap_yi", "market_cap_share", "company_count", "pe_median")
  if (!is.data.frame(df) || nrow(df) == 0L || !all(required %in% names(df))) {
    return(chart_empty_state("暂无行业市值结构数据"))
  }
  if (!chart_has_highcharter()) {
    return(chart_fallback_table("行业市值结构", df, "未检测到 highcharter，已显示行业汇总数据。"))
  }

  colors <- chart_colors()
  plot_df <- chart_top_n(df, "market_cap_yi", 12L)
  plot_df <- plot_df[order(chart_safe_number(plot_df$market_cap_yi)), , drop = FALSE]

  chart_widget(
    chart_hc_base("bar") |>
      highcharter::hc_xAxis(xAxis = chart_hc_axis("", categories = as.character(plot_df$industry))) |>
      highcharter::hc_yAxis(yAxis = chart_hc_axis("总市值（亿元）")) |>
      highcharter::hc_plotOptions(series = list(borderWidth = 0, pointPadding = 0.08, groupPadding = 0.06)) |>
      highcharter::hc_add_series(name = "总市值", type = "bar", color = colors$bse_blue, data = chart_safe_number(plot_df$market_cap_yi)) |>
      highcharter::hc_tooltip(
        formatter = highcharter::JS(sprintf(
          "function(){var p=this.point, d=%s[this.point.index]; return '<b>'+this.x+'</b><br/>总市值：'+Highcharts.numberFormat(this.y,1)+' 亿元<br/>市值占比：'+Highcharts.numberFormat(d.market_cap_share*100,1)+'%%<br/>公司数：'+d.company_count+' 家<br/>PE 中位数：'+Highcharts.numberFormat(d.pe_median,1)+' 倍';}",
          jsonlite::toJSON(unname(split(plot_df, seq_len(nrow(plot_df)))), auto_unbox = TRUE)
        ))
      )
  )
}
