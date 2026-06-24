# 用途：绘制年度上市与融资趋势图（新增上市柱状图 + IPO/再融资折线图组合）。
# 输入来源：`calc_listing_financing_trend()` 的输出数据框。
# Input: listing and financing trend metrics. Output: a column-line trend chart.
plot_listing_financing_trend <- function(df) {
  required <- c("year", "new_listing_count", "ipo_financing_amount_yi", "refinancing_amount_yi")
  if (!is.data.frame(df) || nrow(df) == 0L || !all(required %in% names(df))) {
    return(chart_empty_state("暂无上市融资趋势数据"))
  }
  if (!chart_has_highcharter()) {
    return(chart_fallback_table("上市与融资趋势", df, "未检测到 highcharter，已显示趋势数据。"))
  }

  colors <- chart_colors()
  plot_df <- df[order(df$year), , drop = FALSE]
  categories <- as.character(plot_df$year)

  chart_widget(
    chart_hc_base("column") |>
      hc_x_axis("", categories = categories) |>
      highcharter::hc_yAxis_multiples(
        chart_hc_axis("新增上市（家）", min = 0),
        chart_hc_axis("融资金额（亿元）", opposite = TRUE, min = 0)
      ) |>
      highcharter::hc_plotOptions(column = list(borderWidth = 0, borderRadius = 2, pointPadding = 0.08)) |>
      highcharter::hc_add_series(name = "新增上市公司", type = "column", yAxis = 0, color = colors$bse_blue, data = chart_safe_number(plot_df$new_listing_count)) |>
      highcharter::hc_add_series(name = "IPO 融资额", type = "spline", yAxis = 1, color = colors$risk_orange, data = chart_safe_number(plot_df$ipo_financing_amount_yi), marker = list(radius = 3)) |>
      highcharter::hc_add_series(name = "再融资额", type = "spline", yAxis = 1, color = colors$bse_cyan, data = chart_safe_number(plot_df$refinancing_amount_yi), marker = list(radius = 3)) |>
      highcharter::hc_tooltip(shared = TRUE, valueDecimals = 1)
  )
}

# 用途：绘制交易活跃与市场生态趋势图（成交金额、活跃公司、换手率多轴组合）。
# 输入来源：`calc_trading_ecosystem_trend()` 的输出数据框。
# Input: trading and ecosystem trend metrics. Output: a multi-axis trend chart.
plot_trading_ecosystem_trend <- function(df) {
  required <- c("period", "turnover_amount_yi", "active_company_count", "avg_turnover_rate")
  if (!is.data.frame(df) || nrow(df) == 0L || !all(required %in% names(df))) {
    return(chart_empty_state("暂无交易生态趋势数据"))
  }
  if (!chart_has_highcharter()) {
    return(chart_fallback_table("交易活跃与市场生态", df, "未检测到 highcharter，已显示趋势数据。"))
  }

  colors <- chart_colors()
  plot_df <- df[order(df$period), , drop = FALSE]
  categories <- as.character(plot_df$period)
  chart <- chart_hc_base("column") |>
    highcharter::hc_xAxis(xAxis = chart_hc_axis("", categories = categories)) |>
    highcharter::hc_yAxis_multiples(
      chart_hc_axis("成交金额（亿元）", min = 0),
      chart_hc_axis("换手率（%）", opposite = TRUE, min = 0)
    ) |>
    highcharter::hc_plotOptions(column = list(borderWidth = 0, borderRadius = 2, pointPadding = 0.08)) |>
    highcharter::hc_add_series(name = "成交金额", type = "column", yAxis = 0, color = colors$bse_blue_soft, data = chart_safe_number(plot_df$turnover_amount_yi)) |>
    highcharter::hc_add_series(name = "活跃公司", type = "spline", yAxis = 0, color = colors$bse_blue, data = chart_safe_number(plot_df$active_company_count), marker = list(radius = 3)) |>
    highcharter::hc_add_series(name = "平均换手率", type = "spline", yAxis = 1, color = colors$risk_orange, data = chart_safe_number(plot_df$avg_turnover_rate), marker = list(radius = 3))

  if ("ecosystem_event" %in% names(plot_df)) {
    event_index <- which(!is.na(plot_df$ecosystem_event) & nzchar(plot_df$ecosystem_event))
    if (length(event_index) > 0L) {
      event_points <- lapply(event_index, function(i) list(x = i - 1L, y = chart_safe_number(plot_df$active_company_count[[i]]), name = as.character(plot_df$ecosystem_event[[i]])))
      chart <- chart |>
        highcharter::hc_add_series(name = "关键事件", type = "scatter", yAxis = 0, color = colors$warning, data = event_points, marker = list(symbol = "diamond", radius = 5))
    }
  }

  chart_widget(chart |> highcharter::hc_tooltip(shared = TRUE, valueDecimals = 1))
}

# 用途：绘制三大指数走势线图（创业板指、科创50、北证50），以 2022-04-29 为基准日计算涨跌幅
# 输入来源：`calc_index_trend()` 的输出数据框
# Input: index trend data frame (date + 3 index columns in % change). Output: a multi-line trend chart.
plot_index_trend <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0L) {
    return(chart_empty_state("暂无指数走势数据"))
  }
  if (!chart_has_highcharter()) {
    return(chart_fallback_table("主要指数走势", df, "未检测到 highcharter，已显示指数数据。"))
  }

  plot_df <- df[order(df$date), , drop = FALSE]
  categories <- as.character(plot_df$date)

  chart <- chart_hc_base("spline") |>
    highcharter::hc_xAxis(categories = categories, labels = list(style = list(fontSize = "10px"))) |>
    highcharter::hc_yAxis(
      title = list(text = "涨跌幅（%）"),
      labels = list(format = "{value}%")
    ) |>
    highcharter::hc_add_series(
      name = "创业板指",
      data = chart_safe_number(plot_df[["创业板指"]]),
      color = "#002B5B",
      lineWidth = 2,
      marker = list(enabled = FALSE)
    ) |>
    highcharter::hc_add_series(
      name = "科创50指数",
      data = chart_safe_number(plot_df[["科创50指数"]]),
      color = "#EA5455",
      lineWidth = 2,
      marker = list(enabled = FALSE)
    ) |>
    highcharter::hc_add_series(
      name = "北证50指数",
      data = chart_safe_number(plot_df[["北证50指数"]]),
      color = "#E8AA42",
      lineWidth = 2,
      marker = list(enabled = FALSE)
    ) |>
    highcharter::hc_tooltip(
      shared = TRUE,
      valueDecimals = 1,
      valueSuffix = "%"
    ) |>
    highcharter::hc_legend(
      align = "center",
      verticalAlign = "bottom",
      layout = "horizontal"
    ) |>
    highcharter::hc_credits(enabled = FALSE)

  chart_widget(chart)
}
