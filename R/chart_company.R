# Input: industry contribution metrics. Output: a contribution heatmap.
plot_company_industry_matrix <- function(df) {
  required <- c("industry", "company_share", "market_cap_share", "revenue_share", "net_profit_share", "r_and_d_share")
  if (!is.data.frame(df) || nrow(df) == 0L || !all(required %in% names(df))) {
    return(chart_empty_state("暂无行业贡献矩阵数据"))
  }
  if (!chart_has_highcharter()) {
    return(chart_fallback_table("行业结构与经营贡献", df, "未检测到 highcharter，已显示行业贡献数据。"))
  }

  plot_df <- utils::head(df[order(chart_safe_number(df$revenue_yi), decreasing = TRUE, na.last = TRUE), , drop = FALSE], 12)
  measure_cols <- c("company_share", "market_cap_share", "revenue_share", "net_profit_share", "r_and_d_share")
  measure_labels <- c("公司数占比", "市值占比", "营收占比", "净利润占比", "研发费用占比")
  heat_points <- unlist(lapply(seq_len(nrow(plot_df)), function(row) {
    lapply(seq_along(measure_cols), function(col) {
      list(x = col - 1L, y = row - 1L, value = round(chart_safe_number(plot_df[[measure_cols[[col]]]][[row]]) * 100, 1))
    })
  }), recursive = FALSE)
  max_value <- max(vapply(heat_points, function(point) point$value, numeric(1)), 1)

  chart_widget(
    chart_hc_base("heatmap") |>
      highcharter::hc_xAxis(xAxis = chart_hc_axis("", categories = measure_labels)) |>
      highcharter::hc_yAxis(yAxis = chart_hc_axis("", categories = as.character(plot_df$industry), opposite = FALSE)) |>
      highcharter::hc_colorAxis(
        min = 0,
        max = max_value,
        stops = list(list(0, "#EDF5FF"), list(0.55, "#81B3E5"), list(1, "#005BAC"))
      ) |>
      highcharter::hc_add_series(
        name = "占比",
        type = "heatmap",
        data = heat_points,
        borderWidth = 1,
        borderColor = "#FFFFFF",
        dataLabels = list(enabled = TRUE, format = "{point.value:.1f}%", style = list(color = "#17324D", fontSize = "10px", textOutline = "none"))
      ) |>
      highcharter::hc_legend(layout = "horizontal", align = "right", verticalAlign = "bottom", symbolWidth = 130)
  )
}

# Input: company growth-profitability metrics. Output: a labelled bubble quadrant.
plot_company_quality_quadrant <- function(df) {
  required <- c("company_name", "industry", "revenue_growth", "roe", "total_market_cap_yi")
  if (!is.data.frame(df) || nrow(df) == 0L || !all(required %in% names(df))) {
    return(chart_empty_state("暂无成长性与盈利能力数据"))
  }
  if (!chart_has_highcharter()) {
    return(chart_fallback_table("成长性与盈利能力四象限", df, "未检测到 highcharter，已显示公司指标数据。"))
  }

  colors <- chart_colors()
  plot_df <- df
  plot_df$revenue_growth <- chart_safe_number(plot_df$revenue_growth)
  plot_df$roe <- chart_safe_number(plot_df$roe)
  plot_df$total_market_cap_yi <- chart_safe_number(plot_df$total_market_cap_yi)
  plot_df <- plot_df[is.finite(plot_df$revenue_growth) & is.finite(plot_df$roe), , drop = FALSE]
  if (nrow(plot_df) == 0L) return(chart_empty_state("暂无有效的成长与盈利指标"))

  x_mid <- stats::median(plot_df$revenue_growth, na.rm = TRUE)
  y_mid <- stats::median(plot_df$roe, na.rm = TRUE)
  plot_df$quadrant <- ifelse(
    plot_df$revenue_growth >= x_mid & plot_df$roe >= y_mid, "高成长 · 高盈利",
    ifelse(plot_df$revenue_growth >= x_mid, "高成长 · 待盈利", ifelse(plot_df$roe >= y_mid, "稳健盈利", "承压观察"))
  )
  palette <- c("高成长 · 高盈利" = colors$bse_blue, "高成长 · 待盈利" = colors$bse_cyan, "稳健盈利" = colors$success, "承压观察" = colors$warning)
  x_range <- range(plot_df$revenue_growth, na.rm = TRUE)
  y_range <- range(plot_df$roe, na.rm = TRUE)
  x_pad <- max(diff(x_range) * 0.08, 1)
  y_pad <- max(diff(y_range) * 0.08, 1)

  chart <- chart_hc_base("bubble") |>
    highcharter::hc_xAxis(xAxis = chart_hc_axis(
      "营业收入增速（%）", min = x_range[[1]] - x_pad, max = x_range[[2]] + x_pad,
      plot_lines = list(list(value = x_mid, color = "#9AA9B8", width = 1, dashStyle = "Dash", zIndex = 3))
    )) |>
    highcharter::hc_yAxis(yAxis = chart_hc_axis(
      "ROE（%）", min = y_range[[1]] - y_pad, max = y_range[[2]] + y_pad,
      plot_lines = list(list(value = y_mid, color = "#9AA9B8", width = 1, dashStyle = "Dash", zIndex = 3))
    )) |>
    highcharter::hc_plotOptions(bubble = list(minSize = 6, maxSize = 48, marker = list(fillOpacity = 0.68))) |>
    highcharter::hc_subtitle(text = "右上：高成长 · 高盈利；左下：承压观察", align = "left", style = list(color = "#60758D", fontSize = "11px"))

  for (quadrant in names(palette)) {
    part <- plot_df[plot_df$quadrant == quadrant, , drop = FALSE]
    if (nrow(part) == 0L) next
    points <- chart_bubble_points(part, "revenue_growth", "roe", "total_market_cap_yi", "company_name")
    chart <- chart |>
      highcharter::hc_add_series(name = quadrant, type = "bubble", color = palette[[quadrant]], data = points)
  }

  chart_widget(
    chart |>
      highcharter::hc_tooltip(pointFormat = "<b>{point.name}</b><br/>营收增速：{point.x:.1f}%<br/>ROE：{point.y:.1f}%<br/>总市值：{point.z:,.1f} 亿元")
  )
}
