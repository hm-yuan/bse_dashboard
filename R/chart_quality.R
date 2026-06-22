# 用途：绘制市场质量状态矩阵气泡图（流动性得分 vs 基本面质量得分，按风险等级着色）。
# 输入来源：`calc_quality_status_matrix()` 的输出数据框。
# Input: company quality status metrics. Output: a liquidity-quality risk matrix.
plot_quality_status_matrix <- function(df) {
  required <- c("company_name", "liquidity_score", "quality_score", "total_market_cap_yi", "risk_level")
  if (!is.data.frame(df) || nrow(df) == 0L || !all(required %in% names(df))) {
    return(chart_empty_state("暂无市场质量状态矩阵数据"))
  }
  if (!chart_has_highcharter()) {
    return(chart_fallback_table("市场质量状态矩阵", df, "未检测到 highcharter，已显示质量监测数据。"))
  }

  colors <- chart_colors()
  plot_df <- df
  plot_df$liquidity_score <- chart_safe_number(plot_df$liquidity_score)
  plot_df$quality_score <- chart_safe_number(plot_df$quality_score)
  plot_df$total_market_cap_yi <- chart_safe_number(plot_df$total_market_cap_yi)
  plot_df <- plot_df[is.finite(plot_df$liquidity_score) & is.finite(plot_df$quality_score), , drop = FALSE]
  if (nrow(plot_df) == 0L) return(chart_empty_state("暂无有效的市场质量指标"))

  x_mid <- stats::median(plot_df$liquidity_score, na.rm = TRUE)
  y_mid <- stats::median(plot_df$quality_score, na.rm = TRUE)
  risk_colors <- c("低" = colors$success, "中" = colors$warning, "高" = colors$danger)
  x_range <- range(plot_df$liquidity_score, na.rm = TRUE)
  y_range <- range(plot_df$quality_score, na.rm = TRUE)
  x_pad <- max(diff(x_range) * 0.08, 1)
  y_pad <- max(diff(y_range) * 0.08, 1)

  chart <- chart_hc_base("bubble") |>
    hc_x_axis(
      "流动性水平", min = x_range[[1]] - x_pad, max = x_range[[2]] + x_pad,
      plot_lines = list(list(value = x_mid, color = "#9AA9B8", width = 1, dashStyle = "Dash", zIndex = 3))
    ) |>
    hc_y_axis(
      "基本面质量", min = y_range[[1]] - y_pad, max = y_range[[2]] + y_pad,
      plot_lines = list(list(value = y_mid, color = "#9AA9B8", width = 1, dashStyle = "Dash", zIndex = 3))
    ) |>
    highcharter::hc_plotOptions(bubble = list(minSize = 6, maxSize = 50, marker = list(fillOpacity = 0.7))) |>
    highcharter::hc_subtitle(text = "右上：活跃稳健；左下：重点观察", align = "left", style = list(color = "#60758D", fontSize = "11px"))

  for (level in names(risk_colors)) {
    part <- plot_df[as.character(plot_df$risk_level) == level, , drop = FALSE]
    if (nrow(part) == 0L) next
    points <- chart_bubble_points(part, "liquidity_score", "quality_score", "total_market_cap_yi", "company_name")
    chart <- chart |>
      highcharter::hc_add_series(name = paste0(level, "风险"), type = "bubble", color = risk_colors[[level]], data = points)
  }

  chart_widget(
    chart |>
      highcharter::hc_tooltip(pointFormat = "<b>{point.name}</b><br/>流动性得分：{point.x:.1f}<br/>基本面得分：{point.y:.1f}<br/>总市值：{point.z:,.1f} 亿元")
  )
}

# 用途：绘制风险类型与行业分布热力图（颜色表示各行业各风险类型的公司数量）。
# 输入来源：`calc_risk_industry_heatmap()` 的输出数据框。
# Input: risk type by industry metrics. Output: an orange-to-red heatmap.
plot_risk_industry_heatmap <- function(df) {
  required <- c("industry", "risk_type", "risk_count")
  if (!is.data.frame(df) || nrow(df) == 0L || !all(required %in% names(df))) {
    return(chart_empty_state("暂无风险行业热力图数据"))
  }
  if (!chart_has_highcharter()) {
    return(chart_fallback_table("风险类型与行业分布", df, "未检测到 highcharter，已显示风险汇总数据。"))
  }

  industries <- unique(as.character(df$industry))
  risk_types <- unique(as.character(df$risk_type))
  heat_points <- lapply(seq_len(nrow(df)), function(i) {
    list(
      x = match(as.character(df$risk_type[[i]]), risk_types) - 1L,
      y = match(as.character(df$industry[[i]]), industries) - 1L,
      value = chart_safe_number(df$risk_count[[i]]),
      risk_type = as.character(df$risk_type[[i]]),
      industry = as.character(df$industry[[i]])
    )
  })
  max_value <- max(vapply(heat_points, function(point) point$value, numeric(1)), 1)

  chart_widget(
    chart_hc_base("heatmap") |>
      hc_x_axis("", categories = risk_types) |>
      hc_y_axis("", categories = industries, reversed = TRUE) |>
      highcharter::hc_colorAxis(
        min = 0,
        max = max_value,
        stops = list(list(0, "#FFF4E9"), list(0.55, "#F6A261"), list(1, "#D64545"))
      ) |>
      highcharter::hc_plotOptions(
        heatmap = list(
          states = list(hover = list(enabled = FALSE)),
          stickyTracking = FALSE
        )
      ) |>
      highcharter::hc_add_series(
        name = "风险公司数",
        type = "heatmap",
        data = heat_points,
        borderWidth = 1,
        borderColor = "#FFFFFF",
        dataLabels = list(enabled = TRUE, format = "{point.value}", style = list(color = "#6B291C", fontSize = "10px", textOutline = "none"))
      ) |>
      highcharter::hc_tooltip(
        pointFormat = "<b>{point.industry}</b><br/>风险类型：{point.risk_type}<br/>风险公司数：{point.value}",
        style = list(width = "160px", whiteSpace = "normal")
      ) |>
      highcharter::hc_legend(layout = "horizontal", align = "right", verticalAlign = "bottom", symbolWidth = 130)
  )
}
