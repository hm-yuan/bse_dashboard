# 用途：绘制行业经营贡献热力图（展示各行业在公司数、市值、营收、净利润、研发费用上的占比）。
# 输入来源：`calc_company_industry_contribution()` 的输出数据框。
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
      list(
        x = col - 1L,
        y = row - 1L,
        value = round(chart_safe_number(plot_df[[measure_cols[[col]]]][[row]]) * 100, 1),
        industry = as.character(plot_df$industry[[row]]),
        measure = measure_labels[[col]]
      )
    })
  }), recursive = FALSE)
  max_value <- max(vapply(heat_points, function(point) point$value, numeric(1)), 1)

  chart_widget(
    chart_hc_base("heatmap") |>
      hc_x_axis("", categories = measure_labels) |>
      hc_y_axis("", categories = as.character(plot_df$industry), opposite = FALSE, reversed = TRUE) |>
      highcharter::hc_colorAxis(
        min = 0,
        max = max_value,
        stops = list(list(0, "#D1E5F0"), list(0.55, "#92C5DE"), list(1, "#2166AC"))
      ) |>
      highcharter::hc_plotOptions(
        series = list(
          animation = FALSE,
          # 热力图已在单元格中直接展示数值；关闭鼠标追踪可避免 HTML tooltip
          # 在 Shiny 容器内反复重定位时触发布局漂移。
          enableMouseTracking = FALSE,
          states = list(
            hover = list(enabled = FALSE),
            inactive = list(enabled = FALSE)
          )
        ),
        heatmap = list(
          animation = FALSE,
          enableMouseTracking = FALSE,
          states = list(
            hover = list(enabled = FALSE),
            inactive = list(enabled = FALSE)
          ),
          stickyTracking = FALSE
        )
      ) |>
      highcharter::hc_add_series(
        name = "占比",
        type = "heatmap",
        data = heat_points,
        borderWidth = 1,
        borderColor = "#FFFFFF",
        dataLabels = list(enabled = TRUE, format = "{point.value:.1f}%", style = list(color = "#17324D", fontSize = "10px", textOutline = "none"))
      ) |>
      highcharter::hc_tooltip(enabled = FALSE) |>
      highcharter::hc_legend(layout = "horizontal", align = "right", verticalAlign = "bottom", symbolWidth = 130)
  )
}

# 用途：绘制中国省级公司分布地图；省级色块表示公司数量，城市气泡表示城市公司数量。
# 输入来源：`calc_company_geography()` 输出的省级与城市汇总数据。
plot_company_geography_map <- function(geo) {
  if (!chart_has_highcharter()) {
    return(chart_empty_state("未检测到 highcharter，无法渲染中国公司分布地图。"))
  }
  if (is.null(geo) || !is.data.frame(geo$provinces) || !is.data.frame(geo$cities) || nrow(geo$cities) == 0L) {
    return(chart_empty_state("暂无可用的公司城市数据。"))
  }

  map_cache <- file.path("data", "cache", "china-cn-all.geo.json")
  map_data <- if (file.exists(map_cache) && requireNamespace("jsonlite", quietly = TRUE)) {
    tryCatch(jsonlite::fromJSON(map_cache, simplifyVector = FALSE), error = function(e) NULL)
  } else {
    tryCatch(highcharter::download_map_data("countries/cn/cn-all"), error = function(e) NULL)
  }
  if (is.null(map_data)) {
    return(chart_empty_state("中国地图底图加载失败，请检查本地地图缓存与 Highcharts Map 数据可用性。"))
  }

  colors <- chart_colors()
  province_data <- geo$provinces[, c("hc_key", "province", "company_count"), drop = FALSE]
  names(province_data) <- c("hc_key", "name", "value")
  city_points <- geo$cities
  city_points$longitude <- chart_safe_number(city_points$longitude)
  city_points$latitude <- chart_safe_number(city_points$latitude)
  city_points$company_count <- chart_safe_number(city_points$company_count)
  city_points <- city_points[
    is.finite(city_points$longitude) &
      is.finite(city_points$latitude) &
      is.finite(city_points$company_count) &
      city_points$company_count > 0,
    ,
    drop = FALSE
  ]
  if (nrow(city_points) == 0L) {
    return(chart_empty_state("暂无可定位的公司城市数据。"))
  }

  # `mapbubble` 的尺寸转换在当前 Highcharts Map 组合中会导致城市层丢失。
  # 使用地图原生 `mappoint`，并在 R 侧按公司数量计算半径，保留气泡图的表达口径。
  max_city_count <- max(city_points$company_count)
  city_data <- lapply(seq_len(nrow(city_points)), function(i) {
    radius <- 4 + 11 * sqrt(city_points$company_count[[i]] / max_city_count)
    list(
      name = city_points$city[[i]],
      province = city_points$province[[i]],
      lon = city_points$longitude[[i]],
      lat = city_points$latitude[[i]],
      company_count = city_points$company_count[[i]],
      marker = list(radius = radius)
    )
  })

  chart_hc_base("map") |>
    highcharter::hc_add_dependency("modules/map.js") |>
    highcharter::hc_chart(backgroundColor = "transparent") |>
    highcharter::hc_add_series(
      type = "map",
      mapData = map_data,
      name = "省级公司数量",
      data = province_data,
      joinBy = c("hc-key", "hc_key"),
      borderColor = "#FFFFFF",
      borderWidth = 0.8,
      nullColor = "#F7F7F7"
    ) |>
    highcharter::hc_colorAxis(
      min = 0,
      minColor = "#E8F0F8",
      maxColor = "#002B5B",
      labels = list(format = "{value} 家")
    ) |>
    highcharter::hc_add_series(
      type = "mappoint",
      name = "城市公司数量",
      data = city_data,
      color = "#00A6C8",
      marker = list(lineColor = "#FFFFFF", lineWidth = 1, fillOpacity = 0.85)
    ) |>
    highcharter::hc_mapNavigation(enabled = TRUE, enableButtons = FALSE) |>
    highcharter::hc_tooltip(
      useHTML = TRUE,
      formatter = highcharter::JS("function(){ if (this.point.series.type === 'mappoint') { return '<b>'+this.point.name+'</b><br/>所在省份：'+this.point.province+'<br/>上市公司：'+Highcharts.numberFormat(this.point.company_count,0)+' 家'; } return '<b>'+this.point.name+'</b><br/>上市公司：'+Highcharts.numberFormat(this.point.value || 0,0)+' 家'; }")
    ) |>
    highcharter::hc_legend(
      layout = "horizontal",
      align = "center",
      verticalAlign = "bottom",
      symbolRadius = 2
    ) |>
    highcharter::hc_credits(enabled = FALSE)
}

# 用途：绘制公司成长性与盈利能力四象限气泡图。
# 输入来源：`calc_company_quality_quadrant()` 的输出数据框。
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
    hc_x_axis(
      "营业收入增速（%）", min = x_range[[1]] - x_pad, max = x_range[[2]] + x_pad,
      plot_lines = list(list(value = x_mid, color = "#9AA9B8", width = 1, dashStyle = "Dash", zIndex = 3))
    ) |>
    hc_y_axis(
      "ROE（%）", min = y_range[[1]] - y_pad, max = y_range[[2]] + y_pad,
      plot_lines = list(list(value = y_mid, color = "#9AA9B8", width = 1, dashStyle = "Dash", zIndex = 3))
    ) |>
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
