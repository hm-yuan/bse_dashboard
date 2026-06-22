placeholder_chart_colors <- function() {
  palette <- chart_theme_palette()
  list(
    blue = palette[[1]],
    blue_soft = palette[[11]],
    cyan = palette[[7]],
    navy = palette[[1]],
    green = palette[[8]],
    orange = palette[[3]],
    red = palette[[2]],
    purple = palette[[9]],
    slate = palette[[5]],
    grid = "#E5EBF2"
  )
}

placeholder_chart_base <- function(type = "line", height = NULL) {
  hc <- highcharter::highchart() |>
    highcharter::hc_add_theme(chart_bloom_theme()) |>
    highcharter::hc_chart(type = type, backgroundColor = "transparent") |>
    highcharter::hc_credits(enabled = FALSE) |>
    highcharter::hc_exporting(enabled = FALSE) |>
    highcharter::hc_tooltip(shared = TRUE, valueDecimals = 1) |>
    highcharter::hc_legend(align = "center", verticalAlign = "bottom", itemStyle = list(color = "#52657A", fontSize = "11px", fontWeight = "500"))

  if (!is.null(height)) {
    hc <- highcharter::hc_size(hc, height = height)
  }
  hc
}

placeholder_axis <- function(title, categories = NULL, opposite = FALSE) {
  highcharter::hc_yAxis(
    title = list(text = title, style = list(color = "#64748B", fontSize = "11px")),
    gridLineColor = "#E5EBF2",
    labels = list(style = list(color = "#718096", fontSize = "10px")),
    opposite = opposite
  )
}

placeholder_market_bubble <- function() {
  colors <- placeholder_chart_colors()
  data <- data.frame(
    name = c("北交所", "科创板", "创业板", "沪市主板", "深市主板"),
    market_cap = c(3.2, 7.8, 12.1, 46.8, 23.5),
    turnover = c(126, 580, 1280, 2300, 1880),
    companies = c(238, 572, 1350, 1690, 1510),
    color = c(colors$blue, colors$cyan, colors$green, colors$slate, "#94A3B8"),
    stringsAsFactors = FALSE
  )
  points <- lapply(seq_len(nrow(data)), function(i) list(name = data$name[[i]], x = data$market_cap[[i]], y = data$turnover[[i]], z = data$companies[[i]], color = data$color[[i]]))
  placeholder_chart_base("bubble") |>
    highcharter::hc_xAxis(title = list(text = "总市值（万亿元）"), type = "logarithmic", gridLineColor = colors$grid, labels = list(style = list(color = "#718096", fontSize = "10px"))) |>
    highcharter::hc_yAxis(title = list(text = "日均成交额（亿元）"), gridLineColor = colors$grid, labels = list(style = list(color = "#718096", fontSize = "10px"))) |>
    highcharter::hc_add_series(data = points, type = "bubble", name = "市场", marker = list(lineColor = "#FFFFFF", lineWidth = 1), dataLabels = list(enabled = TRUE, format = "{point.name}", style = list(color = "#334155", fontSize = "10px", textOutline = "none"))) |>
    highcharter::hc_legend(enabled = FALSE)
}

# 用途：绘制占位行业市值占比矩形树图。
# 输入来源：函数内部硬编码的演示数据（各行业市值占比）。
placeholder_industry_treemap <- function() {
  colors <- placeholder_chart_colors()
  data <- list(
    list(name = "电子", value = 23, color = colors$blue),
    list(name = "机械设备", value = 17, color = "#2F80D1"),
    list(name = "医药生物", value = 13, color = "#4A90E2"),
    list(name = "电力设备", value = 11, color = "#69A6E9"),
    list(name = "基础化工", value = 8, color = "#8DBBEF"),
    list(name = "计算机", value = 7, color = "#A8CAF3"),
    list(name = "汽车", value = 5, color = "#C5DBF7"),
    list(name = "其他", value = 16, color = "#DCE8F7")
  )
  placeholder_chart_base("treemap") |>
    highcharter::hc_add_series(type = "treemap", name = "行业市值", layoutAlgorithm = "squarified", data = data, dataLabels = list(enabled = TRUE, format = "{point.name}<br/>{point.value}%", style = list(color = "#FFFFFF", fontSize = "11px", textOutline = "none"), useHTML = FALSE)) |>
    highcharter::hc_legend(enabled = FALSE)
}

placeholder_company_heatmap <- function() {
  colors <- placeholder_chart_colors()
  industries <- c("电子", "机械设备", "医药生物", "电力设备", "计算机", "基础化工", "汽车")
  metrics <- c("公司数", "市值", "营收", "净利润", "研发费用")
  values <- matrix(c(84, 92, 80, 76, 88, 72, 68, 66, 61, 70, 57, 54, 59, 62, 51, 47, 45, 39, 43, 31, 28, 34, 36, 45, 26, 22, 20, 18, 24, 17, 14, 19, 16, 15, 21), nrow = length(industries), byrow = TRUE)
  points <- unlist(lapply(seq_along(industries), function(row) lapply(seq_along(metrics), function(col) list(x = col - 1L, y = row - 1L, value = values[row, col]))), recursive = FALSE)
  placeholder_chart_base("heatmap") |>
    highcharter::hc_xAxis(categories = metrics, lineColor = colors$grid, labels = list(style = list(color = "#52657A", fontSize = "10px"))) |>
    highcharter::hc_yAxis(categories = industries, title = list(text = NULL), reversed = TRUE, labels = list(style = list(color = "#52657A", fontSize = "10px"))) |>
    highcharter::hc_colorAxis(minColor = "#EEF5FD", maxColor = colors$blue) |>
    highcharter::hc_add_series(data = points, type = "heatmap", name = "贡献度", borderWidth = 1, borderColor = "#FFFFFF", dataLabels = list(enabled = TRUE, format = "{point.value}%", style = list(color = "#17324D", fontSize = "10px", textOutline = "none")))
}

placeholder_company_quadrant <- function() {
  colors <- placeholder_chart_colors()
  placeholder_with_seed(placeholder_seed("company_quadrant", 3L), {
    n <- 68
    sectors <- c("电子", "机械设备", "医药生物", "电力设备", "计算机")
    sector_colors <- c(colors$blue, colors$cyan, colors$green, colors$orange, colors$purple)
    series <- lapply(seq_along(sectors), function(i) {
      idx <- rep(seq_along(sectors), length.out = n) == i
      list(name = sectors[[i]], color = sector_colors[[i]], data = lapply(which(idx), function(j) list(x = round(stats::runif(1, -18, 52), 1), y = round(stats::runif(1, -30, 92), 1), z = round(stats::runif(1, 8, 85), 1))))
    })
    chart <- placeholder_chart_base("bubble") |>
      highcharter::hc_xAxis(title = list(text = "营业收入增速（%）"), plotLines = list(list(value = 0, color = "#CBD5E1", width = 1)), gridLineColor = colors$grid) |>
      highcharter::hc_yAxis(title = list(text = "ROE（%）"), plotLines = list(list(value = 0, color = "#CBD5E1", width = 1)), gridLineColor = colors$grid) |>
      highcharter::hc_annotations(list(labels = list(list(point = list(x = 45, y = 82, xAxis = 0, yAxis = 0), text = "高成长 · 高盈利", style = list(color = colors$blue, fontSize = "10px")), list(point = list(x = -14, y = -20, xAxis = 0, yAxis = 0), text = "低成长 · 低盈利", style = list(color = colors$red, fontSize = "10px")))))
    for (item in series) chart <- highcharter::hc_add_series(chart, data = item$data, type = "bubble", name = item$name, color = item$color, marker = list(lineColor = "#FFFFFF", lineWidth = 0.8))
    chart
  })
}

# 用途：绘制占位上市与融资趋势图（柱状+折线组合）。
# 输入来源：函数内部硬编码的演示数据（年度新增上市、IPO、再融资额）。
placeholder_listing_financing <- function() {
  colors <- placeholder_chart_colors()
  years <- c("2019", "2020", "2021", "2022", "2023", "2024YTD")
  placeholder_chart_base("column") |>
    highcharter::hc_xAxis(categories = years, lineColor = colors$grid) |>
    highcharter::hc_yAxis_multiples(
      list(title = list(text = "新增上市（家）"), gridLineColor = colors$grid),
      list(title = list(text = "融资额（亿元）"), opposite = TRUE, gridLineWidth = 0)
    ) |>
    highcharter::hc_add_series(name = "新增上市", data = c(98, 116, 146, 162, 257, 186), color = colors$blue, type = "column") |>
    highcharter::hc_add_series(name = "IPO融资额", data = c(180, 196, 292, 306, 564, 343), color = colors$orange, type = "spline", yAxis = 1) |>
    highcharter::hc_add_series(name = "再融资额", data = c(368, 432, 567, 564, 869, 500), color = colors$green, type = "spline", yAxis = 1)
}

placeholder_trading_ecosystem <- function() {
  colors <- placeholder_chart_colors()
  years <- c("2019", "2020", "2021", "2022", "2023", "2024YTD")
  placeholder_chart_base("column") |>
    highcharter::hc_xAxis(categories = years, lineColor = colors$grid) |>
    highcharter::hc_yAxis_multiples(
      list(title = list(text = "日均成交额（亿元）"), gridLineColor = colors$grid),
      list(title = list(text = "活跃公司（家）"), opposite = TRUE, gridLineWidth = 0)
    ) |>
    highcharter::hc_add_series(name = "日均成交额", data = c(38, 44, 62, 78, 96, 71), color = colors$blue, type = "column") |>
    highcharter::hc_add_series(name = "活跃公司", data = c(580, 720, 1080, 1360, 1640, 1180), color = colors$cyan, type = "spline", yAxis = 1) |>
    highcharter::hc_add_series(name = "产品生态指数", data = c(35, 52, 61, 78, 98, 88), color = colors$red, type = "spline")
}

# 用途：绘制占位市场质量状态矩阵气泡图（流动性 vs 基本面质量）。
# 输入来源：函数内部通过 `placeholder_seed()` 固定随机种子生成的演示数据。
placeholder_quality_matrix <- function() {
  colors <- placeholder_chart_colors()
  placeholder_with_seed(placeholder_seed("quality_matrix", 5L), {
    levels <- c("低", "中", "高")
    level_colors <- c("低" = colors$green, "中" = colors$orange, "高" = colors$red)
    items <- lapply(levels, function(level) {
      n <- if (level == "低") 48 else if (level == "中") 32 else 16
      list(name = paste0(level, "风险"), color = level_colors[[level]], data = lapply(seq_len(n), function(i) list(x = round(stats::runif(1, 0.4, 5.6), 2), y = round(stats::runif(1, 0.5, 5.5), 2), z = round(stats::runif(1, 4, 56), 1))))
    })
    chart <- placeholder_chart_base("bubble") |>
      highcharter::hc_xAxis(title = list(text = "流动性得分"), plotLines = list(list(value = 3, color = "#CBD5E1", width = 1)), gridLineColor = colors$grid) |>
      highcharter::hc_yAxis(title = list(text = "基本面质量得分"), plotLines = list(list(value = 3, color = "#CBD5E1", width = 1)), gridLineColor = colors$grid) |>
      highcharter::hc_annotations(list(labels = list(list(point = list(x = 0.7, y = 5.2, xAxis = 0, yAxis = 0), text = "低流动性 · 高质量", style = list(color = colors$green, fontSize = "10px")), list(point = list(x = 5.2, y = 0.7, xAxis = 0, yAxis = 0), text = "高流动性 · 低质量", style = list(color = colors$orange, fontSize = "10px")))))
    for (item in items) chart <- highcharter::hc_add_series(chart, data = item$data, type = "bubble", name = item$name, color = item$color, marker = list(lineColor = "#FFFFFF", lineWidth = 0.8))
    chart
  })
}

# 用途：绘制占位风险类型与行业分布热力图。
# 输入来源：函数内部通过 `placeholder_seed()` 固定随机种子生成的演示数据。
placeholder_risk_heatmap <- function() {
  colors <- placeholder_chart_colors()
  industries <- c("电子", "机械设备", "医药生物", "电力设备", "计算机", "基础化工", "汽车", "其他")
  risks <- c("低流动性", "估值偏高", "财务风险", "合规风险", "募投风险", "退市风险")
  placeholder_with_seed(placeholder_seed("risk_heatmap", 7L), {
    points <- unlist(lapply(seq_along(industries), function(row) lapply(seq_along(risks), function(col) list(x = col - 1L, y = row - 1L, value = sample(2:48, 1)))), recursive = FALSE)
    placeholder_chart_base("heatmap") |>
      highcharter::hc_xAxis(categories = risks, lineColor = colors$grid, labels = list(style = list(color = "#52657A", fontSize = "10px"))) |>
      highcharter::hc_yAxis(categories = industries, title = list(text = NULL), reversed = TRUE, labels = list(style = list(color = "#52657A", fontSize = "10px"))) |>
      highcharter::hc_colorAxis(minColor = "#FFF4F2", maxColor = colors$red) |>
      highcharter::hc_add_series(data = points, type = "heatmap", name = "风险数量", borderWidth = 1, borderColor = "#FFFFFF", dataLabels = list(enabled = TRUE, format = "{point.value}", style = list(color = "#5B1F24", fontSize = "10px", textOutline = "none")))
  })
}

# 用途：根据图表类型分发到对应的占位图绘制函数。
# 输入来源：参数 `type`（页面组件类型标识，如 "market_bubble"、"industry_treemap" 等）。
render_placeholder_chart <- function(type) {
  if (!requireNamespace("highcharter", quietly = TRUE)) {
    return(chart_empty_state("未检测到 highcharter，已保留图表内容位。"))
  }
  switch(type,
    market_bubble = placeholder_market_bubble(),
    industry_treemap = placeholder_industry_treemap(),
    company_heatmap = placeholder_company_heatmap(),
    company_quadrant = placeholder_company_quadrant(),
    listing_financing = placeholder_listing_financing(),
    trading_ecosystem = placeholder_trading_ecosystem(),
    quality_matrix = placeholder_quality_matrix(),
    risk_heatmap = placeholder_risk_heatmap(),
    chart_empty_state("未识别的图形类型")
  )
}
