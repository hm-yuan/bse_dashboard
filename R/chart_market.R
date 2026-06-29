# Chart helpers are defined here because this file is sourced before the other
# chart modules. Chart functions consume metrics outputs only.

# 用途：检查当前 R 环境中是否可用 highcharter 包。
# 输入来源：无，直接调用 requireNamespace() 检查已安装的包。
chart_has_highcharter <- function() {
  requireNamespace("highcharter", quietly = TRUE)
}

# 用途：定义全局 Highcharter 配色，并供所有图表基础对象复用。
# 输入来源：无；颜色与页面视觉规范由统一主题维护。
chart_theme_palette <- function() {
  c(
    "#005BAC",
    "#0B2A5B",
    "#00A6C8",
    "#4E95D9",
    "#8DBCEB",
    "#BFD6EF",
    "#6F8095",
    "#22A06B",
    "#F59E0B",
    "#E3232E",
    "#D7ECFF",
    "#EEF5FC"
  )
}

# 用途：生成所有 Highcharter 图表共用的 economist 合并主题。
# 输入来源：`chart_theme_palette()` 定义的项目图表色板。
chart_bloom_theme <- function() {
  highcharter::hc_theme(
    colors = chart_theme_palette(),
    chart = list(
      backgroundColor = "transparent"
    ),
    title = list(style = list(color = "#0B2A5B", fontWeight = "700")),
    subtitle = list(style = list(color = "#65758A")),
    legend = list(itemStyle = list(color = "#26384D", fontWeight = "600")),
    xAxis = list(
      lineColor = "#B7CBE2",
      tickColor = "#B7CBE2",
      gridLineColor = "#E8F0F8",
      labels = list(style = list(color = "#65758A"))
    ),
    yAxis = list(
      lineColor = "#B7CBE2",
      tickColor = "#B7CBE2",
      gridLineColor = "#E8F0F8",
      labels = list(style = list(color = "#65758A"))
    )
  )
}

# 用途：返回图表配色方案（BSE 品牌色及辅助色）。
# 输入来源：无，函数内部定义的常量颜色值。
chart_colors <- function() {
  list(
    bse_blue = "#005BAC",
    bse_blue_soft = "#8DBCEB",
    bse_cyan = "#00A6C8",
    navy = "#0B2A5B",
    slate = "#6F8095",
    grid = "#E8F0F8",
    success = "#22A06B",
    warning = "#F59E0B",
    danger = "#E3232E",
    risk_orange = "#F59E0B",
    risk_light = "#FFF3D0"
  )
}

# 用途：将输入值安全转换为数值型，非数值返回 NA。
# 输入来源：参数 `x`，通常为数据框列或向量。
chart_safe_number <- function(x) {
  suppressWarnings(as.numeric(x))
}

# 用途：将数值转换为百分比字符串（如 0.123 → "12.3%"）。
# 输入来源：参数 `x`（待转换数值）和 `digits`（小数位）。
chart_format_pct <- function(x, digits = 1) {
  value <- chart_safe_number(x)
  ifelse(is.finite(value), paste0(format(round(value * 100, digits), trim = TRUE, nsmall = digits), "%"), "-")
}

# 用途：按指定列降序排列并取前 N 行。
# 输入来源：参数 `df`（数据框）、`column`（排序列名）和 `n`（行数）。
chart_top_n <- function(df, column, n = 12L) {
  if (!is.data.frame(df) || !column %in% names(df)) {
    return(df)
  }
  df[order(chart_safe_number(df[[column]]), decreasing = TRUE, na.last = TRUE)[seq_len(min(nrow(df), n))], , drop = FALSE]
}

# 用途：创建 highcharter 图表基础对象（统一尺寸、标题、图例、提示框样式）。
# 输入来源：参数 `type`（图表类型）和 `height`（高度像素）。
chart_hc_base <- function(type = NULL, height = NULL) {
  colors <- chart_colors()
  hc <- if (identical(type, "map")) highcharter::highchart(type = "map") else highcharter::highchart()
  hc <- hc |>
    highcharter::hc_add_theme(chart_bloom_theme()) |>
    highcharter::hc_chart(
      type = type,
      backgroundColor = "transparent"
    ) |>
    highcharter::hc_credits(enabled = FALSE) |>
    highcharter::hc_add_dependency("highcharts-more.js") |>
    highcharter::hc_add_dependency("modules/heatmap.js") |>
    highcharter::hc_add_dependency("modules/export-data.js") |>
    highcharter::hc_add_dependency("modules/full-screen.js") |>
    highcharter::hc_exporting(
      enabled = TRUE,
      buttons = list(
        contextButton = list(
          menuItems = c("viewFullscreen", "printChart", "separator", "downloadPNG", "downloadJPEG", "downloadPDF", "downloadSVG", "separator", "downloadCSV", "downloadXLS")
        )
      )
    ) |>
    highcharter::hc_legend(
      itemStyle = list(color = colors$navy, fontSize = "11px", fontWeight = "500"),
      itemHoverStyle = list(color = colors$bse_blue),
      symbolRadius = 2
    ) |>
    highcharter::hc_tooltip(
      backgroundColor = "#FFFFFF",
      borderColor = "#B7CBE2",
      borderRadius = 4,
      shadow = FALSE,
      style = list(color = colors$navy, fontSize = "12px")
    )

  if (!is.null(height)) {
    hc <- highcharter::hc_size(hc, height = height)
  }
  hc
}

# 用途：生成 highcharter 坐标轴配置列表。
# 输入来源：参数 `title`、`categories`、`opposite`、`min`、`max`、`plot_lines`、`reversed`、`type`、`tick_positions`。
chart_hc_axis <- function(title, categories = NULL, opposite = FALSE, min = NULL, max = NULL, plot_lines = NULL, reversed = FALSE, type = NULL, tick_positions = NULL, labels = NULL) {
  colors <- chart_colors()
  axis <- list(
    title = list(text = title, style = list(color = "#60758D", fontSize = "11px", fontWeight = "500")),
    opposite = opposite,
    lineColor = "#C8D5E3",
    tickColor = "#C8D5E3",
    gridLineColor = colors$grid,
    labels = list(style = list(color = "#60758D", fontSize = "11px"))
  )
  if (!is.null(categories)) axis$categories <- categories
  if (!is.null(min)) axis$min <- min
  if (!is.null(max)) axis$max <- max
  if (!is.null(plot_lines)) axis$plotLines <- plot_lines
  if (isTRUE(reversed)) axis$reversed <- TRUE
  if (!is.null(type)) axis$type <- type
  if (!is.null(tick_positions)) axis$tickPositions <- tick_positions
  if (!is.null(labels)) axis$labels <- utils::modifyList(axis$labels, labels)
  axis
}

# 用途：为 highcharter 图表添加 X 轴。
# 输入来源：参数 `hc`（highcharter 对象）及 `chart_hc_axis()` 生成的坐标轴配置。
hc_x_axis <- function(hc, title, categories = NULL, opposite = FALSE, min = NULL, max = NULL, plot_lines = NULL, reversed = FALSE, type = NULL, tick_positions = NULL, labels = NULL) {
  args <- chart_hc_axis(title, categories, opposite, min, max, plot_lines, reversed, type, tick_positions, labels)
  do.call(highcharter::hc_xAxis, c(list(hc = hc), args))
}

# 用途：为 highcharter 图表添加 Y 轴。
# 输入来源：参数 `hc`（highcharter 对象）及 `chart_hc_axis()` 生成的坐标轴配置。
hc_y_axis <- function(hc, title, categories = NULL, opposite = FALSE, min = NULL, max = NULL, plot_lines = NULL, reversed = FALSE, type = NULL, tick_positions = NULL, labels = NULL) {
  args <- chart_hc_axis(title, categories, opposite, min, max, plot_lines, reversed, type, tick_positions, labels)
  do.call(highcharter::hc_yAxis, c(list(hc = hc), args))
}

# 用途：将数据框按指定列转换为气泡图点列表（含 x、y、z、名称及可选颜色）。
# 输入来源：参数 `df` 及列名 `x`、`y`、`z`、`name`、`color`、`extra`。
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

# 用途：将不等距的实际数值映射至等宽刻度区间，供分段坐标轴使用。
# 输入来源：数值向量 `values` 与按升序排列的刻度断点 `breaks`。
chart_equal_interval_position <- function(values, breaks) {
  values <- chart_safe_number(values)
  breaks <- sort(unique(chart_safe_number(breaks)))
  if (length(breaks) < 2L) return(rep(NA_real_, length(values)))

  segment <- findInterval(values, breaks)
  segment <- pmin(pmax(segment, 1L), length(breaks) - 1L)
  lower <- breaks[segment]
  upper <- breaks[segment + 1L]
  (segment - 1L) + (values - lower) / (upper - lower)
}

# 用途：生成等距分段轴标签，将轴坐标 0, 1, 2… 还原为原始刻度文本。
chart_equal_interval_labels <- function(breaks) {
  labels <- as.character(format(breaks, big.mark = ",", scientific = FALSE, trim = TRUE))
  highcharter::JS(sprintf(
    "function(){ var labels=%s, index=Math.round(this.value); return labels[index] || ''; }",
    jsonlite::toJSON(labels, auto_unbox = TRUE)
  ))
}

# 用途：绘制市场定位气泡图（总市值 vs 日均成交额，气泡大小表示上市公司数量）。
# 输入来源：`calc_market_position_bubble()` 的输出数据框。
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

  # 用途：为各市场指定固定顺序与配色，与市场板块成交统计图保持一致。
  board_order <- c("上证主板", "深证主板", "创业板", "科创板", "北交所")
  board_palette <- c("#0B2A5B", "#4E95D9", "#BFD6EF", "#6F8095", "#00A6C8"
)
  names(board_palette) <- board_order
  # 调色盘采用现代蓝色为主，北交所使用品牌蓝高亮。
  # 气泡图数据中的市场名称与板块配色名称略有差异，建立映射关系。
  market_to_board <- c(
    "沪市主板" = "上证主板",
    "深市主板" = "深证主板",
    "创业板" = "创业板",
    "科创板" = "科创板",
    "北交所" = "北交所"
  )

  plot_df <- df
  plot_df$board_key <- market_to_board[plot_df$market]
  plot_df$board_key[is.na(plot_df$board_key)] <- as.character(plot_df$market[is.na(plot_df$board_key)])
  plot_df$marker_color <- board_palette[plot_df$board_key]
  plot_df$marker_color[is.na(plot_df$marker_color)] <- "#60758D"

  hc <- chart_hc_base("bubble") |>
    hc_x_axis("总市值（亿元）") |>
    hc_y_axis("日均成交额（亿元）", min = 0) |>
    highcharter::hc_plotOptions(
      bubble = list(minSize = 15, maxSize = 100, marker = list(fillOpacity = 1))
    ) |>
    highcharter::hc_tooltip(
      pointFormat = "<b>{point.name}</b><br/>总市值：{point.x:,.0f} 亿元<br/>日均成交额：{point.y:,.1f} 亿元<br/>上市公司：{point.z:,.0f} 家"
    ) |>
    highcharter::hc_legend(
      enabled = TRUE,
      layout = "horizontal",
      align = "center",
      verticalAlign = "bottom"
    )

  # 用途：按固定顺序为每个市场单独建 series，确保图例中每个市场都有独立条目。
  present_boards <- intersect(board_order, plot_df$board_key)
  for (b in present_boards) {
    sub <- plot_df[plot_df$board_key == b, , drop = FALSE]
    points <- chart_bubble_points(
      sub, "total_market_cap_yi", "avg_daily_turnover_yi", "listed_company_count", "market", "marker_color",
      extra = function(data, i) {
        list(
          dataLabels = list(
            enabled = TRUE,
            format = as.character(data$market[[i]]),
            align = "center",
            verticalAlign = "bottom",
            allowOverlap = TRUE,
            crop = FALSE,
            positioner = highcharter::JS("function(labelWidth, labelHeight, point) {
              var radius = point.marker && point.marker.radius ? point.marker.radius : 20;
              return {
                x: point.plotX - labelWidth / 2,
                y: point.plotY + radius + 16
              };
            }"),
            style = list(color = "#12365D", fontSize = "12px", fontWeight = "600", textOutline = "none")
          )
        )
      }
    )
    hc <- hc |>
      highcharter::hc_add_series(
        name = b,
        type = "bubble",
        color = board_palette[[b]],
        data = points
      )
  }

  chart_widget(hc)
}

# 用途：绘制企业市值-市盈率散点图，横轴为市值，纵轴为市盈率。
#       按所属板块着色，支持 tooltip 显示公司名称。
# 输入来源：`calc_company_pe_market_cap_data()` 返回的公司级数据框。
plot_company_pe_market_cap_scatter <- function(df) {
  required <- c("company_name", "board", "market_cap_yi", "pe")
  if (!is.data.frame(df) || nrow(df) == 0L || !all(required %in% names(df))) {
    return(chart_empty_state("暂无市值-市盈率散点数据"))
  }
  if (!chart_has_highcharter()) {
    return(chart_fallback_table("市值-市盈率散点", df, "未检测到 highcharter，已显示原始数据。"))
  }

  board_order <- c("上证主板", "深证主板", "创业板", "科创板", "北交所")
  board_palette <- c("#0B2A5B", "#4E95D9", "#6F8095", "#BFD6EF", "#00A6C8")
  names(board_palette) <- board_order
  # 调色盘采用现代蓝色为主，北交所使用品牌蓝高亮。

  df$board <- factor(df$board, levels = board_order)
  df <- df[df$board %in% board_order, , drop = FALSE]

  x_ticks <- c(0, 10, 20, 30, 50, 100, 200, 400, 800, 1000, 2000, 5000, 10000, 25000)
  y_ticks <- c(0, 5, 10, 20, 40, 50, 100, 200, 400, 800, 2000, 10000, 50000)

  df <- df[df$market_cap_yi >= min(x_ticks) & df$market_cap_yi <= max(x_ticks) &
             df$pe >= min(y_ticks) & df$pe <= max(y_ticks), , drop = FALSE]

  df$x_axis_position <- chart_equal_interval_position(df$market_cap_yi, x_ticks)
  df$y_axis_position <- chart_equal_interval_position(df$pe, y_ticks)
  x_axis_labels <- list(
    formatter = chart_equal_interval_labels(x_ticks),
    overflow = "allow",
    crop = FALSE
  )
  y_axis_labels <- list(formatter = chart_equal_interval_labels(y_ticks))

  x_plot_lines <- lapply(board_order, function(b) {
    sub <- df[df$board == b, "market_cap_yi", drop = TRUE]
    if (length(sub) == 0L) return(NULL)
    med <- round(stats::median(sub, na.rm = TRUE))
    pos <- chart_equal_interval_position(med, x_ticks)
    list(
      color = board_palette[[b]],
      width = 1.5,
      value = pos,
      zIndex = 3,
      dashStyle = "ShortDash",   # 新增：虚线
      label = list(
        text = paste0(b, "中位数", med, "亿元"),
        align = "center",
        verticalAlign = "top",
        y = 35,
        style = list(color = board_palette[[b]], fontSize = "9px", fontWeight = "600")
      )
    )
  })
  x_plot_lines <- Filter(Negate(is.null), x_plot_lines)

  y_plot_lines <- lapply(board_order, function(b) {
  sub <- df[df$board == b, "pe", drop = TRUE]
  if (length(sub) == 0L) return(NULL)

  med <- round(stats::median(sub, na.rm = TRUE))
  pos <- chart_equal_interval_position(med, y_ticks)

  list(
    color = board_palette[[b]],
    width = 1.5,
    value = pos,
    zIndex = 3,
    dashStyle = "Dot",   # 新增：虚线
    label = list(
      text = paste0(b, "市盈率中位数", med, "倍"),
      align = "right",
      verticalAlign = "middle",
      x = 8,
      style = list(
        color = board_palette[[b]],
        fontSize = "9px",
        fontWeight = "600"
      )
    )
  )
})
  y_plot_lines <- Filter(Negate(is.null), y_plot_lines)

  hc <- chart_hc_base("scatter") |>
    hc_x_axis("总市值（亿元）", min = 0, max = length(x_ticks) - 1L, tick_positions = seq_along(x_ticks) - 1L, labels = x_axis_labels, plot_lines = x_plot_lines) |>
    hc_y_axis("市盈率（倍）", min = 0, max = length(y_ticks) - 1L, tick_positions = seq_along(y_ticks) - 1L, labels = y_axis_labels, plot_lines = y_plot_lines) |>
    highcharter::hc_plotOptions(
      scatter = list(
        turboThreshold = 0,
        marker = list(radius = 3, symbol = "circle"),
        tooltip = list(headerFormat = "")
      )
    ) |>
    highcharter::hc_tooltip(
      pointFormat = "<b>{point.name}</b><br/>板块：{point.board}<br/>总市值：{point.market_cap:,.0f} 亿元<br/>市盈率：{point.pe:,.1f} 倍"
    ) |>
    highcharter::hc_legend(
      enabled = TRUE,
      layout = "horizontal",
      align = "center",
      verticalAlign = "bottom"
    )

  for (b in board_order) {
    sub <- df[df$board == b, , drop = FALSE]
    if (nrow(sub) == 0L) next
    points <- lapply(seq_len(nrow(sub)), function(i) {
      list(
        name = as.character(sub$company_name[[i]]),
        x = sub$x_axis_position[[i]],
        y = sub$y_axis_position[[i]],
        board = b,
        market_cap = chart_safe_number(sub$market_cap_yi[[i]]),
        pe = chart_safe_number(sub$pe[[i]])
      )
    })
    hc <- hc |>
      highcharter::hc_add_series(
        name = b,
        type = "scatter",
        color = board_palette[[b]],
        data = points
      )
  }

  hc
}

# 用途：绘制企业所有权性质百分比堆积柱状图，横轴为板块，纵轴为百分比。
#       固定 5 个板块、4 类企业性质及配色：国有企业、民营企业、外资企业、公众企业。
# 输入来源：`calc_enterprise_nature_data()` 返回的数据框。
plot_enterprise_nature_bar <- function(df) {
  required <- c("board", "nature", "count", "pct")
  if (!is.data.frame(df) || nrow(df) == 0L || !all(required %in% names(df))) {
    return(chart_empty_state("暂无企业性质数据，请确认 Excel 中包含[企业性质]列。"))
  }
  if (!chart_has_highcharter()) {
    return(chart_fallback_table("企业性质", df, "未检测到 highcharter，已显示原始数据。"))
  }

  boards <- c("上证主板", "深证主板", "创业板", "科创板", "北交所")
  natures <- c("国有企业", "民营企业", "外资企业", "公众企业")
  nature_colors <- c(
    "国有企业" = "#0B2A5B",
    "民营企业" = "#005BAC",
    "外资企业" = "#D8D8D8",
    "公众企业" = "#00A6C8"
  )

  hc <- chart_hc_base("column") |>
    highcharter::hc_chart(type = "column") |>
    hc_x_axis("", categories = boards) |>
    hc_y_axis("占比（%）", min = 0, max = 100) |>
    highcharter::hc_plotOptions(
      column = list(
        stacking = "percent",
        dataLabels = list(
          enabled = TRUE,
          format = "{point.percentage:.0f}%",
          style = list(color = "#FFFFFF", fontSize = "10px", fontWeight = "500", textOutline = "none", textShadow = "0 1px 2px rgba(0,0,0,0.45)"),
          allowOverlap = TRUE,
          crop = FALSE,
          overflow = "allow"
        )
      )
    ) |>
    highcharter::hc_tooltip(
      pointFormat = "<b>{series.name}</b><br/>板块：{point.category}<br/>占比：{point.percentage:.1f}%<br/>公司数：{point.count} 家"
    )

  for (nat in natures) {
    series_data <- lapply(boards, function(b) {
      sub <- df[df$board == b & df$nature == nat, , drop = FALSE]
      if (nrow(sub) == 0L) return(list(y = 0, count = 0))
      list(y = sub$count[[1L]], count = sub$count[[1L]])
    })
    hc <- hc |>
      highcharter::hc_add_series(
        name = nat,
        type = "column",
        color = nature_colors[[nat]],
        data = series_data
      )
  }

  hc
}

# 用途：绘制北交所研发强度超过阈值的企业比例指示图。
#       使用水平堆叠条形图，左侧为达标比例，右侧为未达标比例。
# 输入来源：`calc_bse_rd_intensity_ratio()` 返回的 0~1 比例值。
plot_bse_rd_intensity_indicator <- function(ratio) {
  if (!chart_has_highcharter()) {
    return(chart_empty_state("未检测到 highcharter"))
  }

  ratio <- chart_safe_number(ratio)
  if (length(ratio) == 0L || is.na(ratio)) ratio <- 0.62
  ratio <- max(0, min(1, ratio))

  hc <- chart_hc_base("bar", height = 120) |>
    highcharter::hc_chart(type = "bar", backgroundColor = "transparent", margin = c(10, 10, 10, 10)) |>
    highcharter::hc_title(text = NULL) |>
    highcharter::hc_xAxis(myaxis = list(enabled = FALSE)) |>
    highcharter::hc_yAxis(myaxis = list(enabled = FALSE, min = 0, max = 100)) |>
    highcharter::hc_plotOptions(
      bar = list(
        stacking = "percent",
        borderWidth = 0,
        pointPadding = 0.15,
        groupPadding = 0,
        dataLabels = list(
          enabled = TRUE,
          format = "{y}%",
          style = list(color = "#FFFFFF", fontSize = "12px", fontWeight = "700", textOutline = "none")
        )
      )
    ) |>
    highcharter::hc_legend(enabled = FALSE) |>
    highcharter::hc_tooltip(
      pointFormat = "<b>{series.name}</b>: {point.y}%"
    ) |>
    highcharter::hc_add_series(
      name = "研发强度 > 5%",
      type = "bar",
      color = "#0B2A5B",
      data = list(round(ratio * 100, 1))
    ) |>
    highcharter::hc_add_series(
      name = "其他",
      type = "bar",
      color = "#005BAC",
      data = list(round((1 - ratio) * 100, 1))
    )

  hc
}

# 用途：绘制北交所国家级专精特新企业数量指示图。
#       使用圆环图展示国家级专精特新企业数量占北交所总数的比例。
# 输入来源：`calc_bse_specialized_new_count()` 返回的整数数量。
plot_bse_specialized_new_indicator <- function(count) {
  if (!chart_has_highcharter()) {
    return(chart_empty_state("未检测到 highcharter"))
  }

  count <- chart_safe_number(count)
  if (length(count) == 0L || is.na(count)) count <- 186
  count <- as.integer(max(0, count))

  # 尝试读取上市公司基本情况表统计北交所总数，失败则使用演示总数
  total <- 260L
  path <- "data/raw/上市公司基本情况.xlsx"
  if (file.exists(path) && requireNamespace("readxl", quietly = TRUE)) {
    raw <- as.data.frame(readxl::read_excel(path, sheet = "公司", .name_repair = "unique"), stringsAsFactors = FALSE)
    if ("上市板块" %in% names(raw)) {
      n <- sum(as.character(raw[["上市板块"]]) == "北证", na.rm = TRUE)
      if (n > 0L) total <- as.integer(n)
    }
  }

  other <- max(total - count, 0L)
  pct <- if (total > 0L) round(count / total * 100, 1) else 0
  pie_data <- list(
    list(name = "国家级专精特新", y = count, color = "#005BAC"),
    list(name = "其他", y = other, color = "#BFD6EF")
  )

  title_html <- paste0(
    "<span style='font-size:22px;font-weight:700;color:#005BAC'>", pct,
    "</span><span style='font-size:14px;font-weight:600;color:#005BAC'>%</span><br/><span style='font-size:10px;color:#6B7280'>国家级专精特新</span>"
  )

  hc <- chart_hc_base("pie", height = 120) |>
    highcharter::hc_chart(type = "pie", backgroundColor = "transparent", margin = c(0, 0, 0, 0)) |>
    highcharter::hc_title(
      text = title_html,
      useHTML = TRUE,
      align = "center",
      verticalAlign = "middle",
      y = 0,
      style = list(fontSize = "0px")
    ) |>
    highcharter::hc_plotOptions(
      pie = list(
        innerSize = "65%",
        startAngle = 0,
        endAngle = 360,
        borderWidth = 0,
        dataLabels = list(enabled = FALSE),
        showInLegend = FALSE,
        center = list("50%", "50%")
      )
    ) |>
    highcharter::hc_legend(enabled = FALSE) |>
    highcharter::hc_tooltip(
      pointFormat = "<b>{point.name}</b><br/>数量：{point.y} 家 ({point.percentage:.1f}%)"
    ) |>
    highcharter::hc_add_series(
      name = "企业数量",
      type = "pie",
      data = pie_data
    ) |>
    highcharter::hc_credits(enabled = FALSE)

  hc
}

# 用途：绘制行业市值结构横向条形图。
# 输入来源：`calc_market_industry_structure()` 的输出数据框。
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
      hc_x_axis("", categories = as.character(plot_df$industry)) |>
      hc_y_axis("总市值（亿元）") |>
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

# 用途：绘制可下钻的行业矩形树图。
#       第一层显示各行业占比，点击行业色块下钻到第二层查看该行业具体公司。
#       图表右上角使用 highcharts 内置自定义按钮，在“公司家数”与“公司市值”之间切换色块大小单位。
# 输入来源：`data`（dashboard 数据列表，包含 `market_position_company_detail` 公司明细）。
plot_market_industry_treemap <- function(data) {
  df <- calc_market_industry_structure(data)
  detail <- metric_table(data, "market_position_company_detail")

  required <- c("industry", "market_cap_yi", "market_cap_share", "company_count", "pe_median")
  if (!is.data.frame(df) || nrow(df) == 0L || !all(required %in% names(df))) {
    return(chart_empty_state("暂无行业市值结构数据"))
  }
  if (!chart_has_highcharter()) {
    return(chart_fallback_table("行业市值结构", df, "未检测到 highcharter，已显示行业汇总数据。"))
  }

  plot_df <- chart_top_n(df, "market_cap_yi", 12L)
  plot_df <- plot_df[order(chart_safe_number(plot_df$market_cap_yi), decreasing = TRUE), , drop = FALSE]

  total_count <- sum(chart_safe_number(plot_df$company_count), na.rm = TRUE)
  total_cap <- sum(chart_safe_number(plot_df$market_cap_yi), na.rm = TRUE)

  has_detail <- metric_has_cols(detail, c("company_name", "industry", "total_market_cap_yi"))

  # 行业父节点：按行业市值排名使用品牌蓝连续深浅渐变
  n_industries <- max(nrow(plot_df), 2L)
  industry_palette <- grDevices::colorRampPalette(c("#005BAC", "#D7ECFF"))(n_industries)

  build_parent_nodes <- function(size_by = "count") {
    lapply(seq_len(nrow(plot_df)), function(i) {
      count <- as.integer(plot_df$company_count[[i]])
      cap <- chart_safe_number(plot_df$market_cap_yi[[i]])
      list(
        id = paste0("industry_", i),
        parent = "root",
        name = as.character(plot_df$industry[[i]]),
        color = industry_palette[[i]],
        value = if (identical(size_by, "market_cap")) cap else count,
        count_share = if (total_count > 0) chart_safe_number(count) / total_count * 100 else NA_real_,
        cap_share = if (total_cap > 0) cap / total_cap * 100 else NA_real_,
        count = count,
        cap = cap
      )
    })
  }

  # 公司叶子节点：按公司市值排名使用品牌蓝连续深浅渐变
  build_company_nodes <- function(size_by = "count") {
    if (!has_detail) return(list())
    company_df <- detail[detail$industry %in% plot_df$industry, , drop = FALSE]
    company_df <- company_df[!is.na(chart_safe_number(company_df$total_market_cap_yi)), , drop = FALSE]
    company_df <- company_df[order(-chart_safe_number(company_df$total_market_cap_yi), na.last = TRUE), , drop = FALSE]
    company_df$industry_id <- paste0("industry_", match(company_df$industry, plot_df$industry))

    n_companies <- max(nrow(company_df), 2L)
    company_palette <- grDevices::colorRampPalette(c("#005BAC", "#D7ECFF"))(n_companies)

    lapply(seq_len(nrow(company_df)), function(i) {
      cap <- chart_safe_number(company_df$total_market_cap_yi[[i]])
      list(
        name = as.character(company_df$company_name[[i]]),
        parent = as.character(company_df$industry_id[[i]]),
        value = if (identical(size_by, "market_cap")) cap else 1L,
        color = company_palette[[i]],
        cap = cap,
        industry = as.character(company_df$industry[[i]])
      )
    })
  }

  parents_count <- build_parent_nodes("count")
  parents_cap <- build_parent_nodes("market_cap")
  root_count <- list(id = "root", name = "全部行业", value = total_count, color = "transparent")
  root_cap <- list(id = "root", name = "全部行业", value = total_cap, color = "transparent")
  data_count <- c(list(root_count), parents_count, build_company_nodes("count"))
  data_cap <- c(list(root_cap), parents_cap, build_company_nodes("market_cap"))

  series_count <- list(
    name = "公司家数",
    visible = TRUE,
    type = "treemap",
    layoutAlgorithm = "squarified",
    rootId = "root",
    allowDrillToNode = TRUE,
    levelIsConstant = FALSE,
    data = data_count,
    levels = list(
      list(
        level = 2,
        dataLabels = list(
          enabled = TRUE,
          formatter = highcharter::JS("function(){return '<span style=\"font-size:11px;font-weight:600;\">'+this.point.name+'</span><br/><span style=\"font-size:10px;\">'+Highcharts.numberFormat(this.point.count_share,1)+'%</span>';}")
        )
      ),
      list(
        level = 3,
        dataLabels = list(
          enabled = TRUE,
          formatter = highcharter::JS("function(){return '<span style=\"font-size:10px;font-weight:500;\">'+this.point.name+'</span>';}")
        )
      )
    )
  )

  series_cap <- list(
    name = "公司市值",
    visible = FALSE,
    type = "treemap",
    layoutAlgorithm = "squarified",
    rootId = "root",
    allowDrillToNode = TRUE,
    levelIsConstant = FALSE,
    data = data_cap,
    levels = list(
      list(
        level = 2,
        dataLabels = list(
          enabled = TRUE,
          formatter = highcharter::JS("function(){return '<span style=\"font-size:11px;font-weight:600;\">'+this.point.name+'</span><br/><span style=\"font-size:10px;\">'+Highcharts.numberFormat(this.point.cap_share,1)+'%</span>';}")
        )
      ),
      list(
        level = 3,
        dataLabels = list(
          enabled = TRUE,
          formatter = highcharter::JS("function(){return '<span style=\"font-size:10px;font-weight:500;\">'+this.point.name+'</span>';}")
        )
      )
    )
  )

  toggle_js <- highcharter::JS("function(){
    var chart = this;
    var s0 = chart.series[0];
    var s1 = chart.series[1];
    var btn = chart.exporting.buttons.customButton;
    var textEl = btn && btn.text ? btn.text : (btn && btn.element ? btn.element.querySelector('.highcharts-button-text') : null);
    if (s0.visible) {
      if (s0.setRootNode) s0.setRootNode('root', false);
      s0.hide();
      s1.show();
      if (s1.setRootNode) s1.setRootNode('root', false);
      chart.setTitle({text:'本所上市公司行业结构（按公司市值）'});
      if (textEl) textEl.attr ? textEl.attr({text:'切换为公司家数'}) : (textEl.textContent = '切换为公司家数');
    } else {
      if (s1.setRootNode) s1.setRootNode('root', false);
      s0.show();
      s1.hide();
      if (s0.setRootNode) s0.setRootNode('root', false);
      chart.setTitle({text:'本所上市公司行业结构（按公司家数）'});
      if (textEl) textEl.attr ? textEl.attr({text:'切换为公司市值'}) : (textEl.textContent = '切换为公司市值');
    }
  }")

  chart_widget(
    chart_hc_base("treemap") |>
      highcharter::hc_add_dependency("modules/treemap.js") |>
      highcharter::hc_add_dependency("modules/breadcrumbs.js") |>
      highcharter::hc_title(
        text = "本所上市公司行业结构（按公司家数）",
        style = list(color = "#12365D", fontSize = "14px", fontWeight = "600")
      ) |>
      highcharter::hc_subtitle(
        text = "点击行业色块下钻查看公司明细",
        style = list(color = "#60758D", fontSize = "11px", fontWeight = "400")
      ) |>
      highcharter::hc_add_series_list(list(series_count, series_cap)) |>
      highcharter::hc_plotOptions(
        treemap = list(
          borderWidth = 1,
          borderColor = "#FFFFFF",
          drillUpButton = list(relativeTo = "spacingBox", position = list(x = 0, y = 0))
        )
      ) |>
      highcharter::hc_tooltip(
        formatter = highcharter::JS("function(){
      var d = this.point, mode = this.series.name;
      if (d.parent && d.parent !== 'root') {
        if (mode === '公司家数') {
          return '<b>' + d.name + '</b><br/>所属行业：' + d.industry + '<br/>公司家数：1 家<br/>总市值：' + Highcharts.numberFormat(d.cap, 1) + ' 亿元';
        }
        return '<b>' + d.name + '</b><br/>所属行业：' + d.industry + '<br/>总市值：' + Highcharts.numberFormat(d.value, 1) + ' 亿元';
      }
      if (mode === '公司家数') {
        return '<b>' + d.name + '</b><br/>公司数：' + d.count + ' 家<br/>总市值：' + Highcharts.numberFormat(d.cap, 1) + ' 亿元<br/>占比（按家数）：' + Highcharts.numberFormat(d.count_share, 1) + '%<br/>点击下钻查看明细';
      }
      return '<b>' + d.name + '</b><br/>公司数：' + d.count + ' 家<br/>总市值：' + Highcharts.numberFormat(d.cap, 1) + ' 亿元<br/>占比（按市值）：' + Highcharts.numberFormat(d.cap_share, 1) + '%<br/>点击下钻查看明细';
    }")
      ) |>
      highcharter::hc_exporting(
        enabled = TRUE,
        buttons = list(
          contextButton = list(enabled = FALSE),
          customButton = list(
            text = "切换为公司市值",
            align = "right",
            verticalAlign = "top",
            x = -10,
            y = 10,
            theme = list(
              fill = "#FFFFFF",
              stroke = "#C9D8E8",
              r = 4,
              states = list(hover = list(fill = "#E6EDF5")),
              style = list(color = "#12365D", fontSize = "12px", fontWeight = "500")
            ),
            onclick = toggle_js
          )
        )
      ) |>
      highcharter::hc_legend(enabled = FALSE)
  )
}

# 用途：渲染市场定位页的两层矩形树图。初始层只展示行业；选中行业后才展示该行业公司。
# 输入来源：`market_position_company_detail` 与行业汇总结果；`selected_industry` 由 Shiny 模块维护。
plot_market_industry_treemap_drill <- function(data,
                                               selected_industry = NULL,
                                               size_by = c("count", "market_cap"),
                                               click_input_id = NULL) {
  size_by <- match.arg(size_by)
  industry <- calc_market_industry_structure(data)
  detail <- metric_table(data, "market_position_company_detail")

  if (!chart_has_highcharter()) {
    return(chart_fallback_table("行业结构", industry, "未检测到 highcharter，已显示行业汇总数据。"))
  }

  colors <- chart_colors()
  is_company_level <- !is.null(selected_industry) && nzchar(selected_industry)
  metric_label <- if (identical(size_by, "market_cap")) "公司市值" else "公司家数"
  ranked_industry <- industry[order(chart_safe_number(industry$market_cap_yi), decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  top_limit <- 19L
  top_industries <- utils::head(as.character(ranked_industry$industry), top_limit)

  if (!is_company_level) {
    required <- c("industry", "company_count", "market_cap_yi", "market_cap_share")
    if (!is.data.frame(industry) || nrow(industry) == 0L || !all(required %in% names(industry))) {
      return(chart_empty_state("暂无行业结构数据"))
    }

    plot_df <- utils::head(ranked_industry, top_limit)
    if (nrow(ranked_industry) > top_limit) {
      other_rows <- ranked_industry[(top_limit + 1L):nrow(ranked_industry), , drop = FALSE]
      other_row <- other_rows[1, , drop = FALSE]
      other_row[] <- NA
      other_row$industry <- "其他行业"
      other_row$company_count <- sum(chart_safe_number(other_rows$company_count), na.rm = TRUE)
      other_row$market_cap_yi <- sum(chart_safe_number(other_rows$market_cap_yi), na.rm = TRUE)
      other_row$market_cap_share <- sum(chart_safe_number(other_rows$market_cap_share), na.rm = TRUE)
      plot_df <- rbind(plot_df, other_row)
    }
    palette <- grDevices::colorRampPalette(c("#005BAC", "#D7ECFF"))(nrow(plot_df))
    points <- lapply(seq_len(nrow(plot_df)), function(i) {
      list(
        name = as.character(plot_df$industry[[i]]),
        value = if (identical(size_by, "market_cap")) chart_safe_number(plot_df$market_cap_yi[[i]]) else chart_safe_number(plot_df$company_count[[i]]),
        color = palette[[i]],
        company_count = chart_safe_number(plot_df$company_count[[i]]),
        market_cap = chart_safe_number(plot_df$market_cap_yi[[i]]),
        share = chart_safe_number(plot_df$market_cap_share[[i]]) * 100,
        industry_key = if (identical(plot_df$industry[[i]], "其他行业")) "__other__" else as.character(plot_df$industry[[i]])
      )
    })

    click_event <- if (!is.null(click_input_id)) {
      highcharter::JS(sprintf("function(){Shiny.setInputValue('%s', this.options.industry_key, {priority:'event'});}", click_input_id))
    } else {
      NULL
    }

    return(
      chart_hc_base("treemap") |>
          highcharter::hc_add_dependency("modules/treemap.js") |>
          highcharter::hc_plotOptions(treemap = list(layoutAlgorithm = "squarified", borderWidth = 1, borderColor = "#FFFFFF", dataLabels = list(enabled = TRUE, formatter = highcharter::JS("function(){return this.point.name+'<br/>'+Highcharts.numberFormat(this.point.value,0)+(this.series.name === '公司市值' ? ' 亿元' : ' 家');}")), cursor = "pointer"), series = list(point = list(events = list(click = click_event)))) |>
          highcharter::hc_add_series(name = metric_label, type = "treemap", data = points) |>
          highcharter::hc_tooltip(formatter = highcharter::JS("function(){var p=this.point;return '<b>'+p.name+'</b><br/>公司数：'+Highcharts.numberFormat(p.company_count,0)+' 家<br/>总市值：'+Highcharts.numberFormat(p.market_cap,1)+' 亿元<br/>市值占比：'+Highcharts.numberFormat(p.share,1)+'%<br/>点击查看公司';}")) |>
          highcharter::hc_legend(enabled = FALSE)
    )
  }

  is_other <- identical(selected_industry, "__other__")
  company_df <- if (is_other) {
    detail[!detail$industry %in% top_industries, , drop = FALSE]
  } else {
    detail[detail$industry == selected_industry, , drop = FALSE]
  }
  selected_label <- if (is_other) "其他行业" else selected_industry
  required <- c("company_name", "total_market_cap_yi")
  if (!is.data.frame(company_df) || nrow(company_df) == 0L || !all(required %in% names(company_df))) {
    return(chart_empty_state(paste0(selected_label, "暂无公司明细")))
  }

  company_df <- company_df[order(chart_safe_number(company_df$total_market_cap_yi), decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  palette <- grDevices::colorRampPalette(c("#005BAC", "#D7ECFF"))(nrow(company_df))
  points <- lapply(seq_len(nrow(company_df)), function(i) {
    list(
      name = as.character(company_df$company_name[[i]]),
      value = if (identical(size_by, "market_cap")) chart_safe_number(company_df$total_market_cap_yi[[i]]) else 1,
      color = palette[[i]],
      market_cap = chart_safe_number(company_df$total_market_cap_yi[[i]]),
      company_code = if ("company_code" %in% names(company_df)) as.character(company_df$company_code[[i]]) else ""
    )
  })

  chart_hc_base("treemap") |>
      highcharter::hc_add_dependency("modules/treemap.js") |>
      highcharter::hc_plotOptions(treemap = list(layoutAlgorithm = "squarified", borderWidth = 1, borderColor = "#FFFFFF", dataLabels = list(enabled = TRUE, formatter = highcharter::JS("function(){return this.point.name;}")))) |>
      highcharter::hc_add_series(name = metric_label, type = "treemap", data = points) |>
      highcharter::hc_tooltip(formatter = highcharter::JS("function(){var p=this.point;return '<b>'+p.name+'</b><br/>证券代码：'+p.company_code+'<br/>总市值：'+Highcharts.numberFormat(p.market_cap,1)+' 亿元';}")) |>
      highcharter::hc_legend(enabled = FALSE)
}

# 用途：绘制各市场板块年度成交趋势横向条形图，数据来自市场板块成交统计。
#       纵轴按年份分组，板块作为系列；指标切换由卡片右上角下拉菜单控制。
# 输入来源：`calc_board_trading_data()` 读取的 raw Excel 数据。
plot_board_trading <- function(metric = c("turnover_amount_yi", "avg_daily_turnover_yi")) {
  metric <- match.arg(metric)
  df <- calc_board_trading_data()
  if (!is.data.frame(df) || nrow(df) == 0L) {
    return(chart_empty_state("暂无市场板块成交统计数据"))
  }
  if (!chart_has_highcharter()) {
    return(chart_fallback_table("市场板块成交统计", df, "未检测到 highcharter，已显示板块成交数据。"))
  }

  board_order <- c("上证主板", "深证主板", "创业板", "科创板", "北交所")
  board_palette <- c("#0B2A5B", "#4E95D9", "#BFD6EF", "#6F8095", "#00A6C8")
  names(board_palette) <- board_order

  df$year <- format(df$date, "%Y")
  years <- sort(unique(df$year))
  df$board <- factor(df$board, levels = board_order)

  y_axis_title <- if (identical(metric, "turnover_amount_yi")) "年成交额（亿元）" else "日均成交额（亿元）"

  # 构造各年份完整 5 个板块数据，缺失为 0
  complete_grid <- expand.grid(year = years, board = board_order, stringsAsFactors = FALSE)
  complete_grid$board <- factor(complete_grid$board, levels = board_order)
  merged <- merge(complete_grid, df, by = c("year", "board"), all.x = TRUE)
  merged[[metric]][is.na(merged[[metric]])] <- 0
  merged$board <- factor(merged$board, levels = board_order)
  merged <- merged[order(merged$year, merged$board), , drop = FALSE]

  hc <- chart_hc_base(NULL) |>
    highcharter::hc_chart(type = "bar", backgroundColor = "transparent", spacing = c(4, 10, 4, 0)) |>
    highcharter::hc_xAxis(
      categories = years,
      title = list(text = NULL),
      labels = list(style = list(fontSize = "11px", color = "#60758D", fontWeight = "650"))
    ) |>
    hc_y_axis(y_axis_title, min = 0) |>
    highcharter::hc_plotOptions(
      bar = list(
        borderWidth = 0,
        pointPadding = 0.04,
        groupPadding = 0.12,
        dataLabels = list(
          enabled = TRUE,
          crop = FALSE,
          overflow = "allow",
          format = "{point.y:,.0f}",
          style = list(color = "#0F172A", fontSize = "9px", fontWeight = "700", textOutline = "none")
        )
      )
    ) |>
    highcharter::hc_tooltip(
      shared = TRUE,
      headerFormat = "<b>{point.key} 年</b><br/>",
      pointFormat = paste0("<span style=\"color:{point.color}\">●</span> {series.name}：<b>{point.y:,.1f}</b> 亿元<br/>")
    ) |>
    highcharter::hc_legend(
      enabled = TRUE,
      layout = "horizontal",
      align = "center",
      verticalAlign = "bottom",
      itemStyle = list(fontSize = "10px", fontWeight = "600")
    )

  for (b in board_order) {
    sub <- merged[merged$board == b, , drop = FALSE]
    sub <- sub[match(years, sub$year), , drop = FALSE]
    hc <- hc |>
      highcharter::hc_add_series(
        name = b,
        type = "bar",
        color = board_palette[[b]],
        data = chart_safe_number(sub[[metric]])
      )
  }

  hc
}

# 用途：读取并合并北交所交易规模日度数据。
#       数据来源为 2020-2024 历史文件与 Wind 统计文件，字段保持只读，不改写 raw。
calc_bse_trading_growth_data <- function(period = c("daily", "monthly", "yearly")) {
  period <- match.arg(period)
  files <- c(
    "data/raw/北交所日度成交情况2020-2024.xlsx",
    "data/raw/市场交易统计(Wind统计).xlsx"
  )
  files <- files[file.exists(files)]
  if (length(files) == 0L || !requireNamespace("readxl", quietly = TRUE)) {
    return(data.frame())
  }

  read_one <- function(path) {
    dat <- tryCatch(readxl::read_excel(path, sheet = 1), error = function(e) NULL)
    if (is.null(dat) || nrow(dat) == 0L) return(data.frame())
    required <- c("日期", "成交额(亿元)", "成交额占AB股总成交额比重(%)", "区间换手率")
    if (!all(required %in% names(dat))) return(data.frame())

    date_raw <- dat[["日期"]]
    date_val <- if (inherits(date_raw, "Date")) {
      date_raw
    } else if (inherits(date_raw, "POSIXt")) {
      as.Date(date_raw)
    } else if (is.numeric(date_raw)) {
      as.Date(date_raw, origin = "1899-12-30")
    } else {
      as.Date(substr(as.character(date_raw), 1L, 10L))
    }

    out <- data.frame(
      date = date_val,
      turnover_amount_yi = chart_safe_number(dat[["成交额(亿元)"]]),
      turnover_share = chart_safe_number(dat[["成交额占AB股总成交额比重(%)"]]),
      turnover_rate = chart_safe_number(dat[["区间换手率"]]),
      source_file = basename(path),
      stringsAsFactors = FALSE
    )
    out[!is.na(out$date), , drop = FALSE]
  }

  df <- do.call(rbind, lapply(files, read_one))
  if (!is.data.frame(df) || nrow(df) == 0L) return(data.frame())
  df <- df[!is.na(df$turnover_amount_yi) | !is.na(df$turnover_share) | !is.na(df$turnover_rate), , drop = FALSE]
  df <- df[order(df$date, df$source_file), , drop = FALSE]
  df <- df[!duplicated(df$date, fromLast = TRUE), , drop = FALSE]

  if (identical(period, "daily")) {
    df$period_date <- df$date
    df$period_label <- format(df$date, "%Y-%m-%d")
    return(df[order(df$period_date), c("period_date", "period_label", "turnover_amount_yi", "turnover_rate", "turnover_share"), drop = FALSE])
  }

  if (identical(period, "monthly")) {
    df$period_key <- format(df$date, "%Y-%m")
    period_date <- as.Date(paste0(df$period_key, "-01"))
    label <- df$period_key
  } else {
    df$period_key <- format(df$date, "%Y")
    period_date <- as.Date(paste0(df$period_key, "-01-01"))
    label <- df$period_key
  }

  keys <- sort(unique(df$period_key))
  out <- do.call(rbind, lapply(keys, function(k) {
    sub <- df[df$period_key == k, , drop = FALSE]
    data.frame(
      period_date = period_date[match(k, df$period_key)],
      period_label = label[match(k, df$period_key)],
      turnover_amount_yi = sum(sub$turnover_amount_yi, na.rm = TRUE),
      turnover_rate = mean(sub$turnover_rate, na.rm = TRUE),
      turnover_share = mean(sub$turnover_share, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  out[order(out$period_date), , drop = FALSE]
}

# 用途：绘制北交所交易规模成长 area 线条图，支持日度/月度/年度和指标切换。
#       月度、年度下，成交额为区间加总，换手率和成交占比为区间平均。
plot_bse_trading_growth_area <- function(metric = "turnover_amount_yi", period = "monthly") {
  metric_choices <- c("turnover_amount_yi", "turnover_rate", "turnover_share")
  period_choices <- c("daily", "monthly", "yearly")
  metric <- if (metric %in% metric_choices) metric else "turnover_amount_yi"
  period <- if (period %in% period_choices) period else "monthly"

  df <- calc_bse_trading_growth_data(period)
  if (!is.data.frame(df) || nrow(df) == 0L || !metric %in% names(df)) {
    return(chart_empty_state("暂无北交所交易规模数据"))
  }
  if (!chart_has_highcharter()) {
    return(chart_fallback_table("北交所交易规模成长", df, "未检测到 highcharter"))
  }

  metric_labels <- c(
    turnover_amount_yi = "成交额（亿元）",
    turnover_rate = "换手率",
    turnover_share = "成交占全A股比重"
  )
  y_titles <- c(
    turnover_amount_yi = "成交额（亿元）",
    turnover_rate = "换手率（%）",
    turnover_share = "成交占全A股比重（%）"
  )
  suffixes <- c(turnover_amount_yi = " 亿元", turnover_rate = "%", turnover_share = "%")
  decimals <- c(turnover_amount_yi = 1, turnover_rate = 2, turnover_share = 2)
  period_labels <- c(daily = "日度", monthly = "月度", yearly = "年度")

  df <- df[!is.na(df[[metric]]), , drop = FALSE]
  df <- df[order(df$period_date), , drop = FALSE]
  if (nrow(df) == 0L) return(chart_empty_state("暂无北交所交易规模数据"))

  timestamps <- as.numeric(df$period_date) * 86400000
  data_pairs <- lapply(seq_len(nrow(df)), function(i) {
    list(x = timestamps[[i]], y = chart_safe_number(df[[metric]][[i]]), label = df$period_label[[i]])
  })
  y_max <- max(chart_safe_number(df[[metric]]), na.rm = TRUE)
  if (!is.finite(y_max)) y_max <- 0

  hs_stripe <- list(
    pattern = list(
      path = list(
        d = "M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11",
        stroke = "#002FA7",
        strokeWidth = 1.5,
        opacity = 0.6
      ),
      width = 10,
      height = 10,
      backgroundColor = "rgba(11, 42, 91, 0.04)"
    )
  )

  chart_hc_base(NULL) |>
    highcharter::hc_add_dependency("modules/pattern-fill.js") |>
    highcharter::hc_chart(type = "area", backgroundColor = "transparent") |>
    highcharter::hc_xAxis(
      type = "datetime",
      title = list(text = NULL),
      dateTimeLabelFormats = list(day = "%Y-%m-%d", week = "%Y-%m-%d", month = "%Y-%m", year = "%Y"),
      labels = list(style = list(fontSize = "10px"))
    ) |>
    hc_y_axis(y_titles[[metric]], min = 0, max = if (y_max > 0) y_max * 1.14 else NULL) |>
    highcharter::hc_plotOptions(
      area = list(
        lineWidth = 2,
        color = "#0B2A5B",
        fillColor = hs_stripe,
        connectNulls = TRUE,
        marker = list(enabled = FALSE, states = list(hover = list(enabled = TRUE, radius = 4)))
      )
    ) |>
    highcharter::hc_tooltip(
      useHTML = TRUE,
      xDateFormat = if (identical(period, "daily")) "%Y-%m-%d" else if (identical(period, "monthly")) "%Y-%m" else "%Y",
      pointFormat = paste0("<span style=\"color:#0B2A5B\">●</span> ", metric_labels[[metric]], "：<b>{point.y:,.", decimals[[metric]], "f}</b>", suffixes[[metric]])
    ) |>
    highcharter::hc_legend(enabled = FALSE) |>
    highcharter::hc_add_series(
      type = "area",
      name = paste0(period_labels[[period]], " ", metric_labels[[metric]]),
      color = "#0B2A5B",
      fillColor = hs_stripe,
      data = data_pairs
    )
}

# 用途：读取全球主要资本市场情况，并标准化为图表可直接使用的字段。
# 输入来源：data/raw/全球主要资本市场情况.xlsx。
calc_global_capital_market_data <- function() {
  path <- "data/raw/全球主要资本市场情况.xlsx"
  if (!file.exists(path) || !requireNamespace("readxl", quietly = TRUE)) {
    return(data.frame())
  }

  dat <- tryCatch(readxl::read_excel(path, sheet = 1), error = function(e) NULL)
  if (is.null(dat) || nrow(dat) == 0L || !"板块" %in% names(dat)) {
    return(data.frame())
  }

  find_col <- function(pattern) {
    hit <- grep(pattern, names(dat), value = TRUE)
    if (length(hit) == 0L) NA_character_ else hit[[1L]]
  }

  market_col <- "板块"
  cap_col <- find_col("总市值")
  turnover_col <- find_col("日均成交额")
  turnover_rate_col <- find_col("日均换手率|区间日均换手率")
  pe_col <- find_col("市盈率")
  required <- c(cap_col, turnover_col, turnover_rate_col, pe_col)
  if (any(is.na(required))) {
    return(data.frame())
  }

  out <- data.frame(
    market = trimws(as.character(dat[[market_col]])),
    total_market_cap_yi = chart_safe_number(dat[[cap_col]]),
    avg_daily_turnover_2026_yi = chart_safe_number(dat[[turnover_col]]),
    avg_turnover_rate_2026 = chart_safe_number(dat[[turnover_rate_col]]),
    pe_ttm_median = chart_safe_number(dat[[pe_col]]),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$market) & nzchar(out$market), , drop = FALSE]
  out <- out[!grepl("^数据来源", out$market), , drop = FALSE]
  out
}

# 用途：绘制全球主要资本市场横向 bar 图。
#       支持中国市场、成长板块筛选，并按所选统计指标降序排列。
plot_global_capital_market_bar <- function(metric = "total_market_cap_yi", china_only = FALSE, growth_only = FALSE) {
  metric_choices <- c("total_market_cap_yi", "avg_daily_turnover_2026_yi", "avg_turnover_rate_2026", "pe_ttm_median")
  metric <- if (metric %in% metric_choices) metric else "total_market_cap_yi"

  df <- calc_global_capital_market_data()
  if (!is.data.frame(df) || nrow(df) == 0L) {
    return(chart_empty_state("暂无全球主要资本市场数据"))
  }
  if (!chart_has_highcharter()) {
    return(chart_fallback_table("全球主要资本市场情况", df, "未检测到 highcharter"))
  }

  china_markets <- c("上证A股", "深证A股", "科创板", "创业板", "北证A股", "上市台股", "上柜台股", "港股主板", "港股创业板")
  growth_markets <- c(
    "科创板", "创业板", "北证A股", "上柜台股", "港股创业板",
    "NASDAQ 全球市场(GM)", "韩国创业板市场", "东证标准市场", "AMEX全部股票",
    "LSE创业板(AIM)", "东证增长市场", "新加坡创业板"
  )

  if (isTRUE(china_only)) {
    df <- df[df$market %in% china_markets, , drop = FALSE]
  }
  if (isTRUE(growth_only)) {
    df <- df[df$market %in% growth_markets, , drop = FALSE]
  }
  df <- df[!is.na(df[[metric]]), , drop = FALSE]
  if (nrow(df) == 0L) {
    return(chart_empty_state("当前筛选条件下暂无数据"))
  }

  highlight_palette <- c(
    "上证A股" = "#0B2A5B",
    "深证A股" = "#4E95D9",
    "科创板" = "#6F8095",
    "创业板" = "#BFD6EF",
    "北证A股" = "#00A6C8"
  )
  metric_labels <- c(
    total_market_cap_yi = "总市值",
    avg_daily_turnover_2026_yi = "2026年日均成交额",
    avg_turnover_rate_2026 = "2026年日均换手率",
    pe_ttm_median = "市盈率TTM"
  )
  axis_titles <- c(
    total_market_cap_yi = "亿元",
    avg_daily_turnover_2026_yi = "亿元",
    avg_turnover_rate_2026 = "%",
    pe_ttm_median = "倍"
  )
  suffixes <- c(
    total_market_cap_yi = " 亿元",
    avg_daily_turnover_2026_yi = " 亿元",
    avg_turnover_rate_2026 = "%",
    pe_ttm_median = " 倍"
  )
  decimals <- c(
    total_market_cap_yi = 0,
    avg_daily_turnover_2026_yi = 1,
    avg_turnover_rate_2026 = 2,
    pe_ttm_median = 1
  )

  df <- df[order(chart_safe_number(df[[metric]]), decreasing = TRUE), , drop = FALSE]
  categories <- df$market
  data <- lapply(seq_len(nrow(df)), function(i) {
    market <- df$market[[i]]
    list(
      name = market,
      y = chart_safe_number(df[[metric]][[i]]),
      color = if (market %in% names(highlight_palette)) highlight_palette[[market]] else "#F2F2F2"
    )
  })

  chart_hc_base(NULL) |>
    highcharter::hc_chart(type = "bar", backgroundColor = "transparent", spacing = c(6, 16, 6, 6)) |>
    highcharter::hc_xAxis(
      categories = categories,
      title = list(text = NULL),
      labels = list(style = list(color = "#2F3A45", fontSize = "11px", fontWeight = "600"))
    ) |>
    hc_y_axis(axis_titles[[metric]], min = 0) |>
    highcharter::hc_plotOptions(
      bar = list(
        borderWidth = 0,
        pointPadding = 0.08,
        groupPadding = 0.04,
        dataLabels = list(
          enabled = TRUE,
          inside = FALSE,
          crop = FALSE,
          overflow = "allow",
          format = paste0("{point.y:,.", decimals[[metric]], "f}"),
          style = list(color = "#0F172A", fontSize = "11px", fontWeight = "700", textOutline = "none")
        )
      )
    ) |>
    highcharter::hc_tooltip(
      headerFormat = "",
      pointFormat = paste0("<b>{point.name}</b><br/>", metric_labels[[metric]], "：<b>{point.y:,.", decimals[[metric]], "f}</b>", suffixes[[metric]])
    ) |>
    highcharter::hc_legend(enabled = FALSE) |>
    highcharter::hc_add_series(
      type = "bar",
      name = metric_labels[[metric]],
      data = data
    )
}

# 用途：读取全球资本市场 IPO 融资规模基础表。
# 输入来源：data/raw/global_capital_market_ipo_financing_2026h1.csv。
calc_global_ipo_financing_data <- function(path = "data/raw/global_capital_market_ipo_financing_2026h1.csv") {
  if (!file.exists(path)) {
    return(data.frame())
  }

  dat <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8", check.names = FALSE),
    error = function(e) NULL
  )
  if (is.null(dat) || nrow(dat) == 0L) {
    return(data.frame())
  }

  required <- c("rank", "exchange", "financing_amount_usd_100m", "financing_yoy_pct", "ipo_count")
  if (!all(required %in% names(dat))) {
    return(data.frame())
  }

  out <- data.frame(
    rank = as.integer(chart_safe_number(dat$rank)),
    exchange = trimws(as.character(dat$exchange)),
    financing_amount_usd_100m = chart_safe_number(dat$financing_amount_usd_100m),
    financing_yoy_pct = chart_safe_number(dat$financing_yoy_pct),
    ipo_count = chart_safe_number(dat$ipo_count),
    period = if ("period" %in% names(dat)) as.character(dat$period) else "2026H1",
    note = if ("note" %in% names(dat)) as.character(dat$note) else "2026年上半年IPO募资",
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$rank) & !is.na(out$exchange) & nzchar(out$exchange), , drop = FALSE]
  out[order(out$rank), , drop = FALSE]
}

# 用途：绘制全球资本市场 IPO 融资规模横向 bar 图。
#       指标由标签页右上角下拉框切换。
# 输入来源：calc_global_ipo_financing_data() 返回的数据。
plot_global_ipo_financing_bar <- function(metric = "financing_amount_usd_100m") {
  metric_choices <- c("financing_amount_usd_100m", "financing_yoy_pct", "ipo_count")
  metric <- if (metric %in% metric_choices) metric else "financing_amount_usd_100m"

  df <- calc_global_ipo_financing_data()
  if (!is.data.frame(df) || nrow(df) == 0L) {
    return(chart_empty_state("暂无全球资本市场融资规模数据"))
  }
  if (!chart_has_highcharter()) {
    return(chart_fallback_table("全球资本市场融资规模", df, "未检测到 highcharter"))
  }

  highlight_palette <- c(
    "上海证券交易所" = "#0B2A5B",
    "深圳证券交易所" = "#4E95D9",
    "北京证券交易所" = "#00A6C8"
  )
  keyboard_gray <- "#F2F2F2"
  metric_labels <- c(
    financing_amount_usd_100m = "募资额",
    financing_yoy_pct = "募资额同比",
    ipo_count = "IPO 数量"
  )
  axis_titles <- c(
    financing_amount_usd_100m = "亿美元",
    financing_yoy_pct = "%",
    ipo_count = "家"
  )
  suffixes <- c(
    financing_amount_usd_100m = " 亿美元",
    financing_yoy_pct = "%",
    ipo_count = " 家"
  )
  decimals <- c(
    financing_amount_usd_100m = 0,
    financing_yoy_pct = 0,
    ipo_count = 0
  )

  df <- df[!is.na(df[[metric]]), , drop = FALSE]
  if (nrow(df) == 0L) {
    return(chart_empty_state("当前指标暂无数据"))
  }
  df <- df[order(chart_safe_number(df[[metric]]), decreasing = TRUE), , drop = FALSE]
  categories <- df$exchange
  data <- lapply(seq_len(nrow(df)), function(i) {
    exchange <- df$exchange[[i]]
    list(
      name = exchange,
      y = chart_safe_number(df[[metric]][[i]]),
      color = if (exchange %in% names(highlight_palette)) highlight_palette[[exchange]] else keyboard_gray,
      rank = df$rank[[i]],
      amount = chart_safe_number(df$financing_amount_usd_100m[[i]]),
      yoy = chart_safe_number(df$financing_yoy_pct[[i]]),
      count = chart_safe_number(df$ipo_count[[i]])
    )
  })

  axis_min <- if (identical(metric, "financing_yoy_pct")) min(-20, min(df[[metric]], na.rm = TRUE) * 1.1) else 0
  axis_max <- max(df[[metric]], na.rm = TRUE) * 1.16
  tooltip_extra <- character()
  if (!identical(metric, "financing_amount_usd_100m")) {
    tooltip_extra <- c(tooltip_extra, "募资额：<b>{point.amount:,.0f}</b> 亿美元")
  }
  if (!identical(metric, "financing_yoy_pct")) {
    tooltip_extra <- c(tooltip_extra, "同比：<b>{point.yoy:,.0f}</b>%")
  }
  if (!identical(metric, "ipo_count")) {
    tooltip_extra <- c(tooltip_extra, "IPO 数量：<b>{point.count:,.0f}</b> 家")
  }

  chart_hc_base(NULL) |>
    highcharter::hc_chart(
      type = "bar",
      backgroundColor = "transparent",
      spacing = c(6, 18, 18, 6)
    ) |>
    highcharter::hc_xAxis(
      categories = categories,
      title = list(text = NULL),
      labels = list(
        style = list(color = "#2F3A45", fontSize = "11px", fontWeight = "650"),
        formatter = highcharter::JS(
          "function(){return '<span style=\"color:#C59B45;font-weight:800;margin-right:6px;\">'+(this.pos+1)+'</span> '+this.value;}"
        ),
        useHTML = TRUE
      )
    ) |>
    hc_y_axis(
      axis_titles[[metric]],
      min = axis_min,
      max = axis_max,
      plot_lines = if (identical(metric, "financing_yoy_pct")) list(list(value = 0, width = 1, color = "#AEB8C2")) else NULL
    ) |>
    highcharter::hc_plotOptions(
      bar = list(
        borderWidth = 0,
        pointPadding = 0.08,
        groupPadding = 0.04,
        dataLabels = list(
          enabled = TRUE,
          crop = FALSE,
          overflow = "allow",
          inside = FALSE,
          format = paste0("{point.y:,.", decimals[[metric]], "f}", if (identical(metric, "financing_yoy_pct")) "%" else ""),
          style = list(color = "#0F172A", fontSize = "11px", fontWeight = "700", textOutline = "none")
        )
      )
    ) |>
    highcharter::hc_tooltip(
      useHTML = TRUE,
      headerFormat = "",
      pointFormat = paste0(
        "<b>{point.name}</b><br/>",
        metric_labels[[metric]], "：<b>{point.y:,.", decimals[[metric]], "f}</b>",
        suffixes[[metric]], "<br/>",
        paste(tooltip_extra, collapse = "<br/>")
      )
    ) |>
    highcharter::hc_legend(enabled = FALSE) |>
    highcharter::hc_caption(
      text = "2026年上半年IPO募资",
      align = "right",
      style = list(color = "#64748B", fontSize = "11px", fontWeight = "600")
    ) |>
    highcharter::hc_add_series(
      type = "bar",
      name = metric_labels[[metric]],
      data = data
    )
}

# 用途：绘制各板块日均成交额堆叠面积图。
#       数据来源为 市场板块成交统计，使用真实日期作为时间轴，
#       堆叠顺序：上证主板 → 深证主板 → 创业板 → 科创板 → 北交所。
# 输入来源：`calc_board_trading_weekly_data()` 返回的日期数据
plot_board_daily_turnover_area <- function() {
  df <- calc_board_trading_weekly_data()
  if (!is.data.frame(df) || nrow(df) == 0L) {
    return(chart_empty_state("暂无板块成交统计数据"))
  }
  if (!chart_has_highcharter()) {
    return(chart_fallback_table("板块成交统计", df, "未检测到 highcharter"))
  }

  board_order <- c("上证主板", "深证主板", "创业板", "科创板", "北交所")
  board_palette <- c("#0B2A5B", "#4E95D9", "#BFD6EF", "#6F8095", "#00A6C8")
  names(board_palette) <- board_order

  df <- df[!is.na(df$avg_daily_turnover_yi) & df$avg_daily_turnover_yi >= 0, , drop = FALSE]
  df$board <- factor(df$board, levels = board_order)
  dates <- sort(unique(df$date))
  date_timestamps <- as.numeric(dates) * 86400000

  hc <- chart_hc_base(NULL) |>
    highcharter::hc_xAxis(
      type = "datetime",
      title = list(text = NULL),
      dateTimeLabelFormats = list(day = "%m-%d", week = "%m-%d", month = "%Y-%m"),
      labels = list(style = list(fontSize = "10px"))
    ) |>
    hc_y_axis("日均成交额（亿元）", min = 0) |>
    highcharter::hc_chart(type = "area", backgroundColor = "transparent") |>
    highcharter::hc_plotOptions(
      area = list(
        stacking = "normal",
        fillOpacity = 1,
        lineWidth = 1.2,
        connectNulls = TRUE,
        marker = list(enabled = FALSE, states = list(hover = list(enabled = TRUE, radius = 4)))
      )
    ) |>
    highcharter::hc_tooltip(
      shared = TRUE,
      valueSuffix = " 亿元",
      valueDecimals = 1,
      xDateFormat = "%Y-%m-%d"
    ) |>
    highcharter::hc_legend(
      enabled = TRUE,
      layout = "horizontal",
      align = "center",
      verticalAlign = "bottom",
      itemStyle = list(fontSize = "10px")
    )

  for (b in board_order) {
    sub <- df[df$board == b, , drop = FALSE]
    sub <- sub[order(sub$date), , drop = FALSE]
    data_pairs <- lapply(seq_along(dates), function(i) {
      row <- sub[sub$date == dates[i], , drop = FALSE]
      val <- if (nrow(row) == 0L) 0 else chart_safe_number(row$avg_daily_turnover_yi[[1L]])
      list(x = date_timestamps[[i]], y = val)
    })
    hc <- hc |>
      highcharter::hc_add_series(
        type = "area",
        name = b,
        color = board_palette[[b]],
        data = data_pairs
      )
  }
  hc
}
# 用途：绘制各市场行业分布情况半圆点阵图（Highcharts item chart）。
#       为控制渲染点数，每个点代表若干家公司/亿元市值，tooltip 显示真实数量。
# 输入来源：`calc_market_industry_distribution()` 返回的行业数值数据框。
plot_market_industry_distribution <- function(df, metric = "company_count", max_dots = 260L) {
  required <- c("industry", "value", "unit_label", "share")
  if (!is.data.frame(df) || nrow(df) == 0L || !all(required %in% names(df))) {
    return(chart_empty_state("暂无行业分布数据"))
  }
  if (!chart_has_highcharter()) {
    return(chart_fallback_table("各市场行业分布情况", df, "未检测到 highcharter，已显示行业分布数据。"))
  }

  total <- sum(df$value, na.rm = TRUE)
  scale <- max(1, ceiling(total / max_dots))

  unit_label <- as.character(df$unit_label[[1]])
  use_market_cap <- identical(metric, "market_cap")
  value_decimals <- if (use_market_cap) 2L else 0L
  value_name <- if (use_market_cap) "总市值" else "公司家数"
  display_unit <- if (use_market_cap) "万亿" else unit_label
  n <- nrow(df)
  palette <- c(
    "#005BAC", "#0B2A5B", "#00A6C8", "#4E95D9", "#8DBCEB",
    "#BFD6EF", "#6F8095", "#22A06B", "#F59E0B"
  )[seq_len(n)]

  data <- lapply(seq_len(n), function(i) {
    value <- as.numeric(df$value[[i]])
    share_val <- chart_safe_number(df$share[[i]])
    display_value <- if (use_market_cap) round(value / 10000, 2) else value
    list(
      name = as.character(df$industry[[i]]),
      y = max(1L, as.integer(value / scale)),
      actual = display_value,
      unit = display_unit,
      share = share_val,
      share_pct = paste0(format(round(share_val * 100, 1), nsmall = 1), "%"),
      color = palette[[i]]
    )
  })

  chart_hc_base(NULL) |>
    highcharter::hc_add_dependency("modules/item-series.js") |>
    highcharter::hc_chart(
      type = "item",
      margin = c(0, 24, 0, 24),
      spacing = c(0, 0, 0, 0)
    ) |>
    highcharter::hc_plotOptions(
      item = list(
        startAngle = -100,
        endAngle = 100,
        center = c("50%", "68%"),
        size = "118%",
        rows = 10,
        layout = "horizontal",
        itemPadding = 0.012,
        marker = list(symbol = "circle"),
        dataLabels = list(
          enabled = TRUE,
          crop = FALSE,
          overflow = "allow",
          format = paste0("{point.name} {point.actual:,.", value_decimals, "f} ", display_unit),
          style = list(color = "#0B2A5B", fontSize = "11px", fontWeight = "650", textOutline = "none")
        )
      )
    ) |>
    highcharter::hc_legend(enabled = FALSE) |>
    highcharter::hc_tooltip(
      pointFormat = paste0("<b>{point.name}</b><br/>", value_name, "：{point.actual:,.", value_decimals, "f} ", display_unit, "<br/>占比：{point.share_pct}")
    ) |>
    highcharter::hc_add_series(
      name = "行业分布",
      type = "item",
      data = data
    )
}

# 用途：绘制北交所历年公司上市数量堆叠柱状图。
#       存量部分（下半部分）使用深蓝斜条纹填充；
#       新增部分（上半部分）为品牌蓝实心填充。
# 输入来源：`df` 参数为 calc_bse_annual_listing() 返回的 data.frame(year, cumulative, new)
plot_bse_annual_listing_bar <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0L) {
    return(chart_empty_state("暂无北交所历年上市数据"))
  }
  if (!chart_has_highcharter()) {
    return(chart_fallback_table("北交所历年上市数量", df, "未检测到 highcharter"))
  }

  years <- as.character(df$year)
  total_listed <- chart_safe_number(df$cumulative) + chart_safe_number(df$new)
  y_max <- max(total_listed, na.rm = TRUE)
  if (!is.finite(y_max)) y_max <- 0

  hs_stripe <- list(
    pattern = list(
      path = list(
        d = "M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11",
        stroke = "#002FA7",
        strokeWidth = 1.5,
        opacity = 0.6
      ),
      width = 10,
      height = 10,
      backgroundColor = "transparent"
    )
  )

  chart_hc_base("column") |>
    highcharter::hc_add_dependency("modules/pattern-fill.js") |>
    hc_x_axis("", categories = years) |>
    hc_y_axis("公司家数", min = 0, max = ceiling(y_max * 1.14)) |>
    highcharter::hc_plotOptions(
      column = list(
        stacking = "normal",
        pointPadding = 0.15,
        groupPadding = 0.05
      )
    ) |>
    highcharter::hc_colors(c("#005BAC")) |>
    highcharter::hc_tooltip(
      shared = TRUE,
      headerFormat = "<b>{point.key} 年</b><br/>",
      pointFormat = "<span style=\"color:{point.color}\">●</span> {series.name}：<b>{point.y}</b> 家<br/>"
    ) |>
    highcharter::hc_legend(
      enabled = TRUE,
      layout = "horizontal",
      align = "center",
      verticalAlign = "bottom"
    ) |>
    highcharter::hc_add_series(
      type = "column",
      name = "新增上市",
      data = df$new,
      borderColor = "#005BAC",
      borderWidth = 1.2
    ) |>
    highcharter::hc_add_series(
      type = "column",
      name = "存量企业",
      data = df$cumulative,
      color = hs_stripe,
      borderColor = "#0B2A5B",
      borderWidth = 1.2
    ) |>
    highcharter::hc_add_series(
      type = "scatter",
      name = "总上市公司数量",
      data = lapply(seq_along(total_listed), function(i) {
        list(x = i - 1L, y = total_listed[[i]])
      }),
      showInLegend = FALSE,
      enableMouseTracking = FALSE,
      marker = list(enabled = FALSE),
      dataLabels = list(
        enabled = TRUE,
        format = "{point.y:.0f}",
        y = -8,
        crop = FALSE,
        overflow = "allow",
        style = list(
          color = "#002B5B",
          fontSize = "11px",
          fontWeight = "700",
          textOutline = "none"
        )
      )
    )
}

# 用途：绘制各板块上市公司市值区间分布堆叠柱状图。
#       100% 堆叠，横轴为板块，纵轴为百分比，不同颜色代表市值区间。
#       调色盘：采用蓝色层次表达不同市值区间，
#               300-1000亿浅蓝，1000亿以上青色
# 输入来源：`df` 参数为 calc_market_cap_distribution() 返回的 data.frame
plot_market_cap_distribution_bar <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0L) {
    return(chart_empty_state("暂无市值分布数据"))
  }
  if (!chart_has_highcharter()) {
    return(chart_fallback_table("市值分布", df, "未检测到 highcharter"))
  }

  boards <- c("上证主板", "深证主板", "创业板", "科创板", "北交所")
  buckets <- c("<50亿", "50-100亿", "100-300亿", "300-1000亿", "1000亿以上")
  palette <- c("#0B2A5B", "#005BAC", "#4E95D9", "#8DBCEB", "#00A6C8")

  hc <- chart_hc_base("column") |>
    hc_x_axis("", categories = boards) |>
    hc_y_axis("占比（%）", min = 0, max = 100) |>
    highcharter::hc_plotOptions(
      column = list(
        stacking = "percent",
        pointPadding = 0.15,
        groupPadding = 0.05,
        dataLabels = list(
          enabled = TRUE,
          format = "{point.percentage:.0f}%",
          style = list(
            color = "#FFFFFF",
            fontSize = "9px",
            fontWeight = "500",
            textOutline = "none",
            textShadow = "0 1px 2px rgba(0,0,0,0.45)"
          ),
          allowOverlap = TRUE,
          crop = FALSE,
          overflow = "allow"
        )
      )
    ) |>
    highcharter::hc_tooltip(
      headerFormat = "<b>{point.category}</b><br/>",
      pointFormat = "<span style=\"color:{point.color}\">●</span> {series.name}：<b>{point.percentage:.1f}%</b>（{point.count} 家）<br/>"
    ) |>
    highcharter::hc_legend(
      enabled = TRUE,
      layout = "horizontal",
      align = "center",
      verticalAlign = "bottom",
      itemStyle = list(fontSize = "10px")
    )

  for (i in rev(seq_along(buckets))) {
    b <- buckets[[i]]
    sub <- df[df$bucket == b, c("board", "count"), drop = FALSE]
    series_data <- lapply(boards, function(bd) {
      row <- sub[sub$board == bd, , drop = FALSE]
      if (nrow(row) == 0L) return(list(y = 0, count = 0))
      list(y = row$count[[1L]], count = row$count[[1L]])
    })
    hc <- hc |>
      highcharter::hc_add_series(
        type = "column",
        name = b,
        color = palette[[i]],
        data = series_data
      )
  }
  hc
}
