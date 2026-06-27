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
plot_company_geography_map <- function(geo, bse_geo = NULL, geo_click_input_id = NULL) {
  if (!chart_has_highcharter()) {
    return(chart_empty_state("未检测到 highcharter，无法渲染中国公司分布地图。"))
  }

  if (
    is.null(geo) ||
      !is.data.frame(geo$provinces) ||
      nrow(geo$provinces) == 0L
  ) {
    return(chart_empty_state("暂无可用的省级公司分布数据。"))
  }

  map_cache <- file.path("data", "cache", "china-cn-all.geo.json")

  map_data <- if (file.exists(map_cache) && requireNamespace("jsonlite", quietly = TRUE)) {
    tryCatch(
      jsonlite::fromJSON(map_cache, simplifyVector = FALSE),
      error = function(e) NULL
    )
  } else {
    tryCatch(
      highcharter::download_map_data("countries/cn/cn-all"),
      error = function(e) NULL
    )
  }

  if (is.null(map_data)) {
    return(chart_empty_state("中国地图底图加载失败，请检查本地地图缓存与 Highcharts Map 数据可用性。"))
  }

  province_data <- geo$provinces[, c("hc_key", "province", "company_count"), drop = FALSE]
  names(province_data) <- c("hc_key", "name", "value")

  # 计算北交所公司总数（用于 tooltip 占比）
  total_bse_count <- 0
  if (is.data.frame(bse_geo$provinces) && nrow(bse_geo$provinces) > 0L) {
    total_bse_count <- sum(bse_geo$provinces$company_count, na.rm = TRUE)
  }
  if (total_bse_count <= 0 && is.data.frame(geo$provinces) && nrow(geo$provinces) > 0L) {
    total_bse_count <- sum(geo$provinces$company_count, na.rm = TRUE)
  }
  if (total_bse_count <= 0) total_bse_count <- 1

  # 共享的 tooltip formatter
  tooltip_formatter <- sprintf("
    function () {
      if (
        this.point &&
        this.point.series &&
        this.point.series.type === 'mappoint'
      ) {
        var pct = (this.point.company_count || 0) / %s * 100;
        var tip = '<div style=\"min-width:180px;\">' +
          '<div style=\"font-weight:700;color:#0F172A;margin-bottom:6px;\">' +
            this.point.name +
          '</div>' +
          '<div style=\"color:#475569;line-height:1.7;\">' +
            '所在省份：' + (this.point.province || '-') + '<br/>' +
            '北交所公司：<b style=\"color:#0F172A;\">' +
              Highcharts.numberFormat(this.point.company_count || 0, 0) +
            '</b> 家 / <b style=\"color:#0F172A;\">' +
              Highcharts.numberFormat(pct, 1) +
            '</b>%%';

        if (this.point.top_companies) {
          tip += '<div style=\"margin-top:6px;padding-top:6px;border-top:1px solid #E2E8F0;\">' +
            '<div style=\"font-weight:600;color:#0F172A;margin-bottom:3px;\">代表公司</div>';
          if (this.point.top_companies.indexOf('、') > -1) {
            var names = this.point.top_companies.split('、');
            var caps = this.point.top_caps ? this.point.top_caps.split('、') : [];
            for (var j = 0; j < names.length; j++) {
              tip += '<div>' + (j+1) + '. ' + names[j] +
                (caps[j] ? '（' + caps[j] + ' 亿）' : '') + '</div>';
            }
          } else {
            tip += '<div>' + this.point.top_companies +
              (this.point.top_caps ? '（' + this.point.top_caps + ' 亿）' : '') + '</div>';
          }
          tip += '</div>';
        }

        tip += '</div></div>';
        return tip;
      }

      var pctProv = (this.point.value || 0) / %s * 100;
      return '<div style=\"min-width:140px;\">' +
               '<div style=\"font-weight:700;color:#0F172A;margin-bottom:4px;\">' +
                 this.point.name +
               '</div>' +
               '<div style=\"color:#475569;line-height:1.6;\">' +
                 '上市公司：<b style=\"color:#0F172A;\">' +
                   Highcharts.numberFormat(this.point.value || 0, 0) +
                 '</b> 家 / <b style=\"color:#0F172A;\">' +
                   Highcharts.numberFormat(pctProv, 1) +
                 '</b>%%' +
               '</div>' +
             '</div>';
    }
  ", total_bse_count, total_bse_count)

  # 北交所城市气泡分档函数
  # 阈值 c(5, 10, 20)，形成四档：
  # ≤5、6-10、11-20、>20
  bse_city_level <- function(x) {
    if (is.na(x)) {
      return("无数据")
    } else if (x <= 5) {
      return("≤5 家")
    } else if (x <= 10) {
      return("6-10 家")
    } else if (x <= 20) {
      return("11-20 家")
    } else {
      return(">20 家")
    }
  }

  # 北交所城市气泡颜色函数
  # 北交所城市气泡颜色函数
# 从高到低使用：
# >20     "#005BAC"
# 11-20   "#4E95D9"
# 6-10    "#8DBCEB"
# ≤5      "#00A6C8"
bse_city_color <- function(x) {
  if (is.na(x)) {
    return("#005BAC")
  } else if (x > 15) {
    return("#005BAC")
  } else if (x > 10) {
    return("#4E95D9")
  } else if (x > 5) {
    return("#8DBCEB")
  } else {
    return("#00A6C8")
  }
}

  hc <- chart_hc_base("map") |>
    highcharter::hc_add_dependency("modules/map.js") |>
    highcharter::hc_chart(
      map = map_data,
      backgroundColor = "transparent"
    ) |>
    highcharter::hc_add_series(
      type = "map",
      name = "省级公司数量",
      data = province_data,
      joinBy = c("hc-key", "hc_key"),
      borderColor = "#0B2A5B",
      borderWidth = 0.8,
      nullColor = "#F4F5F7",
      showInLegend = FALSE,
      states = list(
        hover = list(
          color = "#6B7280"
        )
      )
    ) |>
    highcharter::hc_colorAxis(
      min = 0,
      minColor = "#F4F5F7",
      maxColor = "#005BAC",
      labels = list(
        format = "{value} 家",
        style = list(
          color = "#4B5563",
          fontSize = "11px"
        )
      )
    )

  # 仅叠加北交所公司城市分布
  if (!is.null(bse_geo) && is.data.frame(bse_geo$cities) && nrow(bse_geo$cities) > 0L) {
    bse_points <- bse_geo$cities
    bse_points$longitude <- chart_safe_number(bse_points$longitude)
    bse_points$latitude <- chart_safe_number(bse_points$latitude)
    bse_points$company_count <- chart_safe_number(bse_points$company_count)

    bse_points <- bse_points[
      is.finite(bse_points$longitude) &
        is.finite(bse_points$latitude) &
        is.finite(bse_points$company_count) &
        bse_points$company_count > 0,
      ,
      drop = FALSE
    ]

    if (nrow(bse_points) > 0L) {
      max_bse_count <- max(bse_points$company_count, na.rm = TRUE)

      # 构建同省公司数量查找表
      province_count_lookup <- stats::setNames(
        bse_geo$provinces$company_count,
        bse_geo$provinces$province
      )

      # 构建城市代表公司查找表（city_key = 去掉"市"后缀）
      city_key <- function(x) gsub("市$", "", trimws(as.character(x)))
      bse_top_lookup <- NULL
      if (!is.null(bse_geo$city_top_companies) && is.data.frame(bse_geo$city_top_companies) && nrow(bse_geo$city_top_companies) > 0L) {
        bse_top_lookup <- bse_geo$city_top_companies
        rownames(bse_top_lookup) <- bse_top_lookup$city_key
      }

      bse_data <- lapply(seq_len(nrow(bse_points)), function(i) {
        count <- bse_points$company_count[[i]]
        radius <- 1 + 16 * sqrt(count / max_bse_count)
        point_color <- bse_city_color(count)

        city_province <- bse_points$province[[i]]
        prov_count <- province_count_lookup[city_province]
        if (is.na(prov_count)) prov_count <- 0

        ck <- city_key(bse_points$city[[i]])
        top_info <- if (!is.null(bse_top_lookup) && ck %in% rownames(bse_top_lookup)) {
          bse_top_lookup[ck, , drop = FALSE]
        } else NULL

        list(
          name = bse_points$city[[i]],
          province = city_province,
          company_count = count,
          province_count = prov_count,
          top_companies = if (!is.null(top_info)) top_info$top_companies[[1]] else "",
          top_caps = if (!is.null(top_info)) top_info$top_caps[[1]] else "",
          color = point_color,

          # 使用 GeoJSON geometry 定位点位
          geometry = list(
            type = "Point",
            coordinates = c(
              bse_points$longitude[[i]],
              bse_points$latitude[[i]]
            )
          ),

          marker = list(
            enabled = TRUE,
            radius = radius,
            symbol = "circle",
            fillColor = point_color,
            lineColor = "#FFFFFF",
            lineWidth = 1.5
          )
        )
      })

      hc <- hc |>
        highcharter::hc_add_series(
          type = "mappoint",
          name = "北交所公司数量",
          data = bse_data,
          color = "#00A6C8",
          showInLegend = TRUE,
          zIndex = 21,
          marker = list(
            enabled = TRUE,
            symbol = "circle",
            lineColor = "#FFFFFF",
            lineWidth = 1.5,
            fillOpacity = 0.92
          ),
          states = list(
            hover = list(
              enabled = TRUE,
              brightness = 0.08,
              lineWidthPlus = 1
            )
          )
        )
    }
  }

  # 手动设置 tooltip，避免 list.merge 合并时破坏 JS_EVAL 对象
  hc$x$hc_opts$tooltip <- list(
    useHTML = TRUE,
    backgroundColor = "rgba(255, 255, 255, 0.96)",
    borderColor = "#CBD5E1",
    borderRadius = 4,
    shadow = TRUE,
    formatter = highcharter::JS(tooltip_formatter)
  )

  # 直接设置 mapNavigation，确保放大缩小按钮生效
  hc$x$hc_opts$mapNavigation <- list(
    enabled = TRUE,
    enableButtons = TRUE,
    enableMouseWheelZoom = TRUE,
    buttonOptions = list(
      align = "left",
      verticalAlign = "top",
      x = 10,
      y = 10
    )
  )

  hc |>
    highcharter::hc_legend(
      enabled = TRUE,
      layout = "vertical",
      align = "left",
      verticalAlign = "top",
      floating = FALSE,
      x = 30,
      itemDistance = 6,
      symbolRadius = 3,
      itemStyle = list(
        color = "#334155",
        fontSize = "11px",
        fontWeight = "600"
      )
    ) |>
    highcharter::hc_credits(
      enabled = FALSE
    )

  # 注册点击事件以联动散点图和表格
  if (!is.null(geo_click_input_id)) {
    click_js <- sprintf(
      "function() {
        var data = {};
        if (this.series.type === 'mappoint') {
          data.city = this.name || '';
          data.province = this.province || '';
        } else {
          data.province = this.name || '';
          data.city = '';
        }
        Shiny.setInputValue('%s', data, {priority: 'event'});
      }",
      geo_click_input_id
    )
    hc <- hc |>
      highcharter::hc_plotOptions(
        series = list(
          point = list(
            events = list(
              click = htmlwidgets::JS(click_js)
            )
          )
        )
      )
  }

  return(hc)
}


# 用途：绘制北交所公司营业收入 vs 净利润散点图。
# 输入来源：`calc_company_revenue_profit_scatter()` 的输出数据框。
plot_company_revenue_profit_scatter <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0L) {
    return(chart_empty_state("暂无营业收入与净利润数据"))
  }
  if (!chart_has_highcharter()) {
    return(chart_fallback_table("营业收入与净利润", df, "未检测到 highcharter。"))
  }

  colors <- chart_colors()

  df$revenue_yi <- chart_safe_number(df$revenue_yi)
  df$net_profit_yi <- chart_safe_number(df$net_profit_yi)

  df <- df[
    is.finite(df$revenue_yi) &
      is.finite(df$net_profit_yi) &
      df$revenue_yi > 0,
    ,
    drop = FALSE
  ]

  if (nrow(df) == 0L) {
    return(chart_empty_state("暂无有效的营收与利润数据"))
  }

  # X轴希望展示的真实刻度
  x_ticks <- c(1, 2, 4, 8, 15, 30, 50, 75, 100, 200)

  # Y轴希望展示的真实刻度
  y_ticks <- c(
    -20, -10, -5, -2, -1.2, -0.8, -0.5, -0.3, -0.15,
    0,
    0.15, 0.3, 0.5, 0.8, 1.2, 2, 5, 10, 20
  )

  # 将真实值映射为等距坐标位置
  chart_equal_interval_position <- function(x, breaks) {
    stats::approx(
      x = breaks,
      y = seq_along(breaks),
      xout = x,
      rule = 2
    )$y
  }

  df$x_plot <- chart_equal_interval_position(df$revenue_yi, x_ticks)
  df$y_plot <- chart_equal_interval_position(df$net_profit_yi, y_ticks)

  # 计算中位数及在绘图坐标中的位置
  revenue_median <- stats::median(df$revenue_yi, na.rm = TRUE)
  profit_median <- stats::median(df$net_profit_yi, na.rm = TRUE)
  x_median_pos <- chart_equal_interval_position(revenue_median, x_ticks)
  y_median_pos <- chart_equal_interval_position(profit_median, y_ticks)
  y_zero_pos   <- chart_equal_interval_position(0, y_ticks)

  points <- lapply(seq_len(nrow(df)), function(i) {
    list(
      x = df$x_plot[[i]],
      y = df$y_plot[[i]],
      name = df$company_name[[i]],

      # 保留真实值，用于 tooltip
      revenue_yi = df$revenue_yi[[i]],
      net_profit_yi = df$net_profit_yi[[i]]
    )
  })

  chart_hc_base("scatter") |>
    highcharter::hc_chart(backgroundColor = "#FFFFFF") |>
    hc_x_axis(
      "营业收入（亿元）",
      type = "linear",
      tick_positions = seq_along(x_ticks),
      min = 1,
      max = length(x_ticks),
      labels = list(
        formatter = htmlwidgets::JS(sprintf(
          "
          function () {
            var labels = %s;
            var idx = Math.round(this.value) - 1;
            if (idx >= 0 && idx < labels.length) {
              return labels[idx];
            }
            return '';
          }
          ",
          jsonlite::toJSON(x_ticks, auto_unbox = TRUE)
        ))
      ),
      plot_lines = list(list(
  value = x_median_pos,
  color = "#0B2A5B",
  dashStyle = "ShortDash",
  width = 1,
  zIndex = 5,
  label = list(
    text = sprintf("营收中位数 %.2f 亿元", revenue_median),
    align = "center",
    verticalAlign = "bottom",

    # 关键：负值向上移动
    y = -52,

    style = list(
      color = "#0B2A5B",
      fontSize = "10px",
      fontWeight = "600"
    )
  )
))
    ) |>
    hc_y_axis(
      "净利润（亿元）",
      type = "linear",
      tick_positions = seq_along(y_ticks),
      min = 1,
      max = length(y_ticks),
      labels = list(
        formatter = htmlwidgets::JS(sprintf(
          "
          function () {
            var labels = %s;
            var idx = Math.round(this.value) - 1;
            if (idx >= 0 && idx < labels.length) {
              return labels[idx];
            }
            return '';
          }
          ",
          jsonlite::toJSON(y_ticks, auto_unbox = TRUE)
        ))
      ),
      plot_lines = list(
        list(
          value = y_zero_pos,
          color = "#0B2A5B",
          dashStyle = "Solid",
          width = 2,
          zIndex = 5
        ),
        list(
          value = y_median_pos,
          color = "#0B2A5B",
          dashStyle = "ShortDash",
          width = 1,
          zIndex = 5,
          label = list(
            text = sprintf("净利润中位数 %.2f 亿元", profit_median),
            align = "right",
            verticalAlign = "middle",
            x = 1,
            style = list(color = "#0B2A5B", fontSize = "10px", fontWeight = "600")
          )
        )
      )
    ) |>
    highcharter::hc_add_series(
      name = "北交所公司",
      data = points,
      color = "#005BAC",
      marker = list(
        radius = 5,
        fillOpacity = 0.7,
        lineWidth = 1,
        lineColor = "#FFFFFF"
      )
    ) |>
    highcharter::hc_tooltip(
      pointFormat = paste0(
        "<b>{point.name}</b><br/>",
        "营业收入：{point.revenue_yi:.2f} 亿元<br/>",
        "净利润：{point.net_profit_yi:.2f} 亿元"
      )
    ) |>
    highcharter::hc_legend(enabled = FALSE) |>
    chart_widget()
}

# 用途：绘制公司经营状态南丁格尔玫瑰图。
# 输入来源：`calc_company_operating_status_distribution()` 的输出数据框。
plot_company_operating_status_donut <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0L || sum(df$count, na.rm = TRUE) == 0L) {
    return(chart_empty_state("暂无经营状态数据"))
  }
  if (!chart_has_highcharter()) {
    return(chart_fallback_table("公司经营状态分布", df, "未检测到 highcharter"))
  }

  colors <- c("#005BAC", "#0B2A5B", "#00A6C8", "#8DBCEB", "#6F8095")
  df <- df[df$count > 0L, , drop = FALSE]
  if (nrow(df) == 0L) {
    return(chart_empty_state("暂无经营状态数据"))
  }

  total <- sum(df$count, na.rm = TRUE)
  rose_data <- lapply(seq_len(nrow(df)), function(i) {
    list(
      name = df$status[[i]],
      y = df$count[[i]],
      color = colors[[(i - 1L) %% length(colors) + 1L]],
      pct = round(df$count[[i]] / total * 100, 1)
    )
  })

  chart_hc_base() |>
    highcharter::hc_chart(type = "column", polar = TRUE, backgroundColor = "transparent") |>
    highcharter::hc_title(text = NULL) |>
    highcharter::hc_xAxis(
      categories = df$status,
      tickmarkPlacement = "on",
      lineWidth = 0,
      lineColor = "transparent",
      gridLineWidth = 0,
      tickWidth = 0,
      title = list(text = NULL),
      labels = list(enabled = FALSE)
    ) |>
    highcharter::hc_yAxis(
      gridLineInterpolation = "polygon",
      gridLineColor = "transparent",
      lineWidth = 0,
      lineColor = "transparent",
      tickWidth = 0,
      min = 0,
      title = list(text = NULL),
      labels = list(enabled = FALSE)
    ) |>
    highcharter::hc_plotOptions(
      column = list(
        grouping = FALSE,
        pointPadding = 0,
        groupPadding = 0,
        borderWidth = 1,
        borderColor = "#FFFFFF",
        dataLabels = list(
          enabled = TRUE,
          format = "{point.name}<br>{point.y}家",
          style = list(fontSize = "10px", fontWeight = "500", textOutline = "none")
        )
      )
    ) |>
    highcharter::hc_legend(enabled = FALSE) |>
    highcharter::hc_tooltip(
      pointFormat = "<b>{point.name}</b><br/>公司数：{point.y} 家 ({point.pct}%)"
    ) |>
    highcharter::hc_add_series(
      name = "公司数",
      data = rose_data,
      type = "column"
    )
}
