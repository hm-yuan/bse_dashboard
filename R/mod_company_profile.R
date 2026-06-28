# 用途：从公司画像页面模型中按图表类型提取内容块。
# 输入来源：`dashboard_page_models$company_profile$charts`。
company_profile_chart_block <- function(type) {
  model <- dashboard_page_models$company_profile
  blocks <- Filter(function(block) identical(block$type, type), model$charts)
  if (length(blocks) == 0L) return(NULL)
  blocks[[1L]]
}

# 用途：创建公司地理分布合并卡片（响应式版本），左侧地图 + 右侧散点图/表格。
# 输入来源：`ns` 命名空间函数，`block` 页面内容块配置。
company_geo_map_combined_card_ui <- function(ns, block) {
  div(
    class = "content-card chart-card company-geo-combined-card",
    card_heading(block$title, block$note),
    div(
      class = "chart-content company-geo-combined-content",
      div(
        class = "company-geo-left",
        div(
          class = "company-geo-left-top",
          highcharter::highchartOutput(ns("geo_map"), height = "100%"),
          div(
            class = "geo-reset-btn-wrap",
            shiny::actionButton(
              ns("geo_reset"),
              label = "复位",
              class = "geo-reset-btn"
            )
          )
        ),
        div(
          class = "company-geo-left-bottom",
          div(
            class = "geo-bottom-scatter",
            shiny::uiOutput(ns("scatter_plot"))
          ),
          div(
            class = "geo-bottom-scatter",
            highcharter::highchartOutput(ns("operating_status_donut"), height = "100%")
          )
        )
      ),
      div(
        class = "company-geo-right",
        div(
          class = "company-geo-right-cell geo-geo-table-cell",
          div(class = "geo-table-scroll geo-reactable-wrap", reactable::reactableOutput(ns("finance_table")))
        )
      )
    )
  )
}

# 用途：公司画像 UI 模块函数，按"地图+矩阵 / 四象限+洞察+行业树图"组织三行布局。
# 输入来源：`id` 参数（Shiny 模块命名空间）和页面模型中的内容块配置。
companyProfileUI <- function(id) {
  ns <- shiny::NS(id)
  model <- dashboard_page_models$company_profile
  map_block <- company_profile_chart_block("company_geo_map")

  geo_map_combined <- if (!is.null(map_block)) company_geo_map_combined_card_ui(ns, map_block)

  dashboard_sheet(
    class = "portrait-page company-profile-page",
    kpi_grid(model$kpis, kpi_board_label("company_profile", model)),
    dashboard_grid(
      class = "chart-grid company-profile-main-grid",
      geo_map_combined
    ),
    dashboard_grid(
      class = "bottom-grid-two-col",
      market_industry_distribution_card(ns),
      market_industry_treemap_card(ns)
    )
  )
}

# 用途：公司画像服务端模块函数。
# 输入来源：地图点击事件、树图点击事件。
companyProfileServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- 行业树图相关 ----
    selected_industry <- shiny::reactiveVal(NULL)
    size_by <- shiny::reactiveVal("count")

    shiny::observeEvent(input$industry_treemap_click, {
      selected_industry(as.character(input$industry_treemap_click))
    }, ignoreNULL = TRUE)

    shiny::observeEvent(input$treemap_back, {
      selected_industry(NULL)
    })

    shiny::observeEvent(input$treemap_unit, {
      next_unit <- if (identical(size_by(), "count")) "market_cap" else "count"
      size_by(next_unit)
      shiny::updateActionButton(
        session,
        "treemap_unit",
        label = if (identical(next_unit, "count")) "切换为公司市值" else "切换为公司家数"
      )
    })

    output$treemap_state <- shiny::renderText({
      selected <- selected_industry()
      level <- if (is.null(selected)) "行业分布" else paste0("公司明细：", if (identical(selected, "__other__")) "其他行业" else selected)
      unit <- if (identical(size_by(), "count")) "按公司家数" else "按公司市值"
      paste(level, "·", unit)
    })

    output$industry_treemap <- highcharter::renderHighchart({
      plot_market_industry_treemap_drill(
        data = dashboard_data,
        selected_industry = selected_industry(),
        size_by = size_by(),
        click_input_id = session$ns("industry_treemap_click")
      )
    })

    output$industry_distribution_chart <- highcharter::renderHighchart({
      plot_market_industry_distribution(
        calc_market_industry_distribution(input$market_select, input$industry_metric),
        metric = input$industry_metric
      )
    })

    # ---- 地理联动 ----
    geo_filter <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$geo_click, {
      click <- input$geo_click
      if (is.null(click)) {
        geo_filter(NULL)
        return()
      }
      city <- if (!is.null(click$city) && nzchar(click$city)) click$city else NULL
      province <- if (!is.null(click$province) && nzchar(click$province)) click$province else NULL
      geo_filter(list(city = city, province = province))
    })

    shiny::observeEvent(input$geo_reset, {
      geo_filter(NULL)
    })

    # 地图可能返回英文省份名称（GeoJSON 属性）或简写中文名称（joinBy 后的数据）。
    # 先统一转换为简写中文，后续再通过 normalize_province_name 与 Excel 中的完整名称匹配。
    province_name_map <- c(
      "Anhui" = "安徽", "Beijing" = "北京", "Chongqing" = "重庆", "Fujian" = "福建",
      "Gansu" = "甘肃", "Guangdong" = "广东", "Guangxi" = "广西", "Guizhou" = "贵州",
      "Hainan" = "海南", "Hebei" = "河北", "Heilongjiang" = "黑龙江", "Henan" = "河南",
      "Hubei" = "湖北", "Hunan" = "湖南", "Inner Mongolia" = "内蒙古", "Jiangsu" = "江苏",
      "Jiangxi" = "江西", "Jilin" = "吉林", "Liaoning" = "辽宁", "Ningxia" = "宁夏",
      "Qinghai" = "青海", "Shaanxi" = "陕西", "Shandong" = "山东", "Shanghai" = "上海",
      "Shanxi" = "山西", "Sichuan" = "四川", "Taiwan" = "台湾", "Tianjin" = "天津",
      "Xinjiang" = "新疆", "Xizang" = "西藏", "Yunnan" = "云南", "Zhejiang" = "浙江"
    )

    filtered_finance_data <- shiny::reactive({
      filt <- geo_filter()
      path <- "data/raw/上市公司基本情况.xlsx"
      if (!file.exists(path) || !requireNamespace("readxl", quietly = TRUE)) {
        df <- calc_company_financial_table(dashboard_data)
        if (is.null(filt) || nrow(df) == 0L) return(df)
        return(df)
      }
      raw <- tryCatch(readxl::read_excel(path, sheet = "公司", .name_repair = "unique"), error = function(e) NULL)
      if (is.null(raw)) {
        df <- calc_company_financial_table(dashboard_data)
        if (is.null(filt) || nrow(df) == 0L) return(df)
        return(df)
      }

      raw <- as.data.frame(raw, stringsAsFactors = FALSE)
      board_col <- intersect(c("上市板块", "板块", "board", "market"), names(raw))[[1]]
      if (!is.na(board_col)) {
        raw <- raw[trimws(as.character(raw[[board_col]])) %in% c("北证", "北交所"), , drop = FALSE]
      }

      # 按城市或省份过滤
      city_filter <- filt$city
      province_filter <- filt$province
      if (!is.null(province_filter) && province_filter %in% names(province_name_map)) {
        province_filter <- province_name_map[province_filter]
      }
      province_filter <- normalize_province_name(province_filter)
      if (!is.null(city_filter) && "城市" %in% names(raw)) {
        city_key <- function(x) gsub("市$", "", trimws(as.character(x)))
        matched <- city_key(raw[["城市"]]) == city_key(city_filter)
        raw <- raw[matched | (trimws(as.character(raw[["城市"]])) == city_filter), , drop = FALSE]
      } else if (!is.null(province_filter) && "省份" %in% names(raw)) {
        raw <- raw[normalize_province_name(raw[["省份"]]) == province_filter, , drop = FALSE]
      }

      if (is.null(filt) && nrow(raw) == 0L) {
        df <- calc_company_financial_table(dashboard_data)
        return(df)
      }
      if (is.null(filt)) {
        raw
      } else {
        raw
      }
    })

    output$finance_table <- reactable::renderReactable({
      data <- filtered_finance_data()
      if (is.null(data) || nrow(data) == 0L) return(NULL)

      # 构建主体表格数据
      display_df <- data.frame(
        证券简称 = data[["名称"]],
        `营业收入（亿元）` = round(metric_num(data[["2025年营业收入"]]), 1),
        `净利润（亿元）`   = round(metric_num(data[["2025年净利润"]]), 1),
        `公发募资（亿元）` = round(metric_num(data[["首发募资"]]), 1),
        `总市值（亿元）`   = round(metric_num(data[["总市值"]]), 1),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      display_df <- display_df[order(display_df[["营业收入（亿元）"]], decreasing = TRUE, na.last = TRUE), , drop = FALSE]
      data <- data[order(metric_num(data[["2025年营业收入"]]), decreasing = TRUE, na.last = TRUE), , drop = FALSE]
      rownames(display_df) <- NULL
      rownames(data) <- NULL

      # 展开详情函数
      # 预加载股价走势数据
      stock_price_data <- tryCatch({
        sp <- as.data.frame(readxl::read_excel("data/raw/北证A股近期走势.xlsx"), stringsAsFactors = FALSE)
        rownames(sp) <- trimws(as.character(sp[["代码"]]))
        sp
      }, error = function(e) NULL)

      make_detail <- function(index) {
        row <- data[index, , drop = FALSE]
        company_name <- as.character(row[["名称"]] %||% "-")
        company_code <- trimws(as.character(row[["代码"]] %||% "-"))
        main_product <- as.character(row[["主要产品"]] %||% "-")
        major_shareholder <- as.character(row[["大股东名称"]] %||% "-")
        shareholder_ratio <- metric_num(row[["大股东持股比例"]])
        shareholder_info <- if (!is.na(shareholder_ratio)) {
          paste0(major_shareholder, "（", round(shareholder_ratio * 100, 1), "%）")
        } else {
          major_shareholder
        }

        # 经营业绩明细表
        perf_rows <- list(
          c("2025年营业收入", format(round(metric_num(row[["2025年营业收入"]]), 2), nsmall = 2)),
          c("25营收增长率", if (!is.na(metric_num(row[["25营收增长率"]]))) paste0(round(metric_num(row[["25营收增长率"]]) * 100, 1), "%") else "-"),
          c("2025年净利润", format(round(metric_num(row[["2025年净利润"]]), 2), nsmall = 2)),
          c("24利润增长率", if (!is.na(metric_num(row[["24利润增长率"]]))) paste0(round(metric_num(row[["24利润增长率"]]) * 100, 1), "%") else "-"),
          c("2025年ROE", if (!is.na(metric_num(row[["2025年ROE"]]))) paste0(round(metric_num(row[["2025年ROE"]]), 1), "%") else "-"),
          c("总资产", format(round(metric_num(row[["总资产"]]), 2), nsmall = 2)),
          c("净资产", format(round(metric_num(row[["净资产"]]), 2), nsmall = 2)),
          c("研发强度", if (!is.na(metric_num(row[["研发强度"]]))) paste0(round(metric_num(row[["研发强度"]]) * 100, 1), "%") else "-")
        )

        perf_html <- lapply(perf_rows, function(x) {
          tags$tr(
            tags$td(class = "reactable-detail-metric", x[[1]]),
            tags$td(class = "reactable-detail-value", x[[2]])
          )
        })

        # 近30日股价走势
        stock_chart_div <- NULL
        if (!is.null(stock_price_data)) {
          stock_row <- stock_price_data[company_code, , drop = FALSE]
          if (nrow(stock_row) > 0L) {
            date_cols <- names(stock_price_data)[-(1:2)]
            prices <- as.numeric(stock_row[1, date_cols, drop = TRUE])
            valid <- is.finite(prices)
            if (sum(valid) >= 3) {
              prices <- prices[valid]
              dates <- date_cols[valid]
              prices <- rev(prices)
              dates <- rev(dates)
              dates <- as.character(dates)
              prices <- as.numeric(prices)
              dates_json <- paste0("[", paste(shQuote(dates, type = "cmd"), collapse = ","), "]")
              prices_json <- paste0("[", paste(format(prices, digits = 10, trim = TRUE, scientific = FALSE), collapse = ","), "]")
              chart_id <- paste0("stock-mini-chart-", index)
              stock_chart_div <- tags$div(
                class = "detail-section",
                tags$h2(class = "detail-h2", "近30日股价走势"),
                tags$div(
                  id = chart_id,
                  class = "stock-mini-chart",
                  style = "height: 150px;",
                  `data-dates` = dates_json,
                  `data-prices` = prices_json
                )
              )
            }
          }
        }

        tags$div(
          class = "package-detail",
          tags$div(
            class = "detail-section",
            tags$h2(class = "detail-h2", "公司基本情况"),
            tags$div(
              class = "detail-row",
              tags$span(class = "detail-label", "证券简称："), tags$span(company_name),
              tags$span(class = "detail-label", style = "margin-left: 24px;", "证券代码："), tags$span(company_code)
            ),
            tags$div(
              class = "detail-row",
              tags$span(class = "detail-label", "公司主要产品："), tags$span(main_product)
            ),
            tags$div(
              class = "detail-row",
              tags$span(class = "detail-label", "公司主要股东："), tags$span(shareholder_info)
            )
          ),
          tags$div(
            class = "detail-section",
            tags$h2(class = "detail-h2", "公司经营业绩"),
            tags$table(
              class = "detail-perf-table",
              tags$thead(
                tags$tr(
                  tags$th("指标"),
                  tags$th("数值")
                )
              ),
              tags$tbody(perf_html)
            )
          ),
          stock_chart_div
        )
      }

      reactable::reactable(
        display_df,
        searchable = TRUE,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 20, 25, 50, 100),
        onClick = "expand",
        defaultColDef = reactable::colDef(align = "center", headerClass = "geo-reactable-header"),
        columns = list(
          `营业收入（亿元）` = reactable::colDef(format = reactable::colFormat(digits = 1)),
          `净利润（亿元）`   = reactable::colDef(format = reactable::colFormat(digits = 1)),
          `公发募资（亿元）` = reactable::colDef(format = reactable::colFormat(digits = 1)),
          `总市值（亿元）`   = reactable::colDef(format = reactable::colFormat(digits = 1))
        ),
        details = make_detail,
        wrap = FALSE,
        rowStyle = list(cursor = "pointer"),
        language = reactable::reactableLang(
          searchPlaceholder = "筛选公司",
          noData = "未找到匹配公司"
        ),
        theme = reactable::reactableTheme(
          cellPadding = "5px 8px",
          style = list(fontSize = "11px"),
          searchInputStyle = list(
            padding = "6px 10px",
            width = "100%",
            border = "1px solid var(--border)",
            borderRadius = "6px",
            marginBottom = "10px",
            fontSize = "12px"
          )
        )
      )
    })

    filtered_scatter_data <- shiny::reactive({
      filt <- geo_filter()
      df <- calc_company_revenue_profit_scatter(dashboard_data)
      if (is.null(filt) || nrow(df) == 0L) return(df)
      path <- "data/raw/上市公司基本情况.xlsx"
      if (!file.exists(path) || !requireNamespace("readxl", quietly = TRUE)) return(df)
      raw <- tryCatch(readxl::read_excel(path, sheet = "公司", .name_repair = "unique"), error = function(e) NULL)
      if (is.null(raw)) return(df)

      raw <- as.data.frame(raw, stringsAsFactors = FALSE)
      board_col <- intersect(c("上市板块", "板块", "board", "market"), names(raw))[[1]]
      if (!is.na(board_col)) {
        raw <- raw[trimws(as.character(raw[[board_col]])) %in% c("北证", "北交所"), , drop = FALSE]
      }

      city_filter <- filt$city
      province_filter <- filt$province
      if (!is.null(province_filter) && province_filter %in% names(province_name_map)) {
        province_filter <- province_name_map[province_filter]
      }
      province_filter <- normalize_province_name(province_filter)
      if (!is.null(city_filter) && "城市" %in% names(raw)) {
        city_key <- function(x) gsub("市$", "", trimws(as.character(x)))
        matched <- city_key(raw[["城市"]]) == city_key(city_filter)
        codes <- raw[["代码"]][matched | (trimws(as.character(raw[["城市"]])) == city_filter)]
      } else if (!is.null(province_filter) && "省份" %in% names(raw)) {
        codes <- raw[["代码"]][normalize_province_name(raw[["省份"]]) == province_filter]
      } else {
        return(df)
      }

      df[df$company_name %in% raw[raw[["代码"]] %in% codes, "名称"], , drop = FALSE]
    })

    output$geo_map <- highcharter::renderHighchart({
      plot_company_geography_map(
        calc_company_geography(dashboard_data),
        bse_geo = calc_company_geography(dashboard_data, board_filter = c("北证", "北交所")),
        geo_click_input_id = session$ns("geo_click")
      )
    })

    output$scatter_plot <- shiny::renderUI({
      plot_company_revenue_profit_scatter(filtered_scatter_data())
    })

    output$operating_status_donut <- highcharter::renderHighchart({
      data <- filtered_finance_data()
      if (is.null(data) || nrow(data) == 0L) {
        return(NULL)
      }
      plot_company_operating_status_donut(calc_company_operating_status_distribution(data))
    })
  })
}
