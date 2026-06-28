# 用途：创建市场定位页行业矩形树图卡片，提供单位切换与返回行业操作。
# 输入来源：`ns` Shiny 命名空间和页面配置中的 industry_treemap 内容块。
market_industry_treemap_card <- function(ns) {
  model <- dashboard_page_models$market_position
  chart_index <- which(vapply(model$charts, function(block) identical(block$type, "industry_treemap"), logical(1)))
  block <- model$charts[[chart_index[[1L]]]]

  div(
    class = "content-card chart-card chart-span-side industry-treemap-card",
    card_heading("本所上市公司行业市值结构", "点击行业色块下钻查看公司明细，支持按公司家数/市值切换"),
    div(
      class = "treemap-toolbar",
      tags$span(class = "treemap-state", textOutput(ns("treemap_state"), inline = TRUE)),
      div(
        class = "treemap-actions",
        shiny::actionButton(ns("treemap_back"), "返回行业", class = "treemap-action treemap-action-secondary"),
        shiny::actionButton(ns("treemap_unit"), "切换为公司市值", class = "treemap-action")
      )
    ),
    div(class = "chart-content", highcharter::highchartOutput(ns("industry_treemap"), height = "100%"))
  )
}

# 用途：创建市场板块成交统计分组柱状图卡片；指标切换由右上角下拉菜单控制。
# 输入来源：市场板块成交统计原始数据，由 plot_board_trading() 渲染。
market_board_trading_card <- function(ns) {
  div(
    class = "content-card chart-card chart-span-wide board-trading-card",
    card_heading("市场成交规模", "切换指标查看不同成交额口径"),
    div(
      class = "treemap-toolbar",
      tags$span(class = "treemap-state", ""),
      div(
        class = "treemap-actions",
        shiny::selectInput(
          ns("board_metric"),
          label = NULL,
          choices = c("年成交额" = "turnover_amount_yi", "日均成交额" = "avg_daily_turnover_yi"),
          selected = "turnover_amount_yi",
          width = "140px"
        )
      )
    ),
    div(class = "chart-content", highcharter::highchartOutput(ns("board_trading_chart"), height = "100%"))
  )
}

# 用途：创建各市场规模对比 + 年度成交对比组合卡片。
#       左侧 50% 为各市场规模对比气泡图，右侧 50% 为年度板块成交柱状图。
#       布局参考 market_overview_card 的双面板结构。
board_turnover_combined_card <- function(ns) {
  div(
    class = "content-card chart-card chart-span-wide market-overview-card",
    card_heading("市场规模与成交趋势", "左侧：各市场规模对比；右侧：年度板块成交对比"),
    div(
      class = "chart-content market-overview-content",
      div(
        class = "market-overview-panel market-overview-left",
        div(class = "market-overview-panel-heading", "各市场规模对比"),
        chart_widget(plot_market_position_bubble(calc_market_position_bubble(dashboard_data)))
      ),
      div(
        class = "market-overview-panel market-overview-right",
        div(
          class = "market-overview-panel-header",
          div(class = "market-overview-panel-heading", "各板块年度成交对比"),
          shiny::selectInput(
            ns("board_metric"),
            label = NULL,
            choices = c("年成交额" = "turnover_amount_yi", "日均成交额" = "avg_daily_turnover_yi"),
            selected = "turnover_amount_yi",
            width = "132px"
          )
        ),
        highcharter::highchartOutput(ns("board_trading_chart"), height = "100%")
      )
    )
  )
}

# 用途：创建全球主要资本市场横向对比卡片，替代原“市场规模与成交趋势”和“预留分析区”。
global_capital_market_card <- function(ns) {
  div(
    class = "content-card chart-card chart-span-left-double global-capital-market-card",
    card_heading("全球主要资本市场对比", "横向比较全球主要资本市场的规模、流动性和估值水平"),
    div(
      class = "global-market-toolbar",
      div(
        class = "global-market-checks",
        shiny::checkboxInput(ns("global_market_china_only"), "中国市场", value = FALSE),
        shiny::checkboxInput(ns("global_market_growth_only"), "成长板块", value = FALSE)
      ),
      shiny::selectInput(
        ns("global_market_metric"),
        label = NULL,
        choices = c(
          "总市值" = "total_market_cap_yi",
          "2026年日均成交额" = "avg_daily_turnover_2026_yi",
          "2026年日均换手率" = "avg_turnover_rate_2026",
          "市盈率TTM" = "pe_ttm_median"
        ),
        selected = "total_market_cap_yi",
        width = "180px",
        selectize = FALSE
      )
    ),
    div(class = "chart-content global-capital-market-content", highcharter::highchartOutput(ns("global_capital_market_chart"), height = "100%"))
  )
}

# 用途：创建市场定位页右侧占位演示卡片，用于填充图表网格空白位置。
# 输入来源：无，仅作布局占位。
market_placeholder_card <- function() {
  div(
    class = "content-card chart-card chart-span-side",
    card_heading("演示占位", "后续内容待补充"),
    div(class = "chart-content", chart_empty_state("演示内容将在此位置展示。"))
  )
}

# 用途：创建北交所上市公司数量 + 北交所交易规模成长组合卡片。
#       左侧 50% 为北交所历年上市数量柱状图，右侧 50% 为北交所交易规模成长图。
#       布局参考 market_overview_card 的双面板结构。
# 输入来源：calc_bse_annual_listing() 和北交所交易规模日度数据。
bse_listing_market_bubble_card <- function(ns) {
  div(
    class = "content-card chart-card chart-span-wide market-overview-card",
    card_heading("北交所上市公司数量与交易成长", "左侧：北交所历年上市数量；右侧：北交所交易规模成长"),
    div(
      class = "chart-content market-overview-content",
      div(
        class = "market-overview-panel market-overview-left",
        div(class = "market-overview-panel-heading", "始终保持稳健积极的上市公司规模增长"),
        highcharter::highchartOutput(ns("bse_annual_listing_chart"), height = "100%")
      ),
      div(
        class = "market-overview-panel market-overview-right",
        div(
          class = "market-overview-panel-header",
          div(class = "market-overview-panel-heading", "以流动性水平的改善增强市场价值发现功能"),
          div(
            class = "market-overview-panel-controls",
            shiny::selectInput(
              ns("bse_growth_period"),
              label = NULL,
              choices = c("日度" = "daily", "月度" = "monthly", "年度" = "yearly"),
              selected = "monthly",
              width = "96px",
              selectize = FALSE
            ),
            shiny::selectInput(
              ns("bse_growth_metric"),
              label = NULL,
              choices = c(
                "成交额（亿元）" = "turnover_amount_yi",
                "换手率" = "turnover_rate",
                "成交占全A股比重" = "turnover_share"
              ),
              selected = "turnover_amount_yi",
              width = "156px",
              selectize = FALSE
            )
          )
        ),
        highcharter::highchartOutput(ns("bse_trading_growth_chart"), height = "100%")
      )
    )
  )
}

# 用途：创建各市场行业分布情况卡片，提供市场和统计口径筛选下拉框。
# 输入来源：上市公司基本情况原始 Excel，由 plot_market_industry_distribution() 渲染。
market_industry_distribution_card <- function(ns) {
  div(
    class = "content-card chart-card chart-span-side industry-distribution-card",
    card_heading("各市场行业分布情况", "按大类行业统计，前 7 大行业其余合并为其他行业"),
    div(
      class = "treemap-toolbar",
      tags$span(class = "treemap-state", ""),
      div(
        class = "treemap-actions",
        shiny::selectInput(
          ns("market_select"),
          label = NULL,
          choices = c("全部A股", "上证主板", "深证主板", "创业板", "科创板", "北交所"),
          selected = "全部A股",
          width = "120px"
        ),
        shiny::selectInput(
          ns("industry_metric"),
          label = NULL,
          choices = c("公司家数" = "company_count", "公司市值" = "market_cap"),
          selected = "company_count",
          width = "120px"
        )
      )
    ),
    div(class = "chart-content", highcharter::highchartOutput(ns("industry_distribution_chart"), height = "100%"))
  )
}

# 用途：创建市场定位气泡图卡片（用于底部栏）。
# 输入来源：`dashboard_data` 中的 market_position_kpi。
market_bubble_card <- function() {
  div(
    class = "content-card chart-card chart-span-wide",
    card_heading("多市场定位对比", "横轴：总市值；纵轴：日均成交额；气泡大小：上市公司数量"),
    div(class = "chart-content", chart_widget(plot_market_position_bubble(calc_market_position_bubble(dashboard_data))))
  )
}

# 用途：创建市场规模与板块成交组合卡片，左侧为 bubble 图，右侧为 board_trading 柱状图。
# 输入来源：`dashboard_data` 和板块成交统计原始数据，由服务端根据下拉指标渲染右侧图。
market_overview_card <- function(ns) {
  div(
    class = "content-card chart-card chart-span-wide market-overview-card",
    card_heading("市场规模与板块成交", "左侧：各市场规模对比；右侧：板块成交统计"),
    div(
      class = "chart-content market-overview-content",
      div(
        class = "market-overview-panel market-overview-left",
        div(class = "market-overview-panel-heading", "各市场规模对比"),
        chart_widget(plot_market_position_bubble(calc_market_position_bubble(dashboard_data)))
      ),
      div(
        class = "market-overview-panel market-overview-right",
        div(
          class = "market-overview-panel-header",
          div(class = "market-overview-panel-heading", "各板块年度成交对比"),
          shiny::selectInput(
            ns("board_metric"),
            label = NULL,
            choices = c("年成交额" = "turnover_amount_yi", "日均成交额" = "avg_daily_turnover_yi"),
            selected = "turnover_amount_yi",
            width = "132px"
          )
        ),
        highcharter::highchartOutput(ns("board_trading_chart"), height = "100%")
      )
    )
  )
}

# 用途：创建市场规模气泡图独立卡片（仅左侧面板）。
# 输入来源：`dashboard_data` 中的 market_position_kpi。
market_bubble_only_card <- function() {
  div(
    class = "content-card chart-card chart-span-side",
    card_heading("各市场规模对比", "横轴：总市值；纵轴：日均成交额；气泡大小：上市公司数量"),
    div(class = "chart-content", chart_widget(plot_market_position_bubble(calc_market_position_bubble(dashboard_data))))
  )
}

# 用途：创建各市场规模对比 + 上市公司市值分布组合卡片。
#       左侧 50% 为市场规模气泡图，右侧 50% 为市值区间堆叠柱状图。
#       布局参考 market_overview_card 的双面板结构。
market_bubble_and_cap_distribution_card <- function(ns) {
  div(
    class = "content-card chart-card chart-span-wide market-overview-card",
    card_heading("市场规模与市值分布", "左侧：各市场规模对比；右侧：上市公司市值分布"),
    div(
      class = "chart-content market-overview-content",
      div(
        class = "market-overview-panel market-overview-left",
        div(class = "market-overview-panel-heading", "各市场规模对比"),
        chart_widget(plot_market_position_bubble(calc_market_position_bubble(dashboard_data)))
      ),
      div(
        class = "market-overview-panel market-overview-right",
        div(class = "market-overview-panel-heading", "上市公司市值分布"),
        highcharter::highchartOutput(ns("market_cap_distribution_chart"), height = "100%")
      )
    )
  )
}

# 用途：创建企业市值-市盈率散点图卡片。
# 输入来源：服务端 `calc_company_pe_market_cap_data()` 数据。
market_pe_scatter_card <- function(ns) {
  div(
    class = "content-card chart-card chart-span-wide",
    card_heading("市值与市盈率分布", "横轴：企业总市值（亿元，对数轴）；纵轴：市盈率（倍，对数轴）"),
    div(class = "chart-content", highcharter::highchartOutput(ns("pe_scatter_chart"), height = "100%"))
  )
}

# 用途：创建市场定位页右侧跨行组合卡片。
#       上部 65% 为市值与市盈率分布；下部 35% 为企业所有权性质分布与上市公司市值分布。
# 输入来源：服务端 pe_scatter_chart、enterprise_nature_chart、market_cap_distribution_chart。
market_position_right_stack_card <- function(ns) {
  div(
    class = "content-card chart-card chart-span-right-stack market-position-right-stack-card",
    card_heading("估值、股权性质与市值分布", "上部：市值与市盈率分布；下部：企业性质与上市公司市值结构"),
    div(
      class = "chart-content market-position-right-stack-content",
      div(
        class = "right-stack-top",
        div(class = "market-overview-panel-heading", "市值与市盈率分布"),
        highcharter::highchartOutput(ns("pe_scatter_chart"), height = "100%")
      ),
      div(
        class = "right-stack-bottom",
        div(
          class = "right-stack-bottom-panel",
          div(class = "market-overview-panel-heading", "企业所有权性质分布"),
          highcharter::highchartOutput(ns("enterprise_nature_chart"), height = "100%")
        ),
        div(
          class = "right-stack-bottom-panel",
          div(class = "market-overview-panel-heading", "上市公司市值分布"),
          highcharter::highchartOutput(ns("market_cap_distribution_chart"), height = "100%")
        )
      )
    )
  )
}

# 用途：创建市场定位页左侧第三行占位组合卡片，承载后续新增图表。
market_position_placeholder_pair_card <- function() {
  div(
    class = "content-card chart-card chart-span-wide market-position-placeholder-pair-card",
    card_heading("预留分析区", "用于后续扩展市场定位相关图表"),
    div(
      class = "chart-content market-position-placeholder-pair-content",
      div(
        class = "market-position-placeholder-panel",
        div(class = "market-overview-panel-heading", "占位图 01"),
        chart_empty_state("预留图表")
      ),
      div(
        class = "market-position-placeholder-panel",
        div(class = "market-overview-panel-heading", "占位图 02"),
        chart_empty_state("预留图表")
      )
    )
  )
}

# 用途：创建企业所有权性质百分比柱状图卡片。
enterprise_nature_card <- function(ns) {
  div(
    class = "content-card chart-card chart-span-side",
    card_heading("企业所有权性质分布", "按板块统计，地方国有企业、中央国有企业合并为国有企业；公众企业、集体企业、其他企业合并为公众企业"),
    div(class = "chart-content", highcharter::highchartOutput(ns("enterprise_nature_chart"), height = "100%"))
  )
}

# 用途：创建行业分布 + 企业性质卡片的横向组合，供 bottom_right 使用。
market_industry_placeholder_row <- function(ns) {
  div(
    class = "bottom-right-row",
    div(class = "bottom-right-half", market_industry_distribution_card(ns)),
    div(class = "bottom-right-half", enterprise_nature_card(ns))
  )
}

# 用途：创建市场定位页关键洞察 + 北交所特色指标小图组合卡片。
#       左侧为原有洞察文本，右侧上下排列研发强度占比与国家级专精特新数量。
# 输入来源：`ns` Shiny 命名空间、`model` 页面模型。
market_insight_with_charts_card <- function(ns, model) {
  insights <- if (!is.null(model) && !is.null(model$insights)) model$insights else character()

  div(
    class = "content-card insight-card market-insight-with-charts-card",
    card_heading("关键洞察", "北交所企业专精特新占比高、研发投入比重大"),
    div(
      class = "market-insight-with-charts-content",
      div(
        class = "market-insight-panel market-insight-left",
        div(class = "insight-list", lapply(seq_along(insights), function(i) {
          div(class = "insight-item", span(class = "insight-index", sprintf("%02d", i)), span(insights[[i]]))
        }))
      ),
      div(
        class = "market-insight-panel market-insight-right",
        div(
          class = "market-insight-mini-chart",
          div(class = "market-insight-mini-heading", "北交所研发强度 > 5% 企业占比"),
          highcharter::highchartOutput(ns("bse_rd_intensity_chart"), height = "100%")
        ),
        div(
          class = "market-insight-mini-chart",
          div(class = "market-insight-mini-heading", "北交所国家级专精特新企业数量"),
          highcharter::highchartOutput(ns("bse_specialized_new_chart"), height = "100%")
        )
      )
    )
  )
}

# 用途：市场定位 UI 模块函数，行业矩形树图由 Shiny server 动态渲染。
# 输入来源：`id` 参数（Shiny 模块命名空间）。
marketPositionUI <- function(id) {
  ns <- shiny::NS(id)
  model <- dashboard_page_models[["market_position"]]
  div(
    class = "market-position-page",
    page_model_ui(
      "market_position",
      exclude_charts = c("market_bubble", "industry_treemap"),
      extra_chart_cards = list(
        bse_listing_market_bubble_card(ns),
        market_position_right_stack_card(ns),
        global_capital_market_card(ns)
      ),
      bottom_items = list(),
      exclude_summary = TRUE
    )
  )
}

# 用途：处理市场定位各交互图表的服务端逻辑。
# 输入来源：板块成交指标下拉框、市场/指标筛选下拉框。
marketPositionServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    output$board_trading_chart <- highcharter::renderHighchart(plot_board_trading(input$board_metric))

    output$bse_annual_listing_chart <- highcharter::renderHighchart({
      plot_bse_annual_listing_bar(calc_bse_annual_listing())
    })

    output$bse_trading_growth_chart <- highcharter::renderHighchart({
      plot_bse_trading_growth_area(input$bse_growth_metric, input$bse_growth_period)
    })

    output$global_capital_market_chart <- highcharter::renderHighchart({
      plot_global_capital_market_bar(
        metric = input$global_market_metric,
        china_only = isTRUE(input$global_market_china_only),
        growth_only = isTRUE(input$global_market_growth_only)
      )
    })

    output$market_cap_distribution_chart <- highcharter::renderHighchart({
      plot_market_cap_distribution_bar(calc_market_cap_distribution())
    })

    output$pe_scatter_chart <- highcharter::renderHighchart({
      plot_company_pe_market_cap_scatter(calc_company_pe_market_cap_data())
    })

    output$enterprise_nature_chart <- highcharter::renderHighchart({
      plot_enterprise_nature_bar(calc_enterprise_nature_data())
    })
  })
}
