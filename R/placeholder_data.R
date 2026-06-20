dashboard_presentation_mode <- function() {
  mode <- tolower(trimws(Sys.getenv("BSE_PRESENTATION_MODE", unset = "placeholder")))
  if (!mode %in% c("placeholder", "semantic")) "placeholder" else mode
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

load_page_blocks <- function(path = "config/page_blocks.yml") {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("缺少 yaml 包。请安装 yaml 后重新启动应用。", call. = FALSE)
  }
  if (!file.exists(path)) stop(sprintf("页面配置不存在：%s", path), call. = FALSE)
  yaml::read_yaml(path)$pages
}

placeholder_with_seed <- function(seed, code) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv) else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
  force(code)
}

placeholder_seed <- function(page_id, offset = 0L) {
  sum(utf8ToInt(page_id)) * 101L + as.integer(offset)
}

placeholder_kpis <- function(page_id, definitions) {
  placeholder_with_seed(placeholder_seed(page_id, 1L), {
    rows <- lapply(seq_along(definitions), function(i) {
      item <- definitions[[i]]
      digits <- as.integer(item$digits %||% 0L)
      value <- round(stats::runif(1, as.numeric(item$min), as.numeric(item$max)), digits)
      change <- round(stats::runif(1, -9, 16), 1)
      status <- if (change < -3) "warning" else if (change > 2) "positive" else "neutral"
      data.frame(
        label = as.character(item$label),
        value = format(value, big.mark = ",", nsmall = digits, trim = TRUE),
        unit = as.character(item$unit),
        change = sprintf("较上期 %s%.1f%%", if (change >= 0) "+" else "", change),
        status = status,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  })
}

placeholder_insights <- function(page_id) {
  content <- list(
    home = c("四个画像页面统一采用演示数据模型，便于逐项替换业务口径。", "市场、公司、发展与质量信号被拆分为独立内容块，后续可分别接入。", "当前页面的数值、趋势和明细均不代表真实市场事实。"),
    market_position = c("本所采用蓝色高亮，其他市场使用低饱和配色，突出对标位置。", "行业结构以矩形树图呈现，后续可直接替换为真实市值汇总结果。", "规模、活跃度与估值为独立指标口径，可按最终需求分别调整。"),
    company_profile = c("行业贡献矩阵用于观察公司数、规模和经营质量之间的差异。", "四象限图区分成长性与盈利能力，便于后续切换为公司级真实数据。", "当前行业与公司名称均为演示占位，不对应实际上市公司。"),
    market_development = c("上市、融资、交易和生态被拆分为独立的时间序列内容块。", "柱线组合图保留趋势叙事结构，后续只需替换对应年度序列。", "辅助明细用于承接审核、融资和产品生态等后续钻取信息。"),
    market_quality = c("风险状态矩阵同时呈现流动性与基本面质量，便于识别重点观察区。", "热力图用于定位风险类型在行业间的相对集中度。", "风险数量、等级与公司清单当前均为演示占位，不用于监管判断。")
  )
  content[[page_id]] %||% character()
}

placeholder_summary <- function(page_id) {
  content <- list(
    home = c("统一页面模型", "可配置内容块", "默认演示模式"),
    market_position = c("对标市场规模", "行业市值结构", "估值与活跃度"),
    company_profile = c("行业经营贡献", "成长与盈利分层", "研发与募资观察"),
    market_development = c("上市融资趋势", "交易生态变化", "区域与产品服务"),
    market_quality = c("流动性风险", "质量状态分层", "行业风险集中度")
  )
  content[[page_id]] %||% character()
}

placeholder_table <- function(page_id, table_type) {
  placeholder_with_seed(placeholder_seed(page_id, 51L), {
    if (identical(table_type, "overview")) {
      return(data.frame(
        画像 = c("市场定位", "上市公司", "市场发展", "市场质量"),
        主要内容 = c("对标规模与行业结构", "经营贡献与成长质量", "上市融资与交易生态", "质量状态与风险集中度"),
        展示状态 = c("演示占位", "演示占位", "演示占位", "演示占位"),
        check.names = FALSE
      ))
    }

    industries <- c("电子", "机械设备", "医药生物", "电力设备", "计算机", "基础化工", "汽车", "其他")
    names <- paste0("演示对象", sprintf("%02d", seq_len(8)))
    switch(table_type,
      market_position = data.frame(市场 = c("本所", "沪市主板", "深市主板", "创业板", "科创板"), 总市值 = round(stats::runif(5, 0.3, 48), 2), 日均成交额 = round(stats::runif(5, 80, 2400), 0), 估值中位数 = round(stats::runif(5, 15, 48), 1), check.names = FALSE),
      company_profile = data.frame(行业 = industries, 公司数量 = sample(16:110, 8), 营收占比 = paste0(sample(4:25, 8), "%"), 净利润占比 = paste0(sample(3:28, 8), "%"), 研发强度 = paste0(round(stats::runif(8, 2.5, 10), 1), "%"), check.names = FALSE),
      market_development = data.frame(年度 = 2019:2026, 新增上市 = sample(80:360, 8), IPO融资额 = round(stats::runif(8, 120, 650), 1), 再融资额 = round(stats::runif(8, 260, 1100), 1), 活跃公司 = sample(500:1800, 8), check.names = FALSE),
      market_quality = data.frame(对象 = names, 行业 = sample(industries, 8, replace = TRUE), 风险类型 = sample(c("低流动性", "估值关注", "财务承压", "合规关注"), 8, replace = TRUE), 风险等级 = sample(c("低", "中", "高"), 8, replace = TRUE), 处置状态 = sample(c("持续观察", "跟踪中", "待确认"), 8, replace = TRUE), check.names = FALSE),
      data.frame(项目 = names, 数值 = round(stats::runif(8, 1, 100), 1), check.names = FALSE)
    )
  })
}

build_placeholder_page_models <- function(config = load_page_blocks()) {
  lapply(names(config), function(page_id) {
    definition <- config[[page_id]]
    charts <- definition$charts %||% list()
    list(
      id = page_id,
      section = definition$section,
      title = definition$title,
      subtitle = definition$subtitle,
      judgment = definition$judgment,
      layout = definition$layout %||% "overview",
      kpis = placeholder_kpis(page_id, definition$kpis),
      charts = charts,
      insights = placeholder_insights(page_id),
      summary = placeholder_summary(page_id),
      table = placeholder_table(page_id, definition$table$type),
      table_title = definition$table$title,
      table_note = definition$table$note,
      mode = "placeholder"
    )
  }) |> stats::setNames(names(config))
}
