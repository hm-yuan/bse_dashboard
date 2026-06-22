# Metrics for the market-position portrait.
# Input: the list returned by load_dashboard_data().
# Output: KPI data frames, chart-ready data frames, or insight character vectors.

# 用途：构造包含指定列的空数据框，作为缺省返回值
# 输入来源：参数 `cols`（列名向量）
metric_empty <- function(cols) {
  out <- as.data.frame(setNames(replicate(length(cols), logical(), simplify = FALSE), cols))
  out[0, , drop = FALSE]
}

# 用途：从 dashboard 数据列表中按名称提取数据表
# 输入来源：参数 `data`（dashboard 数据列表，例如 `dashboard_data`），参数 `name`（表名）
metric_table <- function(data, name) {
  x <- data[[name]]
  if (is.null(x) || !is.data.frame(x)) {
    return(data.frame())
  }
  x
}

# 用途：检查数据框是否为非空且包含指定全部列
# 输入来源：参数 `df`（数据框），参数 `cols`（列名向量）
metric_has_cols <- function(df, cols) {
  is.data.frame(df) && nrow(df) > 0L && all(cols %in% names(df))
}

# 用途：将输入向量安全转换为数值型
# 输入来源：参数 `x`（待转换向量）
metric_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

# 用途：将输入向量安全转换为日期型
# 输入来源：参数 `x`（待转换向量）
metric_date <- function(x) {
  suppressWarnings(as.Date(x))
}

# 用途：从数据框中按字典序提取最新的 period 值
# 输入来源：参数 `df`（包含 period 列的数据框），参数 `period_col`（period 列名，默认 "period"）
metric_latest_period <- function(df, period_col = "period") {
  if (!metric_has_cols(df, period_col)) {
    return(NA_character_)
  }
  periods <- as.character(df[[period_col]])
  periods <- periods[!is.na(periods) & nzchar(periods)]
  if (length(periods) == 0L) {
    return(NA_character_)
  }
  sort(unique(periods), decreasing = TRUE)[[1L]]
}

# 用途：对输入向量进行安全求和，全为 NA 时返回 NA
# 输入来源：参数 `x`（数值向量）
metric_safe_sum <- function(x) {
  x <- metric_num(x)
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

# 用途：对输入向量进行安全求中位数，排除非有限值
# 输入来源：参数 `x`（数值向量）
metric_safe_median <- function(x) {
  x <- metric_num(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L) NA_real_ else stats::median(x)
}

# 用途：对输入向量进行安全求均值，排除非有限值
# 输入来源：参数 `x`（数值向量）
metric_safe_mean <- function(x) {
  x <- metric_num(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L) NA_real_ else mean(x)
}

# 用途：将数值格式化为千分位计数字符串，缺失时返回 "--"
# 输入来源：参数 `x`（待格式化数值）
metric_format_count <- function(x) {
  if (length(x) == 0L || is.na(x)) return("--")
  format(round(metric_num(x), 0), big.mark = ",", scientific = FALSE, trim = TRUE)
}

# 用途：将数值格式化为保留指定小数位的千分位字符串，缺失时返回 "--"
# 输入来源：参数 `x`（待格式化数值），参数 `digits`（小数位数，默认 1）
metric_format_number <- function(x, digits = 1) {
  if (length(x) == 0L || is.na(x)) return("--")
  format(round(metric_num(x), digits), big.mark = ",", scientific = FALSE, trim = TRUE, nsmall = digits)
}

# 用途：将小数比例格式化为百分数字符串，缺失时返回 "--"
# 输入来源：参数 `x`（小数比例），参数 `digits`（小数位数，默认 1）
metric_format_percent <- function(x, digits = 1) {
  if (length(x) == 0L || is.na(x)) return("--")
  metric_format_number(metric_num(x) * 100, digits)
}



# 用途：将标签、数值、单位、变动、状态组装为 KPI 数据框
# 输入来源：参数 `labels`、`values`、`units`、`changes`、`statuses`
metric_make_kpis <- function(labels, values, units, changes = rep("", length(labels)),
                             statuses = rep("neutral", length(labels))) {
  if (exists("make_kpis", mode = "function")) {
    return(make_kpis(labels, values, units, changes, statuses))
  }
  data.frame(label = labels, value = values, unit = units, change = changes, status = statuses, stringsAsFactors = FALSE)
}

# 用途：返回 metrics 模块使用的默认阈值配置
# 输入来源：硬编码默认值
metric_defaults <- function() {
  list(
    liquidity = list(low_turnover_rate = 0.01, low_average_daily_turnover_million = 5),
    valuation = list(high_pe = 60, low_growth = 0),
    financial = list(continuous_loss_years = 2, negative_operating_cashflow_years = 2),
    fundraising = list(low_use_progress = 0.45)
  )
}

# 用途：递归合并两个列表，override 覆盖 base 的同名节点
# 输入来源：参数 `base`（基础列表），参数 `override`（覆盖列表）
metric_deep_merge <- function(base, override) {
  for (nm in names(override)) {
    if (is.list(base[[nm]]) && is.list(override[[nm]])) {
      base[[nm]] <- metric_deep_merge(base[[nm]], override[[nm]])
    } else {
      base[[nm]] <- override[[nm]]
    }
  }
  base
}

# 用途：在不使用 yaml 包时，简易解析 thresholds YAML 文件
# 输入来源：参数 `path`（YAML 文件路径，例如 `config/thresholds.yml`）
metric_parse_thresholds_simple <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  out <- list()
  current <- NULL
  for (line in lines) {
    if (!nzchar(trimws(line)) || grepl("^\\s*#", line)) next
    if (grepl("^[A-Za-z_]+:\\s*$", line)) {
      current <- sub(":\\s*$", "", trimws(line))
      out[[current]] <- list()
      next
    }
    hit <- regexec("^\\s+([A-Za-z_]+):\\s*([^#]+)\\s*$", line)
    match <- regmatches(line, hit)[[1]]
    if (!is.null(current) && length(match) == 3L) {
      value <- trimws(gsub("\"", "", match[[3]], fixed = TRUE))
      num <- suppressWarnings(as.numeric(value))
      out[[current]][[match[[2]]]] <- if (!is.na(num)) num else value
    }
  }
  out
}

# 用途：计算各板块上市公司市值区间分布。
#       按板块统计不同市值区间的公司数量及占比：
#       <50亿、50-100亿、100-300亿、300-1000亿、1000亿以上。
# 输入来源：data/raw/上市公司基本情况.xlsx
# 输出：data.frame(board, bucket, count, pct)
calc_market_cap_distribution <- function(path = "data/raw/上市公司基本情况.xlsx") {
  if (!file.exists(path) || !requireNamespace("readxl", quietly = TRUE)) {
    return(data.frame(board = character(), bucket = character(), count = integer(), pct = numeric()))
  }

  raw <- as.data.frame(readxl::read_excel(path, sheet = "公司", .name_repair = "unique"), stringsAsFactors = FALSE)
  required <- c("代码", "上市板块", "总市值")
  if (!all(required %in% names(raw))) {
    return(data.frame(board = character(), bucket = character(), count = integer(), pct = numeric()))
  }

  code_suffix <- gsub(".*\\.", "", as.character(raw[["代码"]]))
  board <- ifelse(
    raw[["上市板块"]] == "北证", "北交所",
    ifelse(raw[["上市板块"]] == "创业板", "创业板",
      ifelse(raw[["上市板块"]] == "科创板", "科创板",
        ifelse(raw[["上市板块"]] == "主板" & code_suffix == "SH", "上证主板",
          ifelse(raw[["上市板块"]] == "主板" & code_suffix == "SZ", "深证主板", "其他")
        )
      )
    )
  )

  cap <- chart_safe_number(raw[["总市值"]])
  valid <- !is.na(cap) & cap > 0 & board %in% c("上证主板", "深证主板", "创业板", "科创板", "北交所")
  if (sum(valid) == 0L) {
    return(data.frame(board = character(), bucket = character(), count = integer(), pct = numeric()))
  }

  cap <- cap[valid]
  board <- board[valid]

  bucket <- ifelse(cap < 50, "<50亿",
    ifelse(cap < 100, "50-100亿",
      ifelse(cap < 300, "100-300亿",
        ifelse(cap < 1000, "300-1000亿", "1000亿以上")
      )
    )
  )

  board_order <- c("上证主板", "深证主板", "创业板", "科创板", "北交所")
  bucket_order <- c("<50亿", "50-100亿", "100-300亿", "300-1000亿", "1000亿以上")

  df <- as.data.frame(table(board = factor(board, levels = board_order), bucket = factor(bucket, levels = bucket_order)), stringsAsFactors = FALSE)
  names(df) <- c("board", "bucket", "count")
  df$count <- as.integer(df$count)

  totals <- tapply(df$count, df$board, sum)
  df$pct <- round(df$count / totals[df$board] * 100, 1)

  rownames(df) <- NULL
  df
}

# 用途：加载并合并默认阈值与外部 thresholds YAML 配置
# 输入来源：`config/thresholds.yml`，硬编码默认阈值
metric_thresholds <- function(path = "config/thresholds.yml") {
  defaults <- metric_defaults()
  if (!file.exists(path)) {
    return(defaults)
  }

  parsed <- tryCatch({
    if (requireNamespace("yaml", quietly = TRUE)) {
      yaml::read_yaml(path)
    } else {
      metric_parse_thresholds_simple(path)
    }
  }, error = function(err) NULL)

  if (is.null(parsed)) {
    return(defaults)
  }
  metric_deep_merge(defaults, parsed)
}

# 用途：从财务周期表中提取最新期和上一期的数据子集
# 输入来源：参数 `financial`（例如 `dashboard_data$fact_financial_period`）
metric_latest_financial_pair <- function(financial) {
  if (!metric_has_cols(financial, c("period", "company_code"))) {
    return(list(latest = data.frame(), previous = data.frame(), latest_period = NA_character_, previous_period = NA_character_))
  }
  periods <- sort(unique(as.character(financial$period)))
  periods <- periods[!is.na(periods) & nzchar(periods)]
  latest_period <- if (length(periods) > 0L) tail(periods, 1L) else NA_character_
  previous_period <- if (length(periods) > 1L) tail(periods, 2L)[[1L]] else NA_character_
  list(
    latest = financial[financial$period == latest_period, , drop = FALSE],
    previous = if (!is.na(previous_period)) financial[financial$period == previous_period, , drop = FALSE] else data.frame(),
    latest_period = latest_period,
    previous_period = previous_period
  )
}

# 用途：计算市场定位的核心 KPI 卡片数据
# 输入来源：`dashboard_data$market_position_kpi`，fallback 为 `dashboard_data$market_position$kpis`
calc_market_position_kpis <- function(data) {
  kpi <- metric_table(data, "market_position_kpi")
  if (!metric_has_cols(kpi, c("listed_company_count", "total_market_cap_yi", "float_market_cap_yi", "avg_daily_turnover_yi", "pe_median", "top10_market_cap_share"))) {
    fallback <- data$market_position$kpis
    if (is.data.frame(fallback)) return(fallback)
    return(metric_make_kpis(c("上市公司数量", "总市值", "流通市值", "日均成交额", "PE 中位数", "前十大公司市值占比"), rep("--", 6), c("家", "亿元", "亿元", "亿元", "倍", "%")))
  }

  row <- kpi[1, , drop = FALSE]
  as_of <- format(as.Date(row$as_of_date[[1]]), "%Y-%m-%d")
  current_year_new <- metric_format_count(row$current_year_new_listed_count[[1]])

  detail <- metric_table(data, "market_position_company_detail")
  top3_names <- "--"
  if (metric_has_cols(detail, c("company_name", "total_market_cap_yi"))) {
    ordered <- detail[order(-metric_num(detail$total_market_cap_yi)), , drop = FALSE]
    top3_names <- paste(utils::head(ordered$company_name, 3L), collapse = "、")
    if (!nzchar(top3_names)) top3_names <- "--"
  }

  metric_make_kpis(
    labels = c("上市公司数量", "总市值", "流通市值", "日均成交额", "PE 中位数", "前十大公司市值占比"),
    values = c(
      metric_format_count(row$listed_company_count[[1]]),
      metric_format_count(row$total_market_cap_yi[[1]]),
      metric_format_count(row$float_market_cap_yi[[1]]),
      metric_format_number(row$avg_daily_turnover_yi[[1]], 1),
      metric_format_number(row$pe_median[[1]], 1),
      metric_format_percent(row$top10_market_cap_share[[1]], 1)
    ),
    units = c("家", "亿元", "亿元", "亿元", "倍", "%"),
    changes = c(
      paste0("2026年新上市公司", current_year_new, "家"),
      paste0("更新日期：", as_of),
      paste0("更新日期：", as_of),
      paste0("更新日期：", as_of),
      paste0("更新日期：", as_of),
      top3_names
    ),
    statuses = c("positive", "positive", "positive", "positive", "neutral", "neutral")
  )
}

# 用途：生成市场定位气泡图数据，包含北交所与其他板块对比
# 输入来源：`dashboard_data$market_position_kpi`，并硬编码其他市场板块数据
calc_market_position_bubble <- function(data) {
  kpi <- metric_table(data, "market_position_kpi")
  if (!metric_has_cols(kpi, c("listed_company_count", "total_market_cap_yi", "avg_daily_turnover_yi", "pe_median"))) {
    return(metric_empty(c("market", "total_market_cap_yi", "avg_daily_turnover_yi", "listed_company_count", "pe_median", "is_bse")))
  }
  row <- kpi[1, , drop = FALSE]
  data.frame(
    market = c("北交所", "沪市主板", "深市主板", "创业板", "科创板"),
    total_market_cap_yi = c(metric_num(row$total_market_cap_yi[[1]]), 468000, 235000, 121000, 78000),
    avg_daily_turnover_yi = c(metric_num(row$avg_daily_turnover_yi[[1]]), 4100, 3260, 2140, 890),
    listed_company_count = c(metric_num(row$listed_company_count[[1]]), 1690, 1510, 1350, 580),
    pe_median = c(metric_num(row$pe_median[[1]]), 12.8, 18.2, 31.5, 43.6),
    is_bse = c(TRUE, FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
}

# 用途：计算各行业公司数量、市值、成交额及 PE 中位数的结构分布
# 输入来源：`dashboard_data$market_position_company_detail`、`dashboard_data$fact_market_period`
calc_market_industry_structure <- function(data) {
  detail <- metric_table(data, "market_position_company_detail")
  market <- metric_table(data, "fact_market_period")
  latest_period <- metric_latest_period(market)
  latest_market <- if (!is.na(latest_period)) market[market$period == latest_period, , drop = FALSE] else data.frame()

  if (!metric_has_cols(detail, c("company_code", "industry", "total_market_cap_yi", "pe"))) {
    return(metric_empty(c("industry", "company_count", "market_cap_yi", "market_cap_share", "turnover_amount_yi", "turnover_share", "pe_median")))
  }

  merged <- merge(detail, latest_market[, intersect(names(latest_market), c("company_code", "turnover_amount_yi")), drop = FALSE], by = "company_code", all.x = TRUE)
  industries <- sort(unique(merged$industry))
  total_cap <- metric_safe_sum(merged$total_market_cap_yi)
  total_turnover <- metric_safe_sum(merged$turnover_amount_yi)

  out <- do.call(rbind, lapply(industries, function(industry) {
    part <- merged[merged$industry == industry, , drop = FALSE]
    cap <- metric_safe_sum(part$total_market_cap_yi)
    turnover <- metric_safe_sum(part$turnover_amount_yi)
    data.frame(
      industry = industry,
      company_count = nrow(part),
      market_cap_yi = round(cap, 4),
      market_cap_share = round(cap / total_cap, 6),
      turnover_amount_yi = round(turnover, 4),
      turnover_share = if (!is.na(total_turnover) && total_turnover > 0) round(turnover / total_turnover, 6) else NA_real_,
      pe_median = round(metric_safe_median(part$pe), 4),
      stringsAsFactors = FALSE
    )
  }))
  out[order(out$market_cap_yi, decreasing = TRUE), , drop = FALSE]
}

# 用途：生成市场定位的文字洞察
# 输入来源：`dashboard_data$market_position_kpi`、`calc_market_industry_structure()` 结果
calc_market_position_insights <- function(data) {
  kpi <- metric_table(data, "market_position_kpi")
  industry <- calc_market_industry_structure(data)
  if (!metric_has_cols(kpi, c("listed_company_count", "total_market_cap_yi", "top10_market_cap_share"))) {
    fallback <- data$market_position$insights
    if (!is.null(fallback)) return(fallback)
    return(character())
  }
  row <- kpi[1, , drop = FALSE]
  lead_industry <- if (nrow(industry) > 0L) industry$industry[[1L]] else "重点行业"
  c(
    paste0("上市公司数量为 ", metric_format_count(row$listed_company_count[[1]]), " 家，总市值 ", metric_format_count(row$total_market_cap_yi[[1]]), " 亿元。"),
    paste0("行业市值结构中，", lead_industry, " 当前贡献最高，是定位画像的主要产业线索。"),
    paste0("前十大公司市值占比 ", metric_format_percent(row$top10_market_cap_share[[1]], 1), "%，可用于观察头部集中度变化。")
  )
}

# 用途：生成市场定位的公司明细表
# 输入来源：`dashboard_data$market_position_company_detail`
calc_market_company_detail <- function(data) {
  detail <- metric_table(data, "market_position_company_detail")
  if (!metric_has_cols(detail, c("company_code", "company_name", "industry", "total_market_cap_yi", "float_market_cap_yi", "pe"))) {
    return(metric_empty(c("company_code", "company_name", "industry", "total_market_cap_yi", "float_market_cap_yi", "pe", "market_cap_rank")))
  }
  cols <- intersect(c("company_code", "company_name", "board", "listing_date", "industry", "total_market_cap_yi", "float_market_cap_yi", "pe", "market_cap_rank", "is_top10_market_cap"), names(detail))
  out <- detail[, cols, drop = FALSE]
  if ("market_cap_rank" %in% names(out)) {
    out <- out[order(metric_num(out$market_cap_rank), na.last = TRUE), , drop = FALSE]
  }
  out
}

# 用途：汇总四个画像的 KPI，生成首页概览的 8 项核心指标
# 输入来源：`calc_market_position_kpis()`、`calc_company_profile_kpis()`、`calc_development_kpis()` 结果及 `dashboard_data$fact_risk_tag`
calc_home_overview_kpis <- function(data) {
  market <- calc_market_position_kpis(data)
  company <- calc_company_profile_kpis(data)
  development <- calc_development_kpis(data)
  risk <- metric_table(data, "fact_risk_tag")

  risk_company_count <- if (metric_has_cols(risk, "company_code")) length(unique(risk$company_code)) else NA_integer_
  risk_kpi <- metric_make_kpis(
    labels = "风险公司数量",
    values = metric_format_count(risk_company_count),
    units = "家",
    changes = "最新期",
    statuses = "warning"
  )

  rbind(
    market[c(1, 2, 4, 5), , drop = FALSE],
    development[3, , drop = FALSE],
    company[c(3, 5), , drop = FALSE],
    risk_kpi
  )
}

# 用途：读取市场板块成交统计原始 Excel，提取各板块最新日均成交额及成交额
# 输入来源：data/raw/市场板块成交统计.xlsx
calc_board_trading_data <- function(path = "data/raw/市场板块成交统计.xlsx") {
  if (!file.exists(path)) return(data.frame())
  if (!requireNamespace("readxl", quietly = TRUE)) return(data.frame())

  raw <- as.data.frame(readxl::read_excel(path, .name_repair = "unique"), stringsAsFactors = FALSE)

  # 第一行是子标题行（成交量、成交额、日均成交量、日均成交额），实际数据从第 2 行开始
  # 宽表结构：日期 | 上证主板×4 | 深证主板×4 | 创业板×4 | 科创板×4 | 北交所×4 | ETF×4
  board_configs <- list(
    list(name = "上证主板", start_col = 3),
    list(name = "深证主板", start_col = 7),
    list(name = "创业板",   start_col = 11),
    list(name = "科创板",   start_col = 15),
    list(name = "北交所",   start_col = 19)
  )

  # 提取日期列
  date_col <- raw[[2]]
  date_vals <- suppressWarnings(as.Date(date_col))
  valid_rows <- which(!is.na(date_vals))

  if (length(valid_rows) == 0L) return(data.frame())

  board_names <- c()
  dates <- c()
  turnover_amounts <- c()  # 成交额(亿元) - col offset 2
  avg_daily_turnovers <- c() # 日均成交额(亿元) - col offset 4

  for (cfg in board_configs) {
    sc <- cfg$start_col
    # 成交额列 = start_col + 1, 日均成交额列 = start_col + 3
    amt_col <- sc + 1
    daily_col <- sc + 3

    if (daily_col > ncol(raw)) next

    for (r in valid_rows) {
      board_names <- c(board_names, cfg$name)
      dates <- c(dates, date_vals[r])

      amt_val <- suppressWarnings(as.numeric(raw[[amt_col]][r]))
      daily_val <- suppressWarnings(as.numeric(raw[[daily_col]][r]))

      turnover_amounts <- c(turnover_amounts, if (is.na(amt_val)) NA_real_ else amt_val)
      avg_daily_turnovers <- c(avg_daily_turnovers, if (is.na(daily_val)) NA_real_ else daily_val)
    }
  }

  out <- data.frame(
    board = board_names,
    date = as.Date(dates, origin = "1970-01-01"),
    avg_daily_turnover_yi = avg_daily_turnovers,
    turnover_amount_yi = turnover_amounts,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$avg_daily_turnover_yi), , drop = FALSE]
  out
}

# 用途：汇总四个画像的洞察，生成首页概览的 4 条文字摘要
# 输入来源：`calc_market_position_insights()`、`calc_company_profile_insights()`、`calc_development_insights()`、`calc_quality_insights()` 结果
calc_home_overview_insights <- function(data) {
  c(
    calc_market_position_insights(data)[[1]],
    calc_company_profile_insights(data)[[1]],
    calc_development_insights(data)[[1]],
    calc_quality_insights(data)[[1]]
  )
}

# 用途：读取上市公司基本情况 Excel，按所选市场统计各大类行业的公司数量或总市值。
#       返回前 7 大行业，其余行业合并为“其他行业”。
# 输入来源：data/raw/上市公司基本情况.xlsx
# 输出列：industry（行业）、value（数值）、unit_label（单位）、share（占比）
calc_market_industry_distribution <- function(market = "全部A股", metric = "company_count", path = "data/raw/上市公司基本情况.xlsx", top_n = 7L) {
  if (!file.exists(path)) {
    return(metric_empty(c("industry", "value", "unit_label", "share")))
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    return(metric_empty(c("industry", "value", "unit_label", "share")))
  }

  raw <- as.data.frame(readxl::read_excel(path, sheet = "公司", .name_repair = "unique"), stringsAsFactors = FALSE)
  required <- c("代码", "上市板块")
  if (!all(required %in% names(raw))) {
    return(metric_empty(c("industry", "value", "unit_label", "share")))
  }

  if (!("大类行业" %in% names(raw)) && !("行业" %in% names(raw))) {
    return(metric_empty(c("industry", "value", "unit_label", "share")))
  }

  if (!("大类行业" %in% names(raw))) {
    raw[["大类行业"]] <- raw[["行业"]]
  }

  code_suffix <- gsub(".*\\.", "", as.character(raw[["代码"]]))
  market_board <- ifelse(
    raw[["上市板块"]] == "北证", "北交所",
    ifelse(
      raw[["上市板块"]] == "创业板", "创业板",
      ifelse(
        raw[["上市板块"]] == "科创板", "科创板",
        ifelse(
          raw[["上市板块"]] == "主板" & code_suffix == "SH", "上证主板",
          ifelse(raw[["上市板块"]] == "主板" & code_suffix == "SZ", "深证主板", "其他")
        )
      )
    )
  )

  if (!identical(market, "全部A股")) {
    raw <- raw[market_board == market, , drop = FALSE]
  }

  if (nrow(raw) == 0L) {
    return(metric_empty(c("industry", "value", "unit_label", "share")))
  }

  industries <- as.character(raw[["大类行业"]])
  industries[is.na(industries) | industries == ""] <- "未分类"

  use_market_cap <- identical(metric, "market_cap")

  out <- data.frame(
    industry = industries,
    stringsAsFactors = FALSE
  )

  if (use_market_cap) {
    out$value <- chart_safe_number(raw[["总市值"]])
    out$value[is.na(out$value)] <- 0
    out <- stats::aggregate(value ~ industry, data = out, FUN = sum)
    out$value <- round(out$value)
    out$unit_label <- "亿元"
    sort_col <- "value"
  } else {
    out$value <- 1L
    out <- stats::aggregate(value ~ industry, data = out, FUN = sum)
    out$value <- as.integer(out$value)
    out$unit_label <- "家"
    sort_col <- "value"
  }

  if (nrow(out) > top_n) {
    top_industries <- out$industry[order(out[[sort_col]], decreasing = TRUE)][seq_len(top_n)]
    out$industry <- ifelse(out$industry %in% top_industries, out$industry, "其他行业")
    out <- stats::aggregate(value ~ industry, data = out, FUN = sum)
    out$unit_label <- if (use_market_cap) "亿元" else "家"
  }

  total <- sum(out$value, na.rm = TRUE)
  out$share <- if (total > 0) out$value / total else 0
  out <- out[order(out$value, decreasing = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# 用途：读取上市公司基本情况 Excel，获取每家公司所属板块、总市值和市盈率。
#       用于绘制市值-市盈率散点图，过滤掉市值或市盈率缺失/非正的数据。
# 输入来源：data/raw/上市公司基本情况.xlsx
# 输出列：company_name（公司名称）、board（板块）、market_cap_yi（总市值，亿元）、pe（市盈率）
calc_company_pe_market_cap_data <- function(path = "data/raw/上市公司基本情况.xlsx") {
  if (!file.exists(path)) {
    return(metric_empty(c("company_name", "board", "market_cap_yi", "pe")))
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    return(metric_empty(c("company_name", "board", "market_cap_yi", "pe")))
  }

  raw <- as.data.frame(readxl::read_excel(path, sheet = "公司", .name_repair = "unique"), stringsAsFactors = FALSE)
  required <- c("名称", "代码", "上市板块", "总市值", "市盈率")
  if (!all(required %in% names(raw))) {
    return(metric_empty(c("company_name", "board", "market_cap_yi", "pe")))
  }

  code_suffix <- gsub(".*\\.", "", as.character(raw[["代码"]]))
  board <- ifelse(
    raw[["上市板块"]] == "北证", "北交所",
    ifelse(
      raw[["上市板块"]] == "创业板", "创业板",
      ifelse(
        raw[["上市板块"]] == "科创板", "科创板",
        ifelse(
          raw[["上市板块"]] == "主板" & code_suffix == "SH", "上证主板",
          ifelse(raw[["上市板块"]] == "主板" & code_suffix == "SZ", "深证主板", "其他")
        )
      )
    )
  )

  out <- data.frame(
    company_name = as.character(raw[["名称"]]),
    board = board,
    market_cap_yi = chart_safe_number(raw[["总市值"]]),
    pe = chart_safe_number(raw[["市盈率"]]),
    stringsAsFactors = FALSE
  )

  out <- out[!is.na(out$market_cap_yi) & out$market_cap_yi > 0 & !is.na(out$pe) & out$pe > 0, , drop = FALSE]
  out <- out[out$board != "其他", , drop = FALSE]
  rownames(out) <- NULL
  out
}

# 用途：生成企业所有权性质分布的演示数据。
#       当真实 Excel 缺少[企业性质]列时，作为占位数据保证图表可渲染。
# 输出列：board（板块）、nature（企业性质）、count（公司数量）、pct（百分比）
enterprise_nature_demo_data <- function() {
  board_order <- c("上证主板", "深证主板", "创业板", "科创板", "北交所")
  nature_order <- c("国有企业", "民营企业", "外资企业", "公众企业")

  # 各板块公司总数（演示用），反映主板 larger、北交所 smaller 的特征
  totals <- c(1690, 1510, 1350, 580, 249)
  names(totals) <- board_order

  # 各板块 4 类性质的近似占比（演示用），主板国企偏高，北交所民营偏高
  share_matrix <- matrix(c(
    0.65, 0.25, 0.05, 0.05, # 上证主板
    0.45, 0.40, 0.08, 0.07, # 深证主板
    0.15, 0.65, 0.12, 0.08, # 创业板
    0.13, 0.55, 0.20, 0.12, # 科创板
    0.07, 0.75, 0.10, 0.08  # 北交所
  ), nrow = 5, byrow = TRUE)
  rownames(share_matrix) <- board_order
  colnames(share_matrix) <- nature_order

  counts <- do.call(rbind, lapply(board_order, function(b) {
    n <- totals[[b]]
    shares <- share_matrix[b, ]
    cnt <- round(n * shares)
    cnt[1L] <- cnt[1L] + (n - sum(cnt))
    data.frame(
      board = b,
      nature = nature_order,
      count = as.integer(cnt),
      pct = round(cnt / n * 100, 1),
      stringsAsFactors = FALSE
    )
  }))

  counts$board <- factor(counts$board, levels = board_order)
  counts$nature <- factor(counts$nature, levels = nature_order)
  rownames(counts) <- NULL
  counts
}

# 用途：读取上市公司基本情况 Excel，按板块和企业性质统计公司数量及占比。
#       固定输出 4 类企业性质：国有企业、民营企业、外资企业、公众企业。
#       地方国有企业、中央国有企业统一合并为"国有企业"；公众企业、集体企业、
#       其他企业、空值及未识别值统一合并为"公众企业"。
#       若 Excel 缺少[企业性质]列，自动回退到演示数据，保证页面不空白。
# 输入来源：data/raw/上市公司基本情况.xlsx（推荐包含"企业性质"列）
# 输出列：board（板块）、nature（企业性质）、count（公司数量）、pct（百分比）
calc_enterprise_nature_data <- function(path = "data/raw/上市公司基本情况.xlsx") {
  board_order <- c("上证主板", "深证主板", "创业板", "科创板", "北交所")
  nature_order <- c("国有企业", "民营企业", "外资企业", "公众企业")

  has_real_data <- function(df) {
    is.data.frame(df) &&
      nrow(df) > 0L &&
      all(c("企业性质", "代码", "上市板块") %in% names(df))
  }

  read_real_data <- function(raw) {
    code_suffix <- gsub(".*\\.", "", as.character(raw[["代码"]]))
    board <- ifelse(
      raw[["上市板块"]] == "北证", "北交所",
      ifelse(
        raw[["上市板块"]] == "创业板", "创业板",
        ifelse(
          raw[["上市板块"]] == "科创板", "科创板",
          ifelse(
            raw[["上市板块"]] == "主板" & code_suffix == "SH", "上证主板",
            ifelse(raw[["上市板块"]] == "主板" & code_suffix == "SZ", "深证主板", "其他")
          )
        )
      )
    )

    nature <- as.character(raw[["企业性质"]])
    nature[is.na(nature) | nature == ""] <- "其他"

    state_owned <- c("央企", "中央国有企业", "国企", "国有企业", "地方国企", "地方国有企业")
    public <- c("公众企业", "集体企业", "其他企业", "其他")
    private <- c("民营企业", "私营企业")
    foreign <- c("外资企业", "外商独资企业", "中外合资企业")

    nature <- ifelse(nature %in% state_owned, "国有企业",
      ifelse(nature %in% private, "民营企业",
        ifelse(nature %in% foreign, "外资企业",
          "公众企业")))

    out <- data.frame(board = board, nature = nature, stringsAsFactors = FALSE)
    out <- out[out$board != "其他", , drop = FALSE]

    out$board <- factor(out$board, levels = board_order)
    out$nature <- factor(out$nature, levels = nature_order)

    counts <- as.data.frame(table(out$board, out$nature), stringsAsFactors = FALSE)
    names(counts) <- c("board", "nature", "count")
    counts$count <- as.integer(counts$count)

    pcts <- stats::aggregate(count ~ board, data = counts, FUN = sum)
    names(pcts)[2L] <- "total"
    counts <- merge(counts, pcts, by = "board")
    counts$pct <- ifelse(counts$total > 0, counts$count / counts$total * 100, 0)
    counts$total <- NULL
    counts$board <- factor(counts$board, levels = board_order)
    counts$nature <- factor(counts$nature, levels = nature_order)
    counts <- counts[order(counts$board, counts$nature), , drop = FALSE]
    rownames(counts) <- NULL
    counts
  }

  if (file.exists(path) && requireNamespace("readxl", quietly = TRUE)) {
    raw <- as.data.frame(readxl::read_excel(path, sheet = "公司", .name_repair = "unique"), stringsAsFactors = FALSE)
    if (has_real_data(raw)) {
      return(read_real_data(raw))
    }
  }

  enterprise_nature_demo_data()
}

# 用途：计算北交所上市公司中研发强度超过阈值的企业比例。
#       研发强度 = 2025年研发支出 / 2025年营业收入。
#       若 Excel 缺少所需列，返回演示比例 0.62。
# 输入来源：data/raw/上市公司基本情况.xlsx（必须包含"上市板块"、"2025年研发支出"、"2025年营业收入"列）
# 输出：0~1 之间的比例数值
calc_bse_rd_intensity_ratio <- function(path = "data/raw/上市公司基本情况.xlsx", threshold = 0.05) {
  if (!file.exists(path) || !requireNamespace("readxl", quietly = TRUE)) {
    return(0.62)
  }

  raw <- as.data.frame(readxl::read_excel(path, sheet = "公司", .name_repair = "unique"), stringsAsFactors = FALSE)
  required_cols <- c("上市板块", "2025年研发支出", "2025年营业收入")
  if (!all(required_cols %in% names(raw))) {
    return(0.62)
  }

  is_bse <- as.character(raw[["上市板块"]]) == "北证"
  bse <- raw[is_bse, , drop = FALSE]
  if (nrow(bse) == 0L) {
    return(0.62)
  }

  rd <- metric_num(bse[["2025年研发支出"]])
  revenue <- metric_num(bse[["2025年营业收入"]])
  # 研发支出为万元，营业收入为亿元，统一为亿元后计算强度
  intensity <- (rd / 10000) / revenue
  valid <- !is.na(intensity) & is.finite(intensity) & revenue > 0 & intensity >= 0 & intensity <= 1
  if (sum(valid) == 0L) {
    return(0.62)
  }

  mean(intensity[valid] > threshold)
}

# 用途：计算北交所上市公司中国家级专精特新企业数量。
#       当前 Excel 未包含该字段，按行业生成演示数量（北交所约 186 家）。
#       若后续 Excel 增加"国家级专精特新"列，自动读取真实数据。
# 输入来源：data/raw/上市公司基本情况.xlsx
# 输出：整数数量
calc_bse_specialized_new_count <- function(path = "data/raw/上市公司基本情况.xlsx") {
  if (file.exists(path) && requireNamespace("readxl", quietly = TRUE)) {
    raw <- as.data.frame(readxl::read_excel(path, sheet = "公司", .name_repair = "unique"), stringsAsFactors = FALSE)
    if ("国家级专精特新" %in% names(raw)) {
      is_bse <- as.character(raw[["上市板块"]]) == "北证"
      flag <- as.character(raw[["国家级专精特新"]])
      return(sum(is_bse & !is.na(flag) & flag %in% c("是", "1", "Y", "YES", "yes"), na.rm = TRUE))
    }
  }

  186L
}

# 用途：计算北交所历年上市公司数量。
#       读取原始 Excel，筛选北交所公司（上市板块 == "北证"），
#       按上市日期提取年份后统计各年新增公司数量，并计算累计存量。
# 输入来源：data/raw/上市公司基本情况.xlsx
# 输出：data.frame(year, cumulative, new)，按年份升序排列
#       cumulative = 截至上一年末已上市公司总数（堆叠图下半部分）
#       new        = 当年新上市公司数量（堆叠图上半部分）
calc_bse_annual_listing <- function(path = "data/raw/上市公司基本情况.xlsx") {
  if (!file.exists(path) || !requireNamespace("readxl", quietly = TRUE)) {
    return(data.frame(year = integer(), cumulative = integer(), new = integer()))
  }

  raw <- as.data.frame(readxl::read_excel(path, sheet = "公司", .name_repair = "unique"), stringsAsFactors = FALSE)
  if (!"上市板块" %in% names(raw)) {
    return(data.frame(year = integer(), cumulative = integer(), new = integer()))
  }

  is_bse <- as.character(raw[["上市板块"]]) == "北证"
  bse <- raw[is_bse, , drop = FALSE]
  if (nrow(bse) == 0L) {
    return(data.frame(year = integer(), cumulative = integer(), new = integer()))
  }

  # 尝试多种日期列名
  date_col <- NULL
  for (cn in c("上市日期", "上市时间", "挂牌日期", "上市日")) {
    if (cn %in% names(bse)) {
      date_col <- cn
      break
    }
  }

  if (is.null(date_col)) {
    return(data.frame(year = integer(), cumulative = integer(), new = integer()))
  }

  listing_year <- suppressWarnings(as.integer(format(as.Date(bse[[date_col]]), "%Y")))
  listing_year <- listing_year[is.finite(listing_year) & listing_year >= 1900L & listing_year <= 2100L]
  if (length(listing_year) == 0L) {
    return(data.frame(year = integer(), cumulative = integer(), new = integer()))
  }

  out <- as.data.frame(table(year = listing_year), stringsAsFactors = FALSE)
  names(out) <- c("year", "new")
  out$year <- as.integer(as.character(out$year))
  out <- out[order(out$year), , drop = FALSE]
  out$cumulative <- c(0L, cumsum(out$new)[-nrow(out)])
  out <- out[, c("year", "cumulative", "new")]
  rownames(out) <- NULL
  out
}

# 用途：读取市场板块成交统计（周度日均）Excel，提取各板块每周日均成交额。
#       结构与月度版一致，标注为周度数据源。供面积图使用。
# 输入来源：data/raw/市场板块成交统计.xlsx
calc_board_trading_weekly_data <- function(path = "data/raw/市场板块成交统计.xlsx") {
  calc_board_trading_data(path)
}
