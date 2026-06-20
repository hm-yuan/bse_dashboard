# Metrics for the market-position portrait.
# Input: the list returned by load_dashboard_data().
# Output: KPI data frames, chart-ready data frames, or insight character vectors.

metric_empty <- function(cols) {
  out <- as.data.frame(setNames(replicate(length(cols), logical(), simplify = FALSE), cols))
  out[0, , drop = FALSE]
}

metric_table <- function(data, name) {
  x <- data[[name]]
  if (is.null(x) || !is.data.frame(x)) {
    return(data.frame())
  }
  x
}

metric_has_cols <- function(df, cols) {
  is.data.frame(df) && nrow(df) > 0L && all(cols %in% names(df))
}

metric_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

metric_date <- function(x) {
  suppressWarnings(as.Date(x))
}

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

metric_safe_sum <- function(x) {
  x <- metric_num(x)
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

metric_safe_median <- function(x) {
  x <- metric_num(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L) NA_real_ else stats::median(x)
}

metric_safe_mean <- function(x) {
  x <- metric_num(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L) NA_real_ else mean(x)
}

metric_format_count <- function(x) {
  if (length(x) == 0L || is.na(x)) return("--")
  format(round(metric_num(x), 0), big.mark = ",", scientific = FALSE, trim = TRUE)
}

metric_format_number <- function(x, digits = 1) {
  if (length(x) == 0L || is.na(x)) return("--")
  format(round(metric_num(x), digits), big.mark = ",", scientific = FALSE, trim = TRUE, nsmall = digits)
}

metric_format_percent <- function(x, digits = 1) {
  if (length(x) == 0L || is.na(x)) return("--")
  metric_format_number(metric_num(x) * 100, digits)
}

metric_make_kpis <- function(labels, values, units, changes = rep("", length(labels)),
                             statuses = rep("neutral", length(labels))) {
  if (exists("make_kpis", mode = "function")) {
    return(make_kpis(labels, values, units, changes, statuses))
  }
  data.frame(label = labels, value = values, unit = units, change = changes, status = statuses, stringsAsFactors = FALSE)
}

metric_defaults <- function() {
  list(
    liquidity = list(low_turnover_rate = 0.01, low_average_daily_turnover_million = 5),
    valuation = list(high_pe = 60, low_growth = 0),
    financial = list(continuous_loss_years = 2, negative_operating_cashflow_years = 2),
    fundraising = list(low_use_progress = 0.45)
  )
}

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

calc_market_position_kpis <- function(data) {
  kpi <- metric_table(data, "market_position_kpi")
  if (!metric_has_cols(kpi, c("listed_company_count", "total_market_cap_yi", "float_market_cap_yi", "avg_daily_turnover_yi", "pe_median", "top10_market_cap_share"))) {
    fallback <- data$market_position$kpis
    if (is.data.frame(fallback)) return(fallback)
    return(metric_make_kpis(c("上市公司数量", "总市值", "流通市值", "日均成交额", "PE 中位数", "前十大公司市值占比"), rep("--", 6), c("家", "亿元", "亿元", "亿元", "倍", "%")))
  }

  row <- kpi[1, , drop = FALSE]
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
    changes = c(paste0("截至 ", row$as_of_date[[1]]), "processed", "processed", "最新一期", "中位数", "总市值口径"),
    statuses = c("positive", "positive", "positive", "positive", "neutral", "neutral")
  )
}

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

# Input: the dashboard data list. Output: the eight overview KPIs consumed by
# the home module, assembled from the four portrait metric families.
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

# Input: the dashboard data list. Output: four concise overview insight lines.
calc_home_overview_insights <- function(data) {
  c(
    calc_market_position_insights(data)[[1]],
    calc_company_profile_insights(data)[[1]],
    calc_development_insights(data)[[1]],
    calc_quality_insights(data)[[1]]
  )
}
