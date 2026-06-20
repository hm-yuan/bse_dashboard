# Metrics for the market-development portrait.
# Input: the list returned by load_dashboard_data().
# Output: KPI data frames, trend data frames, or insight character vectors.

calc_development_kpis <- function(data) {
  dim_company <- metric_table(data, "dim_company")
  pipeline <- metric_table(data, "fact_listing_pipeline")
  financing <- metric_table(data, "fact_financing")
  market <- metric_table(data, "fact_market_period")
  thresholds <- metric_thresholds()

  current_year <- as.integer(format(Sys.Date(), "%Y"))
  if (metric_has_cols(market, "period")) {
    years <- suppressWarnings(as.integer(substr(as.character(market$period), 1, 4)))
    current_year <- max(years[!is.na(years)], current_year, na.rm = TRUE)
  }

  listing_year <- if (metric_has_cols(dim_company, "listing_date")) as.integer(format(metric_date(dim_company$listing_date), "%Y")) else integer()
  new_listings <- sum(listing_year == current_year, na.rm = TRUE)

  pipeline_count <- if (metric_has_cols(pipeline, "pipeline_stage")) {
    sum(!pipeline$pipeline_stage %in% c("辅导备案"), na.rm = TRUE)
  } else {
    0L
  }

  financing_year <- if (metric_has_cols(financing, "event_date")) as.integer(format(metric_date(financing$event_date), "%Y")) else integer()
  current_financing <- if (length(financing_year) > 0L) financing[financing_year == current_year, , drop = FALSE] else data.frame()
  ipo_amount <- if (metric_has_cols(current_financing, c("financing_type", "amount_yi"))) metric_safe_sum(current_financing$amount_yi[current_financing$financing_type == "IPO"]) else NA_real_
  refinancing_amount <- if (metric_has_cols(current_financing, c("financing_type", "amount_yi"))) metric_safe_sum(current_financing$amount_yi[current_financing$financing_type != "IPO"]) else NA_real_

  market_year <- if (metric_has_cols(market, "period")) suppressWarnings(as.integer(substr(as.character(market$period), 1, 4))) else integer()
  current_market <- if (length(market_year) > 0L) market[market_year == current_year, , drop = FALSE] else data.frame()
  annual_turnover <- if (metric_has_cols(current_market, "turnover_amount_yi")) metric_safe_sum(current_market$turnover_amount_yi) else NA_real_
  active_threshold <- thresholds$liquidity$low_turnover_rate
  active_company_count <- if (metric_has_cols(current_market, c("company_code", "turnover_rate"))) {
    length(unique(current_market$company_code[metric_num(current_market$turnover_rate) > active_threshold]))
  } else {
    0L
  }

  metric_make_kpis(
    labels = c("本年新增上市公司数", "在审企业数量", "IPO 融资额", "再融资额", "年度成交金额", "活跃公司数量"),
    values = c(
      metric_format_count(new_listings),
      metric_format_count(pipeline_count),
      metric_format_number(ipo_amount, 1),
      metric_format_number(refinancing_amount, 1),
      metric_format_number(annual_turnover, 1),
      metric_format_count(active_company_count)
    ),
    units = c("家", "家", "亿元", "亿元", "亿元", "家"),
    changes = rep(paste0(current_year, " 年"), 6),
    statuses = c("positive", "positive", "positive", "positive", "positive", "neutral")
  )
}

calc_listing_financing_trend <- function(data) {
  dim_company <- metric_table(data, "dim_company")
  financing <- metric_table(data, "fact_financing")
  pipeline <- metric_table(data, "fact_listing_pipeline")

  if (!metric_has_cols(dim_company, "listing_date")) {
    return(metric_empty(c("year", "new_listing_count", "ipo_financing_amount_yi", "refinancing_amount_yi", "pipeline_count")))
  }

  listing_year <- as.integer(format(metric_date(dim_company$listing_date), "%Y"))
  years <- sort(unique(listing_year[!is.na(listing_year)]))
  if (length(years) == 0L) years <- as.integer(format(Sys.Date(), "%Y"))
  financing_year <- if (metric_has_cols(financing, "event_date")) as.integer(format(metric_date(financing$event_date), "%Y")) else integer()

  out <- do.call(rbind, lapply(years, function(year) {
    current_financing <- if (length(financing_year) > 0L) financing[financing_year == year, , drop = FALSE] else data.frame()
    data.frame(
      year = year,
      new_listing_count = sum(listing_year == year, na.rm = TRUE),
      ipo_financing_amount_yi = if (metric_has_cols(current_financing, c("financing_type", "amount_yi"))) round(metric_safe_sum(current_financing$amount_yi[current_financing$financing_type == "IPO"]), 4) else NA_real_,
      refinancing_amount_yi = if (metric_has_cols(current_financing, c("financing_type", "amount_yi"))) round(metric_safe_sum(current_financing$amount_yi[current_financing$financing_type != "IPO"]), 4) else NA_real_,
      pipeline_count = if (metric_has_cols(pipeline, "company_code")) length(unique(pipeline$company_code)) else NA_integer_,
      stringsAsFactors = FALSE
    )
  }))
  out[order(out$year), , drop = FALSE]
}

calc_trading_ecosystem_trend <- function(data) {
  market <- metric_table(data, "fact_market_period")
  thresholds <- metric_thresholds()
  if (!metric_has_cols(market, c("period", "company_code", "turnover_amount_yi", "turnover_rate"))) {
    return(metric_empty(c("period", "turnover_amount_yi", "avg_turnover_rate", "active_company_count", "ecosystem_event")))
  }

  periods <- sort(unique(as.character(market$period)))
  events <- c("指数产品观察", "做市机制跟踪", "流动性改善", "融资生态联动", "产品创新储备")
  out <- do.call(rbind, lapply(seq_along(periods), function(i) {
    part <- market[market$period == periods[[i]], , drop = FALSE]
    data.frame(
      period = periods[[i]],
      turnover_amount_yi = round(metric_safe_sum(part$turnover_amount_yi), 4),
      avg_turnover_rate = round(metric_safe_mean(part$turnover_rate), 6),
      active_company_count = length(unique(part$company_code[metric_num(part$turnover_rate) > thresholds$liquidity$low_turnover_rate])),
      ecosystem_event = events[((i - 1L) %% length(events)) + 1L],
      stringsAsFactors = FALSE
    )
  }))
  out[order(out$period), , drop = FALSE]
}

calc_development_insights <- function(data) {
  kpis <- calc_development_kpis(data)
  trend <- calc_listing_financing_trend(data)
  latest_year <- if (nrow(trend) > 0L) max(trend$year, na.rm = TRUE) else NA_integer_
  latest_new <- if (nrow(trend) > 0L) trend$new_listing_count[trend$year == latest_year][[1L]] else NA_integer_
  c(
    paste0("本年新增上市公司数为 ", kpis$value[kpis$label == "本年新增上市公司数"][[1]], " 家，在审企业数量为 ", kpis$value[kpis$label == "在审企业数量"][[1]], " 家。"),
    paste0("IPO 与再融资合计形成阶段性资金供给，最新年度新增上市 ", metric_format_count(latest_new), " 家。"),
    paste0("年度成交金额为 ", kpis$value[kpis$label == "年度成交金额"][[1]], " 亿元，活跃公司数量 ", kpis$value[kpis$label == "活跃公司数量"][[1]], " 家。")
  )
}

calc_development_detail <- function(data) {
  listing <- calc_listing_financing_trend(data)
  trading <- calc_trading_ecosystem_trend(data)
  if (!metric_has_cols(listing, "year")) {
    return(metric_empty(c("year", "new_listing_count", "financing_amount_yi", "active_company_count", "ecosystem_event")))
  }

  trading_year <- if (metric_has_cols(trading, "period")) as.integer(substr(as.character(trading$period), 1, 4)) else integer()
  out <- do.call(rbind, lapply(listing$year, function(year) {
    trend <- trading[trading_year == year, , drop = FALSE]
    source_row <- listing[listing$year == year, , drop = FALSE]
    data.frame(
      year = year,
      new_listing_count = source_row$new_listing_count[[1L]],
      financing_amount_yi = round(source_row$ipo_financing_amount_yi[[1L]] + source_row$refinancing_amount_yi[[1L]], 4),
      active_company_count = if (metric_has_cols(trend, "active_company_count")) max(trend$active_company_count, na.rm = TRUE) else NA_integer_,
      ecosystem_event = if (metric_has_cols(trend, "ecosystem_event")) paste(unique(trend$ecosystem_event), collapse = "、") else NA_character_,
      stringsAsFactors = FALSE
    )
  }))
  out[order(out$year, decreasing = TRUE), , drop = FALSE]
}
