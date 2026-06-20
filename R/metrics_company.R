# Metrics for the company-profile portrait.
# Input: the list returned by load_dashboard_data().
# Output: KPI data frames, chart-ready data frames, or insight character vectors.

calc_company_profile_kpis <- function(data) {
  financial <- metric_table(data, "fact_financial_period")
  fundraising <- metric_table(data, "fact_fundraising_use")
  pair <- metric_latest_financial_pair(financial)
  latest <- pair$latest

  if (!metric_has_cols(latest, c("revenue_yi", "net_profit_yi", "roe", "r_and_d_ratio"))) {
    fallback <- data$company_profile$kpis
    if (is.data.frame(fallback)) return(fallback)
    return(metric_make_kpis(c("营收中位数", "净利润中位数", "盈利公司占比", "ROE 中位数", "研发费用率中位数", "募集资金使用比例"), rep("--", 6), c("亿元", "亿元", "%", "%", "%", "%")))
  }

  profitable_ratio <- mean(metric_num(latest$net_profit_yi) > 0, na.rm = TRUE)
  use_progress <- if (metric_has_cols(fundraising, "use_progress")) metric_safe_mean(fundraising$use_progress) else NA_real_

  metric_make_kpis(
    labels = c("营收中位数", "净利润中位数", "盈利公司占比", "ROE 中位数", "研发费用率中位数", "募集资金使用比例"),
    values = c(
      metric_format_number(metric_safe_median(latest$revenue_yi), 2),
      metric_format_number(metric_safe_median(latest$net_profit_yi), 2),
      metric_format_percent(profitable_ratio, 1),
      metric_format_percent(metric_safe_median(latest$roe), 1),
      metric_format_percent(metric_safe_median(latest$r_and_d_ratio), 1),
      metric_format_percent(use_progress, 1)
    ),
    units = c("亿元", "亿元", "%", "%", "%", "%"),
    changes = rep(paste0("截至 ", pair$latest_period), 6),
    statuses = c("neutral", "neutral", "positive", "positive", "positive", "positive")
  )
}

calc_company_industry_contribution <- function(data) {
  dim_company <- metric_table(data, "dim_company")
  market <- metric_table(data, "fact_market_period")
  financial <- metric_table(data, "fact_financial_period")
  latest_market_period <- metric_latest_period(market)
  pair <- metric_latest_financial_pair(financial)

  if (!metric_has_cols(dim_company, c("company_code", "industry")) || nrow(pair$latest) == 0L) {
    return(metric_empty(c("industry", "company_count", "company_share", "market_cap_yi", "market_cap_share", "revenue_yi", "revenue_share", "net_profit_yi", "net_profit_share", "r_and_d_expense_yi", "r_and_d_share")))
  }

  latest_market <- if (!is.na(latest_market_period)) market[market$period == latest_market_period, , drop = FALSE] else data.frame()
  merged <- merge(dim_company[, c("company_code", "industry"), drop = FALSE], pair$latest, by = "company_code", all.x = TRUE)
  if (metric_has_cols(latest_market, c("company_code", "total_market_cap_yi"))) {
    merged <- merge(merged, latest_market[, c("company_code", "total_market_cap_yi"), drop = FALSE], by = "company_code", all.x = TRUE)
  } else {
    merged$total_market_cap_yi <- NA_real_
  }

  total_company <- nrow(merged)
  total_cap <- metric_safe_sum(merged$total_market_cap_yi)
  total_revenue <- metric_safe_sum(merged$revenue_yi)
  total_profit <- metric_safe_sum(pmax(metric_num(merged$net_profit_yi), 0))
  total_rd <- metric_safe_sum(merged$r_and_d_expense_yi)

  industries <- sort(unique(merged$industry))
  out <- do.call(rbind, lapply(industries, function(industry) {
    part <- merged[merged$industry == industry, , drop = FALSE]
    cap <- metric_safe_sum(part$total_market_cap_yi)
    revenue <- metric_safe_sum(part$revenue_yi)
    profit <- metric_safe_sum(pmax(metric_num(part$net_profit_yi), 0))
    rd <- metric_safe_sum(part$r_and_d_expense_yi)
    data.frame(
      industry = industry,
      company_count = nrow(part),
      company_share = round(nrow(part) / total_company, 6),
      market_cap_yi = round(cap, 4),
      market_cap_share = round(cap / total_cap, 6),
      revenue_yi = round(revenue, 4),
      revenue_share = round(revenue / total_revenue, 6),
      net_profit_yi = round(profit, 4),
      net_profit_share = if (!is.na(total_profit) && total_profit > 0) round(profit / total_profit, 6) else NA_real_,
      r_and_d_expense_yi = round(rd, 4),
      r_and_d_share = round(rd / total_rd, 6),
      stringsAsFactors = FALSE
    )
  }))
  out[order(out$revenue_yi, decreasing = TRUE), , drop = FALSE]
}

calc_company_quality_quadrant <- function(data) {
  dim_company <- metric_table(data, "dim_company")
  market <- metric_table(data, "fact_market_period")
  financial <- metric_table(data, "fact_financial_period")
  pair <- metric_latest_financial_pair(financial)
  latest_market_period <- metric_latest_period(market)

  if (!metric_has_cols(dim_company, c("company_code", "company_name", "industry")) || nrow(pair$latest) == 0L || nrow(pair$previous) == 0L) {
    return(metric_empty(c("company_code", "company_name", "industry", "revenue_growth", "roe", "net_margin", "total_market_cap_yi", "r_and_d_ratio", "quadrant")))
  }

  prev <- pair$previous[, c("company_code", "revenue_yi"), drop = FALSE]
  names(prev)[names(prev) == "revenue_yi"] <- "previous_revenue_yi"
  latest <- merge(pair$latest, prev, by = "company_code", all.x = TRUE)
  latest$revenue_growth <- (metric_num(latest$revenue_yi) / metric_num(latest$previous_revenue_yi)) - 1
  latest_market <- if (!is.na(latest_market_period)) market[market$period == latest_market_period, , drop = FALSE] else data.frame()
  merged <- merge(dim_company[, c("company_code", "company_name", "industry"), drop = FALSE], latest, by = "company_code", all.x = TRUE)
  if (metric_has_cols(latest_market, c("company_code", "total_market_cap_yi"))) {
    merged <- merge(merged, latest_market[, c("company_code", "total_market_cap_yi"), drop = FALSE], by = "company_code", all.x = TRUE)
  } else {
    merged$total_market_cap_yi <- NA_real_
  }

  growth_mid <- metric_safe_median(merged$revenue_growth)
  roe_mid <- metric_safe_median(merged$roe)
  quadrant <- ifelse(metric_num(merged$revenue_growth) >= growth_mid & metric_num(merged$roe) >= roe_mid, "高成长高盈利",
    ifelse(metric_num(merged$revenue_growth) >= growth_mid, "高成长待盈利",
      ifelse(metric_num(merged$roe) >= roe_mid, "稳健盈利", "承压观察")
    )
  )

  data.frame(
    company_code = merged$company_code,
    company_name = merged$company_name,
    industry = merged$industry,
    revenue_growth = round(metric_num(merged$revenue_growth), 6),
    roe = round(metric_num(merged$roe), 6),
    net_margin = round(metric_num(merged$net_margin), 6),
    total_market_cap_yi = round(metric_num(merged$total_market_cap_yi), 4),
    r_and_d_ratio = round(metric_num(merged$r_and_d_ratio), 6),
    quadrant = quadrant,
    stringsAsFactors = FALSE
  )
}

calc_company_profile_insights <- function(data) {
  kpis <- calc_company_profile_kpis(data)
  contribution <- calc_company_industry_contribution(data)
  quadrant <- calc_company_quality_quadrant(data)
  lead_industry <- if (nrow(contribution) > 0L) contribution$industry[[1L]] else "重点行业"
  pressure_count <- if (nrow(quadrant) > 0L) sum(quadrant$quadrant == "承压观察", na.rm = TRUE) else NA_integer_
  c(
    paste0("营收中位数为 ", kpis$value[kpis$label == "营收中位数"][[1]], " 亿元，盈利公司占比 ", kpis$value[kpis$label == "盈利公司占比"][[1]], "%。"),
    paste0(lead_industry, " 在行业经营贡献中居前，是公司画像的主要结构线索。"),
    paste0("成长性与盈利能力存在分化，承压观察公司 ", metric_format_count(pressure_count), " 家。")
  )
}

calc_company_detail <- function(data) {
  dim_company <- metric_table(data, "dim_company")
  financial <- metric_table(data, "fact_financial_period")
  fundraising <- metric_table(data, "fact_fundraising_use")
  risk <- metric_table(data, "fact_risk_tag")
  pair <- metric_latest_financial_pair(financial)
  if (!metric_has_cols(dim_company, c("company_code", "company_name", "industry"))) {
    return(metric_empty(c("company_code", "company_name", "industry", "revenue_yi", "net_profit_yi", "roe", "r_and_d_ratio", "use_progress", "risk_tag")))
  }
  out <- merge(dim_company[, c("company_code", "company_name", "industry", "strategic_sector", "is_high_tech", "is_specialized_new"), drop = FALSE], pair$latest, by = "company_code", all.x = TRUE)
  if (metric_has_cols(fundraising, c("company_code", "use_progress"))) {
    out <- merge(out, fundraising[, c("company_code", "use_progress", "project_status", "is_delayed", "is_changed"), drop = FALSE], by = "company_code", all.x = TRUE)
  }
  if (metric_has_cols(risk, c("company_code", "risk_type"))) {
    risk_summary <- stats::aggregate(risk_type ~ company_code, risk, function(x) paste(sort(unique(x)), collapse = "、"))
    names(risk_summary)[names(risk_summary) == "risk_type"] <- "risk_tag"
    out <- merge(out, risk_summary, by = "company_code", all.x = TRUE)
  } else {
    out$risk_tag <- NA_character_
  }
  out$risk_tag[is.na(out$risk_tag) | !nzchar(out$risk_tag)] <- "无"
  detail_columns <- c("company_code", "company_name", "industry", "revenue_yi", "net_profit_yi", "roe", "r_and_d_ratio", "use_progress", "risk_tag")
  out <- out[, intersect(detail_columns, names(out)), drop = FALSE]
  out[order(metric_num(out$revenue_yi), decreasing = TRUE, na.last = TRUE), , drop = FALSE]
}
