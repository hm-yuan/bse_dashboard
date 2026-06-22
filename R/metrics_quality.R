# Metrics for the market-quality portrait.
# Input: the list returned by load_dashboard_data().
# Output: KPI data frames, chart-ready data frames, or insight character vectors.

# 用途：将公司、市场、财务、募资、风险等最新数据按 company_code 合并，供质量画像复用
# 输入来源：`dashboard_data$dim_company`、`dashboard_data$fact_market_period`、`dashboard_data$fact_financial_period`、`dashboard_data$fact_fundraising_use`、`dashboard_data$fact_risk_tag`
quality_latest_joined_data <- function(data) {
  dim_company <- metric_table(data, "dim_company")
  market <- metric_table(data, "fact_market_period")
  financial <- metric_table(data, "fact_financial_period")
  fundraising <- metric_table(data, "fact_fundraising_use")
  risk <- metric_table(data, "fact_risk_tag")

  pair <- metric_latest_financial_pair(financial)
  latest_market_period <- metric_latest_period(market)
  latest_market <- if (!is.na(latest_market_period)) market[market$period == latest_market_period, , drop = FALSE] else data.frame()

  if (!metric_has_cols(dim_company, c("company_code", "company_name", "industry"))) {
    return(data.frame())
  }

  out <- dim_company[, intersect(names(dim_company), c("company_code", "company_name", "industry", "province", "strategic_sector")), drop = FALSE]
  if (metric_has_cols(latest_market, "company_code")) {
    out <- merge(out, latest_market, by = "company_code", all.x = TRUE)
  }
  if (nrow(pair$latest) > 0L) {
    previous <- pair$previous[, intersect(names(pair$previous), c("company_code", "revenue_yi", "net_profit_yi")), drop = FALSE]
    names(previous)[names(previous) == "revenue_yi"] <- "previous_revenue_yi"
    names(previous)[names(previous) == "net_profit_yi"] <- "previous_net_profit_yi"
    fin <- merge(pair$latest, previous, by = "company_code", all.x = TRUE)
    out <- merge(out, fin, by = "company_code", all.x = TRUE)
  }
  if (metric_has_cols(fundraising, "company_code")) {
    out <- merge(out, fundraising, by = "company_code", all.x = TRUE)
  }
  if (metric_has_cols(risk, c("company_code", "risk_level"))) {
    risk_rank <- data.frame(risk_level = c("低", "中", "高"), risk_rank = c(1, 2, 3), stringsAsFactors = FALSE)
    risk2 <- merge(risk, risk_rank, by = "risk_level", all.x = TRUE)
    max_risk <- stats::aggregate(risk_rank ~ company_code, risk2, max, na.rm = TRUE)
    max_risk$risk_level <- c("低", "中", "高")[max_risk$risk_rank]
    out <- merge(out, max_risk[, c("company_code", "risk_level"), drop = FALSE], by = "company_code", all.x = TRUE)
  }
  out$risk_level[is.na(out$risk_level)] <- "低"
  out
}

# 用途：计算市场质量画像的核心 KPI 卡片数据（低流动性、高估值低增长、连续亏损、监管措施、募投异常、退市风险）
# 输入来源：`quality_latest_joined_data()` 结果、`dashboard_data$fact_risk_tag`、`dashboard_data$fact_supervision`、`dashboard_data$fact_fundraising_use`、`config/thresholds.yml`
calc_quality_kpis <- function(data) {
  joined <- quality_latest_joined_data(data)
  risk <- metric_table(data, "fact_risk_tag")
  supervision <- metric_table(data, "fact_supervision")
  fundraising <- metric_table(data, "fact_fundraising_use")
  thresholds <- metric_thresholds()

  company_count <- if (metric_has_cols(joined, "company_code")) length(unique(joined$company_code)) else 0L
  if (company_count == 0L) {
    fallback <- data$market_quality$kpis
    if (is.data.frame(fallback)) return(fallback)
    return(metric_make_kpis(c("低流动性公司占比", "高估值低增长公司数量", "连续亏损公司数量", "监管措施数量", "募投异常公司数量", "退市风险公司数量"), rep("--", 6), c("%", "家", "家", "项", "家", "家")))
  }

  low_liq_companies <- if (metric_has_cols(risk, c("company_code", "risk_type"))) {
    unique(risk$company_code[risk$risk_type == "低流动性风险"])
  } else {
    joined$company_code[metric_num(joined$turnover_rate) <= thresholds$liquidity$low_turnover_rate]
  }

  revenue_growth <- (metric_num(joined$revenue_yi) / metric_num(joined$previous_revenue_yi)) - 1
  high_valuation_low_growth <- joined$company_code[metric_num(joined$pe) >= thresholds$valuation$high_pe & revenue_growth <= thresholds$valuation$low_growth]
  continuous_loss <- joined$company_code[metric_num(joined$net_profit_yi) < 0 & metric_num(joined$previous_net_profit_yi) < 0]
  fundraising_abnormal <- if (metric_has_cols(fundraising, c("company_code", "use_progress", "is_delayed", "is_changed"))) {
    unique(fundraising$company_code[metric_num(fundraising$use_progress) < thresholds$fundraising$low_use_progress | fundraising$is_delayed | fundraising$is_changed])
  } else {
    character()
  }
  delisting <- if (metric_has_cols(risk, c("company_code", "risk_type"))) unique(risk$company_code[risk$risk_type == "退市风险"]) else character()

  metric_make_kpis(
    labels = c("低流动性公司占比", "高估值低增长公司数量", "连续亏损公司数量", "监管措施数量", "募投异常公司数量", "退市风险公司数量"),
    values = c(
      metric_format_percent(length(unique(low_liq_companies)) / company_count, 1),
      metric_format_count(length(unique(high_valuation_low_growth))),
      metric_format_count(length(unique(continuous_loss))),
      metric_format_count(if (is.data.frame(supervision)) nrow(supervision) else 0L),
      metric_format_count(length(unique(fundraising_abnormal))),
      metric_format_count(length(unique(delisting)))
    ),
    units = c("%", "家", "家", "项", "家", "家"),
    changes = rep("最新期", 6),
    statuses = c("warning", "warning", "warning", "warning", "warning", "neutral")
  )
}

# 用途：基于 ROE、净利率、经营现金流、研发费率计算公司质量评分矩阵
# 输入来源：`quality_latest_joined_data()` 结果
calc_quality_status_matrix <- function(data) {
  joined <- quality_latest_joined_data(data)
  if (!metric_has_cols(joined, c("company_code", "company_name", "industry"))) {
    return(metric_empty(c("company_code", "company_name", "industry", "liquidity_score", "quality_score", "total_market_cap_yi", "risk_level")))
  }

  quality_score <- metric_num(joined$roe) * 0.45 + metric_num(joined$net_margin) * 0.30 +
    ifelse(metric_num(joined$operating_cashflow_yi) > 0, 0.08, -0.08) +
    pmin(pmax(metric_num(joined$r_and_d_ratio), 0), 0.2) * 0.45

  data.frame(
    company_code = joined$company_code,
    company_name = joined$company_name,
    industry = joined$industry,
    liquidity_score = round(metric_num(joined$turnover_rate), 6),
    quality_score = round(quality_score, 6),
    total_market_cap_yi = round(metric_num(joined$total_market_cap_yi), 4),
    risk_level = joined$risk_level,
    stringsAsFactors = FALSE
  )
}

# 用途：按行业和风险类型统计风险公司数量及高风险公司数量，用于热力图
# 输入来源：`dashboard_data$fact_risk_tag`、`dashboard_data$dim_company`
calc_risk_industry_heatmap <- function(data) {
  risk <- metric_table(data, "fact_risk_tag")
  dim_company <- metric_table(data, "dim_company")
  if (!metric_has_cols(risk, c("company_code", "risk_type")) || !metric_has_cols(dim_company, c("company_code", "industry"))) {
    return(metric_empty(c("industry", "risk_type", "risk_count", "high_risk_count")))
  }
  merged <- merge(risk, dim_company[, c("company_code", "industry", "strategic_sector"), drop = FALSE], by = "company_code", all.x = TRUE)
  keys <- unique(merged[, c("industry", "risk_type"), drop = FALSE])
  out <- do.call(rbind, lapply(seq_len(nrow(keys)), function(i) {
    part <- merged[merged$industry == keys$industry[[i]] & merged$risk_type == keys$risk_type[[i]], , drop = FALSE]
    data.frame(
      industry = keys$industry[[i]],
      risk_type = keys$risk_type[[i]],
      risk_count = length(unique(part$company_code)),
      high_risk_count = length(unique(part$company_code[part$risk_level == "高"])),
      stringsAsFactors = FALSE
    )
  }))
  out[order(out$risk_count, decreasing = TRUE), , drop = FALSE]
}

# 用途：生成市场质量画像的文字洞察
# 输入来源：`calc_quality_kpis()`、`calc_risk_industry_heatmap()` 结果
calc_quality_insights <- function(data) {
  kpis <- calc_quality_kpis(data)
  heatmap <- calc_risk_industry_heatmap(data)
  top_risk <- if (nrow(heatmap) > 0L) heatmap$risk_type[[1L]] else "主要风险"
  top_industry <- if (nrow(heatmap) > 0L) heatmap$industry[[1L]] else "重点行业"
  c(
    paste0("低流动性公司占比为 ", kpis$value[kpis$label == "低流动性公司占比"][[1]], "%，仍是市场质量监测的基础维度。"),
    paste0(top_risk, " 在 ", top_industry, " 中相对集中，应结合公司明细继续跟踪。"),
    paste0("监管措施数量为 ", kpis$value[kpis$label == "监管措施数量"][[1]], " 项，募投异常公司数量为 ", kpis$value[kpis$label == "募投异常公司数量"][[1]], " 家。")
  )
}

# 用途：生成市场质量画像的公司风险明细表
# 输入来源：`quality_latest_joined_data()` 结果、`dashboard_data$fact_risk_tag`、`dashboard_data$fact_supervision`
calc_quality_company_detail <- function(data) {
  joined <- quality_latest_joined_data(data)
  risk <- metric_table(data, "fact_risk_tag")
  supervision <- metric_table(data, "fact_supervision")
  if (!metric_has_cols(joined, c("company_code", "company_name", "industry"))) {
    return(metric_empty(c("company_code", "company_name", "industry", "risk_type", "risk_level", "risk_reason", "status")))
  }
  if (!metric_has_cols(risk, c("company_code", "risk_type", "risk_level", "risk_reason"))) {
    return(joined[, intersect(names(joined), c("company_code", "company_name", "industry", "risk_level")), drop = FALSE])
  }
  out <- merge(risk, joined[, intersect(names(joined), c("company_code", "company_name", "industry", "total_market_cap_yi", "turnover_rate", "roe", "net_margin")), drop = FALSE], by = "company_code", all.x = TRUE)
  if (metric_has_cols(supervision, c("company_code", "status"))) {
    status_summary <- stats::aggregate(status ~ company_code, supervision, function(x) paste(sort(unique(x)), collapse = "、"))
    out <- merge(out, status_summary, by = "company_code", all.x = TRUE)
  } else {
    out$status <- NA_character_
  }
  out$status[is.na(out$status) | !nzchar(out$status)] <- "持续跟踪"
  risk_order <- match(out$risk_level, c("高", "中", "低"))
  detail_columns <- c("company_code", "company_name", "industry", "risk_type", "risk_level", "risk_reason", "status")
  out <- out[, intersect(detail_columns, names(out)), drop = FALSE]
  out[order(risk_order, out$risk_type, na.last = TRUE), , drop = FALSE]
}
