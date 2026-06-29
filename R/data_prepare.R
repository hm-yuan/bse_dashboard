# Demo processed-data generator for downstream dashboard pages.
# The company spine comes from processed market-position detail when available.

demo_processed_table_names <- c(
  "dim_company",
  "dim_industry",
  "fact_market_period",
  "fact_financial_period",
  "fact_financing",
  "fact_fundraising_use",
  "fact_supervision",
  "fact_risk_tag",
  "fact_listing_pipeline"
)

# 用途：以 UTF-8 编码将数据框写入 CSV 文件
# 输入来源：函数输入参数
demo_write_csv_utf8 <- function(df, path) {
  utils::write.csv(df, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

# 用途：以 UTF-8 编码读取 CSV 文件
# 输入来源：data/processed/*.csv
demo_read_csv_utf8 <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8", check.names = FALSE)
}

# 用途：将输入安全转换为数值型向量
# 输入来源：函数输入参数
demo_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

# 用途：将输入安全转换为日期型向量
# 输入来源：函数输入参数
demo_date <- function(x) {
  out <- suppressWarnings(as.Date(x))
  out
}

# 用途：将数值限制在指定上下界之间
# 输入来源：函数输入参数
demo_clamp <- function(x, lower, upper) {
  pmin(pmax(x, lower), upper)
}

# 用途：用有效值的中位数填充数值向量中的无效值
# 输入来源：函数输入参数
demo_fill_numeric <- function(x, default = 1) {
  x <- demo_num(x)
  ok <- is.finite(x)
  fill <- if (any(ok)) stats::median(x[ok], na.rm = TRUE) else default
  x[!ok] <- fill
  x
}

# 用途：将向量循环截取到指定长度
# 输入来源：函数输入参数
demo_pick <- function(x, n) {
  rep(x, length.out = n)
}

# 用途：根据行业名称映射到七大产业分组
# 输入来源：函数输入参数（公司行业字段）
demo_industry_group <- function(industry) {
  x <- as.character(industry)
  ifelse(grepl("软件|信息|计算机|通信|电子|互联网|数据", x),
    "信息技术",
    ifelse(grepl("专用设备|通用设备|仪器|汽车|电气|铁路|航空|机械|金属制品", x),
      "先进制造",
      ifelse(grepl("医药|卫生|生物", x),
        "医药健康",
        ifelse(grepl("化学|橡胶|塑料|非金属|有色|黑色|材料|矿物", x),
          "基础材料",
          ifelse(grepl("环保|水|电力|燃气|生态|新能源", x),
            "绿色低碳",
            ifelse(grepl("食品|纺织|家具|文教|零售|商务|服务", x),
              "消费服务",
              "其他"
            )
          )
        )
      )
    )
  )
}

# 用途：将产业分组映射为战略赛道标签
# 输入来源：函数输入参数（产业分组）
demo_strategic_sector <- function(industry_group) {
  ifelse(industry_group == "信息技术", "数字经济",
    ifelse(industry_group == "先进制造", "高端制造",
      ifelse(industry_group == "医药健康", "生命健康",
        ifelse(industry_group == "基础材料", "新材料",
          ifelse(industry_group == "绿色低碳", "绿色低碳", "专精特新服务")
        )
      )
    )
  )
}

# 用途：当无 processed 公司明细时生成演示用公司清单
# 输入来源：R/sample_data.R
demo_fallback_company_detail <- function(n = 36) {
  if (!exists("load_demo_data", mode = "function") && file.exists("R/sample_data.R")) {
    source("R/sample_data.R", encoding = "UTF-8")
  }

  industries <- c(
    "计算机、通信和其他电子设备制造业",
    "软件和信息技术服务业",
    "通用设备制造业",
    "专用设备制造业",
    "医药制造业",
    "化学原料和化学制品制造业",
    "非金属矿物制品业",
    "电气机械和器材制造业",
    "仪器仪表制造业"
  )

  data.frame(
    company_code = sprintf("DEMO%03d.BJ", seq_len(n)),
    company_name = paste0("演示公司", sprintf("%03d", seq_len(n))),
    board = "北证",
    listing_date = format(seq(as.Date("2020-07-01"), by = "45 days", length.out = n), "%Y-%m-%d"),
    industry = demo_pick(industries, n),
    total_market_cap_yi = round(exp(seq(log(6), log(120), length.out = n)), 4),
    float_market_cap_yi = round(exp(seq(log(2.5), log(55), length.out = n)), 4),
    pe = round(demo_clamp(stats::rnorm(n, 36, 18), 8, 120), 4),
    is_current_year_new = FALSE,
    market_cap_rank = seq_len(n),
    is_top10_market_cap = seq_len(n) <= 10,
    stringsAsFactors = FALSE
  )
}

# 用途：加载公司基础清单作为演示数据生成的骨架
# 输入来源：data/processed/market_position_company_detail.csv、R/sample_data.R
demo_city_key <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[[:space:]　]+", "", x, perl = TRUE)
  x <- gsub("(市|地区|盟|自治州|特别行政区)$", "", x, perl = TRUE)
  x
}

demo_load_city_coordinates <- function(processed_dir) {
  path <- file.path(processed_dir, "city_coordinates.csv")
  if (!file.exists(path)) {
    return(data.frame())
  }

  df <- demo_read_csv_utf8(path)
  if ("city" %in% names(df) && !"city_key" %in% names(df)) {
    df$city_key <- demo_city_key(df$city)
  }
  if ("longitude" %in% names(df)) df$longitude <- suppressWarnings(as.numeric(df$longitude))
  if ("latitude" %in% names(df)) df$latitude <- suppressWarnings(as.numeric(df$latitude))
  df
}

demo_load_company_spine <- function(processed_dir) {
  path <- file.path(processed_dir, "market_position_company_detail.csv")
  if (file.exists(path)) {
    df <- demo_read_csv_utf8(path)
    source <- path
  } else {
    df <- demo_fallback_company_detail()
    source <- "R/sample_data.R fallback"
  }

  required <- c("company_code", "company_name", "board", "listing_date", "industry", "city", "total_market_cap_yi", "float_market_cap_yi", "pe")
  for (col in required) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }

  df <- df[!is.na(df$company_code) & nzchar(df$company_code), required, drop = FALSE]
  df <- df[!duplicated(df$company_code), , drop = FALSE]
  attr(df, "source") <- source
  df
}

# 用途：基于公司清单构建 dim_company 标准维表
# 输入来源：data/processed/market_position_company_detail.csv、R/sample_data.R
demo_build_dim_company <- function(company_detail, city_coordinates = data.frame()) {
  n <- nrow(company_detail)
  province_city <- data.frame(
    province = c("北京", "江苏", "浙江", "广东", "山东", "安徽", "四川", "湖北", "湖南", "河南", "河北", "上海", "福建", "重庆", "陕西"),
    city = c("北京", "南京", "杭州", "深圳", "济南", "合肥", "成都", "武汉", "长沙", "郑州", "石家庄", "上海", "厦门", "重庆", "西安"),
    stringsAsFactors = FALSE
  )

  industry_group <- demo_industry_group(company_detail$industry)
  strategic_sector <- demo_strategic_sector(industry_group)
  is_high_tech <- industry_group %in% c("信息技术", "先进制造", "医药健康", "绿色低碳")
  cap <- demo_fill_numeric(company_detail$total_market_cap_yi, 20)
  specialized_prob <- demo_clamp(0.38 + 0.22 * is_high_tech + 0.12 * (cap < stats::median(cap)), 0.15, 0.82)

  source_city <- trimws(as.character(company_detail$city))
  valid_city <- !is.na(source_city) & nzchar(source_city) & !grepl("^=", source_city)
  fallback_index <- ((seq_len(n) - 1L) %% nrow(province_city)) + 1L

  out <- data.frame(
    company_code = company_detail$company_code,
    company_name = company_detail$company_name,
    board = company_detail$board,
    listing_date = company_detail$listing_date,
    province = province_city$province[fallback_index],
    city = ifelse(valid_city, source_city, province_city$city[fallback_index]),
    industry = company_detail$industry,
    strategic_sector = strategic_sector,
    is_bse = TRUE,
    is_high_tech = is_high_tech,
    is_specialized_new = stats::runif(n) < specialized_prob,
    stringsAsFactors = FALSE
  )

  out$longitude <- NA_real_
  out$latitude <- NA_real_
  if (is.data.frame(city_coordinates) && nrow(city_coordinates) > 0L &&
      all(c("city_key", "longitude", "latitude") %in% names(city_coordinates))) {
    out$city_key <- demo_city_key(out$city)
    city_coordinates <- city_coordinates[is.finite(city_coordinates$longitude) & is.finite(city_coordinates$latitude), , drop = FALSE]
    matched_index <- match(out$city_key, city_coordinates$city_key)
    has_match <- !is.na(matched_index)
    out$longitude[has_match] <- city_coordinates$longitude[matched_index[has_match]]
    out$latitude[has_match] <- city_coordinates$latitude[matched_index[has_match]]
    if ("province" %in% names(city_coordinates)) {
      out$province[has_match] <- city_coordinates$province[matched_index[has_match]]
    }
    out$city_key <- NULL
  }

  out
}

# 用途：基于 dim_company 构建行业维表
# 输入来源：函数输入参数（由 demo_build_dim_company 生成）
demo_build_dim_industry <- function(dim_company) {
  industry <- unique(dim_company$industry)
  industry_group <- demo_industry_group(industry)
  strategic_sector <- demo_strategic_sector(industry_group)
  order_group <- match(industry_group, c("信息技术", "先进制造", "医药健康", "基础材料", "绿色低碳", "消费服务", "其他"))
  data.frame(
    industry = industry,
    industry_group = industry_group,
    strategic_sector = strategic_sector,
    display_order = rank(order_group * 1000 + seq_along(industry), ties.method = "first"),
    stringsAsFactors = FALSE
  )
}

# 用途：基于公司维表和公司明细生成演示用公司指标
# 输入来源：函数输入参数（由 demo_build_dim_company 生成）
demo_company_metrics <- function(dim_company, company_detail) {
  n <- nrow(dim_company)
  cap <- demo_fill_numeric(company_detail$total_market_cap_yi, 20)
  float_cap <- demo_fill_numeric(company_detail$float_market_cap_yi, stats::median(cap) * 0.4)
  pe <- demo_fill_numeric(company_detail$pe, 35)
  pe[pe <= 0 | pe > 300] <- stats::median(pe[pe > 0 & pe <= 300], na.rm = TRUE)
  pe <- demo_fill_numeric(pe, 35)
  cap_score <- rank(cap, ties.method = "average") / n
  high_growth <- dim_company$is_high_tech | dim_company$is_specialized_new

  company_growth <- demo_clamp(
    stats::rnorm(n, 0.065, 0.07) + 0.035 * high_growth + 0.02 * dim_company$is_specialized_new,
    -0.12,
    0.32
  )
  profit_quality <- stats::rnorm(n)
  sales_multiple <- demo_clamp(
    2.2 + 1.0 * dim_company$is_high_tech + 0.45 * dim_company$is_specialized_new + stats::rnorm(n, 0, 0.75),
    1.0,
    7.5
  )
  latest_revenue_yi <- demo_clamp(cap / sales_multiple * stats::runif(n, 0.82, 1.25), 0.15, Inf)
  base_margin <- demo_clamp(
    0.06 + 0.045 * profit_quality + 0.018 * dim_company$is_high_tech - 0.22 * pmax(company_growth - 0.18, 0),
    -0.20,
    0.24
  )
  weak_idx <- stats::runif(n) < 0.11
  base_margin[weak_idx] <- base_margin[weak_idx] - stats::runif(sum(weak_idx), 0.08, 0.22)
  rd_base <- demo_clamp(
    0.028 + 0.045 * dim_company$is_high_tech + 0.025 * dim_company$is_specialized_new + stats::rnorm(n, 0, 0.012),
    0.004,
    0.19
  )

  data.frame(
    company_code = dim_company$company_code,
    total_market_cap_yi = cap,
    float_market_cap_yi = float_cap,
    pe = pe,
    cap_score = cap_score,
    company_growth = company_growth,
    latest_revenue_yi = latest_revenue_yi,
    base_margin = base_margin,
    sales_multiple = sales_multiple,
    rd_base = rd_base,
    stringsAsFactors = FALSE
  )
}

# 用途：基于公司指标构造演示用财务周期事实表
# 输入来源：函数输入参数（由 demo_build_dim_company、demo_company_metrics 生成）
demo_build_financial_period <- function(dim_company, metrics, periods) {
  rows <- vector("list", nrow(dim_company) * length(periods))
  k <- 1L
  n_periods <- length(periods)

  for (i in seq_len(nrow(dim_company))) {
    for (p in seq_along(periods)) {
      year_gap <- p - n_periods
      revenue <- metrics$latest_revenue_yi[[i]] * exp(year_gap * metrics$company_growth[[i]]) * stats::runif(1, 0.93, 1.07)
      growth_pressure <- pmax(metrics$company_growth[[i]] - 0.18, 0) * stats::runif(1, 0.25, 0.65)
      net_margin <- demo_clamp(metrics$base_margin[[i]] - growth_pressure + stats::rnorm(1, 0, 0.024), -0.28, 0.28)
      net_profit <- revenue * net_margin
      deduct_net_profit <- net_profit * stats::runif(1, 0.76, 1.03) - abs(revenue) * stats::runif(1, 0, 0.012)
      roe <- demo_clamp(net_margin * metrics$sales_multiple[[i]] / 2.4 + stats::rnorm(1, 0, 0.025), -0.36, 0.32)
      gross_margin <- demo_clamp(0.22 + 0.08 * dim_company$is_high_tech[[i]] + stats::rnorm(1, 0, 0.075), 0.07, 0.66)
      operating_cashflow <- net_profit * stats::runif(1, 0.55, 1.45) + revenue * stats::rnorm(1, 0, 0.024)
      rd_ratio <- demo_clamp(metrics$rd_base[[i]] * stats::runif(1, 0.82, 1.22), 0.003, 0.22)

      rows[[k]] <- data.frame(
        period = format(periods[[p]], "%Y-%m"),
        company_code = dim_company$company_code[[i]],
        revenue_yi = round(revenue, 4),
        net_profit_yi = round(net_profit, 4),
        deduct_net_profit_yi = round(deduct_net_profit, 4),
        roe = round(roe, 6),
        gross_margin = round(gross_margin, 6),
        net_margin = round(net_margin, 6),
        operating_cashflow_yi = round(operating_cashflow, 4),
        r_and_d_expense_yi = round(revenue * rd_ratio, 4),
        r_and_d_ratio = round(rd_ratio, 6),
        stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
  }

  do.call(rbind, rows)
}

# 用途：基于财务周期数据构造演示用市场周期事实表
# 输入来源：函数输入参数（由 demo_build_financial_period 等生成）
demo_build_market_period <- function(dim_company, metrics, financial_period, periods) {
  rows <- vector("list", nrow(dim_company) * length(periods))
  k <- 1L
  n_periods <- length(periods)
  period_sentiment <- c(0.78, 0.88, 0.96, 1.04, 1.00)

  for (i in seq_len(nrow(dim_company))) {
    float_ratio <- demo_clamp(metrics$float_market_cap_yi[[i]] / metrics$total_market_cap_yi[[i]], 0.22, 0.92)
    for (p in seq_along(periods)) {
      year_gap <- p - n_periods
      period_key <- format(periods[[p]], "%Y-%m")
      fin <- financial_period[financial_period$company_code == dim_company$company_code[[i]] & financial_period$period == period_key, , drop = FALSE]
      roe <- if (nrow(fin) > 0L) fin$roe[[1]] else 0.08
      total_cap <- metrics$total_market_cap_yi[[i]] * period_sentiment[[p]] *
        exp(year_gap * (0.03 + metrics$company_growth[[i]] * 0.35)) * stats::runif(1, 0.88, 1.12)
      float_cap <- total_cap * float_ratio * stats::runif(1, 0.94, 1.06)
      turnover_rate <- demo_clamp(
        0.002 + 0.020 * metrics$cap_score[[i]] + 0.003 * dim_company$is_high_tech[[i]] + stats::rnorm(1, 0, 0.003),
        0.0004,
        0.075
      )
      pe <- demo_clamp(metrics$pe[[i]] * stats::runif(1, 0.75, 1.25), 4, 180)
      if (nrow(fin) > 0L && fin$net_profit_yi[[1]] <= 0) pe <- demo_clamp(pe * stats::runif(1, 1.25, 1.9), 30, 220)
      pb <- demo_clamp(1.0 + roe * 8 + 0.25 * dim_company$is_high_tech[[i]] + stats::rnorm(1, 0, 0.35), 0.35, 9)

      rows[[k]] <- data.frame(
        period = period_key,
        company_code = dim_company$company_code[[i]],
        total_market_cap_yi = round(total_cap, 4),
        float_market_cap_yi = round(float_cap, 4),
        turnover_amount_yi = round(float_cap * turnover_rate * 22, 4),
        turnover_rate = round(turnover_rate, 6),
        pe = round(pe, 4),
        pb = round(pb, 4),
        stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
  }

  do.call(rbind, rows)
}

# 用途：基于公司指标构造演示用融资事件表
# 输入来源：函数输入参数（由 demo_build_dim_company、demo_company_metrics 生成）
demo_build_financing <- function(dim_company, metrics) {
  rows <- list()
  k <- 1L
  today <- Sys.Date()
  use_types <- c("研发中心建设", "智能制造基地", "补充流动资金", "市场渠道建设", "产业化项目")

  for (i in seq_len(nrow(dim_company))) {
    listing_date <- demo_date(dim_company$listing_date[[i]])
    if (is.na(listing_date)) {
      listing_date <- as.Date("2021-01-01") + sample(0:1500, 1)
    }
    listing_date <- min(listing_date, today)
    amount <- demo_clamp(metrics$total_market_cap_yi[[i]] * stats::runif(1, 0.035, 0.12), 0.25, 24)

    rows[[k]] <- data.frame(
      event_date = format(listing_date, "%Y-%m-%d"),
      company_code = dim_company$company_code[[i]],
      financing_type = "IPO",
      amount_yi = round(amount, 4),
      use_type = sample(use_types, 1),
      status = "已完成",
      stringsAsFactors = FALSE
    )
    k <- k + 1L
  }

  extra_n <- max(1L, round(nrow(dim_company) * 0.18))
  extra_idx <- sample(seq_len(nrow(dim_company)), extra_n)
  for (i in extra_idx) {
    listing_date <- demo_date(dim_company$listing_date[[i]])
    if (is.na(listing_date)) listing_date <- as.Date("2021-01-01")
    event_date <- min(Sys.Date(), listing_date + sample(260:1500, 1))
    rows[[k]] <- data.frame(
      event_date = format(event_date, "%Y-%m-%d"),
      company_code = dim_company$company_code[[i]],
      financing_type = sample(c("定向增发", "可转债", "公开发行"), 1, prob = c(0.56, 0.26, 0.18)),
      amount_yi = round(demo_clamp(metrics$total_market_cap_yi[[i]] * stats::runif(1, 0.025, 0.09), 0.2, 18), 4),
      use_type = sample(use_types, 1),
      status = sample(c("已完成", "推进中"), 1, prob = c(0.82, 0.18)),
      stringsAsFactors = FALSE
    )
    k <- k + 1L
  }

  do.call(rbind, rows)
}

# 用途：基于融资事件表构造演示用募资使用表
# 输入来源：函数输入参数（由 demo_build_financing 生成）
demo_build_fundraising_use <- function(dim_company, financing) {
  raised <- stats::aggregate(amount_yi ~ company_code, financing, sum)
  rows <- vector("list", nrow(raised))

  for (i in seq_len(nrow(raised))) {
    company <- dim_company[dim_company$company_code == raised$company_code[[i]], , drop = FALSE]
    listing_date <- demo_date(company$listing_date[[1]])
    years_since_listing <- if (is.na(listing_date)) 2 else as.numeric(Sys.Date() - listing_date) / 365
    progress <- demo_clamp(0.22 + years_since_listing * 0.16 + stats::runif(1, -0.12, 0.18), 0.06, 0.98)
    is_delayed <- progress < 0.45 || stats::runif(1) < 0.08
    is_changed <- (progress < 0.58 && stats::runif(1) < 0.22) || stats::runif(1) < 0.05
    project_status <- if (progress >= 0.82) {
      "基本完成"
    } else if (is_delayed) {
      "延期推进"
    } else {
      "正常推进"
    }

    rows[[i]] <- data.frame(
      period = "2026-06",
      company_code = raised$company_code[[i]],
      raised_amount_yi = round(raised$amount_yi[[i]], 4),
      used_amount_yi = round(raised$amount_yi[[i]] * progress, 4),
      use_progress = round(progress, 6),
      project_status = project_status,
      is_delayed = is_delayed,
      is_changed = is_changed,
      cash_management_balance_yi = round(raised$amount_yi[[i]] * demo_clamp(1 - progress, 0, 1) * stats::runif(1, 0.25, 0.85), 4),
      benefit_realization_rate = round(demo_clamp(progress + stats::rnorm(1, 0, 0.16), 0, 1.25), 6),
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

# 用途：提取周期表中最新的两个周期数据进行合并
# 输入来源：函数输入参数（由 demo_build_financial_period 生成）
demo_latest_pair <- function(df, latest_period, previous_period) {
  latest <- df[df$period == latest_period, , drop = FALSE]
  previous <- df[df$period == previous_period, , drop = FALSE]
  names(previous) <- paste0(names(previous), "_previous")
  merge(latest, previous, by.x = "company_code", by.y = "company_code_previous", all.x = TRUE)
}

# 用途：基于市场、财务和募资数据构造演示用风险标签表
# 输入来源：函数输入参数（由 demo_build_market_period、demo_build_financial_period、demo_build_fundraising_use 生成）
demo_build_risk_tag <- function(dim_company, market_period, financial_period, fundraising_use) {
  latest_market <- market_period[market_period$period == "2026-06", , drop = FALSE]
  latest_fin <- demo_latest_pair(financial_period, "2026-06", "2025-12")
  latest <- merge(latest_market, latest_fin, by = "company_code", all.x = TRUE)
  latest <- merge(latest, fundraising_use, by = "company_code", all.x = TRUE)

  low_liq_cut <- stats::quantile(latest$turnover_rate, 0.25, na.rm = TRUE)
  very_low_liq_cut <- stats::quantile(latest$turnover_rate, 0.10, na.rm = TRUE)
  revenue_growth <- (latest$revenue_yi / latest$revenue_yi_previous) - 1

  rows <- list()
  k <- 1L
  add_risk <- function(company_code, risk_type, risk_level, risk_reason) {
    rows[[k]] <<- data.frame(
      period = "2026-06",
      company_code = company_code,
      risk_type = risk_type,
      risk_level = risk_level,
      risk_reason = risk_reason,
      stringsAsFactors = FALSE
    )
    k <<- k + 1L
  }

  for (i in seq_len(nrow(latest))) {
    if (!is.na(latest$turnover_rate[[i]]) && latest$turnover_rate[[i]] <= low_liq_cut) {
      add_risk(
        latest$company_code[[i]],
        "低流动性风险",
        if (latest$turnover_rate[[i]] <= very_low_liq_cut) "高" else "中",
        "换手率处于市场后四分位，交易活跃度偏低"
      )
    }
    if (!is.na(latest$pe[[i]]) && !is.na(revenue_growth[[i]]) && latest$pe[[i]] >= 80 && revenue_growth[[i]] < 0.06) {
      add_risk(
        latest$company_code[[i]],
        "估值风险",
        if (latest$pe[[i]] >= 120) "高" else "中",
        "估值较高且营收增长偏弱"
      )
    }
    if ((!is.na(latest$net_profit_yi[[i]]) && latest$net_profit_yi[[i]] < 0) ||
        (!is.na(latest$deduct_net_profit_yi[[i]]) && latest$deduct_net_profit_yi[[i]] < 0) ||
        (!is.na(latest$operating_cashflow_yi[[i]]) && latest$operating_cashflow_yi[[i]] < 0)) {
      add_risk(
        latest$company_code[[i]],
        "财务风险",
        if (!is.na(latest$net_profit_yi_previous[[i]]) && latest$net_profit_yi[[i]] < 0 && latest$net_profit_yi_previous[[i]] < 0) "高" else "中",
        "盈利或经营现金流承压"
      )
    }
    if ((!is.na(latest$is_delayed[[i]]) && latest$is_delayed[[i]]) ||
        (!is.na(latest$is_changed[[i]]) && latest$is_changed[[i]]) ||
        (!is.na(latest$use_progress[[i]]) && latest$use_progress[[i]] < 0.45)) {
      add_risk(
        latest$company_code[[i]],
        "募资风险",
        if (!is.na(latest$is_delayed[[i]]) && latest$is_delayed[[i]] && !is.na(latest$is_changed[[i]]) && latest$is_changed[[i]]) "高" else "中",
        "募集资金使用进度偏低或募投项目发生延期变更"
      )
    }
  }

  if (length(rows) == 0L) {
    return(data.frame(
      period = character(),
      company_code = character(),
      risk_type = character(),
      risk_level = character(),
      risk_reason = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, rows)
}

# 用途：基于风险标签构造演示用监管事件表
# 输入来源：函数输入参数（由 demo_build_risk_tag、demo_build_dim_company 生成）
demo_build_supervision <- function(risk_tag, dim_company) {
  risk_companies <- unique(risk_tag$company_code)
  sample_n <- min(length(risk_companies), max(12L, round(nrow(dim_company) * 0.08)))
  selected <- if (sample_n > 0L) sample(risk_companies, sample_n) else sample(dim_company$company_code, min(8L, nrow(dim_company)))
  event_types <- c("问询函", "监管关注", "自律监管措施", "纪律处分")
  rows <- vector("list", length(selected))

  for (i in seq_along(selected)) {
    company_risk <- risk_tag[risk_tag$company_code == selected[[i]], , drop = FALSE]
    high_risk <- any(company_risk$risk_level == "高")
    severity <- if (high_risk) sample(c("中", "高"), 1, prob = c(0.45, 0.55)) else sample(c("低", "中"), 1, prob = c(0.55, 0.45))
    rows[[i]] <- data.frame(
      event_date = format(as.Date("2025-01-01") + sample(0:535, 1), "%Y-%m-%d"),
      company_code = selected[[i]],
      event_type = sample(event_types, 1, prob = c(0.46, 0.28, 0.20, 0.06)),
      severity = severity,
      description = paste0("围绕", if (nrow(company_risk) > 0L) company_risk$risk_type[[1]] else "信息披露事项", "进行监管跟踪"),
      status = sample(c("已回复", "整改中", "持续关注"), 1, prob = c(0.62, 0.26, 0.12)),
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

# 用途：根据监管事件为风险标签补充合规风险记录
# 输入来源：函数输入参数（由 demo_build_risk_tag、demo_build_supervision 生成）
demo_add_compliance_risk <- function(risk_tag, supervision) {
  if (nrow(supervision) == 0L) return(risk_tag)
  severe <- supervision[supervision$severity %in% c("中", "高"), , drop = FALSE]
  if (nrow(severe) == 0L) return(risk_tag)

  compliance <- data.frame(
    period = "2026-06",
    company_code = severe$company_code,
    risk_type = "合规风险",
    risk_level = ifelse(severe$severity == "高", "高", "中"),
    risk_reason = paste0("存在", severe$event_type, "事项，处置状态为", severe$status),
    stringsAsFactors = FALSE
  )

  unique(rbind(risk_tag, compliance))
}

# 用途：基于公司和行业维表构造演示用上市pipeline表
# 输入来源：函数输入参数（由 demo_build_dim_company、demo_build_dim_industry 生成）
demo_build_listing_pipeline <- function(dim_company, dim_industry, n = 60) {
  sponsors <- c("中信证券", "中信建投", "国泰君安", "申万宏源", "招商证券", "海通证券", "开源证券", "东吴证券")
  stages <- c("辅导备案", "已受理", "问询回复", "上市委审议", "提交注册")
  province <- sample(dim_company$province, n, replace = TRUE)
  industry <- sample(dim_industry$industry, n, replace = TRUE)

  data.frame(
    period = "2026-06",
    company_code = sprintf("PIPE2026%03d", seq_len(n)),
    pipeline_stage = sample(stages, n, replace = TRUE, prob = c(0.30, 0.25, 0.24, 0.14, 0.07)),
    industry = industry,
    province = province,
    sponsor = sample(sponsors, n, replace = TRUE),
    days_in_review = sample(35:420, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# 用途：为演示生成的各表构造数据质量日志行
# 输入来源：函数输入参数（generate_demo_processed_data 内部传入）
demo_quality_rows <- function(table_list, source_note, fallback_used) {
  rows <- lapply(names(table_list), function(name) {
    data.frame(
      check_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      source_file = "R/data_prepare.R",
      check_item = paste0("demo_processed_", name),
      status = "ok",
      message = paste0(
        "生成 ", nrow(table_list[[name]]), " 行；公司基础清单来源：", source_note,
        "；除 market_position_company_detail 继承字段外，其余新增字段为演示生成"
      ),
      stringsAsFactors = FALSE
    )
  })

  if (fallback_used) {
    rows[[length(rows) + 1L]] <- data.frame(
      check_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      source_file = "R/data_prepare.R",
      check_item = "demo_company_spine_fallback",
      status = "warning",
      message = "market_position_company_detail.csv 不存在，已使用 R/sample_data.R 构造最小公司清单",
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

# 用途：生成演示用 processed 标准表并写入 CSV 文件
# 输入来源：data/processed/market_position_company_detail.csv、R/sample_data.R
generate_demo_processed_data <- function(processed_dir = "data/processed", seed = 20260619) {
  set.seed(seed)
  if (!dir.exists(processed_dir)) {
    dir.create(processed_dir, recursive = TRUE)
  }

  company_detail <- demo_load_company_spine(processed_dir)
  city_coordinates <- demo_load_city_coordinates(processed_dir)
  source_note <- attr(company_detail, "source", exact = TRUE)
  fallback_used <- identical(source_note, "R/sample_data.R fallback")

  dim_company <- demo_build_dim_company(company_detail, city_coordinates)
  dim_industry <- demo_build_dim_industry(dim_company)
  metrics <- demo_company_metrics(dim_company, company_detail)
  periods <- as.Date(c("2022-12-31", "2023-12-31", "2024-12-31", "2025-12-31", "2026-06-30"))

  fact_financial_period <- demo_build_financial_period(dim_company, metrics, periods)
  fact_market_period <- demo_build_market_period(dim_company, metrics, fact_financial_period, periods)
  fact_financing <- demo_build_financing(dim_company, metrics)
  fact_fundraising_use <- demo_build_fundraising_use(dim_company, fact_financing)
  fact_risk_tag <- demo_build_risk_tag(dim_company, fact_market_period, fact_financial_period, fact_fundraising_use)
  fact_supervision <- demo_build_supervision(fact_risk_tag, dim_company)
  fact_risk_tag <- demo_add_compliance_risk(fact_risk_tag, fact_supervision)
  fact_listing_pipeline <- demo_build_listing_pipeline(dim_company, dim_industry)

  tables <- list(
    dim_company = dim_company,
    dim_industry = dim_industry,
    fact_market_period = fact_market_period,
    fact_financial_period = fact_financial_period,
    fact_financing = fact_financing,
    fact_fundraising_use = fact_fundraising_use,
    fact_supervision = fact_supervision,
    fact_risk_tag = fact_risk_tag,
    fact_listing_pipeline = fact_listing_pipeline
  )

  files <- vapply(names(tables), function(name) {
    path <- file.path(processed_dir, paste0(name, ".csv"))
    demo_write_csv_utf8(tables[[name]], path)
    path
  }, character(1))

  log_path <- file.path(processed_dir, "data_quality_log.csv")
  old_log <- if (file.exists(log_path)) demo_read_csv_utf8(log_path) else data.frame()
  if (nrow(old_log) > 0L && "source_file" %in% names(old_log)) {
    old_log <- old_log[old_log$source_file != "R/data_prepare.R", , drop = FALSE]
  }
  new_log <- demo_quality_rows(tables, source_note, fallback_used)
  if (nrow(old_log) > 0L) {
    new_log <- rbind(old_log, new_log)
  }
  demo_write_csv_utf8(new_log, log_path)

  invisible(list(
    files = c(files, data_quality_log = log_path),
    tables = tables,
    quality_log = new_log
  ))
}
