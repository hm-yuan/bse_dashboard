# Metrics for the company-profile portrait.
# Input: the list returned by load_dashboard_data().
# Output: KPI data frames, chart-ready data frames, or insight character vectors.

# 用途：计算公司画像的核心 KPI 卡片数据（营收、净利润、ROE、研发费率等）
# 输入来源：`dashboard_data$fact_financial_period`、`dashboard_data$fact_fundraising_use`
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

# 用途：计算各行业在公司数量、市值、营收、利润、研发等方面的贡献分布
# 输入来源：`dashboard_data$dim_company`、`dashboard_data$fact_market_period`、`dashboard_data$fact_financial_period`
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

# 用途：按营收增长与 ROE 中位数将公司划分为四象限质量标签
# 输入来源：`dashboard_data$dim_company`、`dashboard_data$fact_market_period`、`dashboard_data$fact_financial_period`
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

# 用途：提供中国省级地图键、中心点及常见城市中心点，用于公司地域分布图。
# 输入来源：函数内部维护的行政区与城市地理参考表；坐标为城市中心近似坐标。
company_geo_reference <- function() {
  provinces <- data.frame(
    province = c("北京", "天津", "河北", "山西", "内蒙古", "辽宁", "吉林", "黑龙江", "上海", "江苏", "浙江", "安徽", "福建", "江西", "山东", "河南", "湖北", "湖南", "广东", "广西", "海南", "重庆", "四川", "贵州", "云南", "西藏", "陕西", "甘肃", "青海", "宁夏", "新疆", "台湾", "香港", "澳门"),
    hc_key = c("cn-bj", "cn-tj", "cn-he", "cn-sx", "cn-nm", "cn-ln", "cn-jl", "cn-hl", "cn-sh", "cn-js", "cn-zj", "cn-ah", "cn-fj", "cn-jx", "cn-sd", "cn-ha", "cn-hb", "cn-hn", "cn-gd", "cn-gx", "cn-hi", "cn-cq", "cn-sc", "cn-gz", "cn-yn", "cn-xz", "cn-sn", "cn-gs", "cn-qh", "cn-nx", "cn-xj", "cn-tw", "cn-hk", "cn-mo"),
    longitude = c(116.41, 117.20, 114.48, 112.55, 111.67, 123.43, 125.32, 126.64, 121.47, 118.78, 120.16, 117.28, 119.30, 115.86, 117.00, 113.62, 114.31, 112.98, 113.27, 108.37, 110.35, 106.55, 104.07, 106.71, 102.71, 91.13, 108.95, 103.82, 101.78, 106.23, 87.62, 120.96, 114.17, 113.54),
    latitude = c(39.90, 39.12, 38.04, 37.87, 40.82, 41.81, 43.89, 45.80, 31.23, 32.04, 30.27, 31.86, 26.08, 28.68, 36.68, 34.75, 30.52, 28.20, 23.13, 22.82, 20.02, 29.56, 30.57, 26.58, 25.04, 29.65, 34.27, 36.06, 36.62, 38.49, 43.79, 23.70, 22.28, 22.20),
    stringsAsFactors = FALSE
  )

  cities <- data.frame(
    city = c("北京", "天津", "石家庄", "唐山", "保定", "太原", "呼和浩特", "沈阳", "大连", "长春", "哈尔滨", "上海", "南京", "苏州", "无锡", "常州", "南通", "杭州", "宁波", "温州", "嘉兴", "绍兴", "合肥", "芜湖", "福州", "厦门", "泉州", "南昌", "济南", "青岛", "烟台", "潍坊", "郑州", "洛阳", "武汉", "宜昌", "长沙", "株洲", "广州", "深圳", "佛山", "东莞", "珠海", "中山", "南宁", "海口", "重庆", "成都", "绵阳", "贵阳", "昆明", "西安", "宝鸡", "兰州", "西宁", "银川", "乌鲁木齐", "拉萨", "台北", "香港", "澳门"),
    province = c("北京", "天津", "河北", "河北", "河北", "山西", "内蒙古", "辽宁", "辽宁", "吉林", "黑龙江", "上海", "江苏", "江苏", "江苏", "江苏", "江苏", "浙江", "浙江", "浙江", "浙江", "浙江", "安徽", "安徽", "福建", "福建", "福建", "江西", "山东", "山东", "山东", "山东", "河南", "河南", "湖北", "湖北", "湖南", "湖南", "广东", "广东", "广东", "广东", "广东", "广东", "广西", "海南", "重庆", "四川", "四川", "贵州", "云南", "陕西", "陕西", "甘肃", "青海", "宁夏", "新疆", "西藏", "台湾", "香港", "澳门"),
    longitude = c(116.41, 117.20, 114.51, 118.18, 115.47, 112.55, 111.67, 123.43, 121.62, 125.32, 126.64, 121.47, 118.78, 120.59, 120.30, 119.97, 120.89, 120.16, 121.55, 120.70, 120.76, 120.58, 117.28, 118.38, 119.30, 118.09, 118.68, 115.86, 117.12, 120.38, 121.39, 119.11, 113.62, 112.45, 114.31, 111.29, 112.94, 113.13, 113.27, 114.06, 113.12, 113.75, 113.58, 113.38, 108.37, 110.20, 106.55, 104.07, 104.74, 106.63, 102.83, 108.95, 107.24, 103.82, 101.78, 106.23, 87.62, 91.13, 121.57, 114.17, 113.54),
    latitude = c(39.90, 39.12, 38.04, 39.63, 38.87, 37.87, 40.82, 41.81, 38.92, 43.89, 45.80, 31.23, 32.04, 31.30, 31.57, 31.77, 31.98, 30.27, 29.87, 28.00, 30.75, 30.01, 31.86, 31.33, 26.08, 24.48, 24.88, 28.68, 36.65, 36.07, 37.54, 36.71, 34.75, 34.62, 30.52, 30.69, 28.23, 27.83, 23.13, 22.54, 23.02, 23.02, 22.27, 22.52, 22.82, 20.04, 29.56, 30.57, 31.46, 26.65, 24.88, 34.27, 34.36, 36.06, 36.62, 38.49, 43.82, 29.65, 25.04, 22.28, 22.20),
    stringsAsFactors = FALSE
  )
  list(provinces = provinces, cities = cities)
}

# 用途：按省份和城市汇总上市公司数量，并自动匹配城市中心经纬度。
# 输入来源：`dashboard_data$dim_company` 中的省份与城市字段。
calc_company_geography <- function(data) {
  dim_company <- metric_table(data, "dim_company")
  empty <- list(
    provinces = data.frame(hc_key = character(), province = character(), company_count = numeric(), stringsAsFactors = FALSE),
    cities = data.frame(city = character(), province = character(), longitude = numeric(), latitude = numeric(), company_count = numeric(), stringsAsFactors = FALSE),
    unmatched_city_count = 0L
  )
  if (!metric_has_cols(dim_company, c("company_code", "city")) || nrow(dim_company) == 0L) return(empty)

  ref <- company_geo_reference()
  city_key <- function(x) gsub("市$", "", trimws(as.character(x)))
  input <- dim_company[, intersect(c("company_code", "province", "city", "longitude", "latitude"), names(dim_company)), drop = FALSE]
  if (!"province" %in% names(input)) input$province <- NA_character_
  input$city <- trimws(as.character(input$city))
  input$province <- trimws(as.character(input$province))
  input <- input[!is.na(input$city) & nzchar(input$city) & !grepl("^=", input$city), , drop = FALSE]
  if (nrow(input) == 0L) return(empty)

  # 若 dim_company 缺少经纬度，尝试从原始 Excel 读取并合并
  has_lonlat <- all(c("longitude", "latitude") %in% names(input)) &&
    any(is.finite(input$longitude) & is.finite(input$latitude))
  if (!has_lonlat) {
    input$longitude <- NULL
    input$latitude <- NULL
    excel_coords <- tryCatch({
      path <- "data/raw/上市公司基本情况.xlsx"
      if (file.exists(path) && requireNamespace("readxl", quietly = TRUE)) {
        raw <- as.data.frame(readxl::read_excel(path, sheet = "公司", .name_repair = "unique"), stringsAsFactors = FALSE)
        code_col <- if ("代码" %in% names(raw)) "代码" else if ("company_code" %in% names(raw)) "company_code" else NA_character_
        lon_col <- intersect(c("经度", "longitude", "Longitude", "lon", "LON"), names(raw))[[1]]
        lat_col <- intersect(c("纬度", "latitude", "Latitude", "lat", "LAT"), names(raw))[[1]]
        if (!is.na(code_col) && !is.na(lon_col) && !is.na(lat_col)) {
          coords <- raw[, c(code_col, lon_col, lat_col), drop = FALSE]
          names(coords) <- c("company_code", "longitude", "latitude")
          coords$longitude <- chart_safe_number(coords$longitude)
          coords$latitude <- chart_safe_number(coords$latitude)
          coords <- coords[is.finite(coords$longitude) & is.finite(coords$latitude), , drop = FALSE]
          if (nrow(coords) > 0L) coords else NULL
        } else NULL
      } else NULL
    }, error = function(e) NULL)
    if (!is.null(excel_coords)) {
      input <- merge(input, excel_coords, by = "company_code", all.x = TRUE)
    }
  }

  input$city_key <- city_key(input$city)
  ref$cities$city_key <- city_key(ref$cities$city)
  city_counts <- stats::aggregate(company_code ~ city_key, input, length)
  names(city_counts)[[2]] <- "company_count"
  matched <- merge(city_counts, ref$cities, by = "city_key", all.x = TRUE)
  supplied_province <- stats::aggregate(province ~ city_key, input, function(x) {
    values <- x[!is.na(x) & nzchar(x)]
    if (length(values) == 0L) NA_character_ else values[[1L]]
  })
  matched <- merge(matched, supplied_province, by = "city_key", all.x = TRUE, suffixes = c("", "_source"))
  matched$province <- ifelse(is.na(matched$province) | !nzchar(matched$province), matched$province_source, matched$province)
  matched$province_source <- NULL

  # 优先使用 dim_company/Excel 提供的经纬度，缺失时回退到内置城市中心
  excel_lonlat <- stats::aggregate(cbind(longitude, latitude) ~ city_key, input, function(x) {
    vals <- x[is.finite(x)]
    if (length(vals) == 0L) NA_real_ else vals[[1L]]
  })
  matched <- merge(matched, excel_lonlat, by = "city_key", all.x = TRUE, suffixes = c("", "_excel"))
  matched$longitude <- ifelse(is.finite(matched$longitude_excel), matched$longitude_excel, matched$longitude)
  matched$latitude <- ifelse(is.finite(matched$latitude_excel), matched$latitude_excel, matched$latitude)
  matched$longitude_excel <- NULL
  matched$latitude_excel <- NULL

  unmatched <- is.na(matched$longitude) | is.na(matched$latitude) | is.na(matched$province)

  # 无精确城市中心点时退回到其所属省级中心；保留该城市汇总，避免丢失公司数量。
  if (any(unmatched)) {
    province_ref <- ref$provinces[, c("province", "longitude", "latitude"), drop = FALSE]
    fallback <- merge(matched[unmatched, , drop = FALSE], province_ref, by = "province", all.x = TRUE, suffixes = c("", "_province"))
    matched$longitude[unmatched] <- fallback$longitude_province
    matched$latitude[unmatched] <- fallback$latitude_province
  }
  matched <- matched[is.finite(matched$longitude) & is.finite(matched$latitude) & !is.na(matched$province), , drop = FALSE]
  if (nrow(matched) == 0L) return(empty)

  province_counts <- stats::aggregate(company_count ~ province, matched, sum)
  province_counts <- merge(ref$provinces[, c("province", "hc_key"), drop = FALSE], province_counts, by = "province", all.x = TRUE)
  province_counts$company_count[is.na(province_counts$company_count)] <- 0
  province_counts <- province_counts[, c("hc_key", "province", "company_count"), drop = FALSE]

  list(
    provinces = province_counts,
    cities = matched[, c("city", "province", "longitude", "latitude", "company_count"), drop = FALSE],
    unmatched_city_count = sum(unmatched)
  )
}

# 用途：生成公司画像的文字洞察
# 输入来源：`calc_company_profile_kpis()`、`calc_company_industry_contribution()`、`calc_company_quality_quadrant()` 结果
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

# 用途：生成公司画像的公司明细表
# 输入来源：`dashboard_data$dim_company`、`dashboard_data$fact_financial_period`、`dashboard_data$fact_fundraising_use`、`dashboard_data$fact_risk_tag`
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
