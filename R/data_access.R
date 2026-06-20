processed_table_path <- function(table_name, processed_dir = "data/processed") {
  if (grepl("[/\\\\]", table_name) || grepl("\\.csv$", table_name, ignore.case = TRUE)) {
    return(table_name)
  }

  file.path(processed_dir, paste0(table_name, ".csv"))
}

empty_data_quality_log <- function() {
  data.frame(
    check_time = character(),
    source_file = character(),
    check_item = character(),
    status = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
}

has_processed_data <- function(table_names = c("market_position_kpi", "market_position_company_detail"),
                               processed_dir = "data/processed") {
  paths <- vapply(table_names, processed_table_path, character(1), processed_dir = processed_dir)
  all(file.exists(paths))
}

read_processed_csv <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("processed 数据文件不存在：%s", path), call. = FALSE)
  }

  utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8", check.names = FALSE)
}

load_processed_table <- function(table_name,
                                 processed_dir = "data/processed",
                                 required = FALSE) {
  path <- processed_table_path(table_name, processed_dir)

  tryCatch(
    read_processed_csv(path),
    error = function(err) {
      msg <- sprintf("读取 processed 表失败：%s。原因：%s", path, conditionMessage(err))
      if (isTRUE(required)) {
        stop(msg, call. = FALSE)
      }
      warning(msg, call. = FALSE)
      NULL
    }
  )
}

format_count <- function(x) {
  if (length(x) == 0L || is.na(x)) return("--")
  format(round(as.numeric(x), 0), big.mark = ",", scientific = FALSE, trim = TRUE)
}

format_decimal <- function(x, digits = 1) {
  if (length(x) == 0L || is.na(x)) return("--")
  format(round(as.numeric(x), digits), big.mark = ",", scientific = FALSE, trim = TRUE, nsmall = digits)
}

format_percent <- function(x, digits = 1) {
  if (length(x) == 0L || is.na(x)) return("--")
  format(round(as.numeric(x) * 100, digits), big.mark = ",", scientific = FALSE, trim = TRUE, nsmall = digits)
}

market_position_kpis_from_processed <- function(kpi) {
  row <- kpi[1, , drop = FALSE]
  current_new <- suppressWarnings(as.numeric(row$current_year_new_listed_count[[1]]))
  previous_new <- suppressWarnings(as.numeric(row$previous_year_new_listed_count[[1]]))
  new_change <- if (!is.na(current_new) && !is.na(previous_new)) {
    paste0("本年新增 ", format_count(current_new), " 家；上年 ", format_count(previous_new), " 家")
  } else if (!is.na(current_new)) {
    paste0("本年新增 ", format_count(current_new), " 家")
  } else {
    "新增上市待补充"
  }

  make_kpis(
    labels = c("上市公司", "总市值", "流通市值", "日均成交额", "PE 中位数", "前十市值占比"),
    values = c(
      format_count(row$listed_company_count[[1]]),
      format_count(row$total_market_cap_yi[[1]]),
      format_count(row$float_market_cap_yi[[1]]),
      format_decimal(row$avg_daily_turnover_yi[[1]], 1),
      format_decimal(row$pe_median[[1]], 1),
      format_percent(row$top10_market_cap_share[[1]], 1)
    ),
    units = c("家", "亿元", "亿元", "亿元", "倍", "%"),
    changes = c(
      paste0("截至 ", row$as_of_date[[1]]),
      "processed 数据",
      "processed 数据",
      "最新一期",
      "剔除异常估值",
      "总市值口径"
    ),
    statuses = c(
      if (!is.na(current_new) && !is.na(previous_new) && current_new < previous_new) "warning" else "positive",
      "positive",
      "positive",
      "positive",
      "neutral",
      "neutral"
    )
  )
}

market_position_details_from_processed <- function(company_detail, max_rows = 12L) {
  if (is.null(company_detail) || nrow(company_detail) == 0L) {
    return(data.frame(
      证券代码 = character(),
      证券简称 = character(),
      行业 = character(),
      总市值亿元 = numeric(),
      流通市值亿元 = numeric(),
      PE = numeric(),
      排名 = integer(),
      check.names = FALSE
    ))
  }

  detail <- utils::head(company_detail, max_rows)
  data.frame(
    证券代码 = detail$company_code,
    证券简称 = detail$company_name,
    行业 = detail$industry,
    总市值亿元 = round(as.numeric(detail$total_market_cap_yi), 1),
    流通市值亿元 = round(as.numeric(detail$float_market_cap_yi), 1),
    PE = round(as.numeric(detail$pe), 1),
    排名 = as.integer(detail$market_cap_rank),
    check.names = FALSE
  )
}

market_position_insights_from_processed <- function(kpi) {
  row <- kpi[1, , drop = FALSE]
  c(
    paste0("截至 ", row$as_of_date[[1]], "，本所上市公司 ", format_count(row$listed_company_count[[1]]),
           " 家，总市值 ", format_count(row$total_market_cap_yi[[1]]), " 亿元。"),
    paste0("本年新增上市公司 ", format_count(row$current_year_new_listed_count[[1]]),
           " 家，上年新增 ", format_count(row$previous_year_new_listed_count[[1]]), " 家。"),
    paste0("前十市值占比 ", format_percent(row$top10_market_cap_share[[1]], 1),
           "%，头部集中度可作为后续定位画像重点观察。")
  )
}

build_market_position_page_data <- function(kpi, company_detail, sample_market_position) {
  if (is.null(kpi) || nrow(kpi) == 0L || is.null(company_detail)) {
    return(sample_market_position)
  }

  sample_market_position$kpis <- market_position_kpis_from_processed(kpi)
  sample_market_position$insights <- market_position_insights_from_processed(kpi)
  sample_market_position$details <- market_position_details_from_processed(company_detail)
  sample_market_position$judgment <- "本所在多层次资本市场中呈现中小市值、成长型企业聚集的市场定位，当前页面已优先接入 processed 基础数据。"
  sample_market_position
}

load_market_position_kpi <- function(path = "data/processed/market_position_kpi.csv") {
  read_processed_csv(path)
}

load_market_position_company_detail <- function(path = "data/processed/market_position_company_detail.csv") {
  read_processed_csv(path)
}

load_data_quality_log <- function(path = "data/processed/data_quality_log.csv") {
  read_processed_csv(path)
}

load_market_position_data <- function(processed_dir = "data/processed",
                                      sample_data = load_demo_data()) {
  required_tables <- c("market_position_kpi", "market_position_company_detail")

  if (!has_processed_data(required_tables, processed_dir)) {
    missing_paths <- vapply(required_tables, processed_table_path, character(1), processed_dir = processed_dir)
    missing_paths <- missing_paths[!file.exists(missing_paths)]
    warning(
      paste0(
        "市场定位 processed 数据缺失：",
        paste(missing_paths, collapse = ", "),
        "。已回退到 R/sample_data.R 的演示数据。"
      ),
      call. = FALSE
    )

    return(list(
      source = "sample_data",
      market_position_kpi = data.frame(),
      market_position_company_detail = data.frame(),
      data_quality_log = empty_data_quality_log(),
      page = sample_data$market_position
    ))
  }

  kpi <- load_processed_table("market_position_kpi", processed_dir)
  company_detail <- load_processed_table("market_position_company_detail", processed_dir)
  quality_log <- load_processed_table("data_quality_log", processed_dir)
  if (is.null(quality_log)) {
    quality_log <- empty_data_quality_log()
  }

  if (is.null(kpi) || is.null(company_detail)) {
    warning("市场定位 processed 数据读取失败，已回退到 R/sample_data.R 的演示数据。", call. = FALSE)
    return(list(
      source = "sample_data",
      market_position_kpi = data.frame(),
      market_position_company_detail = data.frame(),
      data_quality_log = quality_log,
      page = sample_data$market_position
    ))
  }

  list(
    source = "processed",
    market_position_kpi = kpi,
    market_position_company_detail = company_detail,
    data_quality_log = quality_log,
    page = build_market_position_page_data(kpi, company_detail, sample_data$market_position)
  )
}

load_dashboard_data <- function(use_processed = TRUE,
                                processed_dir = "data/processed",
                                use_demo = FALSE) {
  sample_data <- load_demo_data()
  dashboard <- sample_data
  dashboard$sample_data <- sample_data
  dashboard$demo_data <- sample_data

  standard_tables <- c(
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

  attach_empty_standard_tables <- function(x) {
    for (table_name in standard_tables) {
      x[[table_name]] <- data.frame()
    }
    x
  }

  if (!isTRUE(use_processed) || isTRUE(use_demo)) {
    dashboard$market_position_kpi <- data.frame()
    dashboard$market_position_company_detail <- data.frame()
    dashboard$data_quality_log <- empty_data_quality_log()
    dashboard$data_source <- list(market_position = "sample_data")
    return(attach_empty_standard_tables(dashboard))
  }

  market_position <- load_market_position_data(processed_dir = processed_dir, sample_data = sample_data)
  dashboard$market_position <- market_position$page
  dashboard$market_position_kpi <- market_position$market_position_kpi
  dashboard$market_position_company_detail <- market_position$market_position_company_detail
  dashboard$data_quality_log <- market_position$data_quality_log
  dashboard$data_source <- list(market_position = market_position$source)

  for (table_name in standard_tables) {
    table <- load_processed_table(table_name, processed_dir = processed_dir)
    dashboard[[table_name]] <- if (is.null(table)) data.frame() else table
  }

  dashboard
}
