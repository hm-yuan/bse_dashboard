# Basic processing pipeline for raw market-position data.
# This file is intentionally independent from Shiny page modules.

.basic_data_log <- new.env(parent = emptyenv())
.basic_data_log$rows <- list()

# 用途：重置全局数据质量日志环境变量
# 输入来源：无，函数内部清空 .basic_data_log
reset_data_quality_log <- function() {
  .basic_data_log$rows <- list()
}

# 用途：向全局数据质量日志中添加一条检查记录
# 输入来源：函数输入参数
log_data_quality <- function(source_file, check_item, status, message) {
  .basic_data_log$rows[[length(.basic_data_log$rows) + 1L]] <- data.frame(
    check_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    source_file = as.character(source_file),
    check_item = as.character(check_item),
    status = as.character(status),
    message = as.character(message),
    stringsAsFactors = FALSE
  )
}

# 用途：获取当前累积的数据质量日志数据框
# 输入来源：.basic_data_log 全局环境
get_data_quality_log <- function() {
  if (length(.basic_data_log$rows) == 0L) {
    return(data.frame(
      check_time = character(),
      source_file = character(),
      check_item = character(),
      status = character(),
      message = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, .basic_data_log$rows)
}

invalid_data_tokens <- c("", "#NAME?", "#VALUE!", "#N/A", "N/A", "NA", "--", "-", "NULL", "null")

# 用途：反转义 XML 中的常见特殊字符实体
# 输入来源：函数输入参数（XML 字符串）
xml_unescape <- function(x) {
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&apos;", "'", x, fixed = TRUE)
  x
}

# 用途：从 XML 节点字符串中提取指定属性的值
# 输入来源：函数输入参数（XML 字符串）
extract_attr <- function(x, attr_name) {
  pattern <- paste0(attr_name, "=\"([^\"]*)\"")
  hit <- regexec(pattern, x, perl = TRUE)
  out <- regmatches(x, hit)
  vapply(out, function(item) {
    if (length(item) >= 2L) item[[2L]] else NA_character_
  }, character(1))
}

# 用途：从 zip 压缩包中读取指定 XML 成员内容为字符串
# 输入来源：data/raw/*.xlsx
read_zip_xml <- function(path, member) {
  paste(readLines(unz(path, member), warn = FALSE, encoding = "UTF-8"), collapse = "")
}

# 用途：读取 xlsx 文件中的共享字符串表
# 输入来源：data/raw/*.xlsx
read_xlsx_shared_strings <- function(path) {
  files <- utils::unzip(path, list = TRUE)$Name
  if (!"xl/sharedStrings.xml" %in% files) {
    return(character())
  }

  xml <- read_zip_xml(path, "xl/sharedStrings.xml")
  items <- regmatches(xml, gregexpr("<si[^>]*>.*?</si>", xml, perl = TRUE))[[1]]
  if (length(items) == 0L || identical(items, character(0))) {
    return(character())
  }

  vapply(items, function(item) {
    text_nodes <- regmatches(item, gregexpr("<t[^>]*>.*?</t>", item, perl = TRUE))[[1]]
    if (length(text_nodes) == 0L || identical(text_nodes, character(0))) {
      return("")
    }
    text <- gsub("^<t[^>]*>|</t>$", "", text_nodes, perl = TRUE)
    xml_unescape(paste(text, collapse = ""))
  }, character(1), USE.NAMES = FALSE)
}

# 用途：将 Excel 列引用字母转换为列索引
# 输入来源：函数输入参数（单元格引用字符串）
col_ref_to_index <- function(ref) {
  letters <- gsub("[0-9]", "", ref)
  chars <- strsplit(letters, "", fixed = TRUE)[[1]]
  as.integer(sum((match(chars, LETTERS)) * (26 ^ rev(seq_along(chars) - 1L))))
}

# 用途：不依赖外部包读取 xlsx 工作表内容为数据框
# 输入来源：data/raw/*.xlsx
read_xlsx_base <- function(path, sheet_member = "xl/worksheets/sheet1.xml") {
  shared_strings <- read_xlsx_shared_strings(path)
  sheet_xml <- read_zip_xml(path, sheet_member)
  rows <- regmatches(sheet_xml, gregexpr("<row[^>]*>.*?</row>", sheet_xml, perl = TRUE))[[1]]

  if (length(rows) == 0L || identical(rows, character(0))) {
    return(data.frame())
  }

  parsed <- vector("list", length(rows))
  max_col <- 0L

  for (i in seq_along(rows)) {
    cells <- regmatches(rows[[i]], gregexpr("<c[^>]*>.*?</c>", rows[[i]], perl = TRUE))[[1]]
    if (length(cells) == 0L || identical(cells, character(0))) {
      parsed[[i]] <- list(col = integer(), value = character())
      next
    }

    refs <- extract_attr(cells, "r")
    types <- extract_attr(cells, "t")
    cols <- vapply(refs, col_ref_to_index, integer(1))
    max_col <- max(max_col, cols, na.rm = TRUE)

    values <- vapply(seq_along(cells), function(j) {
      cell <- cells[[j]]
      type <- types[[j]]

      if (identical(type, "inlineStr")) {
        text_nodes <- regmatches(cell, gregexpr("<t[^>]*>.*?</t>", cell, perl = TRUE))[[1]]
        if (length(text_nodes) == 0L || identical(text_nodes, character(0))) return(NA_character_)
        return(xml_unescape(gsub("^<t[^>]*>|</t>$", "", paste(text_nodes, collapse = ""), perl = TRUE)))
      }

      v <- regmatches(cell, regexpr("<v>.*?</v>", cell, perl = TRUE))
      if (length(v) == 0L || is.na(v) || identical(v, character(0))) {
        return(NA_character_)
      }

      v <- xml_unescape(gsub("^<v>|</v>$", "", v, perl = TRUE))
      if (identical(type, "s")) {
        idx <- suppressWarnings(as.integer(v)) + 1L
        if (!is.na(idx) && idx >= 1L && idx <= length(shared_strings)) {
          return(shared_strings[[idx]])
        }
      }
      v
    }, character(1), USE.NAMES = FALSE)

    parsed[[i]] <- list(col = cols, value = values)
  }

  mat <- matrix(NA_character_, nrow = length(rows), ncol = max_col)
  for (i in seq_along(parsed)) {
    item <- parsed[[i]]
    if (length(item$col) > 0L) {
      mat[i, item$col] <- item$value
    }
  }

  non_empty_row <- apply(mat, 1, function(x) any(!is.na(x) & nzchar(trimws(x))))
  mat <- mat[non_empty_row, , drop = FALSE]
  if (nrow(mat) == 0L) return(data.frame())

  header <- trimws(mat[1, ])
  header[is.na(header) | !nzchar(header)] <- paste0("X", which(is.na(header) | !nzchar(header)))
  header <- make.unique(header)

  data <- as.data.frame(mat[-1, , drop = FALSE], stringsAsFactors = FALSE)
  names(data) <- header
  data
}

# 用途：读取 xlsx 文件，优先使用 readxl/openxlsx 包，否则回退到基础实现
# 输入来源：data/raw/*.xlsx
read_xlsx_file <- function(path) {
  if (requireNamespace("readxl", quietly = TRUE)) {
    return(as.data.frame(readxl::read_excel(path, .name_repair = "unique"), stringsAsFactors = FALSE))
  }
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    return(as.data.frame(openxlsx::read.xlsx(path, detectDates = TRUE), stringsAsFactors = FALSE))
  }

  read_xlsx_base(path)
}

# 用途：检测数据框中是否存在 Wind/Excel 异常公式值并记录日志
# 输入来源：函数输入参数（由 read_xlsx_file 读取的数据框）
detect_wind_formula_errors <- function(df, source_file) {
  values <- trimws(as.character(unlist(df, use.names = FALSE)))
  bad <- values[values %in% c("#NAME?", "#VALUE!", "#N/A")]

  if (length(bad) > 0L) {
    log_data_quality(
      source_file,
      "wind_formula_errors",
      "warning",
      paste0("检测到 Wind/Excel 异常值 ", length(bad), " 个：", paste(unique(bad), collapse = ", "))
    )
  } else {
    log_data_quality(source_file, "wind_formula_errors", "ok", "未检测到 #NAME?/#VALUE!/#N/A")
  }
}

# 用途：读取上市公司基本情况 Excel 文件并记录数据质量日志
# 输入来源：data/raw/上市公司基本情况.xlsx
read_basic_company_data <- function(path) {
  if (!file.exists(path)) {
    log_data_quality(path, "file_exists", "error", "输入文件不存在")
    df <- data.frame()
    attr(df, "source_file") <- path
    return(df)
  }

  log_data_quality(path, "file_exists", "ok", "输入文件存在")
  df <- read_xlsx_file(path)
  attr(df, "source_file") <- path
  log_data_quality(path, "read_file", "ok", paste0("读取 ", nrow(df), " 行、", ncol(df), " 列"))
  detect_wind_formula_errors(df, path)
  df
}

# 用途：读取市场板块成交统计 Excel 文件并记录数据质量日志
# 输入来源：data/raw/市场板块成交统计.xlsx
read_board_trading_data <- function(path) {
  if (!file.exists(path)) {
    log_data_quality(path, "file_exists", "error", "输入文件不存在")
    df <- data.frame()
    attr(df, "source_file") <- path
    return(df)
  }

  log_data_quality(path, "file_exists", "ok", "输入文件存在")
  df <- read_xlsx_file(path)
  attr(df, "source_file") <- path
  log_data_quality(path, "read_file", "ok", paste0("读取 ", nrow(df), " 行、", ncol(df), " 列"))
  detect_wind_formula_errors(df, path)
  df
}

# 用途：标准化字段名，去除大小写、空格和特殊符号用于匹配
# 输入来源：函数输入参数（字段名字符串）
normalize_field_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  gsub("[[:space:]_\\-\\.（）()【】\\[\\]：:]+", "", x, perl = TRUE)
}

# 用途：在数据框中按候选字段名匹配实际字段
# 输入来源：函数输入参数（由 read_xlsx_file 读取的数据框）
match_field <- function(df, candidates, source_file, check_item, required = FALSE) {
  nms <- names(df)
  if (length(nms) == 0L) {
    log_data_quality(source_file, check_item, if (required) "error" else "warning", "输入表为空，无法匹配字段")
    return(NA_character_)
  }

  normalized_names <- normalize_field_name(nms)
  normalized_candidates <- normalize_field_name(candidates)

  for (candidate in normalized_candidates) {
    idx <- match(candidate, normalized_names)
    if (!is.na(idx)) {
      log_data_quality(source_file, check_item, "ok", paste0("匹配字段：", nms[[idx]]))
      return(nms[[idx]])
    }
  }

  for (candidate in normalized_candidates) {
    idx <- which(grepl(candidate, normalized_names, fixed = TRUE))
    idx <- idx[nzchar(normalized_names[idx])]
    if (length(idx) > 0L) {
      log_data_quality(source_file, check_item, "ok", paste0("模糊匹配字段：", nms[[idx[[1L]]]]))
      return(nms[[idx[[1L]]]])
    }
  }

  log_data_quality(
    source_file,
    check_item,
    if (required) "error" else "warning",
    paste0("未匹配字段，候选：", paste(candidates, collapse = " / "))
  )
  NA_character_
}

# 用途：判断向量中的值是否为无效/缺失标记
# 输入来源：函数输入参数
is_invalid_value <- function(x) {
  x <- trimws(as.character(x))
  is.na(x) | x %in% invalid_data_tokens
}

# 用途：从字符串中提取第一个数值
# 输入来源：函数输入参数
extract_first_number <- function(x) {
  x <- gsub(",", "", as.character(x), fixed = TRUE)
  hit <- regexpr("[-+]?[0-9]*\\.?[0-9]+", x, perl = TRUE)
  out <- rep(NA_real_, length(x))
  ok <- hit > 0L
  out[ok] <- suppressWarnings(as.numeric(regmatches(x, hit)[ok]))
  out
}

# 用途：解析金额字符串并统一换算为亿元单位
# 输入来源：函数输入参数（Excel 原始字段值）
parse_money_yi <- function(x, source_file, field_label) {
  raw <- trimws(as.character(x))
  invalid <- is_invalid_value(raw)
  value <- extract_first_number(raw)
  value[invalid] <- NA_real_

  factor <- rep(NA_real_, length(raw))
  factor[grepl("万亿", raw, fixed = TRUE)] <- 10000
  factor[is.na(factor) & grepl("亿元|亿", raw, perl = TRUE)] <- 1
  factor[is.na(factor) & grepl("万元", raw, fixed = TRUE)] <- 1 / 10000
  factor[is.na(factor) & grepl("元", raw, fixed = TRUE)] <- 1 / 100000000

  no_unit <- !is.na(value) & is.na(factor)
  if (any(no_unit)) {
    med <- suppressWarnings(stats::median(abs(value[no_unit]), na.rm = TRUE))
    inferred_factor <- if (is.finite(med) && med > 10000000) {
      1 / 100000000
    } else if (is.finite(med) && med > 10000) {
      1 / 10000
    } else {
      1
    }

    inferred_unit <- if (identical(inferred_factor, 1 / 100000000)) {
      "元"
    } else if (identical(inferred_factor, 1 / 10000)) {
      "万元"
    } else {
      "亿元"
    }

    factor[no_unit] <- inferred_factor
    log_data_quality(
      source_file,
      paste0(field_label, "_amount_unit_inference"),
      "warning",
      paste0("无显式单位的金额字段按 ", inferred_unit, " 推断，并统一换算为亿元")
    )
  }

  out <- value * factor
  converted <- sum(!is.na(out))
  total <- sum(!invalid)
  log_data_quality(
    source_file,
    paste0(field_label, "_numeric_conversion"),
    if (converted > 0L || total == 0L) "ok" else "warning",
    paste0("有效值 ", total, " 个，成功转换 ", converted, " 个")
  )
  out
}

# 用途：解析普通数值字符串，支持百分号自动转换为比例
# 输入来源：函数输入参数（Excel 原始字段值）
parse_plain_number <- function(x, source_file, field_label) {
  raw <- trimws(as.character(x))
  invalid <- is_invalid_value(raw)
  out <- extract_first_number(raw)
  percent <- grepl("%", raw, fixed = TRUE)
  out[percent] <- out[percent] / 100
  out[invalid] <- NA_real_

  converted <- sum(!is.na(out))
  total <- sum(!invalid)
  log_data_quality(
    source_file,
    paste0(field_label, "_numeric_conversion"),
    if (converted > 0L || total == 0L) "ok" else "warning",
    paste0("有效值 ", total, " 个，成功转换 ", converted, " 个")
  )
  out
}

# 用途：安全解析多种格式的日期字符串
# 输入来源：函数输入参数（Excel 原始字段值）
parse_date_safe <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))

  raw <- trimws(as.character(x))
  out <- rep(as.Date(NA), length(raw))
  invalid <- is_invalid_value(raw)

  numeric_value <- suppressWarnings(as.numeric(raw))
  excel_date <- !invalid & !is.na(numeric_value) & numeric_value > 20000 & numeric_value < 70000
  out[excel_date] <- as.Date(numeric_value[excel_date], origin = "1899-12-30")

  formats <- c("%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y%m%d", "%Y-%m", "%Y/%m", "%Y")
  remaining <- which(!invalid & is.na(out))
  for (fmt in formats) {
    if (length(remaining) == 0L) break
    parsed <- suppressWarnings(as.Date(raw[remaining], format = fmt))
    ok <- !is.na(parsed)
    out[remaining[ok]] <- parsed[ok]
    remaining <- remaining[!ok]
  }

  out
}

# 用途：将上市公司基本情况原始字段标准化为统一字段结构
# 输入来源：data/raw/上市公司基本情况.xlsx
standardize_company_fields <- function(df) {
  source_file <- attr(df, "source_file", exact = TRUE)
  if (is.null(source_file)) source_file <- "上市公司基本情况.xlsx"

  if (nrow(df) == 0L) {
    return(data.frame(
      company_code = character(),
      company_name = character(),
      board = character(),
      listing_date = as.Date(character()),
      industry = character(),
      city = character(),
      total_market_cap_yi = numeric(),
      float_market_cap_yi = numeric(),
      pe = numeric(),
      pe_source = character(),
      stringsAsFactors = FALSE
    ))
  }

  code_col <- match_field(df, c("代码", "证券代码", "股票代码", "公司代码", "Wind代码", "wind_code"), source_file, "match_company_code", TRUE)
  name_col <- match_field(df, c("名称", "证券简称", "股票简称", "公司名称", "简称"), source_file, "match_company_name", TRUE)
  board_col <- match_field(df, c("上市板块", "板块", "市场板块", "交易板块", "所属板块"), source_file, "match_board", TRUE)
  date_col <- match_field(df, c("上市日期", "上市时间", "挂牌日期", "上市日"), source_file, "match_listing_date", FALSE)
  industry_col <- match_field(df, c("行业", "所属行业", "证监会行业", "申万行业", "行业名称"), source_file, "match_industry", FALSE)
  city_col <- match_field(df, c("城市", "所在城市", "注册城市", "办公城市", "所在地城市"), source_file, "match_city", FALSE)
  province_col <- match_field(df, c("省份", "省", "所在省份", "注册省份"), source_file, "match_province", FALSE)
  lon_col <- match_field(df, c("经度", "longitude", "lon", "Longitude", "LON"), source_file, "match_longitude", FALSE)
  lat_col <- match_field(df, c("纬度", "latitude", "lat", "Latitude", "LAT"), source_file, "match_latitude", FALSE)
  total_cap_col <- match_field(df, c("总市值", "总市值亿元", "总市值(亿元)", "总市值（亿元）", "市值"), source_file, "match_total_market_cap", TRUE)
  float_cap_col <- match_field(df, c("流通市值", "流通市值亿元", "流通市值(亿元)", "流通市值（亿元）"), source_file, "match_float_market_cap", TRUE)
  pe_ttm_col <- match_field(df, c("PE_TTM", "PETTM", "市盈率TTM", "市盈率_TTM", "市盈率 TTM"), source_file, "match_pe_ttm", FALSE)
  pe_col <- match_field(df, c("PE", "市盈率"), source_file, "match_pe", FALSE)
  issue_pe_col <- match_field(df, c("发行市盈率"), source_file, "match_issue_pe", FALSE)

  pick <- function(col, default = NA_character_) {
    if (!is.na(col) && col %in% names(df)) as.character(df[[col]]) else rep(default, nrow(df))
  }

  pe_source <- "PE_TTM"
  pe_values <- if (!is.na(pe_ttm_col)) {
    parse_plain_number(df[[pe_ttm_col]], source_file, "pe_ttm")
  } else if (!is.na(pe_col)) {
    pe_source <- "PE"
    parse_plain_number(df[[pe_col]], source_file, "pe")
  } else if (!is.na(issue_pe_col)) {
    pe_source <- "发行市盈率"
    log_data_quality(source_file, "pe_source", "warning", "未匹配 PE_TTM/PE，暂用发行市盈率；该字段不等同于当前估值水平")
    parse_plain_number(df[[issue_pe_col]], source_file, "issue_pe")
  } else {
    pe_source <- NA_character_
    log_data_quality(source_file, "pe_source", "warning", "未匹配可用 PE 字段")
    rep(NA_real_, nrow(df))
  }

  pick_num <- function(col, default = NA_real_) {
    if (!is.na(col) && col %in% names(df)) chart_safe_number(df[[col]]) else rep(default, nrow(df))
  }

  data.frame(
    company_code = pick(code_col),
    company_name = pick(name_col),
    board = pick(board_col),
    listing_date = if (!is.na(date_col)) parse_date_safe(df[[date_col]]) else as.Date(rep(NA_character_, nrow(df))),
    industry = pick(industry_col),
    city = pick(city_col),
    province = pick(province_col),
    longitude = pick_num(lon_col),
    latitude = pick_num(lat_col),
    total_market_cap_yi = if (!is.na(total_cap_col)) parse_money_yi(df[[total_cap_col]], source_file, "total_market_cap") else rep(NA_real_, nrow(df)),
    float_market_cap_yi = if (!is.na(float_cap_col)) parse_money_yi(df[[float_cap_col]], source_file, "float_market_cap") else rep(NA_real_, nrow(df)),
    pe = pe_values,
    pe_source = rep(pe_source, nrow(df)),
    stringsAsFactors = FALSE
  )
}

# 用途：生成城市匹配键，兼容“北京/北京市”等常见写法
# 输入来源：城市名称字段
normalize_city_key <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[[:space:]　]+", "", x, perl = TRUE)
  x <- gsub("(市|地区|盟|自治州|特别行政区)$", "", x, perl = TRUE)
  x
}

# 用途：从上市公司基本情况标准化结果中拆分城市经纬度信息表
# 输入来源：standardize_company_fields() 的输出
build_city_coordinates <- function(company_df, source_file = "data/raw/上市公司基本情况.xlsx") {
  empty <- data.frame(
    province = character(),
    city = character(),
    city_key = character(),
    company_count = integer(),
    longitude = numeric(),
    latitude = numeric(),
    source_file = character(),
    last_update_time = character(),
    stringsAsFactors = FALSE
  )

  if (!all(c("city", "longitude", "latitude") %in% names(company_df))) {
    log_data_quality(source_file, "city_coordinates_extract", "warning", "缺少城市或经纬度字段，未生成城市经纬度信息表")
    return(empty)
  }

  coord <- company_df[, intersect(c("company_code", "province", "city", "longitude", "latitude"), names(company_df)), drop = FALSE]
  if (!"province" %in% names(coord)) coord$province <- NA_character_
  if (!"company_code" %in% names(coord)) coord$company_code <- seq_len(nrow(coord))
  coord$city <- trimws(as.character(coord$city))
  coord$province <- trimws(as.character(coord$province))
  coord$city_key <- normalize_city_key(coord$city)
  coord$longitude <- chart_safe_number(coord$longitude)
  coord$latitude <- chart_safe_number(coord$latitude)
  coord <- coord[
    !is.na(coord$city_key) & nzchar(coord$city_key) &
      is.finite(coord$longitude) & is.finite(coord$latitude),
    ,
    drop = FALSE
  ]

  if (nrow(coord) == 0L) {
    log_data_quality(source_file, "city_coordinates_extract", "warning", "未提取到有效城市经纬度记录")
    return(empty)
  }

  province <- stats::aggregate(province ~ city_key, coord, function(x) {
    vals <- unique(x[!is.na(x) & nzchar(x)])
    if (length(vals) == 0L) NA_character_ else vals[[1L]]
  })
  city <- stats::aggregate(city ~ city_key, coord, function(x) {
    vals <- unique(x[!is.na(x) & nzchar(x)])
    if (length(vals) == 0L) NA_character_ else vals[[1L]]
  })
  count <- stats::aggregate(company_code ~ city_key, coord, function(x) length(unique(x[!is.na(x) & nzchar(as.character(x))])))
  names(count)[[2]] <- "company_count"
  lonlat <- stats::aggregate(cbind(longitude, latitude) ~ city_key, coord, function(x) stats::median(x, na.rm = TRUE))

  out <- Reduce(function(x, y) merge(x, y, by = "city_key", all = TRUE), list(city, province, count, lonlat))
  out$source_file <- basename(source_file)
  out$last_update_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  out <- out[, c("province", "city", "city_key", "company_count", "longitude", "latitude", "source_file", "last_update_time"), drop = FALSE]
  out <- out[order(out$province, out$city, na.last = TRUE), , drop = FALSE]

  log_data_quality(source_file, "city_coordinates_extract", "ok", paste0("生成城市经纬度信息 ", nrow(out), " 条"))
  out
}

# 用途：读取已拆分的城市经纬度信息表
# 输入来源：data/processed/city_coordinates.csv
read_city_coordinates <- function(path) {
  if (!file.exists(path)) {
    return(data.frame())
  }

  out <- utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8", check.names = FALSE)
  if ("city" %in% names(out) && !"city_key" %in% names(out)) {
    out$city_key <- normalize_city_key(out$city)
  }
  if ("longitude" %in% names(out)) out$longitude <- chart_safe_number(out$longitude)
  if ("latitude" %in% names(out)) out$latitude <- chart_safe_number(out$latitude)
  out
}

# 用途：在公司基础表缺少经纬度时，使用城市经纬度信息表按城市匹配补齐
# 输入来源：standardize_company_fields() 输出和 city_coordinates.csv
apply_city_coordinates <- function(company_df, city_coordinates, source_file = "data/raw/上市公司基本情况.xlsx") {
  if (!is.data.frame(company_df) || nrow(company_df) == 0L) return(company_df)
  if (!"longitude" %in% names(company_df)) company_df$longitude <- NA_real_
  if (!"latitude" %in% names(company_df)) company_df$latitude <- NA_real_
  if (!"province" %in% names(company_df)) company_df$province <- NA_character_
  if (!"city" %in% names(company_df)) return(company_df)

  if (!is.data.frame(city_coordinates) || nrow(city_coordinates) == 0L ||
      !all(c("city_key", "longitude", "latitude") %in% names(city_coordinates))) {
    log_data_quality(source_file, "city_coordinates_match", "warning", "城市经纬度信息表不可用，未执行坐标匹配")
    return(company_df)
  }

  company_df$city_key <- normalize_city_key(company_df$city)
  city_coordinates <- city_coordinates[is.finite(city_coordinates$longitude) & is.finite(city_coordinates$latitude), , drop = FALSE]
  matched_index <- match(company_df$city_key, city_coordinates$city_key)
  has_match <- !is.na(matched_index)
  need_lonlat <- !is.finite(company_df$longitude) | !is.finite(company_df$latitude)
  fill <- has_match & need_lonlat

  company_df$longitude[fill] <- city_coordinates$longitude[matched_index[fill]]
  company_df$latitude[fill] <- city_coordinates$latitude[matched_index[fill]]

  if ("province" %in% names(city_coordinates)) {
    need_province <- is.na(company_df$province) | !nzchar(trimws(as.character(company_df$province)))
    fill_province <- has_match & need_province
    company_df$province[fill_province] <- city_coordinates$province[matched_index[fill_province]]
  }

  company_df$city_key <- NULL
  log_data_quality(source_file, "city_coordinates_match", "ok", paste0("按城市匹配补齐坐标 ", sum(fill, na.rm = TRUE), " 行"))
  company_df
}

# 用途：将市场板块成交统计原始字段标准化为统一字段结构
# 输入来源：data/raw/市场板块成交统计.xlsx
standardize_board_trading <- function(df) {
  source_file <- attr(df, "source_file", exact = TRUE)
  if (is.null(source_file)) source_file <- "市场板块成交统计.xlsx"

  empty <- data.frame(
    board = character(),
    period = as.Date(character()),
    avg_daily_turnover_yi = numeric(),
    stringsAsFactors = FALSE
  )

  if (nrow(df) == 0L) return(empty)

  board_col <- match_field(df, c("板块", "市场板块", "市场", "交易所", "名称", "板块名称"), source_file, "match_trading_board", FALSE)
  period_col <- match_field(df, c("日期", "交易日期", "统计日期", "期间", "年度", "年份", "月份", "时间"), source_file, "match_trading_period", FALSE)
  turnover_col <- match_field(df, c("日均成交金额", "日均成交额", "日均成交", "平均每日成交额", "avg_daily_turnover"), source_file, "match_avg_daily_turnover", FALSE)

  if (!is.na(board_col) && !is.na(turnover_col)) {
    out <- data.frame(
      board = as.character(df[[board_col]]),
      period = if (!is.na(period_col)) parse_date_safe(df[[period_col]]) else as.Date(rep(NA_character_, nrow(df))),
      avg_daily_turnover_yi = parse_money_yi(df[[turnover_col]], source_file, "avg_daily_turnover"),
      stringsAsFactors = FALSE
    )
    log_data_quality(source_file, "turnover_extraction", "ok", "按标准长表字段提取日均成交额")
    return(out)
  }

  bse_cols <- grep("北证|北交|BJ|BSE", names(df), ignore.case = TRUE, perl = TRUE)
  if (length(bse_cols) > 0L && nrow(df) >= 2L) {
    first_row <- as.character(df[1, ])
    metric_text <- paste(names(df), first_row, sep = " ")
    turnover_candidates <- bse_cols[grepl("日均.*成交额|日均成交金额", metric_text[bse_cols], perl = TRUE)]

    if (length(turnover_candidates) == 0L && length(bse_cols) >= 4L) {
      turnover_candidates <- bse_cols[[4L]]
      log_data_quality(source_file, "turnover_extraction", "warning", "北交所宽表未显式匹配日均成交额，按第 4 个北交所指标列回退提取")
    }

    if (length(turnover_candidates) > 0L) {
      turnover_candidate <- turnover_candidates[[1L]]
      data_rows <- seq.int(2L, nrow(df))
      board_name <- sub("\\.[0-9]+$", "", names(df)[[turnover_candidate]])

      out <- data.frame(
        board = rep(board_name, length(data_rows)),
        period = if (!is.na(period_col)) parse_date_safe(df[[period_col]][data_rows]) else as.Date(rep(NA_character_, length(data_rows))),
        avg_daily_turnover_yi = parse_money_yi(df[[turnover_candidate]][data_rows], source_file, "avg_daily_turnover"),
        stringsAsFactors = FALSE
      )
      out <- out[!is.na(out$avg_daily_turnover_yi), , drop = FALSE]
      log_data_quality(source_file, "turnover_extraction", "ok", paste0("按宽表二级指标提取 ", board_name, " 日均成交额"))
      return(out)
    }
  }

  all_values <- as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  bse_row <- apply(all_values, 1, function(row) any(grepl("北证|北交|BJ|BSE", row, ignore.case = TRUE, perl = TRUE)))
  if (!any(bse_row)) {
    log_data_quality(source_file, "turnover_extraction", "warning", "未能在成交统计表中识别北证/北交所行")
    return(empty)
  }

  metric_cols <- grep("日均|成交", names(all_values), perl = TRUE)
  if (length(metric_cols) == 0L) {
    metric_cols <- seq_along(all_values)
    log_data_quality(source_file, "turnover_extraction", "warning", "未匹配日均成交额字段，回退扫描北证行中的数值")
  }

  rows <- all_values[bse_row, , drop = FALSE]
  values <- unlist(rows[, metric_cols, drop = FALSE], use.names = FALSE)
  turnover <- parse_money_yi(values, source_file, "avg_daily_turnover")
  turnover <- turnover[!is.na(turnover)]

  if (length(turnover) == 0L) {
    log_data_quality(source_file, "turnover_extraction", "warning", "北证行中未提取到可用日均成交额")
    return(empty)
  }

  log_data_quality(source_file, "turnover_extraction", "warning", "按宽表/非标准结构回退提取日均成交额")
  data.frame(
    board = rep("北交所", length(turnover)),
    period = as.Date(rep(NA_character_, length(turnover))),
    avg_daily_turnover_yi = turnover,
    stringsAsFactors = FALSE
  )
}

# 用途：从公司表中筛选出北交所上市公司
# 输入来源：data/raw/上市公司基本情况.xlsx
filter_bse_companies <- function(df) {
  source_file <- attr(df, "source_file", exact = TRUE)
  if (is.null(source_file)) source_file <- "上市公司基本情况.xlsx"

  if (!all(c("company_code", "board") %in% names(df))) {
    df <- standardize_company_fields(df)
  }

  board <- as.character(df$board)
  code <- as.character(df$company_code)
  is_bse <- grepl("北证|北交|BJ|BSE", board, ignore.case = TRUE, perl = TRUE) |
    grepl("\\.(BJ|BSE)$", code, ignore.case = TRUE, perl = TRUE)

  out <- df[is_bse & !is.na(is_bse), , drop = FALSE]
  if (nrow(out) > 0L) {
    log_data_quality(source_file, "bse_company_count", "ok", paste0("识别北证股票 ", nrow(out), " 行"))
  } else {
    log_data_quality(source_file, "bse_company_count", "error", "北证股票筛选结果为 0")
  }
  out
}

# 用途：基于上市公司和成交统计数据计算市场定位 KPI
# 输入来源：data/raw/上市公司基本情况.xlsx、data/raw/市场板块成交统计.xlsx
calc_market_position_kpi <- function(company_df, board_trading_df) {
  if (!all(c("company_code", "total_market_cap_yi", "float_market_cap_yi", "pe") %in% names(company_df))) {
    company_df <- standardize_company_fields(company_df)
  }
  if (!all(c("board", "period", "avg_daily_turnover_yi") %in% names(board_trading_df))) {
    board_trading_df <- standardize_board_trading(board_trading_df)
  }

  bse <- filter_bse_companies(company_df)
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  previous_year <- current_year - 1L

  company_codes <- unique(bse$company_code[!is.na(bse$company_code) & nzchar(bse$company_code)])
  listing_year <- suppressWarnings(as.integer(format(bse$listing_date, "%Y")))

  total_market_cap <- if (all(is.na(bse$total_market_cap_yi))) NA_real_ else sum(bse$total_market_cap_yi, na.rm = TRUE)
  float_market_cap <- if (all(is.na(bse$float_market_cap_yi))) NA_real_ else sum(bse$float_market_cap_yi, na.rm = TRUE)

  valid_pe <- bse$pe[!is.na(bse$pe) & bse$pe > 0 & bse$pe <= 300]
  pe_median <- if (length(valid_pe) > 0L) stats::median(valid_pe, na.rm = TRUE) else NA_real_
  if (length(valid_pe) == 0L) {
    log_data_quality("上市公司基本情况.xlsx", "pe_median", "warning", "PE 有效样本为 0，输出 NA")
  }

  top10_share <- NA_real_
  if (!is.na(total_market_cap) && total_market_cap > 0) {
    ranked_cap <- sort(bse$total_market_cap_yi[!is.na(bse$total_market_cap_yi)], decreasing = TRUE)
    top10_share <- sum(utils::head(ranked_cap, 10L), na.rm = TRUE) / total_market_cap
  }

  bse_trading <- board_trading_df[grepl("北证|北交|BJ|BSE", board_trading_df$board, ignore.case = TRUE, perl = TRUE), , drop = FALSE]
  if (nrow(bse_trading) == 0L && nrow(board_trading_df) > 0L) {
    bse_trading <- board_trading_df
  }

  avg_daily_turnover <- NA_real_
  as_of_date <- Sys.Date()
  if (nrow(bse_trading) > 0L) {
    if (any(!is.na(bse_trading$period))) {
      latest_period <- max(bse_trading$period, na.rm = TRUE)
      latest_rows <- bse_trading[bse_trading$period == latest_period, , drop = FALSE]
      as_of_date <- latest_period
    } else {
      latest_rows <- utils::tail(bse_trading, 1L)
    }
    avg_daily_turnover <- latest_rows$avg_daily_turnover_yi[[1L]]
  }

  if (is.na(avg_daily_turnover)) {
    log_data_quality("市场板块成交统计.xlsx", "avg_daily_turnover", "warning", "未取得北交所最新日均成交额")
  } else {
    log_data_quality("市场板块成交统计.xlsx", "avg_daily_turnover", "ok", paste0("北交所日均成交额：", round(avg_daily_turnover, 4), " 亿元"))
  }

  data.frame(
    as_of_date = format(as_of_date, "%Y-%m-%d"),
    listed_company_count = length(company_codes),
    current_year_new_listed_count = length(unique(bse$company_code[listing_year == current_year])),
    previous_year_new_listed_count = length(unique(bse$company_code[listing_year == previous_year])),
    total_market_cap_yi = round(total_market_cap, 4),
    float_market_cap_yi = round(float_market_cap, 4),
    avg_daily_turnover_yi = round(avg_daily_turnover, 4),
    pe_median = round(pe_median, 4),
    top10_market_cap_share = round(top10_share, 6),
    data_source = "上市公司基本情况.xlsx; 市场板块成交统计.xlsx",
    last_update_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )
}

# 用途：基于上市公司数据构建市场定位公司明细表
# 输入来源：data/raw/上市公司基本情况.xlsx
build_market_position_company_detail <- function(company_df) {
  if (!all(c("company_code", "total_market_cap_yi", "float_market_cap_yi", "pe") %in% names(company_df))) {
    company_df <- standardize_company_fields(company_df)
  }

  bse <- filter_bse_companies(company_df)
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  listing_year <- suppressWarnings(as.integer(format(bse$listing_date, "%Y")))

  rank_value <- rank(-bse$total_market_cap_yi, ties.method = "first", na.last = "keep")
  out <- data.frame(
    company_code = bse$company_code,
    company_name = bse$company_name,
    board = bse$board,
    listing_date = ifelse(is.na(bse$listing_date), NA_character_, format(bse$listing_date, "%Y-%m-%d")),
    industry = bse$industry,
    city = if ("city" %in% names(bse)) bse$city else NA_character_,
    total_market_cap_yi = round(bse$total_market_cap_yi, 4),
    float_market_cap_yi = round(bse$float_market_cap_yi, 4),
    pe = round(bse$pe, 4),
    is_current_year_new = !is.na(listing_year) & listing_year == current_year,
    market_cap_rank = as.integer(rank_value),
    is_top10_market_cap = !is.na(rank_value) & rank_value <= 10L,
    stringsAsFactors = FALSE
  )

  out[order(out$market_cap_rank, na.last = TRUE), , drop = FALSE]
}

# 用途：以 UTF-8 编码写入 CSV 文件
# 输入来源：函数输入参数
write_csv_utf8 <- function(df, path) {
  utils::write.csv(df, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

# 用途：执行基础数据处理流程，生成 market_position 相关 processed 表
# 输入来源：data/raw/上市公司基本情况.xlsx、data/raw/市场板块成交统计.xlsx
write_processed_market_position <- function(output_dir = "data/processed",
                                            company_path = "data/raw/上市公司基本情况.xlsx",
                                            board_trading_path = "data/raw/市场板块成交统计.xlsx") {
  reset_data_quality_log()
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  company_raw <- read_basic_company_data(company_path)
  board_trading_raw <- read_board_trading_data(board_trading_path)

  company_std <- standardize_company_fields(company_raw)
  attr(company_std, "source_file") <- company_path
  city_coord_path <- file.path(output_dir, "city_coordinates.csv")
  city_coordinates <- build_city_coordinates(company_std, company_path)
  if (nrow(city_coordinates) > 0L) {
    write_csv_utf8(city_coordinates, city_coord_path)
  } else if (file.exists(city_coord_path)) {
    city_coordinates <- read_city_coordinates(city_coord_path)
    log_data_quality(company_path, "city_coordinates_reuse", "ok", "本批次未提供经纬度，已复用既有城市经纬度信息表")
  } else {
    log_data_quality(company_path, "city_coordinates_reuse", "warning", "本批次未提供经纬度，且未找到既有城市经纬度信息表")
  }
  company_std <- apply_city_coordinates(company_std, city_coordinates, company_path)
  board_trading_std <- standardize_board_trading(board_trading_raw)
  attr(board_trading_std, "source_file") <- board_trading_path

  kpi <- calc_market_position_kpi(company_std, board_trading_std)
  detail <- build_market_position_company_detail(company_std)
  log <- get_data_quality_log()

  kpi_path <- file.path(output_dir, "market_position_kpi.csv")
  detail_path <- file.path(output_dir, "market_position_company_detail.csv")
  log_path <- file.path(output_dir, "data_quality_log.csv")

  write_csv_utf8(kpi, kpi_path)
  write_csv_utf8(detail, detail_path)
  write_csv_utf8(log, log_path)

  expected <- c(kpi_path, detail_path, city_coord_path, log_path)
  for (path in expected) {
    log_data_quality(
      output_dir,
      "processed_file_exists",
      if (file.exists(path)) "ok" else "error",
      paste0(path, if (file.exists(path)) " 已生成" else " 未生成")
    )
  }
  write_csv_utf8(get_data_quality_log(), log_path)

  invisible(list(
    kpi = kpi,
    company_detail = detail,
    city_coordinates = city_coordinates,
    quality_log = get_data_quality_log(),
    files = expected
  ))
}

# 用途：基础数据处理入口函数，执行处理并打印关键指标
# 输入来源：data/raw/上市公司基本情况.xlsx、data/raw/市场板块成交统计.xlsx
process_basic_data <- function() {
  result <- write_processed_market_position()
  kpi <- result$kpi[1, , drop = FALSE]

  cat("基础数据处理完成\n")
  cat("北证股票数量：", kpi$listed_company_count, "\n", sep = "")
  cat("当前年份新增数量：", kpi$current_year_new_listed_count, "\n", sep = "")
  cat("总市值（亿元）：", kpi$total_market_cap_yi, "\n", sep = "")
  cat("流通市值（亿元）：", kpi$float_market_cap_yi, "\n", sep = "")
  cat("日均成交额（亿元）：", kpi$avg_daily_turnover_yi, "\n", sep = "")
  cat("PE 中位数：", kpi$pe_median, "\n", sep = "")
  cat("前十市值占比：", kpi$top10_market_cap_share, "\n", sep = "")
  cat("输出文件：\n")
  cat(paste0(" - ", result$files, collapse = "\n"), "\n", sep = "")

  invisible(result)
}
