# DATA_PROCESSING.md

## 文件定位

Shiny App 原则上不直接读取 `data/raw/` 下的原始 Excel。所有原始数据应先经过数据处理脚本清洗、标准化和校验，再输出到 `data/processed/`，页面、指标函数和图表函数优先消费 processed 结果表。

当前项目处于“真实基础数据 + 标准演示表 + 页面语义层”的过渡阶段。市场定位已优先使用 processed 基础表；公司画像中仍有少量为了地图联动、公司展开详情和股价小图直接读取 raw Excel 的逻辑，后续应迁移到 processed 标准表。

## 当前处理入口

基础真实数据处理入口：

```r
source("R/process_basic_data.R", encoding = "UTF-8")
process_basic_data()
```

标准演示数据补齐入口：

```r
source("R/data_prepare.R", encoding = "UTF-8")
generate_demo_processed_data()
```

应用读取入口：

```r
source("R/data_access.R", encoding = "UTF-8")
load_dashboard_data()
```

## 当前输入数据源

当前 `data/raw/` 中存在以下原始文件：

* `data/raw/上市公司基本情况.xlsx`
* `data/raw/市场板块成交统计.xlsx`
* `data/raw/市场板块成交统计（周度日均）.xlsx`
* `data/raw/时间序列数据.xlsx`
* `data/raw/北证A股近期走势.xlsx`
* `data/raw/北交所日度成交情况2020-2024.xlsx`
* `data/raw/市场交易统计(Wind统计).xlsx`
* `data/raw/全球主要资本市场情况.xlsx`

当前基础处理脚本主要使用：

* `data/raw/上市公司基本情况.xlsx`
* `data/raw/市场板块成交统计.xlsx`

当前部分图表或交互过渡读取：

* `data/raw/市场板块成交统计（周度日均）.xlsx`：用于板块日均成交额趋势。
* `data/raw/时间序列数据.xlsx`：用于指数走势或市场生态相关时间序列。
* `data/raw/北证A股近期走势.xlsx`：用于公司展开详情中的近 30 日股价走势。
* `data/raw/北交所日度成交情况2020-2024.xlsx` 和 `data/raw/市场交易统计(Wind统计).xlsx`：用于市场定位页“北交所交易规模成长”图，当前由图表函数只读合并；后续应迁移到 processed 标准表。
* `data/raw/全球主要资本市场情况.xlsx`：用于市场定位页“全球主要资本市场对比”图，当前由图表函数只读读取；后续应迁移到 processed 标准表。

维护规则：

1. 原始文件只放在 `data/raw/`。
2. 处理脚本和 App 运行时不得覆盖、改写原始文件。
3. 新增 raw 文件后，必须在本文档记录来源、用途和下游输出。

## 当前输出数据表

基础真实数据处理输出：

* `data/processed/market_position_kpi.csv`
* `data/processed/market_position_company_detail.csv`
* `data/processed/data_quality_log.csv`

标准演示表补齐输出：

* `data/processed/dim_company.csv`
* `data/processed/dim_industry.csv`
* `data/processed/fact_market_period.csv`
* `data/processed/fact_financial_period.csv`
* `data/processed/fact_financing.csv`
* `data/processed/fact_fundraising_use.csv`
* `data/processed/fact_supervision.csv`
* `data/processed/fact_risk_tag.csv`
* `data/processed/fact_listing_pipeline.csv`

缓存数据：

* `data/cache/china-cn-all.geo.json`
* `data/cache/china_city_coordinates.csv`
* `data/cache/bse_city_coordinates.csv`

缓存只用于地图和坐标渲染辅助，不作为唯一事实来源。

## App 读取机制

Shiny App 统一通过 `R/data_access.R` 暴露的数据读取入口消费数据。

当前启动流程：

1. `global.R` source 所有 R 文件。
2. 如 `data/raw/上市公司基本情况.xlsx` 比 `data/processed/market_position_kpi.csv` 更新，`global.R` 会尝试自动运行 `process_basic_data()`。
3. `load_dashboard_data()` 读取 `data/processed/` 下的标准表和辅助表。
4. `build_dashboard_page_models()` 根据 `BSE_PRESENTATION_MODE` 构建页面模型。
5. 页面 module 使用 `dashboard_data` 和 `dashboard_page_models` 渲染。

展示模式：

* `semantic`：默认模式，使用 processed 标准表和 `metrics_*.R` 构建 KPI、洞察和明细。
* `placeholder`：使用 `config/page_blocks.yml` 和固定随机种子生成演示内容。

如果关键 processed 文件缺失或读取失败，应用会尽量回退到 `R/sample_data.R` 或 placeholder 数据，保证 App 可以启动。

## 北证股票识别规则

北证股票按上市板块字段优先识别，兼容以下名称：

* 北证
* 北交所
* 北交所股票
* 北证股票
* BJ
* BSE

如果无法匹配板块字段，脚本会尝试使用证券代码后缀 `.BJ` 或 `.BSE` 作为辅助识别。若筛选结果为 0，必须写入 `data_quality_log.csv`。

## 市场定位 KPI 加工规则

`market_position_kpi.csv` 当前服务于市场定位顶部 KPI 和部分页面洞察，字段口径如下：

* `listed_company_count`：北证股票公司数，按公司代码去重。
* `current_year_new_listed_count`：系统日期所在年份内新增上市的北证股票数，按公司代码去重。
* `previous_year_new_listed_count`：系统日期前一年新增上市的北证股票数，按公司代码去重。
* `total_market_cap_yi`：北证股票总市值合计，统一换算为亿元。
* `float_market_cap_yi`：北证股票流通市值合计，统一换算为亿元。
* `avg_daily_turnover_yi`：市场板块成交统计中北交所最新一期日均成交额，统一换算为亿元。
* `pe_median`：北证股票 PE 中位数，优先使用 `PE_TTM` / `市盈率TTM`，其次使用 `PE` / `市盈率`。如果只能使用 `发行市盈率`，必须在质量日志中标注。
* `top10_market_cap_share`：总市值前 10 公司市值合计 / 北证股票总市值合计，输出小数。

PE 中位数剔除非数值、负值、0 和极端高值，当前上限为 300。

## 输出字段说明

### market_position_kpi.csv

* `as_of_date`：数据口径日期，优先使用成交统计最新日期；无法识别时使用处理日期。
* `listed_company_count`：上市公司家数。
* `current_year_new_listed_count`：当前年份新增上市公司数。
* `previous_year_new_listed_count`：前一年新增上市公司数。
* `total_market_cap_yi`：总市值，单位亿元。
* `float_market_cap_yi`：流通市值，单位亿元。
* `avg_daily_turnover_yi`：日均成交额，单位亿元。
* `pe_median`：PE 中位数。
* `top10_market_cap_share`：前十市值占比，小数。
* `data_source`：原始数据来源文件。
* `last_update_time`：处理完成时间。

### market_position_company_detail.csv

* `company_code`：公司或证券代码。
* `company_name`：公司或证券简称。
* `board`：上市板块。
* `listing_date`：上市日期，输出为 `YYYY-MM-DD`。
* `industry`：行业。
* `city`：原始表“城市”字段；用于公司画像中的城市点位匹配。
* `total_market_cap_yi`：总市值，单位亿元。
* `float_market_cap_yi`：流通市值，单位亿元。
* `pe`：PE 数值。
* `is_current_year_new`：是否当前年份新增上市。
* `market_cap_rank`：按总市值降序排名。
* `is_top10_market_cap`：是否总市值前 10。

### data_quality_log.csv

* `check_time`：检查时间。
* `source_file`：来源文件或输出目录。
* `check_item`：检查项。
* `status`：`ok` / `warning` / `error`。
* `message`：检查说明。

### dim_company.csv

公司维表。`company_code`、`company_name`、`board`、`listing_date`、`industry` 继承自基础公司清单；`city` 优先继承基础公司清单的城市字段。

演示生成字段：

* `province`
* `strategic_sector`
* `is_bse`
* `is_high_tech`
* `is_specialized_new`

字段：

* `company_code`
* `company_name`
* `board`
* `listing_date`
* `province`
* `city`
* `industry`
* `strategic_sector`
* `is_bse`
* `is_high_tech`
* `is_specialized_new`

`city` 用于公司画像的城市点位地图：优先使用已处理公司所在地；城市中心坐标由缓存表匹配，无法匹配时回退到省级中心点并保留数量。

### dim_industry.csv

行业维表，基于公司清单中的行业字段归类生成。

字段：

* `industry`
* `industry_group`
* `strategic_sector`
* `display_order`

其中 `industry_group`、`strategic_sector`、`display_order` 当前为演示生成字段。

### fact_market_period.csv

市场行情期表，用于市场定位、公司画像和市场质量画像。

字段：

* `period`
* `company_code`
* `total_market_cap_yi`
* `float_market_cap_yi`
* `turnover_amount_yi`
* `turnover_rate`
* `pe`
* `pb`

`company_code` 继承自公司清单；市值和 PE 以基础公司清单为锚，按期间生成合理波动；成交额与市值、流通市值和换手率保持正相关。除继承字段外，当前均为演示生成字段。

### fact_financial_period.csv

财务期间表，用于公司画像和市场质量画像。

字段：

* `period`
* `company_code`
* `revenue_yi`
* `net_profit_yi`
* `deduct_net_profit_yi`
* `roe`
* `gross_margin`
* `net_margin`
* `operating_cashflow_yi`
* `r_and_d_expense_yi`
* `r_and_d_ratio`

全部财务字段当前为演示生成字段，生成时保持高科技和专精特新公司研发费用率相对更高，且营收增长与利润质量不完全同步。

### fact_financing.csv

融资事件表，用于市场生态。

字段：

* `event_date`
* `company_code`
* `financing_type`
* `amount_yi`
* `use_type`
* `status`

`company_code` 继承自公司清单；融资类型、金额、用途和状态当前为演示生成字段，金额与公司市值保持一定相关性。

### fact_fundraising_use.csv

募集资金使用表，用于公司画像和质量画像。

字段：

* `period`
* `company_code`
* `raised_amount_yi`
* `used_amount_yi`
* `use_progress`
* `project_status`
* `is_delayed`
* `is_changed`
* `cash_management_balance_yi`
* `benefit_realization_rate`

全部进度、项目状态和效益字段当前为演示生成字段；使用进度偏低的公司更容易出现延期、变更或募资风险标签。

### fact_supervision.csv

监管事项表，用于市场质量画像。

字段：

* `event_date`
* `company_code`
* `event_type`
* `severity`
* `description`
* `status`

监管事项当前为演示生成字段，优先从已有风险标签公司中抽样生成。

### fact_risk_tag.csv

风险标签表，用于市场质量画像。

字段：

* `period`
* `company_code`
* `risk_type`
* `risk_level`
* `risk_reason`

风险标签当前为演示生成字段，生成逻辑考虑低流动性、高估值低增长、盈利或现金流承压、募投进度偏低以及监管事项。

### fact_listing_pipeline.csv

上市审核储备表，用于市场生态。该表为候选企业演示数据，不对应当前已上市公司清单。

字段：

* `period`
* `company_code`
* `pipeline_stage`
* `industry`
* `province`
* `sponsor`
* `days_in_review`

## 数据质量处理规则

### 异常值识别

以下值不得作为有效真实数据使用：

* `#NAME?`
* `#VALUE!`
* `#N/A`
* 空字符串
* `NA`
* `N/A`
* `--`
* `-`

检测到 Wind 公式未刷新导致的异常值时，必须写入质量日志。

### 金额单位转换

金额字段最终统一输出为亿元。脚本支持显式单位：

* 万亿：乘以 10000
* 亿元 / 亿：保持不变
* 万元 / 万：除以 10000
* 元：除以 100000000

如果金额字段没有显式单位，脚本会根据数值量级推断单位，并在质量日志中记录推断结果。

### 比例字段转换

比例字段统一输出为小数。例如 `18.6%` 输出为 `0.186`。当前 `top10_market_cap_share` 由脚本计算生成。

### 日期字段转换

日期字段统一转换为 `Date`，输出 CSV 时使用 `YYYY-MM-DD`。脚本兼容 Excel 日期序列号和常见文本日期格式。

### 缺失字段处理

字段名允许轻微差异，脚本使用候选字段匹配。关键字段缺失时不应导致整个任务崩溃，输出字段使用 `NA`，并写入 `data_quality_log.csv`。

### Wind 公式未刷新处理

`#NAME?`、`#VALUE!`、`#N/A` 等 Wind 或 Excel 公式异常不得参与指标计算。质量日志必须记录异常数量和类型。

## 后续收敛方向

1. 将公司画像地图、公司明细、展开详情和股价走势所需字段统一输出到 processed。
2. 为 `时间序列数据.xlsx` 和 `市场板块成交统计（周度日均）.xlsx` 建立标准处理表。
3. 为风险阈值和风险标签生成逻辑补充 `config/thresholds.yml` 对应字段。
4. 增加 `data/processed/data_manifest.yml`，记录数据来源、批次、处理脚本和校验结果。
5. 对所有演示生成字段增加更明确的字段级标记。

## 维护原则

1. 原始数据只放在 `data/raw/`，不覆盖、不直接改写。
2. 处理结果只输出到 `data/processed/`。
3. App 和页面优先读取 `R/data_access.R` 暴露的标准入口。
4. 每次新增输出表、字段、指标口径或拟合逻辑，必须同步更新本文档。
5. 如果字段由模拟、估算或拟合生成，必须在字段说明或质量日志中标注。
6. 每次运行数据处理后，应检查 `data/processed/` 下是否生成预期文件。
