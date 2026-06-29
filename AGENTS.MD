# AGENTS.md

## 项目名称

BSE-Dashboard

## 项目目标

本项目用于搭建一个面向对外展示和内部演示的交易所画像 Dashboard。

当前目标不是完整生产级业务系统，而是形成高质量、可讲述、可交互、可继续接入真实数据的 R Shiny 原型。

Dashboard 的主线是：

看定位 -> 看公司 -> 看发展 -> 看质量。

当前 App 已挂载四个画像页面：

1. 市场定位
2. 公司画像
3. 市场生态
4. 市场质量画像

`config/page_blocks.yml` 和页面模型中保留了“首页总览”定义，但 `app.R` 当前未挂载首页导航。后续如恢复首页，应新增或恢复 `R/mod_home.R`，并在 `app.R` 中注册首页 UI 和 server。

## Codex 工作规则

在开始较大开发任务前，请先阅读：

* `PROJECT_BRIEF.MD`：理解当前页面叙事、已实现图表和后续页面边界。
* `ROADMAP.MD`：理解当前开发阶段和任务优先级。
* `UI_STYLE_GUIDE.MD`：理解视觉风格、布局规则和组件规范；进行 UI 修改前必须阅读该文件，并严格遵守“现代蓝色主基调 + 瑞士国际主义数据分析风格”的唯一视觉规范。

进行任何数据导入、清洗、字段调整或 processed 表生成任务前，必须先阅读：

* `docs/DATA_PROCESSING.md`

## 技术路线

第一版采用 R Shiny 工程化架构，不要擅自切换到 React、Next.js、FastAPI 等架构。

当前主要技术栈：

* R Shiny
* bslib
* htmltools
* highcharter
* reactable
* readxl
* yaml
* jsonlite
* htmlwidgets

后续可按需引入：

* tidyverse / data.table：用于更复杂的数据加工。
* duckdb / arrow / parquet：用于更大规模 processed 数据。
* Quarto：用于后续报告导出。

新增依赖必须同步更新 `README.md`，并说明用途。不要为了单个视觉或占位效果引入重型依赖。

## 当前项目结构

不要采用单文件 `app.R` 写法。当前项目已采用模块化 Shiny 骨架，后续开发应在现有结构上增量扩展，不要把页面、指标、图表和数据逻辑重新堆回 `app.R`。

```text
bse_dashboard/
├─ app.R                         # Shiny 应用入口；当前装配四个画像页导航
├─ global.R                      # source 依赖、主题、数据对象和页面模型初始化
├─ AGENTS.MD                     # Codex 协作和工程约定
├─ PROJECT_BRIEF.MD              # 项目背景、页面叙事和当前实现说明
├─ ROADMAP.MD                    # 阶段路线图
├─ UI_STYLE_GUIDE.MD             # 现代蓝色主基调 + 瑞士国际主义数据分析风格视觉规范
├─ README.md                     # 安装依赖、运行方式和当前功能说明
├─ docs/
│  └─ DATA_PROCESSING.md         # 数据处理口径、字段、质量日志和维护规则
├─ config/
│  ├─ indicators.yml             # 页面指标清单
│  ├─ page_blocks.yml            # 页面模型、KPI、主图和表格配置
│  ├─ thresholds.yml             # 风险、质量、状态阈值配置
│  └─ theme.yml                  # 主题 token 配置参考
├─ R/
│  ├─ data_access.R              # processed 数据读取统一入口
│  ├─ data_prepare.R             # 标准演示数据补齐入口
│  ├─ process_basic_data.R       # 基础真实数据处理入口；生成市场定位基础表和城市坐标表
│  ├─ sample_data.R              # 最小可运行模拟数据
│  ├─ placeholder_data.R         # placeholder 页面模型
│  ├─ semantic_data.R            # semantic 页面模型
│  ├─ ui_components.R            # 通用 UI 组件
│  ├─ metrics_market.R
│  ├─ metrics_company.R
│  ├─ metrics_development.R
│  ├─ metrics_quality.R
│  ├─ chart_market.R
│  ├─ chart_company.R
│  ├─ chart_development.R
│  ├─ chart_quality.R
│  ├─ placeholder_charts.R
│  ├─ mod_market_position.R
│  ├─ mod_company_profile.R
│  ├─ mod_market_development.R
│  └─ mod_market_quality.R
├─ data/
│  ├─ raw/                       # 原始真实数据落地区，保持只读和可追溯
│  ├─ processed/                 # 清洗后的标准化 CSV
│  └─ cache/                     # 可再生成的地图与坐标缓存
├─ www/
│  ├─ custom.css                 # 全局样式
│  ├─ logo_bse.png               # 北京证券交易所 LOGO
│  ├─ force_chart_width.js       # 图表宽度修正
│  ├─ kpi_flip.js                # KPI 翻牌动画
│  └─ stock_mini_chart.js        # 公司详情小型走势辅助脚本
└─ reports/                      # 后续报告导出模板和结果
```

## 代码组织规则

1. `app.R` 只负责应用装配：主题、导航、页面 module UI 和 server 注册；不要在 `app.R` 中写业务指标、图表绘制或数据清洗逻辑。
2. `global.R` 负责加载依赖、source R 文件、创建主题对象、初始化 `dashboard_data` 和 `dashboard_page_models`；不要在 `global.R` 中堆复杂业务计算。
3. 页面 UI 和 server 逻辑写入对应的 Shiny module：`R/mod_market_position.R`、`R/mod_company_profile.R`、`R/mod_market_development.R`、`R/mod_market_quality.R`。如恢复首页，再新增或恢复 `R/mod_home.R`。
4. 页面 module 只负责页面布局、组件调用和轻量交互；不要在页面 module 中直接写复杂指标计算。
5. 通用 UI 组件写入 `R/ui_components.R`。KPI Card、Chart Card、Insight Card、Detail Card、Status Badge、布局容器和 `page_model_ui()` 等可复用组件优先在这里封装。
6. 全局样式集中写入 `www/custom.css`。不要在 R 文件中散落大量 `style = ""`；少量局部差异通过 class 控制。
7. UI 修改前必须阅读 `UI_STYLE_GUIDE.MD`，并遵守其中的现代蓝色主基调、瑞士国际主义网格、强字体层级、hairline 细线和克制蓝色锚点规范。`UI_STYLE_GUIDE.MD` 是唯一视觉规范文件，不新增 `DESIGN_TARGET.md`，除非用户明确要求。
8. 页面内容结构优先维护在 `config/page_blocks.yml`；指标名称同步维护在 `config/indicators.yml`。
9. 应用级标准数据读取统一通过 `R/data_access.R` 暴露入口，例如 `load_dashboard_data()`；页面模块不得绕过该入口直接消费 `data/processed/`。当前仍存在若干过渡性 raw 只读逻辑：市场定位页部分图表函数会读取 `data/raw/上市公司基本情况.xlsx`、`data/raw/市场板块成交统计.xlsx`、`data/raw/北交所日度成交情况2020-2024.xlsx`、`data/raw/市场交易统计(Wind统计).xlsx` 和 `data/raw/全球主要资本市场情况.xlsx`；公司画像会读取 `data/raw/上市公司基本情况.xlsx` 和 `data/raw/北证A股近期走势.xlsx` 用于联动明细与股价小图。后续应逐步迁移到 processed 和指标层。
10. 清洗、标准化和衍生字段处理写入 `R/data_prepare.R` 或 `R/process_basic_data.R`；不要混入页面 module 或图表函数。
11. 指标计算写入 `R/metrics_*.R`，按画像主题拆分：market、company、development、quality。指标函数应输入标准化数据，输出页面可直接消费的结构化结果。
12. 图表函数写入 `R/chart_*.R`，按画像主题拆分。图表函数只负责可视化表达，不负责清洗原始数据；如必须临时读取 Excel，应在函数注释和文档中标明，并优先安排后续迁移。
13. 风险阈值、状态阈值和口径边界写入 `config/thresholds.yml`，不要散落在 R 代码中。
14. 主题颜色、圆角、阴影、字体等设计 token 优先同步维护在 `www/custom.css` 和 `config/theme.yml`；CSS 是实际生效来源，YAML 作为配置参考和后续主题化入口。

## 当前页面和图表边界

### 市场定位

当前页面是项目中图表最丰富的页面，已不只是两张主图。实际布局以页面模型 KPI 为基础，额外挂载多个定制组合卡片。

已实现重点：

* 页面模型中的多市场规模气泡图作为基础能力保留。
* “北交所上市公司数量与交易成长”组合图：历年上市公司数量 + 交易规模成长，并支持日度/月度/年度和成交额/换手率/成交占比切换。
* “估值、股权性质与市值分布”右侧组合卡：市值与市盈率散点图 + 企业所有权性质分布 + 上市公司市值区间分布。
* “全球主要资本市场对比”三标签组合卡：全球主要资本市场横向对比、层层递进的市场结构、服务创新型中小企业。
* 市值与市盈率散点图。
* 行业分布、行业矩形树图、专精特新数量和研发强度占比等可复用图表函数。

维护原则：

* 仍以“市场定位”为叙事核心，不继续无限堆图。
* 新增图表应优先合并到已有组合卡片或放入明细区。
* 与成交、行业、市值、PE、全球市场对比和北交所特色指标相关的新口径优先进入 `metrics_market.R` 和 `chart_market.R`，并优先安排迁移到 processed 标准表。

### 公司画像

当前页面已形成“地理分布 + 经营明细 + 经营状态 + 行业结构”的交互式画像。早期页面模型中的行业矩阵和四象限图仍作为通用能力保留，但当前公司画像模块实际主要使用地图联动卡和底部行业结构组件。

已实现重点：

* 中国省级地图和城市气泡，支持点击省份或城市筛选，并支持复位。
* 地图下方联动收入与利润散点图、经营状态环图。
* 右侧 reactable 公司明细表，支持搜索、分页和展开详情。
* 展开详情中展示公司基本情况、经营业绩和近 30 日股价走势。
* 各市场行业分布和行业矩形树图复用市场定位图表能力。

维护原则：

* 地图、公司表和展开详情应优先读取 processed 标准字段；当前财务表、筛选联动和股价小图读取 raw Excel 的逻辑属于过渡实现。
* 公司画像的新增分析应围绕“公司群体特征”，不要变成单家公司档案系统。

### 市场生态

当前页面主要通过 `page_model_ui("market_development")` 渲染。

主图：

* 上市与融资发展趋势。
* 交易活跃与市场生态趋势。

维护原则：

* 优先补强时间序列和关键事件解释。
* 后续交互可围绕日期、阶段、融资类型、审核状态展开。

### 市场质量画像

当前页面主要通过 `page_model_ui("market_quality")` 渲染。

主图：

* 市场质量状态矩阵。
* 风险类型-行业热力图。

维护原则：

* 风险色保持克制。
* 风险阈值优先进入 `config/thresholds.yml`。
* 公司明细重点体现“公司-风险类型-风险等级-处置状态”。

## 展示模式

当前页面模型支持两种模式：

* `semantic`：默认模式，从 processed 标准表和指标函数构建页面 KPI、洞察和明细。
* `placeholder`：演示占位模式，由 `config/page_blocks.yml` 和固定随机种子生成页面内容。

切换方式：

```r
Sys.setenv(BSE_PRESENTATION_MODE = "placeholder")
shiny::runApp()
```

或：

```r
Sys.setenv(BSE_PRESENTATION_MODE = "semantic")
shiny::runApp()
```

## 数据维护规则

真实数据接入应遵循“原始数据可追溯、处理数据标准化、页面只读标准数据”的原则。

1. `data/raw/` 保存原始真实数据。原始数据不在应用运行时修改；如需更新，新增批次或明确替换来源，不在处理脚本中覆盖改写。
2. `data/processed/` 保存清洗后的标准数据。当前以 UTF-8 CSV 为主，已包含 `city_coordinates.csv` 作为城市经纬度的项目级事实来源。
3. `data/cache/` 仅保存可再生成的地图、坐标或中间缓存。缓存不能作为唯一事实来源。
4. `R/data_access.R` 是应用读取数据的统一入口。页面模块原则上不得绕过该入口读取文件。
5. 每次更新真实数据后，应记录数据来源、处理时间、处理脚本版本和关键校验结果，至少更新 `data/processed/data_quality_log.csv`。
6. 真实数据质量校验至少应覆盖：主键唯一性、日期范围、核心字段缺失率、金额/比例字段类型、指标口径范围、异常值数量。
7. 不要把敏感源数据、账号、密钥、接口 token 写入仓库；密钥使用环境变量或本地配置，并避免提交。

## 数据处理规则

进行任何数据导入、清洗、加工、字段调整或 processed 表生成任务前，必须先阅读 `docs/DATA_PROCESSING.md`，并保持处理口径、字段说明和质量日志规则同步。

1. 任何数据处理逻辑不得写入 `app.R`。
2. Shiny 页面原则上不得直接读取 `data/raw/` 下的原始 Excel；当前市场定位和公司画像中的 raw 只读逻辑属于过渡实现，修改时应优先迁移。
3. Shiny 页面和指标函数应优先读取 `data/processed/` 下的标准化结果表，或通过 `R/data_access.R` 暴露的读取入口获取数据。
4. 原始数据只放在 `data/raw/`，不得在应用运行或处理脚本中覆盖、改写原文件。
5. 处理结果统一输出到 `data/processed/`。
6. 数据处理脚本应放在 `R/` 目录下，例如 `R/process_basic_data.R`、`R/data_prepare.R`。
7. 每次新增输出表、字段、指标口径或拟合逻辑，必须同步更新 `docs/DATA_PROCESSING.md`。
8. Wind 公式未刷新形成的 `#NAME?`、`#VALUE!`、`#N/A` 等异常值不得作为真实数据使用。
9. 每次数据处理应生成或更新数据质量日志，例如 `data/processed/data_quality_log.csv`。
10. 如果字段由模拟、估算或拟合生成，必须在字段说明或质量日志中标注。
11. 完成数据处理相关任务后，应检查 `data/processed/` 下是否生成预期文件。

## 页面结构规则

当前 App 以四个画像页为主。每个画像页面原则上采用：

* 一句话核心判断
* 5-6 个 KPI 卡片
* 1-2 个核心主图或组合主图区域
* 关键洞察说明区
* 明细钻取表或交互明细组件

市场定位和公司画像已经根据演示需要扩展了组合图和联动组件，但新增内容仍必须服务页面核心问题，不要做成图表堆砌页。

## 开发原则

1. 优先保证 App 可以运行。
2. 优先完成可演示原型，再逐步接入真实数据。
3. 每次修改后检查明显运行错误。
4. 不要过度工程化。
5. 不要在 `app.R` 中堆叠业务逻辑。
6. 不要在 server 中直接写复杂指标计算。
7. 不要让各页面 UI 风格割裂。
8. 页面布局可以差异化，但必须遵守统一视觉系统。
9. 图表和表格应服务于讲述，不要追求数量。
10. 如遇实现复杂度较高的问题，先做稳定简化版。

## 运行方式

优先使用：

```r
shiny::runApp()
```

如新增依赖，请同步更新 `README.md`。
