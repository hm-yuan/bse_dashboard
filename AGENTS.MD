# AGENTS.md

## 项目名称

BSE-Dashboard

## 项目目标

本项目用于搭建一个面向对外展示和内部演示的交易所画像 dashboard。

第一版目标是形成高质量、可讲述、可交互的 R Shiny 原型，不追求完整生产级业务系统。

Dashboard 的主线是：

看定位 → 看公司 → 看发展 → 看质量。

## Codex 工作规则

在开始较大开发任务前，请先阅读：

* PROJECT_BRIEF.md：理解项目背景、页面逻辑和指标体系
* ROADMAP.md：理解当前开发阶段和任务优先级
* UI_STYLE_GUIDE.md：理解视觉风格、布局规则和组件规范；进行 UI 修改前必须阅读 UI_STYLE_GUIDE.md，并严格遵守其中的 Fluent Light + Carbon Edge + BSE Brand 视觉规范和 Ant Design Pro 页面组织方式

## 技术路线

第一版采用 R Shiny 工程化架构。

主要技术栈：

* R Shiny
* bslib
* htmltools
* highcharter（主图优先）
* reactable 或 DT
* tidyverse
* data.table
* duckdb
* arrow / parquet
* yaml
* Quarto，可选，用于后续报告导出

不要擅自切换到 React、Next.js、FastAPI 等架构。
如果确需引入新技术，必须先说明原因和影响。

## 项目结构要求

不要采用单文件 app.R 写法。当前项目已采用模块化 Shiny 骨架，后续开发应在现有结构上增量扩展，不要把页面、指标、图表和数据逻辑重新堆回 app.R。

当前结构：

```text
bse_dashboard/
├─ app.R                         # Shiny 应用入口，只负责装配主题、导航、UI 和 server
├─ global.R                      # 全局依赖、模块 source、主题对象、演示数据加载
├─ AGENTS.md                     # Codex 协作和工程约定
├─ PROJECT_BRIEF.md              # 项目背景、页面叙事和指标体系
├─ ROADMAP.md                    # 阶段路线图
├─ UI_STYLE_GUIDE.md             # Fluent Light + Carbon Edge + BSE Brand 视觉规范
├─ README.md                     # 安装依赖和运行方式
├─ docs/
│  └─ DATA_PROCESSING.md         # 数据处理口径、字段、质量日志和维护规则
├─ config/
│  ├─ indicators.yml             # 页面指标清单和后续指标配置入口
│  ├─ thresholds.yml             # 风险、质量、状态阈值配置
│  └─ theme.yml                  # 主题 token 配置参考
├─ R/
│  ├─ data_access.R              # 数据读取统一入口，当前调用演示数据，后续切换真实数据
│  ├─ process_basic_data.R       # 基础真实数据处理入口，生成 processed 标准表和质量日志
│  ├─ sample_data.R              # 最小可运行模拟数据
│  ├─ ui_components.R            # 通用 UI 组件，如 KPI、Chart、Insight、Detail、Badge
│  ├─ mod_home.R                 # 首页总览 Shiny module
│  ├─ mod_market_position.R      # 市场定位 Shiny module
│  ├─ mod_company_profile.R      # 公司画像 Shiny module
│  ├─ mod_market_development.R   # 市场生态 Shiny module
│  └─ mod_market_quality.R       # 市场质量画像 Shiny module
├─ data/
│  ├─ raw/                       # 原始真实数据落地区，保持只读和可追溯
│  ├─ processed/                 # 清洗后的标准化数据，建议 parquet/duckdb
│  └─ cache/                     # 可再生成的缓存数据，不作为事实来源
├─ www/
│  ├─ custom.css                 # 全局样式，所有通用视觉优先写在这里
│  └─ logo_bse.png               # 北京证券交易所 LOGO
└─ reports/                      # 后续报告导出模板和结果
```

后续真实数据、指标和图表接入时，可按需新增以下文件，但不要提前空转式过度工程化：

```text
R/data_prepare.R
R/process_basic_data.R
R/metrics_market.R
R/metrics_company.R
R/metrics_development.R
R/metrics_quality.R
R/chart_market.R
R/chart_company.R
R/chart_development.R
R/chart_quality.R
reports/export_template.qmd
```

## 代码组织规则

1. `app.R` 只负责应用装配：主题、导航、页面 module UI 和 server 注册；不要在 `app.R` 中写业务指标、图表绘制或数据清洗逻辑。
2. `global.R` 负责加载依赖、source R 文件、创建主题对象和初始化数据对象；不要在 `global.R` 中堆复杂业务计算。
3. 页面 UI 和 server 逻辑写入对应的 Shiny module：`R/mod_home.R`、`R/mod_market_position.R`、`R/mod_company_profile.R`、`R/mod_market_development.R`、`R/mod_market_quality.R`。
4. 页面 module 只负责页面布局、组件调用和轻量交互；不要在页面 module 中直接写复杂指标计算。
5. 通用 UI 组件写入 `R/ui_components.R`。KPI Card、Chart Card、Insight Card、Detail Card、Status Badge、布局容器等可复用组件优先在这里封装。
6. 全局样式集中写入 `www/custom.css`。不要在 R 文件中散落大量 `style = ""`；少量局部差异通过 class 控制。
7. UI 修改前必须阅读 `UI_STYLE_GUIDE.md`，并遵守 Fluent Light + Carbon Edge + BSE Brand 视觉规范。导航、卡片、表格、状态标签等通用样式应优先复用既有 class。
8. 模拟数据生成和演示数据结构写入 `R/sample_data.R`，用于保证 app 在无真实数据时可运行。
9. 数据读取统一通过 `R/data_access.R` 暴露入口，例如 `load_dashboard_data()`；页面模块不得直接读取 `data/` 目录或硬编码文件路径。
10. 后续如接入真实数据，清洗、标准化和衍生字段处理写入 `R/data_prepare.R` 或独立处理脚本，例如 `R/process_basic_data.R`；不要混入页面 module 或图表函数。
11. 指标计算写入 `R/metrics_*.R`，按画像主题拆分：market、company、development、quality。指标函数应输入标准化数据，输出页面可直接消费的结构化结果。
12. 图表函数写入 `R/chart_*.R`，按画像主题拆分。图表函数只负责可视化表达，不负责读取原始数据或清洗数据；主图优先使用 `highcharter`，仅在运行环境缺少该包时回退到简化数据表，避免页面中断。
13. 风险阈值、状态阈值和口径边界写入 `config/thresholds.yml`，不要散落在 R 代码中。
14. 指标清单、指标命名、页面指标映射写入 `config/indicators.yml`，保持各页面指标名称一致。
15. 主题颜色、圆角、阴影、字体等设计 token 优先同步维护在 `www/custom.css` 和 `config/theme.yml`；CSS 是实际生效来源，YAML 作为配置参考和后续主题化入口。
16. 新增依赖必须同步更新 `README.md`，并说明用途；不要为了单个视觉或占位效果引入重型依赖。

## 真实数据维护规则

真实数据接入应遵循“原始数据可追溯、处理数据标准化、页面只读标准数据”的原则。

1. `data/raw/` 保存原始真实数据，按来源和日期分目录维护，例如：

```text
data/raw/{source}/{YYYYMMDD}/...
```

原始数据不在应用运行时修改；如需更新，新增批次目录，不覆盖旧批次。

2. `data/processed/` 保存清洗后的标准数据，推荐使用 parquet 或 duckdb。后续建议逐步形成以下标准表：

```text
dim_company
dim_industry
fact_market_daily
fact_financial_period
fact_financing
fact_fundraising_use
fact_supervision
fact_risk_tag
fact_listing_pipeline
```

3. `data/cache/` 仅保存可再生成的中间缓存、预聚合结果或图表缓存。缓存不能作为唯一事实来源。
4. 真实数据导入流程应优先写成可重复运行的函数或脚本，入口放在 `R/data_prepare.R` 或后续 `scripts/` 目录中。
5. `R/data_access.R` 是应用读取数据的唯一入口。未来可以让 `load_dashboard_data(use_demo = FALSE)` 从 `data/processed/` 读取真实数据；演示模式继续从 `R/sample_data.R` 提供最小数据。
6. 页面模块和 UI 组件不得直接依赖真实数据文件格式；真实数据变化应通过 `data_access`、`data_prepare` 和 `metrics_*` 层吸收。
7. 每次更新真实数据后，应记录数据来源、批次日期、处理时间、处理脚本版本和关键校验结果。建议后续增加：

```text
data/processed/data_manifest.yml
```

8. 真实数据质量校验至少应覆盖：主键唯一性、日期范围、核心字段缺失率、金额/比例字段类型、指标口径范围、异常值数量。
9. 不要把敏感源数据、账号、密钥、接口 token 写入仓库；密钥使用环境变量或本地配置，并避免提交。

## 数据处理规则

进行任何数据导入、清洗、加工、字段调整或 processed 表生成任务前，必须先阅读 `docs/DATA_PROCESSING.md`，并保持处理口径、字段说明和质量日志规则同步。

1. 任何数据处理逻辑不得写入 `app.R`。
2. Shiny 页面不得直接读取 `data/raw/` 下的原始 Excel。
3. Shiny 页面和指标函数应优先读取 `data/processed/` 下的标准化结果表，或通过 `R/data_access.R` 暴露的读取入口获取数据。
4. 原始数据只放在 `data/raw/`，不得在应用运行或处理脚本中覆盖、改写原文件。
5. 处理结果统一输出到 `data/processed/`。
6. 数据处理脚本应放在 `R/` 目录下，例如 `R/process_basic_data.R`。
7. 每次新增输出表、字段、指标口径或拟合逻辑，必须同步更新 `docs/DATA_PROCESSING.md`。
8. Wind 公式未刷新形成的 `#NAME?`、`#VALUE!`、`#N/A` 等异常值不得作为真实数据使用。
9. 每次数据处理应生成或更新数据质量日志，例如 `data/processed/data_quality_log.csv`。
10. 如果字段由模拟、估算或拟合生成，必须在字段说明或质量日志中标注。
11. 完成数据处理相关任务后，应检查 `data/processed/` 下是否生成预期文件。

## 页面结构规则

Dashboard 包含五个页面：

1. 首页总览
2. 市场定位
3. 公司画像
4. 市场生态
5. 市场质量画像

每个画像页面采用：

* 一句话核心判断
* 5-6 个 KPI 卡片
* 1-2 张主图
* 关键洞察说明区
* 明细钻取表

不要把页面做成大量指标和图表堆砌。

## 四个画像页面主图

### 市场定位

核心问题：本交易所在资本市场中处于什么位置？

主图：

* 多市场定位气泡图
* 本所行业市值结构图

### 公司画像

核心问题：本交易所上市公司是什么样的企业？

主图：

* 行业结构与经营贡献矩阵
* 成长性 × 盈利能力四象限图

### 市场生态

核心问题：本交易所的业务功能是否在持续增强？

主图：

* 上市—融资发展趋势图
* 交易活跃与市场生态趋势图

### 市场质量画像

核心问题：本交易所市场运行是否健康、风险是否可控？

主图：

* 市场质量状态矩阵
* 风险类型—行业热力图

## 开发原则

1. 优先保证 app 可以运行。
2. 优先完成可演示原型，再逐步接入真实数据。
3. 每次修改后检查明显运行错误。
4. 不要过度工程化。
5. 不要在 app.R 中堆叠业务逻辑。
6. 不要在 server 中直接写复杂指标计算。
7. 不要让各页面 UI 风格割裂。
8. 页面布局可以差异化，但必须遵守统一视觉系统。
9. 图表和表格应服务于讲述，不要追求数量。
10. 如遇实现复杂度较高的问题，先做稳定简化版。

## 运行方式

优先使用：

shiny::runApp()

如新增依赖，请同步更新 README.md。
