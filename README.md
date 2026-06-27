# BSE-Dashboard

面向对外展示和内部演示的交易所画像 Dashboard 原型。当前版本采用 R Shiny 工程化架构，围绕“看定位 -> 看公司 -> 看发展 -> 看质量”组织四个画像页。

## 当前页面

当前 `app.R` 已挂载四个页面：

1. 市场定位
2. 公司画像
3. 市场生态
4. 市场质量画像

`config/page_blocks.yml` 中保留首页总览配置，但当前未挂载首页导航。

## 技术栈

核心运行依赖：

* R Shiny
* bslib
* htmltools
* highcharter
* reactable
* readxl
* yaml
* jsonlite
* htmlwidgets

当前图表主力为 Highcharter；公司画像明细表使用 reactable；原始 Excel 读取和部分过渡联动逻辑使用 readxl。

## 安装依赖

在 R 控制台中执行：

```r
install.packages(c(
  "shiny",
  "bslib",
  "highcharter",
  "reactable",
  "readxl",
  "yaml",
  "jsonlite",
  "htmlwidgets"
))
```

## 运行应用

在项目根目录执行：

```r
shiny::runApp()
```

或在命令行中执行：

```bash
Rscript -e "shiny::runApp('.', host = '127.0.0.1', port = 3838)"
```

## 展示模式

页面模型支持两种展示模式：

* `semantic`：默认模式，从 processed 标准表和指标函数构建 KPI、洞察、表格与图表数据。
* `placeholder`：占位演示模式，根据 `config/page_blocks.yml` 和固定随机种子生成可复现演示内容。

切换到 placeholder：

```r
Sys.setenv(BSE_PRESENTATION_MODE = "placeholder")
shiny::runApp()
```

切换回 semantic：

```r
Sys.setenv(BSE_PRESENTATION_MODE = "semantic")
shiny::runApp()
```

## 当前功能概览

### 市场定位

已实现的重点图表和交互：

* 多市场规模气泡图
* 板块成交规模与趋势组合图
* 市场规模与市值分布组合图
* 北交所历年上市公司数量与企业所有权性质组合图
* 市值与市盈率散点图
* 行业分布和行业矩形树图
* 板块成交指标切换

### 公司画像

已实现的重点图表和交互：

* 中国省级公司分布地图和城市气泡
* 地图点击筛选与复位
* 营业收入 vs 净利润散点图
* 公司经营状态环图
* reactable 公司明细表
* 公司展开详情和近 30 日股价走势
* 各市场行业分布和行业矩形树图

### 市场生态

当前通过页面模型渲染：

* 上市与融资发展趋势
* 交易活跃与市场生态趋势
* KPI、洞察和发展明细

### 市场质量画像

当前通过页面模型渲染：

* 市场质量状态矩阵
* 风险类型-行业热力图
* 风险 KPI、洞察和重点风险对象明细

## 项目结构

```text
bse_dashboard/
├─ app.R
├─ global.R
├─ AGENTS.MD
├─ PROJECT_BRIEF.MD
├─ ROADMAP.MD
├─ UI_STYLE_GUIDE.MD
├─ README.md
├─ config/
│  ├─ indicators.yml
│  ├─ page_blocks.yml
│  ├─ thresholds.yml
│  └─ theme.yml
├─ R/
│  ├─ data_access.R
│  ├─ data_prepare.R
│  ├─ process_basic_data.R
│  ├─ sample_data.R
│  ├─ placeholder_data.R
│  ├─ semantic_data.R
│  ├─ ui_components.R
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
│  ├─ raw/
│  ├─ processed/
│  └─ cache/
├─ docs/
│  └─ DATA_PROCESSING.md
├─ www/
│  ├─ custom.css
│  ├─ logo_bse.png
│  ├─ force_chart_width.js
│  ├─ kpi_flip.js
│  └─ stock_mini_chart.js
└─ reports/
```

## 重新生成 processed 演示数据

如需根据现有公司清单补齐后续页面使用的标准演示表，在项目根目录的 R 控制台执行：

```r
source("R/data_prepare.R", encoding = "UTF-8")
generate_demo_processed_data()
```

该入口优先使用 `data/processed/market_position_company_detail.csv` 作为公司基础清单；如果该文件不存在，会回退到 `R/sample_data.R` 构造最小演示公司清单。

生成结果写入 `data/processed/`，包括：

* `dim_company.csv`
* `dim_industry.csv`
* `fact_market_period.csv`
* `fact_financial_period.csv`
* `fact_financing.csv`
* `fact_fundraising_use.csv`
* `fact_supervision.csv`
* `fact_risk_tag.csv`
* `fact_listing_pipeline.csv`

每次生成后会同步更新 `data/processed/data_quality_log.csv`。

## 基础真实数据处理

当前基础处理入口：

```r
source("R/process_basic_data.R", encoding = "UTF-8")
process_basic_data()
```

该脚本当前处理：

* `data/raw/上市公司基本情况.xlsx`
* `data/raw/市场板块成交统计.xlsx`

处理结果包括：

* `data/processed/market_position_kpi.csv`
* `data/processed/market_position_company_detail.csv`
* `data/processed/data_quality_log.csv`

## 当前数据读取机制

应用启动时：

1. `global.R` source 全部 R 文件。
2. `load_dashboard_data()` 读取 `data/processed/` 标准表和辅助表。
3. `build_dashboard_page_models()` 根据 `BSE_PRESENTATION_MODE` 构建 `semantic` 或 `placeholder` 页面模型。
4. 各页面 module 使用 `dashboard_data` 和 `dashboard_page_models` 渲染。

如果关键 processed 文件缺失或读取失败，应用会尽量回退到演示数据，保证 Shiny 可以启动。

## 视觉风格

当前界面采用 **Fluent Light + Carbon Edge + BSE Brand**：

* 浅灰蓝页面背景
* 近白商务导航
* BSE 官方 LOGO
* BSE 蓝和青色强调
* 明确边框和标题区分割线
* 克制阴影和 8px 间距体系
* 面向对外展示的金融数据产品风格

UI 修改前请先阅读 `UI_STYLE_GUIDE.MD`。

## 维护注意事项

* 不要把业务逻辑堆进 `app.R`。
* 页面模块只负责布局、组件调用和轻量交互。
* 指标计算写入 `R/metrics_*.R`。
* 图表绘制写入 `R/chart_*.R`。
* 通用组件写入 `R/ui_components.R`。
* 通用样式写入 `www/custom.css`。
* 数据处理任务必须同步更新 `docs/DATA_PROCESSING.md`。
* 当前公司画像中直接读取 raw Excel 的逻辑属于过渡实现，后续应迁移到 processed 标准表。
