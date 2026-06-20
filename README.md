# BSE-Dashboard

面向对外展示和内部演示的交易所画像 Dashboard 原型。第一阶段目标是建立稳定、可维护、可运行的 R Shiny 工程骨架。

## 技术栈

- R Shiny
- bslib
- htmltools（随 Shiny 使用）
- highcharter（主图交互图表；缺失时图表函数会回退为简化表格或说明区）

## 视觉风格

当前界面采用 **Fluent Light + Carbon Edge + BSE Brand**：浅灰蓝页面背景、近白商务导航、BSE 蓝色强调线、12px 边框卡片、系统无衬线字体和紧凑的 8px 间距体系。四个画像页面共享 KPI、图表、洞察和明细表组件，同时分别强调对标分析、编辑叙事、时间推进和风险驾驶舱的阅读重点。

## 项目结构

```text
bse_dashboard/
├─ app.R
├─ global.R
├─ config/
│  ├─ indicators.yml
│  ├─ thresholds.yml
│  └─ theme.yml
├─ R/
│  ├─ data_access.R
│  ├─ data_prepare.R
│  ├─ process_basic_data.R
│  ├─ sample_data.R
│  ├─ ui_components.R
│  ├─ mod_home.R
│  ├─ mod_market_position.R
│  ├─ mod_company_profile.R
│  ├─ mod_market_development.R
│  └─ mod_market_quality.R
├─ data/
│  ├─ raw/
│  ├─ processed/
│  └─ cache/
├─ www/
│  └─ custom.css
└─ reports/
```

## 安装依赖

在 R 控制台中执行：

```r
install.packages(c("shiny", "bslib", "highcharter"))
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

## 重新生成 processed 演示数据

如需根据现有公司清单补齐后续页面使用的标准演示表，在项目根目录的 R 控制台执行：

```r
source("R/data_prepare.R", encoding = "UTF-8")
generate_demo_processed_data()
```

该入口优先使用 `data/processed/market_position_company_detail.csv` 作为公司基础清单；如果该文件不存在，会回退到 `R/sample_data.R` 构造最小演示公司清单。

生成结果写入 `data/processed/`，包括：

- `dim_company.csv`
- `dim_industry.csv`
- `fact_market_period.csv`
- `fact_financial_period.csv`
- `fact_financing.csv`
- `fact_fundraising_use.csv`
- `fact_supervision.csv`
- `fact_risk_tag.csv`
- `fact_listing_pipeline.csv`

每次生成后会同步更新 `data/processed/data_quality_log.csv`。

## 当前阶段说明

- 已实现五个页面导航：首页总览、市场定位画像、上市公司画像、市场发展画像、市场质量画像。
- 每个页面包含页面标题、一句话核心判断、KPI 卡片区、主图区、关键洞察区和明细表区。
- 当前 Shiny 数据统一通过 `R/data_access.R` 读取，不在页面 module 中直接读取原始文件。
- 原始真实数据保存在 `data/raw/`，只作为数据处理输入，不由 Shiny 页面直接消费。
- 清洗和标准化后的处理结果保存在 `data/processed/`。当前市场定位画像优先读取：
  - `data/processed/market_position_kpi.csv`
  - `data/processed/market_position_company_detail.csv`
  - `data/processed/data_quality_log.csv`
- 后续画像页面所需的标准演示表也保存在 `data/processed/`，由 `R/data_prepare.R` 生成。
- 如果关键 processed 文件缺失或读取失败，`load_dashboard_data()` 会给出 warning，并回退到 `R/sample_data.R` 的演示数据，保证应用仍可启动。
