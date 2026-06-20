# ROADMAP.md

## Phase 1：项目骨架

* [ ] 创建 R Shiny 工程目录
* [ ] 创建 app.R 和 global.R
* [ ] 创建 R/ 模块目录
* [ ] 创建 config/ 配置目录
* [ ] 创建 www/custom.css
* [ ] 创建 README.md
* [ ] 创建模拟数据脚本
* [ ] 实现五个页面导航
* [ ] 每个页面实现占位布局
* [ ] 确保 shiny::runApp() 可以启动

## Phase 2：模拟数据

* [x] 生成 dim_company
* [x] 生成 dim_industry
* [x] 生成 fact_market_daily / fact_market_period
* [x] 生成 fact_financial_period
* [x] 生成 fact_financing
* [x] 生成 fact_fundraising_use
* [x] 生成 fact_supervision
* [x] 生成 fact_risk_tag
* [x] 生成 fact_listing_pipeline
* [x] 提供 load_demo_data() 函数
* [x] 保存数据到 data/processed/

## Phase 3：指标和主图

* [x] 实现市场定位画像 KPI 和主图
* [x] 实现上市公司画像 KPI 和主图
* [x] 实现市场发展画像 KPI 和主图
* [x] 实现市场质量画像 KPI 和主图
* [x] 将指标计算写入 metrics_*.R
* [x] 将图表函数写入 chart_*.R

## Phase 4：页面 UI 优化

* [x] 新增或完善 R/ui_components.R
* [x] 统一 KPI 卡片
* [x] 统一 Chart Card
* [x] 统一 Insight Card
* [x] 统一明细表样式
* [x] 优化 www/custom.css
* [x] 四个画像页面形成差异化布局
* [x] 落实 Fluent Light + Carbon Edge + BSE Brand 视觉系统

## Phase 5：交互和钻取

* [ ] 增加日期筛选
* [ ] 增加行业筛选
* [ ] 增加地区筛选
* [ ] 增加风险类型筛选
* [ ] 增加公司明细表
* [ ] 增加公司详情弹窗或展开行
* [ ] 增加关键洞察自动生成

## Phase 6：真实数据接入

* [ ] 明确真实数据来源
* [ ] 建立数据导入脚本
* [ ] 替换模拟数据
* [ ] 增加数据质量校验
* [ ] 增加指标口径说明

## Phase 7：报告导出和部署

* [ ] 增加导出报告功能
* [ ] 增加截图或 PDF 输出能力
* [ ] 编写部署说明
* [ ] 整理 README
* [ ] 形成演示版本
