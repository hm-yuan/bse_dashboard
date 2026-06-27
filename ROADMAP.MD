# ROADMAP.md

## 当前阶段判断

项目已从“骨架搭建”进入“演示原型打磨 + 数据层收敛”阶段。

当前 App 已挂载四个画像页：

* 市场定位
* 公司画像
* 市场生态
* 市场质量画像

首页总览仍保留在页面模型中，但当前未挂载到 `app.R` 导航。

## Phase 1：项目骨架

* [x] 创建 R Shiny 工程目录
* [x] 创建 `app.R` 和 `global.R`
* [x] 创建 `R/` 模块目录
* [x] 创建 `config/` 配置目录
* [x] 创建 `www/custom.css`
* [x] 创建 `README.md`
* [x] 创建模拟数据脚本
* [x] 实现四个画像页面导航
* [ ] 恢复或新增首页总览导航
* [x] 确保 `shiny::runApp()` 可以启动

## Phase 2：数据层和演示数据

* [x] 生成 `dim_company`
* [x] 生成 `dim_industry`
* [x] 生成 `fact_market_period`
* [x] 生成 `fact_financial_period`
* [x] 生成 `fact_financing`
* [x] 生成 `fact_fundraising_use`
* [x] 生成 `fact_supervision`
* [x] 生成 `fact_risk_tag`
* [x] 生成 `fact_listing_pipeline`
* [x] 提供 `load_demo_data()` 兼容回退
* [x] 提供 `load_dashboard_data()` 统一读取入口
* [x] 保存数据到 `data/processed/`
* [x] 生成或更新 `data_quality_log.csv`
* [ ] 将公司画像中直接读取 raw Excel 的过渡逻辑迁移到 processed 标准表

## Phase 3：指标和主图

* [x] 实现市场定位 KPI 和图表函数
* [x] 实现公司画像 KPI 和图表函数
* [x] 实现市场生态 KPI 和主图
* [x] 实现市场质量画像 KPI 和主图
* [x] 将指标计算写入 `metrics_*.R`
* [x] 将图表函数写入 `chart_*.R`
* [x] 增加 `placeholder` / `semantic` 页面模型
* [x] 增加 `config/page_blocks.yml` 页面块配置
* [ ] 对齐 `config/indicators.yml`、`page_blocks.yml` 和真实指标口径

## Phase 4：页面 UI 优化

* [x] 完善 `R/ui_components.R`
* [x] 统一 KPI 卡片
* [x] 统一 Chart Card
* [x] 统一 Insight Card
* [x] 统一明细表样式
* [x] 优化 `www/custom.css`
* [x] 建立现代蓝色主基调 + 瑞士国际主义数据分析风格视觉系统
* [x] 将视觉规范调整为现代蓝色主基调 + 瑞士国际主义数据分析风格
* [x] 增加 BSE 官方 LOGO 导航
* [x] 增加 KPI 翻牌动画
* [x] 增加图表宽度修正脚本
* [x] 增加公司详情小型走势脚本
* [ ] 检查 1280px 和 1440px 展示宽度下的图表拥挤问题
* [ ] 清理未使用或重复的图表卡片函数

## Phase 5：已实现的重点交互

* [x] 市场定位：板块成交指标切换
* [x] 市场定位：行业树图公司家数/市值切换
* [x] 市场定位：市值与市盈率散点图
* [x] 市场定位：市值区间分布和企业性质分布
* [x] 公司画像：地图点击筛选
* [x] 公司画像：地图复位
* [x] 公司画像：公司明细表搜索、分页、展开详情
* [x] 公司画像：近 30 日股价走势小图
* [ ] 市场生态：日期、阶段或融资类型筛选
* [ ] 市场质量：风险类型、行业或风险等级筛选

## Phase 6：钻取和业务口径收敛

* [ ] 明确每页最终展示 KPI 口径
* [ ] 明确四个画像页主图和辅助图保留清单
* [ ] 将页面中过渡读取 raw Excel 的逻辑移入数据处理层
* [ ] 增加公司详情标准 processed 表或明细视图
* [ ] 增加关键洞察自动生成或半自动生成
* [ ] 将风险阈值完整迁移到 `config/thresholds.yml`
* [ ] 更新 `docs/DATA_PROCESSING.md` 中所有新增字段口径

## Phase 7：真实数据接入

* [x] 接入基础原始文件：上市公司基本情况、市场板块成交统计
* [x] 建立基础处理脚本 `R/process_basic_data.R`
* [x] 建立演示标准表补齐脚本 `R/data_prepare.R`
* [x] 增加基础数据质量日志
* [ ] 明确正式真实数据来源和更新频率
* [ ] 为全部画像页替换模拟字段
* [ ] 增加主键、日期范围、缺失率和异常值校验
* [ ] 增加数据来源 manifest

## Phase 8：报告导出和部署

* [x] 增加基础部署脚本 `deploy.R`
* [ ] 编写正式部署说明
* [ ] 增加截图或 PDF 输出能力
* [ ] 增加 Quarto 报告导出模板
* [ ] 整理对外演示口径
* [ ] 形成可交付演示版本
