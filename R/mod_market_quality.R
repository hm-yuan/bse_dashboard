# 用途：市场质量画像 UI 模块函数，渲染该画像页面。
# 输入来源：`id` 参数（Shiny 模块命名空间）。
marketQualityUI <- function(id) page_model_ui("market_quality")

# 用途：市场质量画像服务端模块函数（当前无额外交互逻辑）。
# 输入来源：`id` 参数（Shiny 模块命名空间）。
marketQualityServer <- function(id) {
  moduleServer(id, function(input, output, session) {})
}
