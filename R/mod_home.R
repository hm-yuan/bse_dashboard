# 用途：首页 UI 模块函数，渲染首页完整界面。
# 输入来源：`id` 参数（Shiny 模块命名空间）。
homeUI <- function(id) page_model_ui("home")

# 用途：首页服务端模块函数（当前无额外交互逻辑）。
# 输入来源：`id` 参数（Shiny 模块命名空间）。
homeServer <- function(id) {
  moduleServer(id, function(input, output, session) {})
}
