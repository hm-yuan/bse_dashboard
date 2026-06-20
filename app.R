source("global.R", encoding = "UTF-8")

library(shiny)
library(bslib)

ui <- page_navbar(
  title = div(
    class = "app-brand",
    tags$img(
      class = "app-brand-logo",
      src = "logo_bse.png",
      alt = "北京证券交易所"
    )
  ),
  window_title = "交易所画像 Dashboard",
  id = "main_nav",
  theme = app_theme,
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  nav_panel("首页总览", homeUI("home")),
  nav_panel("市场定位画像", marketPositionUI("market_position")),
  nav_panel("上市公司画像", companyProfileUI("company_profile")),
  nav_panel("市场发展画像", marketDevelopmentUI("market_development")),
  nav_panel("市场质量画像", marketQualityUI("market_quality"))
)

server <- function(input, output, session) {
  homeServer("home")
  marketPositionServer("market_position")
  companyProfileServer("company_profile")
  marketDevelopmentServer("market_development")
  marketQualityServer("market_quality")
}

shinyApp(ui, server)
