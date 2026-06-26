# 部署脚本：推送到 shinyapps.io
library(rsconnect)

# 记录日志
cat("开始部署到 shinyapps.io...\n")

# 部署应用
deployApp(
  appDir = ".",
  appName = "bse_dashboard",
  account = "yuanhaoming",
  server = "shinyapps.io",
  forceUpdate = TRUE,
  lint = FALSE
)

cat("部署完成。\n")
