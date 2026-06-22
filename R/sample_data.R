make_kpis <- function(labels, values, units = rep("", length(labels)),
                      changes = rep("", length(labels)),
                      statuses = rep("neutral", length(labels))) {
  data.frame(
    label = labels,
    value = values,
    unit = units,
    change = changes,
    status = statuses,
    stringsAsFactors = FALSE
  )
}

# 用途：加载首页与四个画像页面的演示数据（判断、KPI、洞察、明细）。
# 输入来源：无参数，函数内硬编码的演示数据。
load_demo_data <- function() {
  list(
    home = list(
      judgment = "本所整体呈现稳步扩容、交易活跃度改善、风险水平可控的成长型市场特征。",
      kpis = make_kpis(
        labels = c("上市公司", "总市值", "日均成交额", "PE 中位数", "盈利公司占比", "风险公司"),
        values = c("249", "4,280", "182", "28.6", "78", "16"),
        units = c("家", "亿元", "亿元", "倍", "%", "家"),
        changes = c("+8 家", "+6.4%", "+12.1%", "较上期 -1.8", "+2.3pct", "-3 家"),
        statuses = c("positive", "positive", "positive", "neutral", "positive", "warning")
      ),
      insights = c(
        "市场规模保持温和扩张，新增上市公司以先进制造和专精特新企业为主。",
        "交易活跃度较上期改善，但活跃度仍集中在少数头部公司。",
        "整体风险可控，需持续关注低流动性和连续亏损公司。"
      ),
      details = data.frame(
        板块 = c("市场定位", "上市公司", "市场发展", "市场质量"),
        本期关注 = c("对标市场中的规模与估值位置", "行业结构与盈利质量", "上市融资与交易生态变化", "流动性、财务和合规风险"),
        状态 = c("待完善主图", "待完善主图", "待完善趋势", "待完善矩阵"),
        check.names = FALSE
      )
    ),
    market_position = list(
      judgment = "本所在多层次资本市场中以中小市值和创新型企业聚集为主要定位。",
      kpis = make_kpis(
        labels = c("上市公司", "总市值", "流通市值", "日均成交额", "PE 中位数", "前十市值占比"),
        values = c("249", "4,280", "3,190", "182", "28.6", "21"),
        units = c("家", "亿元", "亿元", "亿元", "倍", "%"),
        changes = c("+8 家", "+6.4%", "+5.2%", "+12.1%", "-1.8", "-0.6pct"),
        statuses = c("positive", "positive", "positive", "positive", "neutral", "neutral")
      ),
      insights = c(
        "市场体量仍小于主要对标板块，但企业数量与交易活跃度呈同步改善。",
        "行业结构以制造业和信息技术为主，契合成长型市场定位。",
        "头部集中度保持可控，市场画像具有一定多元化特征。"
      ),
      details = data.frame(
        市场 = c("本所", "沪市主板", "深市主板", "创业板", "科创板"),
        上市公司 = c(249, 1690, 1510, 1350, 580),
        总市值亿元 = c(4280, 468000, 235000, 121000, 78000),
        日均成交额亿元 = c(182, 4100, 3260, 2140, 890),
        check.names = FALSE
      )
    ),
    company_profile = list(
      judgment = "上市公司群体以中小规模、制造业占优、研发投入较高的成长型企业为主。",
      kpis = make_kpis(
        labels = c("营收中位数", "净利润中位数", "盈利公司占比", "ROE 中位数", "研发费用率", "募资使用率"),
        values = c("5.6", "0.48", "78", "7.8", "5.9", "83"),
        units = c("亿元", "亿元", "%", "%", "%", "%"),
        changes = c("+4.1%", "+3.5%", "+2.3pct", "+0.8pct", "+0.4pct", "+5.0pct"),
        statuses = c("positive", "positive", "positive", "positive", "positive", "positive")
      ),
      insights = c(
        "公司规模整体偏中小，但盈利面和研发强度具备成长型市场特征。",
        "制造业贡献较高，是当前公司画像的主导产业线索。",
        "成长性与盈利能力分化明显，后续四象限主图需要突出结构差异。"
      ),
      details = data.frame(
        行业 = c("高端制造", "信息技术", "医药生物", "基础材料", "消费服务"),
        公司数 = c(82, 46, 31, 28, 24),
        营收占比 = c("38%", "18%", "12%", "11%", "8%"),
        净利润占比 = c("41%", "20%", "10%", "9%", "7%"),
        check.names = FALSE
      )
    ),
    market_development = list(
      judgment = "上市、融资和交易生态均处于渐进增强阶段，发展动能仍需持续跟踪。",
      kpis = make_kpis(
        labels = c("本年新增上市", "在审企业", "IPO 融资额", "再融资额", "年度成交金额", "活跃公司"),
        values = c("18", "42", "96", "68", "41,200", "137"),
        units = c("家", "家", "亿元", "亿元", "亿元", "家"),
        changes = c("+5 家", "+9 家", "+18.4%", "+11.0%", "+12.1%", "+14 家"),
        statuses = c("positive", "positive", "positive", "positive", "positive", "positive")
      ),
      insights = c(
        "新增上市与在审企业形成一定储备，市场扩容具备后续承接基础。",
        "融资结构以 IPO 为主，再融资能力正在逐步增强。",
        "交易活跃度改善需要结合产品生态和做市机制继续观察。"
      ),
      details = data.frame(
        年份 = c(2022, 2023, 2024, 2025, 2026),
        新增上市 = c(34, 38, 22, 29, 18),
        IPO融资亿元 = c(166, 178, 112, 148, 96),
        日均成交额亿元 = c(92, 108, 136, 162, 182),
        check.names = FALSE
      )
    ),
    market_quality = list(
      judgment = "市场质量总体稳定，主要风险集中在低流动性、连续亏损和合规关注企业。",
      kpis = make_kpis(
        labels = c("低流动性占比", "高估值低增长", "连续亏损公司", "监管措施", "募投异常", "退市风险"),
        values = c("18", "12", "9", "21", "7", "3"),
        units = c("%", "家", "家", "项", "家", "家"),
        changes = c("-2.1pct", "-1 家", "-2 家", "+4 项", "-1 家", "持平"),
        statuses = c("positive", "neutral", "positive", "warning", "positive", "warning")
      ),
      insights = c(
        "低流动性风险有所缓解，但仍是市场质量监测的首要维度。",
        "财务风险集中在少数连续亏损公司，需要与行业景气度联动分析。",
        "合规和监管措施数量上升，后续应建立处置进展跟踪。"
      ),
      details = data.frame(
        公司 = c("公司 A", "公司 B", "公司 C", "公司 D"),
        行业 = c("高端制造", "信息技术", "医药生物", "基础材料"),
        风险类型 = c("低流动性", "连续亏损", "合规关注", "募投异常"),
        风险等级 = c("中", "高", "中", "低"),
        check.names = FALSE
      )
    )
  )
}
