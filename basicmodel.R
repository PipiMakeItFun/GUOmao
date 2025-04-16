library(readxl)
library(dplyr)
library(fixest)
library(broom)
library(dplyr)
library(ggplot2)
data <- read_excel("traindata.xlsx", sheet = 3)
data$wastewater_ems <- as.numeric(data$wastewater_ems)
data$SO2_ems <- as.numeric(as.character(data$SO2_ems))
data$smoke_ems <- as.numeric(as.character(data$smoke_ems))
data$entropy_score <- as.numeric(as.character(dataszf$entropy_score))
unique(data$中欧班列开通时间)
# 生成 treat 列
data$treat <- ifelse(data$中欧班列开通时间 == 10000000, 0, 1)
# 检查生成结果
head(data[, c("中欧班列开通时间", "treat")])
# 核心变量
data$X <- data$after * data$treat
model_1 <- lm(log(wastewater_ems) ~ X + LN_GDP + FIEI + pop_num + openness + gov_intervention + factor(Ctnm) + factor(year), data = data)
summary(model_1)
model_2 <- lm(log(SO2_ems) ~ X + LN_GDP + FIEI + pop_num + openness + gov_intervention + factor(Ctnm) + factor(year), data = data)
summary(model_2)
model_3 <- lm(log(smoke_ems) ~ X + LN_GDP + FIEI + pop_num + openness + gov_intervention + factor(Ctnm) + factor(year), data = data)
summary(model_3)
#开始画图
library(dplyr)
did_plot_data <- data %>%
  mutate(group = ifelse(treat == 1, "Treatment", "Control")) %>%
  group_by(group, year) %>%
  summarise(avg_pollution = mean(log(wastewater_ems), na.rm = TRUE)) %>%
  ungroup()
library(ggplot2)
ggplot(did_plot_data, aes(x = year, y = avg_pollution, color = group)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "gray40") +
  labs(title = "DID Effect: Wastewater Emissions Over Time",
       x = "Year", y = "log(Wastewater Emissions)",
       color = "Group") +
  theme_minimal()
#画图1结束，开始szf
library(dplyr)
pollution_vars <- data %>%
  select(Ctnm, year, wastewater_ems, SO2_ems, smoke_ems)
pollution_std <- pollution_vars %>%
  mutate(across(c(wastewater_ems, SO2_ems, smoke_ems),
                ~ (as.numeric(.) - min(as.numeric(.), na.rm = TRUE)) /
                  (max(as.numeric(.), na.rm = TRUE) - min(as.numeric(.), na.rm = TRUE))))
std_matrix <- pollution_std %>% select(-Ctnm, -year)
P <- apply(std_matrix, 2, function(x) {
  x / sum(x, na.rm = TRUE)
})
k <- 1 / log(nrow(P))
e <- -k * colSums(P * log(P + 1e-10), na.rm = TRUE)
d <- 1 - e
w <- d / sum(d)
print(w)
composite_score <- as.matrix(std_matrix) %*% w 
pollution_std$pollution_index <- as.numeric(composite_score)
data <- data %>%
  left_join(pollution_std %>% select(Ctnm, year, pollution_index), by = c("Ctnm", "year"))
model_4 <- lm(log(pollution_index + 1e-5) ~ X + LN_GDP + FIEI + pop_num + openness + gov_intervention + factor(Ctnm) + factor(year),
              data = data)
summary(model_4)
model_5 <- lm(log(SO2_ems) ~ X + LN_GDP + FIEI + pop_num + openness + gov_intervention , data = data)
summary(model_5)
model_6 <- lm(log(SO2_ems) ~ X , data = data)
summary(model_6)
model_7 <- lm(SO2_ems ~ X + LN_GDP + FIEI + pop_num + openness + gov_intervention , data = data)
summary(model_7)
model_8 <- lm(SO2_ems ~ X + LN_GDP + FIEI + pop_num + openness + gov_intervention + factor(Ctnm) + factor(year), data = data)
summary(model_8)
model9 <- lm(entropy_score ~ X + LN_GDP + FIEI + pop_num + openness + gov_intervention + factor(Ctnm) + factor(year), data = data)
summary(model_9)
#结论：用sqf，不取对数版本。接下来，平行趋势检验。
model_paratrend <- feols(
  log(pollution_index + 1e-5) ~ i(year, treat, ref = 2013) + X + LN_GDP + FIEI + pop_num + openness + gov_intervention + factor(Ctnm),
  data = data
)
coef_df <- tidy(model_paratrend) %>%
  filter(grepl("year::\\d{4}:treat", term)) %>%  # 匹配形如 "year::2010:treat"
  mutate(
    year = as.numeric(sub("year::(\\d{4}):treat", "\\1", term))  # 提取出 2010、2011 等年份
  )

ggplot(coef_df, aes(x = year, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error,
                    ymax = estimate + 1.96*std.error), width = 0.2) +
  geom_vline(xintercept = 2012, linetype = "dashed") +
  labs(x = "Year", y = "Treatment Effect", title = "Parallel Trend Test") +
  theme_minimal()

table(data$year)
print(coef_df$year)
coef_df_all <- tidy(model_paratrend)
unique(coef_df_all$term)
#
model_paratrend <- feols(
  log(pollution_index + 1e-5) ~ i(year, treat, ref = 2013) +
    LN_GDP + FIEI + pop_num + openness + gov_intervention |
    Ctnm + year,
  data = data
)

# 整理模型结果
coef_df <- tidy(model_paratrend) %>%
  filter(grepl("^year::\\d{4}$", term)) %>%  # 只保留格式为 "year::2020" 这样完整的项
  mutate(year = as.numeric(sub("year::", "", term)))

# 画图
ggplot(coef_df, aes(x = year, y = estimate)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "gray") +
  labs(title = "Traditional DID: Parallel Trend Check",
       x = "Year",
       y = "Treatment Effect on log(wastewater_ems)") +
  theme_minimal()
