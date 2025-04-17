library(readxl)
library(dplyr)
library(fixest)
library(broom)
library(dplyr)
library(ggplot2)
data <- read_excel("traindata.xlsx", sheet = 3)
data$entropy_score <- as.numeric(as.character(dataszf$entropy_score))
unique(data$中欧班列开通时间)
data$treat <- ifelse(data$中欧班列开通时间 == 10000000, 0, 1)
head(data[, c("中欧班列开通时间", "treat")])
data$X <- data$after * data$treat
model_3 <- lm(log(entropy_score) ~ X + LN_GDP + FIEI + openness + gov_intervention + factor(Ctnm) + factor(year), data = data)
summary(model_3)

#可视化-多期did效应
model_paratrend <- feols(
  log(entropy_score + 1e-5) ~ i(year, treat, ref = 2012) + 
    LN_GDP + FIEI + pop_num + openness + gov_intervention + factor(Ctnm),
  data = data
)
coef_df <- tidy(model_paratrend) %>%
  filter(grepl("year::\\d{4}:treat", term)) %>%
  mutate(year = as.numeric(sub("year::(\\d{4}):treat", "\\1", term)))

ggplot(coef_df, aes(x = year, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.3) +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray") +
  scale_x_continuous(breaks = seq(min(coef_df$year), max(coef_df$year), 1)) +
  labs(
    x = "Year",
    y = "Treatment Effect (relative to 2012)",
    title = "DID Effect Visualization (Event Study Plot)"
  ) +
  theme_minimal()
#可视化-组均值趋势
did_plot_data <- data %>%
  mutate(group = ifelse(treat == 1, "Treatment", "Control")) %>%
  group_by(group, year) %>%
  summarise(avg_entropy = mean(log(entropy_score), na.rm = TRUE)) %>%
  ungroup()
ggplot(did_plot_data, aes(x = year, y = avg_entropy, color = group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "gray40") +
  labs(
    title = "Trends in Pollution Index (Entropy Score)",
    subtitle = "Treatment vs Control Groups over Time",
    x = "Year", y = "Average Entropy Score",
    color = "Group"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )
#平行趋势检验
#para1:有pop
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
#平行2:无pop
model_paratrend2 <- feols(
  log(pollution_index + 1e-5) ~ i(year, treat, ref = 2013) + X + LN_GDP + FIEI + openness + gov_intervention + factor(Ctnm),
  data = data
)
coef_df <- tidy(model_paratrend2) %>%
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
#平行3:无FIEI
model_paratrend3 <- feols(
  log(pollution_index + 1e-5) ~ i(year, treat, ref = 2013) + X + LN_GDP + pop_num + openness + gov_intervention + factor(Ctnm),
  data = data
)
coef_df <- tidy(model_paratrend3) %>%
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
#平行4:无控制变量
model_paratrend4 <- feols(
  log(pollution_index + 1e-5) ~ i(year, treat, ref = 2013) + X + factor(Ctnm),
  data = data
)
coef_df <- tidy(model_paratrend4) %>%
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
#可视化版本1
did_plot_data <- data %>%
  mutate(group = ifelse(treat == 1, "Treatment", "Control")) %>%
  group_by(group, year) %>%
  summarise(avg_pollution = mean(entropy_score, na.rm = TRUE)) %>%
  ungroup()
ggplot(did_plot_data, aes(x = year, y = avg_pollution, color = group)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "gray40") +
  labs(title = "DID Effect: Wastewater Emissions Over Time",
       x = "Year", y = "log(Wastewater Emissions)",
       color = "Group") +
  theme_minimal()

