library(car) 
library(lmtest) 
library(sandwich)
library(ggplot2)
library(dplyr)
library(zoo)
library(gridExtra)
library(tseries)  
library(modelsummary)
library(stargazer)
library(dplyr)
library(lubridate)

setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/_Estimation/_combined data")
df <- read.csv("df_LTWAI.csv") %>%
  mutate(date = as.Date(date))

df_cleaned <- df %>%
  select(iso_year, iso_week, quarter, date, LTWAI, Pct_change_GDP) %>%
  mutate(date = as.Date(date),
         d_LTWAI = LTWAI - lag(LTWAI)) %>%
  filter(date < as.Date("2025-12-23"))
  

# Creating rolling means
for (w in 1:13) {
  df_cleaned[[paste0("LTWAI_roll_", w)]] <- rollmean(df_cleaned$LTWAI, k = w, fill = NA, align = "right")
}

# Create lagged LTWAI_roll_w variables with decreasing lag
lag_amount = 13
for (w in 1:13) {
  lag_amount <- lag_amount - 1   # gives 12,11,10,...,1
  df_cleaned[[paste0("LTWAI_roll_", w)]] <- 
    dplyr::lag(df_cleaned[[paste0("LTWAI_roll_", w)]], n = lag_amount)
}


df_reg <- df_cleaned %>%
  filter(!is.na(Pct_change_GDP))

# Running regressions
models <- list()

for (w in 1:13) {
  f <- as.formula(paste0("Pct_change_GDP ~ LTWAI_roll_", w))
  models[[paste0("W", w)]] <- lm(f, data = df_reg)
}

vcov_list <- lapply(models, function(m) NeweyWest(m))

modelsummary(
  models,
  vcov = vcov_list,
  statistic = "({std.error})",
  stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01),
  gof_omit = "IC|Log|RMSE",
  title = "GDP growth and LTWAI: in-sample regressions at weekly frequency",
  coef_rename = function(x) gsub("LTWAI_roll_", "LTWAI_roll_", x)
)

se_list <- lapply(vcov_list, function(V) sqrt(diag(V)))

stargazer(
  models,
  type = "latex",
  title = "GDP growth and LTWAI: in-sample regressions at weekly frequency",
  se = se_list,
  dep.var.labels = "GDP Growth",
  star.cutoffs = c(0.10, 0.05, 0.01),
  digits = 3,
  no.space = TRUE,
  omit = "Intercept",          # drop intercept cleanly
  order = paste0("LTWAI_roll_", 1:13)   # force correct variable order
)

# Cross-correlation between LTWAI and gdp
df_ccf <- df_cleaned %>%
  arrange(date) %>%
  mutate(GDP_weekly = zoo::na.locf(Pct_change_GDP, na.rm = FALSE, fromLast = TRUE))

ccf(df_ccf$LTWAI, df_ccf$GDP_weekly, lag.max = 20)

################################################################################
# Monthly and Quarterly freq
################################################################################
df_monthly <- df %>%
  filter(date < as.Date("2025-12-23")) %>%
  mutate(
    iso_year  = lubridate::year(date),
    Month = lubridate::month(date)
  ) %>%
  group_by(iso_year, Month) %>%
  summarise(LTWAI_month = mean(LTWAI, na.rm = TRUE), .groups = "drop")

GDP_quarterly <- df_cleaned %>%
  group_by(iso_year, quarter) %>%
  summarise(Pct_change_GDP = last(Pct_change_GDP))

df_q_2 <- df_monthly %>%
  mutate(quarter = ceiling(Month/3)) %>%
  group_by(iso_year, quarter) %>%
  summarise(
    LTWAI_q = mean(LTWAI_month),
    LTWAI_m1 = first(LTWAI_month),
    LTWAI_m2 = nth(LTWAI_month, 2),
    LTWAI_m3 = nth(LTWAI_month, 3)
  ) %>%
  left_join(GDP_quarterly, by = c("iso_year", "quarter")) %>%
  filter(!is.na(Pct_change_GDP))

m1 <- lm(Pct_change_GDP ~ LTWAI_q, data = df_q_2)
m2 <- lm(Pct_change_GDP ~ LTWAI_m2 + LTWAI_m1, data = df_q_2)
m3 <- lm(Pct_change_GDP ~ LTWAI_m3 + LTWAI_m2 + LTWAI_m1, data = df_q_2)
m4 <- lm(Pct_change_GDP ~ LTWAI_m1, data = df_q_2)

anova(m3)

models_q <- list(m1, m4, m2, m3)
se_list_q <- lapply(models_q, function(m) sqrt(diag(NeweyWest(m))))

stargazer(
  m1,m4,  m2, m3,
  type = "latex",
  title = "GDP Regression Results (LTWAI Monthly Aggregation)",
  dep.var.labels = "Quarterly GDP Growth",
  se = se_list_q,
  covariate.labels = c(
    "LTWAI Quarterly", 
    "LTWAI Month 3", "LTWAI Month 2", "LTWAI Month 1"
  ),
  omit.stat = c("ser"),
  star.cutoffs = c(0.10, 0.05, 0.01),
  digits = 3,
  no.space = TRUE
)











