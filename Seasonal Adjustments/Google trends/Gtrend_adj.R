library(tidyverse)
library(zoo)
library(ggplot2)
library(tsibble)
library(feasts)
library(fable)
library(seastests)

# Loading the data
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/GOOGLE Trends/Linked Data")
df <- read.csv("gtrend_nedarbas.csv") %>%
  mutate(Nedarbas_gtrend = Nedarbas_gtrend,
         Time = as.Date(Time)) %>%
  filter(Time > as.Date("2011-01-01"))
  

# Creating a time series with fractional frequency
y <- ts(df$Nedarbas_gtrend, start = c(2010, 52), frequency = 52.178)

weekly_ts <- df %>%
  mutate(week = as.Date(Time)) %>%  # adjust column name
  as_tsibble(index = week)

monthly_data <- weekly_ts %>%
  index_by(month = ~ yearmonth(.)) %>%
  summarise(value = mean(Nedarbas_gtrend))

# =============================================================================
# SEASONALITY TESTS - BEFORE ADJUSTMENT
# =============================================================================

# ACF plot
p1 <- weekly_ts %>%
  ACF(Nedarbas_gtrend, lag_max = 104) %>%
  autoplot() +
  labs(title = "ACF - Before Seasonal Adjustment") +
  theme_minimal()
print(p1)

# QS Test for monthly seasonality (frequency ~4.33 weeks)
ts_monthly <- ts(monthly_data$value, frequency = 12)
qs_monthly_before <- combined_test(ts_monthly)

# QS Test for yearly seasonality (frequency = 52)
qs_yearly_before <- combined_test(y)

# Friedman Test for monthly seasonality
x <- as.numeric(ts_monthly)
n_years <- floor(length(x) / 12)
monthly_matrix <- matrix(
  x[1:(n_years * 12)],
  nrow = n_years,
  ncol = 12,
  byrow = TRUE
)
friedman_monthly_before <- friedman.test(monthly_matrix)


# Friedman Test for yearly seasonality
n_years <- floor(length(weekly_ts$Nedarbas_gtrend) / 52)
yearly_matrix <- matrix(weekly_ts$Nedarbas_gtrend[1:(n_years * 52)], 
                        nrow = n_years, ncol = 52, byrow = TRUE)
friedman_yearly_before <- friedman.test(yearly_matrix)

################################################################################
# Performing STL decomposition (to remove yearly seasonality)
################################################################################

# STL decomposition
dcmp <- weekly_ts %>%
  model(STL(Nedarbas_gtrend ~ season(period = "1 year", window = 5) +
                              season(period = "1 month", window = 23),
            iterations = 2, 
            robust = TRUE,
            outer = 2
            ))

components_df <- components(dcmp)

# Visualize decomposition
components_df %>%
  autoplot() +
  labs(title = "MSTL Decomposition",
       caption = "Note: Data from Google Trends, calculations my own.") +
  theme_minimal()

# Add seasonally adjusted series
weekly_ts <- weekly_ts %>%
  mutate(sa_value = components_df$season_adjust)

monthly_data_sa <- weekly_ts %>%
  mutate(month = yearmonth(week)) %>%
  index_by(month) %>%
  summarise(value = mean(sa_value, na.rm = TRUE))

# =============================================================================
# SEASONALITY TESTS - AFTER ADJUSTMENT
# =============================================================================
# ACF plot
p2 <- weekly_ts %>%
  ACF(sa_value, lag_max = 104) %>%
  autoplot() +
  labs(title = "ACF - After Seasonal Adjustment") +
  theme_minimal()
#print(p2)

# QS Test for monthly seasonality
ts_monthly_sa <- ts(monthly_data_sa$value, frequency = 12)
qs_monthly_after <- combined_test(ts_monthly_sa)

# QS Test for yearly seasonality
ts_yearly_sa <- ts(weekly_ts$sa_value, frequency = 52)
qs_yearly_after <- combined_test(ts_yearly_sa)

# Friedman Test for monthly seasonality
x_sa <- as.numeric(ts_monthly_sa)
n_years_sa <- floor(length(x_sa) / 12)
monthly_matrix_sa <- matrix(
  x_sa[1:(n_years_sa * 12)],
  nrow = n_years_sa,
  ncol = 12,
  byrow = TRUE
)
friedman_monthly_after <- friedman.test(monthly_matrix_sa)

# Friedman Test for yearly seasonality
n_years_sa <- floor(length(weekly_ts$sa_value) / 52)
yearly_matrix_sa <- matrix(weekly_ts$sa_value[1:(n_years_sa * 52)], 
                           nrow = n_years_sa, ncol = 52, byrow = TRUE)
friedman_yearly_after <- friedman.test(yearly_matrix_sa)

# =============================================================================
# COMPARISON TABLE
# =============================================================================

comparison <- data.frame(
  Test = c("QS - Monthly", "QS - Yearly", "Friedman - Monthly", "Friedman - Yearly"),
  Before_pvalue = c(
    round(qs_monthly_before$Pval["QS p-value"], 4),
    round(qs_yearly_before$Pval["QS p-value"], 4),
    round(friedman_monthly_before$p.value, 4),
    round(friedman_yearly_before$p.value, 4)
  ),
  After_pvalue = c(
    round(qs_monthly_after$Pval["QS p-value"], 4),
    round(qs_yearly_after$Pval["QS p-value"], 4),
    round(friedman_monthly_after$p.value, 4),
    round(friedman_yearly_after$p.value, 4)
  ),
  Seasonality_Removed = c(
    ifelse(qs_monthly_after$Pval["QS p-value"] > 0.05, "Yes", "No"),
    ifelse(qs_yearly_after$Pval["QS p-value"] > 0.05, "Yes", "No"),
    ifelse(friedman_monthly_after$p.value > 0.05, "Yes", "No"),
    ifelse(friedman_yearly_after$p.value > 0.05, "Yes", "No")
  )
)
print(comparison)

# Plotting the adjusted time series and the original together
ggplot()+
  geom_line(data = weekly_ts, aes(x = week, y = Nedarbas_gtrend, colour = "Unadjusted")) +
  geom_line(data = weekly_ts, aes(x = week, y = sa_value, color = "Adjusted")) +
  labs(title = "Google Trends for Unemployment topic unadjusted vs adjusted series",
       x = "Time",
       y = "Values",
       caption = "Note: Data obtained from Google Trends. Calculation my own.") +
  theme_minimal() +
  theme(legend.position = "bottom")
  
# Writing the adjusted series
to_write <- as.data.frame(weekly_ts) %>%
  mutate(nedarbas_adj = sa_value) %>%
  select(Time, nedarbas_adj)

write.csv(to_write, file = "adjusted_gtrend.csv", row.names = FALSE)
