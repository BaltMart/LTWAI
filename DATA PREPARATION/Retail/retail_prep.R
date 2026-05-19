library(ggplot2)
library(lubridate)
library(tidyverse)
library(zoo)

# Setting a working directory
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/Retail index")

# Reading data
df <- read.csv("G47_Trade.csv")

retail <- df %>%
  select(Time, Value) %>%
    mutate(
      Value = Value,
      
      year = as.numeric(str_sub(Time, 1, 4)),
      month = as.numeric(str_sub(Time, 6, 7)),
      
      date = as.Date(paste(year, month, "01", sep = "-")),
      
      month_end = ceiling_date(date, "month") - days(1),
      
      # Get ISO week of the last day of the month
      iso_year = isoyear(month_end),
      iso_week = isoweek(month_end),
      
      # Keep calendar coordinates
      calendar_year = year,
      calendar_month = month
    ) %>%
    select(calendar_year, calendar_month, iso_year, iso_week, Value) %>%
  rename(Ret_value = Value)

# Creating quarterlized growth rates
retail_monthly <- retail %>%
  arrange(iso_year, iso_week) %>%
  mutate(week_number = row_number()) %>%
  mutate(
    ma_3m_current = rollmean(Ret_value, k = 3, fill = NA, align = "right"),
    ma_3m_lag = lag(ma_3m_current, 3),
    growth_q = log(ma_3m_current / ma_3m_lag) * 100
  )

# Writing the prepared data set
to_write <- retail_monthly %>%
  mutate(growth_q_ret = growth_q) %>%
  select(iso_year, iso_week, growth_q_ret)

write.csv(to_write, "Retail_changes_adjusted.csv", row.names = FALSE)
    