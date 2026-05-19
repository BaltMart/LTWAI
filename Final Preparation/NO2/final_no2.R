library(tidyverse)
library(ggplot2)
library(zoo)
library(lubridate)

# Loading the working directory of target data
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/NO2")

# Loading the data set
df <- read.csv("NO2_adjusted.csv")

# Assigning the iso week/years to the observations
df <- df %>%
  mutate(iso_year = isoyear(Date),
         iso_week = isoweek(Date))

# Summarizing data into weeks
weekly_averages <- df %>%
  group_by(iso_year, iso_week) %>%
  summarise(Average_no2 = mean(No2_adj, na.rm = TRUE),
            .groups = 'drop')

# Calculating weekly growth rates
no2_weekly <- weekly_averages %>%
  arrange(iso_year, iso_week) %>%
  # Create a continuous week counter
  mutate(
    week_number = row_number()
  ) %>%
  mutate(
    # Now the moving average works continuously
    ma_13w_current = rollmean(Average_no2, k = 13, fill = NA, align = "right"),
    ma_13w_lagged = lag(ma_13w_current, 13),
    growth_q = log(ma_13w_current / ma_13w_lagged) * 100
  ) 

# Selecting the data to write to be used later on
to_write <- no2_weekly %>%
  mutate(growth_q_no2 = growth_q) %>%
  select(iso_year, iso_week, growth_q_no2)

setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/NO2/_final")

write.csv(to_write, "final_no2.csv", row.names = FALSE)
