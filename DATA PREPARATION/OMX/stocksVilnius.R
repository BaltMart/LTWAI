library(ggplot2)
library(tidyverse)
library(lubridate)
library(zoo)

# Setting the working directory of the data
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/OMX Vilnius")

# loading the data
df <- readxl::read_xlsx("index.xlsx")

df <- df %>%
  mutate(iso_year = isoyear(Data),
         iso_week = isoweek(Data),
         day_of_week = wday(Data, label = FALSE))

# Creating a weekly (iso week) average of the index values
weekly_averages <- df %>%
  filter(!day_of_week %in% c(1, 7)) %>%
  group_by(iso_year, iso_week) %>%
  summarise(Ave = mean(Value, na.rm = TRUE),
            .groups = 'drop')

# Calculating weekly growth rates
stock_weekly <- weekly_averages %>%
  arrange(iso_year, iso_week) %>%
  # Create a continuous week counter
  mutate(
    week_number = row_number()
  ) %>%
  mutate(
    # Now the moving average works continuously
    ma_13w_current = rollmean(Ave, k = 13, fill = NA, align = "right"),
    ma_13w_lagged = lag(ma_13w_current, 13),
    growth_q = log(ma_13w_current / ma_13w_lagged) * 100
  )

# Writing stock data
to_write <- stock_weekly %>%
  mutate(growth_q_omx = growth_q) %>%
  select(iso_year, iso_week, growth_q_omx)

write.csv(to_write, "stocks_fully_preped.csv", row.names = FALSE)

# Plotting the data set as prepared
ggplot(data = stock_weekly, aes(x = ))







