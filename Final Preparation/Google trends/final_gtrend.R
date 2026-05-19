library(tidyverse)
library(ggplot2)
library(zoo)
library(lubridate)

# Loading the working directory of target data
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/GOOGLE Trends/Linked Data")

# Loading the data set
df <- read.csv("adjusted_gtrend.csv")

# Assigning the iso week/years to the observations
df <- df %>%
  mutate(iso_year = isoyear(Time),
         iso_week = isoweek(Time))

# Calculating weekly growth rates
gtrend_weekly <- df %>%
  arrange(iso_year, iso_week) %>%
  # Create a continuous week counter
  mutate(
    week_number = row_number()
  ) %>%
  mutate(
    # Now the moving average works continuously
    ma_13w_current = rollmean(nedarbas_adj, k = 13, fill = NA, align = "right"),
    ma_13w_lagged = lag(ma_13w_current, 13),
    growth_q = log(ma_13w_current / ma_13w_lagged) * 100
  ) 

# Selecting the data to write to be used later on
to_write <- gtrend_weekly %>%
  mutate(growth_q_nedar = growth_q) %>%
  select(iso_year, iso_week, growth_q_nedar)

setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/GOOGLE Trends/_final")

write.csv(to_write, "gtrend_final.csv", row.names = FALSE)
