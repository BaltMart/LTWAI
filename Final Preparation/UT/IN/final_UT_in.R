library(tidyverse)
library(ggplot2)
library(zoo)
library(lubridate)

# Loading the working directory of target data
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/Uzimtumo tarnyba/Adjrez")

# Loading the data set
df <- read.csv("Unemp_adj.csv")

# Assigning the iso week/years to the observations
df <- df %>%
  mutate(iso_year = isoyear(Date),
         iso_week = isoweek(Date))

# Summarizing data into weeks
weekly_averages <- df %>%
  group_by(iso_year, iso_week) %>%
  summarise(Average_unemp_in = mean(unemp_adj, na.rm = TRUE),
            .groups = 'drop')

# Calculating weekly growth rates
unemp_in_weekly <- weekly_averages %>%
  arrange(iso_year, iso_week) %>%
  # Create a continuous week counter
  mutate(
    week_number = row_number()
  ) %>%
  mutate(
    # Now the moving average works continuously
    ma_13w_current = rollmean(Average_unemp_in, k = 13, fill = NA, align = "right"),
    ma_13w_lagged = lag(ma_13w_current, 13),
    growth_q = log(ma_13w_current / ma_13w_lagged) * 100
  ) 

# Selecting the data to write to be used later on
to_write <- unemp_in_weekly %>%
  mutate(growth_q_unemp_in = growth_q) %>%
  select(iso_year, iso_week, growth_q_unemp_in)

setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/Uzimtumo tarnyba/Adjrez/_final")

write.csv(to_write, "final_unemp_in.csv", row.names = FALSE)
