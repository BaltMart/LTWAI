library(arrow)
library(ggplot2)
library(tidyverse)
library(lubridate)

# Setting the working directory of historical data
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/NO2/E1a")
# Loading the historical data
files_upto2024 <- list.files(pattern = "*.parquet")

raw_data <- map_df(files_upto2024, ~{
  read_parquet(.x) %>%
    select(Samplingpoint, Start, Value, Validity) %>%
    filter(Validity == 1) %>% 
    filter(Value < 999) %>%
    mutate(Start = as_datetime(Start) + hours(1))
})

full_timeline <- tibble(
  Start = seq(
    from = as_datetime("2013-01-01 00:00:00"), 
    to = as_datetime("2024-12-31 23:00:00"), 
    by = "1 hour"
  )
)

no2_lithuania_index <- full_timeline %>%
  left_join(raw_data, by = "Start") %>%
  group_by(Start) %>%
  summarise(
    no2_avg = mean(Value, na.rm = TRUE),
    stations_active = sum(!is.na(Value)),
    .groups = "drop"
  ) %>%
  # Handle hours where no stations were reporting
  mutate(no2_avg = ifelse(is.nan(no2_avg), NA, no2_avg))

# Plotting how to the values look like
ggplot(data = no2_lithuania_index, aes(x = Start, y = no2_avg)) +
  geom_line() +
  labs(x = "Day of observation",
       y = "NO2 concentration") +
  theme_minimal()

# Setting the working directory of present data (2025 and would be to present day)
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/NO2/E2a")
# Loading the sets of present date data
files_new <- list.files(pattern = "*.parquet")

raw_data_new <- map_df(files_new, ~{
  read_parquet(.x) %>%
    select(Samplingpoint, Start, Value, Validity) %>%
    filter(Validity == 1) %>% 
    filter(Value < 999) %>%
    mutate(Start = as_datetime(Start))
})

full_timeline_new <- tibble(
  Start = seq(
    from = as_datetime("2025-01-01 00:00:00"), 
    to = as_datetime("2026-03-19 23:00:00"), 
    by = "1 hour"
  )
)

no2_lithuania_index_new <- full_timeline_new %>%
  left_join(raw_data_new, by = "Start") %>%
  group_by(Start) %>%
  summarise(
    no2_avg = mean(Value, na.rm = TRUE),
    stations_active = sum(!is.na(Value)),
    .groups = "drop"
  ) %>%
  # Handle hours where no stations were reporting
  mutate(no2_avg = ifelse(is.nan(no2_avg), NA, no2_avg))

# Plotting how to the values look like
ggplot(data = no2_lithuania_index_new, aes(x = Start, y = no2_avg)) +
  geom_line() +
  labs(x = "Day of observation",
       y = "NO2 concentration") +
  theme_minimal()

## Merging the two data sets
no2_lithuania_index_full <- bind_rows(no2_lithuania_index, no2_lithuania_index_new) %>%
  mutate(Day = as.Date(Start),
         Hour = hour(Start))

ggplot(data = no2_lithuania_index_full, aes(x = Start, y = no2_avg)) +
  geom_line() +
  labs(x = "Day of observation",
       y = "NO2 concentration") +
  theme_minimal()

# Creating daily averages of No2 concentration
no2_lithuania_index_daily <- no2_lithuania_index_full %>%
  group_by(Day) %>%
  summarise(NO2_avg = mean(no2_avg)) %>%
  mutate(iso_year = isoyear(Day),
         iso_week = isoweek(Day))

ggplot(data = no2_lithuania_index_daily, aes(x = Day)) +
  geom_line(aes(y = NO2_avg), color = "green") +
  labs(x = "Day of observation",
       y = "NO2 concentration",
       #title = "Average NO2 concentration in Lithuania as measured by observation stations",
       caption = "Note: Data from EEA, work my own. Data in graph up to 2026-03-19.") +
  
  theme_minimal()

# Writing the unadjusted data set
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/NO2")
write.csv(no2_lithuania_index_daily, "no2_daily_unadjusted.csv")




