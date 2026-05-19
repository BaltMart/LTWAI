library(ggplot2)
library(lubridate)
library(tidyverse)
library(zoo)

# Setting a working directory
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/Number of Flights")

# Loading the data
df <- read.csv("total-number-of-flights.csv")

# Creating separate year columns
yr2019 <- df %>%
  select(X2019.Number.of.flights) %>%
  rename(flights = X2019.Number.of.flights)

yr2020 <- df %>%
  select(X2020.Number.of.flights) %>%
  rename(flights = X2020.Number.of.flights)

yr2021 <- df %>%
  select(X2021.Number.of.flights) %>%
  rename(flights = X2021.Number.of.flights)

yr2022 <- df %>%
  select(X2022.Number.of.flights) %>%
  rename(flights = X2022.Number.of.flights)

yr2023 <- df %>%
  select(X2023.Number.of.flights) %>%
  rename(flights = X2023.Number.of.flights)

yr2024 <- df %>%
  select(X2024.Number.of.flights) %>%
  rename(flights = X2024.Number.of.flights)

yr2025 <- df %>%
  select(X2025.Number.of.flights) %>%
  rename(flights = X2025.Number.of.flights)

yr2026 <- df %>%
  select(X2026.Number.of.flights) %>%
  rename(flights = X2026.Number.of.flights) %>%
  filter(!is.na(flights))

# Creating a timeline
timeline_no_leap <- tibble(
  Day = seq(
    from = as.Date("2019-01-01"), 
    to = as.Date("2026-03-22"), 
    by = "1 day"
  )
) %>%
  filter(!(month(Day) == 2 & day(Day) == 29))

full_timeline <- tibble(
  Day = seq(
    from = as.Date("2019-01-01"), 
    to = as.Date("2026-03-22"), 
    by = "1 day"
  )
)

# Binding and joining the timeline with the numbers
long_flights <- rbind(yr2019, yr2020, yr2021, yr2022, yr2023, yr2024, yr2025, yr2026)

combined_df_no_leap <- cbind(timeline_no_leap, long_flights)

combined_df <- full_timeline %>%
  left_join(combined_df_no_leap, by = "Day") %>%
  mutate(
    # Filling in Feb 29 with linear interpolation between Feb 28 and Mar 1
    flights = na.approx(flights, na.rm = FALSE)
  ) %>%
  mutate(iso_year = isoyear(Day),
         iso_week = isoweek(Day))

# Plotting the series
ggplot(data = combined_df, aes(x = Day)) +
  geom_line(aes(y = flights), linewidth = 0.5, color = "darkblue") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Number of global flights",
       x = "Time", y = "Count",
       caption = "Note: Data from Flightradar24, work my own. Data in graph up to 2026-03-22.") +
  theme_minimal()

# Writing the unadjusted data
write.csv(combined_df, "Total_flights_unadjusted.csv")

