library(tidyverse)
library(ggplot2)

# Setting the working directory of the data
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/ENERGY")

file_list <- list.files(pattern = "*.csv")
data_list <- list() # Initialize an empty list

for (i in seq_along(file_list)) {
  
  data_list[[i]] <- read.csv(file_list[i])
  
  # Optional: Progress message
  message("Processed file: ", file_list[i])
}

energy_full <- do.call(rbind, data_list) %>%
  mutate(Total_Load = as.numeric(Actual.Total.Load..MW.),
         Forecast = as.numeric(Day.ahead.Total.Load.Forecast..MW.)) %>%
  filter(!is.na(Total_Load)) %>%
  mutate(date = str_sub(MTU..EET.EEST., 1, 10),
         Day = dmy(date))

energy_full_day <- energy_full %>%
  group_by(Day) %>%
  summarise(Total_load= mean(Total_Load)) %>%
  mutate(iso_year = isoyear(Day),
         iso_week = isoweek(Day))

ggplot(data = energy_full_day, aes(x = Day)) +
  geom_line(aes(y = Total_load), linewidth = 0.5, color = "cyan") +
  labs(#title = "Average daily hourly electricity load in Lithuania over time",
       x = "Time", y = "Average daily hourly electricity load",
       caption = "Note: Data from ENTSOE, work my own. Data in graph up to 2026-03-20.") +
  theme_minimal()

# Writing unadjusted energy averages per day
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/ENERGY/Unadjusted")
write.csv(energy_full_day, "Energy_per_day_unadjusted.csv")
