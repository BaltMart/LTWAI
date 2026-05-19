library(ggplot2)
library(lubridate)
library(tidyverse)

# functions
has_53_weeks <- function(year) {
  # A year has 53 weeks if Dec 31 is in week 53, or if Dec 30 is (for leap years)
  dec_31 <- as.Date(paste0(year, "-12-31"))
  isoweek(dec_31) == 53
}

# Setting working directory
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/GDP")

# Reading the data set
df <- read.csv("GDP_fred.csv")

GDP <- df %>%
  rename(Pct_change_GDP = CLVMNACSCAB1GQLT,
         date = observation_date) %>%
  mutate(
    date = as.Date(date),
    calendar_year = year(date),
    calendar_quarter = quarter(date),
    iso_year = calendar_year,
    
    # Assign Q4 to week 53 if that year has 53 weeks, otherwise week 52
    iso_week = case_when(
      calendar_quarter == 1 ~ 13,
      calendar_quarter == 2 ~ 26,
      calendar_quarter == 3 ~ 39,
      calendar_quarter == 4 & has_53_weeks(calendar_year) ~ 53,
      calendar_quarter == 4 ~ 52
    )
  ) %>%
  select(iso_year, iso_week, Pct_change_GDP)

# Writing a csv file of prepared data
write.csv(GDP, "GDP_isoed.csv", row.names = FALSE)



