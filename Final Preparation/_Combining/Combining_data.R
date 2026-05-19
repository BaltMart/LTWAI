################################################################################
# Combining timeseries into a single dataset
################################################################################
library(tidyverse)
library(ggplot2)
library(zoo)
library(lubridate)

# Loading UT data 
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/Uzimtumo tarnyba/Adjrez/_final")
UT_in <- read.csv("final_unemp_in.csv")
UT_out <- read.csv("final_unemp_out.csv")

# Loading retail data
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/Retail index")
Retail <- read.csv("Retail_changes_adjusted.csv")

# Loading industrial production data
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/Industrial index")
Industrial <- read.csv("Industry_changes_adjusted.csv")

# Loading shipping data
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/Shipping data/_final")
Shipping <- read.csv("final_shipping.csv")

# Loading flights data
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/Number of Flights/_final")
Flights <- read.csv("final_flights.csv")

# Loading no2 data
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/NO2/_final")
NO2 <- read.csv("final_no2.csv")

# Loading the energy data
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/ENERGY/_final")
Elec <- read.csv("final_elec.csv")

# Loading OMX Vilnius index data
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/OMX Vilnius")
OMX <- read.csv("stocks_fully_preped.csv")

# Loading Google Trends data
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/GOOGLE Trends/_final")
Gtrend <- read.csv("gtrend_final.csv")

# Loading GDP data
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/GDP")
GDP <- read.csv("GDP_isoed.csv")

# Joining the data sets

df_list <- list(Elec, Flights, Gtrend, Industrial, NO2, OMX, Retail, Shipping, UT_in, UT_out, GDP)

df <- reduce(df_list, full_join, by = c("iso_year", "iso_week")) %>%
  arrange(iso_year, iso_week)

# Filtering out the df so that at the start of the dataset, most timeseries are already there.
df_filtered <- df %>%
  filter(iso_year > 2015 | (iso_year == 2015 & iso_week >= 27)) %>%
  filter(iso_year < 2026 | (iso_year == 2026 & iso_week <= 9)) # For now because the data was collected at different points

# Writing the combined data set
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/_Estimation/_combined data")
write.csv(df_filtered, "data.csv", row.names = FALSE)

# Creating alternative data sets that start from a couple different starting points
df_filtered_2000 <- df %>%
  filter(iso_year > 2000 | (iso_year == 2000 & iso_week >= 25)) %>%
  filter(iso_year < 2026 | (iso_year == 2026 & iso_week <= 9))

df_filtered_2019 <- df %>%
  filter(iso_year > 2019 | (iso_year == 2019 & iso_week >= 26)) %>%
  filter(iso_year < 2026 | (iso_year == 2026 & iso_week <= 9))

setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/_Estimation/_combined data")
write.csv(df_filtered_2000, "data_2000.csv", row.names = FALSE)
write.csv(df_filtered_2019, "data_2019.csv", row.names = FALSE)