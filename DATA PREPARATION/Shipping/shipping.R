library(lubridate)
library(tidyverse)
library(ggplot2)
library()

# Setting the working directory
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/Shipping data")

# Reading data
Operation <- read.csv("Operacija.csv")

Operation_cleaned <- Operation %>%
  filter(op_rusis %in% c("iš dalies iškraunamas",
                       "visiškai iškraunamas",
                       "iš dalies iš naujo pakrautas",
                       "visiškai iš naujo pakrautas",
                       "visiškai iškraunamas, iš dalies iškraunamas",
                       "visiškai iš naujo pakrautas, iš dalies iš naujo pakrautas",
                       "su balastu arba tuščias, iš dalies iš naujo pakrautas",
                       "su balastu arba tuščias, visiškai iškraunamas",
                       "visiškai iš naujo pakrautas, su balastu arba tuščias",
                       "su balastu arba tuščias, visiškai iš naujo pakrautas",
                       "remontas, iš dalies iš naujo pakrautas",
                       "visiškai iš naujo pakrautas, remontas")) %>%
  mutate(atvykimo_data = str_sub(faktinis_atvykimas, 1, 10),
         Day_atvykimas = ymd(atvykimo_data),
         isvykimo_data = str_sub(faktinis_isvykimas, 1, 10),
         Day_isvykimas = ymd(isvykimo_data)) %>%
  filter(atvykimo_data >= as.Date("2005-01-01"))

Operation_day_counts <- Operation_cleaned %>%
  group_by(Day_isvykimas) %>%
  summarise(n_at = n()) %>%
  mutate(iso_year = isoyear(Day_isvykimas),
         iso_week = isoweek(Day_isvykimas))

ggplot(Operation_day_counts, aes(x = Day_isvykimas)) +
  geom_line(aes(y = n_at), color = "purple", linewidth = 0.5) +
  labs(x = "Time", y = "Ships leaving the port", 
       #title = "Number of ships leaving the port of Klaipėda per day",
       caption = "Note: Data from Port of Klaipėda, work my own. Data in graph up to 2026-03-20.") + 
  theme_minimal()

# Writing the raw counts data 
write.csv(Operation_day_counts, "Operations_per_day_unadjusted.csv")





