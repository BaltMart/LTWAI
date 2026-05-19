library(ggplot2)
library(tidyverse)

## Data Reading
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/Uzimtumo tarnyba")

file_list <- list.files(pattern = "*.csv")
data_list <- list() # Initialize an empty list

for (i in seq_along(file_list)) {
  
  data_list[[i]] <- read.delim(
    file_list[i], 
    fileEncoding = "UTF-8", 
    stringsAsFactors = FALSE
  )
  
  # Optional: Progress message
  message("Processed file: ", file_list[i])
}

# 2. Combine all data frames
final_df <- do.call(rbind, data_list)

final_df <- final_df %>%
  mutate(Asmens_registracijos_data = ymd(Asmens_registracijos_data)) %>%
  mutate(Asmens_isregistravimo_data = ymd(Asmens_isregistravimo_data))

bedarbiai <- final_df  %>%
  filter(Asmens_statusas == "Bedarbis")

nebedarbiai <- final_df %>%
  filter(Asmens_statusas != "Bedarbis")

nebebarbiai_daily <- nebedarbiai %>%
  group_by(Asmens_registracijos_data) %>%
  summarise(n = n())

ggplot(data = nebebarbiai_daily, aes(x = Asmens_registracijos_data, y = n)) +
  geom_line() +
  labs(title = "Asmens statutas ne bedarbis") +
  theme_minimal()

## Calculating the numbers of persons registered per day
dayCounts <- final_df %>%
  filter(Asmens_statusas  == "Bedarbis") %>%
  group_by(Asmens_registracijos_data) %>%
  summarise(n = n())

# Plotting
ggplot(data = dayCounts, aes(x = Asmens_registracijos_data, y = n)) +
  geom_line() +
  theme_minimal()

# Filtering out days before 2000
dayCounts_after2000 <- dayCounts %>%
  filter(Asmens_registracijos_data >= as.Date("2000-01-01"))

ggplot(data = dayCounts_after2000, aes(x = Asmens_registracijos_data, y = n)) +
  geom_line() +
  theme_minimal()

## Calculating the numbers of persons unregistered per day
# Filtered for those, that have found a job
dayCountsUnregistered <- bedarbiai %>%
  filter(Asmens_isregistravimo_priezastis %in% c("Darbo paieškos nutraukimas įsidarbinus per Užimtumo tarnybą",
                                               "Darbo paieškos nutraukimas įsidarbinus savarankiškai, UŽT suteikus darbo rinkos paslaugas",
                                               "Darbo paieškos nutraukimas pradėjus savo verslą",
                                               "Darbo paieškos nutraukimas įsidarbinus į steigiamą darbo vietą",
                                               "Darbo paieškos nutraukimas įsidarbinus į kvotinę / įdarbinimo darbo vietą",
                                               "Darbo paieškos nutraukimas pradėjus savo verslą su paskola",
                                               "Darbo paieškos nutraukimas, įsidarbinus į subsidijuojamą ne kvotinę darbo vietą",
                                               "Darbo paieškos nutraukimas įsidarbinus pagal trišalę mokymo sutartį",
                                               "Darbo paieškos nutraukimas, įdarbinus subsidijuojant",
                                               "DP nutraukimas, įdarbinus darbo įgūdžių įgijimui",
                                               "Darbo paieškos nutraukimas, įsidarbinus į subsidijuojamą kvotinę darbo vietą",
                                               "Darbo paieškos nutraukimas, įsidarbinus į VUIP įsteigtas DV (A1-473 25.1 p.)",
                                               "DP nutraukimas, įdarbinus į įsteigtą DV neįgaliems",
                                               "Darbo paieškos nutraukimas, įdarbinus darbo rotacijos būdu",
                                               "Darbo paieškos nutraukimas, įdarbinus subsidijuojant, kurių darbingumo lygis 20-40%",
                                               "Darbo paieškos nutraukimas, įsidarbinus į socialinių įmonių įsteigtas DV (A1-473 25.1 p.)",
                                              # "Pasibaigus COVID19 priemonių finansavimui",
                                               "DP nutraukimas, įsidarbinus į savo įsteigtą DV neįgaliems",                                                                                                                                                                                                   
                                               "DP nutraukimas, įdarbinus į buvusio bedarbio įsteigtą DV neįgaliems",
                                               "Darbo paieškos nutraukimas palikus dirbti esamoje darbovietėje"
                                               )) %>%
  group_by(Asmens_isregistravimo_data) %>%
  summarise(n_un = n()) %>%
  filter(Asmens_isregistravimo_data >= as.Date("2000-01-01"))

ggplot(data = dayCountsUnregistered, aes(x = Asmens_isregistravimo_data, y = n_un)) +
  geom_line() +
  theme_minimal()

## Checking for the missing days in both data sets
# Setting up a full calendar
startDate <- as.Date("2000-01-01")
endDate <- as.Date("2025-12-18")

full_calendar <- data.frame(
  Asmens_registracijos_data = seq.Date(startDate, endDate, by = "day"),
  Asmens_isregistravimo_data = seq.Date(startDate, endDate, by = "day")
)

gaps_reg <- full_calendar %>%
  left_join(dayCounts, by = "Asmens_registracijos_data") %>%
  filter(is.na(n)) %>%
  select(-c(Asmens_isregistravimo_data))

gaps_unreg <- full_calendar %>%
  left_join(dayCountsUnregistered, by = "Asmens_isregistravimo_data") %>%
  filter(is.na(n_un)) %>%
  select(-c(Asmens_registracijos_data))


# Joining two data sets
combined_counts <- full_join(
  dayCounts_after2000, 
  dayCountsUnregistered, 
  by = c("Asmens_registracijos_data" = "Asmens_isregistravimo_data")
) %>%
  rename(Date = Asmens_registracijos_data) %>%
  mutate(iso_year = isoyear(Date),
         iso_week = isoweek(Date))

# 2. Plot both lines together
ggplot(combined_counts, aes(x = Date)) +
  geom_line(aes(y = n, color = "Registered"), alpha = 0.7) +
  geom_line(aes(y = n_un, color = "Unregistered"), alpha = 0.7) +
  scale_color_manual(values = c("Registered" = "steelblue", "Unregistered" = "firebrick")) +
  labs(#title = "Lithuanian Employment Service Unemployed people flow in and out of registration",
       y = "Number",
       color = "Legend",
       caption = "Note: Data from Lithuanian Employment Service, work my own. Data in graph up to 2026-03-01.") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Writing the data
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/Uzimtumo tarnyba/Adjrez")

write.csv(combined_counts, "UT_flows_unadjusted.csv")


