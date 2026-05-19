library(tidyverse)
library(ggplot2)
library(lubridate)
library(zoo)
library(gtrendsR)

# Functions


# Setting the working directory
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/GOOGLE Trends")

file_list <- list.files(pattern = "*.csv")
data_list <- list()

# Loading the data
for (i in seq_along(file_list)) {
  data_list[[i]] <- read.csv(file_list[i])
  
  message("Processed file: ", file_list[i])
}

# Chain-Linking my Google trends data
chain_linked <- data_list[[1]] %>%
  mutate(chain_value = Nedarbas)

for (i in 2:length(data_list)) {
  
  curr <- data_list[[i]]
  
  # Find ALL overlapping weeks
  overlap_weeks <- curr %>%
    filter(Time %in% chain_linked$Time) %>%
    pull(Time)
  
  cat(sprintf("\nFile %d: Found %d overlapping weeks\n", i, length(overlap_weeks)))
  
  # Get values for overlapping period
  prev_overlap <- chain_linked %>%
    filter(Time %in% overlap_weeks) %>%
    arrange(Time)
  
  curr_overlap <- curr %>%
    filter(Time %in% overlap_weeks) %>%
    arrange(Time)
  
  # Calculate scaling factor as the MEAN ratio across all overlap weeks
  ratios <- prev_overlap$chain_value / curr_overlap$Nedarbas
  
  # Remove infinite/NaN values (from zeros)
  ratios <- ratios[is.finite(ratios)]
  
  scaling_factor <- mean(ratios)
  
  cat(sprintf("  Scaling factor (mean of %d ratios): %.4f\n", 
              length(ratios), scaling_factor))
  cat(sprintf("  Range of ratios: %.4f to %.4f\n", 
              min(ratios), max(ratios)))
  
  # Rescale the NON-overlapping part of current file
  rescaled <- curr %>%
    filter(!Time %in% overlap_weeks) %>%  # only new weeks
    mutate(chain_value = Nedarbas * scaling_factor)
  
  # Append to chain
  chain_linked <- bind_rows(chain_linked, rescaled)
}


# Reading the monthly version from google for comparison
mnthly <- read.csv("C:/Users/Lenovo/Downloads/Monthly_gtrend.csv")

mnthly <- mnthly %>%
  rename(Time = 1, monthly_value = 2) %>%
  mutate(Time = as.Date(Time))

# Plotting
chain_linked <- chain_linked %>%
  mutate(Time = as.Date(Time)) %>%
  mutate(chain_value_rescaled = (chain_value / max(chain_value)) * 100)

ggplot() +
  geom_line(data = chain_linked, 
            aes(x = Time, y = chain_value_rescaled, colour = "Weekly (chain-linked)"), 
            linewidth = 0.8) +
  geom_line(data = mnthly, 
            aes(x = Time, y = monthly_value, colour = "Monthly (Google)"), 
            linewidth = 0.8) +
  scale_colour_manual(values = c("Weekly (chain-linked)" = "red", 
                                 "Monthly (Google)" = "steelblue")) +
  labs(
    title = "Chain-Linked Weekly vs Monthly Google Trends",
    x = NULL,
    y = "Value (peak = 100)",
    colour = NULL,
    caption = "Note: Data obtained from Google Trends. Chain-linking calculation my own.Data in graph up to 2026-03-15."
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Writing the connected data series
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/GOOGLE Trends/Linked Data")

to_write <- chain_linked %>%
  mutate(Nedarbas_gtrend = chain_value_rescaled) %>%
  select(Time, Nedarbas_gtrend)

write.csv(to_write, "gtrend_nedarbas.csv", row.names = FALSE)

  






