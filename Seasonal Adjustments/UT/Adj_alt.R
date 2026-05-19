library(tidyverse)
library(ggplot2)
library(seastests)
library(zoo)
library(timeDate)
library(xts)
library(dsa2)
# Functions
create_lithuanian_holidays_detailed <- function(dates) {
  years <- unique(year(dates))
  
  # Holidays always on the same calendar day
  fixed_holidays <- c("NewYear", "Feb16", "Mar11", "May1", "Jun24", 
                      "Jul6", "Aug15", "Nov1", "Nov2", "Dec24", "Dec25", "Dec26")
  
  days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  all_col_names <- as.vector(outer(fixed_holidays, days, paste, sep = "_"))
  
  # Adding Easter and the general BridgeDay column
  all_col_names <- c(all_col_names, "EasterSun", "EasterMon", "BridgeDay")
  
  result <- matrix(0, nrow = length(dates), ncol = length(all_col_names))
  colnames(result) <- all_col_names
  
  # Helper to identify if a date is a weekend
  is_weekend <- function(d) { wday(d, week_start = 1) %in% c(6, 7) }
  
  for (yr in years) {
    easter <- as.Date(timeDate::Easter(yr))
    
    # 1. First, identify all actual holidays for the year to avoid "Bridge Overlap"
    # We create a temporary list of holiday dates for this year
    year_hols <- as.Date(c(
      paste0(yr, c("-01-01", "-02-16", "-03-11", "-05-01", "-06-24", 
                   "-07-06", "-08-15", "-11-01", "-12-24", "-12-25", "-12-26")),
      if(yr >= 2020) paste0(yr, "-11-02"),
      as.character(easter), 
      as.character(easter + 1)
    ))
    
    mark <- function(target_date, base_name) {
      target_date <- as.Date(target_date)
      idx <- which(dates == target_date)
      
      if(length(idx) > 0) {
        wd <- wday(target_date, week_start = 1)
        day_label <- days[wd]
        
        # Mark the specific Holiday Column
        full_name <- if (base_name %in% c("EasterSun", "EasterMon")) base_name else paste0(base_name, "_", day_label)
        result[idx, full_name] <<- 1
        
        # --- BRIDGE DAY LOGIC (Option 2B & 4) ---
        # If Holiday is Tuesday, check if Monday is a Bridge
        if (wd == 2) {
          bridge_date <- target_date - 1
          b_idx <- which(dates == bridge_date)
          # Only mark if it's not a holiday and not a weekend
          if (length(b_idx) > 0 && !(bridge_date %in% year_hols) && !is_weekend(bridge_date)) {
            result[b_idx, "BridgeDay"] <<- 1
          }
        }
        
        # If Holiday is Thursday, check if Friday is a Bridge
        if (wd == 4) {
          bridge_date <- target_date + 1
          b_idx <- which(dates == bridge_date)
          # Only mark if it's not a holiday and not a weekend
          if (length(b_idx) > 0 && !(bridge_date %in% year_hols) && !is_weekend(bridge_date)) {
            result[b_idx, "BridgeDay"] <<- 1
          }
        }
      }
    }
    
    # Mark Fixed Holidays
    mark(paste0(yr, "-01-01"), "NewYear")
    mark(paste0(yr, "-02-16"), "Feb16")
    mark(paste0(yr, "-03-11"), "Mar11")
    mark(paste0(yr, "-05-01"), "May1")
    mark(paste0(yr, "-06-24"), "Jun24")
    mark(paste0(yr, "-07-06"), "Jul6")
    mark(paste0(yr, "-08-15"), "Aug15")
    mark(paste0(yr, "-11-01"), "Nov1")
    if (yr >= 2020) mark(paste0(yr, "-11-02"), "Nov2")
    mark(paste0(yr, "-12-24"), "Dec24")
    mark(paste0(yr, "-12-25"), "Dec25")
    mark(paste0(yr, "-12-26"), "Dec26")
    
    # Mark Easter
    mark(easter, "EasterSun")
    mark(easter + 1, "EasterMon")
  }
  
  # Remove unused columns but keep BridgeDay if it has values
  result <- result[, colSums(result) > 0, drop = FALSE]
  return(xts(result, order.by = dates))
}

# Setting the working directory
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/Uzimtumo tarnyba/Adjrez")

# Loading the data
df <- read.csv("UT_flows_unadjusted.csv")
df$Date <- as.Date(df$Date, format = "%Y-%m-%d")

###################
# Unemployment flow

n_unemployed <- df$n
n_days <- df$Date
unemployed_xts <- xts(n_unemployed, order.by = n_days)
unemployed_xts[is.na(unemployed_xts)] <- 1
unemployed_xts[unemployed_xts < 1] <- 1

unemployed_xts <- log(unemployed_xts)

# Create reference series
reference_series <- unemployed_xts
restrict <- seq.Date(from = start(reference_series), 
                     to = end(reference_series), by = "days")
restrict_forecast <- seq.Date(from = end(reference_series) + 1,
                              length.out = 365, by = "days")

all_dates <- seq.Date(
  from = min(restrict),
  to = max(restrict_forecast),
  by = "day"
)

# Create Lithuanian holidays
holidays_lt <- create_lithuanian_holidays_detailed(all_dates)

# Similarly to the example creating matrices for dummy variables
AllHolUse <- holidays_lt[all_dates]
in_sample_cols <- colSums(coredata(AllHolUse[restrict])) != 0
AllHolUse <- AllHolUse[, in_sample_cols]
AllHolUse_matrix <- as.matrix(AllHolUse)

result0 <- dsa(unemployed_xts, xreg = AllHolUse, log = FALSE)

pre_series <- result0$series

result1 <- dsa(unemployed_xts, 
              log = FALSE,
              s7 = stl_method(swindow = 3),
              s31 = "x11",
              n_iterations = 3,
              pre_processing = result0)

final_series <- result1$series

# Plotting results
plot(result1)
plot_interactive(result1)

# Seasonality tests
seastests::qs(ts(coredata(final_series$seas_adj), frequency = 7), freq = 7)
seastests::qs(ts(coredata(final_series$original), frequency = 7), freq = 7)

seastests::fried(ts(coredata(final_series$seas_adj), frequency = 7), freq = 7)
seastests::fried(ts(coredata(final_series$original), frequency = 7), freq = 7)

seastests::qs(ts(coredata(pre_series$seas_adj), frequency = 12), freq = 12)
seastests::qs(ts(coredata(pre_series$original), frequency = 12), freq = 12)

seastests::fried(ts(coredata(pre_series$seas_adj), frequency = 12), freq = 12)
seastests::fried(ts(coredata(pre_series$original), frequency = 12), freq = 12)

seastests::qs(ts(coredata(pre_series$seas_adj), frequency = 365), freq = 365)
seastests::qs(ts(coredata(pre_series$original), frequency = 365), freq = 365)

seastests::fried(ts(coredata(final_series$seas_adj), frequency = 365), freq = 365)
seastests::fried(ts(coredata(final_series$original), frequency = 365), freq = 365)

