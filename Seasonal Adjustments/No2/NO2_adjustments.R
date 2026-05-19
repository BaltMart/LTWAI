library(dsa)
library(tidyverse)
library(ggplot2)
library(seastests)
library(zoo)
library(timeDate)
library(xts)

# Functions
set_of_seastests <- function(x) {
  fried365 <- seastests::fried(dsa::xts2ts(x, 365), freq=365)  
  qs365 <- seastests::qs(dsa::xts2ts(x, 365), freq=365) 
  fried12 <- seastests::fried(dsa:::.to_month(x), freq=12)
  qs12 <- seastests::qs(dsa:::.to_month(x), freq=12)
  fried7 <- seastests::fried(xts::last(x,70), freq=7)
  qs7 <- seastests::qs(xts::last(x,70), freq=7)
  fried7_all <- seastests::fried(x, freq=7)
  qs7_all <- seastests::qs(x, freq=7)
  
  stats <- round(c(fried365$stat, qs365$stat, fried12$stat, qs12$stat, 
                   fried7$stat, qs7$stat, fried7_all$stat, qs7_all$stat),1)
  
  pvals <- round(c(fried365$Pval, qs365$Pval, fried12$Pval, qs12$Pval, 
                   fried7$Pval, qs7$Pval, fried7_all$Pval, qs7_all$Pval),3)
  
  out <- cbind(stats, pvals)
  rownames(out) <- c("Friedman365", "QS365", "Friedman12", "QS12", 
                     "Friedman7", "QS7", "Friedman7all", "QS7all")
  colnames(out) <- c("Teststat", "P-value")
  return(out)
}

all_seas <- function(x, ...) {
  if (missing(...)) {
    bc <- x
  } else {
    bc <- list(x, ...)
  }
  
  out <- set_of_seastests(bc[[1]])
  if (length(bc)>1) {
    for (j in 2:length(bc)) {
      out <- cbind(out, set_of_seastests(bc[[j]]))
    }
  }
  return(out)
}

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
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/NO2")

# Loading the data
df <- read.csv("no2_daily_unadjusted.csv")
df$Day <- as.Date(df$Day, format = "%Y-%m-%d")

no2 <- df$NO2_avg
#no2 <- no2[!is.na(no2)]

na_dates <- df$Day[is.na(df$NO2_avg)]
na_dates <- as.Date(na_dates)

no2_dates <- df$Day
no2_xts <- xts(no2, order.by = no2_dates)
# Interpolation
no2_NA <- xts::xts(as.numeric(ts(no2_xts, frequency=7)), zoo::index(no2_xts))
#no2_spline <- xts::xts(as.numeric(zoo::na.spline(ts(no2_xts, frequency=7))), zoo::index(no2_xts))
no2 <- xts::xts(as.numeric(forecast::na.interp(ts(no2_xts, frequency=7))), zoo::index(no2_xts))

# Creating reference series
reference_series <- no2
restrict <- seq.Date(from = start(reference_series), 
                     to = end(reference_series), by = "days")
restrict_forecast <- seq.Date(from = end(reference_series) + 1,
                              length.out = 365, by = "days")

all_dates <- seq.Date(
  from = min(restrict),
  to = max(restrict_forecast),
  by = "day"
)

# Creating holidays
holidays_lt <- create_lithuanian_holidays_detailed(all_dates)

# Removing from the holidays the days that were interpolated
holidays_lt_mat <- coredata(holidays_lt)
holidays_lt_mat[zoo::index(holidays_lt) %in% na_dates, ] <- 0
holidays_lt <- xts(holidays_lt_mat, order.by = zoo::index(holidays_lt))

# Removing holidays not statistically significant
holidays_lt_mat <- holidays_lt_mat[, !colnames(holidays_lt_mat) %in% not_significant_hol]
holidays_lt <- xts(holidays_lt_mat, order.by = zoo::index(holidays_lt))

# Creating end use matrices
AllHolUse <- multi_xts2ts(holidays_lt[restrict])
AllHolForecast <- multi_xts2ts(holidays_lt[restrict_forecast], short=TRUE)

AllHolForecast <- AllHolForecast[,colSums(AllHolUse)!=0]
AllHolUse <- AllHolUse[,colSums(AllHolUse)!=0]

# Running the adjustment
no2_sa <- dsa(no2, 
               Log = FALSE, 
               s.window1 = 31, s.window3 = 13, 
               fourier_number = 1, 
               regressor = AllHolUse, 
               forecast_regressor = AllHolForecast, 
               robust3 = FALSE, 
               feb29 = "sfac",
               progress_bar = TRUE)

dsa_no2_out <- get_sa(no2_sa)

# Extracting factors to save and use for later adjustments
Factors <- no2_sa$sfac_result

adjustment_factors <- as.data.frame(Factors) %>%
  mutate(Date = index(Factors))

write.csv(adjustment_factors, "Adjustment_factors_no2.csv", row.names = FALSE)

# Looking at the results
plot(no2_sa)

all_seas(no2, dsa_no2_out)

# Checking the regressor pvalues
coefficients <- coef(no2_sa$reg)
std_errors <- sqrt(diag(no2_sa$reg$var.coef))
t_stats <- coefficients / std_errors
p_values <- 2 * (1 - pnorm(abs(t_stats)))

# Create a table
regressor_stats <- data.frame(
  Coefficient = round(coefficients, 2),
  Std_Error = round(std_errors,2),
  T_Statistic = round(t_stats,2),
  P_Value = round(p_values, 2)
)
print(regressor_stats)

# Selecting regressors that are not statistically significant and so should be removed from the estimation
not_significant_hol <- c("Feb16_Mon", "May1_Mon", "Jun24_Mon",  "Jul6_Mon","Nov1_Mon",  "Nov2_Mon",
                         "Dec24_Mon", "Dec25_Mon", "Feb16_Tue", "Jul6_Tue", "Aug15_Tue",
                         "Dec24_Tue", "Dec25_Tue", "Feb16_Wed", "Mar11_Wed", "May1_Wed", "Jun24_Wed",
                         "Jul6_Wed", "Nov1_Wed", "Dec24_Wed", "NewYear_Thu", "Jul6_Thu", "Nov2_Thu",
                         "Dec24_Thu", "Dec26_Thu", "Feb16_Fri", "Mar11_Fri", "Jun24_Fri", "Jul6_Fri",
                         "Aug15_Fri", "Nov1_Fri", "Dec24_Fri", "Dec26_Fri", "NewYear_Sat", "Feb16_Sat",
                         "Mar11_Sat", "May1_Sat", "Jun24_Sat", "Jul6_Sat", "Aug15_Sat", "Nov1_Sat",
                         "Nov2_Sat", "Dec24_Sat", "Dec25_Sat", "Dec26_Sat", "NewYear_Sun", "Feb16_Sun",
                         "Mar11_Sun", "May1_Sun", "Jun24_Sun", "Jul6_Sun", "Aug15_Sun", "Nov1_Sun", 
                         "Nov2_Sun", "Dec24_Sun", "Dec25_Sun", "EasterSun", "BridgeDay")

# Plots
g1 <- xtsplot(merge(no2, dsa_no2_out)["2015/"], 
              names=c("Original","DSA"), color = c("blue", "red"),
              main="Comparison of seasonal adjustment result for NO2 daily average in Lithuania", 
              submain="From 2015",
              linesize=0.75) + 
  ggplot2::theme(legend.position="None")

g2 <- xtsplot(merge(no2, dsa_no2_out)["2023/"], 
              names=c("Original", "DSA"), color = c("blue", "red"),
              main="Comparison of seasonal adjustment result", 
              submain="From 2019",
              linesize=0.75) + 
  ggplot2::theme(legend.position="None", plot.title = ggplot2::element_blank())

g3 <- xtsplot(merge(no2, dsa_no2_out)["2020-01-01/2020-03-31"], 
              names=c("Original", "DSA"), color = c("blue", "red"),
              main="Comparison of seasonal adjustment result", 
              submain="2020-01-01 to 2020-03-31",
              linesize=0.75) + 
  ggplot2::theme(plot.title = ggplot2::element_blank()) +
  ggplot2::labs(caption = "Note: Data from EEA. Calculations my own.")

gridExtra::grid.arrange(g1, g2, g3, layout_matrix=matrix(c(1,1,2,2,3,3,3), ncol=1))

# plot 2
g1 <- xtsplot(no2_sa$output[,c(2,1)]["2013/2026-02"], 
              color=c("#9c9e9f", "darkred"), 
              main="Original and Seasonally adjusted series of NO2 daily average in Lithuania", 
              names=c("Original", "Adjusted")) + 
  ggplot2::theme(legend.position = c(0.175, 0.775))
g2 <- xtsplot(no2_sa$sfac_result[,1]["2013/2026-02"], 
              color="#0062a1", 
              main="Intra-weekly seasonal component", 
              linesize=0.3) + 
  ggplot2::theme(legend.position = "None")
g3 <- xtsplot(no2_sa$sfac_result[,2]["2013/2026-02"], 
              color="#0062a1", 
              main="Moving holiday effect") + 
  ggplot2::theme(legend.position = "None")
g5 <- xtsplot(no2_sa$sfac_result[,4]["2013/2026-02"], 
              color="#0062a1", 
              main="Intra-annual seasonal component") + 
  ggplot2::theme(legend.position = "None") + 
  ggplot2::labs(caption = "Note: Data from EEA. Calculations my own.")

gridExtra::grid.arrange(g1, g2, g3, g5, nrow=4)


# Creating a data frame to export as an adjusted series
df_to_write <- as.data.frame(dsa_no2_out) %>%
  mutate(Date = zoo::index(dsa_no2_out)) %>%
  mutate(No2_adj = seas_adj) %>%
  select(Date, No2_adj)

rownames(df_to_write) <- NULL

write.csv(df_to_write, "NO2_adjusted.csv", row.names = FALSE)





