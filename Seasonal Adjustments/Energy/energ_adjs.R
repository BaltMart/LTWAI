library(dsa)
library(tidyverse)
library(ggplot2)
library(seastests)
library(zoo)
library(timeDate)
library(xts)
library(zoo)

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

# Setting the working directory of the data
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/ENERGY/Unadjusted")

# Loading the data
df <- read.csv("Energy_per_day_unadjusted.csv")
df$Day <- as.Date(df$Day, format = "%Y-%m-%d")

df <- df %>%
  filter(Day >= as.Date("2015-01-05")) # Here I filter out a week of missing data that was causing me issues

# Loading
elec <- df$Total_load
elec_dates <- df$Day
elec_xts <- xts(elec, order.by = elec_dates)

# Create reference series
reference_series <- elec_xts
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

# Similarly to the example creating matrixes for dummy variables
AllHolUse <- multi_xts2ts(holidays_lt[restrict])
AllHolForecast <- multi_xts2ts(holidays_lt[restrict_forecast], short=TRUE)

AllHolForecast <- AllHolForecast[,colSums(AllHolUse)!=0]
AllHolUse <- AllHolUse[,colSums(AllHolUse)!=0]


# Running the DSA
elec_sa <- dsa(elec_xts, 
               Log = FALSE, 
               s.window1 = 13, s.window3 = 13, 
               fourier_number = 26, 
               regressor = AllHolUse, 
               forecast_regressor = AllHolForecast, 
               robust3 = FALSE, 
               feb29 = "sfac",
               progress_bar = TRUE)

dsa_elec_out <- get_sa(elec_sa)

# Extracting factors to save and use for later adjustments
Factors <- elec_sa$sfac_result

adjustment_factors <- as.data.frame(Factors) %>%
  mutate(Date = zoo::index(Factors))

write.csv(adjustment_factors, "Adjustment_factors_elec.csv", row.names = FALSE)

# Creating a frame to plot with ggplot
combined_xts <- cbind(elec_xts, dsa_elec_out)

# Convert to data frame, then use mutate
combined_df <- as.data.frame(combined_xts) %>%
  mutate(Date = zoo::index(combined_xts))

ggplot(combined_df, aes(x = Date)) + 
  geom_line(aes(y = elec_xts, color = "Unadjusted")) +
  geom_line(aes(y = seas_adj, color = "Adjusted")) +
  theme_minimal()

all_seas(elec_xts, dsa_elec_out)

plot(elec_sa)

# Checking regressors
coefficients <- coef(elec_sa$reg)
std_errors <- sqrt(diag(elec_sa$reg$var.coef))
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

not_significant_hol <- c("Dec24_Wed", "Feb16_Sat", "Mar11_Sat", "May1_Sat", "Jun24_Sat",
                         "Aug15_Sat", "Nov2_Sat", "Dec24_Sat",
                         "Feb16_Sun", "Mar11_Sun", "May1_Sun", "Jun24_Sun", "Jul6_Sun",
                         "Aug15_Sun", "Nov2_Sun","Dec24_Sun", "Dec26_Sun", "Dec25_Sun", "Nov2_Sun")

holidays_lt_mat <- coredata(holidays_lt)
holidays_lt_mat <- holidays_lt_mat[, !colnames(holidays_lt_mat) %in% not_significant_hol]
holidays_lt <- xts(holidays_lt_mat, order.by = zoo::index(holidays_lt))

# Plots from DSA vignette
# plot 1
g1 <- xtsplot(merge(elec_xts, dsa_elec_out)["2015/"]/1e6, 
              names=c("Original", "DSA"), color = c("darkgrey",  "red"),
              main="Seasonal adjustment result of electricity usage in Lithuania", 
              submain="From 2015",
              linesize=0.75) + 
  ggplot2::theme(legend.position="None")

g2 <- xtsplot(merge(elec_xts, dsa_elec_out)["2019/"]/1e6, 
              names=c("Original", "DSA"), color = c("darkgrey", "red"),
              main="Seasonal adjustment result", 
              submain="From 2019",
              linesize=0.75) + 
  ggplot2::theme(legend.position="None", plot.title = ggplot2::element_blank())

g3 <- xtsplot(merge(elec_xts, dsa_elec_out)["2020-02-01/2020-04-30"]/1e6, 
              names=c("Original", "DSA"), color = c("darkgrey", "red"),
              main="Seasonal adjustment result", 
              submain="2020-02-01 to 2020-04-31",
              linesize=0.75) + 
  ggplot2::theme(plot.title = ggplot2::element_blank()) +
  ggplot2::labs(caption = "Note: Data from ENTSO-E. Calculations my own.")

gridExtra::grid.arrange(g1, g2, g3, layout_matrix=matrix(c(1,1,2,2,3,3,3), ncol=1))


# plot 2
g1 <- xtsplot(elec_sa$output[,c(2,1)]["2015/2026-02"], 
              color=c("#9c9e9f", "darkred"), 
              main="Original and Seasonally adjusted series of electricity usage in Lithuania", 
              names=c("Original", "Adjusted")) + 
  ggplot2::theme(legend.position = c(0.175, 0.775))
g2 <- xtsplot(elec_sa$sfac_result[,1]["2015/2026-02"], 
              color="#0062a1", 
              main="Intra-weekly seasonal component", 
              linesize=0.3) + 
  ggplot2::theme(legend.position = "None")
g3 <- xtsplot(elec_sa$sfac_result[,2]["2015/2026-02"], 
              color="#0062a1", 
              main="Moving holiday effect") + 
  ggplot2::theme(legend.position = "None")
g5 <- xtsplot(elec_sa$sfac_result[,4]["2015/2026-02"], 
              color="#0062a1", 
              main="Intra-annual seasonal component") + 
  ggplot2::theme(legend.position = "None") + 
  ggplot2::labs(caption = "Note: Data from ENTSO-E. Calculations my own.")

gridExtra::grid.arrange(g1, g2, g3, g5, nrow=4)

# Writing the adjusted series
elektra <- combined_df %>%
  mutate(elec_adj = seas_adj) %>%
  select(Date, elec_adj)

write.csv(elektra, "Energy_adjusted.csv", row.names = FALSE)
