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
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/Uzimtumo tarnyba/Adjrez")

# Loading the data
df <- read.csv("UT_flows_unadjusted.csv")
df$Date <- as.Date(df$Date, format = "%Y-%m-%d")

# Testing: Removing the last observation to see if it causes issues
#df <- df %>%
#  filter(Date < as.Date("2026-02-13"))

###################
# Unemployment flow
n_unemployed <- df$n
n_days <- df$Date
unemployed_xts <- xts(n_unemployed, order.by = n_days)

na_dates <- df$Date[is.na(df$n)]
na_dates <- as.Date(na_dates)

unem_NA <- xts::xts(as.numeric(ts(unemployed_xts, frequency=7)), zoo::index(unemployed_xts))
unemployed_xts <- xts::xts(as.numeric(forecast::na.interp(ts(unemployed_xts, frequency=7))), zoo::index(unemployed_xts))
unemployed_xts[unemployed_xts < 1] <- 1

#unemployed_xts <- log(unemployed_xts) 

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
holidays_lt_mat <- coredata(holidays_lt)
holidays_lt_mat[index(holidays_lt) %in% na_dates, ] <- 0
holidays_lt <- xts(holidays_lt_mat, order.by = index(holidays_lt))

# note: this step only after initial run
# removing the insignificant regressors
holidays_lt_mat <- holidays_lt_mat[, !colnames(holidays_lt_mat) %in% not_significant_inflow_hol]
holidays_lt <- xts(holidays_lt_mat, order.by = index(holidays_lt))

# Similarly to the example creating matrices for dummy variables
AllHolUse <- multi_xts2ts(holidays_lt[restrict])
AllHolForecast <- multi_xts2ts(holidays_lt[restrict_forecast], short=TRUE)

# Removing unused columns 
AllHolForecast <- AllHolForecast[,colSums(AllHolUse)!=0]
AllHolUse <- AllHolUse[,colSums(AllHolUse)!=0]

# Running the adjustments
unemp_sa <- dsa(unemployed_xts, 
               Log = FALSE, 
               s.window1 = 21, s.window2 = 7, s.window3 = 7,
               regressor = AllHolUse,  
               forecast_regressor = AllHolForecast, 
               robust3 = FALSE, 
               feb29 = "sfac",
               #mean_correction = TRUE,
               progress_bar = TRUE)

dsa_unemp <- get_sa(unemp_sa)

all_seas(unemployed_xts, dsa_unemp)

plot(unemp_sa)

Factors <- unemp_sa$sfac_result

# Coefficients of regressors
coefficients <- coef(unemp_sa$reg)
std_errors <- sqrt(diag(unemp_sa$reg$var.coef))
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

not_significant_outflow_hol <- c("Feb16_Mon", "Dec24_Mon", "Dec25_Mon",
                                 "Dec26_Mon", "Nov2_Tue", "Dec26_Tue", "NewYear_Wed",
                                 "Feb16_Wed", "Jun24_Wed", "Aug15_Wed", "Dec24_Wed",
                                 "Dec26_Wed", "NewYear_Thu", "Feb16_Thu", "Mar11_Thu",
                                 "Jun24_Thu", "Jul6_Thu", "Aug15_Thu", "Dec24_Thu", "Dec25_Thu",
                                 "Dec26_Thu", "NewYear_Fri", "Feb16_Fri", "Mar11_Fri", "May1_Fri",
                                 "Jun24_Fri", "Aug15_Fri", "Nov1_Fri", "Dec24_Fri", "Dec25_Fri",
                                 "Dec26_Fri", "Feb16_Sat", "Mar11_Sat", "Jun24_Sat", "Jul6_Sat",
                                 "Aug15_Sat", "Nov1_Sat", "Nov2_Sat", "Dec24_Sat", "Dec25_Sat", 
                                 "Dec26_Sat", "NewYear_Sun", "Feb16_Sun", "Mar11_Sun", "Jun24_Sun",
                                 "Aug15_Sun", "Dec24_Sun", "Dec26_Sun")

not_significant_inflow_hol <- c("Feb16_Sat", "Mar11_Sat", "Jun24_Sat", "Jul6_Sat",
                                 "Aug15_Sat", "Nov1_Sat", "Nov2_Sat", "Dec24_Sat", 
                                 "Dec26_Sat", "Feb16_Sun", "Mar11_Sun", "May1_Sun","Jun24_Sun",
                                 "Aug15_Sun", "Nov1_Sun", "Nov2_Sun", "Dec26_Sun")

# Plot 1
g1 <- xtsplot(merge(unemployed_xts, dsa_unemp)["2000/"]/1e6, 
              names=c("Original", "DSA"), color = c("darkgrey",  "red"),
              main="Seasonal adjustment result of flow into unemployment", 
              submain="From 2000",
              linesize=0.75) + 
  ggplot2::theme(legend.position="None")

g2 <- xtsplot(merge(unemployed_xts, dsa_unemp)["2019-01-01/2019-12-31"]/1e6, 
              names=c("Original", "DSA"), color = c("darkgrey", "red"),
              main="Seasonal adjustment result", 
              submain="From 2019",
              linesize=0.75) + 
  ggplot2::theme(legend.position="None", plot.title = ggplot2::element_blank())

g3 <- xtsplot(merge(unemployed_xts, dsa_unemp)["2020-01-01/2020-03-31"]/1e6, 
              names=c("Original", "DSA"), color = c("darkgrey", "red"),
              main="Seasonal adjustment result", 
              submain="2020-01-01 to 2020-03-31",
              linesize=0.75) + 
  ggplot2::theme(plot.title = ggplot2::element_blank())  +
  ggplot2::labs(caption = "Note: Data from Užimtumo Tarnyba. Calculations my own.")
gridExtra::grid.arrange(g1, g2, g3, layout_matrix=matrix(c(1,1,2,2,3,3,3), ncol=1))


# plot 2
g1 <- xtsplot(unemp_sa$output[,c(2,1)]["2000/2026-02"], 
              color=c("#9c9e9f", "darkred"), 
              main="Original and Seasonally adjusted series of flows into unemployment", 
              names=c("Original", "Adjusted")) + 
  ggplot2::theme(legend.position = c(0.175, 0.775))
g2 <- xtsplot(unemp_sa$sfac_result[,1]["2000/2026-02"], 
              color="#0062a1", 
              main="Intra-weekly seasonal component", 
              linesize=0.3) + 
  ggplot2::theme(legend.position = "None")
g3 <- xtsplot(unemp_sa$sfac_result[,2]["2000/2026-02"], 
              color="#0062a1", 
              main="Moving holiday effect") + 
  ggplot2::theme(legend.position = "None")
g4 <- xtsplot(unemp_sa$sfac_result[,3]["2000/2026-02"], 
              color="#0062a1", 
              main="Intra-monthly seasonal component") + 
  ggplot2::theme(legend.position = "None")
g5 <- xtsplot(unemp_sa$sfac_result[,4]["2000/2026-02"], 
              color="#0062a1", 
              main="Intra-annual seasonal component") + 
  ggplot2::theme(legend.position = "None") +
  ggplot2::labs(caption = "Note: Data from Užimtumo Tarnyba. Calculations my own.")
gridExtra::grid.arrange(g1, g2, g3, g4, g5, nrow=5)

# Making final adjustements of data before writing
#dsa_unemp_real <- exp(dsa_unemp)

#dsa_unemp[dsa_unemp < 1] <- 1

# Creating data frame to write into csv
adjusted_df <- as.data.frame(dsa_unemp) %>%
  mutate(Date = index(dsa_unemp))

flow <- adjusted_df %>%
  mutate(unemp_adj = seas_adj) %>%
  select(Date, unemp_adj)

write.csv(flow, "Unemp_adj.csv", row.names = FALSE)

adjustment_factors <- as.data.frame(Factors) %>%
  mutate(Date = index(Factors))

write.csv(adjustment_factors, "Adjustment_factors_flows_in.csv", row.names = FALSE)






