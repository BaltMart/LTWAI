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

# Setting the working directory
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/Number of Flights")

# Loading the data
df <- read.csv("Total_flights_unadjusted.csv")
df$Day <- as.Date(df$Day, format = "%Y-%m-%d")

###################
# Unemployment flow

flights <- df$flights
n_days <- df$Day
flights_xts <- xts(flights, order.by = n_days)

# Interpolation
flights_NA <- xts::xts(as.numeric(ts(flights_xts, frequency=7)), zoo::index(flights_xts))
flights_xts <- xts::xts(as.numeric(forecast::na.interp(ts(flights_xts, frequency=7))), zoo::index(flights_xts))

# Create reference series
reference_series <- flights_xts
restrict <- seq.Date(from = start(reference_series), 
                     to = end(reference_series), by = "days")
restrict_forecast <- seq.Date(from = end(reference_series) + 1,
                              length.out = 365, by = "days")

all_dates <- seq.Date(
  from = min(restrict),
  to = max(restrict_forecast),
  by = "day"
)


# Running the adjustment
flight_sa <- dsa(flights_xts, 
                Log = FALSE, 
                s.window1 = 53, s.window3 = 13,
                robust3 = FALSE, 
                feb29 = "sfac",
                progress_bar = TRUE)

dsa_flight_out <- get_sa(flight_sa)

# Extracting factors to save and use for later adjustments
Factors <- flight_sa$sfac_result

adjustment_factors <- as.data.frame(Factors) %>%
  mutate(Date = index(Factors))

write.csv(adjustment_factors, "Adjustment_factors_flights.csv", row.names = FALSE)

# Plotting the series together to inspect
plot(flight_sa)

# Seasonality tests
all_seas(flights_xts, dsa_flight_out)

# Plots from DSA vignette
# plot 1
g1 <- xtsplot(merge(flights_xts, dsa_flight_out)["2019/"]/1e6, 
              names=c("Original", "DSA"), color = c("darkgrey",  "red"),
              main="Seasonal adjustment result of global flights", 
              submain="From 2019",
              linesize=0.75) + 
  ggplot2::theme(legend.position="None")

g2 <- xtsplot(merge(flights_xts, dsa_flight_out)["2024-01/2024-03"]/1e6, 
              names=c("Original", "DSA"), color = c("darkgrey", "red"),
              main="Seasonal adjustment result", 
              submain="From 2024",
              linesize=0.75) + 
  ggplot2::theme(legend.position="None", plot.title = ggplot2::element_blank())

g3 <- xtsplot(merge(flights_xts, dsa_flight_out)["2020-02-01/2020-05-31"]/1e6, 
              names=c("Original", "DSA"), color = c("darkgrey", "red"),
              main="Seasonal adjustment result", 
              submain="2020-02-01 to 2020-05-31",
              linesize=0.75) + 
  ggplot2::theme(plot.title = ggplot2::element_blank()) +
  ggplot2::labs(caption = "Note: Data form Flightradar24. Calculations my own.")

gridExtra::grid.arrange(g1, g2, g3, layout_matrix=matrix(c(1,1,2,2,3,3,3), ncol=1))


# plot 2
g1 <- xtsplot(flight_sa$output[,c(2,1)]["2019/2026-03"], 
              color=c("#9c9e9f", "darkred"), 
              main="Original and Seasonally adjusted series of global number of flights", 
              names=c("Original", "Adjusted")) + 
  ggplot2::theme(legend.position = c(0.175, 0.775))
g2 <- xtsplot(flight_sa$sfac_result[,1]["2019/2026-03"], 
              color="#0062a1", 
              main="Intra-weekly seasonal component", 
              linesize=0.3) + 
  ggplot2::theme(legend.position = "None")
g5 <- xtsplot(flight_sa$sfac_result[,4]["2019/2026-03"], 
              color="#0062a1", 
              main="Intra-annual seasonal component") + 
  ggplot2::theme(legend.position = "None") +
  ggplot2::labs(caption = "Note: Data form Flightradar24. Calculations my own.")
gridExtra::grid.arrange(g1, g2, g5, nrow=3)

# Writing the adjusted series

# Convert to data frame, then use mutate
adjusted_df <- as.data.frame(dsa_flight_out) %>%
  mutate(Date = index(dsa_flight_out))

flights <- adjusted_df %>%
  mutate(flights_adj = seas_adj) %>%
  select(Date, flights_adj)

write.csv(flights, "Flights_adj.csv", row.names = FALSE)



