# This file is used for the purposes of testing how the index responds to the removing one series at the time

library(tidyverse)
library(ggplot2)
library(lubridate)
library(zoo)
library(dplyr)
library(ISOweek)
library(purrr)

# Main function
em_pca <- function(X, n_factors = 1, max_iter = 1000, tol = 1e-6) {
  
  T <- nrow(X)
  N <- ncol(X)
  
  # Track which entries were originally missing
  missing_mask <- is.na(X)
  
  # --- INITIALISATION ---
  # (1) Replace missing with column (unconditional) means
  col_means <- colMeans(X, na.rm = TRUE)
  col_sds   <- apply(X, 2, sd, na.rm = TRUE)
  
  X0 <- X
  for (j in 1:N) {
    X0[missing_mask[, j], j] <- col_means[j]
  }
  
  # (2) Demean and standardise
  demean_std <- function(mat, means, sds) {
    sweep(sweep(mat, 2, means, "-"), 2, sds, "/")
  }
  
  X_std <- demean_std(X0, col_means, col_sds)
  
  # (3) PCA on initial filled matrix
  pca_fit  <- prcomp(X_std, center = FALSE, scale. = FALSE)
  F_hat    <- pca_fit$x[, 1:n_factors, drop = FALSE]   # T x n_factors
  Lambda   <- pca_fit$rotation[, 1:n_factors, drop = FALSE]  # N x n_factors
  
  # --- ITERATIONS ---
  for (iter in 1:max_iter) {
    F_old <- F_hat
    
    # (4) Update ONLY originally missing values
    X_filled <- X0  # start from last filled version (on original scale)
    fitted_original_scale <- F_hat %*% t(Lambda)  # T x N, standardised scale
    
    # Rescale fitted values back to original scale
    fitted_rescaled <- sweep(sweep(fitted_original_scale, 2, col_sds, "*"), 2, col_means, "+")
    
    for (j in 1:N) {
      X_filled[missing_mask[, j], j] <- fitted_rescaled[missing_mask[, j], j]
    }
    X0 <- X_filled
    
    # Recompute means/sds from the filled matrix and standardise
    col_means_new <- colMeans(X0)
    col_sds_new   <- apply(X0, 2, sd)
    X_std <- demean_std(X0, col_means_new, col_sds_new)
    
    # (6) PCA on updated matrix
    pca_fit <- prcomp(X_std, center = FALSE, scale. = FALSE)
    F_hat   <- pca_fit$x[, 1:n_factors, drop = FALSE]
    Lambda  <- pca_fit$rotation[, 1:n_factors, drop = FALSE]
    
    # (7) Check convergence
    delta <- max(abs(F_hat - F_old))
    if (delta < tol) {
      message("Converged after ", iter, " iterations.")
      break
    }
  }
  
  list(
    factor   = F_hat,       # WAI (weekly activity index)
    loadings = Lambda,
    X_filled = X0           # Estimated values for IP and Retail index at weekly freq
  )
}

# Loading the data set
setwd("C:/Users/Lenovo/Desktop/uni/Bakalauras/MAIN/_DATA/_Estimation/_combined data")
df <- read.csv("data.csv")
gdp_data <- read.csv("data_2000.csv")

df_no_dates <- df %>%
  select(-c("iso_year", "iso_week", "Pct_change_GDP"))

# Initializing data list
variable_names <- c("growth_q_elec", "growth_q_flights", "growth_q_nedar", "growth_q_ind", "growth_q_no2",
                    "growth_q_omx", "growth_q_ret", "growth_q_ships", "growth_q_unemp_in", "growth_q_unemp_out" , "none")
iterations <- list()

for (i in variable_names) {
  if (i == "none") {
    df_iteration <- df_no_dates
  } else {
    df_iteration <- df_no_dates %>%
      select(-all_of(i))
  }
  
  X <- as.matrix(df_iteration)
  
  result <- em_pca(X, n_factors = 1)
  
  Index <- result$factor
  
  # Variance explained by the first factor
  pca_var <- prcomp(result$X_filled, center = FALSE, scale. = FALSE)
  var_explained <- pca_var$sdev^2 / sum(pca_var$sdev^2)
  
  # Adding the result to the copy of the main data frame
  df_copy <- df %>%
    mutate(WAI = Index) %>%
    mutate(WAI = -WAI)
  
  df_copy <- df_copy %>%
    mutate(quarter = case_when(
      iso_week <= 13 ~ 1,
      iso_week <= 26 ~ 2,
      iso_week <= 39 ~ 3,
      TRUE           ~ 4  # captures week 52 and 53
    )) %>%
    mutate(date = ISOweek2date(paste0(iso_year, "-W", sprintf("%02d", iso_week), "-1"))) #gives the date of the monday of the week
  
  # Get the last week of each quarter for each year
  quarter_ends <- df_copy %>%
    group_by(iso_year, quarter) %>%
    slice_max(iso_week, n = 1) %>%
    ungroup()
  
  # Computing the scaling parameters
  wai_mean <- mean(quarter_ends$WAI,            na.rm = TRUE)
  wai_sd   <- sd(quarter_ends$WAI,              na.rm = TRUE)
  
  gdp_mean <- mean(gdp_data$Pct_change_GDP, na.rm = TRUE)
  gdp_sd   <- sd(gdp_data$Pct_change_GDP,   na.rm = TRUE)
  
  # Scaling the data
  df_copy <- df_copy %>%
    mutate(LTWAI = (WAI - wai_mean) / wai_sd * gdp_sd + gdp_mean)
  
  # Again getting the quarter ends
  quarter_ends <- df_copy %>%
    group_by(iso_year, quarter) %>%
    slice_max(iso_week, n = 1) %>%
    ungroup()
  
  # Calculating correlation
  correlation <- cor(
    quarter_ends$LTWAI,
    quarter_ends$Pct_change_GDP,
    use = "complete.obs"
  )
  
  # Writing the results into a list
  iterations[[i]] <- list(
    Correlation = correlation,
    name = variable_names[i],
    Index = df_copy$LTWAI,
    Loadings = result$loadings,
    Var_exp = var_explained[1],
    gdp = df_copy$Pct_change_GDP,
    time = df_copy$date # last two are for the purpose of creating graphs
  )
}

# Creating data frame to use for graphic
df_graph <- data.frame(
  time  = iterations$growth_q_flights$time,
  Index_noflight = iterations$growth_q_flights$Index[, 1],
  Index_noelec = iterations$growth_q_elec$Index[, 1],
  Index_nedar = iterations$growth_q_nedar$Index[, 1],
  Index_noind = iterations$growth_q_ind$Index[, 1],
  Index_nono2 = iterations$growth_q_no2$Index[, 1],
  Index_noomx = iterations$growth_q_omx$Index[, 1],
  Index_noret = iterations$growth_q_ret$Index[, 1],
  Index_noships = iterations$growth_q_ships$Index[, 1],
  Index_nounempin = iterations$growth_q_unemp_in$Index[, 1],
  Index_nounempout = iterations$growth_q_unemp_out$Index[, 1],
  gdp   = iterations$growth_q_flights$gdp,
  LTWAI = iterations$none$Index[,1]
)

# Comparing the results of the index and actual GDP changes
ggplot(df_graph, aes(x = time)) +
  geom_segment(data = df_graph %>% filter(!is.na(gdp)),
               aes(x = time, xend = time, y = 0, yend = gdp, color = "Actual GDP (Quarterly)"),
               linewidth = 2.5) +
  geom_line(aes(y = Index_noflight, color = "LTWAI without flight data"), linewidth = 0.8) +
  geom_line(aes(y = Index_noelec, color = "LTWAI without electricity data"), linewidth = 0.8) +
  geom_line(aes(y = Index_nedar, color = "LTWAI without Google Trends data"), linewidth = 0.8) +
  geom_line(aes(y = Index_noind, color = "LTWAI without industrial index data"), linewidth = 0.8) +
  geom_line(aes(y = Index_nono2, color = "LTWAI without NO2 data"), linewidth = 0.8) +
  geom_line(aes(y = Index_noomx, color = "LTWAI without OMX Vilnius GI data"), linewidth = 0.8) +
  geom_line(aes(y = Index_noret, color = "LTWAI without retail index data"), linewidth = 0.8) +
  geom_line(aes(y = Index_noships, color = "LTWAI without shipping data"), linewidth = 0.8) +
  geom_line(aes(y = Index_nounempin, color = "LTWAI without unemployment flows in"), linewidth = 0.8) +
  geom_line(aes(y = Index_nounempout, color = "LTWAI without unemployment flows out"), linewidth = 0.8) +
  geom_line(aes(y = Index_noflight, color = "LTWAI without flight data"), linewidth = 0.8) +
  geom_line(aes(y = LTWAI, color = "LTWAI"), linewidth = 0.8) +
  scale_color_manual(name = NULL, values = c("LTWAI without flight data" = "blue", "Actual GDP (Quarterly)" = "black",
                                             "LTWAI" = "red",
                                             "LTWAI without electricity data" = "darkgoldenrod1",
                                             "LTWAI without Google Trends data" = "darkolivegreen3",
                                             "LTWAI without industrial index data" = "darkorange",
                                             "LTWAI without NO2 data" = "deepskyblue",
                                             "LTWAI without OMX Vilnius GI data" = "deeppink",
                                             "LTWAI without retail index data" = "darkorchid2",
                                             "LTWAI without shipping data" = "aquamarine3",
                                             "LTWAI without unemployment flows in" = "green",
                                             "LTWAI without unemployment flows out" = "brown1"
                                             )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Comparing alternative specifications of the LTWAI",
    x = "Quarter", y = "Growth Rate (%)",
    caption = "Note: Quarterly GDP data from Eurostat via FRED, work my own. Last LTWAI is the 9th iso week of 2026."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.4, "cm"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(df_graph, aes(x = time)) +
  geom_segment(data = df_graph %>% filter(!is.na(gdp)),
               aes(x = time, xend = time, y = 0, yend = gdp, color = "Actual GDP (Quarterly)"),
               linewidth = 2.5) +
  geom_line(aes(y = Index_noflight, color = "LTWAI without flight data"), linewidth = 0.8) +
  geom_line(aes(y = LTWAI, color = "LTWAI"), linewidth = 0.8) +
  scale_color_manual(name = NULL, values = c("LTWAI without flight data" = "blue", "Actual GDP (Quarterly)" = "black",
                                             "LTWAI" = "red"
  )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Comparing alternative specifications of the LTWAI",
    x = "Quarter", y = "Growth Rate (%)",
    caption = "Note: Quarterly GDP data from Eurostat via FRED, work my own. Last LTWAI is the 9th iso week of 2026."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.4, "cm"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


