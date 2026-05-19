library(tidyverse)
library(ggplot2)
library(lubridate)
library(zoo)
library(dplyr)
library(ISOweek)

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
df <- read.csv("data_2019.csv")
gdp_data <- read.csv("data_2000.csv")

df_no_dates <- df %>%
  select(-c("iso_year", "iso_week", "Pct_change_GDP"))

X <- as.matrix(df_no_dates)

# Running the EM-PCA
result <- em_pca(X, n_factors = 1)

Index <- result$factor

# Variance explained by the first factor
pca_var <- prcomp(result$X_filled, center = FALSE, scale. = FALSE)
var_explained <- pca_var$sdev^2 / sum(pca_var$sdev^2)

# Adding the result to the main data frame
df <- df %>%
  mutate(WAI = Index) %>%
  mutate(WAI = -WAI)

# Creating Quarter end variables
df <- df %>%
  mutate(quarter = case_when(
    iso_week <= 13 ~ 1,
    iso_week <= 26 ~ 2,
    iso_week <= 39 ~ 3,
    TRUE           ~ 4  # captures week 52 and 53
  )) %>%
  mutate(date = ISOweek2date(paste0(iso_year, "-W", sprintf("%02d", iso_week), "-1"))) #gives the date of the monday of the week

# Get the last week of each quarter for each year
quarter_ends <- df %>%
  group_by(iso_year, quarter) %>%
  slice_max(iso_week, n = 1) %>%
  ungroup()

# Computing the scaling parameters
wai_mean <- mean(quarter_ends$WAI,            na.rm = TRUE)
wai_sd   <- sd(quarter_ends$WAI,              na.rm = TRUE)
gdp_mean <- mean(quarter_ends$Pct_change_GDP, na.rm = TRUE)
gdp_sd   <- sd(quarter_ends$Pct_change_GDP,   na.rm = TRUE)

gdp_mean <- mean(gdp_data$Pct_change_GDP, na.rm = TRUE)
gdp_sd   <- sd(gdp_data$Pct_change_GDP,   na.rm = TRUE)


# Scaling the data
df <- df %>%
  mutate(LTWAI = (WAI - wai_mean) / wai_sd * gdp_sd + gdp_mean)

# Again getting the quarter ends
quarter_ends <- df %>%
  group_by(iso_year, quarter) %>%
  slice_max(iso_week, n = 1) %>%
  ungroup()

# Calculating correlation
correlation <- cor(
  quarter_ends$LTWAI,
  quarter_ends$Pct_change_GDP,
  use = "complete.obs"
)

# Comparing the results of the index and actual GDP changes
ggplot(df, aes(x = date)) +
  geom_segment(data = df %>% filter(!is.na(Pct_change_GDP)),
               aes(x = date, xend = date, y = 0, yend = Pct_change_GDP, color = "Actual GDP (Quarterly)"),
               linewidth = 2.5) +
  geom_line(aes(y = LTWAI, color = "LTWAI (Weekly)"), linewidth = 0.8) +
  
  scale_color_manual(name = NULL, values = c("LTWAI (Weekly)" = "red", "Actual GDP (Quarterly)" = "black")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "LTWAI vs. Quarterly GDP Growth",
    x = "Quarter", y = "Growth Rate (%)",
    caption = "Note: Quarterly GDP data from Eurostat via FRED, work my own. Last LTWAI is the 9th iso week of 2026."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


