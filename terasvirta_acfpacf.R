# Load the necessary packages
library(readxl)
library(tseries)
library(vars)

# Define the path to your Excel file
file_path <- "E:/GitHub/sherlock-final-project/pooled_data.xlsx"

# Read the data from the Excel file
data <- read_excel(file_path, sheet = 1)

# View the first few rows of the data to understand its structure
head(data)

# Perform the Terasvirta test
terasvirta.test(data$x1, data$y, type=c("F"), scale=TRUE)
terasvirta.test(data$x2, data$y, type=c("F"), scale=TRUE)
terasvirta.test(data$x3, data$y, type=c("F"), scale=TRUE)

# ACF-PACF Test

# Plot ACF and PACF for the aggregated data
plot_acf_pacf <- function(series, title) {
  par(mfrow=c(1,2))
  acf(series, main=paste(title, "- ACF"), lag.max=136)
  pacf(series, main=paste(title, "- PACF"), lag.max=136)
  par(mfrow=c(1,1))
}

# Function to determine significant lags based on PACF
significant_pacf_lags <- function(series, threshold=0.2) {
  pacf_values <- pacf(series, plot=FALSE)$acf
  significant_lags <- which(abs(pacf_values) > threshold)
  significant_lags <- significant_lags[significant_lags != 1] - 1
  return(significant_lags)
}

# Function to check for stationarity
check_stationarity <- function(series, alpha=0.05) {
  result <- adf.test(series)
  p_value <- result$p.value
  return(p_value < alpha)  # If TRUE, the series is stationary
}

# Function to make the series stationary
make_stationary <- function(series) {
  differenced_series <- diff(series)
  return(na.omit(differenced_series))
}

# Function to determine the best N_PAST
determine_best_n_past <- function(aggregated_series, seasonal_period=34) {
  # Check if the series is stationary
  if (!check_stationarity(aggregated_series)) {
    cat("Series is not stationary. Applying differencing...\n")
    aggregated_series <- make_stationary(aggregated_series)
  }
  
  plot_acf_pacf(aggregated_series, "ACF and PACF for Aggregated Data")
  
  # Calculate significant lags for the aggregated data
  significant_lags <- significant_pacf_lags(aggregated_series)
  # Determine a common N_PAST
  max_significant_lag <- ifelse(length(significant_lags) > 0, max(significant_lags), 1)
  # Adjust N_PAST to be a multiple of the seasonal period
  common_n_past <- ceiling(max_significant_lag / seasonal_period) * seasonal_period
  
  cat("Suggested common N_PAST (iterations):", common_n_past / seasonal_period, "\n")
  cat("Actual N_PAST (data points):", common_n_past, "\n")
}

# Assuming `data` is a data frame with columns 'x1', 'x2', 'x3'
determine_best_n_past(data$x1)
determine_best_n_past(data$x2)
determine_best_n_past(data$x3)