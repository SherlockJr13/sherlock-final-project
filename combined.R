# Load the necessary packages
lapply(c("readxl", "tseries", "vars", "car", "MVN", "ggplot2", "GGally", "dplyr", "biotools", "stats",
         "MASS", "sf", "RColorBrewer", "viridis"), library, character.only = TRUE)

file_path <- "E:/GitHub/sherlock-final-project/pooled_data.xlsx"
data <- read_excel(file_path, sheet = 1)
y_lag <- data$y[1:578]
data <- data[35:612,]
data$x4 <- y_lag

# Perform the Terasvirta test
terasvirta.test(data$x1, data$y, type=c("F"), scale=TRUE)
terasvirta.test(data$x2, data$y, type=c("F"), scale=TRUE)
terasvirta.test(data$x3, data$y, type=c("F"), scale=TRUE)
terasvirta.test(data$x4, data$y, type=c("F"), scale=TRUE)

# ACF-PACF Test
library(forecast)
auto.arima(data$x1)
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

significant_pacf_lags <- function(series, alpha=0.05) {
  pacf_result <- pacf(series, plot=FALSE)
  pacf_values <- pacf_result$acf
  n <- length(series)
  critical_value <- qnorm(1 - alpha / 2) / sqrt(n)
  
  significant_lags <- which(abs(pacf_values) > critical_value)
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
    cat("Tidak stasioner, differencing\n")
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


#Clustering Visualization
indo_sf <- st_read("E:/GitHub/sherlock-final-project/map_cluster/indo_province_map.shp")
print(head(indo_sf))
print(indo_sf)

# Read the cluster data from the Excel file
cluster_data <- read_excel("E:/GitHub/sherlock-final-project/cluster_summary.xlsx", sheet='Sheet1')
cluster_data$kmedoids <- cluster_data$kmedoids +1

# Merge the shapefile data with the cluster data
merged_data <- indo_sf %>%
  left_join(cluster_data, by = c("PROVINSI" = "province"))

# Visualize with ggplot2 using viridis color palette
ggplot(data = merged_data) +
  geom_sf(aes(fill = factor(kmeans))) +
  scale_fill_viridis_d(option = "viridis", name = "kmeans") +
  theme_minimal() +
  labs(title = "Klaster Tingkat Kemiskinan Indonesia - K-means")

# Visualize with ggplot2 using viridis color palette
ggplot(data = merged_data) +
  geom_sf(aes(fill = factor(kmedoids))) +
  scale_fill_viridis_d(option = "viridis", name = "kmedoids") +
  theme_minimal() +
  labs(title = "Klaster Tingkat Kemiskinan Indonesia - K-medoids")


#MANOVA
data_check <- read_excel("E:/GitHub/sherlock-final-project/cluster_summary.xlsx", sheet='Sheet1')

# Mardia Test
data_multivariate <- data_check[, c('x1', 'x2', 'x3')]
result_mvn <- mvn(data = data_multivariate, mvnTest = "mardia", multivariatePlot = "qq")
print(result_mvn)

# Using biotools package for Box's M test
box_test <- boxM(data_check[, c('x1', 'x2', 'x3')], data_check$kmedoids)
print(box_test)

# Multicolinearity Check
cor_matrix <- cor(data_multivariate)
print(cor_matrix)

# MANOVA using stats package
manova_res <- manova(cbind(x1, x2, x3) ~ factor(kmedoids), data = data_check)
# Summary using Wilks' lambda
summary(manova_res, test = "Wilks")