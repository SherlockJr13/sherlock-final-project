library(readxl)
library(forecast)
file_path <- "E:/AKTUARIA FINAL SEASON/DATA/fpdata.xlsx"
sheet_name <- "x3raw"
data <- read_excel(file_path, sheet = sheet_name)
# Define a function to forecast the 18th value for a time series
forecast_18th_value <- function(ts_data) {
  ts_data <- ts(ts_data, frequency = 2)  # Semi-annual frequency
  fit <- auto.arima(ts_data)
  forecasted_values <- forecast(fit, h = 1)
  return(forecasted_values$mean)
}

# Extract regions and their respective data
regions <- data[[1]]  # Assuming first column is region names
results <- data.frame(Region = regions, Forecast_18th = NA)

for (i in 1:nrow(data)) {
  ts_data <- as.numeric(data[i, -1])  # Exclude the first column (region names)
  results$Forecast_18th[i] <- forecast_18th_value(ts_data)
}

# Print results
print(results)

library(writexl)
# Specify the output file path
output_file_path <- "E:/AKTUARIA FINAL SEASON/DATA/forecasted_results.xlsx"

# Write the results to an Excel file
write_xlsx(results, output_file_path)

