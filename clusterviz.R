library(ggplot2)
library(sf)
library(dplyr)
library(readxl)
library(RColorBrewer)
library(viridis)

# Read the shapefile
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
