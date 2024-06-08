library(ggplot2)
library(sf)
library(dplyr)
library(readxl)
library(RColorBrewer)

# Read the shapefile
indo_sf <- st_read("E:/GitHub/sherlock-final-project/map_cluster/indo_province_map.shp")
print(head(indo_sf))
print(indo_sf)

# Read the cluster data from the Excel file
cluster_data <- read_excel("E:/GitHub/sherlock-final-project/best_kmedoids_cluster.xlsx", sheet='Sheet1')

# Merge the shapefile data with the cluster data
merged_data <- indo_sf %>%
  left_join(cluster_data, by = c("PROVINSI" = "province"))

library(viridis)
# Visualize with ggplot2 using viridis color palette
ggplot(data = merged_data) +
  geom_sf(aes(fill = factor(cluster_index))) +
  scale_fill_viridis_d(option = "viridis", name = "Cluster") +
  theme_minimal() +
  labs(title = "Cluster Visualization of 34 Provinces in Indonesia")
