###Koissi Savi
###VISUALIZATION
###### This code is written by Koissi Savi
##### SDMs' Training at Parakou - Benin 
###### April 2024


# Load necessary libraries 
library(readxl)
library(sp)
library(sf)
library(rgdal)
library(raster)
library(caret)
#install.packages("adehabitatHS")
library(adehabitatHS) # ENFA-SDM
library(tidyverse)
library(dismo)


# EXPLORATORY DATA ANALYSIS
data <- read_xlsx("Data_base.xlsx") # Read dataset

dataClean <- data %>% 
  dplyr::select(-c("Type", "Voucher", "Référence photo", "Time"))

dataUsable <- dataClean %>% 
  dplyr::select(-c("Identifiant (Nom de l'espèce)", "Formation végétale",
                   "Statut de comestibilité", "Substrat")) %>% 
  drop_na()

# Identify some duplicate 
sum(duplicated(dataClean[c("Identifiant (Nom de l'espèce)",
                        "latitude", "longitude")]))


# Visualize on a map 
# Define the coordinate reference system (CRS) for latitude and longitude data
latlong_crs <- CRS("+proj=longlat +datum=WGS84")

# Check for missing values in longitude and latitude columns
missing_values <- is.na(dataUsable$longitude) | is.na(dataUsable$latitude)

# Print the number of missing values
print(sum(missing_values))

# Remove rows with missing values
dataUsable <- dataUsable[!missing_values, ]

# Convert to spatial data
spatial_points <- dataUsable %>%
  dplyr::select(longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = latlong_crs)

# Define the desired projection for your map
map_projection <- "+proj=merc +ellps=WGS84"

# Convert sf object to sp object
spatial_points_sp <- as(spatial_points, "Spatial")

# Transform spatial points using map projection
projected_points <- st_transform(spatial_points, map_projection)

# Load your shapefile
background_shapefile <- st_read("Ben_shapefile/BEN_adm2.shp")
background_shapefile_F <- background_shapefile %>% 
  filter(NAME_1 == "Collines")

# Plot the background shapefile with projected points
# Calculate species counts

ggplot() +
  geom_sf(data = background_shapefile_F) +
  geom_sf(data = projected_points, color = "red", size = 2) +
  labs(title = "Distribution of Mushroom") +
  theme_minimal()

ggplot() +
  geom_sf(data = background_shapefile) +
  geom_sf(data = projected_points, color = "red", size = 2) +
  labs(title = "Distribution of Mushroom") +
  theme_minimal()

presence_species <- unique(data$Espèce)

directories <-c("data_Connexe/", "data_Connexe/wc2.1_10m_tmax/",  "data_Connexe/wc2.1_10m_tavg/") 

# List files from each directory and combine them into a single vector
predictor_files <- unlist(lapply(directories, function(dir) {
  list.files(dir, pattern = '.tif$', full.names = TRUE)
}))

# Stack all predictor files
predictors <- stack(predictor_files)

# Extract presence and background points
presence_points <- data %>% 
  dplyr::filter(Espèce %in% presence_species) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = latlong_crs)

set.seed(123)
extEnd <- terra::rast(xmin=0.774574, xmax = 3.851701, ymin = 6.23514, ymax = 12.41835)

background_points <- randomPoints(predictors, 500, extEnd)

# Extract environmental values at presence and background points
presvals <- raster::extract(predictors, presence_points)
absvals <- raster::extract(predictors, background_points)

# Create a binary response variable indicating presence (1) or absence (0)
presence_data <- cbind(presence = rep(1, nrow(presvals)), presvals)
absence_data <- cbind(presence = rep(0, nrow(absvals)), absvals)

# Combine presence and absence data
sdmdata <- rbind(presence_data, absence_data)

# Rename the columns
names(sdmdata) <- c("presence", names(sdmdata)[-1])

# View the resulting data frame
print(sdmdata %>% head())

# Extract coordinates

existing_coords <- cbind(data$longitude, data$latitude)
colnames(existing_coords) <- c("longitude", "latitude")

# Extract coordinates from 'background_points'
background_coords <- coordinates(background_points)

# Rename the columns of 'background_coords' to match 'existing_coords'
colnames(background_coords) <- c("longitude", "latitude")

# Combine existing coordinates with random background points
all_coords <- rbind(existing_coords, background_coords)

# Create 'loc' object
loc <-data.frame(longitude = all_coords[, "longitude"], latitude = all_coords[, "latitude"])















