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


# EXPLORATORY DATA ANALYSIS
data <- read_xlsx("Data_base.xlsx") # Read dataset

dataClean <- data %>% 
  # Remove some variables of no use
  dplyr::select(-c("Type", "Voucher", "Référence photo", "Time"))# %>% 
  # Replace in the column species N/A with "Unknown"
  #mutate(Espèce = if_else(is.na(Espèce), "Unknown", Espèce))

# Make an important decision about species' distribution to model
species_summary <- dataClean %>%
  count(Espèce, name = "Species_Count") %>%
  arrange(desc(Species_Count))  # Arrange by descending order of species count

species_summary %>% View()

dataUsable <- dataClean
  #filter(Espèce == 'RUSSULA OLEIFERA')

# Identify some duplicate 
duplicated(dataUsable[c("Identifiant (Nom de l'espèce)",
                                      "latitude", "longitude")])

# Remove variable of no interest
dataUsable <- dataUsable %>%
  dplyr::select(-c("Identifiant (Nom de l'espèce)"))%>% 
  drop_na()


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



# Set the directory containing your TIFF files
elv <- raster("data_Connexe/wc2.1_10m_elev 2.tif")

proj4string(elv)  # Check CRS of elevation raster

# Crop the elevation raster to the extent of your background shapefile
elv_cropped <- crop(elv, extent(background_shapefile_F))

# Convert cropped elevation raster to dataframe
elv_df <- rasterToPoints(elv_cropped)

# Convert to dataframe
elv_df <- as.data.frame(elv_df)

# Rename columns
colnames(elv_df) <- c("x", "y", "elevation")

# Extract elevation data for shapefile
elev_shapefile <- raster::extract(elv_cropped, background_shapefile_F)

# Extract elevation data for points
elev_points <- raster::extract(elv_cropped, projected_points)

# Convert elevation points to dataframe
elev_points_df <- data.frame(elev = unlist(elev_points))

# Merge elevation data with dataUsable
dataUsable_with_elev <- cbind(dataUsable, elev_points_df)

# Visualize elevation data with each point within the selected shapefile
ggplot() +
  geom_raster(data = elv_df, aes(x = x, y = y, fill = elevation)) +
  geom_sf(data = background_shapefile_F, fill = NA, color = "black") +
  geom_sf(data = projected_points, color = "red", size = 2) +
  labs(title = "Elevation Data within Selected Shapefile") +
  scale_fill_gradientn(colours = terrain.colors(10)) +  # Adjust color gradient if needed
  theme_minimal()

### Load temp_min files 

# Set the directory containing your temperature TIFF files
tempmax_files <- list.files("data_Connexe/wc2.1_10m_tmax/", pattern = ".tif", full.names = TRUE)

# Load all temperature rasters
tempmax_rasters <- lapply(tempmax_files, raster)

# Crop each temperature raster to the extent of your background shapefile
cropped_tempmax_rasters <- lapply(tempmax_rasters, function(r) crop(r, extent(background_shapefile_F)))

# Convert cropped temperature rasters to dataframes
tempmax_dfs <- lapply(cropped_tempmax_rasters, function(r) {
  df <- rasterToPoints(r)
  df <- as.data.frame(df)
  colnames(df) <- c("x", "y", "tempmax")  # Rename columns if necessary
  return(df)
})

# Extract temperature data for points
points_temp_data <- lapply(cropped_tempmax_rasters, function(r) {
  raster::extract(r, projected_points)
})


# Combine temperature data with dataUsable_with_elv dataframe
dataUsable_with_temp <- dataUsable_with_elev  # Create a copy of the original dataframe
for (i in 1:length(cropped_tempmax_rasters)) {
  temp_col_name <- paste0("tempmax_", i)  # Create a unique column name for temperature
  dataUsable_with_temp[[temp_col_name]] <- points_temp_data[[i]]
}


# Create a function to plot temperature raster along with elevation data and shapefile
plot_temp_raster <- function(temp_df, shapefile, points) {
  ggplot() +
    geom_raster(data = temp_df, aes(x = x, y = y, fill = tempmax)) +
    geom_sf(data = shapefile, fill = NA, color = "black") +
    geom_sf(data = points, color = "red", size = 2) +
    labs(title = "Temperature Data within Selected Shapefile") +
    scale_fill_gradientn(colours = terrain.colors(10)) +  # Adjust color gradient if needed
    theme_minimal()
}

# Create a list to store plots for each tempmax raster
temp_plots <- list()

# Loop through each cropped temperature raster dataframe
for (i in 1:length(tempmax_dfs)) {
  temp_plot <- plot_temp_raster(tempmax_dfs[[i]], background_shapefile_F, projected_points)
  temp_plots[[i]] <- temp_plot
}

# Display plots (you can modify the layout if needed)
multiplot <- do.call(gridExtra::grid.arrange, temp_plots)
print(multiplot)

# Execute the same process for the remaining dataset 

### Load tavg files 

# Set the directory containing your temperature TIFF files
tavg_files <- list.files("data_Connexe/wc2.1_10m_tavg/", pattern = ".tif", full.names = TRUE)

# Load all temperature rasters
tavg_rasters <- lapply(tavg_files, raster)

# Crop each temperature raster to the extent of your background shapefile
cropped_tavg_rasters <- lapply(tavg_rasters, function(r) crop(r, extent(background_shapefile_F)))

# Convert cropped temperature rasters to dataframes
tavg_dfs <- lapply(cropped_tavg_rasters, function(r) {
  df <- rasterToPoints(r)
  df <- as.data.frame(df)
  colnames(df) <- c("x", "y", "tavg")  # Rename columns if necessary
  return(df)
})

# Extract temperature data for the shapefile
shapefile_tavg_data <- lapply(cropped_tavg_rasters, function(r) {
  raster::extract(r, background_shapefile_F)
})

# Extract average temperature data for points
points_tavg_data <- lapply(cropped_tavg_rasters, function(r) {
  raster::extract(r, projected_points)
})


# Combine temperature data with dataUsable_with_elv dataframe
dataUsable_with_tavg <- dataUsable_with_temp  # Create a copy of the original dataframe
for (i in 1:length(cropped_tavg_rasters)) {
  tavg_col_name <- paste0("tavg_", i)  # Create a unique column name for temperature
  dataUsable_with_tavg[[tavg_col_name]] <- points_tavg_data[[i]]
}


# Create a function to plot temperature raster along with elevation data and shapefile
plot_tavg_raster <- function(tavg_df, shapefile, points) {
  ggplot() +
    geom_raster(data = tavg_df, aes(x = x, y = y, fill = tavg)) +
    geom_sf(data = shapefile, fill = NA, color = "black") +
    geom_sf(data = points, color = "red", size = 2) +
    labs(title = "Temperature Data within Selected Shapefile") +
    scale_fill_gradientn(colours = terrain.colors(10)) +  # Adjust color gradient if needed
    theme_minimal()
}

# Create a list to store plots for each tempmax raster
tavg_plots <- list()

# Loop through each cropped temperature raster dataframe
for (i in 1:length(tavg_dfs)) {
  tavg_plot <- plot_tavg_raster(tavg_dfs[[i]], background_shapefile_F, projected_points)
  tavg_plots[[i]] <- tavg_plot
}

# Display plots (you can modify the layout if needed)
multiplot2 <- do.call(gridExtra::grid.arrange, tavg_plots)
print(multiplot2)

#Exercise: Repeat the same thing on all the remaining dataset and share the combined dataset

dataUsable_with_tavg %>% glimpse()

#Variables selection
# These methods are not applicable so based on the existing literature we can select some variables 

# Remove non-predictor columns
predictors <- dplyr::select(dataUsable_with_tavg, 
                            -c( "latitude", "longitude", "x_proj", "y_proj", "Espèce"))

# 1. Correlation analysis
cor_matrix <- cor(predictors)
highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.7)  # Example cutoff
highly_correlated_vars <- colnames(predictors)[highly_correlated]

# Remove highly correlated variables
predictors_no_cor <- predictors[, -highly_correlated]

# 2. VIF calculation
# Calculate VIF values for each predictor variable
vif_values <- apply(predictors_no_cor, 2, function(x) car::vif(lm(x ~ ., data = predictors_no_cor)))

# 3. Variable Importance (using Random Forest as an example) 
set.seed(123)
rf_model <- train(as.matrix(predictors), as.factor(Espèce) ~ ., data = dataUsable_with_tavg, method = "rf")
var_importance <- varImp(rf_model) 

# 4. Biological Relevance: Expert knowledge or literature review

# 5. Model Performance: Cross-validation or other techniques

# Based on the above analysis, select a subset of predictor variables for modeling
selected_predictors <- select(predictors, -highly_correlated_vars)  # Remove highly correlated variables

# data_complete <- dataUsable_with_tavg %>% 
#   dplyr::select(c(latitude, longitude, y_proj, x_proj, Espèce, 
#                   elev, tempmax_4, tavg_2 )) 

saveRDS(dataUsable_with_tavg, "mash.RDS")
# ###PROFIL TECHNIQUE FOR SDMs
# 
# #Ecological Niche Factor Analysis (ENFA)
# 
# # Convert latitude and longitude to numeric
# 
# # Convert latitude and longitude to numeric
# dataUsable_with_tavg$latitude <- as.numeric(dataUsable_with_tavg$latitude)
# dataUsable_with_tavg$longitude <- as.numeric(dataUsable_with_tavg$longitude)
# 
# # Create a data frame containing only the environmental variables
# env_data <- dataUsable_with_tavg[, c("elev", paste0("tempmax_", 1:12), paste0("tavg_", 1:12))]
# 
# # Create a dudi object for the environmental data
# env_data_dudi <- dudi.pca(env_data, scannf = FALSE, nf = 3)
# 
# # Execute ENFA
# enfa_result <- enfa(env_data_dudi, pr = as.factor(dataUsable_with_tavg$Espèce))
# enfa_result <- enfa(env_data_dudi, pr = as.factor(dataUsable_with_tavg$Espèce_ascii))
# 
# 
# 
# 
# # Print the results
# print(enfa_result)
# 















