# Day 2 

install.packages("geodata")
library(geodata)
install.packages('SSDM')
library(SSDM)
library()
bioclim_data <- worldclim_global(var = "bio",
                                 res = 2.5,
                                 path = "day2_data")
data <- read_rds(file = "mash.RDS")

obs_data <- data %>% 
  dplyr::select(c(latitude, longitude, Espèce)) %>% 
  relocate(where(is.numeric), .after = where(is.character))

species <- obs_data %>% 
  dplyr::filter(grepl("^RUSSULA", Espèce))

Env <- load_var(path = "day2_data/", format =".tif")

# SSP245 is one of the SSP scenarios used in climate modeling, 
# representing a medium-low greenhouse gas emissions pathway.
# "ACCESS.CM2_ssp245_2021.2040" refers to climate model simulations 
# or projections based on the ACCESS.CM2 model using the 
# SSP245 scenario for the time period from 2021 to 2040.

SDM <- modelling('GLM', species, 
                 Env, Xcol = 'longitude', Ycol = 'latitude', verbose = FALSE)

plot(SDM@projection, main = 'SDM\nfor Russula\nwith GLM algorithm')

ESDM <- ensemble_modelling(c('CTA', 'MARS'),  species, 
                           Env, rep = 1, Xcol = 'longitude', Ycol = 'latitude',
                           ensemble.thresh = 0, verbose = FALSE)
plot(ESDM@projection, main = 'ESDM\nfor Russela\nwith CTA and MARS algorithms')

SSDM <- stack_modelling(c('CTA', 'SVM'), species, Env, rep = 1, ensemble.thresh = 0,
                        Xcol = 'longitude', Ycol = 'latitude',
                        Spcol = 'Espèce', method = "pSSDM", verbose = FALSE)
plot(SSDM@diversity.map, main = 'SSDM\nfor Russela genus\nwith CTA and SVM algorithms')


ESDM@evaluation
SSDM@evaluation
SSDM@variable.importance

# Download predicted climate data
forecast_data <- cmip6_world(model = "MPI-ESM1-2-HR",
                             ssp = "245",
                             time = "2061-2080",
                             var = "bioc",
                             res = 2.5,
                             path = "data")

