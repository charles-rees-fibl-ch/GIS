### Setup

library(tidyverse)
library(MODISTools)
library(raster)

### Extracting Products from MODIS and setting mid point

latit <- 47.176508
longi <- 8.354773

products <- mt_products() %>% as_tibble()
bands <- mt_bands(product = "MOD13Q1") %>% as_tibble() # NDVI at 250 m resolution
dates <- mt_dates(product = "MOD13Q1", lat = latit, lon = longi) %>% as_tibble() # Setting center to home 

### Pulling NDVI

df_ndvi <- mt_subset(product = "MOD13Q1",               # the chosen product
                     lat = latit,                       # desired lat/lon
                     lon = longi,
                     band = "250m_16_days_NDVI",        # chosen band defining spatial and temporal scale
                     start = "2022-06-10",              # start date: 10th June 2022
                     end = "2023-06-10",                # end date: 10th June 2022
                     km_lr = 10,                        # kilometers left & right of the chosen location (lat/lon above)
                     km_ab = 10,                        # kilometers above and below the location
                     site_name = "Abtwil (AG)",         # the site name we want to give the data
                     internal = TRUE,
                     progress = FALSE
) %>% 
  as_tibble()

head(df_ndvi)

raster_ndvi <- mt_to_raster(df = df_ndvi) # converting subset into raster

### Plotting Data
plot(raster_ndvi[[1:4]], zlim=c(-0.1, 0.95))