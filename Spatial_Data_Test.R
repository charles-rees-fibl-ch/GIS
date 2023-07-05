### Setup
install.packages('spDataLarge',
                 repos='https://nowosad.github.io/drat/', type='source')

library(tidyverse)
library(MODISTools)
library(raster)
library(stringr)
library(lubridate)
library(plyr)
library(sf)
library(tmap)
library(rgdal)
library(spData)
library(spDataLarge)
library(terra)
library(ggplot2)

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
                     start = "2023-05-25",              # start date: 10th June 2022
                     end = "2023-06-10",                # end date: 10th June 2022
                     km_lr = 60,                        # kilometers left & right of the chosen location (lat/lon above)
                     km_ab = 60,                        # kilometers above and below the location
                     site_name = "Abtwil (AG)",         # the site name we want to give the data
                     internal = TRUE,
                     progress = FALSE
) %>% 
  as_tibble()

raster_ndvi <- mt_to_terra(df = df_ndvi) # converting subset into raster

### Plotting Data
#plot(raster_ndvi[[1]], zlim=c(-0.1, 0.95))

################################################################################
switch_on <- 0

if(switch_on == 1){
### 
scale_factor <- bands %>% 
  filter(band == "250m_16_days_NDVI") %>% 
  pull(scale_factor) %>% 
  as.numeric()

df_ndvi_spatialmean <- df_ndvi %>% 
  mutate(calendar_date = ymd(calendar_date)) %>%  # make the dates into comprehensible values not X.yyyy.mm.dd
  group_by(calendar_date) %>%                     # group the data by day 
  summarise(mean = mean(value), min = min(value), max = max(value)) %>%   # calculate mean, min and max across pixels
  mutate(mean = mean * scale_factor, min = min * scale_factor, max = max * scale_factor)  # apply scale_factor (see )

df_ndvi_spatialmean %>% 
  ggplot(aes(x = calendar_date)) +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "grey70") +
  geom_line(aes(y = mean))
}
################################################################################

### Gemeinde Grenzen
Gemeidegrenzen = read_sf("//fibl.ch/FILES/Dep_FSS/2_Projects/2020_CH-SNF_LEAF_35188/02_WorkPackages/WP2_ModelDevelopment/01_Data/00_Public/CH_Gemeindegrenzen/ch.swisstopo.swissboundaries3d-gemeinde-flaeche.fill/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")

Gemeidegrenzen <- st_transform(Gemeidegrenzen, "EPSG:4326")
#tm_shape(Gemeidegrenzen) + 
  #tm_borders() +
  #tm_grid()

### Reducing to Gemeinde in Aargau, Luzern, Zürich and Zug
Study_gemeinde <- Gemeidegrenzen %>% filter(KANTONSNUM %in% c(1, 3, 9, 19))

tm_shape(Study_gemeinde) + 
  tm_borders() +
  tm_grid()

#st_crs(ZG_gemeinde)

#cat(crs(raster_ndvi))

raster_ndvi <- project(raster_ndvi, "EPSG:4326", method = "near")

### Bezirksgebiet
#Bezirksgebiet = read_sf("//fibl.ch/FILES/Dep_FSS/2_Projects/2020_CH-SNF_LEAF_35188/02_WorkPackages/WP2_ModelDevelopment/01_Data/00_Public/CH_Gemeindegrenzen/ch.swisstopo.swissboundaries3d-gemeinde-flaeche.fill/swissBOUNDARIES3D_1_3_TLM_BEZIRKSGEBIET.shp")

###
area <- st_transform(Study_gemeinde, crs(raster_ndvi))

area_cropped <- crop(raster_ndvi, Study_gemeinde)

area_final <- mask(area_cropped, Study_gemeinde)

tm_shape(Study_gemeinde) + 
  tm_borders() +
  tm_grid()


plot(area_final[[1]], zlim=c(-0.1, 0.95))

area_final_df <- as.data.frame(area_final[[1]], xy = TRUE) %>%
  na.omit()

colnames(area_final_df) <- c("Longitude", "Latitude", "NDVI")
head(area_final_df)

ggplot() +
  geom_raster(data = area_final_df, aes(x = Longitude, y = Latitude, fill = NDVI)) +
  geom_sf(data = Study_gemeinde, fill = NA) + 
  scale_fill_viridis_c() +
  theme_bw()
  
  
