### Setup

library(tidyverse)
library(MODISTools)
library(raster)
library(stringr)
library(lubridate)

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
                     start = "2000-02-18",              # start date: 10th June 2022
                     end = "2023-06-10",                # end date: 10th June 2022
                     km_lr = 15,                        # kilometers left & right of the chosen location (lat/lon above)
                     km_ab = 15,                        # kilometers above and below the location
                     site_name = "Abtwil (AG)",         # the site name we want to give the data
                     internal = TRUE,
                     progress = FALSE
) %>% 
  as_tibble()

raster_ndvi <- mt_to_terra(df = df_ndvi) # converting subset into raster

### Plotting Data
plot(raster_ndvi[[21:24]], zlim=c(-0.1, 0.95))

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

c
#------------------------------------------------------------------------------#
### Pulling LAI

latit <- 47.176508
longi <- 8.354773

bands <- mt_bands(product = "MOD11A2") %>% as_tibble() # weather at 1 km resolution
dates <- mt_dates(product = "MOD11A2", lat = latit, lon = longi) %>% as_tibble() # Setting center to home 

df_met <- mt_subset(product = "MOD11A2",               # the chosen product
                     lat = latit,                       # desired lat/lon
                     lon = longi,
                     band = "LST_Day_1km",        # chosen band defining spatial and temporal scale
                     start = "2023-04-07",              # start date: x
                     end = "2023-06-18",                # end date: y
                     km_lr = 25,                        # kilometers left & right of the chosen location (lat/lon above)
                     km_ab = 25,                        # kilometers above and below the location
                     site_name = "Abtwil (AG)",         # the site name we want to give the data
                     internal = TRUE,
                     progress = FALSE
) %>% 
as_tibble()

raster_met <- mt_to_terra(df = df_met) # converting subset into raster

### Plotting Data
plot(raster_met[[7:12]], zlim=c(285, 315))
