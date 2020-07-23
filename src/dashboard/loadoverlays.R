library(sf)
library(sp)
library(tidyverse)
library(here)
library(dplyr)

### loading overlay data, check file names
## food points
food_points <- here("/data/shps/cntrd/cntrd.shp") %>% st_read() %>%
  mutate(radius = case_when(pymnt_w == "yes" ~ 8,
                            pymnt_s == "yes" ~ 6,
                            is.na(pymnt_s) ~ 3))

## isochrones
isochrones <- here("/data/shps/isochrones/isochrones.shp") %>% st_read()
#isochrones <- spTransform(isochrones, CRS("+proj=longlat +datum=WGS84"))

