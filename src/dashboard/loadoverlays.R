library(sf)
library(sp)
library(tidyverse)
library(here)
library(dplyr)

### loading overlay data, check file names
## food points
food_points <- ("Data/shps/cntrd/cntrd.shp") %>% st_read() %>%
  mutate(radius = case_when(pymnt_w == "yes" ~ 8,
                            pymnt_s == "yes" ~ 6,
                            is.na(pymnt_s) ~ 3)) %>% 
  mutate(pymnt_types = case_when(
    pymnt_s == "yes" & pymnt_w == "yes" ~ "SW",
    pymnt_s == "yes" & is.na(pymnt_w) ~ "S",
    is.na(pymnt_s) & pymnt_w == "yes" ~ "W",
    is.na(pymnt_s) & is.na(pymnt_w) ~ ""
  ))

## isochrones
isochrones <- ("Data/shps/isochrones/isochrones.shp") %>% st_read()
#isochrones <- spTransform(isochrones, CRS("+proj=longlat +datum=WGS84"))

