library(sf)
library(tidyverse)
library(here)

### loading baselayer data, check file names
## food
allfood <- read_csv(here("/data/shps/allfood.csv"))
allfood <- allfood %>% st_as_sf(coords = c("X", "Y"), crs = 4326, agr  = "constant")
