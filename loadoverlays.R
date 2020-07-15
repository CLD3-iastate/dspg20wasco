library(sf)
library(tidyverse)

### loading baselayer data, check file names
## food
allfood <- read_csv("~/git/DSPG2020/wasco/data/shps/allfood.csv")
allfood <- allfood %>% st_as_sf(coords = c("X", "Y"), crs = 4326, agr  = "constant")
