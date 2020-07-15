library(sf)
library(tidyverse)

### loading baselayer data, check file names
## food
allfood <- read_csv("~/git/dspg20wasco/data/shps/allfood.csv")
allfood <- allfood %>% st_as_sf(coords = c("X", "Y"), crs = 4326, agr  = "constant")
