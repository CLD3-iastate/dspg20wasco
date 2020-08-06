library(sf)
library(here)
library(tidyverse)
library(osmdata)

## Loading in baselayer data, check file names

# townships
townships <- ("Data/shps/townships/townships.shp") %>% st_read()
# unincorporated
unincorporated <- ("Data/shps/unincorporated/unincorporated.shp") %>% st_read()
# schools
swsd <- ("Data/shps/swsd/swsd.shp") %>% st_read()
# county
countyline <- ("Data/shps/county/countyline.shp") %>% st_read()
# roads
roads <- ("Data/shps/roads/roads.shp") %>% st_read()
# neighboring counties
neighboring_counties <- ("Data/shps/neighboring_counties/neighboring_counties.shp") %>% st_read
# acs_tracts
acs_tracts <- ("Data/shps/acs_tracts/acs_tracts.shp") %>% st_read



