# install.packages("remotes")
# remotes::install_github("GIScience/openrouteservice-r")
library(remotes)
library(openrouteservice)
library(mapview)
library(sf)
library(here)
library(tidyverse)
library(join)

## NOTE: I could not install openrouteservice on R instance in Rivanna. I had to run this script locally.

# API key
ors_api_key("5b3ce3597851110001cf6248e8202f23d229460592d1a52a016738d9")

# Reading in shapefile of store points
cntrd <- st_read(here("/cntrd/cntrd.shp"))

# Creating a table for input to ors_isochrones, must be only lons and lats
coords <- do.call(rbind, st_geometry(cntrd)) %>%
  as_tibble() %>% setNames(c("lon","lat"))

## 1 hour range split into 20 minute intervals, I can only request 5 at a time so I subset in groups of five, stored in objects named res1-10
res10 <- ors_isochrones(coords[51:55,], range = 3600, interval = 1800, output = "sf")
res1

## Here I had to hack at rejoining the isochrones to the original cntrd data frame. I went back to in the groups of five and added a group_index variable to join with the res data frame and dropped the geometries. Then I left joined so that only the store information is matched to the isochrones geometry
resmatch <- cntrd[51:55,] %>% mutate(group_index = 0:4) %>% st_drop_geometry()
res10 <- left_join(res10, resmatch, by = "group_index")

# To create the final dataframe of all isochrones I rbind
all_isochrones <- do.call("rbind", list(res, res1, res2, res3, res4, res5, res6, res7, res8, res9, res10))

# Write it as a shapefile
st_write(all_isochrones, here("/isochrones/isochrones.shp"))

# I did a sanity check to make sure all isochrones are mapping to the stores they should be.

