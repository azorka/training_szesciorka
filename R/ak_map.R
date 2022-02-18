library(sf)
library(ggplot2)
library(leaflet)
library(scales)
library(ggmap)
library(dplyr)

## Read in shapefile using sf
ak_regions <- read_sf("shapefiles/ak_regions_simp.shp")

plot(ak_regions)  

head(ak_regions)

st_crs(ak_regions)


