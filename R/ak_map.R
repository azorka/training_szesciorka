library(sf)
library(ggplot2)
library(leaflet)
library(scales)
library(ggmap)
library(dplyr)

## Read in shapefile using sf
ak_regions <- read_sf("shapefiles/ak_regions_simp.shp")

plot(ak_regions)  

# look at data
head(ak_regions)

# see what crs is is and other data
st_crs(ak_regions)

# transform crs
ak_regions_3338 <- ak_regions %>% st_transform(crs=3338)
st_crs(ak_regions_3338)

# plot
plot(ak_regions_3338)

# shows reginos
ak_regions_3338 %>% select(region)

# filter out to a region
ak_regions_3338 %>% filter(region == "Southeast")

# read in population
pop <- read.csv("shapefiles/alaska_population.csv")

# convert to sf
# Have to read it in the crs it was created in THEN transform it!!!
pop_4326 <- st_as_sf(pop, coords=c('lng', 'lat'), crs=4326, remove=FALSE)
head(pop_4326)

# spatial join - will not work
pop_joined <- st_join(pop_4326, ak_regions_3338, join=st_within)

# transform crs
pop_3338 <- st_transform(pop_4326, crs=3338)

# spatial join
pop_joined <- st_join(pop_3338, ak_regions_3338, join=st_within)
head(pop_joined)

# group and summarize

pop_region <- pop_joined %>% 
  as.data.frame() %>% 
  group_by(region) %>% 
  summarise(total_pop=sum(population))

head(pop_region)

# drop geom 
drop <- pop_joined %>% as.data.frame() 
drop <- st_drop_geometry(pop_joined)

pop_region_3338 <- left_join(ak_regions_3338, pop_region)

#plot to check
plot(pop_region_3338["total_pop"])

# population of AK
pop_mgmt_338 <- pop_region_3338 %>% 
  group_by(mgmt_area) %>% 
  summarize(total_pop=sum(total_pop))

plot(pop_mgmt_338["total_pop"])

pop_mgmt_3338 <- pop_region_3338 %>% 
  group_by(mgmt_area) %>% 
  summarize(total_pop=sum(total_pop), do_union=F)

plot(pop_mgmt_3338["total_pop"])

write_sf(pop_region_3338, "shapefiles/ak_regions_population.shp", delete_layer=TRUE)

# visualization witg ggplot

ggplot(pop_region_3338) +
  geom_sf(aes(fill=total_pop)) +
  theme_bw() +
  labs(fill="Total Population") +
  scale_fill_continuous(low="khaki", high= "firebrick", labels=comma)

rivers_3338 <- read_sf("shapefiles/ak_rivers_simp.shp")
st_crs(rivers_3338)

ggplot() +
  geom_sf(data=pop_region_3338, aes(fill=total_pop)) +
  geom_sf(data=rivers_3338, aes(size=StrOrder), color="black") +
  geom_sf(data=pop_3338, aes(), size=.5) +
  scale_size(range=c(0.01, 0.2), guide=F) +
  theme_bw() +
  labs(fill="Total Population") +
  scale_fill_continuous(low="khaki", high= "firebrick", labels=comma)

# incorporate base and static
pop_3857 <- pop_3338 %>%
  st_transform(crs=3857)

# Define a function to fix the bbox to be in EPSG:3857
# See https://github.com/dkahle/ggmap/issues/160#issuecomment-397055208
ggmap_bbox_to_3857 <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs=4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

bbox <- c(-170, 52, -130, 64)   # This is roughly southern Alaska
ak_map <- get_stamenmap(bbox, zoom=4)
ak_map_3857 <- ggmap_bbox_to_3857(ak_map)

ggmap(ak_map_3857) + 
  geom_sf(data=pop_3857, aes(color=population), inherit.aes=F) +
  scale_color_continuous(low="khaki", high= "firebrick", labels=comma)

# Visualize sf objects with leaflet

epsg3338 <- leaflet::leafletCRS(
  crsClass="L.Proj.CRS",
  code="EPSG:3338",
  proj4def= "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
  resolutions=2^(16:7))

st_crs(pop_region_3338)

pop_region_4326 <- pop_region_3338 %>% st_transform(crs=4326)

m <- leaflet(options=leafletOptions(crs=epsg3338)) %>%
  addPolygons(data=pop_region_4326, 
              fillColor="gray",
              weight=1)

m


