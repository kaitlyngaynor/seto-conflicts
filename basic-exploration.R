# basic exploration

setwd("~/Dropbox/AAS GIS")

library(mapview)
library(sf)
library(dplyr)
library(spatialEco)
library(sp)
library(raster)

# have a look at what is in there - first two don't have correct CRS
incidents1 <- st_read("Kelly Data/PotentialIncidentLocations2_17_16.shp")
incidents2 <- st_read("Kelly Data/PotentialLocations2_17_16.shp")
incidents3 <- st_read("Kelly Data/PotLoc2_17_16.shp")
incidents4 <- st_read("Kelly Data/PotentialIncidentLocations01Revised.shp")
incidents5 <- st_read("Kelly Data/Kellymadeitwork2_18_16.shp")
incidents6 <- st_read("Kelly Data/incidents_all_merge.shp")

# pick one that seems right, ish
incidents <- incidents5
mapview(incidents)
head(incidents)

# count points per incident
point_counts <- count(incidents, cid) %>% 
    st_drop_geometry
head(point_counts)

# join and calculate weights
incidents <- left_join(incidents, point_counts) %>% 
    mutate(weight = 1/n)

# convert to SPDF
incidents_sp <- as(incidents, "Spatial")

# calculate kernel density
kernel_incidents <- sp.kde(x = incidents_sp,
                           y = incidents_sp$weight,
                           nr = 100,
                           nc = 100)
plot(kernel_incidents)

# calculate various rasters
kernel_95 <- raster.vol(test, p = 0.95)
kernel_75 <- raster.vol(test, p = 0.75)
kernel_50 <- raster.vol(test, p = 0.50)

plot(kernel_75)
plot(kernel_50)

# convert to polygons
kernel_95_poly <- rasterToPolygons(kernel_95)

# https://gis.stackexchange.com/questions/272950/95-contour-from-kernel-density-estimates