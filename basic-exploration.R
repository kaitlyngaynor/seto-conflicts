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

# figure out summary stats
min(point_counts$n)
max(point_counts$n)
mean(point_counts$n)
sd(point_counts$n)
nrow(point_counts)
min(incidents$Depth_m)

# histogram of sample sizes
ggplot(point_counts, aes(x = n)) +
    geom_histogram() +
    theme_bw() +
    xlab("Number of Potential Conflict Locations") +
    ylab("Number of Reports")

# join and calculate weights
incidents <- left_join(incidents, point_counts) %>% 
    mutate(weight = 1/n)

# convert to SPDF
incidents_sp <- as(incidents, "Spatial")

# calculate kernel density
kernel_incidents <- sp.kde(x = incidents_sp,
                           y = incidents_sp$weight,
                           bw = 7500,
                           nr = 1000,
                           nc = 1000)

# convert 0 points to NA
kernel_incidents_thin <- kernel_incidents
kernel_incidents_thin[kernel_incidents_thin < 0.0000000000001] <- NA
# kernel_incidents_thin[kernel_incidents_thin == 0] <- NA

plot(kernel_incidents, col=viridis(256))

# calculate various rasters
# https://gis.stackexchange.com/questions/272950/95-contour-from-kernel-density-estimates
kernel_95 <- raster.vol(kernel_incidents, p = 0.95)
kernel_75 <- raster.vol(kernel_incidents, p = 0.75)
kernel_50 <- raster.vol(kernel_incidents, p = 0.50)

plot(kernel_95)
plot(kernel_75)
plot(kernel_50)



