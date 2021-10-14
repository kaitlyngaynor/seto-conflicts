library(raster)
library(mapview)
library(dplyr)
library(sf)
library(ggplot2)
library(rgdal)

setwd("~/Dropbox/AAS GIS")

bathy <- raster("AllGEBCO/ghana_bathy")
plot(bathy)
mapview(bathy)

conflict <- raster("Kelly Data/kelly_kernd1")
plot(conflict)

bathy_75 <- bathy
bathy_75[bathy_75 < -75] <- NA
plot(bathy_75)

bathy_75_poly <- rasterToPolygons(bathy_75)
plot(bathy_75_poly)
names(bathy_75_poly) <- "bathy_75_poly"
writeOGR(bathy_75_poly, "Kaitlyn Data/bathy_75_poly.shp", 
         layer = "bathy_75_poly",
         driver = "ESRI Shapefile")

bathy_30 <- bathy
bathy_30[bathy_30 < -30] <- NA
plot(bathy_30)
plot(bathy_30, col = "grey")

# does this look like the shapefile?
contour_30 <- st_read("Kelly Data/30mContourpoly.shp") %>% 
    st_transform(crs = "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs")
plot(contour_30)

mapview(bathy_30) + mapview(contour_30)

ggplot() +
    geom_sf(data = contour_30,
            fill = "grey", color = "grey", alpha = 1) +
    theme_minimal() 

# no it doesn't
