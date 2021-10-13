library(raster)
library(mapview)

setwd("~/Dropbox/AAS GIS")

bathy <- raster("AllGEBCO/ghana_bathy")
plot(bathy)
mapview(bathy)

conflict <- raster("Kelly Data/kelly_kernd1")
plot(conflict)

bathy_75 <- bathy
bathy_75[bathy_75 < -75] <- NA
plot(bathy_75)

bathy_30 <- bathy
bathy_30[bathy_30 < -30] <- NA
plot(bathy_30)
