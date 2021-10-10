library(raster)
library(viridis)
library(spatialEco)
library(mapview)
library(sf)
library(tidyr)

setwd("~/Dropbox/AAS GIS")

test1 <- raster("Kelly Data/kelly_pointd1")
test2 <- raster("Kelly Data/kelly_pointd2")
test3 <- raster("Kelly Data/kelly_pointd3")
test4 <- raster("Kelly Data/kelly_pointd4")
test5 <- raster("Kelly Data/kelly_kernd1")

plot(test1, col = viridis(256))
plot(test2, col = viridis(256))
plot(test3, col = viridis(256))
plot(test4, col = viridis(256))
plot(test5, col = viridis(256))

# let's go with the last one, looks good?
kernel_incidents <- test5
kernel_incidents[kernel_incidents == 0] <- NA

kernel_99 <- raster.vol(kernel_incidents, p = 0.99)
kernel_95 <- raster.vol(kernel_incidents, p = 0.95)
kernel_90 <- raster.vol(kernel_incidents, p = 0.90)
kernel_75 <- raster.vol(kernel_incidents, p = 0.75)
kernel_50 <- raster.vol(kernel_incidents, p = 0.50)

plot(kernel_99)
plot(kernel_95)
plot(kernel_90)
plot(kernel_75)
plot(kernel_50)

# all_kernel <- kernel_50 + kernel_75 + kernel_95 + kernel_99
all_kernel <- kernel_50 + kernel_75 + kernel_95
all_kernel[all_kernel == 0] <- NA
plot(all_kernel)
mapview(all_kernel)


# compare to polygons
eez <- st_read("Kelly Data/coastlinebufftoEEZ_15km.shp") %>% 
    st_transform(crs = "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs")
port_10 <- st_read("Kelly Data/3Port_10km.shp") %>% 
    st_transform(crs = "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs")
port_20 <- st_read("Kelly Data/3Port_20km.shp") %>% 
    st_transform(crs = "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs")
contour_30 <- st_read("Kelly Data/30mContourpoly.shp") %>% 
    st_transform(crs = "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs")
iez <- st_read("AAS_Layers/IEZ.shp") %>% 
    st_transform(crs = "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs")
mapview(iez) + mapview(contour_30, col.regions = "red")

# overlap of kernels

# for rasters equal to 0 or 1, calculating sum is just counting the pixels equal to 1 (relative area)
cellStats(kernel_95, 'sum')
cellStats(kernel_90, 'sum')
cellStats(kernel_75, 'sum')
cellStats(kernel_50, 'sum')

cellStats(crop(kernel_95, iez), 'sum')

mapview(iez) + mapview(kernel_95)


# write function to calculate kernel overlap with different polygons
calculate_overlap <- function(kernel, spatial_layer) {
    
    # calculate the kernel of interest
    kernel_layer <- raster.vol(kernel_incidents, p = kernel)
    
    # calculate number of pixels = 1
    kernel_area <- cellStats(kernel_layer, 'sum')
    
    # calculate number that overlap
    kernel_overlap <- cellStats(crop(kernel_layer, spatial_layer), 'sum')
    
    # calculate percent
    return(kernel_overlap/kernel_area)
    
}

# this version has error catching
calculate_overlap <- function(kernel, spatial_layer) {
    
    out <- tryCatch(
        {
            # calculate the kernel of interest
            kernel_layer <- raster.vol(kernel_incidents, p = kernel)
            
            # calculate number of pixels = 1
            kernel_area <- cellStats(kernel_layer, 'sum')
            
            # calculate number that overlap
            kernel_overlap <- cellStats(crop(kernel_layer, spatial_layer), 'sum')
            
            # calculate percent
            kernel_overlap/kernel_area
        },
        error=function(cond) {
            return(0)
        }
    )
    return(out)
    
    
}


calculate_overlap(kernel = 50, spatial_layer = iez)
calculate_overlap(kernel = 50, spatial_layer = port_10)
calculate_overlap(kernel = 50, spatial_layer = eez)


    
    