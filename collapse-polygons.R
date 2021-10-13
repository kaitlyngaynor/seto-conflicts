library(dplyr)
library(sf)
library(ggplot2)

setwd("~/Dropbox/AAS GIS")

# read in original IEZ layer
iez <- st_read("AAS_Layers/IEZ.shp") %>% 
    st_transform(crs = "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs")

# plot it
ggplot() +
    geom_sf(data = iez,
            fill = "grey", color = "black", alpha = 0.6) +
    theme_bw() 

# now find the union among these polygons
iez_union <- iez %>% 
    st_union()

# plot it
ggplot() +
    geom_sf(data = iez_union,
            fill = "grey", color = "black", alpha = 0.6) +
    theme_bw() 

# export it
st_write(iez_union, "Kaitlyn Data/st_union.shp")
