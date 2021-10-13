library(sf)

setwd("~/Dropbox/AAS GIS")

# read in shape file
potential_points <- st_read("Kelly Data/Kellymadeitwork2_18_16.shp")

# export it as a csv
write.csv(potential_points, "Kaitlyn Data/Kellymadeitwork2_18_16.csv", row.names = F)


## as a bonus, here is an easy way to visualize spatial points in R
library(mapview)
mapview(potential_points)
