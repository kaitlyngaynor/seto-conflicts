library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)

setwd("~/Dropbox/AAS GIS")

# read in csv of reports
reports <- st_read("Kaitlyn Data/Kellymadeitwork2_18_16.csv")
u_reports<-reports[, -c(3, 4, 5, 6, 7)]
u_reports<- unique(u_reports)

# read in village locations layer
villages <- read.csv("Kelly Data/Villages1.12.16.csv")

#making a table out of the count of villages 
length(unique(u_reports$Village))
table<- u_reports %>% 
    count(Village)
table<- rename(table, NAME = Village)
merged<- merge(villages, table, by = "NAME", all.x= TRUE)
head(merged)

merged <- st_as_sf(merged,
                   coords = c("LONGITUDE", "LATITUDE"),
                   crs = "+proj=longlat +datum=WGS84")

# load in coast
ghana <- st_read("Borders/countries_shp/countries.shp") %>% 
    dplyr::filter(COUNTRY == "Ghana") %>% 
    st_transform(crs = "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs")

others <- st_read("Borders/countries_shp/countries.shp") %>% 
    dplyr::filter(COUNTRY %in% c("Côte d'Ivoire", "Togo", "Benin")) %>% 
    st_transform(crs = "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs")

ggplot() +
    geom_sf(data = ghana, fill = "gray") +
    geom_sf(data = others, fill = "gray") +
    geom_sf(data = merged, col = "blue", alpha = 0.5, aes(size = n)) +
    coord_sf(xlim = c(-3.25, 1.5), ylim = c(4.5, 6.25),
             crs = "+proj=longlat +datum=WGS84") + 
    annotation_scale(location = "br", width_hint = 0.25) +
    theme_bw()
ggsave("Kaitlyn Data/village_map.pdf")
