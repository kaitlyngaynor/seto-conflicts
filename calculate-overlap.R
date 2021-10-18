library(sf)
library(raster)
library(tidyr)
library(spatialEco)
library(dplyr)
library(mapview)
library(ggplot2)
library(forcats)
library(stringr)

setwd("~/Dropbox/AAS GIS")

# import raster of modeled incidents
kernel_incidents <- raster("Kelly Data/kelly_kernd1")
kernel_incidents[kernel_incidents == 0] <- NA
plot(kernel_incidents)

# import and reproject all spatial data layers
contour_75 <- st_read("Kaitlyn Data/bathy_75_poly.shp") %>% 
    st_transform(crs = "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs")
port_10 <- st_read("Kelly Data/3Port_10km.shp") %>% 
    st_transform(crs = "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs")
port_20 <- st_read("Kelly Data/3Port_20km.shp") %>% 
    st_transform(crs = "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs")
contour_30 <- st_read("Kelly Data/30mContourpoly.shp") %>% 
    st_transform(crs = "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs")
iez <- st_read("AAS_Layers/IEZ.shp") %>% 
    st_transform(crs = "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs")

calculate_overlap <- function(kernel) {
    
    # calculate the kernel of interest
    kernel_layer <- raster.vol(kernel_incidents, p = kernel)
    
    plot(kernel_layer)
    
    # calculate number of pixels = 1
    kernel_area <- cellStats(kernel_layer, 'sum')
    
    overlap <- data.frame(kernel = paste0("K", kernel),
                          kernel_num = kernel,
                          iez = cellStats(crop(kernel_layer, iez), 'sum')/kernel_area,
                          port_10 = cellStats(crop(kernel_layer, port_10), 'sum')/kernel_area,
                          port_20 = cellStats(crop(kernel_layer, port_20), 'sum')/kernel_area,
                          contour_30 = cellStats(crop(kernel_layer, contour_30), 'sum')/kernel_area,
                          contour_75 = cellStats(crop(kernel_layer, contour_75), 'sum')/kernel_area)
    
    return(overlap)
    
}

kernel_list <- c(seq(0.5, 0.95, by = 0.05), 0.99)

overlap_all <- lapply(kernel_list, calculate_overlap) %>% 
    bind_rows

# Publication figures -----------------------------------------------------------------

# Bar plot
overlap_all %>% 
    filter(kernel %in% c("K0.5", "K0.95", "K0.99")) %>% 
    mutate(kernel2 = recode_factor(kernel, "K0.5" = "50%", 
                                   "K0.95" = "95%",
                                   "K0.99" = "99%")) %>% 
    pivot_longer(cols = iez:contour_75,
                 names_to = "layer",
                 values_to = "overlap") %>% 
    mutate(layer = recode_factor(layer, "contour_30" = "30m Isobath", 
                                 "contour_75" = "Continental Shelf",
                                 "iez" = "Inshore Exclusion Zone",
                                 "port_10" = "10km from Port",
                                 "port_20" = "20km from Port")) %>% 
ggplot(aes(x = layer, y = overlap, fill = kernel2)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(values = c("#e76f51","#e3a612", "#e9c46a")) +
    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 8)) +
    theme_bw() +
    xlab("") +
    ylab("Overlap with Spatial Feature") +
    labs(fill = "Conflict Isopleth")
ggsave("Kaitlyn Data/overlap-bar.pdf", width = 6, height = 3)

# Bar plot v2
overlap_few %>%
    select(-c(kernel_num, kernel2)) %>% 
    pivot_wider(values_from = overlap, names_from = kernel) %>% 
    mutate(K0.5 = K0.5 - K0.95,
           K0.95 = K0.95 - K0.99) %>% 
    pivot_longer(cols = K0.5:K0.99,
                 names_to = "kernel",
                 values_to = "overlap") %>% 
    mutate(layer = recode_factor(layer, "contour_30" = "30m Isobath", 
                                 "contour_75" = "Continental Shelf",
                                 "iez" = "Inshore Exclusion Zone",
                                 "port_10" = "10km from Port",
                                 "port_20" = "20km from Port")) %>% 
    mutate(kernel2 = recode_factor(kernel, "K0.5" = "50%", 
                                   "K0.95" = "95%",
                                   "K0.99" = "99%")) %>% 
    ggplot(aes(x = layer, y = overlap, fill = kernel2)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("#e76f51","#e3a612", "#e9c46a")) +
    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 8)) +
    theme_bw() +
    xlab("") +
    ylab("Overlap with Spatial Feature") +
    labs(fill = "Conflict Isopleth")
ggsave("Kaitlyn Data/overlap-bar-v2.pdf", width = 6, height = 3)


# make a map of kernels
kernel_99 <- raster.vol(kernel_incidents, p = 0.99)
kernel_95 <- raster.vol(kernel_incidents, p = 0.95)
kernel_50 <- raster.vol(kernel_incidents, p = 0.50)
all_kernel <- kernel_50 + kernel_95 + kernel_99
all_kernel[all_kernel == 0] <- NA
plot(all_kernel, col = c("#e9c46a", "#e3a612", "#e76f51"))


# Line plot
overlap_all %>% 
    pivot_longer(cols = iez:contour_75,
                 names_to = "layer",
                 values_to = "overlap") %>% 
    mutate(layer = recode_factor(layer, "contour_30" = "30m Isobath", 
                                 "contour_75" = "Continental Shelf",
                                 "iez" = "Inshore Exclusion Zone",
                                 "port_10" = "10km from Port",
                                 "port_20" = "20km from Port")) %>% 
    mutate(layer = fct_reorder(layer, desc(overlap))) %>% 
ggplot(aes(x = kernel_num, y = overlap, col = layer)) +
    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    #scale_x_continuous(labels = scales::percent_format(scale = 100)) +
    scale_x_reverse() +
    scale_color_manual(values = c("#3CB7CC", "#FFD94A", "#32A251", "#FF7F0F", "#B85A0D")) +
    geom_line(size = 1) +
    theme_bw() +
    ylab("Overlap with Spatial Feature") +
    xlab("Modeled Conflict Intensity (Isopleth)") +
    theme(legend.title = element_blank())
ggsave("Kaitlyn Data/overlap-line.pdf", width = 6, height = 3)

# Line plot v2
overlap_all %>% 
    pivot_longer(cols = iez:contour_75,
                 names_to = "layer",
                 values_to = "overlap") %>% 
    mutate(layer = recode_factor(layer, "contour_30" = "30m Isobath", 
                                 "contour_75" = "Continental Shelf",
                                 "iez" = "Inshore Exclusion Zone",
                                 "port_10" = "10km from Port",
                                 "port_20" = "20km from Port")) %>% 
    mutate(layer = fct_reorder(layer, desc(overlap))) %>% 
    ggplot(aes(x = kernel_num, y = overlap, col = layer)) +
    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    #scale_x_continuous(labels = scales::percent_format(scale = 100)) +
    #scale_x_reverse() +
    scale_color_manual(values = c("#3CB7CC", "#FFD94A", "#32A251", "#FF7F0F", "#B85A0D")) +
    geom_line(size = 1) +
    theme_bw() +
    ylab("Overlap with Spatial Feature") +
    xlab("Modeled Conflict Intensity (Isopleth)") +
    theme(legend.title = element_blank())
ggsave("Kaitlyn Data/overlap-line-v2.pdf", width = 6, height = 3)

# Extra figures -----------------------------------------------------------------

overlap_few <- overlap_all %>% 
    filter(kernel %in% c("K0.5", "K0.95", "K0.99")) %>% 
    mutate(kernel2 = recode_factor(kernel, "K0.5" = "50% Conflict Density (Core Zone)", 
                                   "K0.95" = "95% Conflict Density",
                                   "K0.99" = "99% Conflict Density")) %>% 
    pivot_longer(cols = iez:contour_75,
                 names_to = "layer",
                 values_to = "overlap") %>% 
    mutate(layer = recode_factor(layer, "contour_30" = "30m Isobath", 
                                 "contour_75" = "Continental Shelf",
                                 "iez" = "IEZ",
                                 "port_10" = "10km from Port",
                                 "port_20" = "20km from Port"))

ggplot(overlap_few, aes(x = layer, y = overlap, fill = kernel2)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(values = c("#e76f51","#e3a612", "#e9c46a")) +
    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    theme_bw() +
    xlab("") +
    ylab("Percent Overlap") +
    theme(legend.title = element_blank())

# just 50 and 99
overlap_fewer <- overlap_all %>% 
    filter(kernel %in% c("K0.5", "K0.95")) %>% 
    mutate(kernel2 = recode_factor(kernel, "K0.5" = "50% Conflict Density (Core Zone)", 
                                   "K0.95" = "95% Conflict Density")) %>% 
    pivot_longer(cols = iez:contour_75,
                 names_to = "layer",
                 values_to = "overlap") %>% 
    mutate(layer = recode_factor(layer, "contour_30" = "30m Isobath", 
                                 "contour_75" = "Continental Shelf",
                                 "iez" = "IEZ",
                                 "port_10" = "10km from Port",
                                 "port_20" = "20km from Port"))

ggplot(overlap_fewer, aes(x = layer, y = overlap, fill = kernel2)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(values = c("#e76f51", "#e9c46a")) +
    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    theme_bw() +
    xlab("") +
    ylab("Percent Overlap") +
    theme(legend.title = element_blank())

# calculate version with marginal 
overlap_marginal <- overlap_few %>%
    select(-c(kernel_num, kernel2)) %>% 
    pivot_wider(values_from = overlap, names_from = kernel) %>% 
    mutate(K0.5 = K0.5 - K0.95,
           K0.95 = K0.95 - K0.99) %>% 
    pivot_longer(cols = K0.5:K0.99,
                 names_to = "kernel",
                 values_to = "overlap") %>% 
    mutate(layer = recode_factor(layer, "contour_30" = "30m Isobath", 
                                 "contour_75" = "Continental Shelf",
                                 "iez" = "IEZ",
                                 "port_10" = "10km from Port",
                                 "port_20" = "20km from Port")) %>% 
    mutate(kernel2 = recode_factor(kernel, "K0.5" = "50% Conflict Density (Core Zone)", 
                                   "K0.95" = "95% Conflict Density",
                                   "K0.99" = "99% Conflict Density"))

ggplot(overlap_marginal, aes(x = layer, y = overlap, fill = kernel2)) +
    geom_bar(stat = "identity") +
    #scale_fill_brewer(palette = "Greens") +
    scale_fill_manual(values = c("#e76f51","#e3a612", "#e9c46a")) +
    theme_bw() +
    xlab("") +
    ylab("Percent Overlap") +
    theme(legend.title = element_blank())

# something continuous?
overlap_long <- overlap_all %>% 
    pivot_longer(cols = iez:contour_30,
                 names_to = "layer",
                 values_to = "overlap")
ggplot(overlap_long, aes(x = kernel_num, y = overlap, col = layer)) +
    geom_line(size = 1) +
    theme_bw()



