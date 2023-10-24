# calculate distance to the sea (shoreline) for Kerguelen:



library(sf)
library(terra)
library(stars)



# source("run4/clean_occurrences.R")


ker_shp <- sf::st_read("../data/SIG/Contours/KER_contours.shp")
# plot(ker_shp)

ker_sites_sf <- sf::st_as_sf(ker_sites_xy, coords=c("longitude", "latitude"), crs=4326)
sea_dist <- sf::st_distance(ker_sites_sf, ker_shp)/1000 %>% as.numeric()


# ffffuuuck pythagorian distance at the pole