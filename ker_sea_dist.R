# calculate distance to the sea (shoreline) for Kerguelen:

crozet=FALSE

library(sf)
library(terra)
library(stars)
library(ggplot2)
library(magrittr)

source("run4/clean_occurrences.R")


# Calculating distances from the sea.
# we have the contour of Ker, and need to calculate the distance of every grid
# cell to the sea. So we need the grid cells. Let's align on the ones from the
# other chelsa environmental stuff:


# ideas from here: https://gis.stackexchange.com/questions/243994/how-to-calculate-distance-from-point-to-linestring-in-r-using-sf-library-and-g
# 
# 
# 
bio1_ker <- terra::rast("../data/chelsa/bio1_downscaled_ker.tif")*0.1-273.15 # this is the downscaled temperature
ker_shp <- sf::st_read("../data/SIG/Contours/KER_contours.shp")

# crop bio1 and ker_shp:
ker_vect <- terra::vect(ker_shp)
terra::crs(ker_vect) == terra::crs(bio1_ker)
bio1_ker <- terra::project(bio1_ker, ker_vect)
bio1_ker_crop <- terra::crop(bio1_ker, ker_vect, mask=TRUE)
plot(bio1_ker_crop)
plot(bio1_ker)
plot(ker_vect, add=TRUE)

# non-na cells:
no_na <- which(is.na(values(bio1_ker_crop)))
vec <- 1:ncell(bio1_ker_crop)
no_na <- vec[-no_na]
ker_cell_xy <- terra::xyFromCell(bio1_ker_crop, no_na) # extracts the coords of non na bio_ker cells
ker_xy_sf <- ker_cell_xy %>% 
  as.data.frame %>% 
  st_as_sf(coords=c("x", "y"), crs = terra::crs(bio1_ker_crop) )

# ok now to calculate nearest distances, need to break up the data into smaller
# tiles, since kerguelen borders are too long. 

terra::ext(bio1_ker_crop)
terra::ext(bio1_ker)

topleft <- terra::ext(68.420, 69.5, -49.25, -48.4519) 
topright <-   terra::ext( 69.5, 70.56, -49.25, -48.4519)
bottomright <-   terra::ext(69.5, 70.56, -50.001, -49.25)
bottomleft <-   terra::ext(68.420, 69.5, -50.001, -49.25)

topleft_r <- terra::crop(bio1_ker_crop, topleft)
topright_r <- terra::crop(bio1_ker_crop, topright)
bottomright_r <- terra::crop(bio1_ker_crop, bottomright)
bottomleft_r <- terra::crop(bio1_ker_crop, bottomleft)

topleft_v <- terra::crop(ker_vect, topleft)
topright_v <- terra::crop(ker_vect, topright)
bottomright_v <- terra::crop(ker_vect, bottomright)
bottomleft_v <- terra::crop(ker_vect, bottomleft)




dist1 <- sf::st_distance(ker_xy_sf)


distances <- sf::st_distance(ker_xy_sf[1000:1050,], ker_shp) # this takes forever

ker_vect <- terra::vect("../data/SIG/Contours/KER_contours.shp")

library(ggplot2)
 ggplot() + geom_sf(data=ker_shp) +
  geom_sf(data=ker_xy_sf[1:50,]) # this makes no sense at all
 
ker_points <- stars::read_stars("../data/chelsa/bio1_downscaled_ker.tif") 

kerp <- st_as_sf(ker_points, as_points=TRUE, merge=FALSE) # not work, or too long
 
crs(bio1_ker) == crs(ker_shp)
bio1_ker <- terra::project(bio1_ker, ker_shp)
bio1 <- terra::crop(bio1_ker, ker_shp, mask=TRUE) # still don't think this fits right ...???

plot(bio1)
 ker_contour <- terra::rasterize(ker_shp, bio1_ker) # ok but it's filled in!
 
# en fait va falloir que je mette des mask sur les rasters de ker



border_cells <- sf::st_intersection(ker_shp, bio1_ker)



ker_shp <- sf::st_read("../data/SIG/Contours/KER_contours.shp")
# plot(ker_shp)
ker_shp_4326 <- sf::st_transform(ker_shp, crs = 4326)
ker_line <- st_cast(ker_shp_4326, to="MULTILINESTRING")
ker_shp_7079 <- ker_shp %>% sf::st_transform(crs = 7079)
ker_line_7079 <- ker_line %>% sf::st_transform(crs = 7079)
ker_sites_sf <- sf::st_as_sf(ker_sites_xy, coords=c("longitude", "latitude"), crs=4326) %>% st_transform(crs = 4326)
ker_sites_sf_7079 <- sf::st_as_sf(ker_sites_xy, coords=c("latitude", "longitude"), crs=4326) %>%
 st_transform(crs=7079)
ker_coord <- ker_sites_sf_7079 %>% st_coordinates() %>% as.data.frame


box1 <- st_bbox(c(xmin = 68, xmax = 71, ymax = -50, ymin = -48), crs = st_crs(4326))
mask <- st_difference(box1, ker_shp)


sea_dist_wrong <- sf::st_distance(ker_sites_sf, ker_shp_4326)# this gives a response in degrees. But why the weird format ? 10 columns ?
sea_dist_deg <- sf::st_distance(ker_sites_sf_7079, ker_shp_7079)# this gives a response in degrees.
sea_dist_deg <- sf::st_distance(ker_sites_sf_7079, ker_line_7079)# this gives a response in degrees.


p <- ggplot(data=ker_shp) + geom_sf()  + geom_sf(data=ker_sites_sf) + coord_sf(crs = sf::st_crs(ker_shp))

ggplot() + geom_sf(data=ker_sites_sf) 

# How do I convert distances in degrees to actual km distances ?
# 
ggplot(ker_shp) +geom_sf(ker_sites_sf)



library(sf)
library(lwgeom)
nc = read_sf(system.file("gpkg/nc.gpkg", package="sf")) 
lwgeom::st_geod_distance(nc[1:10,],nc[1:10,])
sf::st_distance(nc[1:10,],nc[1:10,])
# but when I map, there's 1 point that's far away

# EPSG for Kerguelen: 7079 : Crozet = 7076
ker_nats_sf <- ker_nats %>%
  st_as_sf(coords = c("longitude", "latitude"), crs=4326) %>%
  st_transform(crs=7079)
ker_nats_sf
ker_coord <- ker_nats_sf %>% st_coordinates() %>% as.data.frame


p1 = st_point(c(7,52))
p2 = st_point(c(-30,20))
sfc = st_sfc(p1, p2, crs = 4326)
sfc
st_transform(sfc, 3857)
st_transform(st_sf(a=2:1, geom=sfc), "+init=epsg:3857")
try(st_transform(sfc, 3857, aoi = c(-280,-90,180,90)))

# carte
p <- ggplot() + geom_sf(data = ker_shp)
p+ geom_sf(data= ker_nats_sf)



# Thanks to the power of sf, a geom_sf nicely handles varying projections
# setting the aspect ratio correctly.

  library(maps)
  world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
  ggplot() + geom_sf(data = world1)
  
  world2 <- sf::st_transform(
    world1,
    "+proj=laea +y_0=0 +lon_0=155 +lat_0=-90 +ellps=WGS84 +no_defs"
  )
  ggplot() + geom_sf(data = world2)



plotlist <- list()
count = 1
for (i in 2010:2022){
  pp <- p
  pp <-pp + geom_point(data = ker_nats %>% filter(annee == i), 
                       aes(x=longitude, y = latitude, color = annee))
  plotlist[[count]] <- pp
  count=count+1
}

multiplot(plotlist, cols = 2)


nats_sf <- nats %>%
  filter(district =="Crozet") %>%
  st_as_sf(coords =c("longitude", "latitude"), crs=4326) 