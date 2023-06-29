# process topoclimatic layers, with Manuele layers for Crozet

# DEM: use mine, but remove islands using the contour shape
dem_stars <- stars::read_stars("../data/DEM.tif")
cro_coastline <- sf::st_read("../data/Manuele/coastline_poly.shp")
dem2 <- sf::st_crop(dem_stars, cro_coastline)

# # Other remaining layers from Manuele
# insol_year <- raster("../data/Manuele/insol_year.tif") # Insolation time
# Sea.dist <- raster("../data/Manuele/sea_dist.tif") # Distance from the shoreline
# Waterways.dist <- raster("../data/Manuele/waterways_dist.tif") # Distance from waterbodies
# NDVI <- raster("../data/Manuele/NDVI_GC.tif") # NDVI

# But I need to get the same ones from Kerguelen  => maybe just run those for Cro
# and see afterwards for Ker?
ker_coast <- sf::st_read("../data/SIG/Contours/KER_contours.shp")
ker_dem <- stars::read_stars("../data/DEM/ker/Ker_DEM.tif")
# ker_dem <- st_crop(ker_dem, ker_coast)


# calculate distance to coast layer, based on :
# https://dominicroye.github.io/en/2019/calculating-the-distance-to-the-sea-in-r/
# need to get the proper local projection to calculate the right distances.
# the local ESPG code is 3336.
ker_coast_loc <- sf::st_transform(ker_coast, 3336)

# make grid
grid <- sf::st_make_grid(ker_coast_loc, cellsize=30, what="centers")

grid <- sf::st_intersection(grid, ker_coast_loc)

ker_sea_dist <- sf::st_distance(ker_coast_loc, grid)

# create a data.frame with the distance and the coordinates of the points
df <- data.frame(ker_sea_dist = as.vector(ker_sea_dist)/1000,
                 st_coordinates(grid))


save.image("manu.RData")


# 
# 
# 
# 
# bio1_cro <- raster("../data/chelsa/bio1_downscaled_Cro.tif")*0.1-273.15
# bio5_cro <- raster("../data/chelsa/bio5_downscaled_cro.tif")*0.1-273.15
# bio6_cro <- raster("../data/chelsa/bio6_downscaled_cro.tif")*0.1-273.15
# box1 <- c(51.6, 51.9, -46.5, -46.3)
# bio1_cro <- raster::crop(bio1_cro, box1)
# bio5_cro <- raster::crop(bio5_cro, box1)
# bio6_cro <- raster::crop(bio6_cro, box1)
# 
# 
# 
# 
# # add Manuele's environmental layers (only for Crozet):
# 
# 
# 
# # downscaled topoclimatic layers
# 
# Bio_01 <- raster("../data/Manuele/bio_01_dwnsc_sa.tif") # Mean temperature
# Bio_05 <- raster("../data/Manuele/bio_05_dwnsc_sa.tif") # Max Temperature of Warmest Month
# Bio_06 <- raster("../data/Manuele/bio_06_dwnsc_sa.tif") # Min Temperature of Coldest Month
# Bio_12 <- raster("../data/Manuele/bio_12_dwnsc_sa.tif") # Annual precipitation
