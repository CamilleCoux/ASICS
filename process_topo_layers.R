library(raster)
library(ncdf4)
library(magrittr)
library(tidyverse)
library(terra)


# read in the .nc files, convert to raster, plot, and export .tif :
local <- "../data/DEM/ker"
read.granules <- function(local, plot, write, name =NULL){
  img <- list.files(local, ".nc", full.names=TRUE)
  ic <- sprc(lapply(img, rast))
  r <- mosaic(ic)
  if (plot) plot(r)
  if (write) writeRaster(x=r, filename = paste(local, "/", name, ".tif", sep=""), overwrite=T)
  
}

read.granules(local, F, T, name = "Ker_DEM")

read.granules("../data/DEM/cro", T, T, "Cro_DEM")




library(biogeo)

data(dem)
plot(dem)
str(dem)

# 
# 
# local <- here::here("../data/NDVI/ndvi3g_geo_v1_1_2002_0712.nc4")
# local <- here::here("../data/NDVI/ndvi3g_geo_v1_1_1982_0106.tif")
# ic <- rast(local)
# plot(ic)
# 
# writeRaster(ic, here::here("../data/NDVI/ndvi3g_geo_v1_1_1982_0106.tif"))
# essai <- raster(here::here("../data/NDVI/ndvi3g_geo_v1_1_1982_0106.tif"))
# 
# library(stars)
# box1 <- c(68, 71,-50, -48)
# box1 <- c(58, 81,-60, -58)
# box1 <- c(-180, 180, 60, 80)
# essai2 <- st_crop(essai, box1)
# 
# 
# l7 <- read_stars(local, package="stars")
# d <- st_dimensions(l7)
# bb<- st_bbox(c(xmin=68, ymin=71,xmax=-50, ymax=-48),
#              crs=st_crs(l7))
# plot(l7[bb])                  
