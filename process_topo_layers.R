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

