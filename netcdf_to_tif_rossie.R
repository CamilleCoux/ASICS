
# rossie script to open and convert NetCDF files into geotiff files.

library(ncdf4)
ncpath <- "/media/sancho/ccoux/data/NDVI/" ## changi this to rossie paths

ncfilenames <- list.files(ncpath)[grep("\\.nc4", list.files(ncpath))]

for (i in ncfilenames){
  ncname <- paste0(ncpath, i)
  ncin <- ncdf4::nc_open(ncname)
  
  terra::writeRaster(ncin, paste0(ncpath, i, ".tif"))
}


library(raster)
ncin <- raster("C:/Users/Camille/Documents/ASICS/data/NDVI/ndvi3g_geo_v1_1_1982_0106.nc4")
