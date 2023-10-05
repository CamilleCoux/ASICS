install.packages("ncdf4")
library(ncdf4)

# Open connection:

# nc = nc_open("../data/NDVI/AVHRR-Land_v005_AVH13C1_NOAA-07_19810624_c20170610041337.nc")

##### download NetCDF files: ##################################################

ncfiles <- c("
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1982_0106.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1982_0712.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1983_0106.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1983_0712.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1984_0106.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1984_0712.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1985_0106.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1985_0712.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1986_0106.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1986_0712.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1987_0106.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1987_0712.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1988_0106.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1988_0712.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1989_0106.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1989_0712.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1990_0106.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1990_0712.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1991_0106.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1991_0712.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1992_0106.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1992_0712.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1993_0106.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1993_0712.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_1994_0106.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_2007_0106.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_2007_0712.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_2008_0106.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_2008_0712.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_2009_0106.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_2009_0712.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_2010_0106.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/data/ndvi3g_geo_v1_1_2010_0712.nc4
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/comp/Global_Veg_Greenness_GIMMS_3G.pdf
    https://daac.ornl.gov/orders/2edb7c94c1d6c626b2a51bc042bc97aa/Global_Veg_Greenness_GIMMS_3G/guide/Global_Veg_Greenness_GIMMS_3G.html
               ")


ncfiles2 <- stringr::str_split(ncfiles, pattern = "\\n", simplify = TRUE) 
ncfiles2 <- ncfiles2[grep(".nc4", ncfiles2)]
ncfiles2 <- gsub("\\s.", "", ncfiles2)

ncfiles2 <- ncfiles2[-c(1:5)]

for (url in ncfiles2) {
  download.file(url, destfile = paste("../data/NDVI/", basename(url), sep = "") )
}

##### end download NetCDF files: ##################################################



##### open NetCDF files: ##################################################

ncpath <- here::here("../data/NDVI/") ## changi this to rossie paths

ncfilenames <- list.files(ncpath)[grep("\\.nc4", list.files(ncpath))]

for (i in ncfilenames){
  ncname <- paste0(ncpath %>% gsub("\\/ASICS_code\\/\\.\\.", "", .), "/", i)
  ncin <- ncdf4::nc_open(ncname)
  
  terra::writeRaster(ncin, paste0(ncpath, i, ".tif"))
}




# https://www.r-bloggers.com/2016/08/a-netcdf-4-in-r-cheatsheet/


# Get a list of the NetCDF's R attributes:
attributes(ncin)$names

##  [1] "filename"    "writable"    "id"          "safemode"    "format"     
##  [6] "is_GMT"      "groups"      "fqgn2Rindex" "ndims"       "natts"      
## [11] "dim"         "unlimdimid"  "nvars"       "var"

print(paste("The file has",ncin$nvars,"variables,",ncin$ndims,"dimensions and",ncin$natts,"NetCDF attributes"))


attributes(ncin$var)$names
ncatt_get(ncin, attributes(ncin$var)$names[4])

ndvi <- ncvar_get(ncin, attributes(ncin$var)$names[4], collapse_degen = FALSE)

dim(ndvi)

# ok so these are biweelky measures of ndvi, starting from 1/1/82, and until 
# the next layer. So need to average over 12 layers of array... + the next 12 
# ones, to get a yearmy mean.

# Retrieve the latitude and longitude values.
attributes(ncin$dim)$names

nc_lat <- ncvar_get( ncin, attributes(ncin$dim)$names[4])
nc_lon <- ncvar_get( ncin, attributes(ncin$dim)$names[3])

print(paste(dim(nc_lat), "latitudes and", dim(nc_lon), "longitudes"))


essai <- apply(ndvi, 1:2, mean)


# https://rdrr.io/cran/daymetr/man/nc2tif.html




nc_close(ncin)









##### end open NetCDF files: ##################################################
##### 
##### 
##### 

install.packages("ncmeta")
essai <- terra::rast(here::here("../data/NDVI/ndvi3g_geo_v1_1_1982_0106.nc4"))
# ok this is pretty neat. Now I want to average to yealy means
essai2 <- terra::rast(here::here("../data/NDVI/ndvi3g_geo_v1_1_1982_0712.nc4"))

essai2$percentile_1 %>% writeRaster("../data/NDVI/essai2_perc1.tif")
 






# chatGPT version

nc_stars <- stars::read_stars(paste0(ncpath, "ndvi3g_geo_v1_1_1982_0106.nc4"), 
                              sf_column = "short ndvi") 


ndvi <- ncvar_get(ncin, "ndvi")
lon <- ncvar_get(ncin, "lon")
lat <- ncvar_get(ncin, "lat")
time <- ncvar_get(ncin, "time")

# Create a stars object manually
st_ndvi <- st(as.data.frame(lon, lat, time), ndvi, crs = st_crs("+proj=longlat +datum=WGS84"))

# Set axis names
st_dimensions(st_ndvi)[["z"]] <- c("time")
st_dimensions(st_ndvi)[["x"]] <- c("lon")
st_dimensions(st_ndvi)[["y"]] <- c("lat")

# Create a stars object manually
st_dimensions <- st_dimensions(c("lon", "lat", "time"))
st_ndvi <- st_sfc(as.array(ndvi, dim = st_dimensions), crs = st_crs("+proj=longlat +datum=WGS84"))


# Create dimensions
dim <- st_dimensions(
  c("lon", "lat"),  # Spatial dimensions (longitude and latitude)
  list(c(51.6, 51.9), c(-46.5, -46.3))  # Dimension extents
)
attributes <- st_crs("+proj=longlat +datum=WGS84")

st_ncin <- stars::st_as_stars(
  list(ndvi = ndvi, lon=lon, lat=lat, time=time),
  dimensions = 
)

st_ndvi <- stars::st_as_stars(ndvi, dimensions = list(x = "lon", y = "lat", z = "time"), 
                              axes = list(x = "Longitude", y = "Latitude", z = "Time"))
