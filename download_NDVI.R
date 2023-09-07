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

ncpath <- "C:/Users/Camille/Documents/ASICS/data/NDVI/" ## changi this to rossie paths

ncfilenames <- list.files(ncpath)[grep("\\.nc4", list.files(ncpath))]

for (i in ncfilenames){
  ncname <- paste0(ncpath, i)
  ncin <- ncdf4::nc_open(ncname)
  
  terra::writeRaster(ncin, paste0(ncpath, i, ".tif"))
}




i <- "AVHRR-Land_v005_AVH13C1_NOAA-07_19810624_c20170610041337.nc"



##### end open NetCDF files: ##################################################




# You may need to install the httr package.
# install.packages("httr")
library(httr)
netrc_path <- "/path/to/.netrc"
cookie_path <- "/path/to/.urs_cookies"
downloaded_file_path <- "/path/to/filename"
# Before using the script
#Set up your ~/.netrc file as listed here: https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget
set_config(config(followlocation=1,netrc=1,netrc_file=netrc_path,cookie=cookie_path,cookiefile=cookie_path,cookiejar=cookie_path))
httr::GET(url = "https://disc2.gesdisc.eosdis.nasa.gov/data/TRMM_RT/TRMM_3B42RT_Daily.7/2000/03/3B42RT_Daily.20000301.7.nc4",
          write_disk(downloaded_file_path, overwrite = TRUE))






