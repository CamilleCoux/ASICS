library(raster)
library(tidyverse)
library(stars)
library(magrittr)


getwd()

gemted <- raster("C:/Users/Camille/Documents/ASICS/data/DEM/cro/GMTED2010_mean_7_5_arcsec_cro.tif")
plot(gemted)

