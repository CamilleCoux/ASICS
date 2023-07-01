
## add the environmental measures for eauch occurrence of cro and ker:

library(raster)
library(stars)

# add downscaled temperature
# bio1 CHELSA layer = mean annual daily mean air temperatures averaged over 1 year
# bio5 CHELSA layer = highest temperature of any monthly daily mean maximum temperature
# bio6 CHELSA layer = lowest temperature of an monthly daily mean maximum temperature

if(crozet){
  
  
  bio1_cro <- raster("../data/chelsa/bio1_downscaled_Cro.tif")*0.1-273.15
  bio5_cro <- raster("../data/chelsa/bio5_downscaled_cro.tif")*0.1-273.15
  bio6_cro <- raster("../data/chelsa/bio6_downscaled_cro.tif")*0.1-273.15
  box1 <- c(51.6, 51.9, -46.5, -46.3)
  bio1_cro <- raster::crop(bio1_cro, box1)
  bio5_cro <- raster::crop(bio5_cro, box1)
  bio6_cro <- raster::crop(bio6_cro, box1)
  
  
  
  # add Manuele's environmental layers (only for Crozet):
  
  insol_year <- raster("../data/Manuele/insol_year.tif") # Insolation time
  Sea.dist <- raster("../data/Manuele/sea_dist.tif") # Distance from the shoreline
  Waterways.dist <- raster("../data/Manuele/waterways_dist.tif") # Distance from waterbodies
  NDVI <- raster("../data/Manuele/NDVI_GC.tif") # NDVI
  
  # downscaled topoclimatic layers

  Bio_01 <- raster("../data/Manuele/bio_01_dwnsc_sa.tif") # Mean temperature
  Bio_05 <- raster("../data/Manuele/bio_05_dwnsc_sa.tif") # Max Temperature of Warmest Month
  Bio_06 <- raster("../data/Manuele/bio_06_dwnsc_sa.tif") # Min Temperature of Coldest Month
  Bio_12 <- raster("../data/Manuele/bio_12_dwnsc_sa.tif") # Annual precipitation
  
  # create environmental variable table : sites x variables
  env_vars <- cro_nats %>%
    dplyr::select(numero_observation, latitude, longitude, jour, mois, annee, date_observation, pente, exposition) %>%
    unique %>%
    dplyr::filter(!numero_observation %in% dup) %>%
    st_as_sf(coords = c("longitude", "latitude" ), crs=st_crs(bio1_cro))
  
  # extraction of my downscaled temperatures, followed by Manuele's
  # mean temp
  env_vars <- st_extract(st_as_stars(bio1_cro), env_vars) %>% 
    rename(mean_temp = layer) %>% 
    st_join(env_vars) %>%
    unique
  rownames(env_vars) <- 1:nrow(env_vars)
  
  env_vars <- st_extract(st_as_stars(Bio_01), env_vars) %>% 
    rename(mean_temp_Manu = bio_01_dwnsc_sa ) %>% 
    st_join(env_vars) %>%
    unique
  rownames(env_vars) <- 1:nrow(env_vars)
  
  # max_temp
  env_vars <- st_extract(st_as_stars(bio5_cro), env_vars) %>% 
    rename(max_temp = layer) %>% 
    st_join(env_vars) %>%
    unique
  rownames(env_vars) <- 1:nrow(env_vars)
  
  env_vars <- st_extract(st_as_stars(Bio_05), env_vars) %>% 
    rename(max_temp_Manu = bio_05_dwnsc_sa ) %>% 
    st_join(env_vars) %>%
    unique
  rownames(env_vars) <- 1:nrow(env_vars)
  
  # min_temp
  env_vars <- st_extract(st_as_stars(bio6_cro), env_vars) %>% 
    rename(min_temp = layer) %>% 
    st_join(env_vars) %>%
    unique
  rownames(env_vars) <- 1:nrow(env_vars)
  
  env_vars <- st_extract(st_as_stars(Bio_06), env_vars) %>% 
    rename(min_temp_Manu = bio_06_dwnsc_sa) %>% 
    st_join(env_vars) %>%
    unique
  rownames(env_vars) <- 1:nrow(env_vars)
  
  
  # same with precipitation (scale 0.1, )
  # bio12 CHELSA layer = Accumulated precipitation amount over 1 year
  # bio13 CHELSA layer = The precipitation of the wettest month.
  # bio14 CHELSA layer = The precipitation of the driest month
  
  prec12_cro <- raster("../data/chelsa/CHELSA_bio12_1981-2010_V.2.1.tif")
  prec13_cro <- raster("../data/chelsa/CHELSA_bio13_1981-2010_V.2.1.tif")
  prec14_cro <- raster("../data/chelsa/CHELSA_bio14_1981-2010_V.2.1.tif")
  
  prec12_cro <- raster::crop(prec12_cro, box1)*0.1
  prec13_cro <- raster::crop(prec13_cro, box1)*0.1
  prec14_cro <- raster::crop(prec14_cro, box1)*0.1
  
  
  env_vars <- st_extract(st_as_stars(prec12_cro), env_vars) %>% 
    dplyr::rename(accum_prec = CHELSA_bio12_1981.2010_V.2.1) %>% 
    st_join(env_vars) %>%
    unique
  rownames(env_vars) <- 1:nrow(env_vars)
  
  env_vars <- st_extract(st_as_stars(Bio_12), env_vars) %>% 
    dplyr::rename(accum_prec_Manu = bio_12_dwnsc_sa ) %>% 
    st_join(env_vars) %>%
    unique
  rownames(env_vars) <- 1:nrow(env_vars)
  
  env_vars <- st_extract(st_as_stars(prec13_cro), env_vars) %>% 
    dplyr::rename(wet_month_prec = CHELSA_bio13_1981.2010_V.2.1) %>% 
    st_join(env_vars) %>%
    unique
  rownames(env_vars) <- 1:nrow(env_vars)
  
  env_vars <- st_extract(st_as_stars(prec14_cro), env_vars) %>% 
    dplyr::rename(dry_month_prec = CHELSA_bio14_1981.2010_V.2.1) %>% 
    st_join(env_vars) %>%
    unique
  rownames(env_vars) <- 1:nrow(env_vars)
  
  # and now extract from other layers of Manuele:
  # insolation
  env_vars <- st_extract(st_as_stars(insol_year), env_vars) %>% 
    dplyr::rename(insolation = insol_year) %>% 
    st_join(env_vars) %>%
    unique
  rownames(env_vars) <- 1:nrow(env_vars)
  
  # distance to sea
  env_vars <- st_extract(st_as_stars(Sea.dist), env_vars) %>% 
    st_join(env_vars) %>%
    unique
  rownames(env_vars) <- 1:nrow(env_vars)
  
  # Distance to water bodies Waterways.dist
  env_vars <- st_extract(st_as_stars(Waterways.dist), env_vars) %>% 
    st_join(env_vars) %>%
    unique
  rownames(env_vars) <- 1:nrow(env_vars)
  
  # NDVI
  env_vars <- st_extract(st_as_stars(NDVI), env_vars) %>% 
    dplyr::rename(NDVI = NDVI_GC ) %>% 
    st_join(env_vars) %>%
    unique
  rownames(env_vars) <- 1:nrow(env_vars)
  
  # change order of columns because OCD:
  env_vars <- env_vars[, c(
    "numero_observation", "jour", "mois", "annee","date_observation", "pente", 
    "exposition", "NDVI", "waterways_dist", "sea_dist", "insolation", 
    "accum_prec" , "accum_prec_Manu", "dry_month_prec", "wet_month_prec",
    "mean_temp", "mean_temp_Manu",  "min_temp", "min_temp_Manu",  "max_temp", 
    "max_temp_Manu", "geometry")]
  
  
  
  # write.csv2(env_vars, "../data/sdm_inputs/env_data_cro.csv")
  
  
}




if(!crozet){
  
  
  bio1_ker <- raster("../data/chelsa/bio1_downscaled_ker.tif")*0.1-273.15
  bio5_ker <- raster("../data/chelsa/bio5_downscaled_ker.tif")*0.1-273.15
  bio6_ker <- raster("../data/chelsa/bio6_downscaled_ker.tif")*0.1-273.15
  box1 <- c(68, 71,-50, -48)
  bio1_ker <- raster::crop(bio1_ker, box1)
  bio5_ker <- raster::crop(bio5_ker, box1)
  bio6_ker <- raster::crop(bio6_ker, box1)
  
  # create environmental variable table : sites x variables
  env_vars <- ker_nats %>%
    dplyr::select(numero_observation, latitude, longitude, jour, mois, annee, date_observation, pente, exposition) %>%
    unique %>%
    st_as_sf(coords = c("longitude", "latitude" ), crs=st_crs(bio1_ker))
  
  
  env_vars <- st_extract(st_as_stars(bio1_ker), env_vars) %>% 
    rename(mean_temp = layer) %>% 
    st_join(env_vars) %>%
    unique
  rownames(env_vars) <- 1:nrow(env_vars)
  
  env_vars <- st_extract(st_as_stars(bio5_ker), env_vars) %>% 
    rename(max_temp = layer) %>% 
    st_join(env_vars) %>%
    unique
  rownames(env_vars) <- 1:nrow(env_vars)
  
  env_vars <- st_extract(st_as_stars(bio6_ker), env_vars) %>% 
    rename(min_temp = layer) %>% 
    st_join(env_vars) %>%
    unique
  rownames(env_vars) <- 1:nrow(env_vars)
  
  
  
  # same with precipitation (scale 0.1, )
  # bio12 CHELSA layer = Accumulated precipitation amount over 1 year
  # bio13 CHELSA layer = The precipitation of the wettest month.
  # bio14 CHELSA layer = The precipitation of the driest month
  
  prec12_ker <- raster("../data/chelsa/CHELSA_bio12_1981-2010_V.2.1.tif")
  prec13_ker <- raster("../data/chelsa/CHELSA_bio13_1981-2010_V.2.1.tif")
  prec14_ker <- raster("../data/chelsa/CHELSA_bio14_1981-2010_V.2.1.tif")
  
  prec12_ker <- raster::crop(prec12_ker, box1)*0.1
  prec13_ker <- raster::crop(prec13_ker, box1)*0.1
  prec14_ker <- raster::crop(prec14_ker, box1)*0.1
  
  
  env_vars <- st_extract(st_as_stars(prec12_ker), env_vars) %>% 
    dplyr::rename(accum_prec = CHELSA_bio12_1981.2010_V.2.1) %>% 
    st_join(env_vars) %>%
    unique
  rownames(env_vars) <- 1:nrow(env_vars)
  
  env_vars <- st_extract(st_as_stars(prec13_ker), env_vars) %>% 
    dplyr::rename(wet_month_prec = CHELSA_bio13_1981.2010_V.2.1) %>% 
    st_join(env_vars) %>%
    unique
  rownames(env_vars) <- 1:nrow(env_vars)
  
  env_vars <- st_extract(st_as_stars(prec14_ker), env_vars) %>% 
    dplyr::rename(dry_month_prec = CHELSA_bio14_1981.2010_V.2.1) %>% 
    st_join(env_vars) %>%
    unique
  rownames(env_vars) <- 1:nrow(env_vars)
  
  
  # write.csv2(env_vars, "../data/sdm_inputs/env_data_ker.csv")
  
}









