# Processing of the occurrence data.
# inputs : raw occurrence data
# - clean and calculate approx surfaces, to be able to sort the sites out and
# make the data more homogenous.
# - subset data to the native species that are common to CRO and KER
# - Outputs : 1. com_mat = the community matrix for Cro (so far), 
#             2. site_xy = df of the sites (numero_observation) with their 
#                corresponding XY coordinates.



library(magrittr)
library(tidyverse)
library(sf)
library(ggplot2)
library(stringr)


theme_set(theme_bw())

d <- readr::read_delim("../data/David_plantes_KerCro/202209_TAAF_HFI_plants_data_complete_UTF8.csv",  
                       delim=";", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# add index:
d %<>% 
  add_column(id = 1:nrow(d), .after=0)
# head(d)
# separate date component in different columns:
d[c("jour", "mois", "annee")] <- str_split_fixed(d$date_observation, "/", 3)


natives <- d %>% 
  filter(statut == "Native") %>%
  dplyr::count(district, taxon) %>%
  dplyr::count(taxon) %>%
  dplyr::filter(n==2) %>%
  dplyr::select(taxon) %>%
  unique %>%
  unlist %>%
  unname 

### dealing with the surface column :

# d$surface %>% unique %>% head(50)
d$surface %<>% as.character
d$surface_orig <- d$surface
# deal with decimals
d$surface <-  gsub(",", "\\.", d$surface)
# remove blank spaces
d$surface <-  gsub(" ", "", d$surface)
# replace with NA when necessary
d$surface[d$surface == ""|d$surface == "#N/A"] <- NA
# remove all non-numeric characters
d$surface <- gsub("[A-z]+.|[A-z]", "", d$surface)
# remove punctuation
d$surface <- gsub("\\?{1-4}|\\!{1-4}", "", d$surface)
# again replace with NA when necessary
d$surface[d$surface == ""] <- NA

# selecting only the numeric entries means half the data is ignored
# c(d$surface[grep("^[0-9]+\\*[0-9]+$|NA", d$surface)], which(is.na(d$surface))) %>% length
# replace x by *
d$surface <-  gsub("100100m", "100*100m", d$surface)
d$surface %>% as.factor %>% unique


d$surf2 <- NA
cells <- grep("^[0-9]+\\*[0-9]+$", d$surface)

for (i in cells){
  d$surf2[i] <- eval(parse(text=d$surface[i] ))
}

d$surf2 <- as.numeric(d$surf2)
# d$surf2 %>% summary

# d$placette %>% unique %>% length # on my mac, nothing comes out here ?
# d$placette <-NULL


# Native plants that are present on both Cro and Ker : 
natives <- d %>% 
  dplyr::filter(statut == "Native") %>%
  dplyr::count(district, taxon) %>%
  dplyr::count(taxon) %>%
  dplyr::filter(n==2) %>%
  dplyr::select(taxon) %>%
  unique %>%
  unlist %>%
  unname

# subset dataset to keep just those species and the env variables I'll need in the model :
nats <- d %>%
  dplyr::filter(taxon %in% natives) %>%
  dplyr::select(id,district, numero_observation,taxon, statut, pente, exposition, 
                sociabilite,  latitude,longitude,Observateur,date_observation, 
                jour,mois,annee,surf2)

# remove the surfaces that are too large to be trusted : 
# nats$surf2 %>% summary
foo <- nats %>%
  dplyr::select(numero_observation, surf2) %>%
  unique 

# foo$surf2 %>% sort(decreasing = T) %>% hist
rm(foo)
  


# what  remains after removal of surfaces larger than...? Cut-off point at 400m2.
# --> because target scale is 20*20m
# --> we remove the 400m, there are a lot less left.
nats2 <- nats %>% filter(surf2 <= 400) # 
nats2 <- rbind(nats2, nats %>% filter(is.na(surf2)))
nats2 <- nats2[order(nats2$id),]
nats2$surf2 <- NULL


# remove NAs for the rows I'll use later in the model. Don't forget to put them
# back in if I end up not using the variables in the end....
nats2 <- na.omit(nats2)




# separate CRO and KER :
cro_nats <- nats2 %>%
  filter(district == "Crozet")
ker_nats <- nats2 %>% filter(district == "Kerguelen")

# build the presence - absence community matrix

cro_com_mat <- cro_nats %>% 
  mutate(count = 1) %>%
  dplyr::select(taxon, numero_observation, count) %>% 
  pivot_wider(names_from = taxon, values_from = count, values_fn = sum,  values_fill = 0) %>%
  as.data.frame

rownames(cro_com_mat) <- cro_com_mat$numero_observation
cro_com_mat$numero_observation <- NULL
# str(cro_com_mat)

# colSums(cro_com_mat) # 
# colSums(cro_com_mat) / nrow(cro_com_mat) *100 # proportion of sites where a plant is observed. 
# # should there be a lower limit for this ? or is it enough if the model has 
# # more than a certain amount of obseervations to deal with ?
# rowSums(cro_com_mat) %>% hist()

# KERGUELEN
ker_com_mat <- ker_nats %>% 
  mutate(count = 1) %>%
  dplyr::select(taxon, numero_observation, count) %>% 
  pivot_wider(names_from = taxon, values_from = count, values_fn = sum,  values_fill = 0) %>%
  as.data.frame

rownames(ker_com_mat) <- ker_com_mat$numero_observation
ker_com_mat$numero_observation <- NULL
# str(ker_com_mat)

# colSums(ker_com_mat) # 
# colSums(ker_com_mat) / nrow(ker_com_mat) *100 # proportion of sites where a plant is observed. 

# # should there be a lower limit for this ? or is it enough if the model has 
# # more than a certain amount of observations to deal with ?
# rowSums(ker_com_mat) %>% hist()



  g <- ggplot(mpg, aes(class)) + geom_bar()
# keep track of the XY locations of each site : 
cro_sites_xy <- cro_nats %>%
  dplyr::select(numero_observation, latitude, longitude) %>%
  unique

ker_sites_xy <- ker_nats %>%
  dplyr::select(numero_observation, latitude, longitude) %>%
  unique



# clean workspace
rm(d, nats, nats2, cells, i)




library(raster)
library(stars)

# add downscaled temperature
# bio1 CHELSA layer = mean annual daily mean air temperatures averaged over 1 year
# bio5 CHELSA layer = highest temperature of any monthly daily mean maximum temperature
# bio6 CHELSA layer = lowest temperature of an ymonthly daily mean maximum temperature

if(crozet){
  
  
  bio1_cro <- raster("../data/chelsa/bio1_downscaled_Cro.tif")*0.1-273.15
  bio5_cro <- raster("../data/chelsa/bio5_downscaled_cro.tif")*0.1-273.15
  bio6_cro <- raster("../data/chelsa/bio6_downscaled_cro.tif")*0.1-273.15
  box1 <- c(51.6, 51.9, -46.5, -46.3)
  bio1_cro <- raster::crop(bio1_cro, box1)
  bio5_cro <- raster::crop(bio5_cro, box1)
  bio6_cro <- raster::crop(bio6_cro, box1)
  
  # create environmental variable table : sites x variables
  env_vars <- cro_nats %>%
    dplyr::select(numero_observation, latitude, longitude, jour, mois, annee, date_observation, pente, exposition) %>%
    unique %>%
    st_as_sf(coords = c("longitude", "latitude" ), crs=st_crs(bio1_cro))
  
  
  env_vars <- st_extract(st_as_stars(bio1_cro), env_vars) %>% 
    rename(mean_temp = layer) %>% 
    st_join(env_vars) %>%
    unique
  rownames(env_vars) <- 1:nrow(env_vars)
  
  env_vars <- st_extract(st_as_stars(bio5_cro), env_vars) %>% 
    rename(max_temp = layer) %>% 
    st_join(env_vars) %>%
    unique
  rownames(env_vars) <- 1:nrow(env_vars)
  
  env_vars <- st_extract(st_as_stars(bio6_cro), env_vars) %>% 
    rename(min_temp = layer) %>% 
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










