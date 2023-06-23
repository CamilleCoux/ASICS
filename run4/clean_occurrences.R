
theme_set(theme_bw())

d <- readr::read_delim("../data/David_plantes_KerCro/202209_TAAF_HFI_plants_data_complete_UTF8.csv",  
                       delim=";", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# add index:
d %<>% 
  tibble::add_column(id = 1:nrow(d), .after=0)
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


# keep track of the XY locations of each site : 
cro_sites_xy <- cro_nats %>%
  dplyr::select(numero_observation, latitude, longitude) %>%
  unique

ker_sites_xy <- ker_nats %>%
  dplyr::select(numero_observation, latitude, longitude) %>%
  unique



# clean workspace
rm(d, nats, nats2, cells, i)


