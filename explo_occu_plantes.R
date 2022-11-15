# explo data occurrences TAAF envoyées par Pierre A le 8/8/22

library(magrittr)
library(tidyverse)
library(sf)
library(ggplot2)
library(stringr)
library(rnaturalearth)
library(rnaturalearthdata)
library(osmdata)
library(ggmap)

theme_set(theme_bw())

source("functions.R")


#d <- read.csv("../data/David_plantes_KerCro/2022_TAAF_HFI_plants_data.csv",  sep=";", row.names = NULL,
#              stringsAsFactors = T)
#d <- readr::read_delim("../data/David_plantes_KerCro/202209_TAAF_HFI_plants_data_complete.csv",  delim=";", 
                       #locale = locale(encoding = "ISO-8859-1"))
#d <- readr::write_delim(d, "../data/David_plantes_KerCro/202209_TAAF_HFI_plants_data_complete_UTF8.csv",  delim=";")

d <- readr::read_delim("../data/David_plantes_KerCro/202209_TAAF_HFI_plants_data_complete_UTF8.csv",  delim=";", locale = locale(encoding = "UTF-8"))

# add index:
d %<>% 
  add_column(id = 1:nrow(d), .after=0)
head(d)
# separate date component in different columns:
d[c("jour", "mois", "annee")] <- str_split_fixed(d$date_observation, "/", 3)


### dealing with the surface column :

d$surface %>% unique %>% head(50)
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
c(d$surface[grep("^[0-9]+\\*[0-9]+$|NA", d$surface)], which(is.na(d$surface))) %>% length
# replace x by *
d$surface <-  gsub("100100m", "100*100m", d$surface)
d$surface %>% as.factor %>% unique


d$surf2 <- NA
cells <- grep("^[0-9]+\\*[0-9]+$", d$surface)

for (i in cells){
  d$surf2[i] <- eval(parse(text=d$surface[i] ))
}

d$surf2 <- as.numeric(d$surf2)
d$surf2 %>% summary

d$placette %>% unique()
d$placette <-NULL




# Native plants that are present on both Cro and Ker : 
natives <- d %>% 
  filter(statut == "Native") %>%
  count(district, taxon) %>%
  count(taxon) %>%
  filter(n==2) %>%
  select(taxon) %>%
  unique %>%
  unlist %>%
  unname

# subset dataset to keep just those species :
nats <- d %>%
  filter(taxon %in% natives)

# look at abundance distributions:

nats %>%
  count(taxon)
abunds <- nats %>% 
  count(district, taxon) %>%
  group_by(district) %>%
  mutate(taxon = fct_rev(fct_reorder(taxon, n)))

p <- ggplot(abunds %>% filter(district == "Crozet") , aes(x= taxon, y = n))+
  geom_bar(stat="identity", position = "dodge", fill = "#39568CFF") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle("Crozet") + ylab( " # of occurrences")
p  


p2 <- ggplot(abunds %>% filter(district == "Kerguelen") , aes(x= taxon, y = n))+
  geom_bar(stat="identity", position = "dodge", fill = "#B8DE29FF") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle("Kerguelen") + ylab ( " # of occurrences")

multiplot(p, p2 ,cols = 1)



# look at the sampling dates

nats %>%
  count(taxon)
abunds <- nats %>% 
  count(district, taxon) %>%
  group_by(district) %>%
  mutate(taxon = fct_rev(fct_reorder(taxon, n)))

nats %>%
  filter(district =="Crozet", annee >=2010) %>%
  group_by(taxon, annee) %>% 
  count %>%
  ggplot(aes(x=as.numeric(annee), y=n, color=taxon)) +
  geom_line()+
  ggtitle("Crozet")

nats %>%
  filter(district =="Kerguelen") %>%
  group_by(taxon, annee) %>% 
  count %>%
  ggplot(aes(x=as.numeric(annee), y=n, color=taxon)) +
  geom_line()+
  ggtitle("Kerguelen")




# plot occurrences just to have an idea : 


foo <- nats %>% 
  filter(district =="Crozet", annee >= 2010) 



# carte
cro <- st_read("../data/SIG/Contours/CRO_contours.shp")
p <- ggplot() + geom_sf(data = cro)

plotlist <- list()
count = 1
for (i in 2010:2022){
  pp <- p
  pp <-pp + geom_point(data = foo %>% filter(annee == i), 
                       aes(x=longitude, y = latitude, color = annee))
  plotlist[[count]] <- pp
  count=count+1
}

multiplot(plotlist, cols = 2)


nats_sf <- nats %>%
  filter(district =="Crozet") %>%
  st_as_sf(coords =c("longitude", "latitude"), crs=4326) 


atlas_sf <- nats %>%
  filter(district =="Crozet", protocole =="Atlas") %>%
  st_as_sf(coords =c("longitude", "latitude"), crs=4326) 

# st_write(nats_sf, "../data/SIG/mes couches/plantes_cro.shp", append=F)
# st_write(atlas_sf, "../data/SIG/mes couches/atlas_cro.shp", append=F)


mailles_atlas <- st_read("../data/David_plantes_KerCro/Mailles ATLAS/2020_CRO_Atlas_mailles.shp")
ggplot()+
  geom_sf(data=cro)+
  geom_sf(data = nats_sf, aes(color=taxon) ) +
  #geom_sf(data = atlas_sf, aes(color="green") ) +
  geom_sf(data = mailles_atlas, alpha = .15, color="darkblue")+ 
  theme(legend.position = "none")


ggplot()+
  geom_sf(data=cro)+
  geom_sf(data = atlas_sf, aes(color="green") ) +
  geom_sf(data = mailles_atlas, alpha = .15, color="darkblue")
# doesn't look like there are 41k points there

nats$latitude %>% unique %>% length()
nats$longitude %>% unique %>% length()

nats$longlat <- paste(nats$longitude, nats$latitude, sep=",") 
nats$longlat %>% unique %>% length

nats$numero_observation %>% unique %>% length

nats %>%
  count(numero_observation) %>% 
  select(n) %>% unlist %>%
  hist

# Luis' idea : round th XY coords to the 4th or 5 decimal and consider they
# are in the same plot.

nats2 <- nats %>% 
  select(numero_observation, longitude, latitude) %>%
  summarise(numero_observation = numero_observation,
            long2 = round(longitude, digits = 4),
            lat2 = round(latitude, digits = 4),
            site = paste(long2, lat2, sep = "/") %>% as.factor %>% as.numeric) %>%
  left_join(nats, ., keep=F)

nats2$site %>% unique %>% length
nats2$numero_observation %>% unique %>% length

nats2 %>%
  count(site) %>% 
  select(n) %>% unlist %>% mean
  hist
  
  nats %>%
    count(numero_observation) %>% 
    select(n) %>% unlist %>%
    hist

nats2 %>%
  filter(district =="Crozet") %>%
  ggplot()+
  geom_point(aes(x=longitude, y=latitude, color="red")) + 
  geom_point(aes(x=long2, y=lat2)) 

nats %>% filter(protocole == "Inventaire") %>%
  group_by(district) %>%
  select(surface) %>% unique %>% print(., n=150)





#___ trying to order all obs by site, such that these sites have a similar surface

nats$surf2%>% summary

# so this means that if I remove all the obs that have NA surf2, only half of
# the obs remain.
# In theory, the numero_observation shoulld match the sites; let's see if there
# are some NA surf2 cases I can complete. Need to check if each num_obs has only 1
# surface associated to it.

nats %>%
  select(numero_observation, surf2) %>%
  unique %>%
  dim # 5098    2
  


foo <- nats %>%
  select(numero_observation, surf2) %>%
  unique %>%
  group_by(numero_observation) %>%
  count 

nats %>%
  filter(is.na(surf2)) %>%
  select(numero_observation) %>% 
  unique %>%
  dim # 2834   1



nats %>%
  filter(is.na(surf2)) %>%
  select(numero_observation, surf2) %>%
  group_by(numero_observation) %>%
  unique()

### KERGUELEN

# plot occurrences just to have an idea : 


foo <- nats %>% 
  filter(district =="Kerguelen", annee >= 2010) 



# carte
ker <- st_read("../data/SIG/Contours/KER_contours.shp")
p <- ggplot() + geom_sf(data = ker)

plotlist <- list()
count = 1
for (i in 2010:2022){
  pp <- p
  pp <-pp + geom_point(data = foo %>% filter(annee == i), 
                       aes(x=longitude, y = latitude, color = annee))
  plotlist[[count]] <- pp
  count=count+1
}

plotlist
multiplot(plotlist, cols = 2)




nats_sf <- nats %>%
  filter(district =="Kerguelen") %>%
  st_as_sf(coords =c("longitude", "latitude"), crs=4326) 

# st_write(nats_sf, "../data/SIG/mes couches/plantes_ker.shp")

ggplot()+
  geom_sf(data=ker)+
  geom_sf(data = nats_sf, aes(color=taxon) )



# doesn't look like there are 41k points there

nats$latitude %>% unique %>% length()

nats$longitude %>% unique %>% length()

nats$longlat <- paste(nats$longitude, nats$latitude, sep=",") 
nats$longlat %>% unique %>% length

nats$numero_observation %>% unique %>% length

nats %>%
  count(numero_observation) %>% 
  select(n) %>% unlist %>% 
  hist


nats %>%
  group_by(protocole) %>%
  count

