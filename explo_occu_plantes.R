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
source("functions.R")

theme_set(theme_bw())

source("functions.R")


d <- read.csv("../data/David_plantes_KerCro/2022_TAAF_HFI_plants_data.csv",  sep=";", row.names = NULL,
              stringsAsFactors = T)
# d_env <- read.csv("../data/David_plantes_KerCro/2022_TAAF_HFI_plants_data_complete.csv",  sep=";", row.names = NULL,
#                   stringsAsFactors = T)
  
# add index:
d %<>% 
  add_column(id = 1:nrow(d), .after=0)
head(d)
# separate date component in different columns:
d[c("jour", "mois", "annee")] <- str_split_fixed(d$date_observation, "/", 3)





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

# st_write(nats_sf, "../data/SIG/mes couches/plantes_cro.shp")

ggplot()+
  geom_sf(data=cro)+
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


