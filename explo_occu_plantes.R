# explo data TAAF envoyées par Pierre A le 8/8/22
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



d <- read.csv("../data/David_plantes_KerCro/2022_TAAF_HFI_plants_data.csv",  sep=";", row.names = NULL)

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
  


p2 <- ggplot(abunds %>% filter(district == "Kerguelen") , aes(x= taxon, y = n))+
  geom_bar(stat="identity", position = "dodge", fill = "#B8DE29FF") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle("Kerguelen") + ylab ( " # of occurrences")

multiplot(p, p2 ,cols = 1)


# carte
land <- st_read("../data/ne_10m_land/ne_10m_land.shp")
coast <- st_read("../data/ne_10m_coastline/ne_10m_coastline.shp")
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# plus joli : 
coast %>% ggplot() + geom_sf()+
  coord_sf(xlim = c(68.5, 70.7), ylim = c(-48.5, -49.7))


# dégueu :
ggplot(data=world) + geom_sf() +
  coord_sf(xlim = c(68, 71), ylim = c(-48, -50))+
  
  
  
nats %>% 
  filter(district =="Crozet", taxon =="Acaena magellanica") %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  ggplot() + 
  geom_sf()
  
spdf_france <- ne_countries(country = 'france')

if (require(sp)) {
  plot(spdf_world)
  plot(spdf_africa)
  plot(spdf_france)
}



ggplot() +
  geom_sf(data = cdn) + coord_sf(crs = st_crs(4326))


data("cdn")
