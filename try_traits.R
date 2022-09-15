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
library(rtry)
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
  unname %>% 
  droplevels

try_sp <- read.csv("../data/TryAccSpecies.txt", sep="\t")
head(try_sp)

essai <- try_sp %>%
  filter(AccSpeciesName %in% natives)

not <- natives[-which(natives %in% try_sp$AccSpeciesName)] %>% droplevels()
not <- not[-3]

Colobanthus <- try_sp[grep("Colobanthus", try_sp$AccSpeciesName),]
Notogrammitis <- try_sp[grep("Notogrammitis", try_sp$AccSpeciesName),] # 0, just like for Austroblechnum
# so there's not much to do, except exclude these species from the analysis 

# get the AccNum:
essai$AccSpeciesID




