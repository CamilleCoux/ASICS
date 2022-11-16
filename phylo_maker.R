library(V.PhyloMaker)
library(ape)
library(magrittr)
library(tidyverse)
library(brranching)
library(magrittr)
library(tidyverse)
library(rtry)
library(rentrez)


d <- read.csv("../data/David_plantes_KerCro/202209_TAAF_HFI_plants_data_complete_UTF8.csv",  sep=";", row.names = NULL,
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
  dplyr::count(district, taxon) %>%
  dplyr::count(taxon) %>%
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

taxa <- essai$AccSpeciesName

phylo_names <- phylomatic_names(taxa, format='isubmit', db="ncbi") 
2 # choose this option

# doesn't work:
# phylo_tree <- brranching::phylomatic(phylo_names, db="apg", storedtree="zanne2014", outformat="newick")

####### 


# trial with phylo.maker on my species :

sp_tab <- str_split_fixed(phylo_names, "/", 3) %>% data.frame 
colnames(sp_tab) = c("family", "genus", "species")
sp_tab <- sp_tab[, c(3, 2, 1)]

result2 <- phylo.maker(sp_tab, scenarios=c("S1","S2","S3"))

library(ape)
plot.phylo(result2$scenario.1, cex = 0.9, main = " ")
nodelabels(round(branching.times(result2$scenario.3), 1), cex = 0.7)

plot.phylo(result2$scenario.2, cex = 0.9, main = " ")
nodelabels(round(branching.times(result2$scenario.3), 1), cex = 0.7)


