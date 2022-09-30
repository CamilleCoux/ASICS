library(V.PhyloMaker)
library(ape)
library(magrittr)
library(tidyverse)
library(brranching)

try_sp <- read.csv("../data/TryAccSpecies.txt", sep="\t")
head(try_sp)

essai <- try_sp %>%
  filter(AccSpeciesName %in% natives)


taxa <- essai$AccSpeciesName

phylo_names <- phylomatic_names(taxa, format='isubmit', db="ncbi")

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




