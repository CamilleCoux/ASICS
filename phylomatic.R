library(taxize)
library(brranching)
library(ape)
library(magrittr)
library(tidyverse)
pacman::p_load(tidyverse, Hmsc,  writexl, taxize, brranching, ape, factoextra, NbClust, compiler)


try_sp <- read.csv("../data/TryAccSpecies.txt", sep="\t")
head(try_sp)

essai <- try_sp %>%
  filter(AccSpeciesName %in% natives)


taxa <- essai$AccSpeciesName

phylo_names <- phylomatic_names(taxa, format='isubmit', db="ncbi")

foo <-phylo_names[1] 
for (i in 2:length(phylo_names)){
  foo <- paste(foo, phylo_names[i], sep=" ")
}

foo <- paste(phylo_names, sep=" ")

tree <- phylomatic(taxa=taxa, get = 'POST')
plot(tree, no.margin=TRUE)

ENTREZ_KEY = '9f6babe2683fa1419807f2c155fa9895e608'


rosa.tree <- read.tree('C:/Users/coux/Desktop/newick_plylo.txt')
plot(rosa.tree)
rosa.tree$tip.label <- capitalize(rosa.tree$tip.label)
plot(rosa.tree)


# Genus names
taxa <- c("Poa annua", "Phlox diffusa", "Helianthus annuus")
phylo_names <- phylomatic_names(taxa, format='isubmit', db="ncbi")
tree <- phylomatic(taxa=taxa, storedtree='R20120829', get='POST')
plot(tree, no.margin=TRUE)

phylo_tree <- brranching::phylomatic(phylo_names, db="apg", storedtree="zanne2014", outformat="newick")
####### 
# trial from tree Luis sent me :

Quian_tree <-  read.tree(file=paste0("../Code/Qian_Jin_2016_tree.txt"))
Quian_tree$tip.label %>% head

# need to change the synthax of taxa to compare them with the Quian tree labels
taxa <- gsub(" ", "_", taxa)
(taxa2 %in% Quian_tree$tip.label) 

# Replacing with sister species : 
grep("Galium", Quian_tree$tip.label, value = T)

taxa2 <- gsub("Callitriche_antarctica", "Callitriche_deflexa", taxa )


### make the example file
c1 <- c("Carya floridana", "Carya pallida", "Epiprinus siletianus", "Platycarya strobilacea", "Tilia amurensis", "Apodanthes caseariae", "Pilostyles blanchetii")
c2 <- c("Carya", "Carya", "Epiprinus", "Platycarya", "Tilia", "Apodanthes", "Pilostyles")
c3 <- c("Juglandaceae", "Juglandaceae", "Euphorbiaceae", "Juglandaceae", "Malvaceae", "Apodanthaceae", "Apodanthaceae")
example <- data.frame(species = c1, genus = c2, family = c3)

### run the function
result <- phylo.maker(example, scenarios=c("S1","S2","S3"))

### plot the phylogenies with node ages displayed.
library(ape)
par(mfrow = c(1, 3))
plot.phylo(result$scenario.1, cex = 1.5, main = "scenario.1")
nodelabels(round(branching.times(result$scenario.1), 1), cex = 1)
plot.phylo(result$scenario.2[[1]], cex = 1.5, main = "scenario.2")
nodelabels(round(branching.times(result$scenario.2[[1]]), 1), cex = 1)
plot.phylo(result$scenario.3, cex = 1.5, main = "scenario.3")
nodelabels(round(branching.times(result$scenario.3), 1), cex = 1)




# trial with my species :
?str_split

sp_tab <- str_split_fixed(phylo_names, "/", 3) %>% data.frame 
colnames(sp_tab) = c("family", "genus", "species")
sp_tab <- sp_tab[, c(3, 2, 1)]

result2 <- phylo.maker(sp_tab, scenarios=c("S1","S2","S3"))

library(ape)
par(mfrow = c(1, 1))
plot.phylo(result2$scenario.1, cex = 1.5, main = "scenario.3")
nodelabels(round(branching.times(result2$scenario.3), 1), cex = 1)




