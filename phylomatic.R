library(taxize)
library(brranching)
library(ape)


try_sp <- read.csv("../data/TryAccSpecies.txt", sep="\t")
head(try_sp)

essai <- try_sp %>%
  filter(AccSpeciesName %in% natives)


taxa <- essai$AccSpeciesName

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

# Lots of names
taxa <- c("Poa annua", "Collomia grandiflora", "Lilium lankongense", "Phlox diffusa",
          "Iteadaphne caudata", "Gagea sarmentosa", "Helianthus annuus")
tree <- phylomatic(taxa=taxa, get = 'POST')
plot(tree, no.margin=TRUE)

# Don't clean - clean=TRUE is default
(tree <- phylomatic(taxa=taxa, clean = FALSE))
## with clean=FALSE, you can get non-splitting nodes, which you
## need to collpase before plotting
library('ape')
plot(collapse.singles(tree), no.margin=TRUE)

# Output NeXML format
taxa <- c("Gonocarpus leptothecus", "Gonocarpus leptothecus", "Lilium lankongense")
out <- phylomatic(taxa=taxa, get = 'POST', outformat = "nexml")
cat(out)

# Lots of names, note that when you have enough names (number depends on length of individual
# names, so there's no per se rule), you will get an error when using `get='GET'`,
# when that happens use `get='POST'`
library("taxize")
spp <- names_list("species", 500)
# phylomatic(taxa = spp, get = "GET")
(out <- phylomatic(taxa = spp, get = "POST", db = "itis"))
plot(out)