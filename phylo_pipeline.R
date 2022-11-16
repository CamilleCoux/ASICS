# Calculate phylogenetic tree from a list of species.


# Extract species names, by species, genus and family

# Opt 1: let phylo.maker get the info from GenBank and create tree. Super fast,
# but black box.

# opt 2 : to test that tree, also possible to calculate it by extracting
# sequences from GenBank, align, and calculate distances "manually"



library(tidyverse)
library(V.PhyloMaker)
library(brranching)
library(traits)
library(seqinr)
library(adegenet)
library(ape)
library(ggtree)
library(DECIPHER)
library(viridis)
library(ggplot2)

# species list : 
d <- read.csv("../data/David_plantes_KerCro/202209_TAAF_HFI_plants_data_complete_UTF8.csv",  
              sep=";", row.names = NULL, stringsAsFactors = T)

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


# extract species, genus and family 

phylo_names <- phylomatic_names(natives %>% as.character, 
                                format='isubmit', db="ncbi") 
2 # choose this option


### OPTION 1 : make tree using phylo.maker -------------------------------------

# need to arrange the phylo names into dataframe with columns in right order:

sp_tab <- str_split_fixed(phylo_names, "/", 3) %>% data.frame 
colnames(sp_tab) = c("family", "genus", "species")
sp_tab <- sp_tab[, c(3, 2, 1)]

# selected all scenarios to compare, but turns out they're equivalent
result <- phylo.maker(sp_tab, scenarios=c("S1", "S2","S3"))
s3tree <- result[[3]]
plot(s3tree, cex = 0.6)




### OPTION 2 -------------------------------------------------------------------

# Extract the nucleotide sequences from ncbi database, using species 
# Accessibility Number (AccNum), which I have stored in a trait file from the 
# TRY database: 
try_sp <- read.csv("../data/TryAccSpecies.txt", sep="\t")

essai <- try_sp %>%
  filter(AccSpeciesName %in% natives)

# get the AccNum:
essai$AccSpeciesID
essai$AccSpeciesName

# loop over plant names and retrieve all sequences. Priority to rbcL, but 
# otherwise all

# try dirty method where I only keep the longest sequences : 
# for rbcL :
seqs <- essai$AccSpeciesName %>% 
  as.list %>%
  lapply(., ncbi_byname, gene="rbcL") 

# I only need to see the sequences and the species names
sp_names <-  lapply(seqs, function(x){return(x$taxon)})
rbcl_sequences <-  lapply(seqs, function(x){return(x$sequence)})

seqinr::write.fasta(rbcl_sequences, 
                    file.out =paste("../data/David_plantes_KerCro/seqinr_rcbl.fasta", sep=""), 
                    names = sp_names)

# and now with matK gene :
seqs <- essai$AccSpeciesName %>% 
  as.list %>%
  lapply(., ncbi_byname, gene="matK") 

# I only need to kee the sequences and the species names
sp_names <-  lapply(seqs, function(x){return(x$taxon)})
matk_sequences <-  lapply(seqs, function(x){return(x$sequence)})

seqinr::write.fasta(matk_sequences, 
                    file.out =paste("../data/David_plantes_KerCro/seqinr_matk.fasta", sep=""), 
                    names = sp_names)




# now claculate phylo distances, based on Russel Gray's code :

# load the sequences from the file
# change "DNA" to "RNA" or "AA" if necessary
seqs <- readDNAStringSet("../data/David_plantes_KerCro/seqinr_rcbl.txt", format = "fasta")

# look at some of the sequences (optional)
seqs

# nucleotide sequences need to be in the same orientation
# if they are not, then they can be reoriented (optional)
seqs <- OrientNucleotides(seqs)

# perform the alignment
aligned <- AlignSeqs(seqs)

# view the alignment in a browser (optional)
BrowseSeqs(aligned, highlight=0)

# write the alignment to a new FASTA file
writeXStringSet(aligned,
                file="../data/David_plantes_KerCro/seqinr_rbcl_aligned.fasta")

# read in the aligned data
dna <- read.alignment("../data/David_plantes_KerCro/seqinr_rbcl_aligned.fasta", format = "fasta")

# create a distance matrix for the alignment 
D <- dist.alignment(dna, matrix = "similarity")


temp <- as.data.frame(as.matrix(D))
temp <-  mutate_all(temp, ~replace_na(.,1))


table.paint(temp, cleg=0, clabel.row=.5, clabel.col=.5)+
  scale_color_viridis()#darker shades of gray mean a larger distance # you can also make cool color plots but they're much more complicated because they use the image() function

# we can start to see a pattern because the data is ordered by year, 
# but we can't really make any conclusions yet

tre <- nj(D)
class(tre) #all trees created using {ape} package will be of class phylo

tre <- ladderize(tre)

# ~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~~Base R plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot(tre, cex = 0.6)
title("similarity in Amanita (ITS)")


# or 
h_cluster <- hclust(D, method = "average", members = NULL) # method = average is used for UPGMA, members can be equal to NULL or a vector with a length of size D
plot(h_cluster, cex = 0.6)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ Tree Plotting in ggtree ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# you can fan it out 
ggtree(tre, yscale = "NA")+
  geom_tiplab(hjust = -0.3, size=4, align = TRUE)+
  xlim(0,0.5) 

# or whatever this thing does???
ggtree(tre,layout = "daylight")+
  geom_tiplab(hjust = -0.3, size=4, align = TRUE)+
  xlim(0,0.5) 

# plot a basic tree
ggtree(tre) + 
  geom_tiplab(hjust = -0.3, size=4, align = TRUE)+
  xlim(0,0.5)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~ Customize your trees ~~~~~~~~~~~~~~~~~~~~~~~~
# plot using ggtree and highlight clusters
# change the node values for your own data
ggtree(tre) + 
  geom_tiplab(hjust = -0.3, size=4, align = TRUE) + 
  geom_hilight(node=19, fill="purple", alpha = 0.2) + 
  geom_hilight(node=17, fill="dark green", alpha = 0.2) +
  geom_hilight(node=20, fill="gold", alpha = 0.2) +
  xlim(0,0.5) 

# highlight clusters and add a vertical line to group clusters
# change the node values for your own data
ggtree(tre) + 
  geom_tiplab(hjust = -0.3, size=4, align = TRUE) + 
  geom_hilight(node=19, fill="purple", alpha = 0.2) + 
  geom_hilight(node=17, fill="dark green", alpha = 0.2) +
  geom_hilight(node=20, fill="gold", alpha = 0.2) +
  geom_cladelabel(node=19, label=" Cluster 1", 
                  color="purple", offset=.1, barsize = 2,
                  fontsize = 5, align=TRUE, alpha = 0.5) + 
  geom_cladelabel(node=17, label=" Cluster 2", 
                  color="dark green", offset=.1, barsize = 2,
                  fontsize = 5, align=TRUE, alpha = 0.5) + 
  geom_cladelabel(node=20, label=" Cluster 3", 
                  color="gold", offset=.1, barsize = 2,
                  fontsize = 5, align=TRUE, alpha = 0.5) + 
  xlim(0,0.5) 



# ~~~~~~~~~~~~~~~~~~~~~~~~~~ Plot the allignment with the tree ~~~~~~~~~~~~~~~~~

# lets plot the alignment with the tree, to do this we first have to
# match the names to the tip labels
# set our tree into a new name
tre.new <- tre
# change tip labels to full alignment names
tre.new$tip.label <- aligned@ranges@NAMES

# plot the alignment 
msaplot(p=ggtree(tre.new), fasta="../data/David_plantes_KerCro/seqinr_rbcl_aligned.fasta", 
        window=c(150, 175))+
  scale_fill_viridis_d(alpha = 0.8)




