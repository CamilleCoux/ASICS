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
library(rentrez)


theme_set(theme_bw())

source("functions.R")


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
  dplyr::filter(statut == "Native") %>%
  dplyr::count(district, taxon) %>%
  dplyr::count(taxon) %>%
  dplyr::filter(n==2) %>%
  dplyr::select(taxon) %>%
  unique %>%
  unlist %>%
  unname %>% 
  droplevels

try_sp <- read.csv("../data/traits_trees/TryAccSpecies.txt", sep="\t")
head(try_sp)

essai <- try_sp %>%
  filter(AccSpeciesName %in% natives)

essai$AccSpeciesID
essai$AccSpeciesName

# get the species AccNumbers to retrieve from TRY:
nums <- essai$AccSpeciesID %>% paste(., collapse = ",")

# then : copypaste the species numbers in TRY > data portal > data explorer >
# traits table for multiple species and download
# remove the 3 1st lines from the text file
# get traitIDs :
traitIDs <-  read.csv("../data/traits_trees/get_trait_IDs.txt", sep="\t")[-20]
colnames(traitIDs) <- gsub("^\\.|\\.$", "", colnames(traitIDs))



# --------------- work in progress : figure out which traits an which species to keep -------
# need in binary
for (i in 1:nrow(traitIDs)){
  for (j in 3:ncol(traitIDs)){
    if(traitIDs[i, j] !=0) {traitIDs[i,j] <- 1}
  }
}

traitIDs$rowsums <- traitIDs[, 3:19] %>% rowSums()
sub <- traitIDs[order(traitIDs$rowsums, decreasing = T), ]
sub <- sub %>% dplyr::filter(rowsums >6)

# get the trait measures from the 
tr_measures <- read.csv("../data/traits_trees/25266.txt", sep="\t")

tr_measures$TraitID %>% unique %>% sort






# so now I'll file a request for those 20 traits on TRY
sub$TraitID %>% paste(., collapse = ",")
tr_measures <- read.csv2("../data/traits_trees/try_20_traits_all_sp.txt", sep="\t")
tr_measures %>% head


foo <- data.frame(index = 3:19, species = colnames(sub)[3:19], nb = colSums(sub[3:19]))
foo <- foo[order(foo$nb, decreasing = T),]

sub[, c(1, 2, foo$index[1:15], 20)]

sub[, foo$index[1:15]] %>% as.matrix %>% image.plot



############################################################################
mat <- sub[, c(2, foo$index[1:15])]
mat <- column_to_rownames(mat, "TraitID")
############################################################################


bloo <- sub[, c(1, 2, foo$index[1:15])] %>% pivot_longer(!c(Trait, TraitID), names_to="species", values_to = "nb")
ggplot(bloo, aes( x=Trait, y=species, fill=nb) ) +
  geom_tile()

bloo2 <- sub[, c(1,foo$index[1:15])] %>%
  gather(key="species", value="z", -1) %>%
  ggplot(aes(Trait, species, fill=z))+
  geom_tile()
# --------------- work in progress : end -------




# ok. Imagine I've sorted out which traits and species I wanna keep. Now I need
# to get those trait values for each species....
ordered <- read.csv2("../data/traits_trees/trait_sp_binmat_optim.csv", sep=",", row.names = 1)
keeps <- ordered[1:8, 1:13]
sp_names <- colnames(keeps) %>% gsub("\\.", " ", .)
tr_id <- rownames(keeps)


# read in the trait data downloaded from TRY webversion : 

tr2 <- read.csv("../data/traits_trees/23996.txt", sep="\t")

trait_vals <- tr2 %>%
  dplyr::select(AccSpeciesName, TraitID, TraitName, OrigValueStr, StdValue) %>%
  dplyr::filter(TraitID %in% tr_id) %>%
  dplyr::filter(AccSpeciesName %in% sp_names)

trait_vals$TraitID %>% unique

foo <- trait_vals %>%
  dplyr::filter(TraitID == 38 ) 
foo$OrigValueStr %>% unique %>% as.numeric %>% range
foo$StdValue %>% unique %>% range

trait_vals$TraitName %>% unique

trait_vals %>%
  split(., .$AccSpeciesName) %>%
  lapply(., function(x){
    split(x, x$TraitID) %>%
      lapply(., function(y){
        if (y$TraitID == 42)
        tab <- y
      })
  })

not <- natives[-which(natives %in% try_sp$AccSpeciesName)] 

Colobanthus <- try_sp[grep("Colobanthus", try_sp$AccSpeciesName),]
Notogrammitis <- try_sp[grep("Notogrammitis", try_sp$AccSpeciesName),] # 0, just like for Austroblechnum
# so there's not much to do, except exclude these species from the analysis for the traits bit.
# for the phylogenies, I'm goint to use sister species, but not sure this would be relevant for the traits bit

# get the AccNum:
# read in the trait data downloaded from TRY webversion : 

tr1 <- read.csv("../data/traits_trees/22563.txt", sep="\t")
tr2 <- read.csv("../data/traits_trees/23996.txt", sep="\t")

tr1$TraitName %>% unique
tr2$TraitName %>% unique
# only 6 species in the Try database that have entries for those traits
(tr1$AccSpeciesName %>% unique) %in% natives
(tr2$AccSpeciesName %>% unique) %in% natives

# new strategy : find out for which traits we have the most entries for our native species list:

try_sp <-  read.csv("../data/traits_trees/TryAccSpecies.txt", sep="\t")
tr_list <- read.csv("../data/traits_trees/try_traits_list.txt", sep="\t")

tr_list <- tr_list[rev(order(tr_list$AccSpecNum)),c("TraitID","Trait", "AccSpecNum")]

tr_sel <- c("Plant growth form", "Leaf photosynthesis pathway", "Plant height vegetative", 
            "Species tolerance to frost", "Seed dry mass"," Plant lifespan (longevity)",
            "Plant nitrogen(N) fixation capacity", "Seed germination rate (germination efficiency)",
            "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or exclu",
            "Leaf nitrogen (N) content per leaf dry mass", "Leaf phosphorus (P) content per leaf dry mass",
            "Leaf dry mass (single leaf)", "Leaf thickness", "Plant growth rate")
tr_list$TraitID[tr_list$Trait %in% tr_sel]

tr_sp_sel <- try_sp %>% dplyr::filter(AccSpeciesName %in% natives) %>%
  dplyr::select(AccSpeciesID) %>% as.vector()

tr_sel <- read.csv("../data/traits_trees/Try_traits_selection.txt", sep="\t")
tr_sel %<>%  dplyr::filter(AccSpeciesName %in% natives)
rownames(tr_sel) <- tr_sel$AccSpeciesName
tr_sel$X <- NULL
tr_sel$AccSpeciesName <- NULL
colnames(tr_sel) <- gsub("^\\.|\\.$", "", colnames(tr_sel))
colnames(tr_sel) <- gsub("Leaf.area.*", "SLA", colnames(tr_sel))

tr_sel[tr_sel>0] <- 1
tr_sel %>% rowSums
tr_sel %>% colSums

tr_sel %>% dplyr::select(Plant.growth.form,  Plant.height.vegetative, SLA  )






# retrieve the genbank sequences for the remL gene, and the MatK
library(rentrez)

path <- "../data/David_plantes_KerCro/"

# We want a character vector of the nucleotide database --> nuccore  
# retmax determines the max number of hits


colker <- "Colobanthus kerguelensis[Organism] AND rbcL[gene]"    
colker_search <- entrez_search(db="nuccore", term=colker, retmax=40) 
colker_seqs <- entrez_fetch(db="nuccore", id=colker_search$ids, rettype="fasta")



calant <- "Callitriche antarctica[Organism] AND rbcL[gene]"
calant_search <- entrez_search(db="nuccore", term=calant, retmax=10)
calant_search$ids



calant_seqs <- entrez_fetch(db="nuccore", id=calant_search$ids, rettype="fasta")
write(c(colker_seqs, calan_seqs), paste(path, "Callitriche antarctica.fasta", sep=""), sep="\n") #gets sequence to a file


# loop over plant names and retrieve all sequences. Priority to rbcL, but 
# otherwise all
library(traits)

# try dirty method where I only keep the longest sequences : 
# for rbcL :
seqs <- essai$AccSpeciesName %>% 
  as.list %>%
  lapply(., ncbi_byname, gene="rbcL") 
  
# I only need to see the sequences and the species names
sp_names <-  lapply(seqs, function(x){return(x$taxon)})
rbcl_sequences <-  lapply(seqs, function(x){return(x$sequence)})

seqinr::write.fasta(rbcl_sequences, file.out =paste(path, "seqinr_rcbl.fasta", sep=""), names = sp_names)

# and now with matK gene :
seqs <- essai$AccSpeciesName %>% 
  as.list %>%
  lapply(., ncbi_byname, gene="matK") 

# I only need to kee the sequences and the species names
sp_names <-  lapply(seqs, function(x){return(x$taxon)})
matk_sequences <-  lapply(seqs, function(x){return(x$sequence)})

seqinr::write.fasta(matk_sequences, file.out =paste(path, "seqinr_matk.fasta", sep=""), names = sp_names)











################ other method, where I authorize multiple sequences per species (intrasp variability tests ? )
rbcl_list <- matK_list <- vector(mode='list', length=nrow(essai))
count=1
for (i in essai$AccSpeciesName){
  # query for the rbcL gene
  colker <- paste(i, "[Organism] AND rbcL[gene]", sep="")    
  colker_search <- entrez_search(db="nuccore", term=colker, retmax=10) 
  if (colker_search$retmax != 0){
    rbcl_list[[count]] <- entrez_fetch(db="nuccore", id=colker_search$ids, rettype="fasta")
  }else{rbcl_list[[count]] <- NA}
  
  
  #query for the matK gene
  colker2 <- paste(i, "[Organism] AND matK[gene]", sep="")    
  colker_search2 <- entrez_search(db="nuccore", term=colker2, retmax=10)
  if (colker_search2$retmax != 0){
    matK_list[[count]] <- entrez_fetch(db="nuccore", id=colker_search2$ids, rettype="fasta")
  }else{matK_list[[count]] <- NA}
  # 
  # if (colker_search$retmax == 0){
  #   colker <- paste(i, "[Organism]", sep="")    
  #   colker_search <- entrez_search(db="nuccore", term=colker, retmax=10)
  # }
  # 
  # if (!is_empty(colker_search[[1]]) & !is_empty(colker_search2[[1]])){
  #   seq_list[[count]]  <- entrez_fetch(db="nuccore", id=colker_search$ids, rettype="fasta")
  #   count = count +1
  #   seq_list[[count]]  <- entrez_fetch(db="nuccore", id=colker_search2$ids, rettype="fasta")
  # }else{
  #   seq_list[[count]] <- entrez_fetch(db="nuccore", id=colker_search$ids, rettype="fasta")
  # }
  count=count+1
}


write(rbcl_list%>% unlist, paste(path, "rbcl.fasta", sep=""), sep="\n") #gets sequence to a file
write(matK_list%>% unlist, paste(path, "matK.fasta", sep=""), sep="\n") #gets sequence to a file





# essai en séparant les gènes rbcL et matK


seq_list <- empty_list <- vector(mode='list', length=nrow(essai))
count=1
for (i in essai$AccSpeciesName){
  # query for the rbcL gene
  colker <- paste(i, "[Organism] AND rbcL[gene]", sep="")    
  colker_search <- entrez_search(db="nuccore", term=colker, retmax=10) 
  
  #query for the matK gene
  colker2 <- paste(i, "[Organism] AND matK[gene]", sep="")    
  colker_search2 <- entrez_search(db="nuccore", term=colker2, retmax=10)
  
  # if (colker_search$retmax == 0){
  #   colker <- paste(i, "[Organism]", sep="")    
  #   colker_search <- entrez_search(db="nuccore", term=colker, retmax=10)
  # }
  
  if (!is_empty(colker_search[[1]]) & !is_empty(colker_search2[[1]])){
    seq_list[[count]]  <- entrez_fetch(db="nuccore", id=colker_search$ids, rettype="fasta")
    count = count +1
  }
}