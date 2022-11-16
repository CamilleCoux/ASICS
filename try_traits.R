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
source("functions.R")

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

not <- natives[-which(natives %in% try_sp$AccSpeciesName)] 
not <- not[-3]

Colobanthus <- try_sp[grep("Colobanthus", try_sp$AccSpeciesName),]
Notogrammitis <- try_sp[grep("Notogrammitis", try_sp$AccSpeciesName),] # 0, just like for Austroblechnum
# so there's not much to do, except exclude these species from the analysis 

# get the AccNum:
essai$AccSpeciesID
essai$AccSpeciesName

# read in the trait data downloaded from TRY webversion : 

tr <- read.csv("../data/David_plantes_KerCro/22563.txt", sep="\t")


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