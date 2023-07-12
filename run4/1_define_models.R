# run 4: step 1 = load data, design models

# contains additional layers from Manuele's code
# further changes made following François' recommendations (meeting of 4/04/23)

# # define whether analysis over Crozet or Kergeulen :
# crozet = TRUE

set.seed(1)
library(tidyverse)
library(ggplot2)
library(readr)
library(Hmsc)
library(ape) 
library(viridis)
library(magrittr)


# SET DIRECTORIES 
if (crozet){
  localDir = "cro"
  if(!dir.exists(localDir)) dir.create(localDir)
  modelDir = file.path(localDir, "models/run_4")
  if(!dir.exists(modelDir)) dir.create(modelDir)
}else{
  localDir = "ker"
  if(!dir.exists(localDir)) dir.create(localDir)
  modelDir = file.path(localDir, "models/run_4/")
  if(!dir.exists(modelDir)) dir.create(modelDir)
}






# READ AND SELECT SPECIES DATA : define crozet = TRUE, or FALSE for Ker
################################################################################
# data = read.csv(file.path(dataDir, "species.csv"))
source("run4/clean_occurrences.R")

source("run4/extract_local_env_data.R")


if (crozet){
  Y = as.matrix(cro_com_mat)
}else{
  Y = as.matrix(ker_com_mat)
}

hist(Y)

# READ AND MODIFY ENVIRONMENTAL DATA (BEGINNING)
XData = env_vars |> 
  st_drop_geometry() |>
  dplyr::select(accum_prec, mean_temp, numero_observation, jour, mois, 
                annee,date_observation, pente,exposition, NDVI, waterways_dist, 
                sea_dist, insolation, accum_prec, accum_prec_Manu, 
                dry_month_prec, wet_month_prec, mean_temp, 
                mean_temp_Manu, min_temp, min_temp_Manu, max_temp, 
                max_temp_Manu )

# remove Nas from XData
XData <- na.omit(XData)
keep_nums <- XData$numero_observation

str(XData)
XData$numero_observation %<>% as.factor
XData$jour %<>% as.integer
XData$mois %<>% as.integer

XData$annee %<>% as.integer
XData$pente %<>% as.factor
XData$exposition %<>% as.factor
XData$date_observation <- NULL

XData$id <- 1:nrow(XData) |> as.factor()



# to check correlation of precipitation and exposition # what is AA in expo ??
ggplot(data=XData, aes(x=accum_prec, group=exposition, fill=exposition)) +
  geom_density(adjust=1.5, alpha=.4) +
  scale_fill_viridis(discrete=T) +
  scale_color_viridis(discrete=T)



# # explore the extra variables and check for correlations
# XData |>
#   dplyr::select(exposition, NDVI, waterways_dist, sea_dist, pente, insolation, accum_prec_Manu, accum_prec) |>
#   summary()
# 
# 
# GGally::ggpairs(XData %>% dplyr::select(exposition, NDVI, waterways_dist, sea_dist, pente, insolation, accum_prec_Manu, accum_prec))
# 
# 
# pairs(XData %>% dplyr::select(accum_prec_Manu, accum_prec, dry_month_prec, wet_month_prec))
# 
# pairs(XData %>% dplyr::select(min_temp, min_temp_Manu, max_temp, max_temp_Manu, mean_temp, mean_temp_Manu))

# READ AND MODIFY PHYLO DATA 
################################################################################

# script to build tree is in ASICS/ASICS_code/phylo_pipeline.R

phylotree <- read.tree("../data/traits_trees/phylomaker_tree")
plot(phylotree)
phylotree$tip.label <- gsub("_", " ", phylotree$tip.label)

# need to remove Limosella australis for Crozet
if (crozet){
  phylo_cro <- drop.tip(phylotree, tip = "Limosella australis")
  plot(phylo_cro)
}else{
  phylo_ker <- drop.tip(phylotree, tip = "Notogrammitis crassior")
  plot(phylo_ker)
}


##################################################################################################
# READ AND MODIFY TRAIT DATA (sp x traits matrix)
################################################################################

source("process_traits.R")

# keep only those species in the community mat, the phylo_mat and the env_vars 
Y = Y[rownames(Y) %in% keep_nums,plant_sp]
# check prevalence : keep only common species
prev = colSums(Y)
sum(prev>=20) # all good
# also remove sites where no data from Y and XData:
empty <- which(rowSums(Y) == 0 )

Y <- Y[-empty,]

XData <- XData[-which(XData$numero_observation %in% names(empty)), ] 
XData <- droplevels(XData)

keep_nums <- XData$numero_observation
cro_sites_xy <- cro_sites_xy[which(cro_sites_xy$numero_observation %in% keep_nums),]


# trim phylo tree again:
if (crozet){
  phylo_cro <- phylo_cro |> keep.tip(colnames(Y))
  plot(phylo_cro)
}else{
  phylo_ker <- phylo_ker |> keep.tip(colnames(Y))
  plot(phylo_ker)
}

# specify  trait data
TrData = t2 



##################################################################################################
# SET UP THE MODEL (BEGINNING)
##################################################################################################
# GLOBAL FORMULAS COMMON TO ALL MODELS:
# studydesign
studyDesign = data.frame(site=XData$numero_observation)
# RANDOM EFFECT STRUCTURE, HERE Site (hierarchical study design)
rL.site = HmscRandomLevel(units = levels(studyDesign$site))
# and optionally id, if we are interested in species associations at that level
rL.id = HmscRandomLevel(units = levels(studyDesign$id))
# REGRESSION MODEL FOR ENVIRONMENTAL COVARIATES.
XFormula = ~ mean_temp_Manu + accum_prec_Manu + NDVI + waterways_dist + sea_dist + insolation
# REGRESSION MODEL FOR TRAITS
TrFormula = ~ height_m + SLA
# CONSTRUCT TAXONOMICAL TREE TO BE USED AS PROXY FOR PHYLOGENETIC TREE


# CONSTRUCT THE MODELS.
if (crozet){
  
  # simple model : no species associations
  studyDesign = data.frame(site=XData$numero_observation)
  m_simple = Hmsc(Y=Y, XData = XData,  XFormula = XFormula,
                  TrData = TrData, TrFormula = TrFormula,
                  phyloTree = phylo_cro,
                  distr="probit",
                  studyDesign = studyDesign, 
                  ranLevels=list(site=rL.site))
  
  
  # site and species associations as random levels, but no sData
  studyDesign = data.frame(site=XData$numero_observation, id=XData$id)
  rL.site = HmscRandomLevel(units = levels(studyDesign$site))
  rL.id = HmscRandomLevel(units = levels(studyDesign$id))
  
  m_site_id = Hmsc(Y=Y, XData = XData,  XFormula = XFormula,
                   TrData = TrData, TrFormula = TrFormula,
                   phyloTree = phylo_cro,
                   distr="probit",
                   studyDesign = studyDesign, ranLevels=list(site=rL.site, id=rL.id))
  
  
  # small spatial model: species associations and sData
  studyDesign = data.frame(id=XData$id, space = as.factor(1:nrow(XData)))
  rL.spatial = HmscRandomLevel(sData = cro_sites_xy[, 2:3])
  rL.spatial = setPriors(rL.spatial,nfMin=1,nfMax=1)
  m_spatial_small = Hmsc(Y=Y, XData = XData,  XFormula = XFormula,
                         TrData = TrData, TrFormula = TrFormula,
                         phyloTree = phylo_cro,
                         distr="probit",
                         studyDesign = studyDesign, 
                         ranLevels=list("space" = rL.spatial))
  
  
}else{ # Kerguelen
  # PRESENCE-ABSENCE MODEL FOR INDIVIDUAL SPECIES (COMMON ONLY)
  m = Hmsc(Y=Y, XData = XData,  XFormula = XFormula,
           TrData = TrData, TrFormula = TrFormula,
           phyloTree = phylo_ker,
           distr="probit",
           studyDesign = studyDesign, ranLevels=list(site=rL.site, id=rL.id, route = sData))
}




# COMBINING AND SAVING MODELS 
################################################################################
models = list(m_simple, m_site_id, m_spatial_small)
names(models) = c("m_simple", "m_site_id", "m_spatial_small")
save(models, file = file.path(modelDir, "unfitted_models.RData"))
