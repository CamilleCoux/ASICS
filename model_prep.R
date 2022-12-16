# Set the base directory
setwd("../formations/HMSC/case study exercises/")
set.seed(1)


library(Hmsc)
library(ape) 


# SET DIRECTORIES 
localDir = ""
dataDir = file.path("../formations/HMSC/case study exercises/data")
modelDir = file.path(localDir, "models")
if(!dir.exists(modelDir)) dir.create(modelDir)


# READ AND SELECT SPECIES DATA 
################################################################################
data = read.csv(file.path(dataDir, "species.csv"))
source("process_occurrences.R")

Y = as.matrix(cro_com_mat)
hist(Y)

# READ AND MODIFY ENVIRONMENTAL DATA (BEGINNING)
XData = env_vars %>% 
  st_drop_geometry %>%
  dplyr::select(accum_prec, mean_temp, numero_observation, 
                jour,mois,annee,date_observation,pente,exposition)

# remove Nas from XData
XData <- na.omit(XData)


str(XData)
XData$numero_observation %<>% as.factor
XData$jour %<>% as.integer
XData$mois %<>% as.integer

XData$annee %<>% as.integer
XData$pente %<>% as.factor
XData$exposition %<>% as.factor
XData$date_observation <- NULL

XData$id <- 1:nrow(XData)



# PairPlot by altitude
pairs(XData[, c(1:6, 13)])


# With transparency (right)
ggplot(data=XData, aes(x=accum_prec, group=exposition, fill=exposition)) +
  geom_density(adjust=1.5, alpha=.4) +
  scale_fill_viridis(discrete=T) +
  scale_color_viridis(discrete=T) 
#p2


# READ AND MODIFY PHYLO DATA 
################################################################################

# script to build tree is in ASICS/ASICS_code/phylo_pipeline.R

phylotree <- read.tree("../data/traits_trees/phylomaker_tree")
plot(phylotree)


# READ AND MODIFY TRAIT DATA (none so far..)
################################################################################



##################################################################################################
# SELECT COMMON SPECIES (BEGINNING)
##################################################################################################
prev = colSums(Y)
hist(prev)
sum(prev>=10)
sum(prev>=20)
sel.sp = (prev>=20)
Y = Y[,sel.sp] #presence-absence data for selected species
# TrData = TrData[sel.sp,]
# TrData = droplevels(TrData)




##################################################################################################
# SET UP THE MODEL (BEGINNING)
##################################################################################################
# STUDY DESIGN
studyDesign = data.frame(site=XData$numero_observation, id=XData$id)
# RANDOM EFFECT STRUCTURE, HERE Site (hierarchical study design)
rL.site = HmscRandomLevel(units = levels(studyDesign$site))
# and optionally id, if we are interested in species associations at that level
rL.id = HmscRandomLevel(units = levels(studyDesign$id))
# REGRESSION MODEL FOR ENVIRONMENTAL COVARIATES.
XFormula = ~ mean_temp + accum_prec + pente + exposition
# REGRESSION MODEL FOR TRAITS
# TrFormula = ~ fb+orn+shape+volume
# CONSTRUCT TAXONOMICAL TREE TO BE USED AS PROXY FOR PHYLOGENETIC TREE


# CONSTRUCT THE MODELS.

# PRESENCE-ABSENCE MODEL FOR INDIVIDUAL SPECIES (COMMON ONLY)
m = Hmsc(Y=Y, XData = XData,  XFormula = XFormula,
         # TrData = TrData, TrFormula = TrFormula,
         phyloTree = phylotree,
         distr="probit",
         studyDesign = studyDesign, ranLevels=list(site=rL.site, id=rL.id))

##################################################################################################
# SET UP THE MODEL (END)
##################################################################################################


##################################################################################################
# COMBINING AND SAVING MODELS (START)
##################################################################################################
models = list(m)
names(models) = c("presence-absence model")
save(models, file = file.path(modelDir, "unfitted_models.RData"))
##################################################################################################
# COMBINING AND SAVING MODELS (END)
##################################################################################################


##################################################################################################
# TESTING THAT MODELS FIT WITHOUT ERRORS (START)
##################################################################################################
for(i in 1:length(models)){
  print(i)
  sampleMcmc(models[[i]],samples=2)
}
##################################################################################################
# TESTING THAT MODELS FIT WITHOUT ERRORS (END)
##################################################################################################
