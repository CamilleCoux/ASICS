support.level.beta = NULL #Default: 0.95
support.level.gamma = NULL #Default: 0.95
support.level.omega = NULL #Default: 0.9
var.part.order.explained = NULL #Default: in variance partitioning of explained variance, species are shown in the order they are in the model
var.part.order.raw = NULL #Default: in variance partitioning of raw variance, species are shown in the order they are in the model
show.sp.names.beta = NULL #Default: species names shown in beta plot if there are at most 30 species and no phylogeny
plotTree = NULL #Default: tree is plotted in Beta plot if the model includes it
omega.order = NULL #Default: species shown in the order they are in the model
show.sp.names.omega = NULL #Default: species names shown in beta plot if there are at most 30 species


if(is.null(support.level.beta)) support.level.beta = 0.95
if(is.null(support.level.gamma)) support.level.gamma =  0.95
if(is.null(support.level.omega)) support.level.omega =  0.9

# Crozet
m <- models_cro[[1]]
postBeta = getPostEstimate(m, parName="Beta")

plotBeta(m, post=postBeta, supportLevel = support.level.beta, param="Sign",
         plotTree = TRUE,
         covNamesNumbers = c(TRUE,FALSE),
         spNamesNumbers=c(c.show.sp.names,FALSE),
         cex=c(0.6,0.6,0.8))
mpost = convertToCodaObject(m)
rhovals = unlist(poolMcmcChains(mpost$Rho))
mymain = paste0("BetaPlot Crozet, E[rho] = ",round(mean(rhovals),2),", Pr[rho>0] = ",round(mean(rhovals>0),2))



# Make map of Crozet based on beta predicted values

crozet = TRUE
cro <- sf::st_read("../data/SIG/Contours/CRO_contours.shp")

source("../ASICS_code/process_occurrences.R")
pred_cro <- predict(m)
map_cro <- cbind(env_vars[models_cro[[1]]$XData$id, ], pred_cro)




Epred_cro <- Reduce("+",pred_cro)/length(pred_cro)
dim(Epred_cro)
head(Epred_cro, 10)

# merge with coordinates from the sites that we kept in the analysis:

Epred_cro <- cbind(Epred_cro, cro_sites_xy[rownames(Epred_cro), c("latitude", "longitude")])

Epred_cro %<>% st_as_sf(coords = c("longitude", "latitude" ), crs = 4326)

Epred_cro %>%
  ggplot() +
  geom_sf( aes(color = `Acaena magellanica`)) 


# merge with coords:
cro_com_mat <- cbind(cro_com_mat, cro_sites_xy[rownames(cro_com_mat) %in% cro_sites_xy$numero_observation, c("latitude", "longitude")]) %>%
  st_as_sf(coords = c("longitude", "latitude" ), crs = 4326)

# extract plants of interest:
A_magellanica <- cro_com_mat %>%
  dplyr::filter(`Acaena magellanica` >0) %>%
  dplyr::select(`Acaena magellanica`, geometry)

# plot presences:
A_magellanica %>%
  ggplot() +
  geom_sf(data=cro)
  geom_sf( ) 


# kerguelen

crozet = FALSE
source("../ASICS_code/process_occurrences.R")
ker <- st_read("../data/SIG/Contours/KER_contours.shp")
m <- models_ker[[1]]
# get predicted values : 
pred_ker <- predict(m)
# this is a list of length = 1000 ( 4 chains of 250 samples) 
dim(pred_ker[[1]])
head(pred_ker[[1]])

# To simplify, we take the means --> posterior mean predictions

Epred_ker <- Reduce("+",pred_ker)/length(pred_ker)
dim(Epred_ker)
head(Epred_ker, 10)

# merge with coordinates from the sites that we kept in the analysis:

Epred_ker <- cbind(Epred_ker, ker_sites_xy[rownames(Epred_ker), c("latitude", "longitude")]) %>%
  as.data.frame
#Epred_ker <- Epred_ker[-which(is.na(Epred_ker$longitude)), ]
Epred_ker %<>% st_as_sf(coords = c("longitude", "latitude" ), crs = 4326)
Epred_ker %>%
  ggplot() +
  geom_sf( aes(color = `Acaena magellanica`)) 

# compare with the presence only:
# merge with coords:
ker_com_mat <- cbind(ker_com_mat, ker_sites_xy[rownames(ker_com_mat) %in% ker_sites_xy$numero_observation, c("latitude", "longitude")]) %>%
  st_as_sf(coords = c("longitude", "latitude" ), crs = 4326)

# extract plants of interest:
A_magellanica <- ker_com_mat %>%
  dplyr::filter(`Acaena magellanica` >0) %>%
  dplyr::select(`Acaena magellanica`, geometry)

# plot presences:
A_magellanica %>%
  ggplot() +
  geom_sf(data=ker) +
  geom_sf( )


# mat var covar :

varcovar <- computeAssociations(m)
varcovar[[1]]$mean


