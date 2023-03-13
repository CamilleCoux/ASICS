
# PROCESS TRAIT DATA 
# not enough traits for these polar species, so need to choose the best
# combo of trait coverage per species.




################################################################################

traits_try2 <- read.table("../data/traits_trees/25266.txt", sep="\t", header=T, na.strings = "NA", dec=".", fill=T, quote="")

# remove all lines which don't have a measured value
traits_try2 <- traits_try2[which(!is.na(traits_try2$StdValue)),]
# remove all lines that don't correspond to a trait measure
traits_try2 <- traits_try2[which(!is.na(traits_try2$TraitID)),]

# evaluate completedness : 
foo <- traits_try2 %>% 
  dplyr::select(TraitName, AccSpeciesName) %>%
  table %>% 
  as.data.frame %>%
  pivot_wider(names_from = TraitName, 
              values_from = Freq) %>%
  as.data.frame
rownames(foo) <- foo$AccSpeciesName
foo <- foo[, -1]
# How many trait values per plant ?
foo %>% colSums()
# how many plant scores per trait ?
foo %>% rowSums()


# ok. Make traits x species df, based on the 3 best coverage traits that aren't correlated

t2 <- traits_try2 %>% 
  dplyr::select(TraitName, TraitID, AccSpeciesName, OrigValueStr, StdValue) %>%
  dplyr::filter(TraitID %in% c(3117, 3106))

t2 %<>%
  dplyr::select(AccSpeciesName, TraitID, StdValue) %>%
  group_by( AccSpeciesName, TraitID) %>%
  dplyr::reframe(TraitID = unique(TraitID),
                 Mean = mean(StdValue)) %>%
  pivot_wider(names_from = TraitID, values_from = Mean) %>%
  as.data.frame
colnames(t2) <- c("taxon", "height_m", "SLA")

t2 <- t2[!is.na(rowSums(t2[,c(2, 3)])), ]

plant_sp <- t2$taxon
rownames(t2) <- t2$taxon


# # add sociabilite: nope. Not enough variation. 
# soc <- cro_nats %>%
#   group_by(taxon) %>%
#   dplyr::reframe(taxon = unique(taxon), 
#                  soc=mean(sociabilite))


rm( foo, traits_try2)
