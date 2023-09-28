# Set the base directory
# setwd("../formations/HMSC/case study exercises/")

# # define whether analysis over Crozet or Kergeulen :
# crozet = TRUE

set.seed(1)


library(Hmsc)
library(ape) 
library(viridis)


# SET DIRECTORIES 
if (crozet){
  localDir = "cro"
  if(!dir.exists(localDir)) dir.create(localDir)
  modelDir = file.path(localDir, "models/with_traits/")
  if(!dir.exists(modelDir)) dir.create(modelDir)
}else{
  localDir = "ker"
  if(!dir.exists(localDir)) dir.create(localDir)
  modelDir = file.path(localDir, "models/with_traits")
  if(!dir.exists(modelDir)) dir.create(modelDir)
}






# READ AND SELECT SPECIES DATA : define crozet = TRUE, or FALSE for Ker
################################################################################
# data = read.csv(file.path(dataDir, "species.csv"))
source("process_occurrences.R")




if (crozet){
  Y = as.matrix(cro_com_mat)
}else{
  Y = as.matrix(ker_com_mat)
}

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

XData$id <- 1:nrow(XData) %>% as.factor



# to check correlation of precipitation and exposition # what is AA in expo ??
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


dim(t2)

# keep only those species in the community mat, the phylo_mat and the env_vars 
Y = Y[,plant_sp]
# check prevalence : keep only common species
prev = colSums(Y)
sum(prev>=20) # all good
# also remove sites where no data from Y and XData:
empty <- which(rowSums(Y) == 0 )

Y <- Y[-empty,]

XData <- XData[-which(XData$numero_observation %in% names(empty)), ] 
XData <- droplevels(XData)


# trim phylo tree again:

phylo_cro <- phylo_cro %>% keep.tip(colnames(Y))
phylo_ker <- phylo_cro %>% keep.tip(colnames(Y))

# specify  trait data
TrData = t2 



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
TrFormula = ~ height_m + SLA
# CONSTRUCT TAXONOMICAL TREE TO BE USED AS PROXY FOR PHYLOGENETIC TREE


# CONSTRUCT THE MODELS.
if (crozet){
  # PRESENCE-ABSENCE MODEL FOR INDIVIDUAL SPECIES (COMMON ONLY)
  m = Hmsc(Y=Y, XData = XData,  XFormula = XFormula,
           #TrData = TrData, TrFormula = TrFormula,
           #phyloTree = phylo_cro,
           distr="probit",
           studyDesign = studyDesign, ranLevels=list(site=rL.site, id=rL.id))
}else{
  # PRESENCE-ABSENCE MODEL FOR INDIVIDUAL SPECIES (COMMON ONLY)
  m = Hmsc(Y=Y, XData = XData,  XFormula = XFormula,
           # TrData = TrData, TrFormula = TrFormula,
           phyloTree = phylo_ker,
           distr="probit",
           studyDesign = studyDesign, ranLevels=list(site=rL.site, id=rL.id))
}




# COMBINING AND SAVING MODELS 
################################################################################
models = list(m)
names(models) = c("presence-absence model")
save(models, file = file.path(modelDir, "unfitted_models.RData"))



# TESTING THAT MODELS FIT WITHOUT ERRORS 
##################################################################################################
for(i in 1:length(models)){
  print(i)
  sampleMcmc(models[[i]],samples=2)
}



# SETTING COMMONLY ADJUSTED PARAMETERS TO NULL WHICH CORRESPONDS TO DEFAULT CHOICE (BEGINNING)
##################################################################################################
nParallel = NULL #Default: nParallel = nChains
# samples=250, thin=1 




# load(file=file.path(modelDir,"unfitted_models2.RData"))
nm = length(models)
samples_list = c(5,250,250,250,250)#,250)
thin_list = c(1,1,10,100,1000)#,10000)
nChains = 4
nst = length(thin_list)

if(is.null(nParallel)) nParallel = nChains
Lst = 1
while(Lst <= length(samples_list)){
  thin = thin_list[Lst]
  samples = samples_list[Lst]
  print(paste0("thin = ",as.character(thin),"; samples = ",as.character(samples)))
  filename = file.path(modelDir,paste("S2_fit_models_thin_", as.character(thin),
                                      "_samples_", as.character(samples),
                                      "_chains_",as.character(nChains),
                                      ".Rdata",sep = ""))
  if(file.exists(filename)){
    print("model had been fitted already")
  } else {
    print(date())
    # for (mi in 1:nm) {
   models <- parallel::mclapply(1:nm, function(mi){ #~version améliorée de lapply, rend une liste
      m = models[[mi]]
      sampleMcmc(m, samples = samples, thin=thin,
                     adaptNf=rep(ceiling(0.4*samples*thin),m$nr), 
                     transient = ceiling(0.5*samples*thin),
                     nChains = nChains,
                     nParallel = nParallel) 
    }, mc.cores = 5) # 5 x 4 chaines. 20 chaines en parallèle NE PAS UTILISER SUR L ORDIIIII

    save(models,file=filename)
  }
  Lst = Lst + 1
}

## 2. EXAMINE PARAMETERS
##################################################################################################

## MAKE THE SCRIPT REPRODUCIBLE (END)
set.seed(1)
# SETTING COMMONLY ADJUSTED PARAMETERS TO NULL WHICH CORRESPONDS TO DEFAULT CHOICE (BEGINNING)
support.level.beta = NULL #Default: 0.95
support.level.gamma = NULL #Default: 0.95
support.level.omega = NULL #Default: 0.9
var.part.order.explained = NULL #Default: in variance partitioning of explained variance, species are shown in the order they are in the model
var.part.order.raw = NULL #Default: in variance partitioning of raw variance, species are shown in the order they are in the model
show.sp.names.beta = NULL #Default: species names shown in beta plot if there are at most 30 species and no phylogeny
plotTree = NULL #Default: tree is plotted in Beta plot if the model includes it
omega.order = NULL #Default: species shown in the order they are in the model
show.sp.names.omega = NULL #Default: species names shown in beta plot if there are at most 30 species


# SET DIRECTORIES (BEGINNING)
resultDir = file.path(localDir, "results/with_traits")
if (!dir.exists(resultDir)) dir.create(resultDir)



if(is.null(support.level.beta)) support.level.beta = 0.95
if(is.null(support.level.gamma)) support.level.gamma =  0.95
if(is.null(support.level.omega)) support.level.omega =  0.9

library(Hmsc)
library(colorspace)
library(corrplot)
library(writexl)

# samples_list = c(250)
# thin_list = c(1)
nst = length(thin_list)
# nChains = 2

text.file = file.path(resultDir,"/parameter_estimates_model1.txt")
cat(c("This file contains additional information regarding parameter estimates.","\n","\n",sep=""),file=text.file)

for (Lst in nst:1) {
  thin = thin_list[Lst]
  samples = samples_list[Lst]
  filename = file.path(modelDir,paste("S2_fit_models_thin_", as.character(thin),
                                      "_samples_", as.character(samples),
                                      "_chains_",as.character(nChains),
                                      ".Rdata",sep = ""))
  if(file.exists(filename)){break}
}
if(file.exists(filename)){
  load(filename)
  cat(c("\n",filename,"\n","\n"),file=text.file,sep="",append=TRUE)
  nm = length(models)
  if(is.null(var.part.order.explained)){
    var.part.order.explained = list()
    for(j in 1:nm) var.part.order.explained[[j]] = 0
  }
  if(is.null(var.part.order.raw)){
    var.part.order.raw = list()
    for(j in 1:nm) var.part.order.raw[[j]] = 0
  }
  if(is.null(omega.order)){
    omega.order = list()
    for(j in 1:nm) omega.order[[j]] = 0
  }
  
  modelnames = names(models)
  
  pdf(file= file.path(resultDir,"parameter_estimates_ex2.pdf"))
  for(j in 1:nm){
    cat(c("\n",names(models)[j],"\n","\n"),file=text.file,sep="",append=TRUE)
    m = models[[j]]
    if(m$XFormula=="~."){
      covariates = colnames(m$X)[-1]
    } else {
      covariates = attr(terms(m$XFormula),"term.labels")
    }
    if(m$nr+length(covariates)>1 & m$ns>1){
      preds = computePredictedValues(m)
      VP = computeVariancePartitioning(m)
      vals = VP$vals
      mycols = rainbow(nrow(VP$vals))
      MF = evaluateModelFit(hM=m, predY=preds)
      R2 = NULL
      if(!is.null(MF$TjurR2)){
        TjurR2 = MF$TjurR2
        vals = rbind(vals,TjurR2)
        R2=TjurR2
      }
      if(!is.null(MF$R2)){
        R2=MF$R2
        vals = rbind(vals,R2)
      }
      if(!is.null(MF$SR2)){
        R2=MF$SR2
        vals = rbind(vals,R2)
      }
      filename = file.path(resultDir, paste("parameter_estimates_VP_",modelnames[j],".csv"))
      write.csv(vals,file=filename)
      if(!is.null(VP$R2T$Beta)){
        filename = file.path(resultDir,paste("parameter_estimates_VP_R2T_Beta",modelnames[j],".csv"))
        write.csv(VP$R2T$Beta,file=filename)
      }
      if(!is.null(VP$R2T$Y)){
        filename = file.path(resultDir, paste("parameter_estimates_VP_R2T_Y",modelnames[j],".csv"))
        write.csv(VP$R2T$Y,file=filename)
      }
      if(all(var.part.order.explained[[j]]==0)){
        c.var.part.order.explained = 1:m$ns
      } else {
        if(all(var.part.order.explained[[j]]=="decreasing")){
          c.var.part.order.explained = order(R2, decreasing = TRUE)
        }
        else {
          c.var.part.order.explained  = var.part.order.explained[[j]]
        }
      }
      VP$vals = VP$vals[,c.var.part.order.explained]
      cat(c("\n","var.part.order.explained","\n","\n"),file=text.file,sep="",append=TRUE)
      cat(m$spNames[c.var.part.order.explained],file=text.file,sep="\n",append=TRUE)
      plotVariancePartitioning(hM=m, VP=VP, main = paste0("Proportion of explained variance, ",modelnames[j]), cex.main=0.8, cols = mycols, args.leg=list(bg="white",cex=0.7))
      if(all(var.part.order.raw[[j]]==0)){
        c.var.part.order.raw = 1:m$ns
      } else {
        if(all(var.part.order.raw[[j]]=="decreasing")){
          c.var.part.order.raw = order(R2, decreasing = TRUE)
        }
        else {
          c.var.part.order.raw  = var.part.order.raw[[j]]
        }
      }
      if(!is.null(R2)){
        VPr = VP
        for(k in 1:m$ns){
          VPr$vals[,k] = R2[k]*VPr$vals[,k]
        }
        VPr$vals = VPr$vals[,c.var.part.order.raw]
        cat(c("\n","var.part.order.raw","\n","\n"),file=text.file,sep="",append=TRUE)
        cat(m$spNames[c.var.part.order.raw],file=text.file,sep="\n",append=TRUE)
        plotVariancePartitioning(hM=m, VP=VPr,main=paste0("Proportion of raw variance, ",modelnames[j]),cex.main=0.8, cols = mycols, args.leg=list(bg="white",cex=0.7),ylim=c(0,1))
      }
    }
  }
  for(j in 1:nm){
    m = models[[j]]
    if(m$nc>1){
      postBeta = getPostEstimate(m, parName="Beta")
      filename = file.path(resultDir, paste("parameter_estimates_Beta_",modelnames[j],".xlsx"))
      me = as.data.frame(t(postBeta$mean))
      me = cbind(m$spNames,me)
      colnames(me) = c("Species",m$covNames)
      po = as.data.frame(t(postBeta$support))
      po = cbind(m$spNames,po)
      colnames(po) = c("Species",m$covNames)
      ne = as.data.frame(t(postBeta$supportNeg))
      ne = cbind(m$spNames,ne)
      colnames(ne) = c("Species",m$covNames)
      vals = list("Posterior mean"=me,"Pr(x>0)"=po,"Pr(x<0)"=ne)
      writexl::write_xlsx(vals,path = filename)
      if(is.null(show.sp.names.beta)){
        c.show.sp.names = (is.null(m$phyloTree) && m$ns<=30) 
      } else {
        c.show.sp.names = show.sp.names.beta[j]
      }
      c.plotTree = !is.null(m$phyloTree)
      if(!is.null(plotTree)){
        c.plotTree = c.plotTree & plotTree[j]
      }
      plotBeta(m, post=postBeta, supportLevel = support.level.beta, param="Sign",
               plotTree = c.plotTree,
               covNamesNumbers = c(TRUE,FALSE),
               spNamesNumbers=c(c.show.sp.names,FALSE),
               cex=c(0.6,0.6,0.8))
      mymain = paste0("BetaPlot, ",modelnames[j])
      if(!is.null(m$phyloTree)){
        mpost = convertToCodaObject(m)
        rhovals = unlist(poolMcmcChains(mpost$Rho))
        mymain = paste0(mymain,", E[rho] = ",round(mean(rhovals),2),", Pr[rho>0] = ",round(mean(rhovals>0),2))
      }
      title(main=mymain, line=2.5, cex.main=0.8)
    }
  }
  for(j in 1:nm){
    m = models[[j]]      
    if(m$nt>1 & m$nc>1){
      postGamma = getPostEstimate(m, parName="Gamma")
      plotGamma(m, post=postGamma, supportLevel = support.level.gamma, param="Sign",
                covNamesNumbers = c(TRUE,FALSE),
                trNamesNumbers=c(m$nt<21,FALSE),
                cex=c(0.6,0.6,0.8))
      title(main=paste0("GammaPlot ",modelnames[j]), line=2.5,cex.main=0.8)
    }
  }
  for(j in 1:nm){
    m = models[[j]]
    if(m$nr>0 & m$ns>1){
      OmegaCor = computeAssociations(m)
      for (r in 1:m$nr){
        toPlot = ((OmegaCor[[r]]$support>support.level.omega) + (OmegaCor[[r]]$support<(1-support.level.omega))>0)*sign(OmegaCor[[r]]$mean)
        if(is.null(show.sp.names.omega)){
          c.show.sp.names = (m$ns<=30) 
        } else {
          c.show.sp.names = show.sp.names.omega[j]
        }
        if(!c.show.sp.names){
          colnames(toPlot)=rep("",m$ns)
          rownames(toPlot)=rep("",m$ns)
        }
        if(all(omega.order[[j]]==0)){
          plotOrder = 1:m$ns
        } else {
          if(all(omega.order[[j]]=="AOE")){
            plotOrder = corrMatOrder(OmegaCor[[r]]$mean,order="AOE")
          } else {
            plotOrder = omega.order[[j]]
          }
        }
        cat(c("\n","omega.order","\n","\n"),file=text.file,sep="",append=TRUE)
        cat(m$spNames[plotOrder],file=text.file,sep="\n",append=TRUE)
        mymain = paste0("Associations, ",modelnames[j], ": ",names(m$ranLevels)[[r]])
        if(m$ranLevels[[r]]$sDim>0){
          mpost = convertToCodaObject(m)
          alphavals = unlist(poolMcmcChains(mpost$Alpha[[1]][,1]))
          mymain = paste0(mymain,", E[alpha1] = ",round(mean(alphavals),2),", Pr[alpha1>0] = ",round(mean(alphavals>0),2))
        }
        corrplot(toPlot[plotOrder,plotOrder], method = "color",
                 col=colorRampPalette(c("blue","white","red"))(3),
                 mar=c(0,0,1,0),
                 main=mymain,cex.main=0.8)
        me = as.data.frame(OmegaCor[[r]]$mean)
        me = cbind(m$spNames,me)
        colnames(me)[1] = ""
        po = as.data.frame(OmegaCor[[r]]$support)
        po = cbind(m$spNames,po)
        colnames(po)[1] = ""
        ne = as.data.frame(1-OmegaCor[[r]]$support)
        ne = cbind(m$spNames,ne)
        colnames(ne)[1] = ""
        vals = list("Posterior mean"=me,"Pr(x>0)"=po,"Pr(x<0)"=ne)
        filename = file.path(resultDir, paste("parameter_estimates_Omega_",modelnames[j],"_",names(m$ranLevels)[[r]],".xlsx"))
        writexl::write_xlsx(vals,path = filename)
      }
    }
  }
  dev.off()
}




# 3. EVALUATE MODEL CONVERGENCE
##################################################################################################
#	INPUT. Fitted models

#	OUTPUT. MCMC convergence statistics for selected model parameters,
# illustrated (for all RUNs performed thus far in S3) in the file "results/MCMC_convergence.pdf",
# and the text file "results/MCMC_convergence.txt".


set.seed(1)


# SETTING COMMONLY ADJUSTED PARAMETERS TO NULL WHICH CORRESPONDS TO DEFAULT CHOICE (BEGINNING)
showBeta = NULL #Default: showBeta = TRUE, convergence shown for beta-parameters
showGamma = NULL #Default: showGamma = FALSE, convergence not shown for gamma-parameters
showOmega = NULL #Default: showOmega = FALSE, convergence not shown for Omega-parameters
maxOmega = NULL #Default: convergence of Omega shown for 50 randomly selected species pairs
showRho = NULL #Default: showRho = FALSE, convergence not shown for rho-parameters
showAlpha = NULL #Default: showAlpha = FALSE, convergence not shown for alpha-parameters


##################################################################################################
# CHANGE DEFAULT OPTIONS BY REMOVING COMMENT AND SETTING VALUE (BEGINNING)
# NOTE THAT THIS IS THE ONLY SECTION OF THE SCRIPT THAT YOU TYPICALLY NEED TO MODIFY
##################################################################################################
showBeta = TRUE
showGamma = TRUE
showOmega = TRUE
maxOmega = 100
showRho = TRUE
showAlpha = TRUE


##################################################################################################
# SET DIRECTORIES (BEGINNING)
##################################################################################################


if(is.null(showBeta)) showBeta = TRUE
if(is.null(showGamma)) showGamma = FALSE
if(is.null(showOmega)) showOmega = FALSE
if(is.null(maxOmega)) maxOmega = 50
if(is.null(showRho)) showRho = FALSE
if(is.null(showAlpha)) showAlpha = FALSE

library(Hmsc)
library(colorspace)
library(vioplot)

samples_list = c(5,250,250,250,250)#,250)
thin_list = c(1,1,10,100,1000)#,10000)
nst = length(thin_list)
nChains = 4

text.file = file.path(resultDir,"/MCMC_convergence.txt")
cat("MCMC Convergennce statistics\n\n",file=text.file,sep="")

ma.beta = NULL
na.beta = NULL
ma.gamma = NULL
na.gamma = NULL
ma.omega= NULL
na.omega = NULL
ma.alpha = NULL
na.alpha = NULL  
ma.rho = NULL
na.rho = NULL
Lst = 1
while(Lst <= nst){
  thin = thin_list[Lst]
  samples = samples_list[Lst]
  
  
  filename = file.path(modelDir,paste("S2_fit_models_thin_", as.character(thin),
                                      "_samples_", as.character(samples),
                                      "_chains_",as.character(nChains),
                                      ".Rdata",sep = ""))
  if(file.exists(filename)){
    load(filename)
    cat(c("\n",filename,"\n\n"),file=text.file,sep="",append=TRUE)
    nm = length(models)
    for(j in 1:nm){
      mpost = convertToCodaObject(models[[j]], spNamesNumbers = c(T,F), covNamesNumbers = c(T,F))
      nr = models[[j]]$nr
      cat(c("\n",names(models)[j],"\n\n"),file=text.file,sep="",append=TRUE)
      if(showBeta){
        psrf = gelman.diag(mpost$Beta,multivariate=FALSE)$psrf
        tmp = summary(psrf)
        cat("\nbeta\n\n",file=text.file,sep="",append=TRUE)
        cat(tmp[,1],file=text.file,sep="\n",append=TRUE)
        if(is.null(ma.beta)){
          ma.beta = psrf[,1]
          na.beta = paste0(as.character(thin),",",as.character(samples))
        } else {
          ma.beta = cbind(ma.beta,psrf[,1])
          if(j==1){
            na.beta = c(na.beta,paste0(as.character(thin),",",as.character(samples)))
          } else {
            na.beta = c(na.beta,"")
          }
        }
      }
      if(showGamma){
        psrf = gelman.diag(mpost$Gamma,multivariate=FALSE)$psrf
        tmp = summary(psrf)
        cat("\ngamma\n\n",file=text.file,sep="",append=TRUE)
        cat(tmp[,1],file=text.file,sep="\n",append=TRUE)
        if(is.null(ma.gamma)){
          ma.gamma = psrf[,1]
          na.gamma = paste0(as.character(thin),",",as.character(samples))
        } else {
          ma.gamma = cbind(ma.gamma,psrf[,1])
          if(j==1){
            na.gamma = c(na.gamma,paste0(as.character(thin),",",as.character(samples)))
          } else {
            na.gamma = c(na.gamma,"")
          }
        }
      }
      if(showRho & !is.null(mpost$Rho)){
        psrf = gelman.diag(mpost$Rho,multivariate=FALSE)$psrf
        cat("\nrho\n\n",file=text.file,sep="",append=TRUE)
        cat(psrf[1],file=text.file,sep="\n",append=TRUE)
      }
      if(showOmega & nr>0){
        cat("\nomega\n\n",file=text.file,sep="",append=TRUE)
        for(k in 1:nr){
          cat(c("\n",names(models[[j]]$ranLevels)[k],"\n\n"),file=text.file,sep="",append=TRUE)
          tmp = mpost$Omega[[k]]
          z = dim(tmp[[1]])[2]
          if(z > maxOmega){
            sel = sample(1:z, size = maxOmega)
            for(i in 1:length(tmp)){
              tmp[[i]] = tmp[[i]][,sel]
            }
          }
          psrf = gelman.diag(tmp, multivariate = FALSE)$psrf
          tmp = summary(psrf)
          cat(tmp[,1],file=text.file,sep="\n",append=TRUE)
          if(is.null(ma.omega)){
            ma.omega = psrf[,1]
            na.omega = paste0(as.character(thin),",",as.character(samples))
          } else {
            ma.omega = cbind(ma.omega,psrf[,1])
            if(j==1){
              na.omega = c(na.omega,paste0(as.character(thin),",",as.character(samples)))
            } else {
              na.omega = c(na.omega,"")
            }
          }
        }
      }
      if(showAlpha & nr>0){
        for(k in 1:nr){
          if(models[[j]]$ranLevels[[k]]$sDim>0){
            cat("\nalpha\n\n",file=text.file,sep="\n",append=TRUE)
            cat(c("\n",names(models[[j]]$ranLevels)[k],"\n\n"),file=text.file,sep="",append=TRUE)
            psrf = gelman.diag(mpost$Alpha[[k]],multivariate = FALSE)$psrf
            cat(psrf[,1],file=text.file,sep="\n",append=TRUE)            
          }
        }
      }
    }
  }
  Lst = Lst + 1
}

pdf(file= file.path(resultDir,"/MCMC_convergence.pdf"))
if(showBeta){
  par(mfrow=c(2,1))
  vioplot(ma.beta,col=rainbow_hcl(nm),names=na.beta,ylim=c(0,max(ma.beta)),main="psrf(beta)")
  legend("topright",legend = names(models), fill=rainbow_hcl(nm))
  vioplot(ma.beta,col=rainbow_hcl(nm),names=na.beta,ylim=c(0.9,1.1),main="psrf(beta)")
}
if(showGamma){
  par(mfrow=c(2,1))
  vioplot(ma.gamma,col=rainbow_hcl(nm),names=na.gamma,ylim=c(0,max(ma.gamma)),main="psrf(gamma)")
  legend("topright",legend = names(models), fill=rainbow_hcl(nm))
  vioplot(ma.gamma,col=rainbow_hcl(nm),names=na.gamma,ylim=c(0.9,1.1),main="psrf(gamma)")
}
if(showOmega & !is.null(ma.omega)){
  par(mfrow=c(2,1))
  vioplot(ma.omega,col=rainbow_hcl(nm),names=na.omega,ylim=c(0,max(ma.omega)),main="psrf(omega)")
  legend("topright",legend = names(models), fill=rainbow_hcl(nm))
  vioplot(ma.omega,col=rainbow_hcl(nm),names=na.omega,ylim=c(0.9,1.1),main="psrf(omega)")
}
dev.off()





# 4. COMPUTE MODEL FIT
##################################################################################################

# INPUT. Fitted models.

#	OUTPUT. Model fits computed by the cross-validation, with fitting
# (which is part of cross-validation) done for multiple RUNs:
# first short MCMC chains (to provide some results fast), and then with increasingly long MCMC chains
# (up to the longest run performed in S2). The results are stored in the files
# "models/MF_thin_1_samples_5_chains_4.Rdata" (RUN 0),
# "models/MF_thin_1_samples_250_chains_4.Rdata" (RUN 1),
# "models/MF_thin_10_samples_250_chains_4.Rdata" (RUN 2), 
# "models/MF_thin_100_samples_250_chains_4.Rdata" (RUN 3), and so on.

set.seed(1)


# SETTING COMMONLY ADJUSTED PARAMETERS TO NULL WHICH CORRESPONDS TO DEFAULT CHOICE (BEGINNING)
nfolds = NULL #Default: two-fold cross-validation
nParallel = NULL #Default: nParallel = nChains


# CHANGE DEFAULT OPTIONS BY REMOVING COMMENT AND SETTING VALUE (BEGINNING)
# NOTE THAT THIS IS THE ONLY SECTION OF THE SCRIPT THAT YOU TYPICALLY NEED TO MODIFY
##################################################################################################
# nfolds = 2 #change the number of CV-folds
# nParallel = 4 #Set to 1 to disable parallel computing


if(is.null(nfolds)) nfolds = 2

library(Hmsc)
samples_list = c(5,250,250,250,250)#,250)
thin_list = c(1,1,10,100,1000)#,10000)
nChains = 4
if(is.null(nParallel)) nParallel = nChains
Lst = 1
while(Lst <= length(samples_list)){
  thin = thin_list[Lst]
  samples = samples_list[Lst]
  filename.in = file.path(modelDir,paste("S2_fit_models_thin_", as.character(thin),
                                         "_samples_", as.character(samples),
                                         "_chains_",as.character(nChains),
                                         ".Rdata",sep = ""))
  filename.out = file.path(modelDir,paste("MF_thin_", as.character(thin),
                                          "_samples_", as.character(samples),
                                          "_chains_",as.character(nChains),
                                          "_nfolds_", as.character(nfolds),
                                          ".Rdata",sep = ""))
  if(file.exists(filename.out)){
    print(paste0("thin = ",as.character(thin),"; samples = ",as.character(samples)))
    print("model fit had been computed already")
  } else {
    if(file.exists(filename.in)){
      print(paste0("thin = ",as.character(thin),"; samples = ",as.character(samples)))
      print(date())
      load(file = filename.in) #models
      nm = length(models)
      
      MF = list()
      MFCV = list()
      WAIC = list()
      
      for(mi in 1:nm){
        print(paste0("model = ",names(models)[mi]))
        m = models[[mi]]
        preds = computePredictedValues(m)
        MF[[mi]] = evaluateModelFit(hM=m, predY=preds)
        partition = createPartition(m, nfolds = nfolds) #USE column = ...
        preds = computePredictedValues(m,partition=partition, nParallel = nParallel)
        MFCV[[mi]] = evaluateModelFit(hM=m, predY=preds)
        WAIC[[mi]] = computeWAIC(m)
      }
      names(MF)=names(models)
      names(MFCV)=names(models)
      names(WAIC)=names(models)
      
      save(MF,MFCV,WAIC,file = filename.out)
    }
  }
  Lst = Lst + 1
}


# 5.SHOW MODEL FIT

#	INPUT. Model fits.
#	OUTPUT. Model fits illustrated (for highest RUN of S4) in the file "results/model_fit.pdf".

nfolds = NULL #Default: two-fold cross-validation


# CHANGE DEFAULT OPTIONS BY REMOVING COMMENT AND SETTING VALUE (BEGINNING)
# NOTE THAT THIS IS THE ONLY SECTION OF THE SCRIPT THAT YOU TYPICALLY NEED TO MODIFY
#nfolds = 10 #change the number of CV-folds



if(is.null(nfolds)) nfolds = 2

library(Hmsc)

samples_list = c(5,250,250,250,250)#,250)
thin_list = c(1,1,10,100,1000)#,10000)
nst = length(thin_list)
nChains = 4

for (Lst in nst:1) {
  thin = thin_list[Lst]
  samples = samples_list[Lst]
  
  filename = file.path(modelDir,paste("MF_thin_", as.character(thin),
                                      "_samples_", as.character(samples),
                                      "_chains_",as.character(nChains),
                                      "_nfolds_", as.character(nfolds),
                                      ".Rdata",sep = ""))
  if(file.exists(filename)){break}
}
if(file.exists(filename)){
  load(filename)
  
  nm = length(MF)
  modelnames = names(MF)
  pdf(file = file.path(resultDir,paste0("/model_fit_nfolds_",nfolds,".pdf")))
  for(j in 1:nm){
    cMF = MF[[j]]
    cMFCV = MFCV[[j]]
    if(!is.null(cMF$TjurR2)){
      plot(cMF$TjurR2,cMFCV$TjurR2,xlim=c(-1,1),ylim=c(-1,1),
           xlab = "explanatory power",
           ylab = "predictive power",
           main=paste0(modelnames[j],", thin = ",
                       as.character(thin),
                       ", samples = ",as.character(samples),
                       ": Tjur R2.\n",
                       "mean(MF) = ",as.character(mean(cMF$TjurR2,na.rm=TRUE)),
                       ", mean(MFCV) = ",as.character(mean(cMFCV$TjurR2,na.rm=TRUE))))
      abline(0,1)
      abline(v=0)
      abline(h=0)
    }
    if(!is.null(cMF$R2)){
      plot(cMF$R2,cMFCV$R2,xlim=c(-1,1),ylim=c(-1,1),
           xlab = "explanatory power",
           ylab = "predictive power",
           main=paste0(modelnames[[j]],", thin = ",as.character(thin),
                       ", samples = ",as.character(samples),
                       ": R2. \n",
                       "mean(MF) = ",as.character(mean(cMF$R2,na.rm=TRUE)),
                       ", mean(MFCV) = ",as.character(mean(cMFCV$R2,na.rm=TRUE))))
      abline(0,1)
      abline(v=0)
      abline(h=0)
    }
    if(!is.null(cMF$AUC)){
      plot(cMF$AUC,cMFCV$AUC,xlim=c(0,1),ylim=c(0,1),
           xlab = "explanatory power",
           ylab = "predictive power",
           main=paste0(modelnames[[j]],", thin = ",as.character(thin),
                       ", samples = ",as.character(samples),
                       ": AUC. \n",
                       "mean(MF) = ",as.character(mean(cMF$AUC,na.rm=TRUE)),
                       ", mean(MFCV) = ",as.character(mean(cMFCV$AUC,na.rm=TRUE))))
      abline(0,1)
      abline(v=0.5)
      abline(h=0.5)
    }
    if(FALSE && !is.null(cMF$O.TjurR2)){
      plot(cMF$O.TjurR2,cMFCV$O.TjurR2,xlim=c(-1,1),ylim=c(-1,1),
           xlab = "explanatory power",
           ylab = "predictive power",
           main=paste0(modelnames[[j]],", thin = ",as.character(thin),", samples = ",as.character(samples),": O.Tjur R2"))
      abline(0,1)
      abline(v=0)
      abline(h=0)
    }
    if(FALSE && !is.null(cMF$O.AUC)){
      plot(cMF$O.AUC,cMFCV$O.AUC,xlim=c(0,1),ylim=c(0,1),
           xlab = "explanatory power",
           ylab = "predictive power",
           main=paste0(modelnames[[j]],", thin = ",as.character(thin),", samples = ",as.character(samples),": O.AUC"))
      abline(0,1)
      abline(v=0.5)
      abline(h=0.5)
    }      
    if(!is.null(cMF$SR2)){
      plot(cMF$SR2,cMFCV$SR2,xlim=c(-1,1),ylim=c(-1,1),
           xlab = "explanatory power",
           ylab = "predictive power",
           main=paste0(modelnames[[j]],", thin = ",as.character(thin),
                       ", samples = ",as.character(samples),
                       ": SR2. \n",
                       "mean(MF) = ",as.character(mean(cMF$SR2,na.rm=TRUE)),
                       ", mean(MFCV) = ",as.character(mean(cMFCV$SR2,na.rm=TRUE))))
      abline(0,1)
      abline(v=0)
      abline(h=0)
    }    
    if(FALSE && !is.null(cMF$C.SR2)){
      plot(cMF$C.SR2,cMFCV$C.SR2,xlim=c(-1,1),ylim=c(-1,1),
           xlab = "explanatory power",
           ylab = "predictive power",
           main=paste0(modelnames[[j]],", thin = ",as.character(thin),", samples = ",as.character(samples),": C.SR2"))
      abline(0,1)
      abline(v=0)
      abline(h=0)
    }  
  }
  dev.off()
}



# 6. SHOW PARAMETER ESTIMATES
#	INPUT. the Fitted models.

# OUTPUT: Parameter estimates illustrated (for highest RUN of S2) in the file
# "results/parameter_estimates.pdf", the text file "results/parameter_estimates.txt", 
# as well as given numerically in multiple csv files (one per parameter type) named 
# "results/parameter_estimates_[parameter_name].csv".

# SETTING COMMONLY ADJUSTED PARAMETERS TO NULL WHICH CORRESPONDS TO DEFAULT CHOICE (BEGINNING)
##################################################################################################
support.level.beta = NULL #Default: 0.95
support.level.gamma = NULL #Default: 0.95
support.level.omega = NULL #Default: 0.9
var.part.order.explained = NULL #Default: in variance partitioning of explained variance, species are shown in the order they are in the model
var.part.order.raw = NULL #Default: in variance partitioning of raw variance, species are shown in the order they are in the model
show.sp.names.beta = NULL #Default: species names shown in beta plot if there are at most 30 species and no phylogeny
plotTree = NULL #Default: tree is plotted in Beta plot if the model includes it
omega.order = NULL #Default: species shown in the order they are in the model
show.sp.names.omega = NULL #Default: species names shown in beta plot if there are at most 30 species

# CHANGE DEFAULT OPTIONS BY REMOVING COMMENT AND SETTING VALUE (BEGINNING)
# NOTE THAT THIS IS THE ONLY SECTION OF THE SCRIPT THAT YOU TYPICALLY NEED TO MODIFY
##################################################################################################
#use support levels to selected the level of statistical support shown
support.level.beta = 0.8
support.level.gamma = 0.8
support.level.omega = 0.8

#use var.part.order.explained to select which order species are shown in the raw variance partitioning
#var.part.order.raw should be a list of length the number of models. 
#for each element provide either 0 (use default);
#or a vector of species indices;
#or "decreasing" if you wish to order according to explanatory power
var.part.order.explained = list()
var.part.order.explained[[1]] = 0
var.part.order.explained[[2]] = c(2,1)

#use var.part.order.raw to select which order species are shown in the explained variance partitioning
#same options apply as for var.part.order.explained
var.part.order.raw = list()
var.part.order.raw[[1]] = "decreasing"
var.part.order.raw[[2]] = c(1,2)

#use show.sp.names.beta to choose to show / not show species names in betaPlot
#if given, show.sp.names.beta should be a vector with length equalling number of models
show.sp.names.beta = c(TRUE,FALSE)

#use plotTree to choose to plot / not plot the tree in betaPlot
#if given, plotTree should be a vector with length equalling number of models
plotTree = c(FALSE,FALSE)

#use omega.order to select which order species are shown in omega plots
#omega.order should be a list of length the number of models. 
#for each element provide either 0 (use default);
#or a vector of species indices;
#or "AOE" if you wish to use the angular order of the eigenvectors.
omega.order = list()
omega.order[[1]] = "AOE"
omega.order[[2]] = c(2,1)
#Default: species shown in the order they are in the model
show.sp.names.omega = c(TRUE,FALSE) #Default: species names shown in beta plot if there are at most 30 species



if(is.null(support.level.beta)) support.level.beta = 0.95
if(is.null(support.level.gamma)) support.level.gamma =  0.95
if(is.null(support.level.omega)) support.level.omega =  0.9

library(Hmsc)
library(colorspace)
library(corrplot)
library(writexl)

samples_list = c(5,250,250,250,250)#,250)
thin_list = c(1,1,10,100,1000)#,10000)
nst = length(thin_list)
nChains = 4

text.file = file.path(resultDir,"/parameter_estimates.txt")
cat(c("This file contains additional information regarding parameter estimates.","\n","\n",sep=""),file=text.file)

for (Lst in nst:1) {
  thin = thin_list[Lst]
  samples = samples_list[Lst]
  filename = file.path(modelDir,paste("S2_fit_models_thin_", as.character(thin),
                                      "_samples_", as.character(samples),
                                      "_chains_",as.character(nChains),
                                      ".Rdata",sep = ""))
  if(file.exists(filename)){break}
}
if(file.exists(filename)){
  load(filename)
  cat(c("\n",filename,"\n","\n"),file=text.file,sep="",append=TRUE)
  nm = length(models)
  if(is.null(var.part.order.explained)){
    var.part.order.explained = list()
    for(j in 1:nm) var.part.order.explained[[j]] = 0
  }
  if(is.null(var.part.order.raw)){
    var.part.order.raw = list()
    for(j in 1:nm) var.part.order.raw[[j]] = 0
  }
  if(is.null(omega.order)){
    omega.order = list()
    for(j in 1:nm) omega.order[[j]] = 0
  }
  
  modelnames = names(models)
  
  pdf(file= file.path(resultDir,"parameter_estimates.pdf"))
  for(j in 1:nm){
    cat(c("\n",names(models)[j],"\n","\n"),file=text.file,sep="",append=TRUE)
    m = models[[j]]
    if(m$XFormula=="~."){
      covariates = colnames(m$X)[-1]
    } else {
      covariates = attr(terms(m$XFormula),"term.labels")
    }
    if(m$nr+length(covariates)>1 & m$ns>1){
      preds = computePredictedValues(m)
      VP = computeVariancePartitioning(m)
      vals = VP$vals
      mycols = rainbow(nrow(VP$vals))
      MF = evaluateModelFit(hM=m, predY=preds)
      R2 = NULL
      if(!is.null(MF$TjurR2)){
        TjurR2 = MF$TjurR2
        vals = rbind(vals,TjurR2)
        R2=TjurR2
      }
      if(!is.null(MF$R2)){
        R2=MF$R2
        vals = rbind(vals,R2)
      }
      if(!is.null(MF$SR2)){
        R2=MF$SR2
        vals = rbind(vals,R2)
      }
      filename = file.path(resultDir, paste("parameter_estimates_VP_",modelnames[j],".csv"))
      write.csv(vals,file=filename)
      if(!is.null(VP$R2T$Beta)){
        filename = file.path(resultDir,paste("parameter_estimates_VP_R2T_Beta",modelnames[j],".csv"))
        write.csv(VP$R2T$Beta,file=filename)
      }
      if(!is.null(VP$R2T$Y)){
        filename = file.path(resultDir, paste("parameter_estimates_VP_R2T_Y",modelnames[j],".csv"))
        write.csv(VP$R2T$Y,file=filename)
      }
      if(all(var.part.order.explained[[j]]==0)){
        c.var.part.order.explained = 1:m$ns
      } else {
        if(all(var.part.order.explained[[j]]=="decreasing")){
          c.var.part.order.explained = order(R2, decreasing = TRUE)
        }
        else {
          c.var.part.order.explained  = var.part.order.explained[[j]]
        }
      }
      VP$vals = VP$vals[,c.var.part.order.explained]
      cat(c("\n","var.part.order.explained","\n","\n"),file=text.file,sep="",append=TRUE)
      cat(m$spNames[c.var.part.order.explained],file=text.file,sep="\n",append=TRUE)
      plotVariancePartitioning(hM=m, VP=VP, main = paste0("Proportion of explained variance, ",modelnames[j]), cex.main=0.8, cols = mycols, args.leg=list(bg="white",cex=0.7))
      if(all(var.part.order.raw[[j]]==0)){
        c.var.part.order.raw = 1:m$ns
      } else {
        if(all(var.part.order.raw[[j]]=="decreasing")){
          c.var.part.order.raw = order(R2, decreasing = TRUE)
        }
        else {
          c.var.part.order.raw  = var.part.order.raw[[j]]
        }
      }
      if(!is.null(R2)){
        VPr = VP
        for(k in 1:m$ns){
          VPr$vals[,k] = R2[k]*VPr$vals[,k]
        }
        VPr$vals = VPr$vals[,c.var.part.order.raw]
        cat(c("\n","var.part.order.raw","\n","\n"),file=text.file,sep="",append=TRUE)
        cat(m$spNames[c.var.part.order.raw],file=text.file,sep="\n",append=TRUE)
        plotVariancePartitioning(hM=m, VP=VPr,main=paste0("Proportion of raw variance, ",modelnames[j]),cex.main=0.8, cols = mycols, args.leg=list(bg="white",cex=0.7),ylim=c(0,1))
      }
    }
  }
  for(j in 1:nm){
    m = models[[j]]
    if(m$nc>1){
      postBeta = getPostEstimate(m, parName="Beta")
      filename = file.path(resultDir, paste("parameter_estimates_Beta_",modelnames[j],".xlsx"))
      me = as.data.frame(t(postBeta$mean))
      me = cbind(m$spNames,me)
      colnames(me) = c("Species",m$covNames)
      po = as.data.frame(t(postBeta$support))
      po = cbind(m$spNames,po)
      colnames(po) = c("Species",m$covNames)
      ne = as.data.frame(t(postBeta$supportNeg))
      ne = cbind(m$spNames,ne)
      colnames(ne) = c("Species",m$covNames)
      vals = list("Posterior mean"=me,"Pr(x>0)"=po,"Pr(x<0)"=ne)
      writexl::write_xlsx(vals,path = filename)
      if(is.null(show.sp.names.beta)){
        c.show.sp.names = (is.null(m$phyloTree) && m$ns<=30) 
      } else {
        c.show.sp.names = show.sp.names.beta[j]
      }
      c.plotTree = !is.null(m$phyloTree)
      if(!is.null(plotTree)){
        c.plotTree = c.plotTree & plotTree[j]
      }
      plotBeta(m, post=postBeta, supportLevel = support.level.beta, param="Sign",
               plotTree = c.plotTree,
               covNamesNumbers = c(TRUE,FALSE),
               spNamesNumbers=c(c.show.sp.names,FALSE),
               cex=c(0.6,0.6,0.8))
      mymain = paste0("BetaPlot, ",modelnames[j])
      if(!is.null(m$phyloTree)){
        mpost = convertToCodaObject(m)
        rhovals = unlist(poolMcmcChains(mpost$Rho))
        mymain = paste0(mymain,", E[rho] = ",round(mean(rhovals),2),", Pr[rho>0] = ",round(mean(rhovals>0),2))
      }
      title(main=mymain, line=2.5, cex.main=0.8)
    }
  }
  for(j in 1:nm){
    m = models[[j]]      
    if(m$nt>1 & m$nc>1){
      postGamma = getPostEstimate(m, parName="Gamma")
      plotGamma(m, post=postGamma, supportLevel = support.level.gamma, param="Sign",
                covNamesNumbers = c(TRUE,FALSE),
                trNamesNumbers=c(m$nt<21,FALSE),
                cex=c(0.6,0.6,0.8))
      title(main=paste0("GammaPlot ",modelnames[j]), line=2.5,cex.main=0.8)
    }
  }
  for(j in 1:nm){
    m = models[[j]]
    if(m$nr>0 & m$ns>1){
      OmegaCor = computeAssociations(m)
      for (r in 1:m$nr){
        toPlot = ((OmegaCor[[r]]$support>support.level.omega) + (OmegaCor[[r]]$support<(1-support.level.omega))>0)*sign(OmegaCor[[r]]$mean)
        if(is.null(show.sp.names.omega)){
          c.show.sp.names = (m$ns<=30) 
        } else {
          c.show.sp.names = show.sp.names.omega[j]
        }
        if(!c.show.sp.names){
          colnames(toPlot)=rep("",m$ns)
          rownames(toPlot)=rep("",m$ns)
        }
        if(all(omega.order[[j]]==0)){
          plotOrder = 1:m$ns
        } else {
          if(all(omega.order[[j]]=="AOE")){
            plotOrder = corrMatOrder(OmegaCor[[r]]$mean,order="AOE")
          } else {
            plotOrder = omega.order[[j]]
          }
        }
        cat(c("\n","omega.order","\n","\n"),file=text.file,sep="",append=TRUE)
        cat(m$spNames[plotOrder],file=text.file,sep="\n",append=TRUE)
        mymain = paste0("Associations, ",modelnames[j], ": ",names(m$ranLevels)[[r]])
        if(m$ranLevels[[r]]$sDim>0){
          mpost = convertToCodaObject(m)
          alphavals = unlist(poolMcmcChains(mpost$Alpha[[1]][,1]))
          mymain = paste0(mymain,", E[alpha1] = ",round(mean(alphavals),2),", Pr[alpha1>0] = ",round(mean(alphavals>0),2))
        }
        corrplot(toPlot[plotOrder,plotOrder], method = "color",
                 col=colorRampPalette(c("blue","white","red"))(3),
                 mar=c(0,0,1,0),
                 main=mymain,cex.main=0.8)
        me = as.data.frame(OmegaCor[[r]]$mean)
        me = cbind(m$spNames,me)
        colnames(me)[1] = ""
        po = as.data.frame(OmegaCor[[r]]$support)
        po = cbind(m$spNames,po)
        colnames(po)[1] = ""
        ne = as.data.frame(1-OmegaCor[[r]]$support)
        ne = cbind(m$spNames,ne)
        colnames(ne)[1] = ""
        vals = list("Posterior mean"=me,"Pr(x>0)"=po,"Pr(x<0)"=ne)
        filename = file.path(resultDir, paste("parameter_estimates_Omega_",modelnames[j],"_",names(m$ranLevels)[[r]],".xlsx"))
        writexl::write_xlsx(vals,path = filename)
      }
    }
  }
  dev.off()
}


# 7. MAKE PREDICTIONS  
#	INPUT. the Fitted models.

#	OUTPUT. Predictions over environmental gradients (for highest RUN of S2) in the file
# "results/predictions.pdf".
# SETTING COMMONLY ADJUSTED PARAMETERS TO NULL WHICH CORRESPONDS TO DEFAULT CHOICE (BEGINNING)
species.list = NULL #one example species shown for each model,
#selected as prevalence closest to 0.5 (probit models) or most abundant species (other models)
trait.list = NULL #community weighted mean shown for all traits
env.list = NULL #predictions constructed over all environmental gradients

# CHANGE DEFAULT OPTIONS BY REMOVING COMMENT AND SETTING VALUE (BEGINNING)
# NOTE THAT THIS IS THE ONLY SECTION OF THE SCRIPT THAT YOU TYPICALLY NEED TO MODIFY

#use species.list to select which species are used as examples for which predictions are shown
#species.list should be a list of length the number of models. 
#for each element provide either 0 (use default) or a vector of species indices
species.list = list()
species.list[[1]] = 0
species.list[[2]] = c(1,2)

#use trait.list to select for which traits predictions for community weighted mean traits are shown
#trait.list should be a list of length the number of models. 
#for each element provide either 0 (use default) or a vector of trait indices
#see models[[j]]$trNames to see which trait each index corresponds to
trait.list = list()
trait.list[[1]] = c(2,10)
trait.list[[2]] = 0

#use env.list to select over which environmental gradients predictions are generated
#env.list should be a list of length the number of models. 
#for each element provide either 0 (use default) or a vector of environmental variables
env.list = list()
env.list[[1]] = 0
env.list[[2]] = c("mean_temp","accum_prec")
##################################################################################################
# CHANGE DEFAULT OPTIONS BY REMOVING COMMENT AND SETTING VALUE (END)
# NOTE THAT THIS IS THE ONLY SECTION OF THE SCRIPT THAT YOU TYPICALLY NEED TO MODIFY
##################################################################################################



library(Hmsc)
library(ggplot2)

samples_list = c(5,250,250,250,250)#,250)
thin_list = c(1,1,10,100,1000)#,10000)
nst = length(thin_list)
nChains = 4

for (Lst in nst:4) {
  thin = thin_list[Lst]
  samples = samples_list[Lst]
  filename = file.path(modelDir,paste("S2_fit_models_thin_", as.character(thin),
                                      "_samples_", as.character(samples),
                                      "_chains_",as.character(nChains),
                                      ".Rdata",sep = ""))
  if(file.exists(filename)){break}
}
if(file.exists(filename)){
  load(filename)
  nm = length(models)
  modelnames = names(models)
  if(is.null(species.list)){
    species.list = list()
    for(j in 1:nm) species.list[[j]] = 0
  }
  if(is.null(trait.list)){
    trait.list = list()
    for(j in 1:nm) trait.list[[j]] = 0
  }
  if(is.null(env.list)){
    env.list = list()
    for(j in 1:nm) env.list[[j]] = 0
  }
  
  pdf(file= file.path(resultDir,"predictions.pdf"))
  for(j in 1:nm){
    m = models[[j]]
    if(all(env.list[[j]]==0)){
      if(m$XFormula=="~."){
        covariates = colnames(m$XData)
      } else {
        covariates = all.vars(m$XFormula)
      }
    } else {
      covariates = env.list[[j]]
    }
    ex.sp = which.max(colMeans(m$Y,na.rm = TRUE)) #most common species as example species
    if(m$distr[1,1]==2){
      ex.sp = which.min(abs(colMeans(m$Y,na.rm = TRUE)-0.5))
    }
    if(!all(species.list[[j]])==0){
      ex.sp = species.list[[j]]
    }
    if(length(covariates)>0){
      for(k in 1:(length(covariates))){
        covariate = covariates[[k]]
        Gradient = constructGradient(m,focalVariable = covariate)
        Gradient2 = constructGradient(m,focalVariable = covariate,non.focalVariables = 1)
        predY = predict(m, Gradient=Gradient, expected = TRUE)  
        predY2 = predict(m, Gradient=Gradient2, expected = TRUE)  
        par(mfrow=c(2,1))
        pl = plotGradient(m, Gradient, pred=predY, yshow = 0, measure="S", showData = TRUE, 
                          main = paste0(modelnames[j],": summed response (total effect)"))
        if(inherits(pl, "ggplot")){
          print(pl + labs(title=paste0(modelnames[j],": summed response (total effect)")))
        }
        pl = plotGradient(m, Gradient2, pred=predY2, yshow = 0, measure="S", showData = TRUE, 
                          main = paste0(modelnames[j],": summed response (marginal effect)"))
        if(inherits(pl, "ggplot")){
          print(pl + labs(title=paste0(modelnames[j],": summed response (marginal effect)")))
        }
        for(l in 1:length(ex.sp)){
          par(mfrow=c(2,1))
          pl = plotGradient(m, Gradient, pred=predY, yshow = if(m$distr[1,1]==2){c(-0.1,1.1)}else{0}, measure="Y",index=ex.sp[l], showData = TRUE, 
                            main = paste0(modelnames[j],": example species (total effect)"))
          if(inherits(pl, "ggplot")){
            print(pl + labs(title=paste0(modelnames[j],": example species (total effect)")))
          }
          pl = plotGradient(m, Gradient2, pred=predY2, yshow = if(m$distr[1,1]==2){c(-0.1,1.1)}else{0}, measure="Y",index=ex.sp[l], showData = TRUE, 
                            main = paste0(modelnames[j],": example species (marginal effect)"))
          if(inherits(pl, "ggplot")){
            print(pl + labs(title=paste0(modelnames[j],": example species (marginal effect)")))
          }
        }
        if(m$nt>1){
          traitSelection = 2:m$nt
          if(!all(trait.list[[j]]==0)) traitSelection = trait.list[[j]]
          for(l in traitSelection){
            par(mfrow=c(2,1))
            pl = plotGradient(m, Gradient, pred=predY, measure="T",index=l, showData = TRUE,yshow = 0,
                              main = paste0(modelnames[j],": community weighted mean trait (total effect)"))
            if(inherits(pl, "ggplot")){
              print(pl + labs(title=paste0(modelnames[j],": community weighted mean trait (total effect)")))
            }
            pl = plotGradient(m, Gradient2, pred=predY2, measure="T",index=l, showData = TRUE, yshow = 0,
                              main = paste0(modelnames[j],": community weighted mean trait (marginal effect)"))
            if(inherits(pl, "ggplot")){
              print(pl + labs(title=paste0(modelnames[j],": community weighted mean trait (marginal effect)")))
            }
          }
        }
      }
    }
  }
  dev.off()
}


