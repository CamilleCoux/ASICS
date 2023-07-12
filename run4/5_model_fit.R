# run 4: step 5 :COMPUTE MODEL FIT



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
# samples_list = c(5,250,250,250,250)#,250)
# thin_list = c(1,1,10,100,1000)#,10000)
# nChains = 4
if(is.null(nParallel)) nParallel = nChains
Lst = 1
while(Lst <= length(samples_list)){
  thin = thin_list[Lst]
  samples = samples_list[Lst]
  filename.in = file.path(modelDir,paste("simple_model_thin_", as.character(thin),
                                         "_samples_", as.character(samples),
                                         "_chains_",as.character(nChains),
                                         ".Rdata",sep = ""))
  filename.out = file.path(modelDir,paste("simple_model_MF_thin_", as.character(thin),
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

# samples_list = c(5,250,250,250,250)#,250)
# thin_list = c(1,1,10,100,1000)#,10000)
# nst = length(thin_list)
# nChains = 4

for (Lst in nst:1) {
  thin = thin_list[Lst]
  samples = samples_list[Lst]
  
  filename = file.path(modelDir,paste("simple_model_MF_thin_", as.character(thin),
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
  pdf(file = file.path(resultDir,paste0("/simple_model_fit_nfolds_",nfolds,".pdf")))
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


