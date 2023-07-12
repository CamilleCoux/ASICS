# run 4: step 2 = fit models, run tests for preliminary results in few samples 
# to see if everything is working ok


# # define whether analysis over Crozet or Kergeulen :

crozet = TRUE


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

# 
# resultDir = file.path(localDir, "results/run4")
# if (!dir.exists(resultDir)) dir.create(resultDir)

if (crozet ==TRUE){
  load("cro/models/run_4/unfitted_models.RData")
}



# TESTING THAT MODELS FIT WITHOUT ERRORS 
##################################################################################################
Sys.time()
for(i in 1:length(models)){
  print(i)
  sampleMcmc(models[[i]],samples=2)
}
Sys.time()



# SETTING COMMONLY ADJUSTED PARAMETERS TO NULL WHICH CORRESPONDS TO DEFAULT CHOICE (BEGINNING)
##################################################################################################
nParallel = NULL #Default: nParallel = nChains
# samples=250, thin=1 

nm = length(models)
samples_list = c(5,250,250)#,250)#,250,250)
thin_list = c(1,1,10)#,100)# ,1000,10000)
nChains = 4
nst = length(thin_list)

if(is.null(nParallel)) nParallel = nChains
Lst = 1
while(Lst <= length(samples_list)){
  thin = thin_list[Lst]
  samples = samples_list[Lst]
  print(paste0("thin = ",as.character(thin),"; samples = ",as.character(samples)))
  filename = file.path(modelDir,paste("simple_model_thin_", as.character(thin),
                                      "_samples_", as.character(samples),
                                      "_chains_",as.character(nChains),
                                      ".Rdata",sep = ""))
  if(file.exists(filename)){
    print("model had been fitted already")
  } else {
    print(date())
    for (mi in 1:nm) {
      print(paste0("model = ",names(models)[mi]))
      m = models[[mi]]
      m = sampleMcmc(m, samples = samples, thin=thin,
                     adaptNf=rep(ceiling(0.4*samples*thin),m$nr), 
                     transient = ceiling(0.5*samples*thin),
                     nChains = nChains,
                     nParallel = nParallel) 
      models[[mi]] = m
    }
    save(models,file=filename)
  }
  Lst = Lst + 1
}
