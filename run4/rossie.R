# script to launch models on Rossie

# need to set up separate scripts for model mcmc sampling on Rossie.
# and for the convergence tests i guess..
# but the model results can be explored together.
# ask if possible to allocate 10 cores to each script.


# 1. define models that include 3 traits and 11 species :
crozet = TRUE

source("run4/1_define_models.R")

# output: a list of models stored here:
load("cro/models/run_4/unfitted_models.RData")

# 2. run crash tests for the 3 models
# simple model ---------------------------
models <- purrr::keep(models, names(models)=="m_simple")

# SET DIRECTORIES for simple model run:
if (crozet){
  localDir = "cro"
  if(!dir.exists(localDir)) dir.create(localDir)
  modelDir = file.path(localDir, "models/run_4/simple_model")
  if(!dir.exists(modelDir)) dir.create(modelDir)
}else{
  localDir = "ker"
  if(!dir.exists(localDir)) dir.create(localDir)
  modelDir = file.path(localDir, "models/run_4/simple_model")
  if(!dir.exists(modelDir)) dir.create(modelDir)
}

resultDir = file.path(localDir, "results/run4/simple_model")
if (!dir.exists(resultDir)) dir.create(resultDir)

source("run4/2_test_runs.R")

# end crash test simple model --------------------------------------------------

load("cro/models/run_4/unfitted_models.RData")

# m_site_id ---------------------------
models <- purrr::keep(models, names(models)=="m_site_id")

# SET DIRECTORIES for simple model run:
if (crozet){
  localDir = "cro"
  if(!dir.exists(localDir)) dir.create(localDir)
  modelDir = file.path(localDir, "models/run_4/m_site_id")
  if(!dir.exists(modelDir)) dir.create(modelDir)
}else{
  localDir = "ker"
  if(!dir.exists(localDir)) dir.create(localDir)
  modelDir = file.path(localDir, "models/run_4/m_site_id")
  if(!dir.exists(modelDir)) dir.create(modelDir)
}

resultDir = file.path(localDir, "results/run4/m_site_id")
if (!dir.exists(resultDir)) dir.create(resultDir)

source("run4/2_test_runs.R")

# end crash test m_site_id -----------------------------------------------------


load("cro/models/run_4/unfitted_models.RData")

# m_spatial_small --------------------------------------------------------------
models <- purrr::keep(models, names(models)=="m_spatial_small")

# SET DIRECTORIES for simple model run:
if (crozet){
  localDir = "cro"
  if(!dir.exists(localDir)) dir.create(localDir)
  modelDir = file.path(localDir, "models/run_4/m_spatial_small")
  if(!dir.exists(modelDir)) dir.create(modelDir)
}else{
  localDir = "ker"
  if(!dir.exists(localDir)) dir.create(localDir)
  modelDir = file.path(localDir, "models/run_4/m_spatial_small")
  if(!dir.exists(modelDir)) dir.create(modelDir)
}

resultDir = file.path(localDir, "results/run4/m_spatial_small")
if (!dir.exists(resultDir)) dir.create(resultDir)

source("run4/2_test_runs.R")

# end crash test m_spatial_small ----------------------------------------------

# ok so this doesn't work, even for a very small number of samples etc.






# 3. examine parameters

crozet=TRUE

load("cro/models/run_4/unfitted_models.RData")

# 2. run crash tests for the 3 models
models <- purrr::keep(models, names(models)=="m_simple")

# SET DIRECTORIES for simple model run:
if (crozet){
  localDir = "cro"
  if(!dir.exists(localDir)) dir.create(localDir)
  modelDir = file.path(localDir, "models/run_4/simple_model/")
  if(!dir.exists(modelDir)) dir.create(modelDir)
}else{
  localDir = "ker"
  if(!dir.exists(localDir)) dir.create(localDir)
  modelDir = file.path(localDir, "models/run_4/simple_model")
  if(!dir.exists(modelDir)) dir.create(modelDir)
}

resultDir = file.path(localDir, "results/run4/simple_model/")
if (!dir.exists(resultDir)) dir.create(resultDir)


