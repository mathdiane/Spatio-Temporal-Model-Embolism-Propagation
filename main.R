## library ####
library(survival)
library("spBayesSurv")

## data path ####
input_data_dir <- file.path(".")
input_filename <- "real_data.RData"
all_leaf_folders <- list.dirs(input_data_dir, full.names = FALSE)
all_leaf_folders <- all_leaf_folders[-1] #ignore the first element (i.e.,"")

folder_i <- "drygla4_leaf" 
#note: some leafs are more time-consuming (because there are more vein segments)
#so use "drygla4_leaf" as an example
#if want to run all the leafs, can uncomment the for-loop
#for(folder_i in all_leaf_folders){
  
  ### print folder name
  print(folder_i)
  
  ### load data
  load(file.path(input_data_dir,folder_i,input_filename))
  
  
  ### assume right-censored
  X$t[X$delta==0] <- T2
  
  ###fit the model
  mcmc <- list(nburn = 5000, nsave = 2000, nskip = 4, ndisplay = 1000)
  prior <- list(maxL = 15, taua0=0.0001, taub0=0.0001) 
  
  #iid model
  res_iid <- survregbayes(formula = Surv(t, delta) ~ frailtyprior("iid", new_region), data = X, survmodel = "PO", dist = "weibull", mcmc = mcmc, prior = prior)
  
  #icar model
  res_icar <- survregbayes(formula = Surv(t, delta) ~ frailtyprior("car", new_region), data = X, survmodel = "PO", dist = "weibull", mcmc = mcmc, prior = prior, Proximity = E_new)
  
  #icar+th model
  res_icar_th <- survregbayes(formula = Surv(t, delta) ~ v + frailtyprior("car", new_region), data = X, survmodel = "PO", dist = "weibull", mcmc = mcmc, prior = prior, Proximity = E_new)
  
  
  ### posterior inference
  sfit_iid <- summary(res_iid)
  print("IID model:")
  print(sfit_iid)
  
  sfit_icar <- summary(res_icar)
  print("ICAR model:")
  print(sfit_icar)
  
  sfit_icar_th <- summary(res_icar_th)
  print("ICAR+th model:")
  print(sfit_icar_th)
  
  
#} #for-loop