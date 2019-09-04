# This script produces the base forecasts for both the bottom-level and aggregate-level series. 
# KDE is used for the bottom-level series, and double exponential smoothing is used for the aggregate-level series.
# The arguments include 
# "do.agg": forecasts are produced for the aggregate-level series? (True or False)
# "algo": the forecasting algorithm ("KD-IC-NML", "DETS" or "Uncond")
# "alliseries": the index of all series for which we want to produce the forecasts
rm(list = ls())
print(base::date())
args = (commandArgs(TRUE))
if(length(args) == 0){
  do.agg <- F
  algo <- c("KD-IC-NML")
  #alliseries <- c(1, 20, 100)
  alliseries <- 320
}else{
  
  for(i in 1:length(args)){
    print(args[[i]])
  }
  
  algo <- args[[1]]
  do.agg <- as.logical(args[[2]])
  alliseries <- NULL
  for(i in seq(3, length(args))){
    alliseries <- c(alliseries, as.numeric(args[[i]]))
  }
}

print(algo)

source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("utils.R")

library(parallel)
library(fBasics)
library(msm)
library(gtools)
library(forecast)
library(abind)
library(glmnet)

load(file.path(work.folder, "myinfo.Rdata"))

algos_allowed <- c("Uncond", "KD-IC-NML", "DETS")
stopifnot(algo %in% algos_allowed)

for(iseries in alliseries){

  print(base::date())
  print(iseries)
  if(do.agg){
    idseries <- aggSeries[iseries]
    load(file.path(aggseries.folder, paste("series-", idseries, ".Rdata", sep = "")))
  }else{
    idseries <- bottomSeries[iseries]
    load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
  }
  
  print(algo)
  res_file <- file.path(basef.folder, algo, paste("results_", idseries, "_", algo, ".Rdata", sep = "")) 
  dir.create(file.path(basef.folder, algo), showWarnings = FALSE)
 
  if(algo == "DETS"){
    
    do.logtrans <- FALSE
    if(do.logtrans){
      my_ts <- log(demand)	     
    }else{
      my_ts <- demand
    }
    
    ids_future <- test$id
    nb_futuredays <- length(seq_testing_interval)/48
    
    all_qf <- all_mf <- all_sd <- all_mfsample <- vector("list", nb_futuredays)
    mydays <- seq(1, nb_futuredays)
    
    for(id_future_day in mydays){
      print(id_future_day)
      print(base::date())
      
      if(id_future_day == 1){
        ids_past   <-  learn$id
        n_past_obs <- length(ids_past)
      }else{
        n_past_obs <- n_past_obs_tbats
        ids_past   <- tail(learn$id, n_past_obs)
      }
      
      offset_nhours <- (id_future_day - 1) * 48
      ids_future_hours <- ids_future[offset_nhours + seq(1, 48)] 
      
      if(offset_nhours > 0){
        ids_past_actual <- c(ids_past, ids_future)[offset_nhours + seq(n_past_obs)]
      }else{
        ids_past_actual <- ids_past
      }
      
      ypast <- as.numeric(my_ts[ids_past_actual])
      
      do.optimize <- (id_future_day - 1) %% 7 == 0
      
      
        # initialization
        a <- 1/336 * (mean(ypast[seq(336)]) - mean(ypast[336 + seq(336)]))
        b <- mean(diff(ypast[seq(336)]))
        T_0 <- (a+b)/2
        l_start <- mean(ypast[seq(2 * 336)]) - 336.5 * T_0
        
        # days
        nb_obs <- 7 * m_1
        indices <- seq(nb_obs)
        smoothed_line <- ma(ypast[indices], m_1)
        #indices <- seq(m_1/2 + 1, nb_obs - m_1/2)
        x <- ypast[indices] - smoothed_line[indices]
        mat <- matrix(x, ncol = 48, byrow = T)
        D <- apply(mat, 2, mean, na.rm = T)
        D <- D - mean(D)
        
        # weeks
        nb_weeks <- 4
        indices <- seq(nb_weeks * m_2)
        smoothed_line <- ma(ypast[indices], m_2)
        x <- ypast[indices] - smoothed_line[indices] - rep(D, nb_weeks * 7)
        mat <- matrix(x, ncol = 336, byrow = T)
        W <- apply(mat, 2, mean, na.rm = T)
        W <- W - mean(W)
        
        e_0 <- rep(0, m_2)
        l_0 <- rep(l_start, m_2)
        d_0 <- rep(D, 7)
        w_0 <- W
        
      ###  
      if(do.optimize){  
        N <- 100
        THETA <-  matrix(runif(N * 4), ncol = 4)
        E <- sapply(seq(nrow(THETA)), function(i){ 
          #print(i)
          func_to_optimize(THETA[i, ], y = ypast, e_0 = e_0, l_0 = l_0, d_0 = d_0, w_0 = w_0, do.forecast = FALSE)
        })
        id <- sort(E, index = T)$ix[1]
        res_optim <- optim(THETA[id, ], fn = func_to_optimize, y = ypast, e_0 = e_0, l_0 = l_0, d_0 = d_0, w_0 = w_0, do.forecast = F,
                           method = "L-BFGS-B", lower = 0, upper = 1)
        if(id_future_day == 1){
          param_file <- file.path(basef.folder, algo, paste("parameters_", idseries, "_", algo, ".Rdata", sep = "")) 
          save(file = param_file, list = c("res_optim"))
          #stop("done")
        }
      }
        
      obj_forecast <- iterate(res_optim$par, ypast, e_0, l_0, d_0, w_0, do.forecast = T)
      
      
      if(id_future_day == 1){
        dir.create(file.path(insample.folder, algo), recursive = TRUE, showWarnings = FALSE)
        # insample mean
        all_mu <- obj_forecast$yhat
        insample_condmean_file <- file.path(insample.folder, algo, paste("condmean_", idseries, "_", algo, "_", id_future_day, ".Rdata", sep = "")) 
        save(file = insample_condmean_file, list = c("all_mu"))
        
        # residuals COPULA
        e_residuals <- obj_forecast$residuals
        resid_file <- file.path(insample.folder, algo, paste("residuals_", idseries, "_", algo, "_", id_future_day, ".Rdata", sep = "")) 
        save(file = resid_file, list = c("e_residuals"))
        
        # residuals MINT
        residuals_MINT <- obj_forecast$residuals
        resid_MINT_file <- file.path(insample.folder, algo, paste("residuals_MINT_", idseries, "_", algo, "_", id_future_day, ".Rdata", sep = "")) 
        save(file = resid_MINT_file, list = c("residuals_MINT"))
      }

      all_mf[[id_future_day]] <- obj_forecast$mf
      all_qf[[id_future_day]] <- obj_forecast$qf
      
      all_mfsample[[id_future_day]] <- obj_forecast$mfsample
      
    }
    list_save <- c("all_qf", "all_mf", "all_mfsample")
    save(file = res_file, list = list_save)
    
  }else if(algo == "Uncond"){
    qFlearn <- quantile(demand[learn$id], taus)
    qFtest <- matrix(rep(qFlearn, length(test$id)), ncol = length(test$id))
    
    mFlearn <- mean(demand[learn$id])
    mFtest <- rep(mFlearn, length(test$id))
    save(file = res_file, list = c("qFtest", "mFtest"))
    
  }else if(grepl("KD-D", algo) || grepl("KD-IC", algo)){
    
    
    if(grepl("TRC", algo)){
      mykernel <- "truncated"
    }else if(grepl("NML", algo)){
      mykernel <- "normal"
    }else if(grepl("LNL", algo)){
      mykernel <- "lognormal"
    }else{
      mykernel <- "normal"
    }
    
    ### LEARNING
    res_learning <- predictkde("learning")
    
    results_crps <- sapply(res_learning$results, function(list_vectors){
        sapply(list_vectors, function(list_two){
          sapply(list_two, function(vector){
            identity(vector)
          }, simplify = "array")
          
          }, simplify = "array")
        }, simplify = "array") 
    
    ic_days <- res_learning$ic_days
    
    idbest_bandwiths <- idbest_lambda <- NULL
    for(ic in seq(3)){
      err <- apply(results_crps[, , , which(ic_days == ic)], c(1, 3), mean)
      idbest <- which(err == min(err), arr.ind = T)

      idbest_bandwiths <- c(idbest_bandwiths, idbest[1, 1])
      idbest_lambda    <- c(idbest_lambda, idbest[1, 2])
    }
    selected_bandwiths_ic <- res_learning$bandwiths[idbest_bandwiths]
    selected_lambdas_ic <- res_learning$lambdas[idbest_lambda]
    
    param_file <- file.path(basef.folder, algo, paste("parameters_", idseries, "_", algo, ".Rdata", sep = "")) 
    bandwiths <- res_learning$lambdas
    save(file = param_file, 
         list = c("selected_bandwiths_ic", "selected_lambdas_ic", "bandwiths"))
    
    ### TESTING
    res_testing <- predictkde("testing", selected_bandwiths = selected_bandwiths_ic, selected_lambdas = selected_lambdas_ic)
    
    # all_crps <- getItem(res_testing$results, "crps")
    all_qf  <- getfromlist(res_testing$results, "q_hat")
    all_mf  <- getfromlist(res_testing$results, "mu_hat")
    all_varf  <- getfromlist(res_testing$results, "var_hat")
    
    save(file = res_file, list = c("all_qf", "all_mf", "all_varf"))
    
    ### IN SAMPLE INFO
    res_insample_info <- predictkde("insample_info", selected_bandwiths = selected_bandwiths_ic, selected_lambdas = selected_lambdas_ic)

    # residuals
    all_residuals <- getfromlist(res_insample_info$results, "residuals")
    e_residuals_unscaled <- unlist(all_residuals)
    all_var <- getfromlist(res_insample_info$results, "var_hat")
    all_mu <- getfromlist(res_insample_info$results, "mu_hat")
    
    all_varhat <- unlist(all_var)
    e_residuals <- e_residuals_unscaled/sqrt(all_varhat)
    
    # save residuals COPULA
    dir.create(file.path(insample.folder, algo), recursive = TRUE, showWarnings = FALSE)
    resid_file <- file.path(insample.folder, algo, paste("residuals_", idseries, "_", algo, ".Rdata", sep = "")) 
    save(file = resid_file, list = c("e_residuals"))
    
    # save residuals MINT
    residuals_MINT <- e_residuals_unscaled
    resid_MINT_file <- file.path(insample.folder, algo, paste("residuals_MINT_", idseries, "_", algo, ".Rdata", sep = "")) 
    save(file = resid_MINT_file, list = c("residuals_MINT"))
    
    # extract insample quantiles
    all_qf_insample  <- getfromlist(res_insample_info$results, "q_hat")

    all_qfe_insample <- lapply(seq_along(length(all_qf_insample)), function(iday){
      t((t(all_qf_insample[[iday]]) - all_mu[[iday]])/sqrt(all_var[[iday]]))
    })
    
    insample_condmean_file <- file.path(insample.folder, algo, paste("condmean_", idseries, "_", algo, ".Rdata", sep = "")) 
    save(file = insample_condmean_file, list = c("all_mu"))
  }
}
