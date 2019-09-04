# This script gives a new representation of the base forecasts to simplify the computational load
# when applying the permutations. It creates new Rdata files where each file gives the forecasts by half-hour.
rm(list = ls())

source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("utils.R")

library(ff)
load(file.path(work.folder, "myinfo.Rdata"))

ntest <- length(test$id)

QF_bottom <- vector("list", length(bottomSeries))
QF_agg <- vector("list", length(aggSeries))

obs_agg    <- revisedmean_agg    <- mean_agg <- matrix(NA, nrow =  length(aggSeries), ncol = ntest)
obs_bottom <- revisedmean_bottom <- mean_bottom <- matrix(NA, nrow =  length(bottomSeries), ncol = ntest)

for(do.agg in c(TRUE, FALSE)){
  
  if(do.agg){
    set_series <- aggSeries
    algo <- algo.agg
  }else{
    set_series <- bottomSeries
    algo <- algo.bottom
  }
  
  for(j in seq_along(set_series)){
   # if(j%%100 == 0)
    print(j)
    
    if(do.agg){
      QF_agg[[j]] <- matrix(NA, nrow = length(taus), ncol = ntest)
    }else{
      QF_bottom[[j]] <- matrix(NA, nrow = length(taus), ncol = ntest)
    }
    
    idseries <- set_series[j]
    
    load(file.path(basef.folder, algo, paste("results_", idseries, "_", algo, ".Rdata", sep = "")))

    load(file.path(work.folder, "revisedf", paste("revised_meanf_", algo.agg, "_", algo.bottom, "_", idseries, ".Rdata", sep = "")) )
    # mu_revised_alltest

    if(do.agg){
      load(file.path(aggseries.folder, paste("series-", idseries, ".Rdata", sep = "")))
      obs_agg[j, ] <- demand[test$id]
      revisedmean_agg[j, ] <- mu_revised_alltest
      mean_agg[j, ] <- unlist(all_mf)
    }else{
      load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
      obs_bottom[j, ] <- demand[test$id]
      revisedmean_bottom[j, ] <- mu_revised_alltest
      mean_bottom[j, ] <- unlist(all_mf)
    }
    
    
    for(idtest in seq(ntest)){
      iday <- getInfo(idtest)$iday
      hour <- getInfo(idtest)$hour
      
      if(do.agg){
        QF_agg[[j]][, idtest] <- all_qf[[iday]][, hour]
      }else{
        QF_bottom[[j]][, idtest] <- all_qf[[iday]][, hour]
      }
    }# idtest
  }# series
}# AGG and BOTTOM

#stop("done")
dir.create(file.path(work.folder, "byidtest"), recursive = TRUE, showWarnings = FALSE)

for(idtest in seq(ntest)){
  print(idtest)
  res_byidtest_file <- file.path(work.folder, "byidtest", paste("results_byidtest_", algo.agg, "_", algo.bottom, "_", idtest, ".Rdata", sep = ""))
  
  QF_agg_idtest <- sapply(seq(length(aggSeries)), function(j){
    QF_agg[[j]][, idtest]
  })
  
  QF_bottom_idtest <- sapply(seq(length(bottomSeries)), function(j){
    QF_bottom[[j]][, idtest]
  })
  
  obs_agg_idtest <- obs_agg[, idtest]
  obs_bottom_idtest <- obs_bottom[, idtest]
  
  revisedmean_bottom_idtest <- revisedmean_bottom[, idtest]
  revisedmean_agg_idtest    <- revisedmean_agg[, idtest]
  
  mean_bottom_idtest <- mean_bottom[, idtest]
  mean_agg_idtest    <- mean_agg[, idtest]

  save(file = res_byidtest_file, list = c("QF_agg_idtest", "QF_bottom_idtest", 
                                          "obs_agg_idtest", "obs_bottom_idtest",
                                          "revisedmean_bottom_idtest", 'revisedmean_agg_idtest',
                                          "mean_agg_idtest", "mean_bottom_idtest"))    
}
