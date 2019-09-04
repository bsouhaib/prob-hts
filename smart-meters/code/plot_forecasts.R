# This script allows to produce a plot for one-day-ahead probabilistic forecasts (with 50% and 95% prediction intervals). 
rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("utils.R")

load(file.path(work.folder, "myinfo.Rdata"))

plot.permsamples <- FALSE
do.jasa <- TRUE

if(plot.permsamples){
  
  do.agg <- T
  algo.agg <- "DETS"
  algo.bottom  <- "KD-IC-NML"
  
  alliseries <- c(1)
  idays <- seq(1, 2, by = 1)
  idays <- 1
  algorithms <- c("INDEPBU", "PERMBU", "MINTshrink", "INDEPBU-MINTshrink", "PERMBU-MINTshrink", "BASE")
  agg_methods <- c("BASE", "INDEPBU", "PERMBU", "PERMBU-MINTshrink", "INDEPBU-MINTshrink", "MINTdiag", "MINTshrink")
  
  nbperjob <- 368
  
  QF_agg <- array(NA, c(M, length(algorithms), 48*2))
  allidtest <- seq(1, 48*2)
  idjob <- 1
  samples_job <- file.path(work.folder, "samples_agg", paste("samples_agg_", algo.agg, "_", algo.bottom, "_", idjob, ".Rdata", sep = "")) 
  load(samples_job)
  
  list_samples_agg_nonull <- list_samples_agg[-which(sapply(list_samples_agg, is.null))]
  BIGARRAY <- sapply(seq_along(list_samples_agg_nonull),  function(i){list_samples_agg_nonull[[i]]}, simplify = 'array')
  QF_agg[, , allidtest] <- BIGARRAY[, alliseries, match(algorithms, agg_methods), ]
  
  mf_agg <- apply(QF_agg, c(2, 3), mean)
  qf_agg <- apply(QF_agg, c(2, 3), quantile, prob = taus)
  all_qf <- lapply(idays, function(iday){
    qf_agg[, , (iday - 1) * 48 + seq(48) ]
  })
  all_mf <- lapply(idays, function(iday){
    mf_agg[, (iday - 1) * 48 + seq(48) ]
  })
  

}else{
  do.agg <- F
  alliseries <- seq(200) #c(1267)
  algorithms <- c("KD-IC-NML")

  idays <- seq(1, 92, by = 1)
  idays <- 1
  
  if(do.jasa){
    series_isagg <- c(TRUE, FALSE) # actual
    alliseries <- c(1, 34) # actual
    idays <- c(11) # bettter coverage
  }
}

only.future <- FALSE

if(do.jasa){
  tag <- "example"
  savepdf(file.path(results.folder, paste("PLOT_forecasts_", tag, sep = "")), height = 27 * 0.25, width = 21)
  par(mfrow = c(1, 2))
}else{
  tag <- "allmethods"
  savepdf(file.path(results.folder, paste("PLOT_forecasts_", tag, sep = "") ))
}

for(iseries in alliseries){

  if(do.jasa){
    do.agg <- series_isagg[which(iseries == alliseries)]
    if(do.agg){
      algorithms <- "DETS"
    }else{
      algorithms <- "KD-IC-NML"
    }
  }
  
  print(iseries)
  if(do.agg){
    idseries <- aggSeries[iseries]
    load(file.path(aggseries.folder, paste("series-", idseries, ".Rdata", sep = "")))
  }else{
    idseries <- bottomSeries[iseries]
    load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
  }
  
  
  if(!do.jasa){
    #par(mfrow = c(2, 2))
    par(mfrow = c(2, 3))
  }
  
  list_load <- vector("list", length(algorithms))
  for(ialgo in seq_along(algorithms)){
    algo <- algorithms[ialgo]
    algo_load <- algo
    
    if(plot.permsamples){
      qf <- lapply(idays, function(iday){
        all_qf[[iday]][, ialgo, ]
      })
      mf <- lapply(idays, function(iday){
        all_mf[[iday]][ialgo, ]
      })
      
      list_load[[ialgo]] <-  list(all_qf = qf, all_mf = mf)
    }else{
      res_file <- file.path(basef.folder, algo, paste("results_", idseries, "_", algo, ".Rdata", sep = "")) 
      load(res_file)
  
      if(algo_load == "KD-IC-NML"){
        list_load[[ialgo]] <- list(all_qf = all_qf, all_mf = all_mf) #res_testing
      }else if(algo_load == "TBATS" || algo_load == "DYNREG" || algo_load == "DETS"){
        list_load[[ialgo]] <- list(all_qf = all_qf, all_mf = all_mf)
        #list_load[[ialgo]] <- list(all_qf = all_qf, all_mf = all_mf, all_mfsample = all_mfsample)
      }else if(algo_load == "Uncond"){
        list_load[[ialgo]] <- list(qFtest = qFtest, mFtest = mFtest)
      }
    }
  }#algo
  
  
  for(iday in idays){
    
    day_min <- Inf
    day_max <- -Inf
    for(ialgo in seq_along(algorithms)){
      day_min <- pmin(day_min, min(list_load[[ialgo]]$all_qf[[iday]]))
      day_max <- pmax(day_max, max(list_load[[ialgo]]$all_qf[[iday]]))
    }

    print(iday)
    for(ialgo in seq_along(algorithms)){
      
      algo <- algorithms[ialgo]
      algo_load <- algo
      
      if(algo_load == "KD-IC-NML" || algo_load == "TBATS" || algo_load == "DYNREG" || algo_load == "DETS" || plot.permsamples){
        
        all_qf <- list_load[[ialgo]]$all_qf
        all_mf <- list_load[[ialgo]]$all_mf
        mu_hat <- matrix(unlist(all_mf), ncol = 48, byrow = T)
        
        qf_allhours <- all_qf[[iday]]

      }else if(algo_load == "Uncond"){
        qFtest <- list_load[[ialgo]]$qFtest
        mFtest <- list_load[[ialgo]]$mFtest
        
        qf_allhours <- qFtest
        mu_hat <- matrix(mFtest, ncol = 48, byrow = T)
      }

      rownames(qf_allhours) <- paste(taus*100, "%", sep = "")
      
      future <- demand[test$id[(iday - 1) * 48 + seq(1, 48)]]
      subtaus    <- c("5%", "25%", "75%", "95%")
      #subtaus    <- c("1%", "25%", "75%", "99%")
      
      mymain <- ifelse(algo == "KD-IC-NML", "Individual smart meter", ifelse(algo == "DETS", "Top aggregated series", algo))
      
      #myYLIM <- c(0, max(c(future, qf_allhours[subtaus, ]), na.rm = T))	
        myYLIM <- c(day_min, day_max)
        
        plotQF(qf_allhours, future, subtaus, id = seq(48), only.future = only.future,
               main = mymain, xlab = "Time of day", ylab = "Consumption (kWh)", xaxt='n', cex.lab = 1.2, ylim = myYLIM)
        
        #axis(1, labels = tday, at = seq(1, 48))
        itday <- c(1, seq(8, 48, by = 8))
        axis(1, labels = tday[itday], at = itday, cex.axis=0.9)
        
        lines(mu_hat[iday, ], col = "red")

    }# ALGO
    
  }# DAY
  
  #dev.off()
} # iseries
dev.off()
