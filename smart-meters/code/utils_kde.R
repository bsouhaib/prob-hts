# Functions needed to compute forecasts based on kernel density estimates.

predictkde <- function(task = c("learning", "testing", "insample_info"), selected_bandwiths = NULL, selected_lambdas = NULL){
  
  n_past_obs <- n_past_obs_kd
  
  if(task == "learning"){
    
    ids_past   <- tail(train$id, n_past_obs)
    ids_future <- validation$id
    
    ##### Bandwith interval #####
    #n_base <- length(train$id)
    n_base <- n_past_obs
    if(grepl("KD-D", algo)){
      n_approx <- n_base/48
    }else if(grepl("KD-IC", algo)){
      #n_approx <- n_base/144 # 144 = 48 * 3
      n_approx <- (n_base/336)*4  #(should be 5 for weekdays and 3 for weekends. So (3+5)/2 = 4)
    }else{
      stop("error in algo")
    }
    
    n_approx <- 8
    
    stopifnot(!is.null(n_approx))
    
    if(mykernel == "normal" || mykernel == "truncated"){
      x_samples <- sample(demand[ids_past], n_approx)
    }else if(mykernel == "lognormal"){
      x_samples <- log(sample(demand[ids_past], n_approx))
    }
    
    res <- sapply(seq(10), function(l){
      sapply(seq(48), function(h){bw.nrd(sample(demand[ids_past][seq(h, length(ids_past), by = 48)], n_approx))})
    })
    bw_normal <- max(apply(res, 1, mean))
    
    bandwiths_vec <- seq(min_bandwith, bw_normal, length.out = 5)
    bandwiths_subvec1 <- seq(bandwiths_vec[1], bandwiths_vec[2], length.out = 5)
    bandwiths_subvec2 <- seq(bandwiths_vec[2], bandwiths_vec[3], length.out = 5)
    bandwiths <- c(bandwiths_subvec1, bandwiths_subvec2, bandwiths_vec[-seq(3)])
    #bandwiths <-  seq(10^-4, bw_normal, length.out = 15)
    
    stopifnot(all(bandwiths>0))
  
    nb_futuredays <- length(seq_validation_interval)/48
    
    #lambdas <- c(0.8, 0.90, 0.95, 1)
    lambdas <- c(0.2, 0.4, 0.6, 0.8, 0.90, 0.95, 1)
    
  }else if(task == "testing"){
    stopifnot(length(selected_bandwiths) == 3)
    
    ids_past   <- tail(learn$id, n_past_obs)
    ids_future <- test$id
    
    nb_futuredays <- length(seq_testing_interval)/48
  }else if(task == "insample_info"){
    stopifnot(length(selected_bandwiths) == 3)
    
    ids_past   <- head(learn$id, n_past_obs)
    ids_future <- tail(learn$id, -n_past_obs)
    nb_futuredays <- length(ids_future)/48
  }
  
  results <- vector("list", nb_futuredays)
  
  ic_days <- calendar$periodOfCycle[ids_future][seq(1, length(ids_future), by = 48)]
 
  
  for(id_future_day in seq(1, nb_futuredays)){

    offset_nhours <- (id_future_day - 1) * 48
    
    ids_future_hours <- ids_future[offset_nhours + seq(1, 48)] 
    
    
    if(offset_nhours > 0){
      #ids_past_actual <- c(tail(ids_past, -offset_nhours), head(ids_future, offset_nhours))
      ids_past_actual <- c(ids_past, ids_future)[offset_nhours + seq(n_past_obs)]
    }else{
      ids_past_actual <- ids_past
    }
    
    # if day is IC 1, 2, ou 3 use different bandwiths
    # mybandwith is eiter a vector or a number
    if(task == "testing" || task == "insample_info"){
      ic_day <-  ic_days[id_future_day]
      bandwiths <- selected_bandwiths[ic_day]
      lambdas <- selected_lambdas[ic_day]
    }
    
    #  results[[id_future_day]] <- lapply(ids_future_hours, function(id){kde(id, ids_past_actual, bandwiths, lambda, task)})
    if(length(lambdas) == 1){
      results[[id_future_day]] <-  lapply(ids_future_hours, function(id){kde(id, ids_past_actual, bandwiths, lambdas, task)})
    }else{
      results[[id_future_day]] <- lapply(lambdas, function(lambda){
        lapply(ids_future_hours, function(id){kde(id, ids_past_actual, bandwiths, lambda, task)})
      })
    }
    
  }	
  list(results = results, bandwiths = bandwiths, lambdas = lambdas, ic_days = ic_days)
}

######################################################
kde <- function(id_query, ids_data, bandwiths, lambda, task){
  #print(id_query)
  ####
  if(algo == "KD-U"){
    ids_data_kept <- ids_data
  }else if(grepl("KD-D", algo)){
    ids_data_kept <- ids_data[which(calendar$periodOfDay[ids_data] == calendar$periodOfDay[id_query])]
  }else if(grepl("KD-IC", algo)){
    
    if(calendar$periodOfCycle[id_query] == 1){ # MONDAY TO FRIDAY
      is_selected <- calendar$periodOfCycle[ids_data] == calendar$periodOfCycle[id_query] & 
        calendar$periodOfDay[ids_data] == calendar$periodOfDay[id_query]
    }else{ # SATURDAY AND SUNDAY
      if(calendar$periodOfDay[id_query] == 1){
        is_selected <- (calendar$periodOfCycle[ids_data] == calendar$periodOfCycle[id_query] & 
                          calendar$periodOfDay[ids_data] %in% (calendar$periodOfDay[id_query] + seq(0, 1))) | 
          (calendar$dweek[ids_data] == (calendar$dweek[id_query] - 1) & calendar$periodOfDay[ids_data] == 48)
      }else if(calendar$periodOfDay[id_query] == 48){
        is_selected <- (calendar$periodOfCycle[ids_data] == calendar$periodOfCycle[id_query] & 
                          calendar$periodOfDay[ids_data] %in% (calendar$periodOfDay[id_query] + seq(-1, 0))) | 
          (calendar$dweek[ids_data] == (calendar$dweek[id_query]%%7 + 1) & calendar$periodOfDay[ids_data] == 1)
      }else{
        is_selected <- calendar$periodOfCycle[ids_data] == calendar$periodOfCycle[id_query] & 
          calendar$periodOfDay[ids_data] %in% (calendar$periodOfDay[id_query] + seq(-1, 1))
      }
    }
    
    is_selected <- calendar$periodOfWeek[ids_data] == calendar$periodOfWeek[id_query]
    
    ids_data_kept <- ids_data[which(is_selected)]
    n <- length(ids_data_kept)
    normalized_weights <- rep(1, n)/n
    
    do.weighting <- TRUE
    if(do.weighting){
      weights_all <- lambda^floor((tail(ids_data) - ids_data)/336)
      weights_selected <- weights_all[which(is_selected)]
      normalized_weights <- weights_selected/sum(weights_selected)
    }
  }

  
  x <- demand[ids_data_kept]
  ####
  
  n <- length(x)
  minx <- min(x)
  maxx <- max(x)
  #logx <- log(x)
  
  lower_bound <- 0  #minx
  r <- 1 # 3
  
  #xgrid <- c(seq(from = minx , to = quantile(x, .9), length = 90), seq(from = quantile(x, .91), to = maxx  , length = 10))
  xgrid <- c(0, 
             seq(from = minx , to = quantile(x, .9), length = 190), 
             seq(from = quantile(x, .91), to = maxx, length = 10),
             seq(from = maxx, to = maxx + r* max(bandwiths), length = 3))
  xgrid <- sort(unique(xgrid))
  
  vec_crps <- residuals <- squared_error <- mu_hat <- var_hat <- numeric(length(bandwiths))
  for(i in seq_along(bandwiths)){
    h <- bandwiths[i]
    vech <- rep(h, length(x))
    
    if(mykernel == "normal"){
      
      ids_boundary <- which(x <= (lower_bound + r * h))
      vech[ids_boundary] <- pmax((x[ids_boundary] - lower_bound) / r, min_bandwith) 

    }
    
    cdfs <- sapply(seq(length(x)), function(i){ 	
      xi <- x[i]		
      if(mykernel == "normal"){
        #pnorm((xgrid - obs)/h)/(n)
        #pnorm((xgrid - xi)/vech[i])/(n)
        pnorm((xgrid - xi)/vech[i]) * normalized_weights[i]
      }else if(mykernel == "lognormal"){
        plnorm(xgrid, meanlog = log(xi), sdlog = vech[i], lower.tail = TRUE, log.p = FALSE)/n
      }else if(mykernel == "truncated"){
        ptnorm(xgrid, mean = xi, sd = vech[i], lower = lowerx, upper = upperx, lower.tail = TRUE, log.p = FALSE)/n
      }
    })
    cdf <- rowSums(cdfs)
    
    if(task == "insample_info"){
     
      mycdf <- function(xq){
        sum(sapply(seq(length(x)), function(i){ 	
          xi <- x[i]
          pnorm((xq - xi)/vech[i]) * normalized_weights[i]
        }))
      }
    }
    
    
    # Mean forecasts 
    if(mykernel == "normal"){
      all_mus <- x
    }else if(mykernel == "lognormal"){
      all_mus <- sapply(x, function(xi){ exp(log(xi) + (h^2)/2) })
    }else if(mykernel == "truncated"){
      all_mus <- sapply(x, function(xi){ 	
        alpha <- (lowerx - xi)/h
        beta  <- (upperx - xi)/h
        xi + ((dnorm(alpha) - dnorm(beta))*h) / (pnorm(beta) - pnorm(alpha))	
      })
    }
    #mu_hat[i] <- sum(all_mus)/n
    mu_hat[i] <- sum(all_mus * normalized_weights)
    
    if(mykernel == "normal"){
      #var_hat[i] <- sum(x^2)/n + sum(vech^2)/n - (mu_hat[i])^2
      var_hat[i] <- sum(normalized_weights * ((x - mu_hat[i])^2 + vech^2))
    }
    
    obs <- demand[id_query]
    
    
    if(task == "insample_info"){
      upit <- mycdf(obs)
      #upit <- ecdf(x)(obs)
    }
    
    residuals[i] <- obs - mu_hat[i] 
    squared_error[i] <- (residuals[i])^2 
    
    if(task != "insample_info"){
      if(mykernel == "normal"){
        
        vec_crps[i] <- crps_mixture(x, vech, normalized_weights, obs)
        
      }else{
        invkcdf <- approxfun(cdf, xgrid, rule = 2)
        X1 <- invkcdf(runif(1000))
        vec_crps[i] <- crps_sampling(X1, obs)
        #vec_crps[i] <- mean(abs(X1 - obs)) - 0.5 * mean(abs(X1 - invkcdf(u2)))
      }
    }
    
    if(task != "learning"){
      invcdf <- approxfun(cdf, xgrid, rule = 2)
      q_hat <- invcdf(taus)
    }
    
  }# bandwiths
  
  
  if(task == "learning"){
    ret <- vec_crps #list(crps = crps)
  }else if(task == "testing"){
    ret <- list(crps = vec_crps, squared_error = squared_error, q_hat = q_hat, mu_hat = mu_hat, var_hat = var_hat)
  }else if(task == "insample_info"){
    ret <- list(residuals = residuals, q_hat = q_hat, mu_hat = mu_hat, var_hat = var_hat, upit = upit)
  }else{
    stop("ERROR ...")
  }
  ret
}