# Various functions used in the "results.R" script.

get_mat <- function(measure, do.skill){
  if(grepl("CRPS", measure)){ 
    if(measure == "CRPS"){
      iweight <- 1
    }else if(measure == "CRPS Tails"){
      iweight <- 2
    }else if(measure == "CRPS Right tail"){
      iweight <- 4
    }else if(measure == "CRPS Left tail"){
      iweight <- 5
    }
    res_agg <- wcrps_agg_byhour[iweight, , , ]
    res_bot <- wcrps_bot_byhour[iweight, , , ]
  }else if(measure == "MSE"){
    res_agg <- mse_agg_byhour
    res_bot <- mse_bot_byhour
  }else if(measure == "RMSE"){
    res_agg <- sqrt(mse_agg_byhour)
    res_bot <- sqrt(mse_bot_byhour)
  }else if(measure == "QS"){
    res_agg <- total_qscores_agg
    res_bot <- total_qscores_bot
  }else{
    stop("error")
  }
  
  if(do.skill){
    mat_agg_skill <- sapply(seq_along(agg_methods), function(iaggmethod){
      (res_agg[, match("BASE", agg_methods), ] - res_agg[, iaggmethod, ])/res_agg[, match("BASE", agg_methods), ]
    }, simplify = 'array')
    
    mat_bot_skill <- sapply(seq_along(bot_methods), function(ibotgmethod){
      (res_bot[, match("BASE", bot_methods), ] - res_bot[, ibotgmethod, ])/res_bot[, match("BASE", bot_methods), ]
    }, simplify = 'array')
    
    #browser()
    res_agg <- aperm(mat_agg_skill, c(1, 3, 2))
    res_bot <- aperm(mat_bot_skill, c(1, 3, 2))
  }
  
  list(res_agg = res_agg, res_bot = res_bot)
}

to_aggname <- function(algo){
  if(algo %in% c("NAIVEBU", "PERMBU"))
  {
    res <- "BASE"
  }else if(algo %in% c("PERMBU-MINT", "NAIVEBU-MINT")){
    # res <- "MINTshrink"
    res <- "BASE-MINT"
  }else{
    res <- algo
  } 
  res
}

