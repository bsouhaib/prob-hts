# This script simply merge the results of all Rdata files produced by "aggregation.R".
rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("utils.R")
library(igraph)

load(file.path(work.folder, "myinfo.Rdata"))

node_nbkids <- apply(Sagg, 1, sum)
node_order <- sort(node_nbkids, index = T, decreasing = T)$ix

ntest <- length(test$id)
n_bottom <- length(bottomSeries)

do.twentyfour <- FALSE
if(do.twentyfour){
  tday <- tday[seq(1, 48, 2)]
}

#nbperjob <- 276
#nbperjob <- 69
#njobs <- ntest/nbperjob

#nbperjob <- 123
#njobs <- 36

#nbperjob <- 130
#njobs <- 34

#nbperjob <- 138
#njobs <- 32


nbperjob <- 368
njobs <- 12

leaves <- V(itree)[degree(itree, mode="out") == 0]
agg_nodes <- V(itree)[degree(itree, mode="out") != 0]

depth_aggnodes <- sapply(agg_nodes, function(agg_node){
  vec <- distances(itree, agg_node, leaves, mode = "out")
  max( vec[which(vec!=Inf)])
})

#agg_methods <- c("BASE", "NAIVEBU", "PERMBU", "NAIVEBU-MINT", "PERMBU-MINT")
#color.agg <- c("black", "orange", "darkblue")
#bot_methods <- c("BASE", "BASE-MINT")
#color.bot <- c("black")

#agg_methods <- c("BASE", "NAIVEBU", "PERMBU", "PERMBU-MINT", "PERMBU-MEANCOMB")
#color.agg <- c("grey", "orange", "cyan", "purple", "darkblue")
#bot_methods <- c("BASE", "BASE-MINT", "BASE-MEANCOMB")
#color.bot <- c("black", "purple", "darkblue")

#agg_methods <- c("BASE", "NAIVEBU", "PERMBU")
#color.agg <- c("grey", "orange", "cyan")
#bot_methods <- c("BASE", "BASE-MINT")
#color.bot <- c("black", "purple")

#bot_methods <- c("BASE", "BASE-MINT", "BASE-MCOMB", "BASE-MCOMBRECON")
#color.bot <- c("black", "purple", "darkgreen", "darkblue")
#agg_methods <- c("BASE", "NAIVEBU", "PERMBU", "PERMBU-MINT", "PERMBU-MCOMB", "PERMBU-MCOMBRECON")
#color.agg <- c("grey", "orange", "cyan", "purple", "darkgreen", "darkblue")



##### JASA PAPER
bot_methods <- c("BASE", "BASE-MINT", "MINTdiag", "MINTshrink")
color.bot <- c("black", "purple", "red", "green")

agg_methods <- c("BASE", "NAIVEBU", "PERMBU", "PERMBU-MINT", "NAIVEBU-MINT", "MINTdiag", "MINTshrink")
color.agg <- c("black", "orange", "purple", "cyan", "pink", "red", "green")

pch.agg <- c(8, 0, 2, 2, 0, 1, 3) 
pch.bot <- c(8, 2, 1, 3)
lty.agg <- c(1, 3, 2, 2, 3, 6, 5)
lty.bot <- c(1, 2, 6, 5)


if(FALSE){
bot_methods <- c("BASE", "BASE-MINT", "BASE-MCOMB", "BASE-MCOMBRECON", "PROBMINT")
color.bot <- c("black", "purple", "darkgreen", "darkblue", "green")
agg_methods <- c("BASE", "NAIVEBU", "PERMBU", "PERMBU-MINT", "PERMBU-MCOMB", 
                 "PERMBU-MCOMBRECON", "PERMBU-MCOMBUNRECON", "NAIVEBU-MINT", "PROBMINT")
color.agg <- c("black", "orange", "cyan", "purple", "darkgreen", "darkblue", "red", "pink", "green")

agg_better_names <- c("BASE", "NAIVEBU", "PERMBU", "PERMBU-MINT", "PERMBU-GTOP1", "PERMBU-GTOP2", "PERMBU-COMB")
bot_better_names <- c("BASE", "PERMBU-MINT", "PERMBU-GTOP1", "PERMBU-GTOP2")
}
if(FALSE){
  bot_methods <- c("BASE", "BASE-MINT", "BASE-MCOMB", "BASE-MCOMBRECON")
  color.bot <- c("black", "purple", "darkgreen", "darkblue")
  agg_methods <- c("BASE", "NAIVEBU", "PERMBU", "PERMBU-MINT", "PERMBU-MCOMB", 
                   "PERMBU-MCOMBRECON", "PERMBU-MCOMBUNRECON")
  color.agg <- c("black", "orange", "cyan", "purple", "darkgreen", "darkblue", "red")
  
  agg_better_names <- c("BASE", "NAIVEBU", "PERMBU", "PERMBU-MINT", "PERMBU-GTOP1", "PERMBU-GTOP2", "PERMBU-COMB")
  bot_better_names <- c("BASE", "PERMBU-MINT", "PERMBU-GTOP1", "PERMBU-GTOP2")
}



wcrps_agg    <- array(NA, c(5, n_agg, ntest, length(agg_methods)))
crps_agg    <- array(NA, c(n_agg, ntest, length(agg_methods)))

wcrps_bottom <- array(NA, c(5, n_bottom, ntest, length(bot_methods)))
crps_bottom <- array(NA, c(n_bottom, ntest, length(bot_methods)))

mse_agg    <- array(NA, c(n_agg, ntest, length(agg_methods)))
mse_bottom <- array(NA, c(n_bottom, ntest, length(bot_methods)))

total_qscores_agg <- total_qscores_bot <- 0

for(idjob in seq(njobs)){
  print(idjob)
  allidtest <- (idjob - 1) * nbperjob + seq(nbperjob) 
  
  if(nbperjob == 123 && idjob == 36){
     allidtest <- 4306:4416
    #allidtest <- 4291:4416
  }
  
    res_job <- file.path(loss.folder, paste("results_HTS_", algo.agg, "_", algo.bottom, "_", idjob, ".Rdata", sep = "")) 
    load(res_job)
  
  # crps agg
  list_crps_agg_nonull <- list_crps_agg[-which(sapply(list_crps_agg, is.null))]
  mat_crps_agg <- sapply(seq_along(list_crps_agg_nonull),  function(i){list_crps_agg_nonull[[i]]}, simplify = 'array')
  
  # wcrps agg
  list_wcrps_agg_nonull <- list_wcrps_agg[-which(sapply(list_wcrps_agg, is.null))]
  mat_wcrps_agg <- sapply(seq_along(list_wcrps_agg_nonull),  function(i){list_wcrps_agg_nonull[[i]]}, simplify = 'array')
  
  # wcrps bot
  list_wcrps_bot_nonull <- list_wcrps_bot[-which(sapply(list_wcrps_bot, is.null))]
  mat_wcrps_bot <- sapply(seq_along(list_wcrps_bot_nonull),  function(i){list_wcrps_bot_nonull[[i]]}, simplify = 'array')

  # crps bot
  list_crps_bot_nonull <- list_crps_bot[-which(sapply(list_crps_bot, is.null))]
  mat_crps_bot <- sapply(seq_along(list_crps_bot_nonull),  function(i){list_crps_bot_nonull[[i]]}, simplify = 'array')
  
  
  #
  crps_bottom[, allidtest, ] <- aperm(mat_crps_bot, c(1, 3, 2))
  wcrps_bottom[, , allidtest, ] <- aperm(mat_wcrps_bot, c(1, 2, 4, 3))
  
  crps_agg[, allidtest,]     <- aperm(mat_crps_agg, c(1, 3, 2))
  wcrps_agg[, , allidtest,]    <- aperm(mat_wcrps_agg, c(1, 2, 4, 3))
  
  
  total_qscores_agg <- total_qscores_agg + avg_qscores_agg
  total_qscores_bot <- total_qscores_bot + avg_qscores_bot

  list_mse_agg_nonull <- list_mse_agg[-which(sapply(list_mse_agg, is.null))]
  mat_mse_agg <- sapply(seq_along(list_mse_agg_nonull),  function(i){list_mse_agg_nonull[[i]]}, simplify = 'array')
  list_mse_bot_nonull <- list_mse_bot[-which(sapply(list_mse_bot, is.null))]
  mat_mse_bot <- sapply(seq_along(list_mse_bot_nonull),  function(i){list_mse_bot_nonull[[i]]}, simplify = 'array')
  
  mse_agg[, allidtest,]     <- aperm(mat_mse_agg, c(1, 3, 2))
  mse_bottom[, allidtest,]     <- aperm(mat_mse_bot, c(1, 3, 2))
}

total_qscores_agg <- total_qscores_agg / njobs
total_qscores_bot <- total_qscores_bot / njobs


# crps_agg   total_qscores_agg
# crps_bottom total_qscores_bot

# AGG MSE
mse_agg_byhour <- sapply(seq(n_agg), function(iagg){
  sapply(seq_along(agg_methods), function(imethod){
    res <- apply(matrix(mse_agg[iagg, , imethod], ncol = 48, byrow = T), 2, mean)
    
    if(do.twentyfour){
      res <- sapply(seq(1, 48, by = 2), function(i){
        mean(res[seq(i, i+1)])
      })
    }
    res
  })
}, simplify = 'array')

# BOT MSE
mse_bot_byhour <- sapply(seq(n_bottom), function(ibot){
  sapply(seq_along(bot_methods), function(imethod){
    res <- apply(matrix(mse_bottom[ibot, , imethod], ncol = 48, byrow = T), 2, mean)
    if(do.twentyfour){
      res <- sapply(seq(1, 48, by = 2), function(i){
        mean(res[seq(i, i+1)])
      })
    }
    res
  })
}, simplify = 'array')

# BOT CRPS
crps_bot_byhour <- sapply(seq(n_bottom), function(ibot){
  sapply(seq_along(bot_methods), function(imethod){
    res <- apply(matrix(crps_bottom[ibot, , imethod], ncol = 48, byrow = T), 2, mean)
    if(do.twentyfour){
      res <- sapply(seq(1, 48, by = 2), function(i){
        mean(res[seq(i, i+1)])
      })
    }
    res
  })
}, simplify = 'array')

# AGG CRPS
crps_agg_byhour <- sapply(seq(n_agg), function(iagg){
  sapply(seq_along(agg_methods), function(imethod){
    res <- apply(matrix(crps_agg[iagg, , imethod], ncol = 48, byrow = T), 2, mean)
    if(do.twentyfour){
      res <- sapply(seq(1, 48, by = 2), function(i){
        mean(res[seq(i, i+1)])
      })
    }
    res
  })
}, simplify = 'array')

# AGG WCRPS
wcrps_agg_byhour <- sapply(seq(5), function(iweight){
  sapply(seq(n_agg), function(iagg){
    sapply(seq_along(agg_methods), function(imethod){
      res <- apply(matrix(wcrps_agg[iweight, iagg, , imethod], ncol = 48, byrow = T), 2, mean)
      if(do.twentyfour){
        res <- sapply(seq(1, 48, by = 2), function(i){
          mean(res[seq(i, i+1)])
        })
      }
      res
    })
  }, simplify = 'array')
}, simplify = 'array')

wcrps_agg_byhour <- aperm(wcrps_agg_byhour, c(4, 1, 2, 3))

# BOT WCRPS
wcrps_bot_byhour <- sapply(seq(5), function(iweight){
  sapply(seq(n_bottom), function(ibot){
    sapply(seq_along(bot_methods), function(imethod){
      res <- apply(matrix(wcrps_bottom[iweight, ibot, , imethod], ncol = 48, byrow = T), 2, mean)
      if(do.twentyfour){
        res <- sapply(seq(1, 48, by = 2), function(i){
          mean(res[seq(i, i+1)])
        })
      }
      res
    })
  }, simplify = 'array')
}, simplify = 'array')

wcrps_bot_byhour <- aperm(wcrps_bot_byhour, c(4, 1, 2, 3))


res_info <- getInfoNode("nb_nodes")
#res_info <- getInfoNode("kwh")
agg_nodes_order <- sort(res_info$info_nodes_agg, index = T, decreasing = T)$ix
bot_nodes_order <- sort(res_info$info_nodes_bottom, index = T, decreasing = T)$ix


