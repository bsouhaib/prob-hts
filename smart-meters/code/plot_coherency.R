# This script produces the figure which shows the incoherency errors.
rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("utils.R")
source("multiplot.R")

library(reshape2)
library(ggplot2)

nhours <- 48

gplot_matrix <- function(X, my_title = ""){
  nbrow <- nrow(X)
  Z <- melt(t(X))
  Zp <- Z
  Zp$Var2 <- nbrow + 1 - Zp$Var2
  
  #mybreaks <- seq(1, nhours, by = 6) 
  mybreaks <- c(1, seq(8, nhours, by = 8)) 
  #mybreaks <- c(1, seq(33, nhours, by = 48))
  mylabels <- rep(tday, nhours/48)[mybreaks]
  
  p <- ggplot(data = Zp, aes(x = Var1, y = Var2)) + 
    geom_tile(aes(fill = value), colour = "white") + 
    #scale_fill_gradient2(name = expression(log(abs(tilde(epsilon)))), low="darkblue", high="darkgreen", guide="colorbar")+
    scale_fill_gradient2(name = "", low="white", high="black", guide="colorbar")+
    scale_x_continuous(breaks = mybreaks, labels = mylabels) +
    xlab("Hour of the day") + 
    ylab("Aggregate series") +
    theme_bw() +
    ggtitle(my_title)
  
  #  geom_vline(xintercept = seq(1, nhours, by = 48), linetype=3) +
  
  p
} 

load(file.path(work.folder, "myinfo.Rdata"))

rank <- sort(apply(Sagg, 1, sum), index = T, decreasing = T)$ix

node_nbkids <- apply(Sagg, 1, sum)
node_order <- sort(node_nbkids, index = T, decreasing = T)$ix

algo.agg <- "DETS"
algo.bottom <- "KD-IC-NML"

ntotal <- n_agg + n_bottom

meanf <- matrix(NA, nrow = ntotal, ncol = 4416)

prob_coherency <- prob_coherency_10 <- prob_coherency_50 <- prob_coherency_90 <- matrix(NA, nrow = n_agg, ncol = 48)
allres <- vector("list", 48)
allres[1:48] <- 0

for(idtest in seq(4416)){
  print(idtest)
  
  res_byidtest_file <- file.path(work.folder, "byidtest", paste("results_byidtest_", algo.agg, "_", algo.bottom, "_", idtest, ".Rdata", sep = "")) 
  load(res_byidtest_file)
  # "QF_agg_idtest", "QF_bottom_idtest", "obs_agg_idtest", "obs_bottom_idtest", 
  # "revisedmean_bottom_idtest", 'revisedmean_agg_idtest', "mean_agg_idtest", "mean_bottom_idtest"

  meanf[seq(1, n_agg), idtest] <- mean_agg_idtest
  meanf[seq(n_agg + 1, n_agg + n_bottom), idtest] <- mean_bottom_idtest
  
  if(FALSE){
    base_samples_agg <- matrix(NA, nrow = M, ncol = n_agg)
    base_samples_bottom <- matrix(NA, nrow = M, ncol = n_bottom)
    
    for(j in seq(n_agg)){
      invcdf <- approxfun(taus, QF_agg_idtest[, j], rule = 2)
      base_samples_agg[, j]    <- invcdf(q_probs)
    }
    
    for(j in seq(n_bottom)){
      invcdf <- approxfun(taus, QF_bottom_idtest[, j], rule = 2)
      base_samples_bottom[, j] <- invcdf(q_probs)
    }
    
    sum_agg <- t(Sagg %*% t(base_samples_bottom))
    q_sum_agg <- apply(sum_agg, 2, sort)
    q_agg <- apply(base_samples_agg, 2, sort)
    
    res <- (q_sum_agg - q_agg)^2

    hour <- idtest%%48
    if(hour == 0)
      hour <- 48
    
    allres[[hour]] <- allres[[hour]] + res
    
    #prob_coherency[, idtest] <- apply(res, 2, mean)
    #prob_coherency_10[, idtest] <- res[which.min(abs(q_probs - 0.1)), ]
    #prob_coherency_50[, idtest] <- res[which.min(abs(q_probs - 0.5)), ]
    #prob_coherency_90[, idtest] <- res[which.min(abs(q_probs - 0.9)), ]
    
  }
  
}

meanres <- lapply(allres, function(mat){
  mat/92
})

stop("done")




listp <- vector("list", 8)

for(p in seq(8)){
  weights <- numeric(M)
  if(p == 1){
    weights[which.min(abs(q_probs - 0.05))] <- 1
    my_title <- "0.05"
  }else if(p == 2){
    weights[which.min(abs(q_probs - 0.25))] <- 1
    my_title <- "0.25"
  }else if(p == 3){
    weights[which.min(abs(q_probs - 0.5))] <- 1
    my_title <- "0.5"
  }else if(p == 4){
    weights[which.min(abs(q_probs - 0.75))] <- 1
    my_title <- "0.75"
  }else if(p == 5){
    weights[which.min(abs(q_probs - 0.95))] <- 1
    my_title <- "0.95"
  }else if(p == 6){
    #weights[which(q_probs <= 0.05 | q_probs >= 0.95)] <- 1
    #my_title <- "both tails"
    
    weights[which(q_probs <= 0.05 | q_probs >= 0)] <- 1
    my_title <- "lower tails"
    
  }else if(p == 7){
    weights[which(q_probs > 0.05 | q_probs < 0.9)] <- 1
    my_title <- "center"
  }else  if(p == 8){
    weights[seq(M)] <- 1
    my_title <- "all"
  }
  weights <- weights/sum(weights)
  
  res <- sapply(meanres, function(mat){
    t(weights) %*% mat
  })
  
  listp[[p]] <-  gplot_matrix(log(res[rank, ]), my_title)
}
multiplot(listp[[1]], listp[[2]], listp[[3]], listp[[4]], listp[[5]], listp[[6]] , listp[[7]], listp[[8]], cols=3)

#####
savepdf(file.path(results.folder, paste("prob-coherency", sep = "") ))
print(listp[[8]])
endpdf()
#####

do.relative <- FALSE
#####
if(do.relative){
  r <- (meanf[seq(1, n_agg), ] - Sagg %*% meanf[seq(n_agg + 1, n_agg + n_bottom), ])/meanf[seq(1, n_agg), ]
  #r <- (meanf[seq(1, n_agg), ] - Sagg %*% meanf[seq(n_agg + 1, n_agg + n_bottom), ])/Sagg %*% meanf[seq(n_agg + 1, n_agg + n_bottom), ]
}else{
  r <- meanf[seq(1, n_agg), ] - Sagg %*% meanf[seq(n_agg + 1, n_agg + n_bottom), ]
}

#r <- r[rank, ]
#r <- r[node_order, ]
#X <- log(abs(r[, seq(48)]))

v <- sapply(node_order, function(iagg){
  #log(abs(matrix(r[iagg, ], ncol = 48, byrow = T)))
  #abs(matrix(r[iagg, ], ncol = 48, byrow = T))
  matrix(r[iagg, ], ncol = 48, byrow = T)
}, simplify = "array")


#########
itday <- c(1, seq(12, 48, by = 12))
id <- c(1, 7, 39, 50)
id <- c(1, 7)

savepdf(file.path(results.folder, paste("mean-coherency", sep = "") ))
#par(mfrow = c(2, 2))
par(mfrow = c(1, 2))
for(iagg in node_order[id]){
  ind <- which(Sagg[iagg, ] == 1)
  boxplot(v[, , iagg], xaxt = "n", main = paste(length(ind), " aggregated meters" , sep = ""), ylab = "Coherency errors", xlab = "Time of day")
  axis(1, labels = tday[itday], at = itday, cex = .8)
  abline(h = 0)
}
endpdf()
#########

v <- aperm(v, c(1, 3, 2))
X <- apply(v, c(2, 3), mean)


if(do.relative){
  #X <- abs(X)
}else{
  X <- log(abs(X))
}



savepdf(file.path(results.folder, paste("mean-coherency", sep = "") ))
print(gplot_matrix(X))
endpdf()


MAT <- log(1 + abs(X))
savepdf(file.path(results.folder, paste("mean-coherency", sep = "") ))
print(gplot_matrix(MAT))
endpdf()
#####


p1 <- gplot_matrix(log(prob_coherency[rank, ]))
p2 <- gplot_matrix(log(prob_coherency_10[rank, ]))
p3 <- gplot_matrix(log(prob_coherency_50[rank, ]))
p4 <- gplot_matrix(log(prob_coherency_90[rank, ]))
multiplot(p1, p2, p3, p4, cols=2)





