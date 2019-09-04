# This script produces the table showing the values for the different parameters of the exponential smoothing methods.
rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("utils.R")
library(Hmisc)

load(file.path(work.folder, "myinfo.Rdata"))


node_nbkids <- apply(Sagg, 1, sum)
node_order <- sort(node_nbkids, index = T, decreasing = T)$ix

algo <- "DETS"

MAT <- matrix(NA, nrow = n_agg, ncol = 4)
for(i in seq_along(aggSeries)){
  idseries <- aggSeries[i]
  param_file <- file.path(basef.folder, algo, paste("parameters_", idseries, "_", algo, ".Rdata", sep = "")) 
  if(file.exists(param_file)){
    load(param_file)
    MAT[i, ] <- res_optim$par
  }else{
    print(paste("file does not exist for ", i, sep = ""))
  }
}
MAT <- MAT[node_order, ]

savepdf(file.path(results.folder, paste("DETS", sep = "")))
par(mfrow = c(2, 2))
xlab <- "aggregate series"
plot(MAT[, 1], xlab = xlab, ylab = expression(phi))
plot(MAT[, 2], xlab = xlab, ylab = expression(alpha))
plot(MAT[, 3], xlab = xlab, ylab = expression(delta))
plot(MAT[, 4], xlab = xlab, ylab = expression(omega))
endpdf()


savepdf(file.path(results.folder, paste("DETS", sep = "")))
par(mfrow = c(2, 2))
xlab <- "number of aggregated meters (log scale) "
for(j in seq(4)){
  if(j == 1){
    my_ylab <- expression(phi)
  }else if(j == 2){
    my_ylab <- expression(alpha)
  }else if(j == 3){
    my_ylab <- expression(delta)
  }else if(j == 4){
    my_ylab <- expression(omega)
  }
  plot(log(node_nbkids), MAT[, j], xlab = xlab, ylab = my_ylab)
}
endpdf()

id <- c(1, 7, 39, 50)
#id <- c(1, 3, 5, 7, 11, 25, 31, 37, 46, 50)
nb <- apply(Sagg, 1, sum)[node_order[id]]

#MAT_toprint <- data.frame(t(MAT)[, id])
#greeks <- c(phi = "$\\phi$", alpha="$\\alpha$", delta = "$\\delta$", omega = "$\\omega$")

MAT_toprint <- data.frame(t(MAT)[c(2, 3, 4, 1), id])
greeks <- c(alpha="$\\alpha$", delta = "$\\delta$", omega = "$\\omega$", phi = "$\\phi$")

row.names(MAT_toprint) <- greeks
colnames(MAT_toprint) <- nb
MAT_toprint <- format.df(MAT_toprint, dec = 3)

latex(MAT_toprint, file= file.path(results.folder, paste("TABLE_DETS.tex")), 
             title = "", cgroup = "Number of aggregated meters", label = "tab:param_dets", 
      caption = "Parameters of the exponential smoothing method at different levels of aggregation.")


MAT_bandwiths <- MAT_decay <-  matrix(NA, nrow = n_bottom, ncol = 3)

algo <- "KD-IC-NML"
for(i in seq_along(bottomSeries)){
  print(i)
  idseries <- bottomSeries[i]
  param_file <- file.path(basef.folder, algo, paste("parameters_", idseries, "_", algo, ".Rdata", sep = "")) 

  if(file.exists(param_file)){
    load(param_file)
    MAT_bandwiths[i, ] <- selected_bandwiths_ic
    MAT_decay[i, ]     <- selected_lambdas_ic
  }else{
    print(paste("file does not exist for ", i, sep = ""))
  }
}

