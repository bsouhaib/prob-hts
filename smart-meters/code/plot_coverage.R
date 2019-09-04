# This script produces the figure which shows the coverage of the 50% and 90% prediction intervals.
# The coverage were computed in aggregation.R
rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("utils.R")

load(file.path(work.folder, "myinfo.Rdata"))

algo.agg <- "DETS"
algo.bottom <- "KD-IC-NML"

  all_coverage_bot <- all_coverage_agg <- 0
  
  all_idtest <- seq(4416)
  #  all_idtest <- c(seq(1, 3000), seq(3400, 4416))
  
  for(idtest in all_idtest){
    if(idtest%%100==0)
      print(idtest)
    coverage_idtest <- file.path(coverage.folder, paste("coverage_", algo.agg, "_", algo.bottom, "_", idtest, ".Rdata", sep = "")) 
    load(coverage_idtest)
    # c("coverage_bot", "coverage_agg") #  2 1578    4
    all_coverage_bot <- all_coverage_bot + coverage_bot
    all_coverage_agg <- all_coverage_agg + coverage_agg
  }
  all_coverage_bot <- all_coverage_bot / length(all_idtest)
  all_coverage_agg <- all_coverage_agg / length(all_idtest)
  
  
  bot_methods <- c("BASE", "BASE-MINT", "MINTdiag", "MINTshrink")
  bot_colors <- c("grey", "purple", "red", "green")
  
  agg_methods <- c("BASE", "NAIVEBU", "PERMBU", "PERMBU-MINT", "NAIVEBU-MINT", "MINTdiag", "MINTshrink")
  agg_colors <- c("grey", "orange", "purple", "cyan", "pink", "red", "green")
  agg_colors[match(c("PERMBU-MINT", "NAIVEBU-MINT"), agg_methods)] <- agg_colors[match(c("PERMBU", "NAIVEBU"), agg_methods)]
  # I have change BASE color from black to grey
  
  
  #agg_newnames <- c("BASE", "IndepBU", "DepBU", "DepBU-MinTShrink", "IndepBU-MinTShrink", "LogN-MinTDiag", "LogN-MinTShrink")
  agg_newnames <- c("BASE", "IndepBU- \n NoMinT", "DepBU- \n NoMinT", 
                    "DepBU- \n MinTShrink", "IndepBU- \n MinTShrink", "LogN- \n MinTDiag", "LogN- \n MinTShrink")
  
  #c("BASE", "BASE-MINT", "MINTdiag", "MINTshrink")
  #c("BASE", "BASE", "BASE", "BASE-MINT", "BASE-MINT", "MINTdiag", "MINTshrink")
  id_matching <- c(1, 1, 1, 2, 2, 3, 4)
  
  myorder <- c(6, 7, 1, 3, 4, 2, 5)
    
  savepdf(file.path(results.folder, "coverage"), height = 6)
  par(mfrow = c(1, 2))
  for(i in 1:2){
    if(i == 1){
      cov90 <- all_coverage_agg[2, , ]
      cov50 <- all_coverage_agg[1, , ]
      mycolors <- agg_colors
    }else{
      cov90 <- all_coverage_bot[2, , id_matching]
      cov50 <- all_coverage_bot[1, , id_matching]
      mycolors <- bot_colors[id_matching]
    }
    colnames(cov90) <- agg_newnames
    colnames(cov50) <- agg_newnames
    
    # changer order
    cov90 <- cov90[, myorder]
    cov50 <- cov50[, myorder]
    mycolors <- mycolors[myorder]
    
    boxplot(cov50, outline = F, ylim = c(0.2, 1), las = 2, cex.axis = .6, yaxt = 'n', col = mycolors,
            ylab = "Coverage rate")
    axis(2, at = c(0.5, 0.9), cex.axis = 0.6, label = c("50%", "90%"))
    
    boxplot(cov90, outline = F, ylim = c(0.2, 1), , las = 2, cex.axis = .6, add = T, yaxt = 'n', col = mycolors)
    abline(h = c(0.5, 0.9), lwd = 0.1)
    
  }
  dev.off()
