# This script computes the permutations to apply to the samples from the marginals 
# in order to restore the dependences among the variables.
rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("utils.R")
#library(parallel)
library(igraph)

load(file.path(work.folder, "myinfo.Rdata"))

# compute the parsing order of the aggregate nodes
leaves <- V(itree)[degree(itree, mode="out") == 0]
agg_nodes <- V(itree)[degree(itree, mode="out") != 0]

list_matpermutations <- list_vecties <- vector("list", length(agg_nodes))

for(inode in seq_along(agg_nodes)){
  agg_node <- agg_nodes[inode]
  idseries_agg <- names(agg_node)
  children_nodes <- ego(itree, order = 1, nodes = agg_node, mode = "out")[[1]][-1]
  print(children_nodes)
    
    mat_residuals <- sapply(seq_along(children_nodes), function(inode){
      child_node <- children_nodes[inode]
      isbottom <- (child_node %in% leaves)
      idseries <- names(child_node)
      if(isbottom){
        resid_file <- file.path(insample.folder, algo.bottom, paste("residuals_", idseries, "_", algo.bottom, ".Rdata", sep = "")) 
        load(resid_file)
        e_residuals <- c(rep(NA, n_past_obs_kd), e_residuals)
      }else{
        resid_file <- file.path(insample.folder, algo.agg, paste("residuals_", idseries, "_", algo.agg, "_", 1, ".Rdata", sep = "")) 
        load(resid_file)
        #e_residuals
      }
      e_residuals
    })
    
    # remove the first few rows (not available for KD)
    mat_residuals <- tail(mat_residuals, -n_past_obs_kd)
    
    #hour_alli <- getInfo(tail(learn$id, -n_past_obs_kd))$hour
    id_all <- tail(learn$id, -n_past_obs_kd)
    
    # compute ranks by time of day
    n_resid <- nrow(mat_residuals)
    stopifnot(n_resid %% 48 == 0)
    
    vec_ties <- sapply(seq(ncol(mat_residuals)), function(j){
      (nrow(mat_residuals) - length(unique(mat_residuals[, j]))) / nrow(mat_residuals)
    }) * 100
    
    mat_residuals <- tail(mat_residuals, M) # 5760 instead of 15168
    mat_permutations <- apply(mat_residuals, 2, rank, ties.method = "random")
    colnames(mat_permutations) <- names(children_nodes)

    list_matpermutations[[inode]] <- mat_permutations
    list_vecties[[inode]] <- vec_ties
    #perm_file <- file.path(permutations.folder, paste("perm_", algo.agg, "_", algo.bottom, "_", idseries_agg, ".Rdata", sep = "")) 
    #save(file = perm_file, list = c("mat_permutations", "vec_ties"))
}

list_matpermutations <- setNames(list_matpermutations, names(agg_nodes))
list_vecties         <- setNames(list_vecties, names(agg_nodes))
perm_file <- file.path(permutations.folder, paste("perm_", algo.agg, "_", algo.bottom, ".Rdata", sep = "")) 
save(file = perm_file, list = c("list_matpermutations", "list_vecties"))
