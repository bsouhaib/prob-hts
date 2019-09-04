# This script produces the figure which shows the hierarchy in the form of a circular graph.
rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("utils.R")
library(igraph)
library(Matrix)

load(file.path(work.folder, "myinfo.Rdata"))

#savepdf(file.path(results.folder, paste("hierarchy", sep = "") ))
#plot(itree, layout = 
#       layout.reingold.tilford(itree, root=1, circular=T), vertex.size=0, vertex.label=NA, edge.arrow.size=0)
#dev.off()

g <- itree
v_names <- names(V(g))
v_names[seq(2, length(v_names))] <- substr(v_names[seq(2, length(v_names))], 4, 7)
v_names[seq(56, length(v_names))] <- ""
g <- set.vertex.attribute(g, "name", value=v_names)

savepdf(file.path(results.folder, paste("hierarchy-plot", sep = "") ))

#plot(g, layout = layout.reingold.tilford(g, root=1, circular=T), vertex.size=0, edge.arrow.size=0, vertex.label.cex = .7, 
#     vertex.label.dist=.3, vertex.label.degree = .30)
#myvsize <- c(apply(Sagg, 1, sum), rep(0, ncol(Sagg)))
#plot(g, layout = layout.reingold.tilford(g, root=1, circular=T), vertex.size=myvsize/90, edge.arrow.size=0, vertex.label.cex = .7)

#myvsize <- log(c(apply(Sagg, 1, sum), rep(1, ncol(Sagg))))
myvsize <- c(apply(Sagg, 1, sum), rep(1, ncol(Sagg)))/56

plot(g, layout = layout.reingold.tilford(g, root=1, circular=T), 
     vertex.size=myvsize, edge.arrow.size=0, vertex.label= NA, vertex.color = "white")

endpdf()

#myvsize <- apply(Sagg, 1, sum)/60
#res <- getInfoNode("kwh")
res <- getInfoNode("nb_nodes")
newg <- delete_vertices(g, seq(56, length(V(g))))
V(newg)$color <- "white"

myvsize <- res$info_nodes_agg/15
savepdf(file.path(results.folder, paste("hierarchy-plot-size", sep = "") ))
plot(newg, layout = layout.reingold.tilford(newg, root=1), vertex.size = myvsize, vertex.label= NA, edge.arrow.size=0)
endpdf()

myvsize <- sqrt(res$info_nodes_agg)
savepdf(file.path(results.folder, paste("hierarchy-plot-sqrtsize", sep = "") ))
plot(newg, layout = layout.reingold.tilford(newg, root=1), vertex.size = myvsize, vertex.label= NA, edge.arrow.size=0)
endpdf()


myvsize <- res$info_nodes_agg/20
savepdf(file.path(results.folder, paste("hierarchy-plot-size", sep = "") ) )
plot(newg, layout = layout_as_tree(newg, root=1), vertex.size = myvsize, vertex.label= NA, edge.arrow.size=0)
endpdf()

#myvsize <- res$info_nodes_agg/20
#plot(newg, layout = layout.reingold.tilford(newg, root=1, circular=T), vertex.size = myvsize, vertex.label= NA, edge.arrow.size=0)

