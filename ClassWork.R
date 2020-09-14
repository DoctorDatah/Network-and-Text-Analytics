
library(igraph)
library(igraphdata)
library(readxl)


gd <- graph_from_literal(A:B:C:D:1:2:3:4:5, A-1, A-4,B-2,B-3, B-4, B-1, C-2,C-3,C-5,D-3,D-4,D,5) 
set.seed(23);plot(gd,main="Digraph")

V(gd)
V(gd)$type=c(F,F,F,F,T,T,T,T,T)

vertex_attr(gd)
vertex_attr_names(gd)


igraph_options(vertex.size=40,annotate.plot=F)
set.seed(23);plot(gd)
plot(gd, layout=layout_as_bipartite)

as_incidence_matrix(gd)
(proj <- bipartite_projection(gd))
par(mfrow=c(1,3))
set.seed(23);plot(gd, layout=layout_as_bipartite,main="Bipartite Network")
plot(proj[[1]],main="Projection onto Alpa")
plot(proj[[2]],main="Projection onto Numbers")


# Comonents problem
matrix()
x = cbind(c(2,5,3,8),c(0,0,2,1),c(0,5,3,2),c(0,3,0,0))
colnames(x) <- c("C1","C2","C3")
