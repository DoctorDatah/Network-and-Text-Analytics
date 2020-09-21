#### HW 3 ####
########################################################################
library(igraph)
library(igraphdata)
library(readxl)

parSave=par(no.readonly = TRUE)
iSave=igraph_options(annotate.plot=T,vertex.size=15,edge.arrow.size=.5)
#igraph_options(iSave)  #Back to the old values  

PATH = "C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/"

########################################################################
# Question 1
# Creating Graph
g31 = graph_from_literal(3-C-1-B,3-A-2,3-D)
V(g31)
V(g31)$type = c(T,F,T,F,F,T,F)
vertex_attr(g31)
vertex_attr_names(g31)

# a
igraph_options(vertex.size=40,annotate.plot=F)
set.seed(23);plot(g31)
plot(g31, layout=layout_as_bipartite)

igraph_options(iSave)    
par(parSave)

# b
summary(V(g31)$type) 

# c
as_incidence_matrix(g31)

# d
(proj <- bipartite_projection(g31))
par(mfrow=c(1,3))
set.seed(23);plot(g31, layout=layout_as_bipartite,main="Bipartite Network")
plot(proj[[1]],main="Projection onto Letters")
plot(proj[[2]],main="Projection onto Numbers")
par(parSave)

diameter(proj[[1]]) # 2

# e
set.seed(23);plot(proj[[2]]) # Yes

########################################################################
# Question 2
#Graph from text files
laz_edges=read.table("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/lazega_edges.txt")
laz_vertics=read.table("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/lazega_vertices.txt",header=T)
laz_attrs=read.table("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/lazega_graph.txt",header=T)

lazega_g = graph_from_data_frame(laz_edges,F,laz_vertics)
(lazega_g = graph_attr_from_df(lazega_g,laz_attrs))

# a
set.seed(23);plot(lazega_g)

# b 
is_connected(lazega_g,mode="weak") # F
is_connected(lazega_g,mode="strong") # F

# c
(co=components(lazega_g,mode="strong")) #Three components
set.seed(23);plot(lazega_g, vertex.color=co$membership,palette=rainbow(10))

max(co$csize) # 34
    
     

