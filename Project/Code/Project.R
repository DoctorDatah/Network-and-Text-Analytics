###################################################################################
#
# Project - Linkedin Connections
#
###################################################################################
# External Functions
###################################################################################
library(igraph)
library(igraphdata)
library(readxl)
###################################################################################
# Internal 

###################################################################################
graph_attr_from_df<-function(g,df) {
  df=as.data.frame(df)
  for (i in 1:ncol(df)) {
    if (class(df[,i])=="factor") df[,i]=as.character(df[,i])
    g=set_graph_attr(g,names(df)[i],df[1,i])
  }
  g
}

getSubs<-function(g,split) {
  if (class(g)!="igraph") stop("Must input a graph")
  if (length(split)!=gorder(g)) stop("split must have a value for each vertex")
  split=as.factor(split)
  subs=list()
  for (i in 1:length(levels(split))) {
    subs[[i]]=induced_subgraph(g,which(as.numeric(split)==i))
  }
  subs
}
###################################################################################
# Save the environment 
###################################################################################
parSave=par(no.readonly = TRUE)
iSave=igraph_options(annotate.plot=F,vertex.size=15,edge.arrow.size=.5)
#options(stringsAsFactors = F)
#igraph_options(iSave)  #Back to the old values 
#par(parSave)
###################################################################################
# Processing 
###################################################################################

edges=read.table("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Project/Data/2. Modified/edges.txt")
vts=read.csv("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Project/Data/2. Modified/vts.csv",header=T)
lazg=read.table("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Project/Data/2. Modified/lazega_graph.txt",header=T)
data =graph_from_data_frame(edges,F,vts)
(glo=graph_attr_from_df(data,lazg))

###################################################################################
# EDA
###################################################################################
V(glo)$name = V(glo)$First.Name
gl = glo
summary(gl)

###################################################################################
# Description
###################################################################################

graph_attr(gl)

###################################################################################
# Order and Size
###################################################################################

gorder(gl) # 36
gsize(gl) # 88


###################################################################################
# Visualization 
###################################################################################

# Gender Shape
gen=as.numeric(factor(V(gl)$Gender))
V(gl)$shape=c("square","circle")[gen]

# Positinos Colors
length(unique(V(gl)$Position)) # 10
po = factor(V(gl)$Position)
posit=as.numeric(po)
ccol=c("red","green","lightblue","yellow","chartreuse4","blue","darkslateblue","forestgreen","darkgrey","darkorange")[posit]
V(gl)$color = ccol

set.seed(23);plot(gl, layout=layout_with_fr, xlim=c(-.8,.8),ylim=c(-.8,.8), vertex.label.dist=2)
legend("topleft",unique(sort(V(gl)$Position)),
       col=c("red","green","lightblue","yellow","chartreuse4","blue","darkslateblue","forestgreen","darkgrey","darkorange"),
       pch = 19,
       cex=0.55)
legend("topright",c("male","female"),pch=c(19,15) ,cex=0.7)


set.seed(23);plot(gl, layout=layout_with_kk, xlim=c(-.8,.8),ylim=c(-.8,.8), vertex.label.dist=2)
set.seed(23);plot(gl, layout=layout_with_fr, xlim=c(-.8,.8),ylim=c(-.8,.8), vertex.label.dist=2)

set.seed(23);plot(gl, layout=layout_as_tree, xlim=c(-.8,.8),ylim=c(-.8,.8), vertex.label.dist=2)
set.seed(23);plot(gl, layout=layout_as_star, xlim=c(-.8,.8),ylim=c(-.8,.8), vertex.label.dist=2)


# Looking various ego graphs
e = make_ego_graph(gl)
# 4 -> Suleman -> analytics Manager 
plot(e[[4]], vertex.label.dist=2)
# Founder and CEO Raja
plot(e[[31]], vertex.label.dist=2)
# Director Machine Learning Amit
plot(e[[29]], vertex.label.dist=2)
# Data Eng Usman
plot(e[[28]], vertex.label.dist=2)


###################################################################################
# Type of network
###################################################################################

# Graph is considered complate if its density is 1
edge_density(gl) #  0.1365079
# it is sparse network

# Connected
is_connected(gl) # True

###################################################################################
# k - cores
###################################################################################

# A k-core is a subgraph for which all vertex degrees are at least 𝑘 and is maximal, 
# i.e. not a subgraph of a larger such subgraph. The igraph function coreness gives the largest core each vertex belongs to.

# k-core
coreness(gl)

#4-core of the graph gl
par(mfrow=c(1,2))
set.seed(23);plot(gl, layout=layout_with_kk, xlim=c(-.8,.8),ylim=c(-.8,.8), vertex.label.dist=2)

set.seed(23); plot(induced_subgraph(gl,V(gl)[coreness(gl)==4]), vertex.label.dist=2)

#3-core of the graph gl
core3=induced_subgraph(gl,V(gl)[coreness(gl)>=3])
set.seed(23); plot(gl,vertex.label.dist=2);plot(core3,vertex.label.dist=2)
degree(core3)
summary(gl)
summary(core3)
par(parSave)
edge_density(core3)
###################################################################################
# components
###################################################################################
is_connected(gl) #true 
(co = components(gl))
set.seed(23);plot(gl, vertex.color=co$membership, vertex.label.dist=2)


###################################################################################
# Coarsion 
###################################################################################
gl2 <- contract(gl, posit)
summary(gl2)
E(gl2)$weight = 1         #Initialize weight attribute
gl2 = simplify(gl2)    #Aggregates the weights
V(gl2)$label=levels(po)
V(gl2)$color=c("red","green","lightblue","yellow","chartreuse4","blue","darkslateblue","forestgreen","darkgrey","darkorange")
E(gl2)$width=E(gl2)$weight /1.1
sz = as.vector(table(posit)) #Vertices 
V(gl2)$size=sz * 4
E(gl2)$color="black"
set.seed(23);plot(gl2, layout=layout_with_fr,main="",
     palette=rainbow(9), vertex.label.dist=2.5)


###################################################################################
# Conectivity
###################################################################################
vertex_connectivity(gl)
edge_connectivity(gl)
articulation_points(gl)
# Let remove Malik
glr=gl-vertex("Malik")
glr
set.seed(23);plot(glr, layout=layout_with_kk, xlim=c(-.8,.8),ylim=c(-.8,.8), vertex.label.dist=2)

is.connected(glr)
(co=components(glr))
co$no     #8 components
co$csize
set.seed(23);plot(glr,vertex.color=co$membership, vertex.label.dist=2)



#Break graph into component graphs
comps=decompose(glr)
table(sapply(comps,gorder))
#component with 2375 vertices is the giant component.
giant=comps[[1]]

summary(giant)
gorder(giant)*100/gorder(gl) # 77.7
#Giant contains more than 77% of the vertices
set.seed(23);plot(giant, vertex.label.dist=2)



# A clique is a complete subgraph. A maximal clique is maximal because 
# it is not a subset of a larger clique. A clique has 𝜌=1. The igraph 
# functions cliques and max_cliques will find these in any graph.

cliques(gl)
cliques(gl,3)          #Ignore singles and pairs
max_cliques(gl)
max_cliques(gl,4) #maximal cliques of size 4 and up


###################################################################################
# Partitioning
###################################################################################

cfg=cluster_fast_greedy(gl)
length(cfg)              #Number of subgraphs = 4
sizes(cfg)               
membership(cfg)
# plotting partitioned sub graph
set.seed(23);plot(cfg,gl,vertex.label.dist=1.5)
set.seed(23);plot(cfg,gl,main="A Partitioned Graph",vertex.label.dist=1.5
                  , vertex.label= V(gl)$Position
                  , vertex.label.cex = 0.8)

###################################################################################
# Degree Distribution
###################################################################################

plot(0:max(degree(gl)),degree_distribution(gl),pch=20,xlab="degree",
     ylab=NA,type='h')   


###################################################################################
# Centrality Measures
###################################################################################

# Degree Centrality
sort(degree(gl),decreasing = T) # 1 has more centerlity

#Normalize:
deg = round(sort(degree(gl), decreasing = T)/(gorder(gl)-1),2)
#Vertex 1 is connected to 100% of the vertices in the network. If we select a
#random vertex in the network, there is a 100% chance it is connected to 1.

colbar=rep("wheat",gorder(gl))
colbar[V(gl)$name %in% names(deg[1:3])]="red"
set.seed(23);plot(gl,xlim=c(-.7,.7),ylim=c(-.7,.7),vertex.color=colbar)


#Closeness Centrality
(clos=round(sort(closeness(gl),decreasing =T)*(gorder(gl)-1),2))
shape=rep("circle",gorder(gl))
shape[V(gl)$name %in% names(clos[1:3])]="square"
set.seed(23);plot(gl,xlim=c(-.7,.7),ylim=c(-.7,.7),vertex.color=colbar,
                  vertex.shape=shape)



# Betweenness Centrality
(betw=round(sort(betweenness(gl),decreasing =T)/
              ((gorder(gl)-1)*(gorder(gl)-2)/2),2))
#Malik has the highest betweenness centrality; when picking a random two vertices
#in the network and a shortest path between them, the probability that Malik lies
#on this shortest path is 0.28%.
fram=rep("black",gorder(gl))
fram[V(gl)$name %in% names(betw[1:3])]="blue"
set.seed(23);plot(gl,xlim=c(-.7,.7),ylim=c(-.7,.7),vertex.color=colbar,
                  vertex.shape=shape,vertex.frame.color=fram)



#Eigenvector centrality
gl = glo
(eig=round(sort(eigen_centrality(gl)$vector,decreasing =T),2))
lab=rep("black",gorder(gl))
lab[V(gl)$name %in% names(eig[1:3])]="green"
set.seed(23);plot(gl,xlim=c(-.7,.7),ylim=c(-.7,.7),vertex.color=colbar,
                  vertex.shape=shape,vertex.frame.color=fram,
                  vertex.label.color=lab)

###################################################################################
# Edge Measures
###################################################################################

ec=2*edge_betweenness(gl)/(gorder(gl)*(gorder(gl)-1))
(idx=order(ec,decreasing =T)[1:3])
E(gl)[idx]
ec[idx]
ecol=rep("black",gsize(gl))
ecol[idx]="pink"
set.seed(23);plot(gl,xlim=c(-.8,.8),ylim=c(-.8,.8),vertex.color=colbar,
                  vertex.shape=shape,vertex.frame.color=fram,
                  vertex.label.color=lab,
                  edge.color=ecol)


#  of network without Malik
ec=2*edge_betweenness(glr)/(gorder(glr)*(gorder(gl)-1))
(idx=order(ec,decreasing =T)[1:3])
E(glr)[idx]
ec[idx]
ecol=rep("black",gsize(glr))
ecol[idx]="pink"
set.seed(23);plot(glr,xlim=c(-.8,.8),ylim=c(-.8,.8),vertex.color=colbar,
                  vertex.shape=shape,vertex.frame.color=fram,
                  vertex.label.color=lab,
                  edge.color=ecol)


###################################################################################
#transitivity
###################################################################################
#global
transitivity(gl) #3 triangles, 20 triples (3 for each triangle)
#20% of triples are closed
#local (calculate for each vertex)
transitivity(gl,"local")

###################################################################################
# Assortative Mixing
###################################################################################
cfg=cluster_fast_greedy(gl)

#Check the assortativity coefficient for the partition
assortativity_nominal(gl,membership(cfg))  # 30% 

assortativity_nominal(gl, as.numeric(factor(V(gl)$Position)))
assortativity_nominal(gl, as.numeric(factor(V(gl)$Gender))) 
# For categorical characteristics, disassortativity indicates that edges tend not to run between vertices of the same category. 


mean_distance(gl)
diameter(gl)
