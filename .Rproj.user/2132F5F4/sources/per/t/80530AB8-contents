###################################################################################
#
# Lecture2.R
#
###################################################################################
# External Functions
###################################################################################
library(igraph)
library(igraphdata)
###################################################################################
# Internal Functions
###################################################################################
# adj_book: Create an adjacency matrix from a graph according to the definitions
#           in the book
###################################################################################
adj_book<-function(g,...) {
  m=as.matrix(as_adj(g,...))
  if (is.directed(g)) t(m) else m+diag(diag(m))
}
###################################################################################
# Save the environment 
###################################################################################
parSave=par(no.readonly = TRUE)
###################################################################################
# Processing 
###################################################################################
# The Adjacency Matrix
###################################################################################
g <- graph_from_literal(1-2,2-3:4,3-4:5:6,5-1)
set.seed(23);plot(g)
as_adj(g)                  #Adjacency Matrix
as_adj(g,sparse=F)         #Adjacency Matrix as a matrix

#multigraph
is_simple(g)               #The previous graph is a simple graph
gm <- graph_from_literal(1-2,2-2:3:3:4,3-4:5:6,5-1:1:1,6-6,simplify=F)
is_simple(gm)              #This graph is a mult1graph
set.seed(23);plot(gm, main="Multigraph")
as_adj(gm)
as_adj(gm,sparse=F)
adj_book(gm)

#Digraph
gd <- graph_from_literal(1-+3-+2-+6-+4-+1,4-+5-+3,5+-6)
set.seed(23);plot(gd,main="Digraph")
as_adj(gd)
as_adj(gd,sparse=F)
adj_book(gd)               #Different ordering from the book
gd <- make_graph(c(1,3,3,2,2,6,6,4,4,1,4,5,5,3,6,5))
adj_book(gd)               #Same ordering as the book

#Weighted matrix
E(g)
as.integer(E(g))
E(g)$weight=E(g)          #Let weight be the internal value of the edges
E(g)$label=E(g)           #Let label also be the internal value of the edges
set.seed(23);plot(g, main="Graph with Weights")
as_adj(g,attr="weight")
adj_book(g,attr="weight") #No difference due to symmetry
g=delete_edge_attr(g,"label") #Delete edge attribute label
###################################################################################
# Subgraphs
###################################################################################
set.seed(23);plot(g)
(h=induced_subgraph(g,c(1:3,5))) 
plot(h, main="Induced Subgraph")
par(mfrow=c(1,2))
set.seed(23);plot(g,main="Original");plot(h, main="Induced Subgraph")
par(parSave)
###################################################################################
# Decorating Graphs
###################################################################################
g$name="Test Graph"   #Graph attribute
g$descr="We use this graph to test some aspects of graphs."
#Vertex attributes
V(g)$label=c("Joe","Jill","Pete","Jack","Mary","Rick")
V(g)$gender=c("M","F",rep("M",2),"F","M")
#Edge attribute
E(g)$group=c("family","friends",rep("work",3),rep("family",2))
set.seed(23);plot(g,main=g$name)
save(g,file="../data/g.RData")
save(gd,file="../data/gd.RData")
###################################################################################
# Basic Graph Concepts
###################################################################################
g2=g
V(g2)$name=V(g2)$label
g2=g2+edge("Jill","Mary")
E(g2)$weight=E(g2)
E(g2)$label=E(g2)
iSave=igraph_options(vertex.size=30)
set.seed(23);plot(g2)
igraph_options(iSave)    #Back to defaults

#Neighbors
neighbors(g2, "Jack")   #Can check only one
adjacent_vertices(g2,c("Jack","Joe"))   #Checks multiple vertices

#Degrees of each vertex of the graph
degree(g2)
#Strength of each vertex of the graph
strength(g2)

#Connected?
is_connected(g2)

#Distances
distances(g2, weights=NA)  #Normal distances
distances(g2)              #Distances using weights instead
diameter(g2,weights=NA)
diameter(g2)               #Diameter using weights
############
#Digraphs
gd2=gd+edge(5,4,1,5)
E(gd2)$weight=E(gd2)
E(gd2)$label=E(gd2)
set.seed(99);plot(gd2)

degree(gd2,mode="in")
degree(gd2,3,mode="in")
degree(gd2,mode="out")
degree(gd2,3,mode="out")
degree(gd2)       #Degrees of the underlying undirected graph
degree(gd2,3)

#This will give "Inf" if 6 is not reachable from 1
#Mode is "out" for directed distances
distances(gd2,1,6,weights=NA,mode="out")
#Vertices on the shortest path from 1 to 6
shortest_paths(gd2,1,6,weights=NA)

is_connected(gd2,mode="weak")
is_connected(gd2,mode="strong")

distances(gd2,2,1,weights=NA,mode="out")
distances(gd2,1,2,weights=NA,mode="out")

#Weighted distances
distances(gd2,2,1,mode="out")
distances(gd2,1,2,mode="out")

diameter(gd2, weights=NA)               #Diameter (directed)
#Undirected diameter
diameter(gd2,F,weights=NA)

#All distances
distances(gd2, weights=NA)              #Undirected distances
distances(gd2,mode="out", weights=NA)   #Directed distance matrix
distances(gd2,mode="in", weights=NA)    #Transpose of the out matrix
###################################################################################
# Acyclic Networks 
###################################################################################
ag=graph_from_literal(a-b:c:d,b-e,d-f:g)
set.seed(23);plot(ag)
plot(ag, layout=layout_as_tree)
as_adj(ag)

#DAG
dag=graph_from_literal(a-+b:c:d,b-+e,c-+b,d-+f:g)
set.seed(23);plot(dag)
plot(dag, layout=layout_as_tree)
V(dag)
as_adj(dag)
#Need a different order to get a strictly lower diagonal adjacency matrix
dag=graph_from_literal(e:f:g:b:c:d:a,a-+b:c:d,b-+e,c-+b,d-+f:g)
plot(dag, layout=layout_as_tree)
V(dag)
as_adj(dag)

par(mfrow=c(1,2))
plot(ag, layout=layout_as_tree,main="Tree")
plot(dag, layout=layout_as_tree,main="DAG")
par(parSave)

dagBook <- make_graph(c(9,5,9,7,8,7,8,4,7,5,7,6,6,3,6,2,5,1,5,4,3,1,3,2))    
plot(dagBook, layout=layout_as_tree)
as_adj(dagBook)
adj_book(dagBook)
