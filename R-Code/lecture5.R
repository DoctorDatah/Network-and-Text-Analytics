###################################################################################
#
# Lecture5.R
#
###################################################################################
# External Functions
###################################################################################
library(igraph)
library(igraphdata)
###################################################################################
# Internal Functions
###################################################################################
###################################################################################
# Save the environment 
###################################################################################
parSave=par(no.readonly = TRUE)
iSave=igraph_options(annotate.plot=F,vertex.size=15,vertex.label=NULL,
                     edge.arrow.size=.5)
options(stringsAsFactors = F)
#igraph_options(iSave)  #Back to the old values 
#par(parSave)
###################################################################################
# Processing 
###################################################################################
#Density and Sparsity
###################################################################################
load("../data/g.RData")
set.seed(23); plot(g,edge.label=E(g)$weight)

edge_density(g)
2*gsize(g)/(gorder(g)*(gorder(g)-1))  #The hard way

load("../data/karate.RData")     #Load karate network
edge_density(karate)
load("../data/aidsblog.RData")   #Load aidsblog data
edge_density(aidsblog)
###################################################################################
#Special Graphs
###################################################################################
#A complete graph of order 7
complete=make_full_graph(7)
edge_density(complete)
set.seed(23); plot(complete)
graph_attr(complete)

set.seed(23); plot(g,edge.label=E(g)$weight)
cliques(g)
cliques(g,3)          #Ignore singles and pairs
max_cliques(g)
max_cliques(complete)
max_cliques(karate,4) #maximal cliques of size 4 and up
(mt=make_tree(20, children=2))
set.seed(23); plot(mt,layout=layout_as_tree)
max_cliques(mt,3)     #No cycles, so no cliques of size 3 and up

#A 3-regular network
reg3=sample_k_regular(6,3)
set.seed(23); plot(reg3)
graph_attr(reg3)

#ring
ring8 <- make_ring(8)
set.seed(23); plot(ring8)
#ring is a 2-regular network
reg2=sample_k_regular(8,2)
set.seed(23); plot(reg2)

#Coreness
set.seed(23); plot(g,edge.label=E(g)$weight)
coreness(g)
#2-core of the graph g
set.seed(23); plot(induced_subgraph(g,V(g)[coreness(g)==2]))

coreness(karate)
#4-core of the graph karate
core4=induced_subgraph(karate,V(karate)[coreness(karate)==4])
par(mfrow=c(1,2))
set.seed(23); plot(karate);plot(core4)

#3-core of the graph karate
core3=induced_subgraph(karate,V(karate)[coreness(karate)>=3])
set.seed(23); plot(karate);plot(core3)
degree(core3)
summary(karate);summary(core3)

par(parSave)
###################################################################################
#Visualization
###################################################################################
?plot.igraph
?igraph_options
?igraph.plotting

igraph_options(vertex.size=3,vertex.label=NA,edge.arrow.size=.5)
(gl=make_lattice(c(5,5,5)))         #5x5x5 lattice
make_lattice(length=5,dim=3)        #Alternate method; 5 in each of 3 dimensions
(gl$name=paste("5x5x5",gl$name))
set.seed(23);plot(gl,main=gl$name)
summary(gl)

set.seed(23);plot(make_lattice(c(4,3,2)))   #4x3x2 lattice

#Circular layout
par(mfrow=c(1,2))
plot(gl, layout=layout_in_circle, main=gl$name)
plot(aidsblog, layout=layout_in_circle)
title("Aidsblog Network")

#Fruchterman/Reingold layout
set.seed(23);plot(gl);plot(gl, layout=layout_with_fr) #No difference
set.seed(23);plot(aidsblog,main="Original Aidsblog Network")
plot(aidsblog, layout=layout_with_fr,main=c("Aidsblog Network","Fruchterman/Reingold"))

#Kamada/Kawai layout
set.seed(23);plot(gl);plot(gl, layout=layout_with_kk,main="Kamada/Kawai")
set.seed(23);plot(aidsblog,main="Original Aidsblog Network")
plot(aidsblog, layout=layout_with_kk,main=c("Aidsblog Network","Kamada/Kawai"))

igraph_options(iSave)

#Layouts
set.seed(23);plot(g)
l <- layout_in_circle(g)
l                         #Coordinates of the vertices in the plot
V(g)
set.seed(23);plot(g, layout=l)

#Trees
gtree <- graph.formula(1-+2,1-+3,1-+4,2-+5,2-+6,2-+7,3-+8,3-+9,4-+10)
par(mfrow=c(1, 3))
igraph_options(vertex.size=30,vertex.label=NULL,edge.arrow.size=.5)
plot(gtree, layout=layout_in_circle)
plot(gtree)
plot(gtree, layout=layout_as_tree)
par(parSave)
igraph_options(iSave)

#Directed tree, no root
treedg=graph_from_literal(1:2:3:4:5,1-+3,2-+3-+5,4-+5)
set.seed(23);plot(treedg)
#Create a customized layout for this directed, but not rooted, tree
plot(treedg,layout=matrix(c(-1,1,0,-1,0,1,1,.5,0,-.5),5))

#Bipartite graphs
load("../data/bip.RData")
#Recall, type=FALSE indicates type I, type=TRUE indicates type II
plot(bip, layout=layout_as_bipartite, vertex.size=50, vertex.size2=30,
     vertex.shape=ifelse(V(bip)$type,"rectangle", "circle"),
     vertex.color=ifelse(V(bip)$type, "red", "cyan"))
###################################################################################
#Wayne Zachary's Karate Club
###################################################################################
load("../data/karate.RData")     #Load karate network
V(karate)
graph_attr(karate)
vertex_attr(karate)
edge_attr(karate)
summary(karate)

igraph_options(vertex.size=10)
par(mfrow=c(1, 2))
set.seed(23); plot(karate)
plot(karate,layout=layout_with_kk)
set.seed(23); plot(karate)
plot(karate,layout=layout_with_fr)
par(parSave)
###################################################################################
# Decorate 
V(karate)$label <- sub("Actor ", "", V(karate)$name) #Just use numbers
set.seed(23); plot(karate)

# Two leaders get shapes different from club members.
V(karate)$shape <- "circle"
V(karate)[c("Mr Hi", "John A")]$shape <- "rectangle"
set.seed(23);plot(karate, layout=layout_with_kk)

#Plot usually goes from -1 to 1 on both the x-axis and the y-axis
#Zoom in a bit
plot(karate, layout=layout_with_kk, xlim=c(-.8,.8),ylim=c(-.8,.8))

# Differentiate two factions by color.
V(karate)[Faction == 1]$color <- "red"
V(karate)[Faction == 2]$color <- "seagreen"
V(karate)$frame.color <- NA
plot(karate, layout=layout_with_kk, xlim=c(-.8,.8),ylim=c(-.8,.8))
V(karate)$frame.color <- "black"
plot(karate, layout=layout_with_kk, xlim=c(-.8,.8),ylim=c(-.8,.8))

# Vertex area proportional to vertex strength
# (i.e., total weight of incident edges).
V(karate)$size <- 2.5*sqrt(strength(karate))
V(karate)$size2 <- V(karate)$size * .8

# Make the width of the edges depend on the number of common activities
E(karate)$width <- sqrt(E(karate)$weight)

#Color edges by within/between faction.
F1 <- V(karate)[Faction==1]
F2 <- V(karate)[Faction==2]
#Find specific edges
E(karate)["Mr Hi"%--%V(karate)]
E(karate)[F1%--%F1]$color <- "pink"          #Within faction 1
E(karate)[F2%--%F2]$color <- "palegreen"     #Within faction 2
E(karate)[F1%--%F2]$color <- "black"         #Between factions

#Offset vertex labels for smaller points (default=0).
V(karate)$label.dist <-ifelse(V(karate)$size >= 7, 0, 0.75)

#Final result
plot(karate)
plot(karate, layout=layout_with_kk, xlim=c(-.8,.8),ylim=c(-.8,.8),
     main=karate$name)
plot(karate, layout=layout_with_fr, xlim=c(-.8,.8),ylim=c(-.8,.8),
     main=karate$name)
plot(karate, layout=layout_with_kk, xlim=c(-.8,.8),ylim=c(-.8,.8),
     main=karate$name)
#Zoom in
plot(karate, layout=layout_with_kk, xlim=c(-.5,.5),ylim=c(0,.5))

#Compare
karate2=karate
load("../data/karate.RData")
par(mfrow=c(1,2))
set.seed(23);plot(karate)
plot(karate2, layout=layout_with_kk, xlim=c(-.8,.8),ylim=c(-.8,.8),
     main=karate2$name)
par(parSave)

#Save the karate object with all the changes we made
save(karate2,file="karate2.RData")
