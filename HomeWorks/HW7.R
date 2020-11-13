###################################################################################
#
# HW7.R
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
iSave=igraph_options(annotate.plot=F,vertex.size=15,edge.arrow.size=.5,
                     vertex.label=NULL)
#igraph_options(iSave)  #Back to the old values 
#par(parSave)
###################################################################################
# Processing 
###################################################################################
#1
load("../data/lazega.RData")
#a
is.connected(lazega)
co=components(lazega)
co$membership
giant=induced_subgraph(lazega,which(co$membership==1))
#Alternative
#giant=induced_subgraph(lazega,names(co$membership[co$membership==1]))
summary(lazega)
summary(giant)
set.seed(23);plot(giant,xlim=c(-.8,.8),ylim=c(-.8,.8))

#b
#Degree centrality
(deg=round(sort(degree(giant),decreasing =T)/(gorder(giant)-1),2))
#V17 has the highest degree; it is connected to 45 percent of the other vertices

#c
colbar=rep("wheat",gorder(giant))
colbar[V(giant)$name %in% names(deg[1:3])]="red"
set.seed(23);plot(giant,xlim=c(-.7,.7),ylim=c(-.7,.7),vertex.color=colbar)

#d
#Closeness centrality
(clos=round(sort(closeness(giant),decreasing =T)*(gorder(giant)-1),2))
shape=rep("circle",gorder(giant))
shape[V(giant)$name %in% names(clos[1:3])]="square"
set.seed(23);plot(giant,xlim=c(-.7,.7),ylim=c(-.7,.7),vertex.color=colbar,
                  vertex.shape=shape)

#e
#Betweenness centrality
(betw=round(sort(betweenness(giant),decreasing =T)/
              ((gorder(giant)-1)*(gorder(giant)-2)/2),2))
#V17 has the highest betweenness centrality; when picking a random two vertices
#in the network and a shortest path between them, the probability that V17 lies
#on this shortest path is 19%.
fram=rep("black",gorder(giant))
fram[V(giant)$name %in% names(betw[1:3])]="blue"
set.seed(23);plot(giant,xlim=c(-.7,.7),ylim=c(-.7,.7),vertex.color=colbar,
                  vertex.shape=shape,vertex.frame.color=fram)

#f
#Eigenvector centrality
(eig=round(sort(eigen_centrality(giant)$vector,decreasing =T),2))
lab=rep("black",gorder(giant))
lab[V(giant)$name %in% names(eig[1:3])]="green"
set.seed(23);plot(giant,xlim=c(-.7,.7),ylim=c(-.7,.7),vertex.color=colbar,
                  vertex.shape=shape,vertex.frame.color=fram,
                  vertex.label.color=lab)

#g
#Edge Centrality
ec=2*edge_betweenness(giant)/(gorder(giant)*(gorder(giant)-1))
(idx=order(ec,decreasing =T)[1:3])
E(giant)[idx]
ec[idx]
ecol=rep("black",gsize(giant))
ecol[idx]="pink"
set.seed(23);plot(giant,xlim=c(-.8,.8),ylim=c(-.8,.8),vertex.color=colbar,
                  vertex.shape=shape,vertex.frame.color=fram,
                  vertex.label.color=lab,vertex.label=V(giant)$Seniority,
                  edge.color=ecol)
#Vertices in the center are well-connected, so there may be many shortest
#paths between pairs of vertices not including 1, 11, or 21.

#h
transitivity(giant)
#39% of connected triples are closed.
(tr=transitivity(giant,type="local"))
V(giant)[which(tr==1)]
#Note that the transitivity for V9 and V13 is 1! Let's take a look.
plot(make_ego_graph(giant,1,"V9")[[1]])
plot(make_ego_graph(giant,1,"V13")[[1]])
#Their neighbors are all connected to each other; the egocentric graphs are
#complete, they have density equal to 1. 

#i
assortativity_nominal(giant,V(giant)$Office)
#Yes, it is assortative; partners tend to have more connections with partners
#in the same office.
assortativity_degree(giant)
#Partners with high degrees tend to have connections with partners with low degree,
#but this relationship is not very strong.

#2
data("UKfaculty")
summary(UKfaculty)
#a
is.connected(UKfaculty,mode="strong")
co=components(UKfaculty,mode="strong")
giant=induced_subgraph(UKfaculty,which(co$membership==1))
is.connected(giant,mode="strong")
summary(giant)
V(giant)$name=V(giant)

#b
#In-degree########################
sort(degree(giant,mode="in"),decreasing = T)[1:3]
#68 and #76 have the highest degree

#c
#Closeness########################
round(sort(closeness(giant,mode="in"),decreasing = T),5)[1:3]
#Distance from all others to 17 is the smallest

#d
#PageRank#########################
round(sort(page_rank(giant)$vector,decreasing = T),5)[1:3]
#76 has the most important neighbors pointing to it

#e
#Hubs and authorities
hs=hub_score(giant)$vector
(hubs=sort(hs,decreasing=T)[1:3])
#28 is the main hub
V(giant)$Group[V(giant)$name %in% names(hubs)] #All in Group 1
vertex_attr(giant,index=28)

#f
as=authority_score(giant)$vector
(auth=sort(as,decreasing=T)[1:3])
#30 is the main authority
V(giant)$Group[V(giant)$name %in% names(auth)] #All in Group 1
vertex_attr(giant,index=30)

#g
#Plot
V(giant)$color=V(giant)$Group
igraph_options(vertex.size=5,edge.arrow.size=.2)
par(mfrow=c(1,3))
palette(rainbow(4))
set.seed(23);plot(giant, layout=layout_with_kk,main="UK Faculty Network",
                  palette=rainbow(4))
legend("topright",paste("Group",levels(factor(V(giant)$Group))),pch=19,col=1:4,
       cex=.9)

#h
plot(giant,layout=layout_with_kk,main="Hubs",vertex.size=15*sqrt(hs),
     palette=rainbow(4))

#i
plot(giant,layout=layout_with_kk,main="Authorities",vertex.size=15*sqrt(as),
     palette=rainbow(4))
par(parSave)
igraph_options(iSave)
palette("default")

#j
#dyads and triads
dyad_census(giant)
triad_census(giant)
?triad_census
#531 triples of vertices are connected in a directed line.

#k
#Reciprocity
reciprocity(giant, mode="default")
#59% of edges are reciprocated.
reciprocity(giant, mode="ratio")
#42% of pairs of vertices that are connected have edges in both directions.

