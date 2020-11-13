###################################################################################
#
# Lecture7.R
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
# Vertex Centrality
###################################################################################
g <- graph_from_literal(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6,
                        4-7, 5-6, 6-7)
set.seed(23);plot(g)

#Degree Centrality###################
sort(degree(g))
#Normalize:
round(sort(degree(g))/(gorder(g)-1),2)
#Vertex 1 is connected to 33% of the vertices in the network. If we select a
#random vertex in the network, there is a 33% chance it is connected to 1.

#Closeness Centrality################
(dis=distances(g))
apply(dis,1,function(x) {1/sum(x)}) #Closeness formula
closeness(g)
#Sorted
sort(closeness(g))
#Normalize:
round(sort(closeness(g))*(gorder(g)-1),2)

#Betweenness Centrality##############
sort(betweenness(g))
#Normalize:
round(sort(betweenness(g))/((gorder(g)-1)*(gorder(g)-2)/2),2)
#Vertex 4 lies on 34% of shortest paths. Choose a random 2 vertices not 
#equal to 4, and pick a shortest path between them. Then there is a 34%
#probability that 4 lies on it.

#Eigenvector centrality##############
eigen_centrality(g)[1:2]
round(sort(eigen_centrality(g)$vector),2)
as_adj(g)
(eig=eigen(as_adj(g)))
eve=eig$vectors[,1]
names(eve)=V(g)$name
eig$value[1];eve
#Example: 7 has 4 and 6 as neighbors. Adding their centrality values
#and dividing by the eigenvalue gives the centrality value for 7. 

#Why is the eigen vector different?################################################
#The eigenvectors given by the eigen function have been normalized
#so they are perpendicular to each other with length 1.
round(t(eig$vectors)%*%eig$vectors,3)
eigen_centrality(g)$vector/eig$vectors[,1]
#The vectors are multiples of each other
###################################################################################

#Compare#############################
#Four is most central by all accounts, but look at 2, 3, and 5
round(sort(degree(g),decreasing =T)/(gorder(g)-1),2)
#Equal degrees
round(sort(closeness(g),decreasing =T)*(gorder(g)-1),2)
#2 and 5 have equal distances to other vertices, but 3 has more distance
#(i.e. higher value)
round(sort(betweenness(g),decreasing =T)/((gorder(g)-1)*(gorder(g)-2)/2),2)
#2 lies on more shortest paths than 5, 5 more than 3
round(sort(eigen_centrality(g)$vector,decreasing =T),2)
#5 has more influential neighbors than 2 and 3, so is itself more important


#karate
load("../data/karate.RData")
#We find the 5 most central vertices for each measure
#Note that we do not need to normalize to compare
sort(degree(karate),decreasing = T)[1:5]
round(sort(closeness(karate),decreasing = T),4)[1:5] 
round(sort(betweenness(karate),decreasing = T),2)[1:5]
round(sort(eigen_centrality(karate)$vector,decreasing = T),3)[1:5]

#Digraph
load("../data/gd.RData")
set.seed(23);plot(gd)
V(gd)$name=V(gd)
is.connected(gd,mode="strong")
#In-degree########################
sort(degree(gd,mode="in"),decreasing = T)
#5 has a the higher in-degree than 2
#Closeness########################
round(sort(closeness(gd,mode="in"),decreasing = T),2)
#2 and 5 have the same closeness
#PageRank#########################
(pr=page_rank(gd))
round(sort(pr$vector,decreasing = T),3)
#2 has more important neighbors pointing to it than 5

#Note; 4 and 6 are more central than 1 and 5, so by Katz' centrality,
#5 would be more central than 3. But 4 and 6 each point to another
#in addition to 5, while 1 and 5 ONLY point to 3. 
###################################################################################
# Hubs and Authorities
###################################################################################
par(mfrow=c(1,3))
set.seed(23);plot(gd)
plot(gd,main="Hubs",vertex.size=20*(hub_score(gd)$vector))
plot(gd,main="Authorities",vertex.size=20*(authority_score(gd)$vector))
par(parSave)

load("../data/aidsblog.RData")      #Directed network of blogs about AIDS
summary(aidsblog)
V(aidsblog)$name=V(aidsblog)
hs=hub_score(aidsblog)$vector
as=authority_score(aidsblog)$vector
igraph_options(vertex.size=3,vertex.label=NA,edge.arrow.size=.5)
par(mfrow=c(1,3))
set.seed(23);plot(aidsblog, layout=layout_with_kk,main="Blog Network")
plot(aidsblog,layout=layout_with_kk,main="Hubs",vertex.size=10*sqrt(hs))
plot(aidsblog,layout=layout_with_kk,main="Authorities",vertex.size=10*sqrt(as))
par(parSave)

#Compare with degrees
#hubs
(hub=round(sort(hs,decreasing=T),2)[1:10])
sort(degree(aidsblog,mode="out"),decreasing=T)[1:10]
#authorities
(auth=round(sort(as,decreasing=T),2)[1:10])
sort(degree(aidsblog,mode="in"),decreasing=T)[1:10]

#Egocentric Graphs
ego=make_ego_graph(aidsblog,1,names(auth)[1])[[1]]
V(ego)$color[V(ego)$name %in% names(hub)]="red"
plot(ego,vertex.label=V(ego)$name,vertex.size=20)
ego2=make_ego_graph(aidsblog,1,names(hub)[1])[[1]]
V(ego2)$color[V(ego2)$name %in% names(auth)]="red"
plot(ego2,vertex.label=V(ego2)$name,vertex.size=20)

igraph_options(iSave)
par(parSave)
###################################################################################
# Edge Centrality
###################################################################################
load("../data/karate2.RData")
set.seed(23);plot(karate2)
eb <- edge_betweenness(karate2)
E(karate2)[order(eb, decreasing=T)[1:3]] #Three most traversed edges
#It appears that actor 20 is important for the interactions between
#Mr. Hi and John A.
#Normalize
eb=2*eb/(gorder(karate2)*(gorder(karate2)-1))
sort(eb,decreasing=T)[1] #Appr. 25% of shortest paths go through this edge
E(karate2)$color[order(eb, decreasing=T)[1]]="blue"
set.seed(23);plot(karate2)

#Line graphs
iSave=igraph_options(vertex.size=15)
E(g)$label=as.numeric(E(g))
par(mfrow=c(1,2))
gl=make_line_graph(g)
V(gl)$name=V(gl)
set.seed(23);plot(g);plot(gl)
igraph_options(iSave)
par(parSave)

eb=2*edge_betweenness(g)/(gorder(g)*(gorder(g)-1))
idx=order(eb,decreasing = T)
E(g)[idx]
eb[idx]
#The edge from 2 to 4 has the highest edge betweenness; approximately
#32.5% of shortest paths pass through it.

sort(degree(gl),decreasing = T)
#Edges 4,6,7 have the highest degree
sort(knn(gl)$knn,decreasing = T)
#Edge 8 has the highest avg. neighbor degree
round(sort(closeness(gl),decreasing = T),3) 
#Edge 4 is the closest to all other edges             
round(sort(betweenness(gl),decreasing = T),2)
#Edge 4 has most shortest paths passing through
#Same result as the edge betweenness
round(sort(eigen_centrality(gl)$vector,decreasing = T),3)
#Edge 6 has the most central neighbors
###################################################################################
# Network Cohesion
###################################################################################
dg <- graph_from_literal(Sam-+Mary, Sam-+Tom, Mary++Tom,Jack:Carol--+Mary)

#Dyads and triads
set.seed(23);plot(dg)
dyad_census(dg)
#Note that (Jack,Tom) gives a null, and also (Jack,Carol),(Jack,Sam),
#(Tom,Carol), and (Sam,Carol). (Mary,Tom) is mutual, all others asymmetric.
triad_census(dg)
?triad_census

summary(aidsblog)
aidsblog2=simplify(aidsblog)
summary(aidsblog2)
dyad_census(aidsblog2)

#transitivity
#global
set.seed(23);plot(g)
transitivity(g) #3 triangles, 20 triples (3 for each triangle)
#45% of triples are closed
#local (calculate for each vertex)
transitivity(g,"local")

#karate network
transitivity(karate)
tl=transitivity(karate,"local")
names(tl)=V(karate)$name
sort(tl)
#For the two main actors
round(transitivity(karate,"local",vids=c(1,34)),3)

#reciprocity
set.seed(23);plot(dg)
dg
reciprocity(dg, mode="default")
dyad_census(dg)
reciprocity(dg, mode="ratio")

reciprocity(aidsblog, mode="default")
reciprocity(aidsblog, mode="ratio")
#Few blogs have links that are reciprocated.

#Small world property
summary(karate)
mean_distance(karate)
diameter(karate)      #Longest shortest distance
###################################################################################
# Assortative Mixing
###################################################################################
?assortativity
cfg=cluster_fast_greedy(g)
set.seed(23);plot(cfg,g)
#Check the assortativity coefficient for the partition
assortativity_nominal(g,membership(cfg))  #58.33%

#karate network
set.seed(23); plot(karate, xlim=c(-.8,.8),ylim=c(-.8,.8))
summary(karate)
assortativity_nominal(karate,V(karate)$Faction)
#This is high and positive, meaning that members of the same
#faction tend to be connected more than members of different
#factions

#For yeast, remove missing values first
data(yeast)
table(V(yeast)$Class,useNA="always")  #Some values are missing
yeast2=induced_subgraph(yeast,V(yeast)[-which(is.na(V(yeast)$Class))])
summary(yeast2)
#By class P or not class P (types vector must have values starting at 1)
assortativity_nominal(yeast2, (V(yeast2)$Class=="P")+1)
#By all classes (types vector must have values starting at 1)
#This is assortative
assortativity_nominal(yeast2, as.numeric(factor(V(yeast2)$Class)))
#This is also assortative (but not as strong as the previous)
#Note that R fails if we leave the NA values in

#For continuous values
assortativity(yeast2,degree(yeast2))
#For degree, we may also use this shortcut
assortativity_degree(yeast2)
#Assortative

assortativity_degree(karate)
#disassortative; people of high degree tend to interact with people of low
#degree

