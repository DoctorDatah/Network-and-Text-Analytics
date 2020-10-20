###################################################################################
#
# Lecture6.R
#
###################################################################################
# External Functions
###################################################################################
library(igraph)
library(igraphdata)
###################################################################################
# Internal Functions
###################################################################################
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
options(stringsAsFactors = F)
#igraph_options(iSave)  #Back to the old values 
#par(parSave)
###################################################################################
# Processing 
###################################################################################
#Lazega Lawyers Network: Problem
###################################################################################
#load("../data/lazega.RData")
load("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/ lazega.RData")
summary(lazega)
graph_attr(lazega)
vertex_attr(lazega)
edge_attr(lazega)
l <- layout_with_kk(lazega)
set.seed(23);plot(lazega)
plot(lazega, layout=l)
plot(lazega, layout=l, xlim=c(-.8,.8),ylim=c(-.8,.8))

# Office location indicated by color
colbar <- c("red", "seagreen", "goldenrod") #Boston/Hartford/Providence
set.seed(23);plot(lazega, vertex.color=V(lazega)$Office,palette=colbar)
plot(lazega, layout=l,vertex.color=V(lazega)$Office,palette=colbar)

# Type of practice indicated by vertex shape
shape <- c("circle", "square")[V(lazega)$Practice] #Litigation/Corporate
plot(lazega, layout=layout_with_fr, vertex.color=V(lazega)$Office, palette=colbar,
     vertex.shape=shape,main=lazega$name)

#Vertex size proportional to years with firm
sz <- 2.5*sqrt(V(lazega)$Years)
plot(lazega, layout=l, vertex.color=V(lazega)$Office, palette=colbar,
     vertex.shape=shape,vertex.size=sz,main=lazega$name)

# Label vertices according to seniority
plot(lazega, layout=l, vertex.color=V(lazega)$Office, palette=colbar,
     vertex.shape=shape,vertex.size=sz, vertex.label=V(lazega)$Seniority,
     main=lazega$name)

# Label color according to gender
gender=c("black","green")[V(lazega)$Gender]
plot(lazega, layout=l, vertex.color=V(lazega)$Office, palette=colbar,
     vertex.shape=shape,vertex.size=sz, vertex.label=V(lazega)$Seniority,
     vertex.label.color=gender,main=lazega$name)


# Offset small labels
plot(lazega, layout=l, vertex.color=V(lazega)$Office, palette=colbar,
     vertex.shape=shape,vertex.size=sz, vertex.label=V(lazega)$Seniority,
     vertex.label.dist=ifelse(sz >= 7, 0, 1.2),
     vertex.label.color=gender,main=lazega$name)

#Title and Description
nchar(lazega$description)
ds=substr(lazega$description,1,62)
ds[2]=substring(lazega$description,64)
plot(lazega, layout=l, vertex.color=V(lazega)$Office, palette=colbar,
     vertex.shape=shape,vertex.size=sz, vertex.label=V(lazega)$Seniority,
     vertex.label.dist=ifelse(sz >= 7, 0, 1.2),
     vertex.label.color=gender,main=c(lazega$name,ds))
legend("bottomright",c("Boston","Hartford","Providence"),pch=19,col=colbar)

legend("bottomleft",c("Litigation","Corporate"),pch=c(1,0))

legend("topleft",c("Male","Female"),lty=1,col=c("black","green"))

#Compare
par(mfrow=c(1,2))
set.seed(23);plot(lazega)
plot(lazega, layout=l, vertex.color=V(lazega)$Office, palette=colbar,
     vertex.shape=shape,vertex.size=sz, vertex.label=V(lazega)$Seniority,
     vertex.label.dist=ifelse(sz >= 7, 0, 1.2),
     vertex.label.color=gender,main=c(lazega$name,ds))
legend("bottomright",c("Boston","Hartford","Providence"),pch=19,col=colbar,
       cex=.7)
legend("bottomleft",c("Litigation","Corporate"),pch=c(1,0),cex=.7)
legend("topleft",c("Male","Female"),lty=1,col=c("black","green"),cex=.7)
par(parSave)
###################################################################################
#Large Networks Visualization: French Political Blogs
###################################################################################
load("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/fblog.RData")
summary(fblog)
#Political parties
sort(unique(V(fblog)$PolParty))
#Color by party
polp=factor(V(fblog)$PolParty) # factor ban k phir numeric main convert kiya idr
polpNum=as.numeric(polp)
plot(fblog, layout=layout_with_kk, vertex.label=NA,vertex.color=polpNum+1, 
     vertex.size=3,vertex.frame.color=NA,palette=rainbow(9))

#Coarsen the network
vertex_attr_names(fblog)
#Contract takes a vector with a number for each vertex and combines all vertices
#with the same number. In the summary, x stands for combined.
#Note that this is now a multi-graph with multiple edges between vertices
fblog2 <- contract(fblog, polpNum)
summary(fblog2)
V(fblog2)[1]                 #Each vertex now has multiple names
E(fblog2)$weight = 1         #Initialize weight attribute
fblog2 = simplify(fblog2)    #Aggregates the weights
V(fblog2)$label=levels(polp)
V(fblog2)$color=2:10
E(fblog2)$width=.5*sqrt(E(fblog2)$weight)
sz = as.vector(table(polpNum)) #Vertices in each party
#Note that weight has the number of links between the blogs of the two parties 
#involved, while party.size contains the number of blogs for each party.
V(fblog2)$size=2*sqrt(sz)
E(fblog2)$color="black"
set.seed(23)
plot(fblog2, layout=layout_with_fr, vertex.label.dist=1.5,main=fblog$name,
     palette=rainbow(9))

#Compare
par(mfrow=c(1,2))
plot(fblog, layout=layout_with_kk, vertex.label=NA,vertex.color=polpNum+1, 
     vertex.size=3,vertex.frame.color=NA,palette=rainbow(9))
set.seed(23)
plot(fblog2, layout=layout_with_fr, vertex.label.dist=1.5,main=fblog$name,
     palette=rainbow(9))
par(parSave)
###################################################################################
#Large Networks Visualization: Egocentric Networks
###################################################################################
#Egocentric network visualizations - continue with the karate network
load("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/karate2.RData")
set.seed(23)
plot(karate2,layout=layout_with_kk, xlim=c(-.8,.8),ylim=c(-.8,.8))
# make_ego_graph is creates (sub)graphs from all neighborhoods of the
# given vertices with the given order parameter.
# This function preserves the vertex, edge and graph attributes.
egog=make_ego_graph(karate2)     #List with all egocentric neighborhoods of order 1
(ord=sapply(egog, gorder))      #Order{# of vertics} of the egocentric neighborhoods 
# note Order function is for sorting whereas gorder is number of vertices
#plot the two largest
# First we need to get the vertics number with the higest gorder
#  the code below gives the correspninting ertics numbers
which.max(ord);which.max(ord[-which.max(ord)])
#alternative: order(ord,decreasing=T)[1:2] 
par(mfrow=c(1,2))
plot(egog[[1]],layout=layout_with_kk, xlim=c(-.8,.8),ylim=c(-.8,.8))
plot(egog[[34]],layout=layout_with_kk, xlim=c(-.8,.8),ylim=c(-.8,.8))

#Note that we could have picked the two vertices with the highest degree
#which is effectively the same thing:
egog2=make_ego_graph(karate2,1,c("Mr Hi","John A"))

#Different color for the center vertex
gorder(egog2[[1]]) # 17 vertics 
plot(egog2[[1]], vertex.label=NA, vertex.color=c("red", rep("lightgreen",16)))
gorder(egog2[[2]]) # 18 vertics 
plot(egog2[[2]], vertex.label=NA, vertex.color=c(rep("lightgreen",17),"red"))
# We giving the ego vertics red color remaing lightgreen color
graph.density(karate2)
graph.density(egog2[[1]])  #Most dense around Mr. Hi
graph.density(egog2[[2]])  #More dense than the network itself

#Level 2 egocentric graph for Mr. Hi
egohi=make_ego_graph(karate2,2,"Mr Hi")[[1]]
summary(karate2);summary(egohi)
par(mfrow=c(1,2))
plot(karate2,layout=layout_with_kk, xlim=c(-.8,.8),ylim=c(-.8,.8))
plot(egohi,layout=layout_with_kk, xlim=c(-.8,.8),ylim=c(-.8,.8))

#Level 2 egocentric graph for John A
egoA=make_ego_graph(karate2,2,"John A")[[1]]
plot(karate2,layout=layout_with_kk, xlim=c(-.8,.8),ylim=c(-.8,.8))
plot(egoA,layout=layout_with_kk, xlim=c(-.8,.8),ylim=c(-.8,.8))
igraph_options(iSave)
par(parSave)
###################################################################################
#Problem
###################################################################################
load("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/aidsblog.RData")
igraph_options(vertex.size=3,edge.arrow.size=.3)
summary(aidsblog)
V(aidsblog)$name=V(aidsblog)
plot(aidsblog,layout=layout_with_fr)
# sorting based on degree
sort(degree(aidsblog,mode="out"),decreasing = T)
#7 has the highest degree with 43 links out
egog=make_ego_graph(aidsblog,order=1, nodes=7)[[1]]
set.seed(23);plot(egog)
plot(egog,layout=layout_with_fr)
plot(egog,layout=layout_as_star)
plot(egog,layout=layout_in_circle)
plot(egog,layout=layout_with_kk)
#Anything other than the star or circle is fine
par(mfrow=c(1,2))
table(degree(egog,mode="out"))
table(degree(egog,mode="in"))
plot(0:max(degree(egog,mode="out")),degree_distribution(egog,mode="out"),
     xlab="degree",ylab=NA,type='h')   
plot(0:max(degree(egog,mode="in")),degree_distribution(egog,mode="in"),
     xlab="degree",ylab=NA,type='h')   
par(parSave)
#The blog of vertex 7 links to all other aids blogs, but only a few of them (5)
#also link to other aids blogs; the rest does not link to anything at all.
#Four blogs get linked to twice, the rest is linked to only once.

# Q: Which blogs with the highest out degree link to the blog7 
# that means we need blog7 in dregree
E(egog)
incident(egog,which(V(egog)$name==7),"in")
#Aidsblog 143 is the only one that links to the main blog, number 7.
###################################################################################
#Independent Paths
###################################################################################
load("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/g.RData")
g2=g+vertex("Ann")
g2=g2+edges("Ann","Rick","Ann","Jack")
g2=g2-edge(c("Jill|Jack"))
g3=g-vertex("Rick")
igraph_options(vertex.size=30)
par(mfrow=c(1,2))
set.seed(10);plot(g2,main="g2")
set.seed(10);plot(g3,main="g3")
par(parSave)
is_connected(g2)
#Connectivity between two vertices
vertex_connectivity(g2,"Ann","Joe")
edge_connectivity(g2,"Ann","Joe")
vertex_connectivity(g2,"Pete","Joe")
###################################################################################
#Cut Sets
###################################################################################
vertex_connectivity(g2)
edge_connectivity(g2)     #g2 is 2-edge connected
vertex_connectivity(g3)
edge_connectivity(g3)

# Articuation points or cut vertices are vertices 
# whose removal increases the number of connected components in a graph.
articulation_points(g2)   #Pete is a cut vertex
articulation_points(g3)   #None (vertex connectivity=2)
#Jill and Pete form a minimum cut set, but also Joe and Pete. But not Joe and Mary.
#Joe, Pete, and Jill also form a cut set, but not a minimum one.

par(mfrow=c(1,2))
set.seed(23);plot(g2-vertex("Pete"))
set.seed(23);plot(g3-vertex("Pete"))
par(parSave)

#Digraph
load("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/gd.RData")
set.seed(23);plot(gd)
is_connected(gd)
# Strong and weeak in only for directed graph
is_connected(gd,"strong")
vertex_connectivity(gd)
edge_connectivity(gd)
#Strong articulation points not implemented in igraph
is_connected(gd-vertex(1),"strong") #  T
is_connected(gd-vertex(2),"strong") # F
is_connected(gd-vertex(3),"strong") # F
is_connected(gd-vertex(4),"strong") # F
is_connected(gd-vertex(5),"strong") # T
is_connected(gd-vertex(6),"strong") # F
#2, 3, 4, and 6 are strong articulation points

gu=as.undirected(gd)

set.seed(23);plot(gu)
degree(gu)
vertex_connectivity(gu)
edge_connectivity(gu)
#Find a vertex cut set
is_connected(gu-vertices("1","2")) # T
is_connected(gu-vertices("3","4"))  #Cut set # F
par(mfrow=c(1,2))
set.seed(23);plot(gu);plot(gu-vertices("3","4"))
par(parSave)
igraph_options(iSave)
###################################################################################
#Graph Partitioning: Hierarchical Clustering
###################################################################################
g <- graph_from_literal(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6,
                        4-7, 5-6, 6-7)
set.seed(23); plot(g, main="g")
cfg=cluster_fast_greedy(g)
class(cfg)               #Communities class
length(cfg)              #Number of subgraphs
sizes(cfg)               #Subgraph 1 has size 4, subgraph 2 has size 3
membership(cfg)          #for each subgraph, give the id
# plotting partioned sub graph
set.seed(23);plot(cfg,g,main="A Partitioned Graph")
#Show the agglomerative process
plot_dendrogram(cfg)
cut_at(cfg,3)            #Membership if we want 3 subgraphs

#karate network


load("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/karate.RData")
load("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/karate2.RData")

set.seed(23); plot(karate, xlim=c(-.8,.8),ylim=c(-.8,.8))
cfg=cluster_fast_greedy(karate)
length(cfg)
sizes(cfg)
membership(cfg)
set.seed(23);plot(cfg,karate2)
plot_dendrogram(cfg)
#We see here that group 3 (blue) can be combined with group 2 (beige)
membership(cfg)[membership(cfg)==1]
membership(cfg)[membership(cfg)==2]
membership(cfg)[membership(cfg)==3]

(ca=cut_at(cfg,2))     #Membership if we only want 2 subgraphs
shape=c("square","circle")[ca]
set.seed(23); plot(karate2, xlim=c(-.8,.8),ylim=c(-.8,.8),vertex.shape=shape)
# set.seed(23);plot(cfg,karate2) not working

#Perfectly split by faction!
par(parSave)
###################################################################################
#Split a Network into subgraphs by the values of a vector
###################################################################################
load("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/g.RData")

#Split by gender
subs = getSubs(g,V(g)$gender)

set.seed(23);plot(subs[[1]])
set.seed(23);plot(subs[[2]])

###################################################################################
#Graph Partitioning: Spectral Partitioning
###################################################################################
set.seed(23);plot(g)

# Getting laplacian_matirx
(lap <- laplacian_matrix(g))
#Alternative
diag(degree(g))-as_adj(g)

# Getting Eagan valus
eig=eigen(lap)
round(eig$values,2)
plot(eig$values,col="pink",pch=16)
#Note that the Fiedler value is .81. # i didnot get it
#We have the Fiedler vector: # Getting fiedler vector
(Fied=eig$vectors[,gorder(g)-1])
# setting colot 
V(g)$color[Fied>=0]=2
V(g)$color[Fied<0]=3
set.seed(23);plot(g)
#Same subgraphs as the hierarchical method

#Find subgraphs using this method iteratively
cle=cluster_leading_eigen(g)
set.seed(23);plot(cle,g)    #Same as before

#Karate
load("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/karate.RData")
load("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/karate2.RData")

summary(karate)
lap <- laplacian_matrix(karate)
eig=eigen(lap)
round(eig$values,2)   #Fiedler value is 1.19
(Fied=eig$vectors[,gorder(karate)-1])    
col=as.character(V(karate2)$color)
#Plot the Fiedler vector and color its elements by faction
par(mfrow=c(1,2))
plot(Fied, pch=16, xlab="Actor Number",ylab="Fiedler Vector Entry", col=col)
abline(0, 0, lwd=2, col="lightgray")
#Note that the split is exactly by faction
set.seed(23); plot(karate2, xlim=c(-.8,.8),ylim=c(-.8,.8))
par(parSave)

#Iteratively
(is=getSubs(karate2,Fied<0))
round(eigen(laplacian_matrix(is[[1]]))$values,2)
round(eigen(laplacian_matrix(is[[2]]))$values,2)
#This time the Fiedler value is about 2 for both graphs, so we may split again
(Fied1=eigen(laplacian_matrix(is[[1]]))$vectors[,gorder(is[[1]])-1])   
(is1=getSubs(is[[1]],Fied1<0))
#Note that is1[[2]] only contains 1 vertex!
round(eigen(laplacian_matrix(is1[[1]]))$values,2)
round(eigen(laplacian_matrix(is1[[2]]))$values,2)

#This will split using spectral partitioning iteratively using modularity
#to determine whether to keep a split.
cle=cluster_leading_eigen(karate)
set.seed(23);plot(cle,karate)

