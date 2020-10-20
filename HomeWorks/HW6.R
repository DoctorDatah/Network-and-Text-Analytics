###################################################################################
#
# HW6.R
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
#igraph_options(iSave)  #Back to the old values 
#par(parSave)
###################################################################################
# Processing 
###################################################################################
#1
data("UKfaculty")
#a
summary(UKfaculty)
#b
V(UKfaculty)$name=V(UKfaculty)
#c
igraph_options(vertex.size=5,edge.arrow.size=.3)
set.seed(23);plot(UKfaculty)
#d
cols=c("red","green","lightblue","yellow")
V(UKfaculty)$Group
plot(UKfaculty,layout=layout_with_kk,vertex.color=cols[V(UKfaculty)$Group],
     vertex.label=NA)

igraph_options(iSave)
#e
# desnity of the graph
edge_density(UKfaculty)
#Alternatives
mean(degree(UKfaculty,mode="in"))/(gorder(UKfaculty)-1)
gsize(UKfaculty)/(gorder(UKfaculty)*(gorder(UKfaculty)-1))
#f
UK <- contract(UKfaculty, V(UKfaculty)$Group)
summary(UK)
plot(UK,layout=layout_with_kk,vertex.color=cols)
#g
V(UK) # we have 4 groups which multiple vetices in them
E(UK)$weight = 1 
UK = simplify(UK)
#h
# h)	Rename the vertices with the group numbers and set their color
# attributes to the colors determined previously for the groups.
V(UK) # we have 4 groups which multiple vetices in them
# have we are grouping them as to just a number 
V(UK)$name=1:4
V(UK)
# giving them previously saved clors
V(UK)$color=cols
#i
E(UK)$width=sqrt(E(UK)$weight)
#j
sz = as.vector(table(V(UKfaculty)$Group)) #Vertices in each group
V(UK)$size=5*sqrt(sz)
#k
E(UK)$color="black"
#l
par(mfrow=c(1,2))
plot(UKfaculty,layout=layout_with_kk,vertex.color=cols[V(UKfaculty)$Group],
     vertex.label=NA,vertex.size=5,edge.arrow.size=.3)
set.seed(23);plot(UK)
#m
edge_density(UK)
#This is a complete graph
#n
#Group 1 has the most faculty members in the network, group 4 the least.
#The graph is complete, so at least one person in each group is friends 
#with someone in each of the other groups. The number of friendships between
#group 1 and 2 are the greatest.There are few people in group 4, but they seem
#to have a significant number of friendships, especially with the people in group 1.

igraph_options(iSave)
par(parSave)

#2
#a
UK2=as.undirected(UKfaculty)
V(UK2)$name=V(UK2)
#b
is.connected(UK2)
edge_density(UK2)
#c
vertex_connectivity(UK2)   #2
edge_connectivity(UK2)     #2
articulation_points(UK2) # None
#No since the vertex connectivity is 2.
#d
neighbors(UK2,"1")                 #No, 59 is not a neighbor
vertex_connectivity(UK2,"1","59")  #9 Vertex-independent paths
shortest_paths(UK2,"1","59")
edge_connectivity(UK2,"1","59")    #9 Edge-independent paths
#e
cfg=cluster_fast_greedy(UK2)
length(cfg)      #5
#f
plot_dendrogram(cfg)
#g
subs=getSubs(UK2,membership(cfg))
membership(cfg)[c(1,59)]     #subgraph 3
vertex_connectivity(subs[[3]],"1","59")  #7
shortest_paths(subs[[3]],"1","59")       #Longer than before
edge_connectivity(subs[[3]],"1","59")    #7
neighbors(subs[[3]],"1")
neighbors(UK2,"1")
#Vertex 1 lost 2 neighbors
#h
lapply(subs,edge_density)
#All of them are more dense
#i
vertex_connectivity(subs[[3]])  #1, so there are cut vertices
articulation_points(subs[[3]])  #Just one, number "81"
UK3=subs[[3]]-vertex("81")
UK3
is.connected(UK3)
co=components(UK3)
set.seed(23);plot(UK3,vertex.color=co$membership)
#j
part=cut_at(cfg,4)
#k
table(sub=part, group=V(UK2)$Group)
#All of group 2 was captured correctly in subgraph 3. Most of group 1 (19) was
#captured in subgraph 2, but 14 of them were captured in other subgraphs. All
#of group 3 and 4 were captured together in subgraph 1.
#l
cle=cluster_leading_eigen(UK2)
length(cle)     #5
#m
table(sub=membership(cle), group=V(UK2)$Group)
#All of group 2 was captured correctly in subgraph 3. Most of group 1 (23) was
#captured in subgraph 2, but 10 of them ended up in other subgraphs. Every
#vertex of group 3 except for 1 was captured in subgraph 1.
#This one did a great job at recognizing group 2 and 3. Both recognized group 2.
#The spectral partitioning seems to have done a better job.
#n
table(hierarchical=membership(cfg), spectral=membership(cle))
#Hierarchical 4 equals spectral 3. Hierarchical 5 equals spectral 5. The rest
#is different.
#o
lap <- laplacian_matrix(UK2)
eig=eigen(lap)
round(eig$values,2)[gorder(UK2)-1]   #Fiedler value is 1.99
#p
Fied=eig$vectors[,gorder(UK2)-1] 
mem=Fied<0
(subs=getSubs(UK2,mem))
#q
table(sub=mem, group=V(UK2)$Group)
#A terrible job

#3
#Yeast data
data(yeast)

#a
?yeast
summary(yeast)    #Very large network!
#order is 2,617, size is 11,855
yeast$URL

#b
table(degree(yeast))
hist(degree(yeast),col="turquoise")
#Most proteins interact with only a few other proteins, but some of them
#interact with many.

#c
#Components
is_connected(yeast)
co=components(yeast)
co$no     #92 components
co$csize
#The first one is a giant component, consisting of 2375 proteins.

#Break graph into component graphs
comps=decompose(yeast)
table(sapply(comps,gorder))
#component with 2375 vertices is the giant component.
giant=comps[[1]]

summary(giant)
gorder(giant)*100/gorder(yeast)
#Giant contains more than 90% of the vertices

#d
mean_distance(giant)   #Average shortest path over all pairs of vertices
diameter(giant)        #Longest shortest path over all pairs of vertices
diameter(giant)/gorder(giant)*100
#On average, we can get from one protein to any other protein by following
#about 5 connections. Worst case scenario, if we pick the 2 worst proteins,
#it will take following 15 connections. Even the worst case scenario only
#passes through .6% of the proteins in the network.

#e
vertex_connectivity(giant)
edge_connectivity(giant)
cutVertices=articulation_points(giant) #Find all cut vertices
length(cutVertices)
length(cutVertices)/gorder(giant)
#Almost 15% of vertices will disconnect the graph if removed

#f
#Frequency table
table(V(giant)$Class)
table(vertex_attr(giant,"Class")) #Alternative method

#g
cfg=cluster_fast_greedy(giant)
length(cfg)             #31 subgraphs
sizes(cfg)
mem=membership(cfg)

#h
#Contingency table
table(mem, V(giant)$Class)
#Members of the first subgraph mostly belong to class "P"
#Members of subgraph 4 are spread over all classes

#i
cle=cluster_leading_eigen(giant)
length(cle)             #8 subgraphs
sizes(cle)               

#j
(tab=table(membership(cle), V(giant)$Class))

#Members of subgraph 2 mostly belong to class "P"
#Members of subgraph 5 are spread over all classes
