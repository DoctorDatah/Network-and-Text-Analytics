###################################################################################
#
# HW4.R
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
iSave=igraph_options(annotate.plot=F,vertex.size=15,edge.arrow.size=.5)
options(stringsAsFactors = F)
#igraph_options(iSave)  #Back to the old values 
#par(parSave)
###################################################################################
# Processing 
###################################################################################
#1
tr=graph_from_literal(A-+X:R,B-+A:C,D-+O:K:M,E-+B:Q:G:H,H-+F,
                     J-+T,O-+N,P-+W,Q-+D:I:P:U,T-+L:S:Z,U-+J,W-+Y)
tr
igraph_options(vertex.size=10,edge.arrow.size=.5)
set.seed(11);plot(tr)
#a
#This is a rooted tree, and the root is E.
#b
plot(tr,layout=layout_as_tree)
#c
#Yes, Y is a descendant of Q
#d
#A and C are the children of B
#e
#The leaves are X,R,C,N,K,M,Y,I,L,S,Z,G,F
#f
gorder(tr);gsize(tr)
#This tells us that the graph is a tree (m=n-1)
#g
table(degree(tr,mode="in"))
#Almost all vertices have one edge going into it, except one (the root)
gsize(tr)/gorder(tr) # that means

#h
table(degree(tr,mode="out"))
hist(degree(tr,mode="out"))
plot(0:max(degree(tr,mode="out")),degree_distribution(tr,mode="out"),
     pch=20,xlab="degree",ylab=NA,type='h',ylim=c(0,.6))           
#13 vertices have no outgoing edges; these are the leaves.
#The mean out-degree equals the mean in-degree, 0.96
#i
sort(degree(tr),decreasing=T)     #Q has the highest degree
#j
sort(knn(tr)$knn, decreasing=T)   #I has avg. neighbor degree of 5
neighbors(tr,"I",mode="all")      #Just Q
#k
isg=induced_subgraph(tr,c("D","I","P","Q","U"))
set.seed(23);plot(isg)
plot(isg,layout=layout_as_star) #This is not working here
#l
#isg is a 4-star
#m
#Yes, a vertex in a tree can have 2 parents. Note, however, that
#we generally consider only rooted trees as having parents.
#No, a vertex in a rooted tree cannot have 2 parents, since the 
#parents must go back to the root thus creating a cycle.

