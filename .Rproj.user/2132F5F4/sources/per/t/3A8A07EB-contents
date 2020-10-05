###################################################################################
#
# Lecture4.R
#
###################################################################################
# External Functions
###################################################################################
library(igraph)
library(igraphdata)
###################################################################################
# Internal Functions
###################################################################################
weight_distribution<-function(g,cumulative = FALSE,...) {
  if (!is_igraph(g)) {
    stop("Not a graph object")
  }
  cs <- strength(g,...)
  hi <- hist(cs, -1:max(cs), plot = FALSE)$density
  if (!cumulative) {
    res <- hi
  }
  else {
    res <- rev(cumsum(rev(hi)))
  }
  res
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
#Trees
###################################################################################
#Tree
treeg=graph_from_literal(8:9:10:11:12,8-9,8-10-11,10-12)
set.seed(23);plot(treeg)
plot(treeg,layout=layout_as_tree)

#Directed tree, no root
treedg=graph_from_literal(8:9:10:11:12,8-+10,9-+10-+12,11-+12)
set.seed(23);plot(treedg)
#This only looks good for rooted trees or undirected trees
plot(treedg,layout=layout_as_tree)

#Rooted tree
treeg=graph_from_literal(8:9:10:11,8-+9,8-+10-+11)
set.seed(23);plot(treeg)
plot(treeg,layout=layout_as_tree)
#Don't need arrows for a rooted tree
plot(treeg,layout=layout_as_tree,edge.arrow.mode=0)

(mt=make_tree(7, children=2))
V(mt)$name=1:7
plot(mt,layout=layout_as_tree)

#Forest
plot(union(treeg,mt),layout=layout_as_tree,main="Forest")

#ancestor, descendant, parents, children, leaf, depth

(ms=make_star(8,mode="out"))    #8-star
par(mfrow=c(1,2))
plot(ms)
plot(ms,layout=layout_as_star)
par(parSave)
###################################################################################
#Degree
###################################################################################
?degree
?igraph::degree

load("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/karate.RData")
#Load karate network
summary(karate)
set.seed(23); plot(karate)
degree(karate)
sort(degree(karate),decreasing=T)

#Mean degree
mean(degree(karate))             #c
2*gsize(karate)/gorder(karate)   #c=2m/n

#Degree distribution

# with simple tree
table(degree(mt))
fd_=table(degree(mt))/gorder(mt)
#This function includes degree zero and all in-between values; otherwise similar
degree_distribution(mt)
#plot a histogram of the degrees
hist(degree(mt))
#Plot the degree distribution
plot(0:max(degree(mt)),degree_distribution(mt),pch=20,xlab="degree",
     ylab=NA,type='h')   


# For Karate Tree #
table(degree(karate))
(fd=table(degree(karate))/gorder(karate))  #Degree distribution
#This function includes degree zero and all in-between values; otherwise similar
degree_distribution(karate)
par(mfrow=c(1,2))
#plot a histogram of the degrees
hist(degree(karate))
#Note how there are 3 distinct groups! The 2 main people who are connected
#the most, a highly connected group consisting of actors 2, 3, and 33,
#and a group with everyone else. See the original plot.

#Plot the degree distribution
plot(0:max(degree(karate)),degree_distribution(karate),pch=20,xlab="degree",
     ylab=NA,type='h')           

#Plot the weighted degree distribution
E(karate)$weight
table(strength(karate))
hist(strength(karate))
plot(0:max(strength(karate)),weight_distribution(karate),pch=20,xlab="strength",
     ylab=NA,type='h')           
par(parSave)

#Average neighbor degree
knn(mt,weights = NA)
sort(knn(mt,weights=NA)$knn,decreasing=T)   #This is all we need
#Check for 3
neighbors(mt,"3")
mean(degree(mt)[as.numeric(neighbors(mt,"3"))]) # not getting same i think because mt is directerd and parrent is not considered as neibours 

knn(karate,weights=NA)
sort(knn(karate,weights=NA)$knn,decreasing=T)   #This is all we need
#Check for Mr. Hi
neighbors(karate,"Mr Hi")
mean(degree(karate)[as.numeric(neighbors(karate,"Mr Hi"))])

#Directed network
load("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/aidsblog.RData")
  #Load aidsblog data
summary(aidsblog)
set.seed(23);plot(aidsblog)
igraph_options(vertex.size=5,edge.arrow.size=.5)
set.seed(23);plot(aidsblog)
(di=degree(aidsblog,mode="in"))  #most vertices have only one outgoing edge
(do=degree(aidsblog,mode="out")) #most vertices have no outgoing edges
#mean degrees
mean(di)                         #mean in-degree
mean(do)                         #mean out-degree
gsize(aidsblog)/gorder(aidsblog) #Cin=Cout=m/n

#Degree distribution
table(degree(aidsblog,mode="out"))
(fd=table(degree(aidsblog,mode="out"))/gorder(aidsblog))  #Degree distribution
degree_distribution(aidsblog,mode="out")
par(mfrow=c(1,2))
#plot a histogram of the out-degrees
hist(degree(aidsblog,mode="out"))

#Plot the out-degree distribution
plot(0:max(degree(aidsblog,mode="out")),degree_distribution(aidsblog,mode="out"),
     pch=20,xlab="degree",ylab=NA,type='h')           

par(parSave)
