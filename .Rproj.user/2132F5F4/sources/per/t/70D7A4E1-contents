###################################################################################
#
# Lecture4B.R
# This is the code for the problem at the end of lecture 4
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
#options(stringsAsFactors = F)
#igraph_options(iSave)  #Back to the old values 
#par(parSave)
###################################################################################
# Processing 
###################################################################################
#Problem
###################################################################################
#1
load("../data/aidsblog.RData")   #Load aidsblog data
igraph_options(vertex.size=5,edge.arrow.size=.5)
set.seed(23);plot(aidsblog)
(di=degree(aidsblog,mode="in"))  #most vertices have only one outgoing edge
(do=degree(aidsblog,mode="out")) #most vertices have no outgoing edges

#Degree distribution
table(di)
table(do)
degree_distribution(aidsblog,mode="in")
degree_distribution(aidsblog,mode="out")
par(mfrow=c(1,2))
#Plot the in-degree distribution
plot(0:max(degree(aidsblog,mode="in")),degree_distribution(aidsblog,mode="in"),
     pch=20,xlab="degree",ylab=NA,type='h',main="In-degrees")
plot(0:max(degree(aidsblog,mode="out")),degree_distribution(aidsblog,mode="out"),
     pch=20,xlab="degree",ylab=NA,type='h',main="Out-degrees")           
#Most blogs are being linked to by one other blog, while a few of them are
#being linked to by up to six blogs. Most blogs do not link to any other blogs at
#all, but some of them link to many, even to as much as 43 other blogs.
par(parSave)
igraph_options(iSave)

#2
load("../data/g.RData")
set.seed(23); plot(g,edge.label=E(g)$weight)
mean(strength(g))
plot(0:max(strength(g)),weight_distribution(g),pch=20,xlab="strength",ylab=NA,
     type='h')           

#Average neighbor degree
knn(g,weights=NA)
sort(knn(g,weights=NA)$knn)
plot(degree(g),knn(g,weights=NA)$knn,xlab="Degree",ylab="Avg. Neighbor Degree",
     main="Degree vs. Average Neighbor Degree")
#Trend is down, so vertices with a high degree tend to have a low average neighbor
#degree and vice versa
