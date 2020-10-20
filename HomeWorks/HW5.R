###################################################################################
#
# HW5.R
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
#options(stringsAsFactors = F)
#igraph_options(iSave)  #Back to the old values 
#par(parSave)
###################################################################################
# Processing 
###################################################################################
#1
load("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/g.RData")

#a
summary(g)
set.seed(23);plot(g)
#b
graph_attr(g)
vertex_attr(g)
edge_attr(g)
#c
gen=as.numeric(factor(V(g)$gender))
gen=as.numeric(V(g)$gender=="M")+1     #Alternative method
#d
V(g)$shape=c("circle","square")[gen]
V(g)$color=c("pink","lightblue")[gen]
#e
E(g)$label=E(g)$group
#f
E(g)$color=as.numeric(factor(E(g)$group))
#g
V(g)$size=15*sqrt(degree(g))
#h
set.seed(23);plot(g, layout=layout_with_fr, palette=rainbow(3),main=g$name)
#i
legend("topright",c("female","male"),col=c("pink","lightblue"),pch=c(19,15))

