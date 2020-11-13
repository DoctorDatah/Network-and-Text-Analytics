##### Quiz 3 ######
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


data("kite")
summary(kite)
# Undirtected graph 
#  order:10 size:18
gorder(kite)
gsize(kite)
set.seed(23);plot(kite)

?kite
# Krackhardt's kite is a fictionary social network with ten actors. 
# It is the smallest graph for which the most central actor is different 
# according to the three classic centality measures: degree, closeness and betweenness.
graph_attr(kite)


edge_density(kite) # 0.4
vertex_connectivity(kite)   #1
articulation_points(kite) # I and H

kite2 = kite - vertex("I")
vertex_connectivity(kite2) 

kite2 = kite - vertex("H")
vertex_connectivity(kite2) 
# vertex connectivity becomes 0

cfg=cluster_fast_greedy(kite)
length(cfg)              #Number of subgraphs = 3
sizes(cfg)               
membership(cfg)
# plotting partitioned sub graph
set.seed(23);plot(cfg,kite,main="A Partitioned Graph")

# changing to 2 subgraphs
(ca=cut_at(cfg,2))     #Membership if we only want 2 subgraphs
col=c("red","blue")[ca]
set.seed(23); plot(kite,vertex.color=col)
# it does not look like good good partitions. looks non symmetric. 3 groups w ere better 

#Split
subs = getSubs(kite,ca)
s1 = subs[[1]]
s2 = subs[[2]]
edge_density(s1) # 0.4
edge_density(s2) # 1
# the 2nd one is dense graph

set.seed(23);plot(s1)
# it does plot but the other vertics that are not present in subgraph1 are also get ploted
graph_attr(s1)

sn = delete_graph_attr(s1, "layout")
set.seed(23);plot(sn)
# removing layout attribute fixes the problem

V(kite)
V(sn)
sn$layout = graph_attr(kite)$layout[c(1,3,6,8:10),]
par(mfrow=c(1,2))
set.seed(23);plot(sn)
set.seed(23); plot(kite,vertex.color=col)




