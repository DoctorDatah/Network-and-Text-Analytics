###################################################################################
#
# Lecture3.R
#
###################################################################################
# External Functions
###################################################################################
library(igraph)
library(igraphdata)
library(readxl)
###################################################################################
# Internal Functions
###################################################################################
graph_attr_from_df<-function(g,df) {
  df=as.data.frame(df)
  for (i in 1:ncol(df)) {
    if (class(df[,i])=="factor") df[,i]=as.character(df[,i])
    g=set_graph_attr(g,names(df)[i],df[1,i])
  }
  g
}
###################################################################################
# Save the environment 
###################################################################################
parSave=par(no.readonly = TRUE)
iSave=igraph_options(annotate.plot=T,vertex.size=15,edge.arrow.size=.5)
#igraph_options(iSave)  #Back to the old values  

PATH = "C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/"
###################################################################################
# Processing 
###################################################################################
# Bipartite Networks
###################################################################################
bip <- graph_from_literal(actor1:actor2:actor3,
                          movie1:movie2:movie3, actor1:actor2 - movie1,
                          actor2:actor3 - movie2,actor3-movie3)
V(bip)$type <- grepl("movie", V(bip)$name)    #Is it a movie?
#Alternative
V(bip)
V(bip)$type=c(F,F,F,T,T,T)

vertex_attr(bip)
vertex_attr_names(bip)
igraph_options(vertex.size=40,annotate.plot=F)
set.seed(23);plot(bip)
plot(bip, layout=layout_as_bipartite)


#Back to defaults
igraph_options(iSave)    
par(parSave)
save(bip, file="C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/bip.RData")
# save(bip,file="../data/bip.RData")
###################################################################################
# Problems
###################################################################################
#1: First book bipartite graph, page 114/116
bip=graph_from_literal(A-1,B-1:2:3:4,A-4,C-2:3:5,D-3:4:5)
V(bip)
V(bip)$type=c(F,T,F,T,T,T,F,T,F)
set.seed(23);plot(bip)
plot(bip, layout=layout_as_bipartite)
as_incidence_matrix(bip)

#2: Second book bipartite graph, page 117
bip2=graph_from_literal(A-1:2:3,B-2:3:4:5,C-4:6,D-5:6:7)
V(bip2)
V(bip2)$type=c(F,T,T,T,F,T,T,F,T,F,T)
set.seed(23);plot(bip2)
plot(bip2, layout=layout_as_bipartite,main="Bipartite Network")
as_incidence_matrix(bip2)

#projection of the bipartite graph onto induced graphs
(proj <- bipartite_projection(bip2))
par(mfrow=c(1,3))
set.seed(23);plot(bip2, layout=layout_as_bipartite,main="Bipartite Network")
plot(proj[[1]],main="Projection onto Letters")
plot(proj[[2]],main="Projection onto Numbers")
par(parSave)
#Neither is acyclic
###################################################################################
# Unnamed Graphs
###################################################################################
#Using this method for creating graphs will use the numbers as the ids of the
#vertices, rather than as their names. This will result in an unnamed graph
#unlike all other graphs we have created.
make_graph(c(2,3,3,5,2,5))
###################################################################################
# Import and Export Network Data: Adjacency Matrices and Edgelists
###################################################################################
#Load a previously created and saved on disk graph into R
load(file="C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/g.RData")

#load("../data/g.RData")
set.seed(23);plot(g)
g


(aa=as_adj(g))
graph_from_adjacency_matrix(aa,"undirected")

par(mfrow=c(1,2))
igraph_options(edge.arrow.size=.5,vertex.size=30) #Change the options

##Create a weighted graph from an adjacency matrix
A=matrix(c(0,2,1,2,0,.5,1,.5,0),3)
colnames(A)=c("Jon","Mary","Mike")   #Name the vertices
A
g2=graph_from_adjacency_matrix(A,"undirected",weighted=T)
g2
E(g2)$label=E(g2)$weight
set.seed(23);plot(g2)

#Create a weighted digraph from an adjacency matrix
A[3,1]=0;A[3,2]=0;A[2,1]=1;A
g2=graph_from_adjacency_matrix(A,"directed",weighted=T)
g2
E(g2)$label=E(g2)$weight
set.seed(23);plot(g2)
par(parSave)

#Edgelists
(ae=as_edgelist(g))        #Create an edgelist
graph_from_edgelist(ae,directed=F)

par(mfrow=c(1,3))
igraph_options(edge.arrow.size=.5,vertex.size=20) #Change the options

#Create an unnamed digraph from an edgelist
(A=matrix(c(1,2,3,4,3,4,6,7,7,3),ncol=2,byrow=T))
(g2=graph_from_edgelist(A))
set.seed(23);plot(g2)

#Create a named digraph from an edgelist
(A=matrix(c("Jane","Jess","Carol","Susan","Carol","Susan","Jacob","Joe","Joe","Carol"),
          ncol=2,byrow=T))
(g2=graph_from_edgelist(A))
set.seed(23);plot(g2)

#Create an undirected, named graph from an edgelist
(g2=graph_from_edgelist(A,F))
set.seed(23);plot(g2)
igraph_options(iSave)
par(parSave)
###################################################################################
# Import and Export Network Data: Data Frames
###################################################################################
#Put all the information for a graph into 3 data frames
(dfg=as_data_frame(g,"both"))
(dfgg=as.data.frame(graph_attr(g)))
#Create a graph from 3 data frames
(g2=graph_from_data_frame(dfg$edges,F,dfg$vertices))
(g2=graph_attr_from_df(g2,dfgg))

#Obtain data from Excel
(gtEdges=read_excel("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/test.xlsx",sheet=1))
(gtVertices=read_excel("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/test.xlsx",2))
(gtGraph=read_excel("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/test.xlsx",3))
#Create an undirected graph
gt=graph_from_data_frame(gtEdges,directed=F,gtVertices)
(gt=graph_attr_from_df(gt,gtGraph))
igraph_options(edge.arrow.size=.5,vertex.size=40) #Change the options
set.seed(23);plot(gt,main=gt$name)

#Create a digraph
gt=graph_from_data_frame(gtEdges,T,gtVertices)
(gt=graph_attr_from_df(gt,gtGraph))
set.seed(23);plot(gt,main=gt$name)
igraph_options(iSave)

#Graph from text files
laze=read.table("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/lazega_edges.txt")
laza=read.table("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/lazega_vertices.txt",header=T)
lazg=read.table("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/lazega_graph.txt",header=T)
lazega=graph_from_data_frame(laze,F,laza)
(lazega=graph_attr_from_df(lazega,lazg))
save(lazega,file= paste(PATH, "lazega.RData"))
###################################################################################
# Import and Export Network Data: Other Methods
###################################################################################
#Write a graph to an external file
?write_graph
#Edge list is the default, but this writes ids minus 1, no names
write_graph(g, paste(PATH,"ggraph.txt"))        
#This way we do get vertex names, and also the weights are stored
write_graph(g, paste(PATH,"ggraph.txt"),  "ncol")
#Import a graph from an external file with names and weights
read_graph(  paste(PATH,"ggraph.txt"), "ncol",   directed=F)

#Graphs available in igraphdata
data(package="igraphdata")
###################################################################################
# Components
###################################################################################
?components

g2=g
V(g2)$name=V(g2)$label
igraph_options(edge.arrow.size=.5,vertex.size=40) #Change the options
set.seed(23);plot(g2)
is_connected(g2)          #This graph is connected
components(g2)            #Only one component
g2=g2-edge(c("Jill|Jack","Pete|Mary","Jill|Pete","Jill|Joe"))
is_connected(g2)          #No longer connected
set.seed(23);plot(g2)
(co=components(g2))       #Now we have three components
set.seed(23);plot(g2, vertex.color=co$membership)

#For a digraph 
load("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/gd.RData")         #Load a digraph
set.seed(23);plot(gd)
is_connected(gd,mode="weak")       #Weakly connected
is_connected(gd,mode="strong")     #Strongly connected
components(gd,mode="strong")       #Just one component
gd2=gd-edge(get.edge.ids(gd,c("6","4","4","1")))   
#Alternative: gd2=gd-edge(c("6|4","4|1"))
set.seed(23);plot(gd2)
is_connected(gd2,mode="weak")      #Weakly connected
is_connected(gd2,mode="strong")    #Not strongly connected
distances(gd2,mode="out")          #Which vertices are the problem?
(co=components(gd2,mode="strong")) #Three components
set.seed(23);plot(gd2, vertex.color=co$membership,palette=rainbow(10))
###################################################################################
# Problem
###################################################################################
A=matrix(c(2,5,3,8,0,0,2,1,0,5,3,2,0,3,0,0),4)
colnames(A)=c("house","yard","town","car")
A
g2=graph_from_adjacency_matrix(A,weighted=T)
g2
E(g2)$label=E(g2)$weight
igraph_options(vertex.size=50) #Change the options
set.seed(23);plot(g2)
is.simple(g2)
is.connected(g2,"weak")
is.connected(g2,"strong")
distances(g2, weights=NA,mode="out")  #Normal distances
(co=components(g2,mode="strong"))
set.seed(23);plot(g2,vertex.color=co$membership)
#house is in a component by itself, while the other three form
#the giant component.
diameter(g2,weights=NA)
degree(g2,"house",mode="in")
(dfg=as_data_frame(g2))
#There are no attributes for vertices that we do not already know of,
#nor are there any attributes for the graph available.
igraph_options(iSave)

