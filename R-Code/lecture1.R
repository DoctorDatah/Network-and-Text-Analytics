###################################################################################
#
# Lecture1.R
#
###################################################################################
# External Functions
###################################################################################
library(igraph)
###################################################################################
# Internal Functions
###################################################################################
###################################################################################
# Save the environment 
###################################################################################
parSave=par(no.readonly = TRUE)
###################################################################################
# Processing 
###################################################################################
?igraph
help(package="igraph")
#colors
colors()
palette()
###################################################################################
# Networks in R - Basics
###################################################################################
# A simple undirected graph
g <- graph_from_literal(Jon-Mary, Jon-Mike)
g
set.seed(23);plot(g,main="G Undirected")

# A simple digraph
dg <- graph_from_literal(Jon-+Mary, Jon-+Mike)
dg
set.seed(23);plot(dg,main="G a Digraph")

V(g)                      #vertices of g
E(g)                      #Edges of g
E(dg)
summary(g)                #Summary of g
gorder(g)                 #Order of g (# of vertices)
gsize(g)                  #Size of g (# of edges)
incident(g,"Jon")         #All edges incident on Jon
incident(g,"Mary")        #All edges incident on Mary
###################################################################################
# Creating Network Graphs
###################################################################################
# A simple undirected graph
g <- graph_from_literal(Alice-Bob-Cecil-Alice, Daniel-Cecil-Eugene,
                        Cecil-Gordon)
g
V(g)                      #Show its vertices
set.seed(23);plot(g)

# Another undirected graph, ":" notation
g2 <- graph_from_literal(Alice-Bob:Cecil:Daniel, Cecil:Daniel-Eugene:Gordon)
g2
set.seed(23);plot(g2)

# A directed graph
g3 <- graph_from_literal(Alice +-+ Bob --+ Cecil +-- Daniel,
                        Eugene --+ Gordon:Helen)
g3
set.seed(23);plot(g3)

# A graph with isolate vertices
g4 <- graph_from_literal(Alice -- Bob -- Daniel, Cecil:Gordon, Helen)
g4
V(g4)
set.seed(23);plot(g4)

# "Arrows" can be arbitrarily long
g5 <- graph_from_literal(Alice +---------+ Bob,Bob++Helen)
g5
set.seed(23);plot(g5)

# Special vertex names
g6 <- graph_from_literal("+" -- "-", "*" -- "/", "%%" -- "%/%")
g6
V(g6)
set.seed(23);plot(g6)
###################################################################################
# Practice Graphs
###################################################################################
g <- graph_from_literal(1-2,1-3,2-3,2-4,3-5,4-5,4-6,4-7,5-6,6-7)
set.seed(23);plot(g)
summary(g)        #Show a summary
V(g)              #Show its vertices
E(g)              #Show its edges
gorder(g)         #Order of a graph (# of vertices)
gsize(g)          #Size of a graph (# of edges)

dg <- graph_from_literal(Sam-+Mary, Sam-+Tom, Mary++Tom)
dg
set.seed(23);plot(dg)
V(dg)$name        #Vertex names
V(dg)$name <- c("A", "B", "C")  #Change the vertex names
V(dg)
set.seed(23);plot(dg)

dg <- graph_from_literal(Sam-+Mary, Sam-+Tom, Mary++Tom)
V(dg)$label <- c("A", "B", "C")    #Overrides names in plot
summary(dg)
V(dg)$name 
set.seed(23);plot(dg)
dg=delete_vertex_attr(dg, "label") #Remove the labels
V(dg)$nums=1:3                     #Create a different attribute
summary(dg)
###################################################################################
# iGraph Options
###################################################################################
?igraph.plotting
iSave=igraph_options(edge.arrow.size=.5,vertex.color="red") #Change the options
set.seed(23);plot(dg)
igraph_options(iSave)    #Back to defaults
###################################################################################
# Multi-Graphs
###################################################################################
set.seed(23);plot(g)
g2=g+vertices(8,9,10)
set.seed(23);plot(g2)
g2=g2+edges(8,9,9,10,5,8,5,8,5,8,9,10,6,6)
set.seed(23);plot(g2)
incident(g2,c(9,10))
E(g2)$weight <- 1        #Initialize weight attribute
#Create a simple graph from the multi-graph by aggregating the weights
#While loops are removed
gs <- simplify(g2)
E(gs)$label=E(gs)$weight    #Edge label
par(mfrow=c(1,2))
set.seed(23);plot(g2);set.seed(23);plot(gs)
par(parSave)
summary(gs)
