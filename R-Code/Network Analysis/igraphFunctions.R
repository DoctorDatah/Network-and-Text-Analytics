###################################################################################
#
# igraphFunctions.R - Useful igraph functions
#
###################################################################################
library(help="igraph")    #Get all functions in alphabetical order
###################################################################################
# Lecture 1
###################################################################################
graph_from_literal(.)     #Create graph (vertex ids created in order of occurrence)
V(g)                      #vertices of g
E(g)                      #Edges of g
summary(g)                #Summary of g
gorder(g)                 #Order of a graph (# of vertices)
gsize(g)                  #Size of a graph (# of edges)
incident(g,v)             #Find all edges incident on the vectors in v
V(g)$name                 #Names of the vertices
V(g)$label                #Overrides names in plot
V(g)$a                    #Vertex attribute a
delete_vertex_attr(g,"a") #Delete vertex attribute a
?igraph.plotting          #Show all the options for plotting graphs
igraph_options(.)         #Set options for plotting graphs
vertices(v);edges(e)      #For +/- vertices or edges to graphs
E(g)$weight               #A vector with weights for the edges
simplify(g)               #Transform a multi-graph to a simple, aggregating weights
E(g)$label                #Labels to appear on the edges in a plot 
get.edge.ids(g,e)         #Find the indices of edges given in e