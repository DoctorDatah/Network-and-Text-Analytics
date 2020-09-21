###################################################################################
#
# HW2 sol.R
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
#1
g = graph_from_literal(A-B,C-X, C-Y,A-X, A-Y, A-Z,B-X,B-Y)
V(g)
set.seed(23);plot(g)
#a
E(g)$weight # Null
E(g)$weight=E(g)  #Let weight be the internal value of the edges
E(g)$weight # 1 2 3 4 5 6 7 8
#b
(h=induced_subgraph(g,c('A','B','X','Y')))
par(mfrow=c(1,2))
set.seed(23);plot(g,main="Original");plot(h, main="Induced Subgraph")
par(mfrow=c(1,1))

#c
#Graph attribute
g$name="Orderd Graph"
g$description  = "Same graph as asignment one just having ordered vertics"
#Vertex attributes
V(g)$color = c('red','pink','black','white','gray','yellow')

set.seed(23);plot(g, main=g$name ,  sub=g$description  )

#d
neighbors(g, 'A')

#e
degree(g,'Y')  #3
strength(g,'Y') #17

#f
# B-Y-A-X-B-A-Z
# No repeated edge 
# Repeated Vertic  # Hence trail 

# A-X-B-Y-A? 
# Same Start and end vertic 
# No repeated edge
# No repeated vertic # Hence cycle 

# B-A-Y-C-X-A-B? 
# Same Start and end vertic 
# repeated edge (B-A, A-B)
# repeated vertic # Hence walk #  not cycle must be a trail, it is not

# What is the length of the last one?
# 6


#g 
distances(g,'B','Y', weights=NA)  #1       
shortest.paths(g,'B','Y', weights=NA)

# weighted distance 
distances(g,'B','Y')  #4   
shortest.paths(g,'B','Y')


#h
#Undirected diameter
diameter(g,weights=NA) #3

#i
strength(g)
strength(g, weights=NA)
diameter(g)


#2
gd <- graph_from_literal(1-+2,1-+3,3-+2,3+-+4,4-+2,2-+5)
E(gd) 
E(gd)$weight = c(20,2,5,5,3,1,3)
E(gd)$label =E(gd)$weight
set.seed(34);plot(gd,main="Digraph")
  
#a
degree(gd,3,mode="in") # "3" labeld vertics | 2 edges  # EDGE 3 AND 3 are incident in 
strength(gd,3,mode="in") # 3 | 5  # overall 3+2 =5 combined weight of edgies

#b
# 3-4-2-3
# non of that can't go from 2-3, so its nothing

#c
distances(gd,2,4,weights=NA,mode="out") # no

#d 
distances(gd,1,2,weights=NA,mode="out") # 1-2
shortest_paths(gd,1,2,weights=NA,mode="out")

#e
distances(gd,1,2,mode="out") # 6 #  Path 1-3-4-2
shortest_paths(gd,1,2,mode="out")
#f
is_connected(gd,mode="weak") # true 
is_connected(gd,mode="strong") # False

#g
distances(gd, weights=NA,mode="out")  #Normal distances
distances(gd,mode="out")              #Distances using weights instead
# 1-5 is the longest distance path.


#h
#Diameter (directed)
diameter(gd, weights=NA) # 2
#i
diameter(gd) #11


