library(igraph)
library(igraphdata)


load("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/g.RData")

summary(g)
set.seed(23);plot(g)
 

graph_attr(g)
vertex_attr(g)
edge_attr(g)

V(g)$gender
V(g)$gen = c(2,1,2,2,1,2)

V(g)$shape = "circle" 
V(g)[gen==2]$shape = "square" # Error was due to nulls. gotta need to initialize the shape variable anyway

V(g)[gen==2]$color = "lightblue"
V(g)[gen==1]$color = "pink"


E(g)$label = E(g)$group
E(g)[group == "family"]$color = 1
E(g)[group == "friends"]$color = 2
E(g)[group == "work"]$color = 3

V(g)$size = degree(g) * 9
set.seed(23);plot(g)


set.seed(23);plot(g,layout=layout_with_fr,main=g$name, pallete=rainbow(3))

