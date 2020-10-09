library(igraph)
library(igraphdata)


load("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/Data/ lazega.RData")
lazega

graph_attr(lazega)
vertex_attr(lazega)

V(lazega)[Office == 1]$color = "Red"
V(lazega)[Office == 2]$color = "Green"
V(lazega)[Office == 3]$color = "Blue"

V(lazega)$shape = "circle" 
V(lazega)[Practice==2]$shape = "square"

V(lazega)$size = V(lazega)$Years / 2

V(lazega)$label = V(lazega)$Seniority

V(lazega)[Gender==1]$label.color = "black"
V(lazega)[Gender==2]$label.color = "pink"


set.seed(23); plot(lazega)

