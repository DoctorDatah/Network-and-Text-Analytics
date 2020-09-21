###############################################
ifrm <- function(obj, env = globalenv()) {
  obj <- deparse(substitute(obj))
  if(exists(obj, envir = env)) {
    rm(list = obj, envir = env)
    print("Deleted")
  }
}
################################################
library(igraph)

# Question 1
# Creating Graph
g <- graph_from_literal(A-Z, A-B, A-X, A-Y,B-Y, B-X, C-X,C-Y )
set.seed(23);plot(g,main="G Undirected")

# Order of Graph  
gorder(g) # 6

# Size of Graph
gsize(g) # 8

# Y is incident on how many edges?
incident(g,"Y") # 3

# Changing Labels 
g2 = g
V(g2)$name # [1] "A" "Z" "B" "X" "Y" "C"
V(g2)$lable = c("A","Z", "B", "?", "Y", "!")
V(g2)$lable

#e # Adding Vertex
g3 = g2+vertex("D","E")

V(g3)$name # [1] "A" "Z" "B" "X" "Y" "C" "D" "E"
V(g3)$lable = c("A","Z", "B", "?", "Y", "!","D","E")

g3 = g3+edge("D","A","E","A")
set.seed(23);plot(g3,main="G Undirected")

gs <- simplify(g3)
set.seed(23);plot(gs,main="G Undirected")


# f,g,h 
ifrm(g4)
g4 = g3
get.edge.ids(g4, c("D","A"  ,"E","A"))
E(g4)$weight[9:10] = 1
E(g4)$weight[0:8] = 2
E(g4)$weight

E(g4)$label=E(g4)$weight    #Edge label

par(mfrow=c(1,2))
set.seed(23);plot(g3);  set.seed(23);plot(g4)


################################################

# Question 2
# Creating Graph
ifrm(g)
g <- graph_from_literal(W1+-W2,W1+-W3,W2-+W5,W2+-W6,W3-+W5,W3+-+W10,W6+-+W7,W8,W9)
set.seed(23);plot(g)

iSave=igraph_options(label.color="pink", edge.arrow.size=.5,edge.color="black",vertex.color="pink")
set.seed(23);plot(g)


# Order of Graph  
gorder(g) # 9

# Size of Graph
gsize(g) # 9

V(g)  #Show its vertices
E(g)  # Show Edges

# Adding new Edges
ifrm(g2b)
g2b = g + edge("W8","W5","W8","W9","W8","W9")
iSave=igraph_options(label.color="pink", edge.arrow.size=.5,edge.color="black",vertex.color="pink")
set.seed(23);plot(g2b)
summary(g2b)
gorder(g2b) # 9
gsize(g2b) # 12
E(g2b)

# Simplifying
E(g2b)$weight <- 1  #Initialize weight attribute
g2bs <- simplify(g2b)
E(g2bs)$label=E(g2bs)$weight    #Edge label
par(mfrow=c(1,2))
set.seed(23);plot(g2b);set.seed(23);plot(g2bs)
par(parSave)
summary(g2bs)
