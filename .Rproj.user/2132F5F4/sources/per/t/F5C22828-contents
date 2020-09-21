library(igraph)

g <- graph_from_literal(A-B:B:B,A-C:C:C,A-D:D,D-B-E,D-G-F:F,F-B,simplify=F)
set.seed(23);plot(g)

degree(g,"B")
# 6

# Only 1 neibour "E" has degree 1
# 3 neibours with single connection
neighbors(g,"B","all")


E(g)$weight <- 1  #Initialize weight attribute
g2bs <- simplify(g)
E(g2bs)$label=E(g2bs)$weight    #Edge label
par(mfrow=c(1,2))
set.seed(23);plot(g);set.seed(23);plot(g2bs)

degree(g2bs,"B") # 4
strength(g2bs,"B") # 6

# degree now hwere just based on one connection with the other vertics

# Question No 2
# degree 3 in is 1
# degree 3 out is 3

# 1-3-4-1-2-7-1 
# has repeated vertics 
# no repeated edge 
# circuit
# Length is 6

# yes Weakly connected

# yes its DAG as it shows Acyclic property. we can't go back to previous vertics
