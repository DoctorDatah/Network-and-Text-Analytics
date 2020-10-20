library(igraph)
library(igraphdata)

parSave=par(no.readonly = TRUE)
iSave=igraph_options(annotate.plot=F,vertex.size=10,edge.arrow.size=.1)

data("enron")
e = enron

# a
summary(e)
gsize(e) # 125409
gorder(e) # 125409


# b
# Simplifying
E(e)$weight = 1
en <- simplify(e)
E(en)$label=E(en)$weight 
summary(en)
gsize(en) # 3010
gorder(en) # 184
# Yes size has decreased alot

#c
V(en)$name = V(en)
V(en)$name

#d
plot(en, xlim=c(-.8,.8),ylim=c(-.8,.8))

#e
par(mfrow=c(1,2))
plot(0:max(degree(en,mode = 'in')),degree_distribution(en,mode = 'in'),pch=20,xlab="degree",
     ylab=NA,type='h')  
plot(0:max(degree(en,mode = 'out')),degree_distribution(en,mode = 'out'),pch=20,xlab="degree",
     ylab=NA,type='h')
par(parSave)
# many vetrics from in degree 5 to 25 and a few ones have degree 30 to 60 
# Similarly, many vetrics from in degree 0 to 30 and a few ones have degree 40 to 50 
# and a couple of from 50 t0 100

# f
di = sort(degree(en, mode = 'in'),decreasing =T)
idx=order(di, decreasing =T)[50:70]

ind = induced.subgraph(en,idx)
plot(ind, xlim=c(-.8,.8),ylim=c(-.8,.8))

# g
(co=components(ind,mode="strong"))
set.seed(23);plot(ind, vertex.color=co$membership,palette=rainbow(6))

# h
dfv=as_data_frame(ind,"vertices")
dfe=as_data_frame(ind,"edges")
lst = c(dfv,dfe)
lst
