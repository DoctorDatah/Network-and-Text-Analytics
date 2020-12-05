###################################################################################
#
# Lecture13.R
#
###################################################################################
# External Functions
###################################################################################
library(readtext)
library(tm)
library(phm)               #Phrase mining

source("R-Code/Text Analysis/toDataFrame.R")
###################################################################################
# Internal Functions
###################################################################################
PubMedSource<-function (x) 
{
  stopifnot(all(!is.na(match(c("PMID", "Title","Abstract","Date","Author"), 
                             names(x)))))
  SimpleSource(length = nrow(x), reader = readPub, content = x, 
               class = "PubMedSource")
}
readPub<-function(elem, language, id) {
  PlainTextDocument(elem$content[,"Abstract"], id=elem$content[,"PMID"],
                    language=language, author=elem$content[,"Author"], 
                    datetimestamp=elem$content[,"Date"],
                    title=elem$content[,"Title"])
}
getElem.PubMedSource<-function (x) 
  list(content = x$content[x$position, ], uri = NULL)

bestDocs<-function(co,num=3,n=10) {
  pd=phraseDoc(co, min.freq=3,silent=T)
  mostf=freqPhrases(pd,n)
  pdm2=getDocs(pd,names(mostf))
  colcounts=sort(apply(pdm2,2,function(x) sum(x>0)),decreasing=T)
  d=as.integer(names(head(colcounts,num)))
  co2=co[d]
  for (i in 1:num) {
    meta(co2[[i]],"oldID")=d[i]
    meta(co2[[i]],"hfPhrases")=colcounts[i]
  }
  co2
}
###################################################################################
# Save the environment 
###################################################################################
parSave=par(no.readonly = TRUE)
#par(parSave)
###################################################################################
# Processing 
###################################################################################
#Clustering
###################################################################################
m=matrix(c(0,1,0,0,0,1,0,1,0,1,1,3,1,2,0,2),4)
colnames(m)=1:4;rownames(m)=c("A","B","C","D")
m

set.seed(1)
k=2   #Number of clusters
km = kmeans(t(m), k)
class(km)
#"cluster" field contains the cluster each document is assigned to
km$cluster
#"centers" field contains a matrix with the clusters as rows and terms as columns
#with the average frequency of each term in the cluster
km$centers

#Document indices in cluster 1
which(km$cluster==1)
#Document indices in cluster 2
which(km$cluster==2)

#Number of documents in cluster 1
km$size[1]

#Most frequent phrases in cluster 1 (note: average frequencies!)
head(sort(km$centers[1,],decreasing=T))
#Most frequent phrases in cluster 2
head(sort(km$centers[2,],decreasing=T))

#Cluster by occurrence of a term only, rather than its frequency
m2=apply(m!=0,2,as.numeric)
rownames(m2)=rownames(m)
m2
set.seed(10)
k=2   #Number of clusters
km2 = kmeans(t(m2), k)
which(km2$cluster==1)
which(km2$cluster==2)
km2$size
###################################################################################
#Problem
###################################################################################
co=VCorpus(PubMedSource(toDataFrame("TextData/tmData/Covid19March1.txt")))

pd=phraseDoc(co)

pdm=as.matrix(pd)

set.seed(123)
k=6   #Number of clusters
km = kmeans(t(pdm), k)

#Sizes
km$size

#Indices of documents in cluster 6
(idx=which(km$cluster==6))

#Nonzero entries of the centroid for cluster 6
km$centers[6,km$centers[6,]!=0]

#Matrix with phrases that occur in documents in cluster 6 by those documents
ap=apply(pdm[,idx,drop=F],1,sum)!=0
cl6=pdm[ap,idx,drop=F]
class(cl6)
#Most frequent phrases in cluster 6 with their frequencies
head(sort(apply(cl6,1,sum),decreasing=T))
#Alternative
head(sort(km$centers[6,],decreasing=T))*km$size[6]

#Most frequent phrases in cluster 6 with their average frequencies
head(sort(km$centers[6,],decreasing=T))
#Alternative
head(sort(apply(cl6,1,sum),decreasing=T)/km$size[6])

#Create a corpus with all documents in cluster 6
(c6=co[idx])
inspect(c6[[1]])
meta(c6[[1]])
#Title: [Recommendations for the regulation of medical practices of
#burn treatment during the outbreak of the coronavirus disease 2019]

