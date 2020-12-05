###################################################################################
# Project: Pakistani Students Health papers form PubMed
# By: Malik Hassan Qayyum
###################################################################################
library(readtext)
library(tm)
library(tidytext)          #For sentiments
library(dplyr)             #For joins
library(wordcloud)
library(wordcloud2)
library(phm)               #Phrase mining
library(sur)               #Skewness ratios
###################################################################################
# Loading Custom Libraries
###################################################################################
source("lib/toDataFrame.R")
source("lib/PubMed_Utils.R")
source("lib/PhraseMining_Utils.R")
source("lib/SentimentAnalysis_Utils.R")

###################################################################################
# Save the environment 
###################################################################################
parSave=par(no.readonly = TRUE)
#par(parSave)
###################################################################################
# Loading Data
###################################################################################
FILE_PATH = "Data/pubmed-pakistanis-set.txt"
(co=VCorpus(PubMedSource(toDataFrame(FILE_PATH))))
# 129 Documents
###################################################################################
# Phrases
###################################################################################
# Creating
pd = phraseDoc(co, min.freq=3,silent=T)
pdm = as.matrix(pd)
dim(pdm)
pdm[1:8,1:8]
# All Principal Phrases
row.names(pdm)
# High Frequency Phrases
(highFreq=freqPhrases(pd,5))

# Stop Words
NAMES_PATH = "Data/names.csv"
OTHER_PHRASES_PATH = "Data/other_phrases.csv"
NAMES = read.csv(NAMES_PATH)$Names
OTHER_PHRASES = read.csv(OTHER_PHRASES_PATH)$Phrases
STOP_PHRASES = c(NAMES,OTHER_PHRASES)

pd = phraseDoc(co,min.freq=3,silent=T,sp = STOP_PHRASES)
pdm = as.matrix(pd)
dim(pdm)
# High Frequency Phrases
(highFreq=freqPhrases(pd,5))

###################################################################################
# Clustering
###################################################################################
set.seed(1)
k=3  #Number of clusters
km = kmeans(t(pdm), k)
km$size

#Most frequent phrases in clusters (note: average frequencies!)
# cluster 1
clust1_hfPfrases = head(sort(km$centers[1,],decreasing=T),100)
df = data.frame(word=names(clust1_hfPfrases),freq=clust1_hfPfrases)
wordcloud2(df,size=.2, shape='cloud')

# cluster 2
clust2_hfPfrases = head(sort(km$centers[2,],decreasing=T),50)
df = data.frame(word=names(clust2_hfPfrases),freq=clust2_hfPfrases)
wordcloud2(df,size=.2, shape='cloud')

# cluster 3
clust3_hfPfrases = head(sort(km$centers[3,],decreasing=T),50)
df = data.frame(word=names(clust3_hfPfrases),freq=clust3_hfPfrases)
wordcloud2(df,size=.2, shape='cloud')

# Indices for all cluster
idx1 = as.integer(which(km$cluster==1))
idx2 = as.integer(which(km$cluster==2))
idx3 = as.integer(which(km$cluster==3))

# Cluster 2 seems the cluster of my interest
(co2 = co[idx2])
pd2 = phraseDoc(co2, min.freq=3,silent=T,sp = STOP_PHRASES)
pdm2 = as.matrix(pd2)
dim(pdm2)

# However, before proceeding with the cluster 2. lets explore the documents 
# containing breast cancer

###################################################################################
# Checking the documents contain Phrase breast cancer
###################################################################################
# Finding docs
docsBC = getDocs(pd,"breast cancer")
(docBC_ids =as.integer(names(docsBC[,])))
pd_BC = phraseDoc(co[docBC_ids], min.freq = 3,sp=STOP_PHRASES)
pdm_BC = as.matrix(pd_BC)
dim(pdm_BC)
# Titles 
lapply(co[docBC_ids],meta,"title")
lapply(co[docBC_ids],meta)
# HF Phrases
hf_BC_phrases = freqPhrases(pd_BC, 50)
df = data.frame(word=names(hf_BC_phrases),freq=hf_BC_phrases)
wordcloud2(df,size=.3, shape='cloud')

# sentiment 
(sumSent_BC = get_overall_sentiments(co[docBC_ids]))
mean(sumSent_BC)
median(sumSent_BC)

###################################################################################
# Documents without principal phrases
###################################################################################
colFreq=apply(pdm,2,sum)
sum(colFreq==0)
# No such Documents

###################################################################################
# let's proceed with cluster number 2
# Sentiment Analysis
###################################################################################

sumSent2 = get_overall_sentiments(co2)
hist(sumSent2,col="violet",main="Sentiments by Document",xlab="Sentiment",
     breaks=15)
skew.ratio(sumSent2)
mean(sumSent2)
median(sumSent2)

###################################################################################
# Best Document
###################################################################################
## 5 High Frequency Phrases
bd=bestDocs(co2,nDocs = 10,nPhrases = 5, STOP_PHRASES)
(topDocIndicsAndFreq = get_doc_oldID_and_num_hfphrases(co=bd))
(id = topDocIndicsAndFreq[1]) 

# Checking this document in original cluster 2 corpus
meta(co[[id]])
inspect(co2[[id]])

# All principal phrases 
phrases = pdm[pdm[,id] != 0,id, drop=F]
df = data.frame(word=row.names(phrases),freq=phrases)
wordcloud2(df,size=.3, shape='cloud')

###################################################################################
## 10 High Frequency Phrases
bd=bestDocs(co2,nDocs = 10,nPhrases = 10, STOP_PHRASES)
(topDocIndicsAndFreq = get_doc_oldID_and_num_hfphrases(co=bd))


# Same Document at the Top
ChosenDocID = topDocIndicsAndFreq[1]

###################################################################################
# Sentiment Analysis of the Doc
###################################################################################

get_overall_sentiments(co2[ChosenDocID])


######## END ######################################################################


