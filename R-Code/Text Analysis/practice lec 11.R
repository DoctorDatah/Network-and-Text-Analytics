###################################################################################
#
# Lecture11.R
#
###################################################################################
# External Functions
###################################################################################
library(readtext)
library(tm)
library(tidytext)          #For sentiments
library(dplyr)             #For joins
library(wordcloud)
library(wordcloud2)
#install.packages('textdata')

source("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/R-Code/Text Analysis/toDataFrame.R")
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
###################################################################################
# Save the environment 
###################################################################################
parSave=par(no.readonly = TRUE)
#par(parSave)
##################################
FilePath = "TextData/tmData/Covid19March1.txt"
co = VCorpus(PubMedSource(toDataFrame(FilePath)))
co
ptd = co[[1]]
inspect(ptd)

myStopWords = c(stopwords(), "covid19")
tf = termFreq(ptd)
class(tf)
sort(tf)

tf = termFreq(ptd,control = list( removePunctuation=T,
                                  stopwords=T,
                                  removeNumbers=T,
                                  stemming=T))
sort(tf)
tail(sort(tf))

# stemming 
stm = stemDocument(ptd)
class(stm)
inspect(stm)

tf2 = termFreq(stm, control = list(removePunctuation=T,
                                   stopwords=T,
                                   removeNumbers=T))
tf2
sort(tf2, decreasing = T)
##########################################################
get_sentiments("bing")
get_sentiments("afinn")
sentiments
class(sentiments)
nrc=get_sentiments("nrc")
class(nrc)

##########################################################
co
ptd
tf=termFreq(ptd,control=list(removePunctuation=T,stopwords=myStopWords,
                             removeNumbers=T))
tf
# we need data frame to make joins with sentiments
df = data.frame(word=names(tf),freq=tf)
being = get_sentiments('bing')
inner_join(df,being )

aff = get_sentiments("afinn")
inner_join(df,aff)

# Select all the words with fear sentiments
nrc = get_sentiments("nrc")
nrc$sentiment
fearNrc = nrc[nrc$sentiment=="fear",]
ptdFear = inner_join(df,fearNrc)
ptdFear

#################################################
## Problem
#################################################
# 1
tdm <- TermDocumentMatrix(co, control = list(removePunctuation = TRUE,
                                             stopwords = T,
                                             removeNumbers = TRUE))
tdm
# note if we have many documents, 
# we may remove sparse terms before creating the sparse matrix as follows
if(length(co)>1000)
  tdm = removeSparseTerms(tdm, sparse = 0.90)
as.matrix(tdm)
sp = removeSparseTerms(tdm,sparse = .9)
as.matrix(sp)


tdm <- TermDocumentMatrix(co, control = list(removePunctuation = TRUE,
                                             stopwords = T,
                                            removeNumbers = TRUE))

tdm
tdmM = as.matrix(tdm)
dim(tdmM)
tdmM[1:5,1:5]

# finding the word age 
colnames(tdmM)
rownames(tdmM)

tdmM["aged",]
idx=which(tdmM['aged',]!=0)
idx # 32064853 

co[[idx]] 
inspect(co[[idx]])
meta(co[[idx]])

# lets check in how many times it appear in that document
tdmM["aged",idx,drop=F]

idx= which(tdmM["local",]!=0)
tdmM["local",idx]
########################################################
sums = apply(tdmM, 1, sum)
head(sort(sums,decreasing = T))

unique(nrc$sentiment)

###### 
class(sums)
names(sums)

# converting to  a data frame
df = data.frame(word=names(sums), freq=sums)
df

#joy words in nrc
nrc$sentiment
class(nrc)
names(nrc)
fearwords  =nrc[nrc$sentiment=="fear",]
# 
all_fear = inner_join(df,fearwords)
fear70 = all_fear[all_fear$freq>70,]$word
fear70

# Find the indices of all documents that contain all 
# three of these words
colnames(tdmM)
rownames(tdmM)
idxr = which(rownames(tdmM) %in% fear70)
# clumsn
(idx=which(tdmM[idxr[1],]>=1&tdmM[idxr[2],]>=1&tdmM[idxr[3],]>=1))
condition_col1 = tdmM[idxr[1],]>0
condition_col2 = tdmM[idxr[2],]>0
condition_col3 = tdmM[idxr[3],]>0

idx=which(condition_col1 & condition_col2 & condition_col3)
tdmM[idxr,idx]

length(idx)
inspect(co[[idx[1]]])
lapply(co[idx],meta,"title")

# Word cloud
set.seed(123)
wordcloud(names(tf),tf,min.freq = 1,scale = c(2.5,.2),
          colors = c(4,3,2),
          random.order = F)

