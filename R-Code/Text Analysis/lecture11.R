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
###################################################################################
# Processing 
###################################################################################
# Term Frequency of a Single Document
###################################################################################
#We will use publications about covid-19 from March 1
co=VCorpus(PubMedSource(toDataFrame("TextData/tmData/Covid19March1.txt")))
ptd=co[[1]]    #First document in the corpus
ptd
inspect(ptd)

#Define your own stopwords
myStopwords <- c(stopwords(), "covid19")

#Collect words from a single text document
tf=termFreq(ptd)
class(tf)
sort(tf)
tf=termFreq(ptd,control=list(tolower=F,removePunctuation=T,stopwords=T))
sort(tf)
tf=termFreq(ptd,control=list(tolower=F,removePunctuation=T,stopwords=myStopwords))
sort(tf)
tf=termFreq(ptd,control=list(removePunctuation=T,stopwords=myStopwords))
sort(tf)
tf=termFreq(ptd,control=list(removePunctuation=T,stopwords=myStopwords,removeNumbers=T))
sort(tf)
tf=termFreq(ptd,control=list(removePunctuation=T,stopwords=T,
                             stemming=T, removeNumbers=T))
tail(sort(tf))

#stemming
stm=stemDocument(ptd)   #A PlainTextDocument with stemmed words
class(stm)
inspect(stm)

tf2=termFreq(stm,control=list(removePunctuation=T,stopwords=T,
                              removeNumbers=T))
tail(sort(tf2))
###################################################################################
# Sentiment Analysis
###################################################################################
#bing lexicon
get_sentiments("bing")
#Alternative
sentiments
class(sentiments)
#afinn lexicon
get_sentiments("afinn")
#nrc lexicon
(nrc=get_sentiments("nrc"))
class(nrc)
###################################################################################
# Sentiment Analysis: Example
###################################################################################
co=VCorpus(PubMedSource(toDataFrame("TextData/tmData/Covid19March1.txt")))
co     #183 documents

ptd=co[[1]]    #First document in the corpus
ptd

tf=termFreq(ptd,control=list(removePunctuation=T,stopwords=myStopwords,
                             removeNumbers=T))

#The joins need a data frame of this format
df=data.frame(word=names(tf),freq=tf)

inner_join(df,get_sentiments("bing"))
inner_join(df,get_sentiments("afinn"))
#Select all the fear words
fearnrc = nrc[nrc$sentiment == 'fear',]
ptdFear = inner_join(df,fearnrc)

(ptdFear=inner_join(df,nrc[nrc$sentiment=='fear',]))
#Left join has all the words, even if they do not have a match in the lexicon
left_join(df,nrc)
###################################################################################
# Problem
###################################################################################
#1
tdm <- TermDocumentMatrix(co, control = list(removePunctuation = TRUE,
                                             stopwords = T,
                                             removeNumbers = TRUE))
tdm    #2972 words

#Note if we have many documents, we may remove sparse terms before creating the
#(sparse) matrix as follows
#if (length(co)>1000) tdm <-removeSparseTerms(tdm, sparse = 0.90)

#Create the (sparse) term document matrix
tdmM=as.matrix(tdm)
dim(tdmM)
tdmM[1:5,1:5]                        #Show some terms/documents

#2
#Analysis with respect to certain words
(idx=which(tdmM["aged",]!=0))        #column index for document that has the word "aged"
#Check the document with the word "aged" in it
inspect(co[[idx]])
meta(co[[idx]])
#How many times did it appear?
tdmM["aged",idx,drop=F]                     #once in document with PMID 32064853

#3
#column indices for documents that have the word "local"
(idx=which(tdmM["local",]!=0))   
#How many times in each document?
tdmM["local",idx]

#4
#Sum the rows to get frequencies of terms over the whole corpus
sums=apply(tdmM,1,sum)
head(sort(sums,decreasing=T))        #largest frequencies

#5
#How many different sentiments do we have?
unique(nrc$sentiment)

#6
#Data frame with terms and their frequencies over all documents in the corpus
df=data.frame(word=names(sums),freq=sums)

#Find all words in the corpus that evoke a certain sentiment
coFear=inner_join(df,nrc[nrc$sentiment=='fear',])
coJoy=inner_join(df,nrc[nrc$sentiment=='joy',])
coPos=inner_join(df,nrc[nrc$sentiment=='positive',])
coNeg=inner_join(df,nrc[nrc$sentiment=='negative',])

coFear[coFear$freq>=70,]
(idxr=which(rownames(tdmM) %in% coFear$word[coFear$freq>=70]))
(idx=which(tdmM[idxr[1],]>=1&tdmM[idxr[2],]>=1&tdmM[idxr[3],]>=1))
tdmM[idxr,idx]
length(idx)        #6 documents
inspect(co[[idx[1]]])
lapply(co[idx],meta,"title")

# my way
# getting fear words
fearwords  =nrc[nrc$sentiment=="fear",]
all_fear = inner_join(df,fearwords)
fear70 = all_fear[all_fear$freq>70,]$word
fear70
# getting row indices
colnames(tdmM)
rownames(tdmM)
idxr = which(rownames(tdmM) %in% fear70)
# getting column indicis
# filering the coulmns with some value at these rows
condition_col1 = tdmM[idxr[1],]>0
condition_col2 = tdmM[idxr[2],]>0
condition_col3 = tdmM[idxr[3],]>0
idx=which(condition_col1 & condition_col2 & condition_col3)
tdmM[idxr,idx]

###################################################################################
# Word Cloud
###################################################################################
?wordcloud

#One document
set.seed(123)
#All words in the document
wordcloud(names(tf),tf,min.freq=1,scale=c(2.5,.2))
#Better with colors
unique(tf)    #Three different frequencies, so we need three colors
wordcloud(names(tf),tf,min.freq=1,scale=c(2.5,.2),colors=c(1,3,2))
#Put more frequent words in the center
wordcloud(names(tf),tf,min.freq=1,scale=c(2.5,.2),colors=c(1,3,2),
          random.order=F)
#All fear words in the document
wordcloud(ptdFear$word,ptdFear$freq,min.freq=1,colors=1:2,random.order=F)

?wordcloud2
#This needs a data frame with columns word and freq
df=data.frame(word=names(tf),freq=tf)
wordcloud2(df)
wordcloud2(df,size=.5)                #Smaller letters
wordcloud2(df,size=.5, ellipticity=1) #Round
wordcloud2(df,size=.5, shape="star")  #Star shape
#Can save this one to a web page
###################################################################################
# Problem
###################################################################################
#1
#Whole corpus
set.seed(123)
#10 colors maximum
mypal=c("black",rainbow(8),"red")
#All words
wordcloud(names(sums),sums,min.freq=15,colors=mypal,random.order=F)
#All fear words
wordcloud(coFear$word,coFear$freq,min.freq=1,colors=mypal,random.order=F)
#All joy words
wordcloud(coJoy$word,coJoy$freq,min.freq=1,colors=mypal,random.order=F)
#All positive words
wordcloud(coPos$word,coPos$freq,min.freq=1,colors=mypal,random.order=F)
#All negative words
wordcloud(coNeg$word,coNeg$freq,min.freq=1,colors=mypal,random.order=F)

#2
?wordcloud2
df=data.frame(word=names(sums),freq=sums)
wordcloud2(df,size=.4)
wordcloud2(df,size=.6,backgroundColor = "wheat",shape="triangle",ellipticity=1)

#3
wordcloud2(coFear)
wordcloud2(coFear,col="random-light",size=.5)
wordcloud2(coJoy)
wordcloud2(coPos)
wordcloud2(coNeg)

