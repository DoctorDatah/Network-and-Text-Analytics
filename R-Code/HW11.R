###################################################################################
#
# HW11.R
#
###################################################################################
# External Functions
###################################################################################
library(tm)
library(tidytext)          #For sentiments
library(dplyr)             #For joins
library(wordcloud)
library(wordcloud2)
library(sur)               #Skewness ratios

source("toDataFrame.R")
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
#1
co=VCorpus(PubMedSource(toDataFrame("../tmData/Covid19March1.txt")))
#a
tdM=as.matrix(TermDocumentMatrix(co, control = list(removePunctuation = TRUE,
                                                    stopwords = T,
                                                    removeNumbers = TRUE)))
#tdM=as.matrix(TermDocumentMatrix(co))
tdM[1:5,1:5]

#b
tdMdf=as.data.frame(tdM)
tdMdf$word=rownames(tdMdf)
#Term document data frame for words in afinn lexicon
tdMdf=inner_join(tdMdf,get_sentiments("afinn"))
rownames(tdMdf)=tdMdf$word
tdMdf$word=NULL
dim(tdMdf)              #204x184
tdMdf[1:5,180:184]

#c
#Find indices for documents that have frequencies for these words
sumFreq=apply(tdMdf[,1:(ncol(tdMdf)-1)],2,sum)
docs=which(sumFreq!=0)
length(docs)            #139

#d
#Multiply frequency by sentiment
tdMdf2=tdMdf[,-ncol(tdMdf)]*tdMdf[,ncol(tdMdf)]
dim(tdMdf2)

#e
#For document 1, find frequencies and sentiments
tdMdf[tdMdf[,1]!=0,c(1,ncol(tdMdf))]
#Frequencies times sentiment for document 1
tdMdf2[tdMdf2[,1]!=0,1,drop=F]
#For document 3
tdMdf[tdMdf[,3]!=0,c(3,ncol(tdMdf))]
tdMdf2[tdMdf2[,3]!=0,3,drop=F]

#f
#Add all the sentiments by document
sumSent=apply(tdMdf2,2,sum)
#Scatterplot of the sentiments of the documents
plot(docs,sumSent[docs],xlab=NA,ylab="Sentiments",xaxt='n')
abline(h=0,col=2)

#g
hist(sumSent,col="violet",main="Sentiments by Document",xlab="Sentiment",
     breaks=15)
#Most of the documents seem pretty neutral, but there are some 
#(apparently very sad) outliers
skew.ratio(sumSent)
#Very negatively skewed

mean(sumSent[docs])
median(sumSent[docs])
#The documents in the corpus are typically neutral.

#h
(id1=which.max(sumSent))                    #Most positive document
sumSent[id1]                                #How positive?
tdMdf[tdMdf[,id1]!=0,c(id1,ncol(tdMdf))]
inspect(co[[id1]])

(id2=which.min(sumSent))                    #Most negative document
sumSent[id2]                                #How negative?
tdMdf[tdMdf[,id2]!=0,c(id2,ncol(tdMdf))]
inspect(co[[id2]])

#i
#Most upbeat document
tfPos=termFreq(co[[id1]],control=list(removePunctuation=T,stopwords=T,
                                      removeNumbers=T))
set.seed(23)
wordcloud(names(tfPos),tfPos,col=c("black","blue","wheat","green","red"),
          min.freq=1,scale=c(3,.2),random.order=F)
wordcloud2(data.frame(word=names(tfPos),freq=tfPos),size=.7,shape="square",
           ellipticity=1)

#Most depressing document
tfNeg=termFreq(co[[id2]],control=list(removePunctuation=T,stopwords=T,
                                      removeNumbers=T))
set.seed(23)
wordcloud(names(tfNeg),tfNeg,col=c("black","blue","green","purple","red"),min.freq=1,
          scale=c(3,.2),random.order=F)
wordcloud2(data.frame(word=names(tfNeg),freq=tfNeg),size=.7)


