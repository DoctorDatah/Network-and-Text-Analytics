###################################################################################
#
# Quiz 5
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
library(phm)               #Phrase Mining

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


# Note: FOR Phrase Document and Matrix
# The names of the columns equal the column indices, 
# rather than their IDs as it is for the term-document matrix on words.
bestDocs<-function(co,nDocs=3,nPhrases=10) {
  # nDocs: Number of most informative Documents in co
  # n: Number of phrases with the highest frequency over the entire corpus
  # Return nDocs number of documents in co2
  
  # Getting Principal phrases with mininum frquency of 3
  pd=phraseDoc(co, min.freq=3,silent=T)
  #  nPhrases number of Most Frequent phrases 
  mostf=freqPhrases(pd,nPhrases)
  # Getting Documents that contain the most frequent phrases
  pdm2=getDocs(pd,names(mostf))
  # Total number of high frequency phrases by document
  colcounts=sort(apply(pdm2,2,function(x) sum(x>0)),decreasing=T)
  
  ## index of the document with the highest number of frequencies for he high-frequency phrases
  # Colcounts conatiain the number of high frqency phrases in a dcocument
  # head gives the top n counts 
  # names to get the names of the document
  # Recall Note: FOR Phrase Document and Matrix
  # The names of the columns equal the column indices, 
  # rather than their IDs as it is for the term-document matrix on words.
  # I.e: idices are stores as document names
  # We need to convert them to integers
  d=as.integer(names(head(colcounts,nDocs)))
  
  # Putting those documents in a new Corpus
  co2=co[d]
  # Updating meta with following info: 
  # 1: the index of the document (it can be used in corpus to get that document)
  # Old indeics mean the original indices of the document
  # 2: Number of High Frequency Phrases in that doc
  for (i in 1:nDocs) {
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

# a
(co=VCorpus(PubMedSource(toDataFrame("TextData/tmData/takotsubo.txt"))))
# 4529 docs
# b
meta(co,type="corpus","alternateName")="Broken Heart Syndrome"
meta(co,type="corpus","search")="Takotsubo"
meta(co,type="corpus")

# c
pd = phraseDoc(co,min.freq=10)
pdm = as.matrix(pd)
pdm[1:5,1:5]
dim(pdm)
# 1022 x 4529

# d
(fp = freqPhrases(pd,6))

# e
phraseToFind = c("chest pain")
gd = getDocs(pd,phraseToFind)
dim(gd)
max(gd)
sort(gd)
length(sort(gd))
# id of max in original
condition = pdm["chest pain",] == max(gd)
id = which.max(condition)
id
# f
# all princapl phrases of this doc
pdm[pdm[,id] != 0,id, drop=F]
# g
inspect(co[[id]])
# e
meta(co[[id]],"id") # 31486879"
# h
names(fp)
#Remove certain phrases
pd2 = removePhrases(pd,names(fp))
class(pd2)
pdm2=as.matrix(pd2)
dim(pdm2)
# i 
(fp2 = freqPhrases(pd2,100))

set.seed(123)
length(unique(fp2))
myCol = rainbow(68)
wordcloud(names(fp2),fp2,colors=myCol,scale=c(2,.2))

#Word cloud2
df = data.frame(word=names(fp2),freq=fp2)
wordcloud2(df,size=.2, shape='cloud')
