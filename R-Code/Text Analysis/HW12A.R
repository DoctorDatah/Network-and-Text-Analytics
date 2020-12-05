###################################################################################
#
# HW12 MEs.R
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
#1
#a
bestDocs

#b
(co=VCorpus(PubMedSource(toDataFrame("TextData/tmData/Covid19March1.txt"))))
# 183 docs
pd = phraseDoc(co)
pdm = as.matrix(pd)

# we can observer here that co has 1 - 183 docments 
# and pdm has doc ids from 1 - 183.
# Hence recall note: index is used as doc in pdm

pdm[1:5,1:5]

#c 
bd=bestDocs(co,nDocs = 10,nPhrases = 10)
bd
# function bestDocs returns the coprpus with the nDocs=10 documents
# for bd the document indices are 1 to 10. 
# we need original indices to fetch the documents in original corpus

### Retrieving orignial(oldid) indiscs for these 10 documents
# Method 1
unlist(lapply(bd, meta,"oldID"))
unlist(lapply(bd, meta,"hfPhrases"))

# Method 2
oldID = 0
hfPhrases = 0
for (i in 1:length(bd)) {
  oldID[i] = meta(bd[[i]],"oldID")
  hfPhrases[i] = meta(bd[[i]],"hfPhrases")
}
oldID
hfPhrases

(topDocIndicsAndFreq = cbind(oldID,hfPhrases))


#d
#	What is the index (in the original corpus) 
# for the document with the highest number of high-frequency phrases? 
#Store this index in the variable

# looking at index 
  # using vectors 
  (id = oldID[1]) # 123 index 
  hfPhrases[1] # 6 hf phrases
  
  # using Matrix
  topDocIndicsAndFreq[1,] # 123 and 6
    # storing in id 
    id = as.integer(topDocIndicsAndFreq[1,"oldID"])

# Checking this docuemnt in original corpus 
inspect(co[[id]])
  
# Checking this document in subset corpose returned by the function 

for (i in 1:length(bd)) {
  if( meta(bd[[i]],"oldID") == id)
  {
    inspect(bd[[i]])
  }
}


# Q: Show all principal phrases in this document and 
# compare it to the top 10 high frequency phrases in co.
  # All principal phrases 
  pdm[,id] != 0
  pdm[pdm[,id] != 0,id, drop=F]
  
  # top 10 high frequency phrases 
  freqPhrases(pd,10)
  
  
###############################################################
# e
# now using only 5 highFrequency prases
  bd=bestDocs(co,nDocs = 10,nPhrases = 5)
  bd
  
  oldID = 0
  hfPhrases = 0
  for (i in 1:length(bd)) {
    oldID[i] = meta(bd[[i]],"oldID")
    hfPhrases[i] = meta(bd[[i]],"hfPhrases")
  }
  oldID
  hfPhrases
  
  # same documnts wins 

# now using 20 highFrequency prases
  bd=bestDocs(co,nDocs = 10,nPhrases = 20)
  bd
  
  oldID = 0
  hfPhrases = 0
  for (i in 1:length(bd)) {
    oldID[i] = meta(bd[[i]],"oldID")
    hfPhrases[i] = meta(bd[[i]],"hfPhrases")
  }
  oldID
  hfPhrases
  
# document (82) and another documnt(126) has highfrequncy phrases of count 7 
  
  inspect(co[[82]])
  inspect(co[[126]])
  
  


