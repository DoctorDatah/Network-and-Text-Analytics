library(dplyr)
library(tidytext)
library(readtext)
library(tm)
install.packages("tidytext")


df=toDataFrame("TextData/tmData/Covid19March1.txt")
names(df)
#We need to create our own reader
readPub 
#And our own source
PubMedSource
#And our own getElem function
getElem.PubMedSource

co=VCorpus(PubMedSource(df))

tdM = as.matrix(TermDocumentMatrix(co, control=list(removePunctuation=T,
                                              stopwords=T,
                                              removeNumbers=T)))
tdM[1:5,1:5]
