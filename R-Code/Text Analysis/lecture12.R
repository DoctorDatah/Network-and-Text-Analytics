###################################################################################
#
# Lecture12.R
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

###################################################################################
# Save the environment 
###################################################################################
parSave=par(no.readonly = TRUE)
#par(parSave)
###################################################################################
# Processing 
###################################################################################
# Phrase Mining  
###################################################################################
library(help="phm")

co=VCorpus(PubMedSource(toDataFrame("TextData/tmData/Covid19March1.txt")))
co                           #183 Publications

pd=phraseDoc(co)
pdm=as.matrix(pd)
dim(pdm)                      #Number of principal phrases and number of documents
pdm[1:5,1:5]
#Note that the document numbers are equal to their column indices.

#Most frequent phrases in the corpus
freqPhrases(pd,6)

#Obtain the total frequencies by phrase
sums=apply(pdm,1,sum)
#Alternate method to get the most frequent phrases in the corpus
head(sort(sums,decreasing=T)) 

#Check the frequent phrases in some of the documents

pdm[pdm[,1]!=0,1,drop=F]      #Frequent phrases in document 1
pdm[pdm[,2]!=0,2,drop=F]      #Frequent phrases in document 2
pdm[pdm[,8]!=0,8,drop=F]      #Frequent phrases in document 8

# ME # 
# lets choose documnt 2
doc2 = pdm[,2]
# now filter the rows with non zeros freq
pdm[doc2!=0,2,drop=F]


#Word cloud 1
set.seed(123)
myCol=c(rep("black",5),rainbow(12),rep("red",11))
wordcloud(names(sums),sums,min.freq=5,colors=myCol,random.order=F,scale=c(3,.3))

#Word cloud2
sums2=sums[sums>=5]
wordcloud2(data.frame(word=names(sums2),freq=sums2),size=.3)

# ME #  #wordcloud 2 uses df
df =  data.frame(names(sums2),freq=sums2)
wordcloud2(df,size = .3)


#Find documents that contain "february 11"
getDocs(pd,"february 11")
#Take a look at those documents
inspect(co[[83]])
inspect(co[[163]])

#Obtain documents for multiple principal phrases
pp=c("february 11","february 2020")
getDocs(pd,pp)


#Remove certain phrases
pdm2=as.matrix(removePhrases(pd,c("article is protected by copyright",
                                  "rights reserved")))
sums3=apply(pdm2,1,sum)

# ME # 
PHRASE_TO_REMOVE = c("article is protected by copyright",
                     "rights reserved")
pd2 = removePhrases(pd, PHRASE_TO_REMOVE)
pdm2 = as.matrix(pd)
pdm2[1:5,1:5]

# Getting freq of this new pdm now
class(pdm2)
sums3 = apply(pdm2,1,sum)
# /Me #

#Word cloud 1 without those phrases
set.seed(123)
wordcloud(names(sums3),sums3,min.freq=5,colors=myCol,random.order=F,scale=c(3,.3))
sums4=sums3[sums3>=5]
#Word cloud 2 without those phrases
wordcloud2(data.frame(word=names(sums4),freq=sums4),size=.5)
#Move your cursor over the cloud
###################################################################################
#Create a phraseDoc using some of the arguments
# sp  means stop pharases
pd2=phraseDoc(co,sp=c("rights reserved"))

#  n = Number of principal phrases to display.
freqPhrases(pd,12)
freqPhrases(pd2,11)
getDocs(pd,"rights reserved")
getDocs(pd2,"rights reserved")
pp=c("february 11","february 2020")
getDocs(pd,pp)
getDocs(pd2,pp)
getDocs(pd2,c(pp,"rights reserved"))
getDocs(pd,c(pp,"rights reserved"))

