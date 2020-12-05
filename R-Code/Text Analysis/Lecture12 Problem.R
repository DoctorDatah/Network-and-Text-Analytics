
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
# Problem  
###################################################################################
(co=VCorpus(PubMedSource(toDataFrame("TextData/tmData/CovidKidsAntibodies.txt"))))
#140 documents

pd = phraseDoc(co, min.freq = 3)
pdm=as.matrix(pd)
dim(pdm)               #368 principal phrases in 140 documents
pdm[1:8,1:8]

#Documents without principal phrases
colFreq=apply(pdm,2,sum)
class(colFreq)
sum(colFreq==0)        #7
which(colFreq==0)
inspect(co[[45]])
#Check all of them
lapply(co[which(colFreq==0)],content)
#######
idx = which(colFreq==0)
lapply(co[idx], content)

(highFreq=freqPhrases(pd,5))
remPhrase=c("patients with covid-19","covid-19 patients")
pd=removePhrases(pd,remPhrase)
pdm=as.matrix(pd)
(highFreq=freqPhrases(pd,5))

#Obtain all documents with phrases like the most frequent ones
# in other words, all documnets with these high freq phrasee only
highFreq
names(highFreq)
pdm2 = getDocs(pd,names(highFreq))
dim(pdm2)

#72 documents with phrases equal to those in highFreq
pdm2[,1:5]
#Document 5 contains kawasaki disease 2 times.
# doing prgramitaclly
fiveDocs = pdm2[,1:5]
class(fiveDocs)
fiveDocs["kawasaki disease",, drop=F] 
fiveDocs["kawasaki disease",, drop=F] !=0
which(fiveDocs["kawasaki disease",, drop=F] !=0)
fiveDocs[,3, drop=F] # doc 5

inspect(co[[5]])
#All its principal phrases in the doc 5
pdm[pdm[,5]!=0,5,drop=F]



#Total frequencies for high frequency phrases by document
(hfDocsf=sort(apply(pdm2,2,sum),decreasing=T))

# ME # 
#  i think it should be done like this
# Q: Create a vector called hfDocsf that contains the total frequencies over all 
# phrases in highFreq for each document in pdm2, sorted in decreasing order.
# No no, its good.  pdm2 has created with only highFreq phrases 
# hence, we just need to sum over columns


#Total number of high frequency phrases by document
# Q: Challenge: Create a vector called hfDocs that contains the total number of 
# high frequency phrases (as in highFreq) by document, sorted in decreasing order. 
# Note that we want the number of phrases, not the sum of their frequencies.

# Direct
(hfDocs=sort(apply(pdm2,2,function(x) sum(x>0)),decreasing=T))
# Step by step 
sum_ifSomeValue  = function(x){
  sum(x>0)
}
sum_ifSomeValue(c(2,2,4,0))

total_prhases = apply(pdm2,2,sum_ifSomeValue)
hfDocs = sort(total_prhases, decreasing = T)
########                                    #######

# Q:Find the index of the document with the highest number of frequencies for 
# the high-frequency phrases (from hfDocsf). Store this index in a variable called idx. 
# Together
(idx=as.numeric(names(hfDocsf)[1]))               #Index in original
(idx2=which(colnames(pdm2)==names(hfDocsf)[1]))   #Index in high freq. matrix
# step by step
# Document id with most frequent phrases
# method 1 # getting docment id
  hfDocsf
  hfDocsf[1] # 
  doc_id = names(hfDocsf[1]) # thats actully the documet id # idex cant be retrived here
  idx = as.numeric(doc_id) # 58 doc id
# method 2 getting actual index for that document
idx2 = which(colnames(pdm2)==names(hfDocsf)[1]) # 29 is the index for doc 58

#All its principal phrases
# one thing to notice if we use document id in the pdm2 it will not go as aspeted 
# it is because it would be considered as index and in original pdm has doc ids as index
#hence this can only be used in original pdm
# Method 1 on original pdm using doc id
pdm[pdm[,idx]!=0,idx,drop=F]
#phrases in document 58 that are in the list of most frequent ones
# Method 2 using index of 58 doc id => 29 for pdm2
pdm2[pdm2[,idx2]!=0,idx2,drop=F]
#Read the abstract
inspect(co[[idx]])
meta(co[[idx]],"id")           #PMID

#Frequencies of the principal phrases over the whole corpus
sums=apply(pdm,1,sum)
unique(sort(sums))
mycols=c(rep("black",8),rainbow(22),rep("red",12))
set.seed(23)
wordcloud(names(sums),sums,min.freq=7,scale=c(2.5,.5),col=mycols,random.order=F)
sums2=sums[sums>=7]
wordcloud2(data.frame(word=names(sums2),freq=sums2),size=.5)

#Check out some of the abstracts that contain specific phrases
getDocs(pd,"cytokine storm")
inspect(co[[122]])          
getDocs(pd,"cell therapy")
inspect(co[[36]])          

