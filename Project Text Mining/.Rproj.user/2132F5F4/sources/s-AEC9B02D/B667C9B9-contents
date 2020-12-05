##################################################################################
# Phrase Mining utility functions 
###################################################################################

library(phm)
library(tm)

###################################################################################
get_doc_oldID_and_num_hfphrases = function(co){
  
  oldID = as.integer((lapply(bd[1:length(bd)], meta,"oldID")))
  hfPhrases = as.integer((lapply(bd[1:length(bd)], meta,"hfPhrases")))
  topDocIndicsAndFreq = cbind(oldID,hfPhrases)
  return(topDocIndicsAndFreq)
  
  # Non-R Method
  #  oldID = 0
  #  hfPhrases = 0
  #  for (i in 1:length(co)) {
  #    oldID[i] = meta(co[[i]],"oldID")
  #    hfPhrases[i] = meta(co[[i]],"hfPhrases")
  #  }
}
###################################################################################

# Note: FOR Phrase Document and Matrix
# The names of the columns equal the column indices, 
# rather than their IDs as it is for the term-document matrix on words.
bestDocs<-function(co,nDocs=3,nPhrases=10, stopPhrases) {
  # Author: Professor Ellie 
  # nDocs: Number of most informative Documents in co
  # n: Number of phrases with the highest frequency over the entire corpus
  # Return nDocs number of documents as co2
  
  # Getting Principal phrases with mininum frquency of 3
  pd=phraseDoc(co, min.freq=3,silent=T, sp = stopPhrases)
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


###################################################################################