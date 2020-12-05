##################################################################################
# Sentiment Analysis utility functions 
###################################################################################

library(phm)
library(tm)

###################################################################################
get_overall_sentiments = function(co){
  
  tdM=as.matrix(TermDocumentMatrix(co, control = list(removePunctuation = TRUE,
                                                      stopwords = T,
                                                      removeNumbers = TRUE)))
  tdMdf=as.data.frame(tdM)
  tdMdf$word=rownames(tdMdf)
  #Term document data frame for words in afinn lexicon
  tdMdf=inner_join(tdMdf,get_sentiments("afinn"))
  rownames(tdMdf)=tdMdf$word
  tdMdf$word=NULL



  
  #Multiply frequency by sentiment
  if(ncol(tdMdf)>2)
  {
  # for multiple documents we handle data structure this way
  sentiment_value = tdMdf[,ncol(tdMdf)]
  all_words_freq = tdMdf[,-ncol(tdMdf)] 
  tdMdf2=all_words_freq * sentiment_value 
  }
  else{
    # for 1 doc
    # it has 2 columns 
    # 1 docs and 1 sentiment value we need to handle data structure this way
    sentiment_value = tdMdf[,ncol(tdMdf),drop=F]
    all_words_freq = tdMdf[,-ncol(tdMdf),drop = F]
    tdMdf2=all_words_freq * sentiment_value 
    
  }
  #Add all the sentiments by document
  sumSent=apply(tdMdf2,2,sum)
  
  return(sumSent)
  
  }