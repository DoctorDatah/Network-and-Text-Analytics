library(readtext)
library(tm)

########################################################################################
## Getting path
(FILE_PATH = system.file("texts", "acq", package = "tm"))
reuters = VCorpus(DirSource(FILE_PATH, mode = "binary"),
         readerControl = list(reader = readReut21578XMLasPlain))

reuters

# a
# 50 documents

#b
meta(reuters,type="corpus")         
meta(reuters,type="corpus","Description") = "All Above Companies" 
meta(reuters,type="corpus")  

#c
tdm = TermDocumentMatrix(reuters, control=list(removePunctuation=T,
                                          stopwords=T,
                                          removeNumbers=T,
                                          bounds=list(global=c(3,Inf)),
                                          wordLengths=c(4,Inf)))

tdm
# 226 terms 
# Sparsity 88%

#d
####### 
tdM = as.matrix(tdm)
# summ all the columns
sums = apply(tdM,2,sum)
# getting column'index for min sum
(id1=which.min(sums))  
# getting that column
col = tdM[,id1]
# filtering only non zero rows
col[col!=0,drop=F]

#e
#Sum the rows to get frequencies of terms over the whole corpus
sums_rows=apply(tdM,1,sum)
top10 = head(sort(sums_rows,decreasing=T),10)        #largest frequencies
top10["company"]
# frequency of company is 63

#f
#column indices for documents that have the word "products"
(idx=which(tdM["products",]!=0))   
# displaying Content of these docs
for (i in 1:length(idx)) {
  print(content(reuters[[i]]))
}
# Taking first one as plain txt document 
frist_one = idx[1]
reuters[frist_one] # returns vcorpus
reuters[[frist_one]] # returns plain txt doc
ptd  = reuters[[frist_one]]
class(ptd)

# g
content(ptd)
inspect(ptd)
# Replacing "\n with space
content(ptd) = gsub("\n"," ",content(ptd))
content(ptd)
inspect(ptd)

