library(readtext)
library(tm)

source("C:/Users/Malik/Documents/GitHub/Network-and-Text-Analytics-/R-Code/Text Analysis/toDataFrame.R")
## source("toDataFrame.R")
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
  list(content = x$content[x$position, ])
########################################################################################

getReaders()
getSources()

####################
rto = readtext("TextData/tmData2/text.txt")
class(rto)

as.data.frame(rto)
dim(rto)
colnames(rto)
rto$doc_id
rto$text

# Creating plain text document
ptdo = PlainTextDocument(rto$text,rto$doc_id, language = "en")
ptdo
class(ptdo)
content(ptdo)
inspect(ptdo)
meta(ptdo)


# Loading multiple txt files
mtf = readtext("TextData/tmData2/*.txt")
mtf
class(mtf)
dim(mtf)
colnames(mtf)
mtf$doc_id # gives all of them together
mtf$text[1]
mtf$text[2]
mtf$doc_id[3]

# wrong 
# it compines them all together
# we need for loop
# creatind plain text docunent for all the documents
ptd2 = PlainTextDocument(mtf$text,mtf$doc_id,language = "en")
class(ptd2)
content(ptd2)
inspect(ptd2)

# using forloop
ptd3 = list()
n = nrow(mtf)
for( i in 1:n){
  ptd3[[i]] = PlainTextDocument(mtf$text[i],mtf$doc_id[i],language = "en")
  
}
class(ptd3)
ptd3
ptd3[1]
content(ptd3[[1]])
class(ptd3[[1]])

#### creatig corpus
co1 = c(ptd3)
class(co1)  # list
co2 = c(ptd3[[1]],ptd3[[2]],ptd3[[3]])
class(co2) # vCorpus
content(co2)
attributes(co2) # Attribuutes of the object
meta(co2)
# updating the meta
meta(co2, "no") = 1:3
meta(co2)
meta(co2, "byDoc")=c("doc1", "doc2", "doc3")
meta(co2)

attributes(co2[[1]])
inspect(co2[[1]])
content(co2[[1]])
class(co2[[1]])
meta(co2[[1]])
meta(co2[[1]],"id")
meta(co2[[1]],'auther') = "Na maloom"
meta(co2[[1]],"New meata") = "xyz"
meta(co2[[1]])
inspect(co2[[1]])
length(content(co2[[1]]))
meta(co2)
meta(co2, type = 'corpus')
meta(co2,type = "corpus", 'title') = 'test'
meta(co2, type='corpus', 'New meta attr') = 'tem value'

inspect(co2)


# now creating direct corpus from text files 
filespath = "TextData/tmData2/*.txt"
rt1 = readtext(filespath)
class(rt1) # hecnce using data frame source
co4 = VCorpus(DataframeSource(rt1))

getSources()
?DirSource
# now using DirSource for same purpose
DIR = "TextData/tmData2"
dir.exists(DIR)
co5 = VCorpus(DirSource(DIR))
length(content(co5[[2]]))

writeCorpus(co5, DIR)


#### Work 
PathWord = "TextData/tmData/word1.docx"
ptdq = PlainTextDocument(readtext(PathWord))
inspect(ptdq)

# Creating corpus

co6 = VCorpus(DataframeSource(as.data.frame(readtext(PathWord))))
inspect(co6) 
inspect(co6[[1]])

# Multiple word files
PathWord2 = "TextData/tmData/*.docx"
co7 = VCorpus(DataframeSource(as.data.frame(readtext(PathWord2))))
inspect(co7)
inspect(co7[[1]])

##### Combining multuplle types using readtxt

### Pdfs
file1 = "TextData/tmData/pdf1.pdf"
file2 = "TextData/tmData/pdf2.pdf"
files = c(file1, file2)
cop = VCorpus(URISource(files), readerControl = list(reader=readPDF))


### importing vectors with text
filep1 = "TextData/tmData/hypHF11.txt"
df  = toDataFrame(filep1)
names(df)
df$Abstract

c1 = VCorpus(VectorSource(df$Abstract))
content(c1[[1]])

# importing reuters files 
reutersPath = system.file("texts","crude",package = "tm")
c2 = VCorpus(DirSource(reutersPath,mode = "binary"), readerControl = list(reader = readReut21578XMLasPlain))

# reading pubmed file
df = toDataFrame(filep1)
names(df)
c3 = VCorpus( PubMedSource(df))
inspect(c3)


#  Creatin Text document matrix
Dir = "TextData/tmData2"
c1 = VCorpus(DirSource(Dir))
c1
inspect(c1[[1]])
tm = TermDocumentMatrix(c1, control = list(removePunctuation=T, stopwords = T, removeNumbers = T))
as.matrix(tm)

tm2 = TermDocumentMatrix(c1, control = list(removePunctuation=T, stopwords=T, removeNumbers = T,
                                            bounds=list(local=c(2,Inf))))
as.matrix(tm2)
as.matrix(TermDocumentMatrix(c1, control=list(removePunctuation=T,
                                              stopwords=T,
                                              bounds=list(global=c(2,Inf)),
                                              wordLengths = c(1,Inf)
                                              )))

tdM=as.matrix(TermDocumentMatrix(c1))
tdM[rownames(tdM)=="file" | rownames(tdM)=="files",]
