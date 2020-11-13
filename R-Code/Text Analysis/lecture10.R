###################################################################################
#
# Lecture10.R
#
###################################################################################
# External Functions
###################################################################################
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
###################################################################################
# Save the environment 
###################################################################################
parSave=par(no.readonly = TRUE)
#par(parSave)
###################################################################################
# Processing 
###################################################################################
# Import Texts into R
###################################################################################
#Make sure that the directory tmData2 only holds the files text.txt, text2.txt, and
#text3.txt.
#The directory tmData may have many files, but among them we need to have:
#   word1.doc and word1.docx
#   hypHF11.txt
#   pdf1.pdf and pdf2.pdf
###################################################################################
getReaders()
getSources()

#library(help="tm")
#library(help="readtext")
#vignette("tm")
#vignette("readtext_vignette")
###################################################################################
# Import Texts into R: Examples
###################################################################################
# Importing Text Files
###################################################################################
rt = readtext("TextData/tmData2/text.txt")
rt
class(rt)             #It's a readtext object, a type of data frame
as.data.frame(rt)
dim(rt)               #1 row, 2 columns
rt$doc_id
rt$text

#Create a plainTextDocument
ptd=PlainTextDocument(rt$text,id=rt$doc_id,language="en")
content(ptd)
inspect(ptd)
meta(ptd)

#Multiple files
rt=readtext("TextData/tmData2/*.txt")
rt    #The data frame containing the pdf files
dim(rt)
rt$text[3]

#Create a plainTextDocument
ptd=PlainTextDocument(rt$text,id=rt$doc_id,language="en")
content(ptd)
length(content(ptd))            #Now content is a vector of length 3
inspect(ptd)
meta(ptd)                       #Id is a vector of all three files
#It combined all files into one plainTextDocument

#We can, of course, create three of them using a for loop, then combine into
#a corpus
ptd=list()
for (i in 1:nrow(rt)) {
  ptd[[i]]=PlainTextDocument(rt$text[i],id=rt$doc_id[i],language="en")
}
class(ptd)

#Create a (volatile) corpus
co=c(ptd[[1]],ptd[[2]],ptd[[3]])
#Check out the corpus
co
class(co)
attributes(co)
content(co)
#In addition to holding document-specific metadata with the document itself,
#it is also possible to store this information in the corpus. The corpus
#keeps metadata for its contents as a dataframe.
#We will, in general, keep document-specific metadata with the document.
meta(co)
#Update it
meta(co,"no")=1:3
meta(co,"byDoc")=c("doc1","doc2","doc3")
meta(co)

#Corpus metadata
meta(co,type="corpus")
#Update corpus metadata
meta(co,type="corpus","title")="test"
meta(co,type="corpus","desc")="Several random texts"
meta(co,type="corpus")
inspect(co)

class(co[[1]])
content(co[[1]])
meta(co[[1]])
meta(co[[1]],"id")
meta(co[[1]],"author")="Jane Doe"     #Update the metadata
meta(co[[1]],"title")="Just a Text"   #Create a new metadata item
meta(co[[1]])
inspect(co[[1]])
inspect(co[[2]])
length(content(co[[1]]))
length(content(co[[2]]))

#Direct route using readtext
files="TextData/tmData2/*.txt"
co=VCorpus(DataframeSource(readtext(files)))

meta(co)                        #This now has something in it
dim(meta(co))                   #But nothing good!
#This method has corrupted the meta, but we can fix this by converting the 
#readtext to a data frame first
co=VCorpus(DataframeSource(as.data.frame(readtext(files))))

#Or direct:
co=VCorpus(DataframeSource(as.data.frame(readtext("TextData/tmData2/*.txt"))))
#Investigate corpus
meta(co)
content(co[[2]])
length(content(co[[2]]))
inspect(co[[2]])

#Direct route using DirSource
dir.exists("TextData/tmData2")
co=VCorpus(DirSource("../tmData2"))
#Investigate corpus
meta(co)                        #Empty
meta(co,type="corpus")          #Empty
length(content(co[[2]]))
#This  is now a vector of length 8!
inspect(co[[2]])

#Write the contents of a corpus to external files
writeCorpus(co, "TextData/tmData3")
###################################################################################
# Importing Word Files
###################################################################################
rt=readtext("TextData/tmData/word1.docx")
rt
#Create a plainTextDocument
ptd=PlainTextDocument(rt$text,id=rt$doc_id,language="en")
inspect(ptd)

#Create a corpus
co=VCorpus(DataframeSource(as.data.frame(readtext("TextData/tmData/word1.docx"))))
inspect(co)
inspect(co[[1]])

#Multiple word files
co=VCorpus(DataframeSource(as.data.frame(readtext("TextData/tmData/*.docx"))))
inspect(co)
inspect(co[[1]])
meta(co[[1]])

#Note; we can combine multiple types when using readtext!
co=VCorpus(DataframeSource(as.data.frame(readtext(c("TextData/tmData/*.docx","TextData/tmData2/*.*",
                                      "TextData/tmData/*.pdf")))))
inspect(co)
inspect(co[[1]])
meta(co[[1]])
###################################################################################
# Importing PDF Files
###################################################################################
#We may do this just like we did the word files. Alternatively, we can use the
#following method which will create additional metadata.

files=c("TextData/tmData/pdf1.pdf","TextData/tmData/pdf2.pdf")
co=VCorpus(URISource(files),readerControl=list(reader=readPDF))
meta(co)                 #Empty
meta(co,type="corpus")   #Empty
meta(co[[1]])
meta(co[[2]])
inspect(co)
inspect(co[[2]])
content(co[[2]])
###################################################################################
# Importing Vectors with Texts
###################################################################################
df=toDataFrame("TextData/tmData/hypHF11.txt")
names(df)
df$Abstract[1]
co=VCorpus(VectorSource(df$Abstract))
inspect(co)
co[[1]]$content
class(co[[1]])
meta(co[[1]])
meta(co[[1]],"author")="Jane Doe"
###################################################################################
#Importing Reuters files
###################################################################################
#These files are part of the tm package; we just need to find where they are
(reut21578 <- system.file("texts", "crude", package = "tm"))
reuters <- VCorpus(DirSource(reut21578, mode = "binary"),
                   readerControl = list(reader = readReut21578XMLasPlain))
names(reuters)
inspect(reuters)
inspect(reuters[[1]])
meta(reuters,type="corpus")
meta(reuters[[1]])
content(reuters[[1]])
###################################################################################
#Importing PubMed Files
###################################################################################
df=toDataFrame("TextData/tmData/hypHF11.txt")
names(df)
#We need to create our own reader
readPub 
#And our own source
PubMedSource
#And our own getElem function
getElem.PubMedSource

#Create the corpus
co=VCorpus(PubMedSource(df))
inspect(co)
inspect(co[[1]])
meta(co[[1]])
###################################################################################
#String Variable Operations
###################################################################################
paste(c("a","b"),1:3)    #Recycles the first vector since it is too short
paste(c("a","b"),1:3, sep=".")
paste(c("a","b"),1:3, sep="")
#Alternative
paste0(c("a","b"),1:3)

fname="Jane"; lname="Doe"
paste(fname,lname)
(a=paste(fname,lname,sep="\n"))
cat(a)
writeLines(a)                   #Same result

(a=c(fname,lname))              #Character vector with each word a different element
cat(a)                          #Combines the elements with a space in between
writeLines(a)                   #Puts each element on a different line

nchar(a)

(str=sprintf("%s has %d dollars", "Sam", 100)) #Same as in C
?sprintf

substr(str,9,18)
substring(str,9,18)          #Same
substr(str,9)                #Does not work
substring(str,9)             #From 9 to end

sub("100", "500",str)        #Substitute
sub("[[:blank:]]",",",str)
gsub("[[:blank:]]",",",str)

(nos=c(100,45,50,25,100,300,400,500,200,100,300))
class(nos)
(nos.sub=sub(100, 500, nos)) #Also works on a vector
class(nos.sub)               #But changed it to a character vector
as.numeric(nos.sub)          #numeric vector again

substring(nos,1,2)           #This one also works on a vector, but turns it into a char. vector
(nos2=c("100 plus 100","Here is another 100 and one more 100"))
sub("100", "500",nos2)       #But it only changes the first occurrence in each string
gsub("100", "500",nos2)      #This one changes all occurrences

#All pattern matching and Replacement functions
?sub
#Patterns
?regex

getwd()                    #Working directory
(wd=strsplit(getwd(),"/")) #Split a string into pieces
class(wd)                  #A list
class(wd[[1]])             #A character vector
(currDir=wd[[1]][length(wd[[1]])])  #Last element
setwd("..")                #Change the working directory to its parent
getwd()
setwd(currDir)             #Change it back
getwd()                    #Back to normal?

strsplit(getwd(),split=NULL)         #Split into characters
(spl=strsplit(c("abc","def"),split=NULL))  #Split each element of the vector into a list item
###################################################################################
# Creating a Term-Document Matrix
###################################################################################
co=VCorpus(DirSource("../tmData2"))
co
inspect(co[[1]])
meta(co[[1]])
as.matrix(TermDocumentMatrix(co))
#Remove punctuation
as.matrix(TermDocumentMatrix(co, control=list(removePunctuation=T)))
#Remove stop words
as.matrix(TermDocumentMatrix(co, control=list(removePunctuation=T,
                                              stopwords=T)))
#Keep uppercase letters
as.matrix(TermDocumentMatrix(co, control=list(removePunctuation=T,
                                              stopwords=T,
                                              tolower=F)))
#Remove numbers
as.matrix(TermDocumentMatrix(co, control=list(removePunctuation=T,
                                              stopwords=T,
                                              removeNumbers=T)))
#Only keep terms that appear in at least 2 documents
as.matrix(TermDocumentMatrix(co, control=list(removePunctuation=T,
                                              stopwords=T,
                                              removeNumbers=T,
                                              bounds=list(global=c(2,Inf)))))
#Only keep terms for a document if it appears there at least 2 times
as.matrix(TermDocumentMatrix(co, control=list(removePunctuation=T,
                                              stopwords=T,
                                              removeNumbers=T,
                                              bounds=list(local=c(2,Inf)))))
#Both bounds
as.matrix(TermDocumentMatrix(co, control=list(removePunctuation=T,
                                              stopwords=T,
                                              removeNumbers=T,
                                              bounds=list(global=c(2,Inf),local=c(2,Inf)))))
#Stemming
as.matrix(TermDocumentMatrix(co, control=list(removePunctuation=T,
                                              stopwords=T,
                                              removeNumbers=T,
                                              bounds=list(global=c(2,Inf)),
                                              stemming=T)))
#Compare
as.matrix(TermDocumentMatrix(co, control=list(removePunctuation=T,
                                              stopwords=T,
                                              removeNumbers=T,
                                              bounds=list(global=c(2,Inf)))))

tdM=as.matrix(TermDocumentMatrix(co))
tdM[rownames(tdM)=="file" | rownames(tdM)=="files",]
#Note that stemming combined the terms file and files, causing it to have a
#frequency that is now within the bounds

#Only keep terms that are in the dictionary
as.matrix(TermDocumentMatrix(co, control=list(removePunctuation=T,
                                              stopwords=T,
                                              removeNumbers=T,
                                              bounds=list(global=c(2,Inf)),
                                              dictionary=c("mining","text"))))
#Only keep words that are within the word length bound
#(we've removed the removeNumber option)
as.matrix(TermDocumentMatrix(co, control=list(removePunctuation=T,
                                              stopwords=T,
                                              bounds=list(global=c(2,Inf)),
                                              wordLengths=c(1,Inf))))
as.matrix(TermDocumentMatrix(co, control=list(removePunctuation=T,
                                              stopwords=T,
                                              bounds=list(global=c(2,Inf)),
                                              wordLengths=c(1,5))))

