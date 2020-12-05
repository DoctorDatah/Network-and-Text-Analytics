###################################################################################
#
# HW10.R
#
###################################################################################
# External Functions
###################################################################################
library(readtext)
library(tm)
###################################################################################
# Internal Functions
###################################################################################
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
(co=VCorpus(DataframeSource(as.data.frame(readtext(c("TextData/tmData/*.docx","TextData/tmData/*.pdf"))))))
inspect(co)
#b
meta(co,type="corpus")
meta(co,type="corpus","desc")="Random Texts"
meta(co,type="corpus")
#c
lapply(co,content)
#d
for (i in 1:length(co)) {
  meta(co[[i]],"author")="Jane Doe"
}
lapply(co,meta)
#e
ptd=co[[4]]
class(ptd)
length(content(ptd))
inspect(ptd)
content(ptd)

# Me 
splt = strsplit(content(ptd),"\n")
class(splt)
class(splt[[1]])
# unlisting
charvector = unlist(splt)
class(charvector)
# /me

content(ptd)=unlist(strsplit(content(ptd),"\n"))
content(ptd)
length(content(ptd))

# sapply() function does the same job as lapply() function but returns a vector
sap=sapply(content(ptd),nchar)
names(sap)=1:5
sap
#or
unlist(lapply(content(ptd),nchar))
#f
content(ptd)[3]
gsub("I am","We are",content(ptd)[3])
######
content(ptd)[3]=gsub("I am","We are",content(ptd)[3])
content(ptd)[3]=gsub("I ","we ",content(ptd)[3])
content(ptd)
#g
tolower(content(ptd))
content(ptd)=tolower(content(ptd))

first = substring(content(ptd),1,1)
firstCAp = toupper(first)
remaining = substring(content(ptd),2)
compined = paste(firstCAp,remaining,sep="")

content(ptd)=paste0(toupper(substring(content(ptd),1,1)),substring(content(ptd),2))
content(ptd)
#h
as.matrix(TermDocumentMatrix(co, control=list(bounds=list(global=c(length(co),Inf)),
                                              wordLengths=c(2,Inf))))
(mat=as.matrix(TermDocumentMatrix(co, control=list(bounds=list(global=c(length(co),Inf)),
                                                   stopwords=T,
                                                   wordLengths=c(2,Inf)))))
#i
# Function over rows that why 1
tot = apply(mat,1,sum)
mat = cbind(mat,tot)
mat
colnames(mat)
ncol(mat)
colnames(mat)[ncol(mat)] = "Total"

mat=cbind(mat,apply(mat,1,sum))
colnames(mat)[ncol(mat)]="Total"
mat
