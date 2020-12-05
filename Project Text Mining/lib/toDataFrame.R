##################################################################################es#
#
# toDataFrame.R:     Create a data frame from a file in MEDLINE format
#
###################################################################################
# Internal Functions
###################################################################################
# toDataFrame: Create a dataframe from the input file
# Input:  fn: path to a file with MEDLINE data
# Output: Data frame with the information from the file
###################################################################################
toDataFrame<-function(fn) {
  x<-readLines(fn, encoding="UTF-8")
  PMID=NULL;Date=NULL;Title=NULL;BTI=NULL;Abstract=NULL;cur=0;i=1;Author=NULL
  while (i<=length(x)) {
    if (substr(x[i],1,4)=="PMID") {
      cur=cur+1
      PMID[cur]=as.numeric(substring(x[i],7))
      i=i+1
    }
    if (substr(x[i],1,2)=="TI") {
      Title[cur]=substring(x[i],7)
      i=i+1
      while (substr(x[i],1,2)=="  ") {
        Title[cur]=paste(Title[cur],substring(x[i],7))
        i=i+1
      }
    }
    if (substr(x[i],1,3)=="BTI") {
      BTI[cur]=substring(x[i],7)
      i=i+1
      while (substr(x[i],1,2)=="  ") {
        BTI[cur]=paste(BTI[cur],substring(x[i],7))
        i=i+1
      }
    }
    if (substr(x[i],1,2)=="AB") {
      Abstract[cur]=substring(x[i],7)
      i=i+1
      while (substr(x[i],1,2)=="  ") {
        Abstract[cur]=paste(Abstract[cur],substring(x[i],7))
        i=i+1
      }
    }
    if (substr(x[i],1,2)=="AU") {
      Author[cur]=substring(x[i],7)
      i=i+1
    }
    if (substr(x[i],1,4)=="EDAT") {
      Date[cur]=as.character(as.Date(substring(x[i],7,16),"%Y/%m/%d"))
      i=i+1
    } else i=i+1
  }
  n<-max(length(PMID),length(Date),length(Title),length(Abstract))
  if (length(PMID)!=n) PMID[n]=NA
  if (length(Date)!=n) Date[n]=NA
  if (length(Title)!=n) Title[n]=NA
  if (length(BTI)!=n) BTI[n]=NA
  if (length(Author)!=n) Author[n]=NA
  #If the title is missing, use the BTI instead
  Title[is.na(Title)]=BTI[is.na(Title)]
  if (length(Abstract)!=n) Abstract[n]=NA
  #If the abstract is missing, use the title as abstract
  Abstract[is.na(Abstract)]=Title[is.na(Abstract)]
  df<-data.frame(PMID,Date,Title,Abstract,Author,stringsAsFactors=F)
}
