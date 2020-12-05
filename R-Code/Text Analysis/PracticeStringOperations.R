paste(c("a","b"),1:3)
paste(c("a","b"),1:3, sep = '.')
paste(c("a","b"),1:3, sep = '')
paste0(c('a','b'),1:3)


fname="Jane"; lname="Doe"
paste(fname,lname)
a = paste(fname,lname,sep = '\n')
a
cat(a)
writeLines(a)

a = c(fname,lname)
cat(a)
writeLines(a)

nchar(a)

str  = sprintf("%s has %d million dollars","Malik", 9)
