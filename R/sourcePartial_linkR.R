sourcePartial_linkR <- function(fn,startTag='#from here',endTag='#to here') {
  lines <- scan(fn, what=character(), sep="\n", quiet=TRUE)
  st<-grep(startTag,lines)[1]
  en<-grep(endTag,lines)[1]
  tc <- textConnection(lines[(st+1):(en-1)])
  r <- source(tc)
  close(tc)
  r$value
}