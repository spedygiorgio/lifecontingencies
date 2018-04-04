#test non ascii files
setwd("data-raw/")
showNonASCIIfile("AF92.csv")
showNonASCIIfile("demoIta_new.csv")
showNonASCIIfile("demoUk.csv")
showNonASCIIfile("demoUsa.csv")
showNonASCIIfile("soaLt.csv")


#in terminal 
# pcregrep --color='auto' -n '[^\x00-\x7F]' soaLt.csv   


#reading 

idat <- read.table("AF92bis.csv", header=TRUE, sep=",", dec=".", numerals="warn.loss",
                   strip.white=TRUE, blank.lines.skip=TRUE, allowEscapes=FALSE)

num2enc2num <- function(x)
  as.numeric(enc2utf8(as.character(x)))

idat$lx <- num2enc2num(idat$lx)
idat$x <- num2enc2num(idat$x)
iconv(idat, from="latin1", to="UTF-8")
