\name{demoUk}
\alias{demoUk}
\docType{data}
\title{UK life tables}
\description{
AM and AF one year mortality rate. Series of 1992
}
\usage{data(demoUk)}
\format{
  A data frame with 74 observations on the following 3 variables.
  \describe{
    \item{\code{Age}}{age}
    \item{\code{AM92}}{one year mortality rate for males}
    \item{\code{AF92}}{one year mortality rate for females}
  }
}
\details{
This data set shows the one year survival rates for males and females of the 1992 series. It has been taken from the Institute of Actuaries. The 
series cannot be directly used to create a life table since neither rates are not provided for ages below 16 nor for ages over 90. Various approach 
can be used to complete the series.
}
\source{
Institute of Actuaries
}
\references{
\url{http://www.actuaries.org.uk/research-and-resources/documents/am92-permanent-assurances-males}
}
\examples{
\dontrun{
#shows the table
data(demoUk)
#create an actuarial table using a Brass - Logit approach
data(soa08Act)
x=seq(0, 110,1)
qx=numeric(length(x))
for(i in 1:111) qx[i]=qxt(soa08Act, x=i, t=1)
temp=data.frame(Age=x, qx=qx)
db=merge(temp, demoUk)
db$lnAm92=with(db, log(AM92))
db$lnAf92=with(db, log(AF92))
db$logqx=with(db, log(qx))
#do the brass model
brassModelAM<-lm(lnAm92~logqx, data=db)
brassModelAF<-lm(lnAf92~logqx, data=db)
temp$logqx=log(temp$qx)
#fit the probabilities
temp$logAm92=predict(brassModelAM, newdata=temp)
temp$logAf92=predict(brassModelAF, newdata=temp)
temp$AM92=with(temp, exp(logAm92))
temp$AF92=with(temp, exp(logAf92))
missingAges=setdiff(temp$Age, demoUk$Age)
#prepare the data
dataOne=demoUk[,c("Age", "AM92", "AF92")]
dataTwo=subset(temp[,c("Age", "AM92", "AF92")], Age %in% missingAges)
temp=rbind(dataOne, dataTwo)
dataFull=temp[order(temp$Age),]
#setting last attainable year death probability equal to one
dataFull$AM92[length(temp$Age)]=1
dataFull$AF92[length(temp$Age)]=1
#produce the tables
AM92Lt<-probs2lifetable(probs=dataFull$AM92, 
	radix=100000,type="qx", name="AM92")
AF92Lt<-probs2lifetable(probs=dataFull$AF92, 
	radix=100000,type="qx", name="AF92")
}
}
\keyword{datasets}
