## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,  collapse = TRUE,
  comment = "#>")

## ----setup, include=FALSE-----------------------------------------------------
#configure
library(lifecontingencies)
data(demoIta)
lxTAB<-demoIta$SIM81
lxTAB<-lxTAB[!is.na(lxTAB) & lxTAB!=0]
xTAB<-seq(0,length(lxTAB)-1,1)
#create the table
lt=new("lifetable",x=xTAB,lx=lxTAB)

## ----cumdef, echo=TRUE, tidy=TRUE---------------------------------------------
CUM<-function(acttableAccPeriod,x,beta,i = actuarialtable@interest,j,t,k=1,payment="advance",acttablePaymPeriod,i2,delta=0){

    out <- numeric(1)
    if (missing(acttableAccPeriod)) 
      stop("Error! Need an actuarial actuarialtable")
    if (missing(acttablePaymPeriod)) 
      acttablePaymPeriod=acttableAccPeriod
    if(missing(i2))
      i2=i
    if (missing(x)) 
      stop("Error! Need age!")
    if (missing(beta)) 
      stop("Error! Retirement age!")
    if (x > getOmega(acttableAccPeriod)) {
      out = 0
      return(out)
    }
    if (missing(t)) 
      stop("Error! Need t")
   if (missing(j)) 
    stop("Error! Need average salary increase rate")
  if (any(x < 0, beta < 0, t < 0)) 
      stop("Error! Negative parameters")
    out=sapply(seq(x,beta-1,1),function(h)Exn(acttableAccPeriod,h,beta-h,i=i)*((1/t)+(h-x)/t*(j/(1+j)))*axn(acttablePaymPeriod,beta,i = (1+i2)/(1+delta)-1,k=1))
    return(out)
  }

## ----cummr, echo=TRUE, tidy=TRUE----------------------------------------------
CUMmr<-function(acttableAccPeriod,x,beta,i = actuarialtable@interest,j,t,k=1,payment="advance",acttablePaymPeriod,i2,delta=0){
  
  out <- numeric(1)
  if (missing(acttableAccPeriod)) 
    stop("Error! Need an actuarial actuarialtable")
  if (missing(acttablePaymPeriod)) 
    acttablePaymPeriod=acttableAccPeriod
  if(missing(i2))
    i2=i
  if (missing(x)) 
    stop("Error! Need age!")
  if (missing(beta)) 
    stop("Error! Retirement age!")
  if (x > getOmega(acttableAccPeriod)) {
    out = 0
    return(out)
  }
  if (missing(t)) 
    stop("Error! Need t")
  if (missing(j)) 
    stop("Error! Need average salary increase rate")
  if (any(x < 0, beta < 0, t < 0)) 
    stop("Error! Negative parameters")
  out=c(sapply(seq(x,beta,1),function(h)Exn(acttableAccPeriod,h,beta-h,i=i)*((h-x)/t*(1+j)^(h-x-1))*axn(acttablePaymPeriod,beta,i = (1+i2)/(1+delta)-1,k=1)),sapply(seq(beta+1,getOmega(acttablePaymPeriod)+1,1),function(h)((beta-x)/t*(1+j)^(beta-x-1))*(1+delta)^(h-beta)*axn(acttablePaymPeriod,h,i = (1+i2)/(1+delta)-1,k=1)))
  return(out)
}

## ----pumdef, echo=TRUE, tidy=TRUE---------------------------------------------
#Projected Unit Method
PUM<-function(acttableAccPeriod,x,beta,i = actuarialtable@interest,j,t,k=1,payment="advance",acttablePaymPeriod,i2,delta=0){
  
  out <- numeric(1)
  if (missing(acttableAccPeriod)) 
    stop("Error! Need an actuarial actuarialtable")
  if (missing(acttablePaymPeriod)) 
    acttablePaymPeriod=acttableAccPeriod
  if(missing(i2))
    i2=i
  if (missing(x)) 
    stop("Error! Need age!")
  if (missing(beta)) 
    stop("Error! Retirement age!")
  if (x > getOmega(acttableAccPeriod)) {
    out = 0
    return(out)
  }
  if (missing(t)) 
    stop("Error! Need t")
  if (missing(j)) 
    stop("Error! Need average salary increase rate")
  if (any(x < 0, beta < 0, t < 0)) 
    stop("Error! Negative parameters")
  out=sapply(seq(x,beta-1,1),function(h)Exn(acttableAccPeriod,h,beta-h,i=i)*1/t*axn(acttablePaymPeriod,beta,i = (1+i2)/(1+delta)-1,k=1)*(1+j)^(beta-h-1))
  return(out)
}

## ----pummr, echo=TRUE, tidy=TRUE----------------------------------------------
PUMmr<-function(acttableAccPeriod,x,beta,i = actuarialtable@interest,j,t,k=1,payment="advance",acttablePaymPeriod,i2,delta=0){
  
  out <- numeric(1)
  if (missing(acttableAccPeriod)) 
    stop("Error! Need an actuarial actuarialtable")
  if (missing(acttablePaymPeriod)) 
    acttablePaymPeriod=acttableAccPeriod
  if(missing(i2))
    i2=i
  if (missing(x)) 
    stop("Error! Need age!")
  if (missing(beta)) 
    stop("Error! Retirement age!")
  if (x > getOmega(acttableAccPeriod)) {
    out = 0
    return(out)
  }
  if (missing(t)) 
    stop("Error! Need t")
  if (missing(j)) 
    stop("Error! Need average salary increase rate")
  if (any(x < 0, beta < 0, t < 0)) 
    stop("Error! Negative parameters")
  out=c(sapply(seq(x,beta,1),function(h)Exn(acttableAccPeriod,h,beta-h,i=i)*((h-x)/t*(1+j)^(beta-x-1))*axn(acttablePaymPeriod,beta,i = (1+i2)/(1+delta)-1,k=1)),sapply(seq(beta+1,getOmega(acttablePaymPeriod)+1,1),function(h)((beta-x)/t*(1+j)^(beta-x-1))*(1+delta)^(h-beta)*axn(acttablePaymPeriod,h,i = (1+i2)/(1+delta)-1,k=1)))
  return(out)
}

## ----ieamdef, echo=TRUE, tidy=TRUE--------------------------------------------
#Individual Entry-Age Unit Method
#Type: 0 constant contribution rate, 1 # Constant Contribution amount (Default is 0)
IEAM<-function(acttableAccPeriod,x,beta,i = actuarialtable@interest,j,t,k=1,payment="advance",acttablePaymPeriod,i2,delta=0,type=0){
  
  out <- numeric(1)
  if (missing(acttableAccPeriod)) 
    stop("Error! Need an actuarial actuarialtable")
  if (missing(acttablePaymPeriod)) 
    acttablePaymPeriod=acttableAccPeriod
  if(missing(i2))
    i2=i
  if (missing(x)) 
    stop("Error! Need age!")
  if (missing(beta)) 
    stop("Error! Retirement age!")
  if (x > getOmega(acttableAccPeriod)) {
    out = 0
    return(out)
  }
  if (missing(t)) 
    stop("Error! Need t")
  if (missing(j)) 
    stop("Error! Need average salary increase rate")
  if (any(x < 0, beta < 0, t < 0)) 
    stop("Error! Negative parameters")
  if(type==0){
  out=(Exn(acttableAccPeriod,x,beta-x,i=i)*(beta-x)/t*axn(acttablePaymPeriod,beta,i = (1+i2)/(1+delta)-1,k=1)*(1+j)^(beta-x-1))/(axn(acttablePaymPeriod,x,beta-x,i = (1+i)/(1+j)-1,k=1,payment="advance"))
  }
  else{
    out=((Exn(acttableAccPeriod,x,beta-x,i=i)*(beta-x)/t*axn(acttablePaymPeriod,beta,i = (1+i2)/(1+delta)-1,k=1)*(1+j)^(beta-x-1))/(axn(acttablePaymPeriod,x,beta-x,i ,k=1,payment="advance")))/(1+j)^seq(0,beta-x-1,1)
  }
  return(out)
}

## ----ieammr, echo=TRUE, tidy=TRUE---------------------------------------------
IEAMmr<-function(acttableAccPeriod,x,beta,i = actuarialtable@interest,j,t,k=1,payment="advance",acttablePaymPeriod,i2,delta=0,type=0){
  
  out <- numeric(1)
  if (missing(acttableAccPeriod)) 
    stop("Error! Need an actuarial actuarialtable")
  if (missing(acttablePaymPeriod)) 
    acttablePaymPeriod=acttableAccPeriod
  if(missing(i2))
    i2=i
  if (missing(x)) 
    stop("Error! Need age!")
  if (missing(beta)) 
    stop("Error! Retirement age!")
  if (x > getOmega(acttableAccPeriod)) {
    out = 0
    return(out)
  }
  if (missing(t)) 
    stop("Error! Need t")
  if (missing(j)) 
    stop("Error! Need average salary increase rate")
  if (any(x < 0, beta < 0, t < 0)) 
    stop("Error! Negative parameters")
  al=IEAM(acttableAccPeriod,x,beta,i,j,t,k=1,payment,acttablePaymPeriod,i2,delta,type)
    if(type==0){
    out=c(sapply(seq(x,beta,1),function(h)Exn(acttableAccPeriod,h,beta-h,i=i)*((beta-x)/t*(1+j)^(beta-x-1))*axn(acttablePaymPeriod,beta,i = (1+i2)/(1+delta)-1,k=1)-al*(1+j)^(h-x)*axn(acttableAccPeriod,h,beta-h,i = (1+i)/(1+j)-1,k=1)),sapply(seq(beta+1,getOmega(acttablePaymPeriod)+1,1),function(h)((beta-x)/t*(1+j)^(beta-x-1))*(1+delta)^(h-beta)*axn(acttablePaymPeriod,h,i = (1+i2)/(1+delta)-1,k=1)))
  }
  else{
    out=c(sapply(seq(x,beta-1,1),function(h)Exn(acttableAccPeriod,h,beta-h,i=i)*((beta-x)/t*(1+j)^(beta-x-1))*axn(acttablePaymPeriod,beta,i = (1+i2)/(1+delta)-1,k=1)-al[h-x+1]*(1+j)^(h-x)*axn(acttableAccPeriod,h,beta-h,i ,k=1)),sapply(seq(beta,getOmega(acttablePaymPeriod)+1,1),function(h)((beta-x)/t*(1+j)^(beta-x-1))*(1+delta)^(h-beta)*axn(acttablePaymPeriod,h,i = (1+i2)/(1+delta)-1,k=1)))
  }
    return(out)
}

## ----assumptions--------------------------------------------------------------
#Current Unit Method
beta=65 # Beta Retirement age
x=25 #x Age of the insured.
i=0.08 # Interest Rate
t=60 #1/t is the % of the salary, recognized as retirement pension, for each year of service
j=0.06 #  average salary increases (for both growth in wages and promotional salary for seniority)
delta=0.03 #Increase of retirement pension

## ----calcandshow, tidy=TRUE---------------------------------------------------
CUM(lt,x,beta,i,j,t,k,delta=0.03)
PUM(lt,x,beta,i,j,t,k,delta=0.03)
IEAM(lt,x,beta,i,j,t,k,delta=0.03,type=0)
plot(seq(x,beta-1,1),CUM(lt,x,beta,i,j,t,k,delta=0.03),xlab="age",ylab="Contribution rate")
lines(seq(x,beta-1,1),PUM(lt,x,beta,i,j,t,k,delta=0.03),type="p",col="red")
lines(seq(x,beta-1,1),rep(IEAM(lt,x,beta,i,j,t,k,delta=0.03),beta-x),type="p",col="blue")
lines(seq(x,beta-1,1),IEAM(lt,x,beta,i,j,t,k,delta=0.03,type=1),type="p",col="green")
legend("topleft",c("CUM","PUM","IEAM (constant rate)","IEAM (constant premium)"),col=c("black","red","blue","green"),pch=c(1,1,1,1),cex=0.6)

## ----calc2andshow, tidy=TRUE--------------------------------------------------
CUMmr(lt,x,beta,i,j,t,k,delta=0.03)
PUMmr(lt,x,beta,i,j,t,k,delta=0.03)
IEAMmr(lt,x,beta,i,j,t,k,delta=0.03,type=0)
IEAMmr(lt,x,beta,i,j,t,k,delta=0.03,type=1)
plot(seq(x,getOmega(lt)+1,1),CUMmr(lt,x,beta,i,j,t,k,delta=0.03),xlab="age",ylab="",main="Mathematical reserve")
lines(seq(x,getOmega(lt)+1,1),PUMmr(lt,x,beta,i,j,t,k,delta=0.03),type="p",col="red")
lines(seq(x,getOmega(lt)+1,1),IEAMmr(lt,x,beta,i,j,t,k,delta=0.03,type=0),type="p",col="blue")
lines(seq(x,getOmega(lt)+1,1),IEAMmr(lt,x,beta,i,j,t,k,delta=0.03,type=1),type="p",col="green")
legend("topleft",c("CUM","PUM","IEAM (constant rate)","IEAM (constant premium)"),col=c("black","red","blue","green"),pch=c(1,1,1,1),cex=0.6)

