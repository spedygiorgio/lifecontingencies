## ----echo=F-------------------------------------------------------------------
knitr::opts_chunk$set(
  warning = F,
  message = F,
  collapse = TRUE,
  error = TRUE,
  fig.width = 6,
  fig.height = 3.5,
  fig.align = 'center',
  comment = "#>",
  fig.show = 'hold'
)

## ----echo=TRUE,  tidy=TRUE----------------------------------------------------
require("lifecontingencies")
PensFund <-function(x, y, r, acttableAccPeriod, decrement, i, j, delta, n, avg, acttablePaymPeriod, CostMet){
if (missing(n)) n <- getOmega(acttableAccPeriod)-x-1
if (x<y) stop("Entry age greater than actual age")
if (missing(decrement)) stop("Which is the contingency of benefit?")
if (class(acttableAccPeriod) != "mdt") stop("Error! Needed Mdt")
if (!(decrement %in% getDecrements(acttableAccPeriod))) stop("Error! Not recognized decrement type")
if (missing(acttableAccPeriod)) stop("Error! Need a lifetable or actuarialtable")
if (missing(x) | missing(y)) stop("Error! Need age and seniority!")
if (x > getOmega(acttableAccPeriod)) { out = 0
                                        return(out)}
if (missing(CostMet)) CostMet = 'BPM_CD'
if (missing(avg)) stop("Error! Need avg period of computing benefit")
if (missing(j)) stop("Error! Need average salary increase rate")
if (any(x < 0, y < 0, avg < 0, delta < 0))  stop("Error! Negative parameters")

##########survive all casuses
acttableAccPeriodTotal = pxt(acttableAccPeriod, x = 0:(getOmega(acttableAccPeriod)), t=1)
acttableAccPeriodTotal = probs2lifetable(acttableAccPeriodTotal, type = "px")
if (missing(acttablePaymPeriod)) acttablePaymPeriod = acttableAccPeriodTotal

########## Set benefit
# fix age valuation
x0=x
B = ifelse(avg==0, 1, (1+j)^(r-x0) *  annuity(j, avg, m=0, k=1, type = "immediate") / avg )
q.r = qxt(object=acttableAccPeriod, x=r, t=1, decrement=decrement)
Br = q.r * axn(acttablePaymPeriod, r, i = (1+i)/(1+delta)-1, n=n, k=1)*B

########## Actuarial liability in age x, fot the age r (r>=x)
ALyNC.x.r = function(x, r){
            
        ## PVFB
        
        PVFB.x = Exn(acttableAccPeriodTotal, x, r-x, i=i) * Br
        
        ## Normal Costs and liabilities under Actuarial Cost Methods 
        
        if(CostMet == 'BPM_CD'){
          K <- (x-y) / (r-y) 
          k <- 1 / (r-y)}
        
        if(CostMet == 'BPM_CP'){
          K <-    (1+j)^(x-x0) * annuity(j, x-y+1, m=0, k=1, type = "immediate") / 
                ( (1+j)^(r-x0) * annuity(j, r-y+1, m=0, k=1, type = "immediate") )
          k <-    (1+j)^(x-x0) / ( annuity(j, r-y, m=0, k=1, type = "immediate") * (1+j)^(r-x0))}
        
        if(CostMet == 'CPM_CD'){
          K <- axn(acttableAccPeriodTotal, y, i = i, n=x-y, k=1) / 
               axn(acttableAccPeriodTotal, y, i = i, n=r-y, k=1) 
          k <- Exn(acttableAccPeriodTotal, y, x-y, i=i) / 
               axn(acttableAccPeriodTotal, y, i = i, n = r-y, k = 1) 
          }
        
        if(CostMet == 'CPM_CB'){
          K <- axn(acttableAccPeriodTotal, y, i = (1+i)/(1+j)-1, n=x-y, k=1) / 
               axn(acttableAccPeriodTotal, y, i = (1+i)/(1+j)-1, n=r-y, k=1) 
          k <- Exn(acttableAccPeriodTotal, y, x-y, i=i) * (1+j)^(x-y) /
               axn(acttableAccPeriodTotal, y, i = (1+i)/(1+j)-1, n=r-y, k=1) }
        
        ## Results
        
        outAL.x.r <- PVFB.x * K
        outNC.x.r <- PVFB.x * k
        outPVFB.x <- PVFB.x
        out.x.r <- c(outAL.x.r, outNC.x.r, outPVFB.x)
        
        return(out.x.r)
}

######## AL y NC for the rest of ages

ages_risk = x:r
outAL.x = sapply(ages_risk,  function(x) ALyNC.x.r(x, r)[1])
outNC.x = sapply(ages_risk,  function(x) ALyNC.x.r(x, r)[2])
outPVFB.x = sapply(ages_risk,  function(x) ALyNC.x.r(x, r)[3])
outNC.x[length(ages_risk)] = NA
out.x <- data.frame(x = ages_risk, AL = outAL.x, NC = outNC.x, PVFB = outPVFB.x)

return(out.x)
}

## ----echo=TRUE,  tidy=TRUE, warning=FALSE, message=FALSE----------------------
data(SoAISTdata)
data(soa08)
MultiDecrTable <- new("mdt", name="testMDT", table=SoAISTdata)

## ----echo=TRUE,  tidy=TRUE----------------------------------------------------
getDecrements(MultiDecrTable)

## -----------------------------------------------------------------------------
x1=30
y1=20
r1=53
decrement1 = "inability"
BPM_CD = PensFund(x1, y1, r1, acttableAccPeriod = MultiDecrTable, decrement = decrement1,  
                  i = .04, j = .06, delta = .03, avg = 5, 
                  acttablePaymPeriod = soa08, CostMet = 'BPM_CD')
BPM_CP = PensFund(x1, y1, r1, acttableAccPeriod = MultiDecrTable, decrement = decrement1,  
                  i = .04, j = .06, delta = .03, avg = 5, 
                  acttablePaymPeriod = soa08, CostMet = 'BPM_CP')
CPM_CD = PensFund(x1, y1, r1, acttableAccPeriod = MultiDecrTable, decrement = decrement1,  
                  i = .04, j = .06, delta = .03, avg = 5, 
                  acttablePaymPeriod = soa08, CostMet = 'CPM_CD')
CPM_CP = PensFund(x1, y1, r1, acttableAccPeriod = MultiDecrTable, decrement = decrement1,  
                  i = .04, j = .06, delta = .03, avg = 5, 
                  acttablePaymPeriod = soa08, CostMet = 'CPM_CB')

## ----echo=FALSE, fig_width----------------------------------------------------
par(mfrow=c(1,2))
plot(x1:r1, BPM_CD$AL, t="o", ylab="ALx", xlab="Age", cex = .7)
  points(x1:r1, BPM_CP$AL, t="o", col=2, cex = .7)
  points(x1:r1, CPM_CD$AL, t="o", col=3, cex = .7)
  points(x1:r1, CPM_CP$AL, t="o", col=4, cex = .7)
  title ("Actuarial liability. Example 1", cex.main=.9)
  legend("topleft", legend = c("BPM_CD", "BPM_CP", "CPM_CD", "CPM_CB"), 
         lty = rep(1,4), col = 1:4, cex=.6, bty = "n")
plot(x1:r1, BPM_CP$NC, t="o", ylab="NCx", xlab="Age", cex = .7)
  points(x1:r1, BPM_CD$NC, t="o", col=2, cex = .7)
  points(x1:r1, CPM_CD$NC, t="o", col=3, cex = .7)
  points(x1:r1, CPM_CP$NC, t="o", col=4, cex = .7)
  title ("Normal Cost. Example 1", cex.main=.9)
  legend("topleft", legend = c("BPM_CP", "BPM_CD", "CPM_CD", "CPM_CB"), 
         lty = rep(1,4), col = 1:4, cex=.6, bty = "n")

## ----echo=TRUE, tidy=TRUE-----------------------------------------------------
# AL = PVBF - PVFNC
# first get survivial for all causes
acttableAccPeriodTotal = pxt(MultiDecrTable, x= 0:(getOmega(MultiDecrTable)), t=1)
acttableAccPeriodTotal = probs2lifetable(acttableAccPeriodTotal, type = "px")
# are equal?
round(BPM_CD$AL[1],10) ==  round(BPM_CD$PVFB[1] - sum(sapply(x1:(r1-1), function(x) Exn(acttableAccPeriodTotal, x1, x-x1, i=.04) * BPM_CD$NC[x-x1+1])),10)

## ----echo=TRUE, tidy=TRUE-----------------------------------------------------
# Restrospective AL
# First get the liability at entry age y
BPM_CD_y = PensFund(y1, y1, r1, acttableAccPeriod = MultiDecrTable, decrement = decrement1,i = .04, j = .06, delta = .03, avg = 5, acttablePaymPeriod = soa08, CostMet = 'BPM_CD')
# are equal?
round(BPM_CD_y$AL[BPM_CD_y$x==45],10) == round(sum(sapply(y1:44, function(y) (1.04)^(45-y) / pxt(acttableAccPeriodTotal, y, 45-y) * BPM_CD_y$NC[BPM_CD_y$x==y])),10)

## ----echo=TRUE, tidy=TRUE-----------------------------------------------------
x2=50
y2=35
decrement2 = "retirement"
pay_grade = data.frame(x=x2:70,gr=c(rep(0,10),seq(.5, 1, .05)))
snty_req = 20
s_x = 5000
Cost_Method2 = "BPM_CD"

## ----echo=TRUE, tidy=TRUE-----------------------------------------------------
AL_xr = data.frame(r=0, AL=0, NC=0)
for(r in x2:70) {
  if ((r-y2) < snty_req) 
    AL_xr[r-x2+1,2:3]=c(0,0) 
  else {
    AL_xr[r-x2+1,2:3] = s_x * pay_grade[pay_grade$x==r,2] * 
                        PensFund(x2, y2, r, acttableAccPeriod = MultiDecrTable, 
                              decrement = decrement2, i = .04, j = .06, delta = .03, avg = 5, 
                              acttablePaymPeriod = soa08, CostMet = Cost_Method2)[1,2:3]
    }
  AL_xr[r-x2+1,1] = r
}

## ----echo=FALSE, tidy=TRUE----------------------------------------------------
par(mar=c(6, 4, 4, 6) + 0.1)
par(mfrow=c(1,1))
plot(AL_xr$r, AL_xr$AL, t="o", ylab="$", xlab="Retirement Age")
title ("Actuarial liability in age x=50, for retirement in age r", cex.main=.9)
par(new=TRUE)
plot(AL_xr$r, qxt(object=MultiDecrTable, x=x2:70, t=1, decrement=decrement2), col=2, 
       ylim = c(0,1), t="o", xlab="", ylab="", axes=FALSE)
axis(side=4, at=seq(0, 1, .2))
mtext("Probability", side = 4, line = 3)
legend('topleft', legend=c('ALx', 'q(retire)'), col=1:2, lty=c(1,1), bty = 'n', cex = .6)

## ----echo=TRUE, tidy=TRUE-----------------------------------------------------
decrement3 = "withdrawal"
x3 = 30
y3 = 20
r3 = 45
BPM_CP3 = s_x * .5 * PensFund(x3, y3, r3, acttableAccPeriod = MultiDecrTable, 
                        decrement = decrement3,  
                        i = .04, j = .06, delta = .03, avg = 0, n=1, 
                        acttablePaymPeriod = soa08, CostMet = 'BPM_CP')


## ----echo=FALSE, tidy=TRUE----------------------------------------------------
par(mfrow=c(1,2))
plot(x3:r3, BPM_CP3$AL, t="o", ylab="ALx", xlab="Age")
title ("Actuarial liability. Example 3", cex.main=.9)
legend("topleft", legend = c("BPM_CD"), lty = 1, col = 1, cex=.6, bty = "n")
plot(x3:r3, BPM_CP3$NC, t="o", ylab="NCx", xlab="Age")
title ("Normal Cost. Example 3", cex.main=.9)
  legend("topleft", legend = c("BPM_CD"), 
         lty = 1, col = 1, cex=.6, bty = "n")


