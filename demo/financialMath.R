#Find the compound interest rate $i^{\left(4\rigth)}(4)$ which is equivalent to a rate of
#compound interest of 8\% payable semi-annually. 

iannual=nominal2Real(i=0.08,k=2)
iquarterly=real2Nominal(iannual,4)

#In return for a payment of \$1,000 at the end of 10 years, a lender agrees to
#pay \$200 immediately, \$500 at the end of 6 years, and a fnal amount at the
#end of 15 years. Find the amount of the final payment at the end of 15 years
#if the nominal rate of interest is 5\% converted semiannually.

x=1000
cashFlows=c(200,500,-1000,x)
times=c(0,6,10,15)
i=nominal2Real(0.05,2)
f<-function(x) presentValue(cashFlows=cashFlows,timeIds = times,interestRates = i)
x<-nlm(f, 500)$minimum
x

#Find the IRR of an investment that requires investing 1000 for five years
#and returning 500 for the following fifteen years.

f<-function(p) (presentValue(cashFlows=c(rep(-1000,5),rep(500,15)),timeIds=0:19,interestRates = p)-0)^2
IRR=nlm(f, 0.05)$minimum
IRR



#What would you be willing to pay for an infnite stream of $37 annual payments
#(cash inflows) beginning now if the interest rate is 8% per annum?

37*annuity(i=0.08,n=Inf, type="due")

#John receives $400 at the end of the rst year, $350 at the end of the second
#year, $300 at the end of the third year and so on, until the final payment of
#$50. Using an annual effective rate of 3.5%, calculate the present value of
#these payments at time 0.

50*decreasingAnnuity(i=0.035,n=400/50)

#What is the difference between a annuity due with 5 years term, interest rate 3% if it is paid once or 
#fractionarily 12 times?
annuity(i=0.03,n=5,k=1,type="due")
annuity(i=0.03,n=5,k=2,type="due")

#A bond whose term is 5 years, par value 100 and yearly coupon rate is
#5\% prices at 95.45. Find its yield.

f<-function(yield) (100*(1+yield)^-5+5*annuity(i=yield,n=5)-95.45)^2
nlm(f,0.05)$estimate

#Redo the duration example taking into account the convexity.
CFs=c(3,3,3,3,103)
TIs=seq(1,5,1)
bondPrice=presentValue(cashFlows = CFs, timeIds = TIs,interestRates = 0.05)
bondPrice
Dur=duration(cashFlows = CFs,timeIds=TIs,i=0.05,macaulay=FALSE)
Conv=convexity(CFs,TIs,i=0.05)
Dur
Conv
Dy=-0.01 #change in interest rate
bondPrice2=presentValue(cashFlows = CFs, timeIds = TIs,interestRates = 0.05+Dy)
PerVar=(-Dur*Dy+0.5*Dy^2*Conv)
bondPrice2
bondPrice*(1+PerVar)