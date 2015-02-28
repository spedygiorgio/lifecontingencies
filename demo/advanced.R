###############################################################################
# Author: Giorgio A. Spedicato
###############################################################################



#calculates the last survival status annuity due APV for a couple of policyholders aged
#30 / 40
coupleLifeTables=list(soa08Act,soa08Act)
axyzn(tablesList=coupleLifeTables,x=c(30,40),n=10,m=1)
#Consider a 20-year term insurance on (30), face value equal to 1000.
#Calculate the gross level yearly premium allowing for 30 flat expenses and
#mainteance yearly expenses equal to 15\% of G.

#G*axn(soa08Act,x=30)=1000*Axn(soa08Act)+30+0.15*G*axn(soa08Act,x=30)
G=(1000*Axn(soa08Act,30)+30)/(0.85*axn(soa08Act,x=30))
G

#Sample a vector of size 100K from $K_25$ and compute summary statistics.

K25=rLife(n = 10^5,object = soa08Act,x=25,type="Kx")
summary(K25)
exn(soa08Act,25)
#hist(K25)

#Find the smallest premium to charge to a policyholder aged 65 that wish
#to purchase an annuity due in order to limit the probability of loss to 25\%

APV=axn(soa08Act, 65)
axn65=rLifeContingencies(n=10000,lifecontingency="axn",object=soa08Act,x=65,parallel=TRUE)
mean(axn65)
APV 
quantile(axn65,1-0.25) #percentile premium
