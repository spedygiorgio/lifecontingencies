# TODO: Add comment
# 
# Author: Giorgio Spedicato
####################################################


data(soa08Act)

#Create one lifetable for tables CL1-CL3 in demoChina dataset
#and compare the expected residual lifetime.
data(demoChina)
tableCL1=probs2lifetable(probs=demoChina$CL1,radix=10000,type="qx","CL1")
tableCL2=probs2lifetable(probs=demoChina$CL2,radix=10000,type="qx","CL2")
tableCL3=probs2lifetable(probs=demoChina$CL3,radix=10000,type="qx","CL3")

exn(tableCL1)
exn(tableCL2)
exn(tableCL3)

#Find the probability of a life aged 2 to survive to age 4.
pxt(object=soa08Act,x=2,t=2)

#Assume that mortality follows the Illustrative Life Table. Calculate the probability
#that a life (65) will die between ages 80 and 90.

pxt(object=soa08Act,x=65,t=80-65)-pxt(object=soa08Act,x=65,t=90-65)

#Find the number of people dying between 35 and 45.

dxt(soa08Act, 35,10)

#calculate the two year curtate expectation of life for a policyholder aged 70

exn(soa08Act, 70,2)

#Calculate the probability that three brothers aged 14,15,16 will be all
#alive after 60 years.

listOfTables=list(soa08Act,soa08Act,soa08Act)
pxyzt(listOfTables, x=c(14,15,16),t=60,"joint")

