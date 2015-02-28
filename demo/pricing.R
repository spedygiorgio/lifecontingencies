#Create the Italian 92 male actuarial table at 3\% from the demoIta dataset.
data(demoIta)
SIM92=new("actuarialtable",x=demoIta$X[0:110],
		lx=demoIta$SIM92[0:110],interest=0.03)

#How much a 25 old policyholder should pay to receive \$ 100,000 when he
#turns 65, assuming he is in life?

Exn(SIM92,x=25,n=65-25)

#How much a 25 old policyholder should pay to receive \$ 100,000 when he
#turns 65 or when he dies?

AExn(SIM92,x=25,n=65-25)
Exn(SIM92,x=25,n=65-25)+Axn(SIM92,x=25,n=65-25)

#On the SOA actuarial table, calculate the APV whole life insurance for a
#policyholder aged 30 with benefit payable at the end of month of death at 4\% interest rate.

Axn(soa08Act, x=30,i=0.04,k=12)


#For the APV calculated at the above point, determine the level benefit
#premium assuming it will be paid until the policyholder's death.
Axn(soa08Act, x=30,i=0.04,k=12)/axn(soa08Act, x=30,k=0.04)

#Calculate the quarterly premium that a policyholder aged (50) shall pay
#until the year of death to insure a face value of 100,000. The face value will be paid at the end of the 
#quarter of death.

P=100000*Axn(soa08Act,50,k=4)/axn(soa08Act,x=50,k=4)

#Compute the 15-year benefit premium for a 35-year term insurance with benefit
#payable at the end of month of deat.
#Assume the interest rate to be 3 %% and the premium to be paid at the beginning of each month.

APV=100000*Axn(soa08Act, x=30,n=35,i=0.03, k=12) #Get the APV
Pa=APV/axn(soa08Act, x=30,n=15,i=0.03,k=12) #annualized benefit premium (payable monthly)
Pm=Pa/12 #montly benefit premium
Pm