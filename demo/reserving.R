# TODO: Reserving life insurances
# 
# Author: Giorgio Alfredo Spedicato
###############################################################################



###calculate the reserve at time 4 for a life insurance on 60 paid with 5 yearly premium
###as long as the policyholder is alive from year

P=Axn(soa08Act, 60)/axn(soa08Act, 60,5)
V4=Axn(soa08Act, 60+4) - P*axn(soa08Act, 60+4,5-4)

#For a 20 year term life insurance on a (75) year policiholder calculate V(5)
#Assume benefit premiums paid on montly basis.

U=Axn(soa08Act, 75,20)
P=U/axn(soa08Act, 75,20)
V=Axn(soa08Act, x=75+5,n=20-5)-P*axn(soa08Act, 75+5,20-5)
V

#Calculate the benefit reserve for a 20-year endowment on a policyholder
#aged (60) at time 10 assuming level benefit premiums to be paid for 15 year.

U=AExn(soa08Act, 60,20)
P=U/axn(soa08Act, 60,15)
V=AExn(soa08Act, 60+10,20-10)-P*axn(soa08Act, 60+10,15-10)
V

#Calculate the benefit reserve at time 10 for a whole life insurance on 60
#payable at the end of year of death with benefit premiums paid quarterly

U=Axn(soa08Act,x=60)
P=U/axn(soa08Act, x=60,k=4)
V=Axn(soa08Act,x=60+10)-P*axn(soa08Act, x=60+10,k=4)
V