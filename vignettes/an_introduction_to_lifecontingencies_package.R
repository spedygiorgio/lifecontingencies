### R code from vignette source 'an_introduction_to_lifecontingencies_package.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: setup
###################################################
	options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
	set.seed(123)
	numSim=200


###################################################
### code chunk number 2: load
###################################################
library("lifecontingencies")


###################################################
### code chunk number 3: structure1
###################################################
showClass("lifetable")


###################################################
### code chunk number 4: structure2
###################################################
showClass("actuarialtable")


###################################################
### code chunk number 5: structure3
###################################################
showMethods(classes=c("actuarialtable","lifetable"))


###################################################
### code chunk number 6: ir1
###################################################
interest2Discount(0.03)
discount2Interest(interest2Discount(0.03))


###################################################
### code chunk number 7: ir1
###################################################
convertible2Effective(i=0.10,k=4)


###################################################
### code chunk number 8: npv1
###################################################
capitals <- c(-1000,200,500,700)
times <- c(0,1,2,5)
presentValue(cashFlows=capitals, timeIds=times, interestRates=0.03)


###################################################
### code chunk number 9: npv3
###################################################
presentValue(cashFlows=capitals, timeIds=times, 
interestRates=c( 0.04, 0.02, 0.03, 0.05), 
probabilities=c(1,1,1,0.5))


###################################################
### code chunk number 10: npv3
###################################################
getIrr <- function(p) (presentValue(cashFlows=capitals, timeIds=times, 
interestRates=p) - 0)^2
nlm(f=getIrr, p=0.1)$estimate


###################################################
### code chunk number 11: ann1
###################################################
100 * annuity(i=0.03, n=5)


###################################################
### code chunk number 12: ann2
###################################################
100 * accumulatedValue(i=0.03, n=5)


###################################################
### code chunk number 13: ann3
###################################################
ann1 <- annuity(i=0.03, n=5, k=1, type="immediate")
ann2 <- annuity(i=0.03, n=5, k=12, type="immediate")
c(ann1,ann2)


###################################################
### code chunk number 14: ann5
###################################################
incrAnn <- increasingAnnuity(i=0.03, n=10, type="due")
decrAnn <- decreasingAnnuity(i=0.03, n=10, type="immediate")
c(incrAnn, decrAnn)


###################################################
### code chunk number 15: ann6
###################################################
annuity(i=((1+0.04)/(1+0.03)-1), n=10)


###################################################
### code chunk number 16: capAmort1
###################################################
capital <- 100000
interest <- 0.05 
payments_per_year <- 2
rate_per_period <- (1+interest)^(1/payments_per_year)-1
years <- 30
R <- 1/payments_per_year * 
capital/annuity(i=interest, n=years, 
		k=payments_per_year)
R


###################################################
### code chunk number 17: capAmort2
###################################################
balanceDue <- numeric(years * payments_per_year)
balanceDue[1] <- capital * (1+rate_per_period) - R
for(i in 2:length(balanceDue)) balanceDue[i]<-
  balanceDue[i-1] * (1+rate_per_period) - R


###################################################
### code chunk number 18: figBalanceDue
###################################################
plot(x=c(1:length(balanceDue)), y=balanceDue, main="Loan amortization", 
		ylab="EoP balance due", xlab="year", type="l",col="steelblue")


###################################################
### code chunk number 19: BPFun1
###################################################
bond<-function(faceValue, couponRate, couponsPerYear, yield,maturity)
{
	out <- numeric(1)
	numberOfCF <- maturity * couponsPerYear
	CFs <- numeric(numberOfCF)
	payments <- couponRate * faceValue / couponsPerYear 
	cf <- payments * rep(1,numberOfCF)
	cf[numberOfCF] <- faceValue + payments 
	times <- seq.int(from=1/couponsPerYear, to=maturity, 
                by=maturity/numberOfCF)
	out <- presentValue(cashFlows=cf, interestRates=yield, 
                   timeIds=times)
	return(out)
}

perpetuity<-function(yield, immediate=TRUE)
{
	out <- numeric(1)
	out <- 1 / yield
	out <- ifelse(immediate==TRUE, out, out*(1+yield))
	return(out)
}



###################################################
### code chunk number 20: BPFun2
###################################################
bndEx1 <-bond(1000, 0.06, 2, 0.05, 3)
bndEx2 <-bond(1000, 0.06, 2, 0.06, 3)
ppTy1 <-perpetuity(0.1)
c(bndEx1, bndEx2, ppTy1)


###################################################
### code chunk number 21: durationAndConvexity
###################################################
cashFlows <- c(100,100,100,600,500,700)
timeVector <- seq(1:6)
interestRate <- 0.03
dur1 <-duration(cashFlows = cashFlows, timeIds = timeVector, 
		i = interestRate, k = 1, macaulay = TRUE)
dur2 <-duration(cashFlows = cashFlows, timeIds = timeVector, 
		i = interestRate, k = 1, macaulay = FALSE)
cvx1 <-convexity(cashFlows = cashFlows, timeIds = timeVector, 
		i = interestRate, k = 1)
c(dur1, dur2, cvx1)


###################################################
### code chunk number 22: alm1
###################################################
GTCFin<- 10000 * (1 + 0.05)^7
GTCFin


###################################################
### code chunk number 23: alm2
###################################################
yieldT0 <- 0.04
durLiab <- 7
pvLiab <- presentValue(cashFlows = GTCFin,timeIds = 7,
		interestRates = yieldT0)
convLiab <- convexity(cashFlows=GTCFin, timeIds = 7, 
		i=yieldT0)
pvBond <- bond(100,0.03,1,yieldT0,5)
durBond <- duration(cashFlows=c(3,3,3,3,103), 
		timeIds=seq(1,5), i = yieldT0)
convBond <- convexity(cashFlows=c(3,3,3,3,103), 
		timeIds=seq(1,5), i = yieldT0)
pvPpty <- perpetuity(yieldT0)
durPpty <- (1+yieldT0)/yieldT0
covnPpty <- 2/(yieldT0^2)


###################################################
### code chunk number 24: alm3
###################################################
a <- matrix(c(durBond, durPpty,1,1), nrow=2, 
		byrow=TRUE)
b <- as.vector(c(7,1))
weights <-solve(a,b)
weights


###################################################
### code chunk number 25: alm4
###################################################
bondNum <- weights[1] * pvLiab / pvBond
pptyNum <- weights[2] * pvLiab / pvPpty	
bondNum
pptyNum


###################################################
### code chunk number 26: alm5
###################################################
convAsset <- weights[1] * convBond + weights[2] * covnPpty
convAsset>convLiab


###################################################
### code chunk number 27: alm6
###################################################
yieldT1low <- 0.03
immunizationTestLow <- (bondNum * bond(100,0.03,1,yieldT1low,5) + 
			pptyNum * perpetuity(yieldT1low)> 
			GTCFin / (1+yieldT1low)^7)
yieldT1high <- 0.05
immunizationTestHigh <- (bondNum * bond(100,0.03,1,yieldT1high,5) + 
			pptyNum * perpetuity(yieldT1high)>
			GTCFin/(1+yieldT1high)^7)
immunizationTestLow
immunizationTestHigh


###################################################
### code chunk number 28: createALifecontingenciesObject
###################################################
x_example <- seq(from=0,to=9, by=1)
lx_example <- c(1000,950,850,700,680,600,550,400,200,50)
exampleLt <- new("lifetable", x=x_example, lx=lx_example, 
		name="example lifetable")


###################################################
### code chunk number 29: printShow
###################################################
print(exampleLt)


###################################################
### code chunk number 30: headAndTail
###################################################
head(exampleLt)


###################################################
### code chunk number 31: fromDataFrame1
###################################################
data("demoUsa")
data("demoIta") 
usaMale07 <- demoUsa[,c("age", "USSS2007M")]
usaMale00 <- demoUsa[,c("age", "USSS2000M")]
names(usaMale07) <- c("x","lx")
names(usaMale00) <- c("x","lx")
usaMale07Lt <-as(usaMale07,"lifetable")
usaMale07Lt@name <- "USA MALES 2007"
usaMale00Lt <-as(usaMale00,"lifetable")
usaMale00Lt@name <- "USA MALES 2000"


###################################################
### code chunk number 32: fromDataFrame2
###################################################
lxIPS55M <- with(demoIta, IPS55M)
pos2Remove <- which(lxIPS55M %in% c(0,NA))
lxIPS55M <-lxIPS55M[-pos2Remove]
xIPS55M <-seq(0,length(lxIPS55M)-1,1)
ips55M <- new("lifetable",x=xIPS55M, lx=lxIPS55M, 
		name="IPS 55 Males")
lxIPS55F <- with(demoIta, IPS55F)
pos2Remove <- which(lxIPS55F %in% c(0,NA))
lxIPS55F <- lxIPS55F[-pos2Remove]
xIPS55F <- seq(0,length(lxIPS55F)-1,1)
ips55F <- new("lifetable",x=xIPS55F, lx=lxIPS55F, 
		name="IPS 55 Females")


###################################################
### code chunk number 33: createFromSurvivalRates
###################################################
data("demoIta")
itaM2002 <- demoIta[,c("X","SIM92")]
names(itaM2002) <- c("x","lx")
itaM2002Lt <- as(itaM2002,"lifetable")
itaM2002Lt@name <- "IT 2002 Males"
itaM2002 <- as(itaM2002Lt,"data.frame")
itaM2002$qx <- 1-itaM2002$px
for(i in 20:60) itaM2002$qx[itaM2002$x==i] = 0.2 * itaM2002$qx[itaM2002$x==i]
itaM2002reduced <- probs2lifetable(probs=itaM2002[,"qx"], radix=100000,
		type="qx",name="IT 2002 Males reduced")


###################################################
### code chunk number 34: createAnActuarialtableObject
###################################################
exampleAct <- new("actuarialtable",x=exampleLt@x, lx=exampleLt@lx, 
interest=0.03, name="example actuarialtable")


###################################################
### code chunk number 35: methods1
###################################################
getOmega(exampleAct)


###################################################
### code chunk number 36: methods2
###################################################
print(exampleLt)
print(exampleAct)


###################################################
### code chunk number 37: methods3
###################################################
exampleActDf <- as(exampleAct, "data.frame")


###################################################
### code chunk number 38: figSurvivalFunction0
###################################################
data("soa08Act")
soa08ActDf <- as(soa08Act, "data.frame")


###################################################
### code chunk number 39: figSurvivalFunction
###################################################
plot(soa08Act, type="l",col="steelblue")


###################################################
### code chunk number 40: probabilityAndDemographics
###################################################
demoEx1<-pxt(ips55M,20,1)
demoEx2<-qxt(ips55M,30,2) 
demoEx3<-exn(ips55M, 50,20,"complete") 
c(demoEx1,demoEx2,demoEx3)


###################################################
### code chunk number 41: fractionalAges
###################################################
data("soa08Act")
pxtLin <- pxt(soa08Act,80,0.5,"linear") 
pxtCnst <- pxt(soa08Act,80,0.5,"constant force") 
pxtHyph <- pxt(soa08Act,80,0.5,"hyperbolic") 
c(pxtLin,pxtCnst,pxtHyph)


###################################################
### code chunk number 42: moreThanOneHead
###################################################
tablesList <- list(ips55M, ips55F)
jsp <- pxyzt(tablesList, x=c(65,63), t=2)
lsp <- pxyzt(tablesList, x=c(65,63), t=2, status="last") 
jelt <- exyzt(tablesList, x=c(65,63), status="joint") 
c(jsp,lsp,jelt)


###################################################
### code chunk number 43: lifeIns1
###################################################
data(soa08Act)
UComm <- Axn(actuarialtable=soa08Act, x=25, n=65-25, k=12)
UCpt <- ((soa08ActDf$Mx[26]-soa08ActDf$Mx[66])/soa08ActDf$Dx[26]) * 
		0.06/real2Nominal(i=0.06,k=12)
c(UComm, UCpt)


###################################################
### code chunk number 44: lifeIns2
###################################################
P <- UCpt/axn(actuarialtable=soa08Act,x=25,n=10)
P


###################################################
### code chunk number 45: lifeIns3
###################################################
(10 + 1 ) * Axn(actuarialtable=soa08Act, x=25, n=10) 
DAxn(actuarialtable = soa08Act, x=25, n=10) + 
IAxn(actuarialtable = soa08Act, x=25, n=10)


###################################################
### code chunk number 46: annuity1
###################################################
UCpt <- axn(actuarialtable=soa08Act, x=75, m=10)
UComm <- with(soa08ActDf,Nx[86]/Dx[76])
c(UCpt,UComm)


###################################################
### code chunk number 47: annuity2
###################################################
P=axn(actuarialtable=soa08Act, x=75, m=10) / 
		axn(actuarialtable=soa08Act, x=75, n=5)
P

PComm <- with(soa08ActDf,(Nx[86]/Dx[76]) / 
				((Nx[76]-Nx[81])/Dx[76]))
PComm


###################################################
### code chunk number 48: annuity3
###################################################
U <- axn(actuarialtable=soa08Act, x=75, m=10, k=12)
P <- axn(actuarialtable=soa08Act, x=75, m=10, k=12) / 
		axn(actuarialtable=soa08Act, x=75, n=5)
c(U,P)


###################################################
### code chunk number 49: lifeInsuranceBenefitReserve
###################################################
P=100000 * Axn(soa08Act,x=25,n=40)/axn(soa08Act,x=25,n=40)
reserveFun = function(t) return(100000*Axn(soa08Act,x=25+t,n=40-t)-P*
					axn(soa08Act,x=25+t,n=40-t))
for(t in 0:40) {if(t%%5==0) cat("At time ",t,
				" benefit reserve is ", 
				reserveFun(t),"\n")}


###################################################
### code chunk number 50: annuityReserve
###################################################
yearlyRate <- 12000
irate <- 0.02
APV <- yearlyRate*axn(soa08Act, x=25, i=irate,m=65-25,k=12)
levelPremium <- APV/axn(soa08Act, x=25,n=65-25,k=12)

annuityReserve<-function(t) {
	out<-NULL
	if(t<65-25) out <- yearlyRate*axn(soa08Act, x=25+t, 
    i=irate, m=65-(25+t),k=12)-levelPremium*axn(soa08Act, 
              x=25+t, n=65-(25+t),k=12) else {
		out <- yearlyRate*axn(soa08Act, x=25+t, i=irate,k=12)
	}
	return(out)
}

years <- seq(from=0, to=getOmega(soa08Act)-25-1,by=1)
annuityRes <- numeric(length(years))
for(i in years) annuityRes[i+1] <- annuityReserve(i)
dataAnnuityRes <- data.frame(years=years, reserve=annuityRes)


###################################################
### code chunk number 51: annuityReserveGraph
###################################################
plot(y=dataAnnuityRes$reserve, x=dataAnnuityRes$years,
col="steelblue", main="Deferred annuity benefit reserve",
ylab="amount",xlab="years",type="l")


###################################################
### code chunk number 52: expAugmented
###################################################
G <- (100000*Axn(soa08Act, x=35) + (2.5*100000/1000 + 25)*
			axn(soa08Act,x=35))/((1-.1)*axn(soa08Act,x=35))
G


###################################################
### code chunk number 53: twoHeadsAnnuitImmediate
###################################################
twoLifeTables <- list(maleTable=soa08Act, femaleTable=soa08Act)
axn(soa08Act, x=65,m=1)+axn(soa08Act, x=70,m=1)-
axyn(soa08Act,soa08Act,	x=65,y=70,status="joint",m=1) 
axyzn(tablesList=twoLifeTables, x=c(65,y=70), status="last",m=1)


###################################################
### code chunk number 54: revesionaryAnuity
###################################################
axn(actuarialtable = soa08Act, x=60,m=1)-
		axyzn(tablesList = twoLifeTables, 
				x=c(65,60),status="joint",m=1)


###################################################
### code chunk number 55: rLife1
###################################################
rLife(n = 5, object = soa08Act, x = 45, type = "Kx")


###################################################
### code chunk number 56: rLife2
###################################################
futureLifetimes <- as.data.frame(rLifexyz(n=numSim, 
				tablesList=list(husband=ips55M,wife=ips55F),
				x=c(68,65), type="Tx"))
names(futureLifetimes) <- c("husband","wife")
temp <- futureLifetimes$wife - futureLifetimes$husband
futureLifetimes$widowance  <-  sapply(temp, max,0)
mean(futureLifetimes$widowance)


###################################################
### code chunk number 57: widowanceFig
###################################################
hist(futureLifetimes$widowance, freq=FALSE, main="Distribution of widowance yars",
			xlab="Widowance years", col="steelblue", nclass=100);abline(v=mean(futureLifetimes$widowance), 
			col="red", lwd=2)


###################################################
### code chunk number 58: AxnAPVAndStochastic
###################################################
APVAxn <- Axn(soa08Act,x=25,n=40,type="EV")
APVAxn
sampleAxn <- rLifeContingencies(n=numSim, lifecontingency="Axn",
		object=soa08Act,x=25,t=40,parallel=TRUE)
tt1<-t.test(x=sampleAxn,mu=APVAxn)$p.value

APVIAxn <- IAxn(soa08Act,x=25,n=40,type="EV")
APVIAxn
sampleIAxn <- rLifeContingencies(n=numSim, lifecontingency="IAxn",
		object=soa08Act,x=25,t=40,parallel=TRUE)
tt2<-t.test(x=sampleIAxn,mu=APVIAxn)$p.value

APVaxn <- axn(soa08Act,x=25,n=40,type="EV")
APVaxn
sampleaxn <- rLifeContingencies(n=numSim, lifecontingency="axn",
		object=soa08Act,x=25,t=40,parallel=TRUE)
tt3 <- t.test(x=sampleaxn,mu=APVaxn)$p.value

APVAExn <- AExn(soa08Act,x=25,n=40,type="EV")
APVAExn
sampleAExn <- rLifeContingencies(n=numSim, lifecontingency="AExn",
		object=soa08Act,x=25,t=40,parallel=TRUE)
tt4<-t.test(x=sampleAExn,mu=APVAExn)$p.value
c(tt1, tt2,tt3, tt4)


###################################################
### code chunk number 59: figsim
###################################################
	par(mfrow=c(2,2))
	hist(sampleAxn, main="Term Insurance", xlab="Actuarial present value",nclass=50, col="steelblue",freq=FALSE);abline(v=APVAxn, col="red", lwd=2)
	hist(sampleIAxn, main="Increasing Life Insurance", xlab="Actuarial present value",nclass=50, col="steelblue",freq=FALSE);abline(v=APVIAxn, col="red", lwd=2)
	hist(sampleaxn, main="Temporary Annuity Due", xlab="Actuarial present value",nclass=50, col="steelblue",freq=FALSE);abline(v=APVaxn, col="red", lwd=2)
	hist(sampleAExn, main="Endowment Insurance", xlab="Actuarial present value",nclass=50, col="steelblue",freq=FALSE);abline(v=APVAExn, col="red", lwd=2)


###################################################
### code chunk number 60: benefitPremium1
###################################################
APV <- Axn(actuarialtable = soa08Act, x=25, n=40)
APV


###################################################
### code chunk number 61: benefitPremium2
###################################################
samples <- rLifeContingencies(n=numSim, lifecontingency = "Axn", 
		object= soa08Act, x=25,t=40,paralle=TRUE)
pct90Pr <- as.numeric(quantile(samples,.90))
pct90Pr


###################################################
### code chunk number 62: benefitPremium3
###################################################
pct90Pr2 <- qnorm(p=0.90,mean=APV, sd=sd(samples)/sqrt(1000))
pct90Pr2


###################################################
### code chunk number 63: stochasticExampleFull1
###################################################
nsim <- 100
employees <- 100
salaryDistribution <- rlnorm(n=employees,m=10.77668944,s=0.086177696)
ageDistribution <- round(runif(n=employees,min=25, max=65))
policyLength <- sapply(65-ageDistribution, min, 1)

getEmployeeBenefit<-function(index,type="EV") {
	out <- numeric(1)
	out <- salaryDistribution[index]*Axn(actuarialtable=soa08Act, 
			x=ageDistribution[index],n=policyLength[index], 
			i=0.02,m=0,k=1, type=type)
	return(out)
}

require(parallel)
cl <- makeCluster(detectCores())
worker.init <- function(packages) {
	for (p in packages) {
		library(p, character.only=TRUE)
	}
	invisible(NULL)
}
clusterCall(cl, 
		worker.init, c('lifecontingencies'))
clusterExport(cl, varlist=c("employees","getEmployeeBenefit",
				"salaryDistribution","policyLength",
				"ageDistribution","soa08Act"))


###################################################
### code chunk number 64: stochasticExampleFull2
###################################################
employeeBenefits <- numeric(employees)
employeeBenefits <- parSapply(cl, 1:employees,getEmployeeBenefit, type="EV")
employeeBenefit <- sum(employeeBenefits)



benefitDistribution<-numeric(nsim)
yearlyBenefitSimulate<-function(i)
{
	out <- numeric(1)
	expenseSimulation <- numeric(employees)
	expenseSimulation <- sapply(1:employees, getEmployeeBenefit, type="ST")
	out <- sum(expenseSimulation)
	return(out)
}

benefitDistribution <- parSapply(cl, 1:nsim,yearlyBenefitSimulate )
stopCluster(cl)

riskMargin <- as.numeric(quantile(benefitDistribution,.75)-employeeBenefit)
totalBookedCost <- employeeBenefit+riskMargin

employeeBenefit
riskMargin
totalBookedCost


