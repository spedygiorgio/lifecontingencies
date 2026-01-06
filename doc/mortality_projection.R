### R code from vignette source 'mortality_projection.Rnw'

###################################################
### code chunk number 1: mortality_projection.Rnw:66-67
###################################################
options(width=80, prompt='R>')


###################################################
### code chunk number 2: load
###################################################
library(demography)
library(forecast)
library(lifecontingencies)


###################################################
### code chunk number 3: createDemogData
###################################################
#italyDemo<-hmd.mx(country="ITA", username="username@email.domain", 
#password="password", label="Italy")
load(file="mortalityDatasets.RData")


###################################################
### code chunk number 4: italyDemoFig
###################################################
par(mfrow=c(1,3))
plot(italyDemo,series="male",datatype="rate", main="Male rates")
plot(italyDemo,series="female",datatype="rate", main="Female rates")
plot(italyDemo,"total",datatype="rate", main="Total rates")


###################################################
### code chunk number 5: italyDemoFigTime
###################################################
par(mfrow=c(1,3))
plot(italyDemo,series="male",datatype="rate",
     plot.type="time", main="Male rates",xlab="Years")
plot(italyDemo,series="female",datatype="rate",
     plot.type="time", main="Female rates",xlab="Years")
plot(italyDemo,series="total",datatype="rate",
     plot.type="time", main="Total rates",xlab="Years")


###################################################
### code chunk number 6: fitLeeCarter
###################################################
italyLcaM<-lca(italyDemo,series="male",max.age=100)
italyLcaF<-lca(italyDemo,series="female",max.age=100)
italyLcaT<-lca(italyDemo,series="total",max.age=100)


###################################################
### code chunk number 7: leeCarterResultsFig
###################################################
  par(mfrow=c(1,3))
  plot(italyLcaT$ax, main="ax", xlab="Age",ylab="ax",type="l")
  lines(x=italyLcaF$age, y=italyLcaF$ax, main="ax", col="red")
  lines(x=italyLcaM$age, y=italyLcaM$ax, main="ax", col="blue")
  legend("topleft" , c("Male","Female","Total"),
  cex=0.8,col=c("blue","red","black"),lty=1);
  plot(italyLcaT$bx, main="bx", xlab="Age",ylab="bx",type="l")
  lines(x=italyLcaF$age, y=italyLcaF$bx, main="bx", col="red")
  lines(x=italyLcaM$age, y=italyLcaM$bx, main="bx", col="blue")
  legend("topright" , c("Male","Female","Total"),
  cex=0.8,col=c("blue","red","black"),lty=1);
  plot(italyLcaT$kt, main="kt", xlab="Year",ylab="kt",type="l")
  lines(x=italyLcaF$year, y=italyLcaF$kt, main="kt", col="red")
  lines(x=italyLcaM$year, y=italyLcaM$kt, main="kt", col="blue")
  legend("topright" , c("Male","Female","Total"),
  cex=0.8,col=c("blue","red","black"),lty=1);


###################################################
### code chunk number 8: ktProjections
###################################################
fM<-forecast(italyLcaM,h=110)
fF<-forecast(italyLcaF,h=110)
fT<-forecast(italyLcaT,h=110)


###################################################
### code chunk number 9: ktProjectionFig
###################################################
par(mfrow=c(1,3))
plot(fM$kt.f,main="Male")
plot(fF$kt.f,main="Female",)
plot(fT$kt.f,main="Total")


###################################################
### code chunk number 10: ktrates
###################################################
ratesM<-cbind(italyDemo$rate$male[1:100,],fM$rate$male[1:100,])
ratesF<-cbind(italyDemo$rate$female[1:100,],fF$rate$female[1:100,])
ratesT<-cbind(italyDemo$rate$total[1:100,],fT$rate$total[1:100,])


###################################################
### code chunk number 11: ktratesFig
###################################################
par(mfrow=c(1,1))
plot(seq(min(italyDemo$year),max(italyDemo$year)+110),ratesF[65,],
     col="red",xlab="Years",ylab="Death Rates",type="l")
lines(seq(min(italyDemo$year),max(italyDemo$year)+110),ratesM[65,],
      col="blue",xlab="Years",ylab="Death Rates")
lines(seq(min(italyDemo$year),max(italyDemo$year)+110),ratesT[65,],
      col="black",xlab="Years",ylab="Death Rates")
legend("topright" , c("Male","Female","Total"),
       cex=0.8,col=c("blue","red","black"),lty=1);


###################################################
### code chunk number 12: lifeTableProject
###################################################

createActuarialTable<-function(yearOfBirth,rate){

  mxcoh <- rate[1:nrow(rate),(yearOfBirth-min(italyDemo$year)+1):ncol(rate)]
  cohort.mx <- diag(mxcoh)
  cohort.px=exp(-cohort.mx)
  #get projected Px
  fittedPx=cohort.px #add px to table
	px4Completion=seq(from=cohort.px[length(fittedPx)], to=0, length=20)
	totalPx=c(fittedPx,px4Completion[2:length(px4Completion)])
	#create life table
	irate=1.04/1.02-1

	cohortLt=probs2lifetable(probs=totalPx, radix=100000,type="px", 
  name=paste("Cohort",yearOfBirth))
	cohortAct=new("actuarialtable",x=cohortLt@x, lx=cohortLt@lx, 
	interest=irate, name=cohortLt@name)
	return(cohortAct)
	}




###################################################
### code chunk number 13: annuityAPV
###################################################
	getAnnuityAPV<-function(yearOfBirth,rate) {
		actuarialTable<-createActuarialTable(yearOfBirth,rate)
		out=axn(actuarialTable,x=65,m=12)
		return(out)
	}
rate<-ratesM
for(i in seq(1920,2000,by=10)) {
		cat("For cohort ",i, "of males the e0 is",
		round(exn(createActuarialTable(i,rate)),2),
		" and the APV is :",round(getAnnuityAPV(i,rate),2),"\n")
		
	}
rate<-ratesF
for(i in seq(1920,2000,by=10)) {
  	cat("For cohort ",i, "of females the e0 at birth is",
	round(exn(createActuarialTable(i,rate)),2),
	" and the APV is :",round(getAnnuityAPV(i,rate),2),"\n")
		
	}
rate<-ratesT
for(i in seq(1920,2000,by=10)) {
    cat("For cohort ",i, "of total population the e0 is",
		round(exn(createActuarialTable(i,rate)),2),
		" and the APV is :",round(getAnnuityAPV(i,rate),2),"\n")
		
	}


###################################################
### code chunk number 14: r_stmomoconfig
###################################################
#loading StMoMo package
require(StMoMo) 
#get data
##use total death, mid year exposures
ita.StMoMoData<-StMoMoData(data=italyDemo, series = "total",type="central")
#bring to initial year
ita.StMoMoData.Ini<-central2initial(ita.StMoMoData)
#assume max age 103
ages.fit = 0:103
#generate weight matrix
wxt <- genWeightMat(ages = ages.fit,  years = ita.StMoMoData.Ini$years,clip = 3)


###################################################
### code chunk number 15: r_stmomofit (eval = FALSE)
###################################################
## #fitting the models
## ## LC
## LC <- lc(link = "logit")
## LCfit <- fit(LC, data = ita.StMoMoData.Ini, ages.fit = ages.fit, wxt = wxt)
## ## CBD
## CBD <- cbd()
## CBDfit <- fit(CBD, data = ita.StMoMoData.Ini, ages.fit = ages.fit, wxt = wxt)


###################################################
### code chunk number 16: stmomo_rh_fit (eval = FALSE)
###################################################
## ## RH
## RH <- rh(link = "logit", cohortAgeFun = "1")
## RHfit <- fit(RH, data = ita.StMoMoData.Ini, ages.fit = ages.fit,   wxt = wxt,start.ax = LCfit$ax,
##              start.bx = LCfit$bx, start.kt = LCfit$kt)


###################################################
### code chunk number 17: r_stmomosaveandload
###################################################
#save(list=c("LCfit","CBDfit"),file="StMoMoModels.RData",compress = "xz")
load(file="StMoMoModels.RData")


###################################################
### code chunk number 18: stmomo_lcplot
###################################################
plot(LCfit)


###################################################
### code chunk number 19: stmomo_cbdlot
###################################################
plot(CBDfit)


###################################################
### code chunk number 20: stmomo_rhplot (eval = FALSE)
###################################################
## plot(RHfit)


###################################################
### code chunk number 21: stmomo_cbdlot
###################################################
horizon=50
LCfor <- forecast(LCfit, h = horizon)
#assuming arima with drift
#RHfor <- forecast(RHfit, h = horizon, gc.order = c(1,1,0))
CBDfor <- forecast(CBDfit, h = horizon)


###################################################
### code chunk number 22: stmomo_simulate
###################################################
#simulazione & proiezione
##simulazione
nsims=100
#RHsim <- simulate(RHfit, nsim = nsims, h = 50, gc.order = c(1, 1, 0))
LCsim <- simulate(LCfit, nsim = nsims, h = 50)
CBDsim <- simulate(CBDfit, nsim = nsims, h = 50)


###################################################
### code chunk number 23: stmomo_lc_sims (eval = FALSE)
###################################################
## ##LC
## plot(LCfit$years, LCfit$kt[1, ], xlim = range(LCfit$years, LCsim$kt.s$years), ylim = range(LCfit$kt, LCsim$kt.s$sim[1, , 1:20]),type = "l", xlab = "year", ylab = "kt", main = "LC model simulations")
## matlines(LCsim$kt.s$years, LCsim$kt.s$sim[1, , 1:20], type = "l", lty = 1)


###################################################
### code chunk number 24: stmomo_lc_sims2
###################################################
##LC
plot(LCfit$years, LCfit$kt[1, ], xlim = range(LCfit$years, LCsim$kt.s$years), ylim = range(LCfit$kt, LCsim$kt.s$sim[1, , 1:20]),type = "l", xlab = "year", ylab = "kt", main = "LC model simulations")
matlines(LCsim$kt.s$years, LCsim$kt.s$sim[1, , 1:20], type = "l", lty = 1)


###################################################
### code chunk number 25: stmomo_lifetable (eval = FALSE)
###################################################
## #plotting historical fitted rates, until 2014
## chosen_cohort=1950
## plot(0:64, extractCohort(fitted(LCfit, type = "rates"), cohort = chosen_cohort),
## type = "l", log = "y", xlab = "age", ylab = "q(x)",
## main = "Cohort 1950 mortality rate", xlim = c(0,103), ylim = c(0.0005, 0.07))
## #adding fitted projections
## lines(65:103, extractCohort(LCfor$rates, cohort = chosen_cohort), lty = 2, lwd=2, col="red")


###################################################
### code chunk number 26: stmomo_lifetable2
###################################################
#plotting historical fitted rates, until 2014
chosen_cohort=1950
plot(0:64, extractCohort(fitted(LCfit, type = "rates"), cohort = chosen_cohort),
type = "l", log = "y", xlab = "age", ylab = "q(x)",
main = "Cohort 1950 mortality rate", xlim = c(0,103), ylim = c(0.0005, 0.07))
#adding fitted projections
lines(65:103, extractCohort(LCfor$rates, cohort = chosen_cohort), lty = 2, lwd=2, col="red")


###################################################
### code chunk number 27: stmomo_mortality_rates
###################################################
#LC
lc_historical_rates <- extractCohort(fitted(LCfit, type = "rates"), 
  cohort = chosen_cohort)
lc_forecasted_rates <- extractCohort(LCfor$rates, 
  cohort = chosen_cohort)
lc_rates_1950 <- c(lc_historical_rates,lc_forecasted_rates)
#RH
#rh_historical_rates <- extractCohort(fitted(RHfit, type = "rates"), cohort = chosen_cohort)
#rh_forecasted_rates <- extractCohort(RHfor$rates, cohort = chosen_cohort)
#rh_rates_1950 <- c(rh_historical_rates,rh_forecasted_rates)
#CBD
cbd_historical_rates <- extractCohort(fitted(CBDfit, type = "rates"), cohort = chosen_cohort)
cbd_forecasted_rates <- extractCohort(CBDfor$rates, cohort = chosen_cohort)
cbd_rates_1950 <- c(cbd_historical_rates,cbd_forecasted_rates)


###################################################
### code chunk number 28: rates_to_prob_1950
###################################################
lc_qx_1950<-mx2qx(lc_rates_1950)
#rh_qx_1950<-mx2qx(rh_rates_1950)
cbd_qx_1950<-mx2qx(cbd_rates_1950)


###################################################
### code chunk number 29: qx_to_lifetable
###################################################
lc_lifetable_1950<-probs2lifetable(probs=lc_qx_1950,type = "qx",
  name = paste("LC","1950","lt",sep="_"))
#rh_lifetable_1950<-probs2lifetable(probs=rh_qx_1950,type = "qx",name = paste("RH","1950","lt",sep="_"))
cbd_lifetable_1950<-probs2lifetable(probs=cbd_qx_1950,type = "qx",
  name = paste("CDB","1950","lt",sep="_"))


###################################################
### code chunk number 30: mortality_projection.Rnw:510-513
###################################################
exn(lc_lifetable_1950,x=65)
#exn(rh_lifetable_1950,x=65)
exn(cbd_lifetable_1950,x=65)


###################################################
### code chunk number 31: stmomo_act
###################################################
lc_acttbl_1950<-new("actuarialtable",x=lc_lifetable_1950@x,lx=lc_lifetable_1950@lx, interest=0.015,name="LC ActTbl")
#rh_acttbl_1950<-new("actuarialtable",x=rh_lifetable_1950@x,lx=rh_lifetable_1950@lx, interest=0.015,name="RH ActTbl")
cdb_acttbl_1950<-new("actuarialtable",x=cbd_lifetable_1950@x,lx=cbd_lifetable_1950@lx, interest=0.015,name="CBD ActTbl")


###################################################
### code chunk number 32: stmomo_axn
###################################################
axn(actuarialtable = lc_acttbl_1950,x=65)
#axn(actuarialtable = rh_acttbl_1950,x=65)
axn(actuarialtable = cdb_acttbl_1950,x=65)


