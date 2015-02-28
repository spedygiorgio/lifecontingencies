### R code from vignette source 'mortality_projection.Rnw'

###################################################
### code chunk number 1: load
###################################################
library(demography)
library(forecast)
library(lifecontingencies)
load(file="mortalityDatasets.RData")


###################################################
### code chunk number 2: createDemogData
###################################################
italyDemo<-demogdata(data=italyMx$rate$total,
		pop=italyMx$pop$total, 
		name="total",
		ages=italyMx$age, 
		years=italyMx$year, 
		type="mortality",
		label="Italy",lambda=1)


###################################################
### code chunk number 3: mortality_projection.Rnw:144-145
###################################################
	plot(italyDemo)


###################################################
### code chunk number 4: fitLeeCarter
###################################################
italyLca<-lca(italyDemo)


###################################################
### code chunk number 5: leeCarterResults
###################################################
	par(mfrow=c(1,3))
	plot(x=italyLca$age, y=italyLca$ax, main="ax")
	plot(x=italyLca$age, y=italyLca$bx, main="bx")
	plot(x=italyLca$year, y=italyLca$kt, main="kt")


###################################################
### code chunk number 6: ktProjections
###################################################
ktSeries<-italyLca$kt
ktArima<-auto.arima(ktSeries,allowdrift=TRUE,max.order=20)
ktArimaForecasts<-forecast(ktArima, h=110)
fullKt<-ts(c(ktArimaForecasts$fitted, ktArimaForecasts$mean),start=1872)	


###################################################
### code chunk number 7: ktProjectionPlot
###################################################
plot(fullKt)


###################################################
### code chunk number 8: lifeTableProject
###################################################

createActuarialTable<-function(yearOfBirth){
	#get projected Px
	ktSubset<-window(fullKt, start=yearOfBirth)
	predictionTable<-data.frame(age=italyLca$age,ax=italyLca$ax,bx=italyLca$bx)
	predictionTable$kt=ktSubset[1:nrow(predictionTable)]
	predictionTable$mux=with(predictionTable,exp(ax+bx*kt))
	predictionTable$px=with(predictionTable,exp(-mux))
	fittedPx=predictionTable$px
	px4Completion=seq(from=predictionTable$px[length(fittedPx)], to=0, length=20)
	totalPx=c(fittedPx,px4Completion[2:length(px4Completion)])
	#create life table
	irate=1.04/1.02-1

	cohortLt=probs2lifetable(probs=totalPx, radix=100000,type="px", name=paste("Cohort",yearOfBirth))
	cohortAct=new("actuarialtable",x=cohortLt@x, lx=cohortLt@lx, 
			interest=irate, name=cohortLt@name)
	return(cohortAct)
	}



###################################################
### code chunk number 9: annuityAPV
###################################################
	getAnnuityAPV<-function(yearOfBirth) {
		actuarialTable<-createActuarialTable(yearOfBirth)
		out=axn(actuarialTable,x=65,m=12)
		return(out)
	}
	for(i in seq(1920,2000,by=10)) {
		cat("For cohort ",i, " the expected lifetime at birth is",
				round(exn(createActuarialTable(i)),2),
				" and the APV is :",round(getAnnuityAPV(i),2),"\n")
		
	}


