# Import life contingencies "sopravviventi - lx" from ISTAT data. 
# Export format should be PCX. Select
#  - all ages
#  - all years
#  - only the whole country (otherwise too many data)

library(lifecontingencies)
library(pxR)
ds<-read.px("ea5da20f-49d7-446e-9356-bee25647f342.px")
dsd<-as.data.frame(ds)

data(demoIta)

# Assuming "classi di età" in correct order
sset <- function(x,year,sex,len) {
  tmp<-subset(x,Anno==as.character(year) & Sesso==sex)
  tmp2<-tmp$value
  length(tmp2)<-len
  tmp2[is.na(tmp2)]<-0
  return(tmp2)
}

len <- nrow(demoIta)
demoIta$SIM81<-sset(dsd,1981,"maschi",len)
demoIta$SIF81<-sset(dsd,1981,"femmine",len)
demoIta$SIM92<-sset(dsd,1992,"maschi",len)
demoIta$SIF92<-sset(dsd,1992,"femmine",len)
demoIta$SIM00<-sset(dsd,2000,"maschi",len)
demoIta$SIF00<-sset(dsd,2000,"femmine",len)
demoIta$SIM02<-sset(dsd,2002,"maschi",len)
demoIta$SIF02<-sset(dsd,2002,"femmine",len)

save(demoIta,file="../data/demoIta_new.rda")

write.csv(demoIta,file="demoIta_new.csv")



