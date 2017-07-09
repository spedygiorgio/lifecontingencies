load("data-raw/deangelisdifalco.RData")
names(pxHealthyF)[1]<-"age"
names(pxHealthyM)[1]<-"age"
names(DisabledFemaleLifeTable)[1]<-"age"
names(DisabledMaleLifeTable)[1]<-"age"
names(qxDisabledF)[1]<-"age"
names(qxDisabledM)[1]<-"age"
names(qxHealthyF)[1]<-"age"
names(qxHealthyM)[1]<-"age"
names(TransitionHealthtoDisF)[1]<-"age"
names(TransitionHealthtoDisM)[1]<-"age"

de_angelis_di_falco<-list(pxHealthyF,pxHealthyM,DisabledFemaleLifeTable,DisabledMaleLifeTable,qxDisabledF,qxDisabledM,
                          qxHealthyF,qxHealthyM,TransitionHealthtoDisF,TransitionHealthtoDisM)


de_angelis_di_falco[["pxHealthyF"]]<-pxHealthyF
de_angelis_di_falco[["pxHealthyM"]]<-pxHealthyM
de_angelis_di_falco[["DisabledFemaleLifeTable"]]<-DisabledFemaleLifeTable
de_angelis_di_falco[["DisabledMaleLifeTable"]]<-DisabledMaleLifeTable
de_angelis_di_falco[["qxDisabledF"]]<-qxDisabledF
de_angelis_di_falco[["qxDisabledM"]]<-qxDisabledM
de_angelis_di_falco[["qxHealthyF"]]<-qxHealthyF
de_angelis_di_falco[["qxHealthyM"]]<-qxHealthyM
de_angelis_di_falco[["TransitionHealthtoDisF"]]<-TransitionHealthtoDisF
de_angelis_di_falco[["TransitionHealthtoDisM"]]<-TransitionHealthtoDisM

devtools::use_data(de_angelis_di_falco, overwrite = TRUE)