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

devtools::use_data(de_angelis_di_falco)