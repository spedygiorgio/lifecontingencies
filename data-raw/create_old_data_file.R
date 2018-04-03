require(data.table)
require(tidyverse)
#demoUsa
demoUsa<-fread(input = "./data-raw/demoUsa.csv",data.table = FALSE,encoding = "UTF-8")
devtools::use_data(demoUsa,overwrite = TRUE)
#soaLt
soaLt<-fread(input = "./data-raw/soaLt.csv",data.table = FALSE,encoding = "UTF-8")
devtools::use_data(soaLt,overwrite = TRUE)
#AF92Lt
#fwrite(x=AF92Lt.data, file = "./data-raw/AF92.csv")
AF92Lt.data<-as(AF92Lt, "data.frame")
AF92Lt.data$px<-NULL
AF92Lt.data$ex<-NULL
fwrite(x=AF92Lt.data, file = "./data-raw/AF92.csv")
AF92.data<-fread(input = "./data-raw/AF92.csv",data.table = FALSE, encoding = "UTF-8")
AF92Lt<-new("lifetable",x=AF92.data$x,lx=AF92.data$lx,name='AF92')
devtools::use_data(AF92Lt,overwrite = TRUE)
#demoUk
data("demoUk")
fwrite(x=demoUk, file="./data-raw/demoUk.csv")
demoUk<-fread(input = "./data-raw/demoUk.csv",data.table = FALSE, encoding = "UTF-8")
devtools::use_data(demoUk,overwrite = TRUE)