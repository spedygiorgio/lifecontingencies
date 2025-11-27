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
#data("demoUk")
#fwrite(x=demoUk, file="./data-raw/demoUk.csv")
demoUk<-fread(input = "./data-raw/demoUk.csv",data.table = FALSE, encoding = "UTF-8")
devtools::use_data(demoUk,overwrite = TRUE)

#ita2024
ita2024<-fread(input = "./data-raw/ita_2024.csv",data.table = FALSE, encoding = "UTF-8")
# renamings:  "Età  x" -> x                                       "Sopravviventi  lx"  -> lx
ita2024 <- rename(ita2024, c("x" = "Età  x", "lx"="Sopravviventi  lx")) %>% dplyr::select(x,lx)
# keep rows where lx>0
ita2024 <- ita2024 %>% filter(lx > 0) %>% arrange(x)
usethis::use_data(ita2024,overwrite = TRUE)

#ita2023
ita2023<-fread(input = "./data-raw/ita_2023.csv",data.table = FALSE, encoding = "UTF-8")
ita2023<-rename(ita2023, c("x"= "Età  x", "lx"="Sopravviventi  lx"))
# keep rows where lx>0
ita2023 <- ita2023 %>% filter(lx > 0) %>% arrange(x)
usethis::use_data(ita2023,overwrite = TRUE)