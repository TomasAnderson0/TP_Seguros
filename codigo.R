library(readxl)
library(ggplot2)
library(tidyverse)

datos <- read_excel("Base de datos_TP_Seguros.xlsx")[,c(1,4)] %>% `colnames<-`(c("tiempo", "cuantia"))



ggplot(datos) + geom_histogram(aes(x = cuantia))


