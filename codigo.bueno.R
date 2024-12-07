library(readxl)
library(ggplot2)
library(tidyverse)
library(EnvStats)
library(ExtDist)

datos <- read_excel("Base de datos_TP_Seguros.xlsx")[,c(1,4)] %>% `colnames<-`(c("tiempo", "cuantia"))
datos = datos %>% mutate(cuantia = cuantia / 10^6)

options(scipen = 999)


# Simulacion

## Poisson 
lambda_poisson = 25615*((1/2) * nrow(datos)/25615 +  (1/6) * 3.023 /24.752 + (2/6) *  3.581 / 25.348)

## Weibull

parametros_wei = eweibull(datos$cuantia)$parameters

set.seed(2023)

resultado = data.frame(s = rep(0, 10000), c = rep(0, 10000))
for (j in 1:10000) {
  
  # Poisson 
  
  poisson = rpois(1,lambda_poisson)
  resultado[j, 1] = poisson
  
  #Weibull
  
  datos_log = numeric(poisson)
  for (i in 1:length(datos_log)) {
    datos_log[i] =  rweibull(1, parametros_wei[1], parametros_wei[2])
  }
  resultado[j, 2] = sum(datos_log)
  
}
sum(datos$cuantia)
quantile(resultado)
(quantile(resultado, probs = .99) - median(resultado))

source("sampling.R")
set.seed(2023)
resultado_sampling = data.frame(s = rep(0, 10000), c = rep(0, 10000))
for (j in 1:10000) {
  
  # Poisson 
  
  poisson = rpois(1,lambda_poisson)
  resultado_sampling[j, 1] = poisson
  
  #Sampling
  
  datos_sampling =  dist_cuantias(poisson,datos$cuantia)
  
  resultado_sampling[j, 2] = sum(datos_sampling)
  
}
quantile(resultado_sampling)
(quantile(resultado_sampling, probs = .99) - median(resultado_sampling))

save(resultado, resultado_sampling, file = "resultados.RData")



