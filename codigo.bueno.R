library(readxl)
library(ggplot2)
library(tidyverse)
library(EnvStats)
library(ExtDist)

datos <- read_excel("Base de datos_TP_Seguros.xlsx")[,c(1,4)] %>% `colnames<-`(c("tiempo", "cuantia"))
datos = datos %>% mutate(cuantia = cuantia / 10^6)

parametros = datos %>% group_by(tiempo) %>% count() %>% ungroup(tiempo) %>%  summarise(promedio = mean(n),
                                                                                       variancia = var(n))

datos %>% group_by(tiempo) %>% count() %>% ungroup(tiempo) %>% 
  ggplot() + geom_point(aes(x = tiempo, y = n))


ggplot(datos) + geom_histogram(aes(x = cuantia))

options(scipen = 999)

ggplot(datos) + geom_point(aes(x = tiempo, y = cuantia))

ggplot(datos) + geom_density(aes(x = cuantia))

# Simulacion


## Poisson 
lambda_poisson = 25615*((1/2) * nrow(datos)/25615 +  (1/6) * 3.023 /24.752 + (2/6) *  3.581 / 25.348)

## Weibull

parametros_wei = eweibull(datos$cuantia)$parameters


resultado = numeric()
for (j in 1:1000) {
  
  # Poisson 
  
  poisson = rpois(1,lambda_poisson)
  
  #Weibull
  
  datos_log = numeric(poisson)
  for (i in 1:length(datos_log)) {
    datos_log[i] =  rweibull(1, parametros_wei[1], parametros_wei[2])
  }
  resultado[j] = sum(datos_log)
  
}
sum(datos$cuantia)
quantile(resultado)
(quantile(resultado, probs = .99) - median(resultado))

hist(rweibull(1000, parametros_wei[1], parametros_wei[2]))
