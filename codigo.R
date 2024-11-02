library(readxl)
library(ggplot2)
library(tidyverse)

datos <- read_excel("Base de datos_TP_Seguros.xlsx")[,c(1,4)] %>% `colnames<-`(c("tiempo", "cuantia"))

# Por ser una variable de escala de razon se puede hacer un cambio de escala

datos = datos %>% mutate(cuantia = cuantia / 10^6)

######## Siniestros

parametros = datos %>% group_by(tiempo) %>% count() %>% ungroup(tiempo) %>%  summarise(promedio = mean(n),
                                                                                       variancia = var(n))
                                                                                       
datos %>% group_by(tiempo) %>% count() %>% ungroup(tiempo) %>% 
  ggplot() + geom_point(aes(x = tiempo, y = n))


# Binomial negativa

lambda = parametros$promedio[1]
h = lambda^2 / (parametros$variancia[1]-lambda) 
p = h / (h + lambda)
q = 1 - p

valor = numeric()
valor[1] = p ^ h 
for (i in 1:50) {
  valor[i+1] =(h + i - 1) * valor[i] * q / i
}

plot(valor)

sum(valor)



datos %>% group_by(tiempo) %>% count() %>% ungroup(tiempo) %>% 
  ggplot() + geom_histogram(aes(x = n))


ggplot(datos) + geom_histogram(aes(x = cuantia))

discrete_inverse_sampling <- function( prob ) {
  U  <- runif(1)
  if(U <= prob[1]){
    return(0)
  }
  for(state in 1:length(prob)) {
    if(sum(prob[1:(state-1)]) < U && U <= sum(prob[1:state]) ) {
      return(state)
    } 
  }
  return(length(prob)+1)
}
datos_bin = numeric(365)
for (i in 1:length(datos_sim)) {
  datos_bin[i] =  discrete_inverse_sampling(valor)
}

ggplot(as.data.frame(datos_bin)) + geom_density(aes( x = datos_bin))


#### Cuantias

options(scipen = 999)

ggplot(datos) + geom_point(aes(x = tiempo, y = cuantia))

ggplot(datos) + geom_density(aes(x = cuantia))

mu = mean(log(datos$cuantia))

sigma2 = mean((log(datos$cuantia)-mu)^2)

R = exp(sigma2)

m1 = exp(mu + log(R) / 2) 

m2 = R * m1 ^ 2

gamma = (R + 2) * sqrt(R-1)

funcion_lognorm = function(x) exp(-(((log(x)-mu)/sqrt(sigma2))^2)/2) / (sqrt(2*pi*sigma2) * x)

sample = data.frame(y = funcion_lognorm(seq(0.01,2,.01)), x = seq(.01,2,.01)) %>%  mutate(y = y/sum(y))
ggplot(sample) + geom_point(aes(x = x, y = y))


datos_log = numeric(sum(datos_bin))
for (i in 1:length(datos_log)) {
  datos_log[i] =  .01 + discrete_inverse_sampling(sample$y)/100
}

ggplot(as.data.frame(datos_log)) + geom_density(aes( x = datos_log))


sum(datos_log)
sum(datos$cuantia)






