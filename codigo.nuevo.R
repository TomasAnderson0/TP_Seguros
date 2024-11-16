library(readxl)
library(ggplot2)
library(tidyverse)
library(EnvStats)
library(ExtDist)

datos <- read_excel("Base de datos_TP_Seguros.xlsx")[,c(1,4)] %>% `colnames<-`(c("tiempo", "cuantia"))

# Por ser una variable de escala de razon se puede hacer un cambio de escala

datos = datos %>% mutate(cuantia = cuantia / 10^6)

######## Siniestros

parametros = datos %>% group_by(tiempo) %>% count() %>% ungroup(tiempo) %>%  summarise(promedio = mean(n),
                                                                                       variancia = var(n))
                                                                                       
datos %>% group_by(tiempo) %>% count() %>% ungroup(tiempo) %>% 
  ggplot() + geom_point(aes(x = tiempo, y = n))



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

datos_bin = numeric(25615)

for (i in 1:length(datos_bin)) {
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


datos_log = numeric(mean(poisson) * 25615)
for (i in 1:length(datos_log)) {
  datos_log[i] =  .01 + discrete_inverse_sampling(sample$y)/100
}

ggplot(as.data.frame(datos_log)) + geom_density(aes( x = datos_log))


sum(datos_log)
sum(datos$cuantia)






# Parametros

lambda_poisson = 25615*((1/2) * nrow(datos)/25615 +  (1/6) * 3.023 /24.752 + (2/6) *  3.581 / 25.348)

mu = mean(log(datos$cuantia))

sigma2 = mean((log(datos$cuantia)-mu)^2)

R = exp(sigma2)

m1 = exp(mu + log(R) / 2) 

m2 = R * m1 ^ 2

gamma = (R + 2) * sqrt(R-1)

funcion_lognorm = function(x) exp(-(((log(x)-mu)/sqrt(sigma2))^2)/2) / (sqrt(2*pi*sigma2) * x)

sample = data.frame(y = funcion_lognorm(seq(0.01,2,.01)), x = seq(.01,2,.01)) %>%  mutate(y = y/sum(y))


resultado = numeric()

for (j in 1:1000) {
  
# Poisson 

poisson = rpois(25615,lambda_poisson)

#Log normal

datos_log = numeric(mean(poisson) * 25615)
for (i in 1:length(datos_log)) {
  datos_log[i] =  .01 + discrete_inverse_sampling(sample$y)/100
}
resultado[j] = sum(datos_log)

}

quantile(resultado/25615)
(quantile(resultado/25615, probs = .99) - median(resultado/25615))/median(resultado/25615)





########### Forma 2

esperanza = lambda_poisson * mean(datos$cuantia)

varianza = lambda_poisson * mean(datos$cuantia^2)


y_0 = 2.33 + (2.33^2-1) * .8/6

y_0*sqrt(varianza) + esperanza


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





# Binomial negativa

lambda = nrow(datos)/25615
h = lambda^2 / (lambda*seq(1.05,2,.05)-lambda) 
p = h / (h + lambda)
q = 1 - p

sum(rnbinom(25615,size = lambda/1088, mu = lambda))

  valor = numeric()
  valor[1] = p ^ h 
  for (i in 1:50) {
    valor[i+1] =(h + i - 1) * valor[i] * q / i
  }







