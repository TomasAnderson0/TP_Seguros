library(readxl)
library(ggplot2)
library(tidyverse)

datos <- read_excel("Base de datos_TP_Seguros.xlsx")[,c(1,4)] %>% `colnames<-`(c("tiempo", "cuantia"))


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


datos %>% group_by(tiempo) %>% count() %>% ungroup(tiempo) %>% 
  ggplot() + geom_histogram(aes(x = n))


ggplot(datos) + geom_histogram(aes(x = cuantia))









