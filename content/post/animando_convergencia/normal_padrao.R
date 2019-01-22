library(ggplot2)
library(tibble)
library(gganimate)
library(dplyr)

#ggplot(datasaurus_dozen, aes(x=x, y=y))+
 # geom_point() +
  #transition_states(dataset, 3, 1) + 
#  ease_aes('cubic-in-out')

set.seed(1234)
lista = list()
n = 1000

for (i in 1:n) {
  
  simulados = rnorm(i) # dados em si
  tirada = factor(i) # fator informando em qual rodada o dado saiu
  base = tibble(simulados = simulados,
                tirada = tirada)
  lista[[i]] = base 
  
}

base = do.call(what = rbind,
               args = lista)

str(base)

base$tirada_simples = base$tirada %>%
                        as.numeric() %>%
                          round(digits = 0)

base$percentil = percentil 

base %>%
  ggplot(aes(x = simulados)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, size = 2) +
  ylim(0,2) +
  transition_states(tirada_simples)



