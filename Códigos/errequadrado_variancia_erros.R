library(ggplot2)
library(dplyr)
library(gganimate)

n = 5000 # tamanho da amostra

X = runif(n = n, min = 10, max = 20) # variável aleatória X ~ U(10,20)

variancias = seq(1, 10, by = 0.1)

errequadrados = vector()

for(i in 1:length(variancias)) {
  
  u = rnorm(n = n, sd = sqrt(variancias[i])) # pertubações aleatórias com distribuição u ~ N(0,1)
  
  Y = 5 + 0.8*X + u
  
  dados = data.frame(explicada = Y,
                     explicativa = X)
  
  modelo = lm(Y ~ X, # fórmula do modelo a ser estimado 
              data = dados) # data.frame em que estão as variáveis
  
  errequadrados[i] = summary(modelo)$r.squared
}

dados %>%
  ggplot(aes(x = explicativa, y = explicada)) +
  geom_point() # exploramos


grafico = data.frame(errequadrados = errequadrados,
                     variancia = variancias)

grafico %>%
  ggplot(aes(y = errequadrados, 
             x = variancia)) +
  geom_line(size = 2) +
  xlab("Variância das pertubações") + 
  ylab("R^2 da regressão")