##################
# Código com scripts para replicação dos resultados do seminário 
# Econometria Tradicional vs Machine Learning
# Autor: Pedro Cavalcante Oliveira, Departamento de Economia, UFF
# email: pc_oliveira@id.uff.br
# Data da última alteração: 30/03/2019
# obs: ...
##################

##### Carregar bibliotecas e definir seed

set.seed(1010)

library(tictoc)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)
library(e1071)
library(InformationValue)
library(randomForest)

######################
tic("Ler os dados")
dados = as_tibble(readRDS("prouni_limpo.Rds"))

dados$dummy = ifelse(dados$medicina == "Medicina", 1, 0)

dados$completo = complete.cases(dados)
dados = dados[dados$completo == TRUE,]
dados$completo = NULL

toc()


n = 100 # numero de validações

resultadosSVM = vector()
resultadosOLS = vector()
resultadosPROBIT = vector()
resultadosRF = vector()

tic("Rodar o Modelo")
for(i in 1:n) {

dados$index = sample(2, 
                     nrow(dados), 
                     replace = TRUE,
                     prob = c(0.5, 0.5))

sample = dados[dados$index == 1,]
out.sample = dados[dados$index == 2,]

#SVM

svmfit = svm(medicina ~ mensalidade + nota,
             data = sample,
             type = "C-classification",
             scale = TRUE,
             kernel = "polynomial")

out.sample$predicaosvm = predict(svmfit, out.sample)

resultSVM = as.vector(table(out.sample$medicina, out.sample$predicaosvm))
resultadosSVM[i] = resultSVM[1]/(resultSVM[1] + resultSVM[2])

## modelo linear simples

modelolinear = lm(dummy ~ nota + mensalidade,
                  data = sample)

out.sample$predicaoOLS = predict(modelolinear, out.sample)
out.sample$predicaoOLS = ifelse(out.sample$predicaoOLS > .5, 
                                "Medicina", "Não-Medicina")

resultOLS = as.vector(table(out.sample$medicina, out.sample$predicaoOLS))
resultadosOLS[i] = resultOLS[1]/(resultOLS[1] + resultOLS[2])


#modelo probit

modeloprobit = glm(dummy ~ mensalidade + nota,
                   data = dados, 
                   family = binomial(link = "logit"))

out.sample$predicaoPROBIT = predict(modeloprobit, out.sample)
out.sample$predicaoPROBIT = ifelse(out.sample$predicaoPROBIT > .1, "Medicina", "Não-Medicina")
out.sample$predicaoPROBITdummy = ifelse(out.sample$predicaoPROBIT == "Medicina", 1, 0)
resultadoPROBIT = table(out.sample$medicina, out.sample$predicaoPROBIT)
resultadosPROBIT[i] = resultadoPROBIT[1]/(resultadoPROBIT[1] + resultadoPROBIT[2])

########## modelo RF

floresta = randomForest(medicina ~ mensalidade + nota,
                        data = sample, 
                        importance = TRUE)

out.sample$predicaoRF = predict(floresta, 
                                out.sample,
                                type = "response")

resultRF = as.vector(table(out.sample$medicina, out.sample$predicaoRF))
resultadosRF[i] = resultRF[1]/(resultRF[1] + resultRF[2])
}
toc()
tic("Tratar e Apresentar Resultados")
########
resultados = tibble(Linear = resultadosOLS,
                    SVM = resultadosSVM,
                    RF = resultadosRF) # formato todo fudido

 ### agora arrumamos os dados
resultados = resultados %>%
  gather("Linear", "SVM", "RF",
         key = método, 
         value = Acerto)

resultados$estim = factor(resultados$estim)

sumario = resultados %>%
  group_by(método) %>%
  summarise(Sucesso = mean(Acerto))

sumario = sumario %>%
  arrange(Sucesso)

sumario %>%
  ggplot(aes(x = método, 
             y = Sucesso, 
             fill = método)) + 
  geom_bar(stat = "identity") +
  labs(title = "Resultados por método")

######################
png(filename = "svmclassplot.png", 
    width = 1280, 
    height = 720, 
    res = 500)

plot(svmfit, 
     dados, 
     nota ~ mensalidade)

##################

plot(floresta,
     main = "Erro das estimativas de Random Forest")

toc()
toc()

########## CLUSTERING K-MEANS

wssplot <- function(data, nc = 15, seed = 1234){
  wss <- (nrow(data)-1)*sum(apply(data, 2, var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type = "b", xlab = "Número de Agrupamentos",
       ylab = "Soma dos Quadrados Intragrupo")}

wssplot(sample) 

##### Pelos criterios anteriores, 3 clusters parece o adequado

analise_kmeans <- kmeans(sample, 
                         centers = 3)

##### Visuailzação e avaliação

table(sample$medicina, analise_kmeans$cluster)

plot(sample, 
     col = analise_kmeans$cluster)

sample %>%
  ggplot(aes(x=mensalidade, 
             y=nota,
             colour = analise_kmeans$cluster, 
             show.legend = FALSE)) +
  geom_point() +
  stat_density_2d() +
  xlab("Mensalidade do curso no ProUni") +
  ylab("Nota de Corte do curso no ProUni") +
  labs(col = "Agrupamento")


normal <- data.frame(apply(sample, 2, scale))
normal$medicina <- sample$medicina

#### Repetimos os procedimentos anteriores

wssplot(finalnormal, 
        nc = 6) 

#### Observe que agora 3 parece ser um k melhor

analise_kmeans_normal <- kmeans(finalnormal, 
                                centers = 3)

table(finalnormal$medicina, 
      analise_kmeans_normal$cluster)

plot(finalnormal, 
     col = analise_kmeans_normal$cluster)

clusplot(sample, analise_kmeans_normal$cluster,
         main = 'Procurando por 3 agrupamentos no ProUni',
         color = TRUE,
         shade = TRUE,
         lines = 0)


