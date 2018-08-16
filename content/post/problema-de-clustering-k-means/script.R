####################################################
####################################################
####### O Problema de Clustering K-means # #########
######## PEDRO CAVALCANTE OLIVEIRA ################# 
################ AZUL ##############################
library(dplyr)
library(cluster)
library(ggplot2)
#####Começaremos gerando dados aleatórios
##### Seja n o tamanho da amostra, o leitor pode alterar se quiser

n <- 100000

#### Geraremos um vetor aleatório no R^2

x <- rnorm(mean = 0, 
            sd = 1,
              n= n)

y <- rnorm(mean = 0,
            sd = 1,
              n = n)

amostra1 <- data.frame(x, y)

amostra1 %>%
  ggplot(aes(x=x, y=y))+
  geom_point(color = "light blue")+
  geom_density_2d(color = "dark blue")+
  geom_vline(aes(xintercept=mean(x)),   
             color="black", linetype="dashed", size=1)+
  geom_hline(aes(yintercept=mean(y)),   
             color="black", linetype="dashed", size=1)

##### Definimos uma semente aleatória

set.seed(1234)

##### Método do Cotovelo

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(amostra1)

kmeans_amostra1_1 <- kmeans(amostra1, 
                            centers = 3)

amostra1 %>%
  ggplot(aes(x=x, y=y))+
  geom_point(color = kmeans_amostra1_1$cluster)+
  geom_density_2d(color = "dark blue")+
  geom_vline(aes(xintercept=mean(x)),   
             color="black", linetype="dashed", size=1)+
  geom_hline(aes(yintercept=mean(y)),   
             color="black", linetype="dashed", size=1)

kmeans_amostra1_2 <- kmeans(amostra1, 
                            centers = 6)

amostra1 %>%
  ggplot(aes(x=x, y=y))+
  geom_point(color = kmeans_amostra1_2$cluster)+
  geom_density_2d(color = "dark blue")+
  geom_vline(aes(xintercept=mean(x)),   
             color="black", linetype="dashed", size=1)+
  geom_hline(aes(yintercept=mean(y)),   
             color="black", linetype="dashed", size=1)

##### O algoritimo encontra agrupamentos
##### Apesar dos dados serem homogeneamente distribuídos



