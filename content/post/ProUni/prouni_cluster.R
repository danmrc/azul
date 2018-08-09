####################################################
####################################################
########ANALISANDO O PROUNI COM CLUSTERING##########
######## PEDRO CAVALCANTE OLIVEIRA ################# 
#################AZUL###############################

##### Carregar bibliotecas

library(ggplot2)
library(readxl)
library(dbscan)
library(mclust)
library(cluster)

##### A base de dados pode ser encontrada aqui: https://github.com/danmrc/azul/tree/master/content/post/ProUni

prouni <- read_excel("C:/Users/e270860661/Downloads/prouni.xlsx", 
                     sheet = "Formatados_para_rodar")
View(prouni)
str(prouni)


##### Aqui selecionamos as variáveis de interesse: 
##### Dummy de Medicina
##### Mensalidade
##### Nota de Corte

coercivo <- data.frame(prouni$mensalidade, 
                        prouni$Medicina, 
                          prouni$nota_integral_ampla)
##### Inspecione a base
summary(coercivo)

##### Algoritimos de clustering não lidam bem com NAs
##### Iremos retirar obs que não sejam completas

coercivo$dropador <- complete.cases(coercivo)
final <- coercivo[coercivo$dropador == TRUE,]

##### Agora retiramos o vetor residual que indica se a obs é completa
final$dropador <- NULL

##### Definimos uma semente aleatória

set.seed(100)

##### Começamos a análise de clustering kmeans
##### Primeiro avaliamos o número ótimo para k
##### A intuição diz que procuramos 2, Medicina e não-Medicina

##### Iremos estimar o k ótimo por bootstraping
##### Não usar esse método em computadores que não tenham bons processadores e memória

clusGap(final, kmeans, K.max = 6, B = 100)

##### Agora pelo critério do cotovelo, computacionalmente menos exigente

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(final, 
          nc=6) 

##### Pelos criterios anteriores, 2 clusters parece o adequado

analise_kmeans <- kmeans(final, 
                          centers = 2)

##### Inspecione o objeto

str(analise)

##### Visuailzação e avaliação

table(final$prouni.Medicina, analise_kmeans$cluster)

plot(final, 
     col = analise_kmeans$cluster)

clusplot(final, analise_kmeans$cluster,
                        main='Procurando por 2 agrupamentos',
                            color=TRUE,
                              shade=TRUE,
                                lines=0)


#### O leitor mais atento percebeu que kmeans não lida bem com os dados do ProUni
#### Isso porque o algoritimo utiliza distância euclidiana
#### Logo, não é adequado para lidar com ordens de grandezas muito díspares
#### Para tanto, podemos normalizar as variáveis para terem média 0 e variância unitária


finalnormal <- data.frame(apply( final, 2, scale))
finalnormal$prouni.Medicina <- final$prouni.Medicina

#### Repetimos os procedimentos anteriores

wssplot(finalnormal, 
            nc=6) 

#### Observe que agora 3 parece ser um k melhor

analise_kmeans_normal <- kmeans(finalnormal, 
                                    centers = 3)

table(finalnormal$prouni.Medicina, 
          analise_kmeans_normal$cluster)

plot(finalnormal, 
        col = analise_kmeans_normal$cluster)

clusplot(final, analise_kmeans_normal$cluster,
            main='Procurando por 3 agrupamentos',
              color=TRUE,
                shade=TRUE,
                  lines=0)


#### Performance esmagadoramente superior :)


##### clustering por dbscan, abordagem por densidade
analise2 <- dbscan(final,
                    eps = 200) 

hullplot(final, 
         cl = analise2$cluster)

##### mclustering
analise3 <- Mclust(final)
plot(analise3)




