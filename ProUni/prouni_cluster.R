library(ggplot2)
library(readxl)
library(dbscan)
library(mclust)


##### Base pode ser encontrada aqui: https://github.com/pcavalcanteoliveira/dados_blog/blob/master/prouni.xlsx
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

##### Começamos a análise com clustering kmeans

analise <- kmeans(final, 
                      centers = 4)

##### Inspecione o objeto

str(analise)

##### Visuailzação
plot(final, 
        col = analise$cluster)

##### clustering por dbscan, abordagem por densidade
analise2 <- dbscan(final,
                    eps = 100) 

hullplot(final, 
         cl = analise2$cluster)

##### mclustering
analise3 <- Mclust(final)
plot(analise3)




