library(AER)
library(foreach)
library(doParallel)

n.cores <- 3
cl <- makeCluster(n.cores)
registerDoParallel(cl)


vies <- function(n,k,peso=1){
  
  cf <- matrix(0,ncol = k, nrow = 1000)
  mqo <- rep(0,1000)
  
  for(i in 1:1000){
    z <- matrix(rnorm(n*k),ncol = k)
    u <- rnorm(n)
    
    x <- peso*u + z%*%rep(1,k) + rnorm(n)
    
    y <- u + x + rnorm(n)
    
    mqo[i] <- coef(lm(y ~ x))[2]
    
    aux <- foreach(j = 1:ncol(z), .packages = "AER", .combine = c) %dopar% {
      mod <- ivreg(y ~ x|z[,1:j])
      coef(mod)[2]
    }
    cf[i,] <- aux
    print(i)
  }
  resposta <- list("mqo" = mqo,"iv" = cf)
    return(resposta)
}

caso_1 <- vies(100,50)

mqo <- mean(caso_1[[1]])

cf <- colMeans(caso_1[[2]])

lb <- min(min(cf),mqo,1)
up <- max(max(cf),mqo,1)

plot(1:length(cf),cf, ylim = c(lb,up), main = "100 observações", ylab = "Valor do coeficiente", xlab="Número de Instrumentos")
lines(1:length(cf),rep(mqo,length(cf)),col = 2)
lines(1:length(cf),rep(1,length(cf)),col = 3)

lb <- min(min(cf[-1]),mqo,1)
up <- max(max(cf[-1]),mqo,1)

plot(2:length(cf),cf[-1], ylim = c(lb,up), main = "100 observações, detalhe sem o primeiro coeficiente", ylab = "Valor do coeficiente", xlab="Número de Instrumentos")
lines(2:length(cf),rep(mqo,(length(cf)-1)),col = 2)
lines(2:length(cf),rep(1,(length(cf)-1)),col = 3)

caso_2 <- vies(100,80)

mqo <- mean(caso_2[[1]])

cf <- colMeans(caso_2[[2]])

lb <- min(min(cf),mqo,1)
up <- max(max(cf),mqo,1)

plot(1:length(cf),cf, ylim = c(lb,up), main = "100 observações", ylab = "Valor do coeficiente", xlab="Número de Instrumentos")
lines(1:length(cf),rep(mqo,length(cf)),col = 2)
lines(1:length(cf),rep(1,length(cf)),col = 3)

lb <- min(min(cf[-1]),mqo,1)
up <- max(max(cf[-1]),mqo,1)

plot(2:length(cf),cf[-1], ylim = c(lb,up), main = "100 observações, detalhe sem o primeiro coeficiente", ylab = "Valor do coeficiente", xlab="Número de Instrumentos")
lines(2:length(cf),rep(mqo,(length(cf)-1)),col = 2)
lines(2:length(cf),rep(1,(length(cf)-1)),col = 3)

caso_3 <- vies(1000,100)

mqo <- mean(caso_3[[1]])

cf <- colMeans(caso_3[[2]])

lb <- min(min(cf),mqo,1)
up <- max(max(cf),mqo,1)

plot(1:length(cf),cf, ylim = c(lb,up), main = "1000 observações", ylab = "Valor do coeficiente", xlab="Número de Instrumentos")
lines(1:length(cf),rep(mqo,length(cf)),col = 2)
lines(1:length(cf),rep(1,length(cf)),col = 3)

lb <- min(min(cf),1)
up <- max(max(cf),1)

plot(1:length(cf),cf, ylim = c(lb,up), main = "1000 observações, detalhe sem o coeficiente de MQO", ylab = "Valor do coeficiente", xlab="Número de Instrumentos")
lines(1:length(cf),rep(1,(length(cf))),col = 3)

caso_4 <- vies(100,50,peso=3)

mqo <- mean(caso_4[[1]])

cf <- colMeans(caso_4[[2]])

lb <- min(min(cf),mqo,1)
up <- max(max(cf),mqo,1)

plot(1:length(cf),cf, ylim = c(lb,up), main = "100 observações, peso = 3", ylab = "Valor do coeficiente", xlab="Número de Instrumentos")
lines(1:length(cf),rep(mqo,length(cf)),col = 2)
lines(1:length(cf),rep(1,length(cf)),col = 3)

lb <- min(min(cf[-1]),mqo,1)
up <- max(max(cf[-1]),mqo,1)

plot(2:length(cf),cf[-1], ylim = c(lb,up), main = "100 observações, detalhe sem o primeiro coeficiente, peso = 3", ylab = "Valor do coeficiente", xlab="Número de Instrumentos")
lines(2:length(cf),rep(mqo,(length(cf)-1)),col = 2)
lines(2:length(cf),rep(1,(length(cf)-1)),col = 3)

caso_5 <- vies(100,50,peso=5)

mqo <- mean(caso_5[[1]])

cf <- colMeans(caso_5[[2]])

lb <- min(min(cf),mqo,1)
up <- max(max(cf),mqo,1)

plot(1:length(cf),cf, ylim = c(lb,up), main = "100 observações, peso = 5", ylab = "Valor do coeficiente", xlab="Número de Instrumentos")
lines(1:length(cf),rep(mqo,length(cf)),col = 2)
lines(1:length(cf),rep(1,length(cf)),col = 3)

caso_6 <- vies(100,50,peso=10)

mqo <- mean(caso_6[[1]])

cf <- colMeans(caso_6[[2]])

lb <- min(min(cf),mqo,1)
up <- max(max(cf),mqo,1)

plot(1:length(cf),cf, ylim = c(lb,up), main = "100 observações, peso = 10", ylab = "Valor do coeficiente", xlab="Número de Instrumentos")
lines(1:length(cf),rep(mqo,length(cf)),col = 2)
lines(1:length(cf),rep(1,length(cf)),col = 3)

caso_7 <- vies(100,100)

mqo <- mean(caso_7[[1]])

cf <- colMeans(caso_7[[2]])

lb <- min(min(cf),mqo,1)
up <- max(max(cf),mqo,1)

plot(1:length(cf),cf, ylim = c(lb,up), main = "100 observações", ylab = "Valor do coeficiente", xlab="Número de Instrumentos")
lines(1:length(cf),rep(mqo,length(cf)),col = 2)
lines(1:length(cf),rep(1,length(cf)),col = 3)

lb <- min(min(cf[-1]),mqo,1)
up <- max(max(cf[-1]),mqo,1)

plot(2:length(cf),cf[-1], ylim = c(lb,up), main = "100 observações, detalhe sem o primeiro coeficiente", ylab = "Valor do coeficiente", xlab="Número de Instrumentos")
lines(2:length(cf),rep(mqo,(length(cf)-1)),col = 2)
lines(2:length(cf),rep(1,(length(cf)-1)),col = 3)

caso_8 <- vies(100,1)

mqo <- mean(caso_8[[1]])

cf <- colMeans(caso_8[[2]])

lb <- min(min(cf),mqo,1)
up <- max(max(cf),mqo,1)

plot(1:length(cf),cf, ylim = c(lb,up), main = "100 observações", ylab = "Valor do coeficiente", xlab="Número de Instrumentos")
points(mqo,pch = 3,col=2)
points(1,pch=3,col = 3)

caso_9 <- vies(1000,50,30)

mqo <- mean(caso_9[[1]])

cf <- colMeans(caso_9[[2]])

lb <- min(min(cf),mqo,1)
up <- max(max(cf),mqo,1)

plot(1:length(cf),cf, ylim = c(lb,up), main = "1000 observações, peso = 30", ylab = "Valor do coeficiente", xlab="Número de Instrumentos")
lines(2:length(cf),rep(mqo,(length(cf)-1)),col = 2)
lines(2:length(cf),rep(1,(length(cf)-1)),col = 3)

caso_9 <- vies(5000,50,50)

mqo <- mean(caso_9[[1]])

cf <- colMeans(caso_9[[2]])

lb <- min(min(cf),mqo,1)
up <- max(max(cf),mqo,1)

plot(1:length(cf),cf, ylim = c(lb,up), main = "5000 observações, peso = 50", ylab = "Valor do coeficiente", xlab="Número de Instrumentos")
lines(2:length(cf),rep(mqo,(length(cf)-1)),col = 2)
lines(2:length(cf),rep(1,(length(cf)-1)),col = 3)
