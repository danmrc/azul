library(benchr)

ols <- function(x,y){
  solve(t(x)%*%x)%*%t(x)%*%y
}

func <- function(){
  x <- matrix(rnorm(500),ncol = 5)
  bet <- c(1,2,3,4,5)
  y <- x%*%bet+rnorm(100)
  ols(x,y)
}

aa <- benchmark(func())

benchmark(lm(y ~ x))

#Para a última seção do artigo:

##2 Otimização

rm(list=ls())


weib <- function(par){
  -1*sum(log(par[2]) - log(par[1]) + (par[2]-1)*(log(x)-log(par[1]))-(x/par[1])^par[2])
}

x0 = c(2,2)

func2 <- function(){
  x <- rweibull(500,1)
  optim(x0,weib,method="L-BFGS-B",lower=c(0,0))
}

benchmark(func2())

##3 Bootstrapping



Boot <- function(){
  amostra <- rnorm(1000)
  boot <- rep(0,10000)
  
  for(i in 1:10000){
    amostra_boot <- sample(amostra,size = 500, replace = T)
    boot[i] <- mean(amostra_boot)
  }
}


benchmark(Boot())
