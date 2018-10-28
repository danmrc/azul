x <- matrix(rnorm(500),ncol = 5)
bet <- c(1,2,3,4,5)
y <- x%*%bet+rnorm(100)

ols <- function(x,y){
  solve(t(x)%*%x)%*%t(x)%*%y
}


benchmark(ols(x,y))

benchmark(lm(y ~ x))

##2 Otimização

rm(list=ls())

x <- rweibull(500,1)

weib <- function(par){
  -1*sum(log(par[2]) - log(par[1]) + (par[2]-1)*(log(x)-log(par[1]))-(x/par[1])^par[2])
}

x0 = c(2,2)

benchmark(optim(x0,weib))

##3 Bootstrapping



Boot <- function(amostra){

  boot <- rep(0,10000)
  
  for(i in 1:10000){
    amostra_boot <- sample(amostra,size = 500, replace = T)
    boot[i] <- mean(amostra_boot)
  }
}

tempo <- rep(0,100)

for(i in 1:100){
  amostra <- rnorm(1000)
  tempo[i] <- system.time(Boot(amostra))[3]
}

mean(tempo)
