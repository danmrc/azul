ols <- function(x,y){
  solve(t(x)%*%x)%*%t(x)%*%y
}

func <- function(){
  for(j in 1:1000){
    x <- matrix(rnorm(500),ncol = 5)
    bet <- c(1,2,3,4,5)
    y <- x%*%bet + rnorm(100)
    ols(x,y)
  }
}

tempo <- rep(0,100)

for(j in 1:100){

  tempo[j] <- system.time(func())[3]
}

func2 <- function(){
  for(j in 1:1000){
    x <- matrix(rnorm(500),ncol = 5)
    bet <- c(1,2,3,4,5)
    y <- x%*%bet + rnorm(100)
    lm(y ~ x)
  }
}

tempo <- rep(0,100)

for(j in 1:100){
  
  tempo[j] <- system.time(func2())[3]
}

##2 Otimização

rm(list=ls())

vals = seq(1,1000,by = 0.01)

f <- function(x)(x[1]^2+x[2]^2)

teste_optim <- function(){
  valores <- matrix(0,ncol = 2, nrow = length(vals))
  
  for (i in 1:length(vals)){
    x0 = c(vals[i],vals[i])
    otimo = optim(x0,f)
    valores[i,] = otimo$par
  }
  return(valores)
}

system.time(teste_optim())
