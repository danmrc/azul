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
