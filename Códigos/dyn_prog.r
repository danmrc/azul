rm(list=ls())

a <- 0.5
beta <- 0.96


prod <- function(k){k^a}
utility <- function(c){log(c)}

grid <- seq(0.1,10,0.01)
f.grid <- utility(grid)

w <- utility(grid)
y <- grid
k<-1
e <- 999

res <- matrix(NA,ncol=3,nrow=length(y))
colnames(res) <- c("Objetivo","Política","Renda")

iter <- list()

while(k<=100&e>0.001){

  inter <- function(x){approx(grid,w,xout=x,rule=2)[[2]]}
  for (i in 1:length(y)){
    obj <- function(c){
      ob <- utility(c)+beta*inter(prod(y[i]-c))
      ob[which(is.na(ob)==T)] <- -99999999
      return(ob)}
    #feasible.cons <- seq(0,y[i],0.001)
    #v <- max(obj(feasible.cons))
    #au <- which.max(obj(feasible.cons))
    #pol <- feasible.cons[au]
    mm <- optimise(obj,lower=0.01,upper=y[i],maximum=T) 
    v <- mm$objective
    pol <- round(mm$maximum,digits=4)
    res[i,] <- c(v,pol,y[i])
  }
  e <- max(abs((1-a*beta)*y-res[,2]))
  w<-res[,1]
  iter[[k]] <- res
  print(k)
  k <- k+1
  print(e)
  #ifelse(k==25,print("Não convergiu"),print("Convergiu"))
}