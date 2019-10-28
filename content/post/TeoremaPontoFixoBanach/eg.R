demanda <- function(alfa,p,w){
  demanda <- rep(0,length(p))
  renda <- p%*%w
  renda <- rep(renda,length(p))
  demanda <- alfa/p*renda
  return(demanda)
}

excess <- function(p,W,...){
  edemanda <- rep(0,length(p))
  dd <- demanda(alfa,p,w)
  edemanda <- dd - W
  return(edemanda)
}

walrasian_auctioneer <- function(excess_demand,p,delta = 0.05){
  new_price = p + delta*excess_demand
  nor <- sum(new_price)
  new_price <- new_price/nor
  return(new_price)
}

p <- matrix(0,ncol=2,nrow=100)
p[1,] <- c(1,2)
w <- c(1,1)
alfa <- c(0.5,0.5)

ee <- matrix(ncol=2,nrow=100)

for(i in 1:99){
  ee[i,] <- excess(W=w,p=p[i,],w = w,alfa=alfa)
  p[i+1,] <- walrasian_auctioneer(ee[i,],p[i,])
}

