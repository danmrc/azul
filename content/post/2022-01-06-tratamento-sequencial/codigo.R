simple_rule1 <- function(periodo,cutoff,media0,media1){
  
  if (periodo < cutoff){
    if(periodo %% 2 == 0){
      return(c(0,TRUE))
    } else{
      return(c(1,TRUE))
    }
    
  } else {
    if(media1 > media0){
      return(c(1,FALSE))
    } else{
      return(c(0,FALSE))
    }
  }
}

dgp <- function(prob0,prob1,n0,n1,media0,media1,decision,update){
  
  if(decision == 0){
    
    suc <- rbinom(1,1,prob0)
    
    if(update){
    
      media0 <- (n0*media0 + suc)/(n0+1)
      n0 <- n0 + 1
    }
    
    return(c(suc,n0,n1,media0,media1))
    
  } else if(decision == 1){
    
    suc <- rbinom(1,1,prob1)
    
    if(update){
      
      media1 <- (n1*media1 + suc)/(n1+1)
      n1 <- n1 + 1
    }
    
    return(c(suc,n0,n1,media0,media1))
    
  }
  
  
  
}

simulacao <- function(N,prob0,prob1,rule,...){
  
  media0 <- 0
  media1 <- 0
  n0 <- 0
  n1 <- 0
  payoff <- 0
  
  for(i in 1:N){
    
    decision <- rule(periodo = i, media0 = media0, media1 = media1,...)
    
    if(decision[1] == 0){
      
      result <- dgp(prob0,prob1,n0,n1,media0,media1,decision[1],decision[2])
      n0 <- result[2]
      media0 <- result[4]
      payoff <- result[1] + payoff
      
    } else if(decision[1] == 1){
      
      result <- dgp(prob0,prob1,n0,n1,media0,media1,decision[1],decision[2])
      n1 <- result[3]
      media1 <- result[5]
      payoff <- result[1] + payoff
      
    } else{
      stop("Decisão deve ser 0 ou 1")
    }
    
  }
  return(c(payoff,decision[1]))
}
