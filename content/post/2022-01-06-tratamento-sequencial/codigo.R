simple_rule1 <- function(periodo,cutoff,media0,media1){
  
  if (periodo < cutoff){
    if(periodo %% 2 == 0){
      return(0)
    } else{
      return(1)
    }
    
  } else {
    if(media1 > media0){
      return(1)
    } else{
      return(0)
    }
  }
}

simulacao <- function(N,prob0,prob1,rule,...){
  
  media0 <- 0
  media1 <- 0
  n0 <- 0
  n1 <- 0
  
  for(i in 1:N){
    
    decision <- rule(periodo = i, media0 = media0, media1 = media1,...)
    
    if(decision == 0){
      
      trat <- rbinom(1,1,prob0)
      n0 <- n0 +1
      media0 <- ((n0-1)*media0 + trat)/n0
      
    } else if(decision == 1){
      
      trat <- rbinom(1,1,prob1)
      n1 <- n1 +1
      media1 <- ((n1-1)*media1 + trat)/n1
      
    } else{
      stop("Decisão deve ser 0 ou 1")
    }
    
  }
  return(n0*media0 + n1*media1)
}
